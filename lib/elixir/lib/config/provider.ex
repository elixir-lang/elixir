defmodule Config.Provider do
  @moduledoc """
  Specifies a provider API that loads configuration during boot.

  Config providers are typically used during releases to load
  external configuration while the system boots. This is done
  by starting the VM with the minimum amount of applications
  running, then invoking all of the providers, and then
  restarting the system. This requires a mutable configuration
  file on disk, as the results of the providers are written to
  the file system. For more information on runtime configuration,
  see `mix release`.

  ## Multiple config files

  One common use of config providers is to specify multiple
  configuration files in a release. Elixir ships with one provider,
  called `Config.Reader`, which is capable of handling Elixir's
  built-in config files.

  For example, imagine you want to list some basic configuration
  on Mix's built-in `config/runtime.exs` file, but you also want
  some additional configuration files. To do so, you can do this
  in your `mix.exs`:

      releases: [
        demo: [
          config_providers: [
            {Config.Reader, {:system, "RELEASE_ROOT", "/extra_config.exs"}}
          ]
        ]
      ]

  You can place this `extra_config.exs` file in your release in
  multiple ways:

    1. If it is available on the host when assembling the release,
      you can place it on "rel/overlays/extra_config.exs" and it
      will be automatically copied to the release root

    2. If it is available on the target during deployment, you can
      simply copy it to the release root as a step in your deployment

  Now once the system boots, it will load both `config/runtime.exs`
  and `extra_config.exs` early in the boot process.

  ## Custom config provider

  You can also implement custom config providers, similar to how
  `Config.Reader` works. For example, imagine you need to load
  some configuration from a JSON file and load that into the system.
  Said configuration provider would look like:

      defmodule JSONConfigProvider do
        @behaviour Config.Provider

        # Let's pass the path to the JSON file as config
        def init(path) when is_binary(path), do: path

        def load(config, path) do
          # We need to start any app we may depend on.
          {:ok, _} = Application.ensure_all_started(:jason)

          json = path |> File.read!() |> Jason.decode!()

          Config.Reader.merge(
            config,
            my_app: [
              some_value: json["my_app_some_value"],
              another_value: json["my_app_another_value"],
            ]
          )
        end
      end

  Then, when specifying your release, you can specify the provider in
  the release configuration:

      releases: [
        demo: [
          config_providers: [
            {JSONConfigProvider, "/etc/config.json"}
          ]
        ]
      ]

  """

  @type config :: keyword
  @type state :: term

  @typedoc """
  A path pointing to a configuration file.

  Since configuration files are often accessed on target machines,
  it can be expressed either as:

    * a binary representing an absolute path

    * a `{:system, system_var, path}` tuple where the config is the
      concatenation of the environment variable `system_var` with
      the given `path`

  """
  @type config_path :: {:system, binary(), binary()} | binary()

  @doc """
  Invoked when initializing a config provider.

  A config provider is typically initialized on the machine
  where the system is assembled and not on the target machine.
  The `c:init/1` callback is useful to verify the arguments
  given to the provider and prepare the state that will be
  given to `c:load/2`.

  Furthermore, because the state returned by `c:init/1` can
  be written to text-based config files, it should be
  restricted only to simple data types, such as integers,
  strings, atoms, tuples, maps, and lists. Entries such as
  PIDs, references, and functions cannot be serialized.
  """
  @callback init(term) :: state

  @doc """
  Loads configuration (typically during system boot).

  It receives the current `config` and the `state` returned by
  `c:init/1`. Then, you typically read the extra configuration
  from an external source and merge it into the received `config`.
  Merging should be done with `Config.Reader.merge/2`, as it
  performs deep merge. It should return the updated config.

  Note that `c:load/2` is typically invoked very early in the
  boot process, therefore if you need to use an application
  in the provider, it is your responsibility to start it.
  """
  @callback load(config, state) :: config

  @doc false
  defstruct [
    :providers,
    :config_path,
    extra_config: [],
    prune_runtime_sys_config_after_boot: false,
    reboot_system_after_config: false,
    validate_compile_env: false
  ]

  @reserved_apps [:kernel, :stdlib]

  @doc """
  Validates a `t:config_path/0`.
  """
  @doc since: "1.9.0"
  @spec validate_config_path!(config_path) :: :ok
  def validate_config_path!({:system, name, path})
      when is_binary(name) and is_binary(path),
      do: :ok

  def validate_config_path!(path) do
    if is_binary(path) and Path.type(path) != :relative do
      :ok
    else
      raise ArgumentError, """
      expected configuration path to be:

        * a binary representing an absolute path
        * a tuple {:system, system_var, path} where the config is the \
      concatenation of the `system_var` with the given `path`

      Got: #{inspect(path)}
      """
    end
  end

  @doc """
  Resolves a `t:config_path/0` to an actual path.
  """
  @doc since: "1.9.0"
  @spec resolve_config_path!(config_path) :: binary
  def resolve_config_path!(path) when is_binary(path), do: path
  def resolve_config_path!({:system, name, path}), do: System.fetch_env!(name) <> path

  # Private keys
  @init_key :config_provider_init
  @booted_key :config_provider_booted

  # Public keys
  @reboot_mode_key :config_provider_reboot_mode

  @doc false
  def init(providers, config_path, opts \\ []) when is_list(providers) and is_list(opts) do
    validate_config_path!(config_path)
    providers = for {provider, init} <- providers, do: {provider, provider.init(init)}
    init = struct!(%Config.Provider{config_path: config_path, providers: providers}, opts)
    [elixir: [{@init_key, init}]]
  end

  @doc false
  def boot(reboot_fun \\ &restart_and_sleep/0) do
    # The config provider typically runs very early in the
    # release process, so we need to make sure Elixir is started
    # before we go around running Elixir code.
    {:ok, _} = :application.ensure_all_started(:elixir)

    case Application.fetch_env(:elixir, @booted_key) do
      {:ok, {:booted, path}} ->
        path && File.rm(path)

        with {:ok, %Config.Provider{} = provider} <- Application.fetch_env(:elixir, @init_key) do
          maybe_validate_compile_env(provider)
        end

        :booted

      _ ->
        case Application.fetch_env(:elixir, @init_key) do
          {:ok, %Config.Provider{} = provider} ->
            path = resolve_config_path!(provider.config_path)
            reboot_config = [elixir: [{@booted_key, booted_value(provider, path)}]]
            boot_providers(path, provider, reboot_config, reboot_fun)

          _ ->
            :skip
        end
    end
  end

  defp boot_providers(path, provider, reboot_config, reboot_fun) do
    validate_no_cyclic_boot!(path)
    original_config = read_config!(path)

    config =
      original_config
      |> Config.__merge__(provider.extra_config)
      |> run_providers(provider)

    if provider.reboot_system_after_config do
      config
      |> Config.__merge__(reboot_config)
      |> write_config!(path)

      reboot_fun.()
    else
      for app <- @reserved_apps, config[app] != original_config[app] do
        abort("""
        Cannot configure #{inspect(app)} because :reboot_system_after_config has been set \
        to false and #{inspect(app)} has already been loaded, meaning any further \
        configuration won't have an effect.

        The configuration for #{inspect(app)} before config providers was:

        #{inspect(original_config[app])}

        The configuration for #{inspect(app)} after config providers was:

        #{inspect(config[app])}
        """)
      end

      _ = Application.put_all_env(config, persistent: true)
      maybe_validate_compile_env(provider)
      :ok
    end
  end

  defp maybe_validate_compile_env(provider) do
    with [_ | _] = compile_env <- provider.validate_compile_env do
      validate_compile_env(compile_env)
    end
  end

  @doc false
  def validate_compile_env(compile_env, ensure_loaded? \\ true) do
    for {app, [key | path], compile_return} <- compile_env,
        ensure_app_loaded?(app, ensure_loaded?) do
      try do
        traverse_env(Application.fetch_env(app, key), path)
      rescue
        e ->
          abort("""
          application #{inspect(app)} failed reading its compile environment #{path(key, path)}:

          #{Exception.format(:error, e, __STACKTRACE__)}

          Expected it to match the compile time value of #{return_to_text(compile_return)}.

          #{compile_env_tips(app)}
          """)
      else
        ^compile_return ->
          :ok

        runtime_return ->
          abort("""
          the application #{inspect(app)} has a different value set #{path(key, path)} \
          during runtime compared to compile time. Since this application environment entry was \
          marked as compile time, this difference can lead to different behaviour than expected:

            * Compile time value #{return_to_text(compile_return)}
            * Runtime value #{return_to_text(runtime_return)}

          #{compile_env_tips(app)}
          """)
      end
    end

    :ok
  end

  defp ensure_app_loaded?(app, true), do: Application.ensure_loaded(app) == :ok
  defp ensure_app_loaded?(app, false), do: Application.spec(app, :vsn) != nil

  defp path(key, []), do: "for key #{inspect(key)}"
  defp path(key, path), do: "for path #{inspect(path)} inside key #{inspect(key)}"

  defp compile_env_tips(app),
    do: """
    To fix this error, you might:

      * Make the runtime value match the compile time one

      * Recompile your project. If the misconfigured application is a dependency, \
    you may need to run "mix deps.compile #{app} --force"

      * Alternatively, you can disable this check. If you are using releases, you can \
    set :validate_compile_env to false in your release configuration. If you are \
    using Mix to start your system, you can pass the --no-validate-compile-env flag
    """

  defp return_to_text({:ok, value}), do: "was set to: #{inspect(value)}"
  defp return_to_text(:error), do: "was not set"

  defp traverse_env(return, []), do: return
  defp traverse_env(:error, _paths), do: :error
  defp traverse_env({:ok, value}, [key | keys]), do: traverse_env(Access.fetch(value, key), keys)

  @compile {:no_warn_undefined, {:init, :restart, 1}}
  defp restart_and_sleep() do
    mode = Application.get_env(:elixir, @reboot_mode_key)

    # TODO: Remove otp_release check once we require Erlang/OTP 23+
    if :erlang.system_info(:otp_release) >= '23' and mode in [:embedded, :interactive] do
      :init.restart(mode: mode)
    else
      :init.restart()
    end

    Process.sleep(:infinity)
  end

  defp booted_value(%{prune_runtime_sys_config_after_boot: true}, path), do: {:booted, path}
  defp booted_value(%{prune_runtime_sys_config_after_boot: false}, _path), do: {:booted, nil}

  defp validate_no_cyclic_boot!(path) do
    if System.get_env("ELIXIR_CONFIG_PROVIDER_BOOTED") do
      bad_path_abort("Got infinite loop when running Config.Provider", path)
    else
      System.put_env("ELIXIR_CONFIG_PROVIDER_BOOTED", "1")
    end
  end

  defp read_config!(path) do
    case :file.consult(path) do
      {:ok, [inner]} ->
        inner

      {:error, reason} ->
        bad_path_abort(
          "Could not read runtime configuration due to reason: #{inspect(reason)}",
          path
        )
    end
  end

  defp run_providers(config, %{providers: providers}) do
    Enum.reduce(providers, config, fn {provider, state}, acc ->
      try do
        provider.load(acc, state)
      catch
        kind, error ->
          IO.puts(:stderr, "ERROR! Config provider #{inspect(provider)} failed with:")
          IO.puts(:stderr, Exception.format(kind, error, __STACKTRACE__))
          :erlang.raise(kind, error, __STACKTRACE__)
      else
        term when is_list(term) ->
          term

        term ->
          abort("Expected provider #{inspect(provider)} to return a list, got: #{inspect(term)}")
      end
    end)
  end

  defp write_config!(config, path) do
    contents = :io_lib.format("%% coding: utf-8~n~tw.~n", [config])

    case File.write(path, IO.chardata_to_string(contents)) do
      :ok ->
        :ok

      {:error, reason} ->
        bad_path_abort(
          "Could not write runtime configuration due to reason: #{inspect(reason)}",
          path
        )
    end
  end

  defp bad_path_abort(msg, path) do
    abort(
      msg <>
        ". Please make sure #{inspect(path)} is writable and accessible " <>
        "or choose a different path"
    )
  end

  defp abort(msg) do
    IO.puts(:stderr, "ERROR! " <> msg)
    :erlang.raise(:error, "aborting boot", [{Config.Provider, :boot, 2, []}])
  end
end
