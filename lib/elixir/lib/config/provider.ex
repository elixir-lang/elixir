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

  ## Sample config provider

  For example, imagine you need to load some configuration from
  a JSON file and load that into the system. Said configuration
  provider would look like:

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

  Then when specifying your release, you can specify the provider:

      config_providers: [{JSONConfigProvider, "/etc/config.json"}]

  Now once the system boots, it will invoke the provider early in
  the boot process, save the merged configuration to the disk, and
  reboot the system with the new values in place.
  """

  @type config :: keyword
  @type state :: term

  @typedoc """
  A path pointing to a configuration file.

  Since configuration files are often accessed on target machines,
  it can be expressed either as:

    * a binary representing an absolute path

    * a tuple {:system, system_var, path} where the config is the
      concatenation of the `system_var` with the given `path`

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
  `c:init/1`. Then you typically read the extra configuration
  from an external source and merge it into the received `config`.
  Merging should be done with `Config.Reader.merge/2`, as it
  performs deep merge. It should return the updated config.

  Note that `c:load/2` is typically invoked very early in the
  boot process, therefore if you need to use an application
  in the provider, it is your responsibility to start it.
  """
  @callback load(config, state) :: config

  @doc false
  defstruct [:providers, :config_path, extra_config: [], prune_after_boot: false]

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

  @doc false
  def init(providers, config_path, opts \\ []) when is_list(providers) and is_list(opts) do
    validate_config_path!(config_path)
    providers = for {provider, init} <- providers, do: {provider, provider.init(init)}
    struct!(%Config.Provider{config_path: config_path, providers: providers}, opts)
  end

  @doc false
  def boot(app, key, restart_fun \\ &System.restart/0) do
    # The app with the config provider settings may not
    # have been loaded at this point, so make sure we load
    # its environment before querying it.
    _ = :application.load(app)

    # The config provider typically runs very early in the
    # release process, so we need to make sure Elixir is started
    # before we go around running Elixir code.
    {:ok, _} = :application.ensure_all_started(:elixir)

    case :application.get_env(app, key) do
      {:ok, %Config.Provider{} = provider} ->
        path = resolve_config_path!(provider.config_path)
        validate_no_cyclic_boot!(path)

        read_config!(path)
        |> Config.__merge__([{app, [{key, booted_key(provider, path)}]} | provider.extra_config])
        |> run_providers(provider)
        |> write_config!(path)

        restart_fun.()

      {:ok, {:booted, path}} ->
        File.rm(path)
        :booted

      {:ok, :booted} ->
        :booted

      _ ->
        :skip
    end
  end

  defp booted_key(%{prune_after_boot: true}, path), do: {:booted, path}
  defp booted_key(%{prune_after_boot: false}, _path), do: :booted

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

    case File.write(path, contents, [:utf8]) do
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
    raise(msg)
  end
end
