defmodule Mix.Release do
  @moduledoc """
  Defines the release structure and convenience for assembling releases.
  """

  @doc """
  The Mix.Release struct has the following read-only fields:

    * `:name` - the name of the release as an atom
    * `:version` - the version of the release as a string
    * `:path` - the path to the release root
    * `:version_path` - the path to the release version inside the release
    * `:applications` - a map of application with their definitions
    * `:erts_source` - the erts source as a charlist (or nil)
    * `:erts_version` - the erts version as a charlist

  The following fields may be modified as long as they keep their defined types:

    * `:boot_scripts` - a map of boot scripts with the boot script name
      as key and a keyword list with **all** applications that are part of
      it and their modes as value
    * `:config_providers` - a list of `{config_provider, term}` tuples where the
      first element is a module that implements the `Config.Provider` behaviour
      and `term` is the value given to it on `c:Config.Provider.init/1`
    * `:options` - a keyword list with all other user supplied release options
    * `:steps` - a list of functions that receive the release and returns a release.
      Must also contain the atom `:assemble` which is the internal assembling step

  """
  defstruct [
    :name,
    :version,
    :path,
    :version_path,
    :applications,
    :boot_scripts,
    :erts_source,
    :erts_version,
    :config_providers,
    :options,
    :steps
  ]

  @type mode :: :permanent | :transient | :temporary | :load | :none
  @type application :: atom()
  @type t :: %{
          name: atom(),
          version: String.t(),
          path: String.t(),
          version_path: String.t(),
          applications: %{application() => keyword()},
          boot_scripts: %{atom() => [{application(), mode()}]},
          erts_version: charlist(),
          erts_source: charlist() | nil,
          config_providers: [{module, term}],
          options: keyword(),
          steps: [(t -> t) | :assemble, ...]
        }

  @default_apps [kernel: :permanent, stdlib: :permanent, elixir: :permanent, sasl: :permanent]
  @safe_modes [:permanent, :temporary, :transient]
  @unsafe_modes [:load, :none]
  @significant_chunks ~w(Atom AtU8 Attr Code StrT ImpT ExpT FunT LitT Line)c
  @copy_app_dirs ["priv"]

  @doc false
  @spec from_config!(atom, keyword, keyword) :: t
  def from_config!(name, config, overrides) do
    {name, apps, opts} = find_release(name, config)

    unless Atom.to_string(name) =~ ~r/^[a-z][a-z0-9_]*$/ do
      Mix.raise(
        "Invalid release name. A release name must start with a lowercase ASCII letter, " <>
          "followed by lowercase ASCII letters, numbers, or underscores, got: #{inspect(name)}"
      )
    end

    opts =
      [overwrite: false, quiet: false, strip_beams: true]
      |> Keyword.merge(opts)
      |> Keyword.merge(overrides)

    {include_erts, opts} = Keyword.pop(opts, :include_erts, true)
    {erts_source, erts_lib_dir, erts_version} = erts_data(include_erts)

    loaded_apps = apps |> Keyword.keys() |> load_apps(%{}, erts_lib_dir, :maybe)

    # Make sure IEx is either an active part of the release or add it as none.
    {loaded_apps, apps} =
      if Map.has_key?(loaded_apps, :iex) do
        {loaded_apps, apps}
      else
        {load_apps([:iex], loaded_apps, erts_lib_dir, :maybe), apps ++ [iex: :none]}
      end

    start_boot = build_start_boot(loaded_apps, apps)
    start_clean_boot = build_start_clean_boot(start_boot)

    {path, opts} =
      Keyword.pop_lazy(opts, :path, fn ->
        Path.join([Mix.Project.build_path(config), "rel", Atom.to_string(name)])
      end)

    path = Path.absname(path)

    {version, opts} =
      Keyword.pop_lazy(opts, :version, fn ->
        config[:version] ||
          Mix.raise(
            "No :version found. Please make sure a :version is set in your project definition " <>
              "or inside the release the configuration"
          )
      end)

    {config_providers, opts} = Keyword.pop(opts, :config_providers, [])
    {steps, opts} = Keyword.pop(opts, :steps, [:assemble])
    validate_steps!(steps)

    %Mix.Release{
      name: name,
      version: version,
      path: path,
      version_path: Path.join([path, "releases", version]),
      erts_source: erts_source,
      erts_version: erts_version,
      applications: loaded_apps,
      boot_scripts: %{start: start_boot, start_clean: start_clean_boot},
      config_providers: config_providers,
      options: opts,
      steps: steps
    }
  end

  defp find_release(name, config) do
    {name, opts} = lookup_release(name, config) || infer_release(config)
    {apps, opts} = Keyword.pop(opts, :applications, [])

    if apps == [] and Mix.Project.umbrella?(config) do
      bad_umbrella!()
    end

    app = Keyword.get(config, :app)
    apps = Keyword.merge(@default_apps, apps)

    if is_nil(app) or Keyword.has_key?(apps, app) do
      {name, apps, opts}
    else
      {name, apps ++ [{app, :permanent}], opts}
    end
  end

  defp lookup_release(nil, config) do
    case Keyword.get(config, :releases, []) do
      [] ->
        nil

      [{name, opts}] ->
        {name, opts}

      [_ | _] ->
        case Keyword.get(config, :default_release) do
          nil ->
            Mix.raise(
              "\"mix release\" was invoked without a name but there are multiple releases. " <>
                "Please call \"mix release NAME\" or set :default_release in your project configuration"
            )

          name ->
            lookup_release(name, config)
        end
    end
  end

  defp lookup_release(name, config) do
    if opts = config[:releases][name] do
      {name, opts}
    else
      found = Keyword.get(config, :releases, [])

      Mix.raise(
        "Unknown release #{inspect(name)}. " <>
          "The available releases are: #{inspect(Keyword.keys(found))}"
      )
    end
  end

  defp infer_release(config) do
    if Mix.Project.umbrella?(config) do
      bad_umbrella!()
    else
      {Keyword.fetch!(config, :app), []}
    end
  end

  defp bad_umbrella! do
    Mix.raise("""
    Umbrella projects require releases to be explicitly defined with \
    a non-empty applications key that chooses which umbrella children \
    should be part of the releases:

        releases: [
          foo: [
            applications: [child_app_foo: :permanent]
          ],
          bar: [
            applications: [child_app_bar: :permanent]
          ]
        ]

    Alternatively you can perform the release from the children applications
    """)
  end

  defp erts_data(erts_data) when is_function(erts_data) do
    erts_data(erts_data.())
  end

  defp erts_data(false) do
    {nil, :code.lib_dir(), :erlang.system_info(:version)}
  end

  defp erts_data(true) do
    version = :erlang.system_info(:version)
    {:filename.join(:code.root_dir(), 'erts-#{version}'), :code.lib_dir(), version}
  end

  defp erts_data(erts_source) when is_binary(erts_source) do
    if File.exists?(erts_source) do
      [_, erts_version] = erts_source |> Path.basename() |> String.split("-")
      erts_lib_dir = erts_source |> Path.dirname() |> Path.join("lib") |> to_charlist()
      {to_charlist(erts_source), erts_lib_dir, to_charlist(erts_version)}
    else
      Mix.raise("Could not find ERTS system at #{inspect(erts_source)}")
    end
  end

  defp load_apps(apps, seen, otp_root, included) do
    for app <- apps, reduce: seen do
      seen ->
        if reentrant_seen = reentrant(seen, app, included) do
          reentrant_seen
        else
          load_app(app, seen, otp_root, included)
        end
    end
  end

  defp reentrant(seen, app, included) do
    properties = seen[app]

    cond do
      is_nil(properties) ->
        nil

      included != :maybe and properties[:included] != included ->
        if properties[:included] == :maybe do
          put_in(seen[app][:included], included)
        else
          Mix.raise(
            "#{inspect(app)} is listed both as a regular application and as an included application"
          )
        end

      true ->
        seen
    end
  end

  defp load_app(app, seen, otp_root, included) do
    path = Path.join(otp_root, "#{app}-*")

    case Path.wildcard(path) do
      [] ->
        case :code.lib_dir(app) do
          {:error, :bad_name} -> Mix.raise("Could not find application #{inspect(app)}")
          path -> do_load_app(app, path, seen, otp_root, false, included)
        end

      paths ->
        path = paths |> Enum.sort() |> List.last()
        do_load_app(app, to_charlist(path), seen, otp_root, true, included)
    end
  end

  defp do_load_app(app, path, seen, otp_root, otp_app?, included) do
    case :file.consult(Path.join(path, "ebin/#{app}.app")) do
      {:ok, terms} ->
        [{:application, ^app, properties}] = terms
        value = [path: path, otp_app?: otp_app?, included: included] ++ properties
        seen = Map.put(seen, app, value)
        seen = load_apps(Keyword.get(properties, :applications, []), seen, otp_root, false)
        load_apps(Keyword.get(properties, :included_applications, []), seen, otp_root, true)

      {:error, reason} ->
        Mix.raise("Could not load #{app}.app. Reason: #{inspect(reason)}")
    end
  end

  defp build_start_boot(all_apps, specified_apps) do
    specified_apps ++
      for(
        {app, props} <- all_apps,
        not List.keymember?(specified_apps, app, 0),
        do: {app, default_mode(props)}
      )
  end

  defp default_mode(props) do
    if props[:included] == true, do: :load, else: :permanent
  end

  defp build_start_clean_boot(boot) do
    for({app, _mode} <- boot, do: {app, :none})
    |> Keyword.put(:stdlib, :permanent)
    |> Keyword.put(:kernel, :permanent)
  end

  defp validate_steps!(steps) do
    if not is_list(steps) or Enum.any?(steps, &(&1 != :assemble and not is_function(&1, 1))) do
      Mix.raise("""
        The :steps option must be a list of:

        * anonymous function that receives one argument
        * the atom :assemble

      Got: #{inspect(steps)}
      """)
    end

    if Enum.count(steps, &(&1 == :assemble)) != 1 do
      Mix.raise("The :steps option must contain the atom :assemble once, got: #{inspect(steps)}")
    end

    :ok
  end

  @doc """
  Makes the `sys.config` structure.

  If there are config providers, then a value is injected into
  the `:elixir` application configuration in `sys_config` to be
  read during boot and trigger the providers.

  It uses the following release options to customize its behaviour:

    * `:start_distribution_during_config`
    * `:prune_runtime_sys_config_after_boot`

  In case there are no config providers, it doesn't change `sys_config`.
  """
  @spec make_sys_config(t, keyword(), Config.Provider.config_path()) ::
          :ok | {:error, String.t()}
  def make_sys_config(release, sys_config, config_provider_path) do
    {sys_config, runtime?} = merge_provider_config(release, sys_config, config_provider_path)
    path = Path.join(release.version_path, "sys.config")

    args = [runtime?, sys_config]
    format = "%% coding: utf-8~n%% RUNTIME_CONFIG=~s~n~tw.~n"
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, :io_lib.format(format, args), [:utf8])

    case :file.consult(path) do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        {:error,
         "Could not read configuration file. It likely has invalid configuration terms " <>
           "such as functions, references, and pids. Please make sure your configuration " <>
           "is made of numbers, atoms, strings, maps, tuples and lists. Reason: #{inspect(reason)}"}
    end
  end

  defp merge_provider_config(%{config_providers: []}, sys_config, _), do: {sys_config, false}

  defp merge_provider_config(release, sys_config, config_path) do
    {extra_config, initial_config} = start_distribution(release)
    prune_after_boot = Keyword.get(release.options, :prune_runtime_sys_config_after_boot, false)
    opts = [extra_config: initial_config, prune_after_boot: prune_after_boot]
    init = Config.Provider.init(release.config_providers, config_path, opts)
    {Config.Reader.merge(sys_config, [elixir: [config_providers: init]] ++ extra_config), true}
  end

  defp start_distribution(%{options: opts}) do
    if Keyword.get(opts, :start_distribution_during_config, false) do
      {[], []}
    else
      {[kernel: [start_distribution: false]], [kernel: [start_distribution: true]]}
    end
  end

  @doc """
  Copies the cookie to the given path.

  If a cookie option was given, we compare it with
  the contents of the file (if any), and ask the user
  if they want to override.

  If there is no option, we generate a random one
  the first time.
  """
  @spec make_cookie(t, Path.t()) :: :ok
  def make_cookie(release, path) do
    cond do
      cookie = release.options[:cookie] ->
        Mix.Generator.create_file(path, cookie, quiet: true)
        :ok

      File.exists?(path) ->
        :ok

      true ->
        File.write!(path, random_cookie())
        :ok
    end
  end

  defp random_cookie, do: Base.url_encode64(:crypto.strong_rand_bytes(40))

  @doc """
  Makes the start_erl.data file with the
  ERTS version and release versions.
  """
  @spec make_start_erl(t, Path.t()) :: :ok
  def make_start_erl(release, path) do
    File.write!(path, "#{release.erts_version} #{release.version}")
    :ok
  end

  @doc """
  Makes boot scripts.

  It receives a path to the boot file, without extension, such as
  `releases/0.1.0/start` and this command will write `start.rel`,
  `start.boot`, and `start.script` to the given path, returning
  `{:ok, rel_path}` or `{:error, message}`.

  The boot script uses the RELEASE_LIB environment variable, which must
  be accordingly set with `--boot-var` and point to the release lib dir.
  """
  @spec make_boot_script(t, Path.t(), [{application(), mode()}], [String.t()]) ::
          :ok | {:error, String.t()}
  def make_boot_script(release, path, modes, prepend_paths \\ []) do
    with {:ok, rel_spec} <- build_release_spec(release, modes) do
      File.write!(path <> ".rel", consultable(rel_spec), [:utf8])

      sys_path = String.to_charlist(path)

      sys_options = [
        :silent,
        :no_dot_erlang,
        :no_warn_sasl,
        variables: build_variables(release),
        path: build_paths(release)
      ]

      case :systools.make_script(sys_path, sys_options) do
        {:ok, _module, _warnings} ->
          script_path = sys_path ++ '.script'
          {:ok, [{:script, rel_info, instructions}]} = :file.consult(script_path)

          instructions =
            instructions
            |> boot_config_provider()
            |> prepend_paths_to_script(prepend_paths)

          script = {:script, rel_info, instructions}
          File.write!(script_path, consultable(script), [:utf8])
          :ok = :systools.script2boot(sys_path)

        {:error, module, info} ->
          message = module.format_error(info) |> to_string() |> String.trim()
          {:error, message}
      end
    end
  end

  defp build_variables(release) do
    for {_, properties} <- release.applications,
        not Keyword.fetch!(properties, :otp_app?),
        uniq: true,
        do: {'RELEASE_LIB', properties |> Keyword.fetch!(:path) |> :filename.dirname()}
  end

  defp build_paths(release) do
    for {_, properties} <- release.applications,
        Keyword.fetch!(properties, :otp_app?),
        do: properties |> Keyword.fetch!(:path) |> Path.join("ebin") |> to_charlist()
  end

  defp build_release_spec(release, modes) do
    %{name: name, version: version, erts_version: erts_version, applications: apps} = release

    rel_apps =
      for {app, mode} <- modes do
        properties = Map.get(apps, app) || throw({:error, "Unknown application #{inspect(app)}"})
        children = Keyword.get(properties, :applications, [])
        validate_mode!(app, mode, modes, children)
        build_app_for_release(app, mode, properties)
      end

    {:ok, {:release, {to_charlist(name), to_charlist(version)}, {:erts, erts_version}, rel_apps}}
  catch
    {:error, message} -> {:error, message}
  end

  defp validate_mode!(app, mode, modes, children) do
    safe_mode? = mode in @safe_modes

    if not safe_mode? and mode not in @unsafe_modes do
      throw(
        {:error,
         "Unknown mode #{inspect(mode)} for #{inspect(app)}. " <>
           "Valid modes are: #{inspect(@safe_modes ++ @unsafe_modes)}"}
      )
    end

    for child <- children do
      child_mode = Keyword.get(modes, child)

      cond do
        is_nil(child_mode) ->
          throw(
            {:error,
             "Application #{inspect(app)} is listed in the release boot, " <>
               "but it depends on #{inspect(child)}, which isn't"}
          )

        safe_mode? and child_mode in @unsafe_modes ->
          throw(
            {:error,
             """
             Application #{inspect(app)} has mode #{inspect(mode)} but it depends on \
             #{inspect(child)} which is set to #{inspect(child_mode)}. If you really want \
             to set such mode for #{inspect(child)} make sure that all applications that depend \
             on it are also set to :load or :none, otherwise your release will fail to boot
             """}
          )

        true ->
          :ok
      end
    end
  end

  defp build_app_for_release(app, mode, properties) do
    vsn = Keyword.fetch!(properties, :vsn)

    case Keyword.get(properties, :included_applications, []) do
      [] -> {app, vsn, mode}
      included_apps -> {app, vsn, mode, included_apps}
    end
  end

  defp boot_config_provider(instructions) do
    {pre, [stdlib | post]} =
      Enum.split_while(
        instructions,
        &(not match?({:apply, {:application, :start_boot, [:stdlib, _]}}, &1))
      )

    config_provider = {:apply, {Config.Provider, :boot, [:elixir, :config_providers]}}
    pre ++ [stdlib, config_provider | post]
  end

  defp prepend_paths_to_script(instructions, []), do: instructions

  defp prepend_paths_to_script(instructions, prepend_paths) do
    prepend_paths = Enum.map(prepend_paths, &String.to_charlist/1)

    Enum.map(instructions, fn
      {:path, paths} ->
        if Enum.any?(paths, &List.starts_with?(&1, '$RELEASE_LIB')) do
          {:path, prepend_paths ++ paths}
        else
          {:path, paths}
        end

      other ->
        other
    end)
  end

  defp consultable(term) do
    :io_lib.format("%% coding: utf-8~n~tp.~n", [term])
  end

  @doc """
  Copies ERTS if the release is configured to do so.

  Returns true if the release was copied, false otherwise.
  """
  @spec copy_erts(t) :: boolean()
  def copy_erts(%{erts_source: nil}) do
    false
  end

  def copy_erts(release) do
    destination = Path.join(release.path, "erts-#{release.erts_version}")
    File.mkdir_p!(destination)
    File.cp_r!(release.erts_source, destination, fn _, _ -> false end)

    _ = File.rm(Path.join(destination, "bin/erl"))
    _ = File.rm(Path.join(destination, "bin/erl.ini"))

    destination
    |> Path.join("bin/erl")
    |> File.write!(~S"""
    #!/bin/sh
    SELF=$(readlink "$0" || true)
    if [ -z "$SELF" ]; then SELF="$0"; fi
    BINDIR="$(cd "$(dirname "$SELF")" && pwd -P)"
    ROOTDIR="$(dirname "$(dirname "$BINDIR")")"
    EMU=beam
    PROGNAME=$(echo "$0" | sed 's/.*\///')
    export EMU
    export ROOTDIR
    export BINDIR
    export PROGNAME
    exec "$BINDIR/erlexec" ${1+"$@"}
    """)

    File.chmod!(Path.join(destination, "bin/erl"), 0o744)
    true
  end

  @doc """
  Copies the given application specification into the release.

  It assumes the application exists in the release.
  """
  @spec copy_app(t, application) :: boolean()
  def copy_app(release, app) do
    properties = Map.fetch!(release.applications, app)
    vsn = Keyword.fetch!(properties, :vsn)

    source_app = Keyword.fetch!(properties, :path)
    target_app = Path.join([release.path, "lib", "#{app}-#{vsn}"])

    if is_nil(release.erts_source) and Keyword.fetch!(properties, :otp_app?) do
      false
    else
      File.rm_rf!(target_app)
      File.mkdir_p!(target_app)

      copy_ebin(release, Path.join(source_app, "ebin"), Path.join(target_app, "ebin"))

      for dir <- @copy_app_dirs do
        source_dir = Path.join(source_app, dir)
        target_dir = Path.join(target_app, dir)

        source_dir =
          case File.read_link(source_dir) do
            {:ok, link_target} -> Path.expand(link_target, source_app)
            _ -> source_dir
          end

        File.exists?(source_dir) && File.cp_r!(source_dir, target_dir)
      end

      true
    end
  end

  @doc """
  Copies the ebin directory at `source` to `target`
  respecting release options such a `:strip_beams`.
  """
  @spec copy_ebin(t, Path.t(), Path.t()) :: boolean()
  def copy_ebin(release, source, target) do
    with {:ok, [_ | _] = files} <- File.ls(source) do
      File.mkdir_p!(target)
      strip_beams? = Keyword.get(release.options, :strip_beams, true)

      for file <- files do
        source_file = Path.join(source, file)
        target_file = Path.join(target, file)

        with true <- strip_beams? and String.ends_with?(file, ".beam"),
             {:ok, binary} <- strip_beam(File.read!(source_file)) do
          File.write!(target_file, binary)
        else
          _ -> File.copy(source_file, target_file)
        end
      end

      true
    else
      _ -> false
    end
  end

  @doc """
  Strips a beam file for a release.

  This keeps only significant chunks necessary for the VM operation,
  discarding documentation, debug info, compile information and others.

  The exact chunks that are kept are not documented and may change in
  future versions.
  """
  @spec strip_beam(binary()) :: {:ok, binary} | {:error, :beam_lib, :beam_lib.chnk_rsn()}
  def strip_beam(binary) do
    case :beam_lib.chunks(binary, @significant_chunks, [:allow_missing_chunks]) do
      {:ok, {_, chunks}} ->
        chunks = for {name, chunk} <- chunks, is_binary(chunk), do: {name, chunk}
        {:ok, binary} = :beam_lib.build_module(chunks)
        {:ok, fd} = :ram_file.open(binary, [:write, :binary])
        {:ok, _} = :ram_file.compress(fd)
        {:ok, binary} = :ram_file.get_file(fd)
        :ok = :ram_file.close(fd)
        {:ok, binary}

      {:error, _, _} = error ->
        error
    end
  end
end
