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
    * `:options` - a keyword list with all other user supplied release options

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
    :options
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
          options: keyword()
        }

  @default_apps %{elixir: :permanent, sasl: :permanent}
  @safe_modes [:permanent, :temporary, :transient]
  @unsafe_modes [:load, :none]
  @significant_chunks ~w(Atom AtU8 Attr Code StrT ImpT ExpT FunT LitT Line)c
  @copy_app_dirs ["priv"]

  @doc false
  @spec from_config!(atom, keyword, keyword) :: t
  def from_config!(name, config, overrides) do
    {name, apps, opts} = find_release(name, config)
    apps = Map.merge(@default_apps, apps)

    unless Atom.to_string(name) =~ Regex.recompile!(~r/^[a-z][a-z0-9_]*$/) do
      Mix.raise(
        "Invalid release name. A release name must start with a lowercase ASCII letter, " <>
          "followed by lowercase ASCII letters, numbers, or underscores, got: #{inspect(name)}"
      )
    end

    opts =
      [force: false, quiet: false, strip_beams: true]
      |> Keyword.merge(opts)
      |> Keyword.merge(overrides)

    {include_erts, opts} = Keyword.pop(opts, :include_erts, true)
    {erts_source, erts_version} = erts_data(include_erts)

    erts_lib_dir = :code.lib_dir()
    loaded_apps = apps |> Map.keys() |> load_apps(%{}, erts_lib_dir)

    # First we built the release without IEx, if IEx is included,
    # then use it as is, otherwise add it as none.
    {loaded_apps, apps} =
      if Map.has_key?(loaded_apps, :iex) do
        {loaded_apps, apps}
      else
        {load_apps([:iex], loaded_apps, erts_lib_dir), Map.put(apps, :iex, :none)}
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

    %Mix.Release{
      name: name,
      version: version,
      path: path,
      version_path: Path.join([path, "releases", version]),
      erts_source: erts_source,
      erts_version: erts_version,
      applications: loaded_apps,
      boot_scripts: %{start: start_boot, start_clean: start_clean_boot},
      options: opts
    }
  end

  defp find_release(name, config) do
    {name, opts} = lookup_release(name, config) || infer_release(config)
    {apps, opts} = Keyword.pop(opts, :applications, [])
    apps = Map.new(apps)

    if Mix.Project.umbrella?(config) do
      if apps == %{} do
        bad_umbrella!()
      end

      {name, apps, opts}
    else
      {name, Map.put_new(apps, Keyword.fetch!(config, :app), :permanent), opts}
    end
  end

  defp lookup_release(nil, config) do
    case Keyword.get(config, :releases, []) do
      [] ->
        nil

      [{name, opts}] ->
        {name, opts}

      [_ | _] ->
        Mix.raise(
          "\"mix release\" was invoked without a name but there are multiple releases. " <>
            "Please call \"mix release NAME\" or set :default_release in your project configuration"
        )
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
    {nil, :erlang.system_info(:version)}
  end

  defp erts_data(true) do
    version = :erlang.system_info(:version)
    {:filename.join(:code.root_dir(), 'erts-#{version}'), version}
  end

  defp erts_data(erts_source) when is_binary(erts_source) do
    if File.exists?(erts_source) do
      [_, erts_version] = erts_source |> Path.basename() |> String.split("-")
      {to_charlist(erts_source), to_charlist(erts_version)}
    else
      Mix.raise("Could not find ERTS system at #{inspect(erts_source)}")
    end
  end

  defp load_apps(apps, seen, otp_root) do
    for app <- apps,
        not Map.has_key?(seen, app),
        reduce: seen do
      seen -> load_app(app, seen, otp_root)
    end
  end

  defp load_app(app, seen, otp_root) do
    case :code.lib_dir(app) do
      {:error, :bad_name} ->
        Mix.raise("Could not find application #{inspect(app)}")

      path ->
        case :file.consult(Path.join(path, "ebin/#{app}.app")) do
          {:ok, terms} ->
            [{:application, ^app, properties}] = terms
            otp_app? = List.starts_with?(path, otp_root)
            seen = Map.put(seen, app, [path: path, otp_app?: otp_app?] ++ properties)
            load_apps(Keyword.get(properties, :applications, []), seen, otp_root)

          {:error, reason} ->
            Mix.raise("Could not load #{app}.app. Reason: #{inspect(reason)}")
        end
    end
  end

  defp build_start_boot(apps, modes) do
    for {app, _properties} <- apps, do: {app, Map.get(modes, app, :permanent)}
  end

  defp build_start_clean_boot(boot) do
    for({app, _mode} <- boot, do: {app, :none})
    |> Keyword.put(:kernel, :permanent)
    |> Keyword.put(:stdlib, :permanent)
  end

  @doc """
  Makes the `sys.config` structure.

  It receives the path to the directory where `sys.config`
  should be added to.
  """
  @spec make_sys_config(t, Path.t(), keyword()) :: :ok | {:error, String.t()}
  def make_sys_config(_release, path, sys_config) do
    File.write!(path, consultable("config", sys_config))

    case :file.consult(path) do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        {:error, "Could not read configuration file. Reason: #{inspect(reason)}"}
    end
  end

  @doc """
  Copies the cookie to the given path.

  If a cookie option was given, we compare it with
  the contents of the file (if any), and as the user
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
      File.write!(path <> ".rel", consultable("rel", rel_spec))

      sys_path = String.to_charlist(path)
      sys_options = [:silent, :no_dot_erlang, :no_warn_sasl, variables: build_variables(release)]

      case :systools.make_script(sys_path, sys_options) do
        {:ok, _module, _warnings} ->
          prepend_paths != [] && prepend_paths_to_script(sys_path, prepend_paths)
          :ok

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

  defp prepend_paths_to_script(sys_path, prepend_paths) do
    prepend_paths = Enum.map(prepend_paths, &String.to_charlist/1)
    script_path = sys_path ++ '.script'
    {:ok, [{:script, rel_info, instructions}]} = :file.consult(script_path)

    new_instructions =
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

    script = {:script, rel_info, new_instructions}
    File.write!(script_path, consultable("script", script))
    :ok = :systools.script2boot(sys_path)
  end

  defp consultable(kind, term) do
    {date, time} = :erlang.localtime()
    args = [kind, date, time, term]
    :io_lib.format("%% coding: utf-8~n%% ~ts generated at ~p ~p~n~p.~n", args)
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
