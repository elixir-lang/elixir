defmodule Mix.Release do
  @moduledoc """
  Defines the release structure and convenience for assembling releases.
  """

  @doc """
  The Mix.Release struct has the following fields:

    * `:name` - the name of the release as an atom
    * `:version` - the version of the release as a string
    * `:path` - the path to the release root
    * `:version_path` - the path to the release version inside the release
    * `:applications` - a list of application release definitions
    * `:erts_source` - the erts source as a charlist (or nil)
    * `:erts_version` - the erts version as a charlist
    * `:config_source` - the path to the build configuration source (or nil)
    * `:consolidation_source` - the path to consolidated protocols source (or nil)
    * `:options` - a keyword list with all other user supplied release options

  """
  defstruct [
    :name,
    :version,
    :path,
    :version_path,
    :applications,
    :erts_source,
    :erts_version,
    :config_source,
    :consolidation_source,
    :options
  ]

  @type mode :: :permanent | :transient | :temporary | :load | :none
  @type application :: {atom(), charlist(), mode} | {atom(), charlist(), mode, [atom()]}
  @type t :: %{
          name: atom(),
          version: String.t(),
          path: String.t(),
          version_path: String.t(),
          applications: [application],
          erts_version: charlist(),
          erts_source: charlist() | nil,
          config_source: String.t() | nil,
          consolidation_source: String.t() | nil,
          options: keyword()
        }

  @default_apps %{iex: :permanent, elixir: :permanent, sasl: :permanent}
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

    rel_apps =
      apps
      |> Map.keys()
      |> load_apps(%{})
      |> app_to_rel(apps)
      |> Enum.sort()

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

    consolidation_source =
      if config[:consolidate_protocols] do
        Mix.Project.consolidation_path(config)
      end

    config_source =
      if File.regular?(config[:config_path]) do
        config[:config_path]
      end

    %Mix.Release{
      name: name,
      version: version,
      path: path,
      version_path: Path.join([path, "releases", version]),
      erts_source: erts_source,
      erts_version: erts_version,
      applications: rel_apps,
      consolidation_source: consolidation_source,
      config_source: config_source,
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
      Mix.raise("Unknown release #{inspect(name)}. Found: #{inspect(Keyword.keys(found))}")
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
    a non-empty applications key that choses which umbrella children \
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

  defp load_apps(apps, seen) do
    for app <- apps,
        not Map.has_key?(seen, app),
        reduce: seen do
      seen -> load_app(app, seen)
    end
  end

  defp load_app(app, seen) do
    case :file.consult(Application.app_dir(app, "ebin/#{app}.app")) do
      {:ok, terms} ->
        [{:application, ^app, properties}] = terms
        seen = Map.put(seen, app, properties)
        load_apps(Keyword.get(properties, :applications, []), seen)

      {:error, reason} ->
        Mix.raise("Could not load #{app}.app. Reason: #{inspect(reason)}")
    end
  end

  defp app_to_rel(apps, modes) do
    for {app, properties} <- apps do
      mode = Map.get(modes, app, :permanent)

      cond do
        mode in @safe_modes ->
          :ok

        mode in @unsafe_modes ->
          parent =
            apps
            |> depends_on(app)
            |> Enum.reverse()
            |> Enum.find(&(Map.get(modes, &1, :permanent) not in @unsafe_modes))

          if parent do
            Mix.raise("""
            Failed to assemble release because application #{inspect(app)} was set to \
            mode #{inspect(mode)} but the application #{inspect(parent)} depends on it \
            and it does not have its mode set to :load nor :none. If you really want \
            to set the mode for #{inspect(app)} to #{inspect(mode)}, make sure that all \
            applications that depend on it are also set to :load or :none
            """)
          end

        true ->
          Mix.raise(
            "Unknown mode #{inspect(mode)} for #{inspect(app)}. " <>
              "Valid modes are: #{inspect(@safe_modes ++ @unsafe_modes)}"
          )
      end

      build_app_for_release(app, mode, properties)
    end
  end

  defp depends_on(apps, pin) do
    children =
      for {app, properties} <- apps,
          children = Keyword.get(properties, :applications, []),
          pin in children,
          do: app

    Enum.flat_map(children, &depends_on(apps, &1)) ++ children
  end

  defp build_app_for_release(app, mode, properties) do
    vsn = Keyword.fetch!(properties, :vsn)

    case Keyword.get(properties, :included_applications, []) do
      [] -> {app, vsn, mode}
      included_apps -> {app, vsn, mode, included_apps}
    end
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
    File.cp_r!(release.erts_source, destination)

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

  It assumes the application exists.
  """
  @spec copy_app(t, application) :: boolean()
  def copy_app(release, app_spec) do
    app = elem(app_spec, 0)
    vsn = elem(app_spec, 1)

    source_app = Application.app_dir(app)
    target_app = Path.join([release.path, "lib", "#{app}-#{vsn}"])

    if skip_app?(release, source_app) do
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

  defp skip_app?(%{erts_source: nil}, source_app),
    do: String.starts_with?(source_app, List.to_string(:code.lib_dir()))

  defp skip_app?(_, _), do: false

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

  @doc """
  Copies the cookie to the given path.

  If a cookie option was given, we compare it with
  the contents of the file (if any), and as the user
  if they want to override.

  If there is no option, we generate a random one
  the first time.

  Returns true if the cookie was copied, false otherwise.
  """
  @spec copy_cookie(t, Path.t()) :: boolean()
  def copy_cookie(release, path) do
    cookie_path = Path.join(release.path, path)

    cond do
      cookie = release.options[:cookie] ->
        Mix.Generator.create_file(cookie_path, cookie, quiet: true)

      File.exists?(cookie_path) ->
        false

      true ->
        File.mkdir_p!(Path.dirname(cookie_path))
        File.write!(cookie_path, random_cookie())
        true
    end
  end

  defp random_cookie, do: Base.url_encode64(:crypto.strong_rand_bytes(40))

  @doc """
  Copies the start_erl.data file with the
  ERTS version and release versions.
  """
  @spec copy_start_erl(t, Path.t()) :: true
  def copy_start_erl(release, path) do
    start_erl_path = Path.join(release.path, path)
    File.mkdir_p!(Path.dirname(start_erl_path))
    File.write!(start_erl_path, "#{release.erts_version} #{release.version}")
    true
  end
end
