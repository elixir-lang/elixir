defmodule Mix.Project do
  @moduledoc """
  Defines and manipulate Mix projects.

  In order to configure Mix, a developer needs to use
  `Mix.Project` in a module and define a function named
  `project` that returns a keyword list with configuration.

      defmodule MyApp do
        use Mix.Project

        def project do
          [app: :my_app,
           version: "0.6.0"]
        end
      end

  After being defined, the configuration for this project can be read
  as `Mix.Project.config/0`. Notice that `config/0` won't fail if a
  project is not defined; this allows many Mix tasks to work
  without a project.

  In case the developer needs a project or wants to access a special
  function in the project, the developer can call `Mix.Project.get!/0`
  which fails with `Mix.NoProjectError` in case a project is not
  defined.

  ## Erlang projects

  Mix can be used to manage Erlang projects that don't have any Elixir code. To
  ensure Mix tasks work correctly for an Erlang project, `language: :erlang`
  has to be added to `project`.

  The setting also makes sure Elixir is not added as a dependency to the
  generated .app file or to the escript generated with `mix escript.build`,
  etc.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @after_compile Mix.Project
    end
  end

  # Invoked after each Mix.Project is compiled.
  @doc false
  def __after_compile__(env, _binary) do
    push env.module, env.file
  end

  # Push a project onto the project stack.
  # Only the top of the stack can be accessed.
  @doc false
  def push(atom, file \\ nil, app \\ nil) when is_atom(atom) do
    file = file ||
           (atom && List.to_string(atom.__info__(:compile)[:source]))

    config = ([app: app] ++ default_config)
             |> Keyword.merge(get_project_config(atom))

    case Mix.ProjectStack.push(atom, config, file) do
      :ok ->
        :ok
      {:error, other} when is_binary(other) ->
        Mix.raise "Trying to load #{inspect atom} from #{inspect file}" <>
                  " but another project with the same name was already defined at #{inspect other}"
    end
  end

  # Pops a project from the stack.
  @doc false
  def pop do
    Mix.ProjectStack.pop
  end

  # The configuration that is pushed down to dependencies.
  @doc false
  def deps_config(config \\ config()) do
    [build_embedded: config[:build_embedded],
     build_per_environment: config[:build_per_environment],
     consolidate_protocols: false,
     deps_path: deps_path(config),
     env_path: build_path(config)]
  end

  @doc """
  Retrieves the current project if there is one.

  Otherwise `nil` is returned. It may happen in cases
  there is no mixfile in the current directory.

  If you expect a project to be defined, i.e. it is a
  requirement of the current task, you should call
  `get!/0` instead.
  """
  @spec get() :: module | nil
  def get do
    case Mix.ProjectStack.peek do
      %{name: name} -> name
      _ -> nil
    end
  end

  @doc """
  Same as `get/0`, but raises an exception if there is no current project.

  This is usually called by tasks that need additional
  functions on the project to be defined. Since such
  tasks usually depend on a project being defined, this
  function raises `Mix.NoProjectError` in case no project
  is available.
  """
  @spec get!() :: module | no_return
  def get! do
    get || Mix.raise Mix.NoProjectError, []
  end

  @doc """
  Returns the project configuration.

  If there is no project defined, it still returns a keyword
  list with default values. This allows many Mix tasks to work
  without the need for an underlying project.

  Note this configuration is cached once the project is
  pushed into the stack. Calling it multiple times won't
  cause it to be recomputed.

  Do not use `Mix.Project.config/0` to rely on runtime configuration.
  Use it only to configure aspects of your project (like
  compilation directories) and not your application runtime.
  """
  @spec config() :: Keyword.t
  def config do
    case Mix.ProjectStack.peek do
      %{config: config} -> config
      _ -> default_config
    end
  end

  @doc """
  Returns a list of project configuration files for this project.

  This function is usually used in compilation tasks to trigger
  a full recompilation whenever such configuration files change.

  By default it includes the mix.exs file, the lock manifest and
  all config files in the `config` directory.
  """
  @spec config_files() :: [Path.t]
  def config_files do
    [Mix.Dep.Lock.manifest] ++
      case Mix.ProjectStack.peek do
        %{config: config, file: file} ->
          configs =
            config[:config_path]
            |> Path.dirname
            |> Path.join("**/*.*")
            |> Path.wildcard
            |> Enum.reject(&String.starts_with?(Path.basename(&1), "."))
          [file|configs]
        _ ->
          []
      end
  end

  @doc """
  Returns `true` if project is an umbrella project.
  """
  @spec umbrella?() :: boolean
  def umbrella?(config \\ config()) do
    config[:apps_path] != nil
  end

  @doc ~S"""
  Runs the given `fun` inside the given project.

  This function changes the current working directory and
  loads the project at the given directory onto the project
  stack.

  A `post_config` can be passed that will be merged into
  the project configuration.

  `fun` is called with the `Mixfile` of the given project as
  its argument. The return value of this function is the return
  value of `fun`.

  ## Examples

      Mix.Project.in_project :my_app, "/path/to/my_app", fn mixfile ->
        "Mixfile is: #{inspect mixfile}"
      end
      #=> "Mixfile is: MyApp.Mixfile"

  """
  @spec in_project(atom, Path.t, Keyword.t, (module -> result)) :: result when result: term
  def in_project(app, path, post_config \\ [], fun)

  def in_project(app, ".", post_config, fun) do
    cached = try do
      load_project(app, post_config)
    rescue
      any ->
        Mix.shell.error "Error while loading project #{inspect app} at #{File.cwd!}"
        reraise any, System.stacktrace
    end

    try do
      fun.(cached)
    after
      Mix.Project.pop
    end
  end

  def in_project(app, path, post_config, fun) do
    File.cd! path, fn ->
      in_project(app, ".", post_config, fun)
    end
  end

  @doc """
  Returns the path to store dependencies for this project.

  The returned path will be expanded.

  ## Examples

      Mix.Project.deps_path
      #=> "/path/to/project/deps"

  """
  @spec deps_path(Keyword.t) :: Path.t
  def deps_path(config \\ config()) do
    Path.expand config[:deps_path]
  end

  @doc """
  Returns the full path of all dependencies as a map.

  ## Examples

      Mix.Project.deps_paths
      #=> %{foo: "deps/foo", bar: "custom/path/dep"}

  """
  @spec deps_paths() :: %{atom => Path.t}
  def deps_paths do
    Enum.reduce Mix.Dep.loaded(env: Mix.env), %{}, fn
      %{app: app, opts: opts}, acc -> Map.put acc, app, opts[:dest]
    end
  end

  @doc """
  Returns the build path for this project.

  The returned path will be expanded.

  ## Examples

      Mix.Project.build_path
      #=> "/path/to/project/_build/shared"

  If :build_per_environment is set to `true` (the default), it
  will create a new build per environment:

      Mix.env
      #=> :dev
      Mix.Project.build_path
      #=> "/path/to/project/_build/dev"

  """
  @spec build_path(Keyword.t) :: Path.t
  def build_path(config \\ config()) do
    config[:env_path] || env_path(config)
  end

  defp env_path(config) do
    build = config[:build_path] || "_build"

    if config[:build_per_environment] do
      Path.expand("#{build}/#{Mix.env}")
    else
      Path.expand("#{build}/shared")
    end
  end

  @doc """
  The path to store manifests.

  By default they are stored in the app path inside
  the build directory. Umbrella applications have
  the manifest path set to the root of the build directory.
  Directories may be changed in future releases.

  The returned path will be expanded.

  ## Examples

      Mix.Project.manifest_path
      #=> "/path/to/project/_build/shared/lib/app"

  """
  @spec manifest_path(Keyword.t) :: Path.t
  def manifest_path(config \\ config()) do
    config[:app_path] ||
      if app = config[:app] do
        Path.join([build_path(config), "lib", Atom.to_string(app)])
      else
        build_path(config)
      end
  end

  @doc """
  Returns the application path inside the build.

  The returned path will be expanded.

  ## Examples

      Mix.Project.app_path
      #=> "/path/to/project/_build/shared/lib/app"

  """
  @spec app_path(Keyword.t) :: Path.t
  def app_path(config \\ config()) do
    config[:app_path] || cond do
      app = config[:app] ->
        Path.join([build_path(config), "lib", Atom.to_string(app)])
      config[:apps_path] ->
        raise "Trying to access Mix.Project.app_path for an umbrella project but umbrellas have no app"
      true ->
        Mix.raise "Cannot access build without an application name, " <>
          "please ensure you are in a directory with a mix.exs file and it defines " <>
          "an :app name under the project configuration"
    end
  end

  @doc """
  Returns the paths this project compiles to.

  The returned path will be expanded.

  ## Examples

      Mix.Project.compile_path
      #=> "/path/to/project/_build/shared/lib/app/ebin"

  """
  @spec compile_path(Keyword.t) :: Path.t
  def compile_path(config \\ config()) do
    Path.join(app_path(config), "ebin")
  end

  @doc """
  Compiles the given project.

  It will run the compile task unless the project
  is in build embedded mode, which may fail as a
  explicit command to `mix compile` is required.
  """
  @spec compile([term], Keyword.t) :: term
  def compile(args, config \\ config()) do
    if config[:build_embedded] do
      path = if umbrella?(config), do: build_path(config), else: compile_path(config)

      unless File.exists?(path) do
        Mix.raise "Cannot execute task because the project was not yet compiled. " <>
                  "When build_embedded is set to true, \"MIX_ENV=#{Mix.env} mix compile\" " <>
                  "must be explicitly executed"
      end

      Mix.Task.run "loadpaths", args
    else
      Mix.Task.run "compile", args
    end
  end

  @doc """
  Builds the project structure for the current application.

  ## Options

    * `:symlink_ebin` - symlink ebin instead of copying it

  """
  @spec build_structure(Keyword.t, Keyword.t) :: :ok
  def build_structure(config \\ config(), opts \\ []) do
    app = app_path(config)
    File.mkdir_p!(app)

    source = Path.expand("ebin")
    target = Path.join(app, "ebin")

    _ = cond do
      opts[:symlink_ebin] ->
        _ = symlink_or_copy(config, source, target)
      match?({:ok, _}, :file.read_link(target)) ->
        _ = File.rm_rf!(target)
        File.mkdir_p!(target)
      true ->
        File.mkdir_p!(target)
    end

    _ = symlink_or_copy(config, Path.expand("include"), Path.join(app, "include"))
    _ = symlink_or_copy(config, Path.expand("priv"), Path.join(app, "priv"))
    :ok
  end

  defp symlink_or_copy(config, source, target) do
    if config[:build_embedded] do
      if File.exists?(source) do
        File.rm_rf!(target)
        File.cp_r!(source, target)
      end
    else
      Mix.Utils.symlink_or_copy(source, target)
    end
  end

  @doc """
  Ensures the project structure exists.

  In case it does exist, it is a no-op. Otherwise, it is built.
  """
  @spec ensure_structure(Keyword.t, Keyword.t) :: :ok
  def ensure_structure(config \\ config(), opts \\ []) do
    if File.exists?(app_path(config)) do
      :ok
    else
      build_structure(config, opts)
    end
  end

  @doc """
  Returns all load paths for this project.
  """
  @spec load_paths(Keyword.t) :: [Path.t]
  def load_paths(config \\ config()) do
    if umbrella?(config) do
      []
    else
      [compile_path(config)]
    end
  end

  # Loads mix.exs in the current directory or loads the project from the
  # mixfile cache and pushes the project to the project stack.
  defp load_project(app, post_config) do
    Mix.ProjectStack.post_config(post_config)

    if cached = Mix.ProjectStack.read_cache(app) do
      {project, file} = cached
      push(project, file, app)
      project
    else
      file = Path.expand("mix.exs")
      old_proj = get()

      {new_proj, file} =
        if File.regular?(file) do
          _ = Code.load_file(file)
          case get() do
            ^old_proj -> Mix.raise "Could not find a Mix project at #{file}"
            new_proj  -> {new_proj, file}
          end
        else
          push(nil, file, app)
          {nil, "nofile"}
        end

      Mix.ProjectStack.write_cache(app, {new_proj, file})
      new_proj
    end
  end

  defp default_config do
    [aliases: [],
     build_embedded: false,
     build_per_environment: true,
     build_scm: Mix.SCM.Path,
     config_path: "config/config.exs",
     consolidate_protocols: true,
     default_task: "run",
     deps: [],
     deps_path: "deps",
     elixirc_paths: ["lib"],
     erlc_paths: ["src"],
     erlc_include_path: "include",
     erlc_options: [:debug_info],
     lockfile: "mix.lock",
     preferred_cli_env: [],
     start_permanent: false]
  end

  @private_config [:app_path, :build_scm, :env_path]
  defp get_project_config(nil),  do: []
  defp get_project_config(atom), do: atom.project |> Keyword.drop(@private_config)
end
