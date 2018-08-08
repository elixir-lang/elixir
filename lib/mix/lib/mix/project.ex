defmodule Mix.Project do
  @moduledoc """
  Defines and manipulates Mix projects.

  A Mix project is defined by calling `use Mix.Project` in a module, usually
  placed in `mix.exs`:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0"
          ]
        end
      end

  ## Configuration

  In order to configure Mix, the module that `use`s `Mix.Project` should export
  a `project/0` function that returns a keyword list representing configuration
  for the project.

  This configuration can be read using `Mix.Project.config/0`. Note that
  `config/0` won't fail if a project is not defined; this allows many Mix tasks
  to work without a project.

  If a task requires a project to be defined or needs to access a
  special function within the project, the task can call `Mix.Project.get!/0`
  which fails with `Mix.NoProjectError` in the case a project is not
  defined.

  There isn't a comprehensive list of all the options that can be returned by
  `project/0` since many Mix tasks define their own options that they read from
  this configuration. For example, look at the "Configuration" section in the
  documentation for the `Mix.Tasks.Compile` task.

  These are a few options that are not used by just one Mix task (and will thus
  be documented here):

    * `:build_per_environment` - if `true`, builds will be *per-environment*. If
      `false`, builds will go in `_build/shared` regardless of the Mix
      environment. Defaults to `true`.

    * `:aliases` - a list of task aliases. For more information, check out the
      "Aliases" section in the documentation for the `Mix` module. Defaults to
      `[]`.

    * `:config_path` - a string representing the path of the main config
      file. See `config_files/0` for more information. Defaults to
      `"config/config.exs"`.

    * `:default_task` - a string representing the default task to be run by
      `mix` when no task is specified. Defaults to `"run"`.

    * `:deps` - a list of dependencies of this project. Refer to the
      documentation for the `Mix.Tasks.Deps` task for more information. Defaults
      to `[]`.

    * `:deps_path` - directory where dependencies are stored. Also see
      `deps_path/1`. Defaults to `"deps"`.

    * `:lockfile` - the name of the lockfile used by the `mix deps.*` family of
      tasks. Defaults to `"mix.lock"`.

    * `:preferred_cli_env` - a keyword list of `{task, env}` tuples where `task`
      is the task name as an atom (for example, `:"deps.get"`) and `env` is the
      preferred environment (for example, `:test`). This option overrides what
      specified by the tasks with the `@preferred_cli_env` attribute (see the
      docs for `Mix.Task`). Defaults to `[]`.

  For more options, keep an eye on the documentation for single Mix tasks; good
  examples are the `Mix.Tasks.Compile` task and all the specific compiler tasks
  (such as `Mix.Tasks.Compile.Elixir` or `Mix.Tasks.Compile.Erlang`).

  Note that sometimes the same configuration option is mentioned in the
  documentation for different tasks; this is just because it's common for many
  tasks to read and use the same configuration option (for example,
  `:erlc_paths` is used by `mix compile.erlang`, `mix compile.yecc`, and other
  tasks).

  ## Erlang projects

  Mix can be used to manage Erlang projects that don't have any Elixir code. To
  ensure Mix tasks work correctly for an Erlang project, `language: :erlang` has
  to be part of the configuration returned by `project/0`. This setting also
  makes sure Elixir is not added as a dependency to the generated `.app` file or
  to the escript generated with `mix escript.build`, and so on.
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
    push(env.module, env.file)
  end

  # Push a project onto the project stack.
  # Only the top of the stack can be accessed.
  @doc false
  def push(atom, file \\ nil, app \\ nil) when is_atom(atom) do
    file = file || (atom && List.to_string(atom.__info__(:compile)[:source]))
    config = Keyword.merge([app: app] ++ default_config(), get_project_config(atom))

    case Mix.ProjectStack.push(atom, config, file) do
      :ok ->
        :ok

      {:error, other} when is_binary(other) ->
        Mix.raise(
          "Trying to load #{inspect(atom)} from #{inspect(file)}" <>
            " but another project with the same name was already defined at #{inspect(other)}"
        )
    end
  end

  # Pops a project from the stack.
  @doc false
  def pop do
    Mix.ProjectStack.pop()
  end

  # The configuration that is pushed down to dependencies.
  @doc false
  def deps_config(config \\ config()) do
    [
      build_embedded: config[:build_embedded],
      build_per_environment: config[:build_per_environment],
      consolidate_protocols: false,
      deps_path: deps_path(config),
      env_path: build_path(config)
    ]
  end

  @doc """
  Retrieves the current project if there is one.

  If there is no current project, `nil` is returned. This
  may happen in cases there is no `mix.exs` in the current
  directory.

  If you expect a project to be defined, i.e., it is a
  requirement of the current task, you should call
  `get!/0` instead.
  """
  @spec get() :: module | nil
  def get do
    case Mix.ProjectStack.peek() do
      %{name: name} -> name
      _ -> nil
    end
  end

  @doc """
  Same as `get/0`, but raises an exception if there is no current project.

  This is usually called by tasks that need additional
  functions on the project to be defined. Since such
  tasks usually depend on a project being defined, this
  function raises a `Mix.NoProjectError` exception in
  case no project is available.
  """
  @spec get!() :: module | no_return
  def get! do
    get() || raise Mix.NoProjectError, []
  end

  @doc """
  Returns the project configuration.

  If there is no project defined, it still returns a keyword
  list with default values. This allows many Mix tasks to work
  without the need for an underlying project.

  Note this configuration is cached once the project is
  pushed onto the stack. Calling it multiple times won't
  cause it to be recomputed.

  Do not use `Mix.Project.config/0` to find the runtime configuration.
  Use it only to configure aspects of your project (like
  compilation directories) and not your application runtime.
  """
  @spec config() :: keyword
  def config do
    case Mix.ProjectStack.peek() do
      %{config: config} -> config
      _ -> default_config()
    end
  end

  @doc """
  Returns a list of project configuration files for this project.

  This function is usually used in compilation tasks to trigger
  a full recompilation whenever such configuration files change.

  It returns the `mix.exs` file, the lock manifest, and all config
  files in the `config` directory that do not start with a trailing
  period (for example, `.my_config.exs`).
  """
  @spec config_files() :: [Path.t()]
  def config_files do
    [Mix.Dep.Lock.manifest() | Mix.ProjectStack.config_files()]
  end

  @doc """
  Returns the latest modification time from config files.

  This function is usually used in compilation tasks to trigger
  a full recompilation whenever such configuration files change.
  For this reason, the mtime is cached to avoid file system lookups.
  """
  @doc since: "1.7.0"
  @spec config_mtime() :: posix_mtime when posix_mtime: integer()
  def config_mtime do
    Mix.Dep.Lock.manifest()
    |> Mix.Utils.last_modified()
    |> max(Mix.ProjectStack.config_mtime())
  end

  @doc """
  Returns `true` if `config` is the configuration for an umbrella project.

  When called with no arguments, tells whether the current project is
  an umbrella project.
  """
  @spec umbrella?() :: boolean
  def umbrella?(config \\ config()) do
    config[:apps_path] != nil
  end

  @doc """
  Returns a map with the umbrella child applications paths.

  These paths are based on the `:apps_path` and `:apps` configurations.

  If the given project configuration identifies an umbrella project, the return
  value is a map of `app => path` where `app` is a child app of the umbrella and
  `path` is its path relative to the root of the umbrella project.

  If the given project configuration does not identify an umbrella project,
  `nil` is returned.

  ## Examples

      Mix.Project.apps_paths()
      #=> %{my_app1: "apps/my_app1", my_app2: "apps/my_app2"}

  """
  @doc since: "1.4.0"
  @spec apps_paths() :: %{optional(atom) => Path.t()} | nil
  def apps_paths(config \\ config()) do
    if apps_path = config[:apps_path] do
      key = {:apps_paths, Mix.Project.get!()}

      if cache = Mix.ProjectStack.read_cache(key) do
        cache
      else
        cache = config[:apps] |> umbrella_apps(apps_path) |> to_apps_paths(apps_path)
        Mix.ProjectStack.write_cache(key, cache)
      end
    end
  end

  defp umbrella_apps(nil, apps_path) do
    case File.ls(apps_path) do
      {:ok, apps} -> Enum.map(apps, &String.to_atom/1)
      {:error, _} -> []
    end
  end

  defp umbrella_apps(apps, _apps_path) when is_list(apps) do
    apps
  end

  defp to_apps_paths(apps, apps_path) do
    for app <- apps,
        path = path_with_mix_exs_otherwise_warn(app, apps_path),
        do: {app, path},
        into: %{}
  end

  defp path_with_mix_exs_otherwise_warn(app, apps_path) do
    path = Path.join(apps_path, Atom.to_string(app))

    cond do
      File.regular?(Path.join(path, "mix.exs")) ->
        path

      File.dir?(path) ->
        Mix.shell().error(
          "warning: path #{inspect(Path.relative_to_cwd(path))} is a directory but " <>
            "it has no mix.exs. Mix won't consider this directory as part of your " <>
            "umbrella application. Please add a \"mix.exs\" or set the \":apps\" key " <>
            "in your umbrella configuration with all relevant apps names as atoms"
        )

        nil

      true ->
        # If it is a stray file, we just ignore it.
        nil
    end
  end

  @doc ~S"""
  Runs the given `fun` inside the given project.

  This function changes the current working directory and
  loads the project at the given directory onto the project
  stack.

  A `post_config` can be passed that will be merged into
  the project configuration.

  `fun` is called with the module name of the given `Mix.Project`.
  The return value of this function is the return value of `fun`.

  ## Examples

      Mix.Project.in_project(:my_app, "/path/to/my_app", fn module ->
        "Mix project is: #{inspect module}"
      end)
      #=> "Mix project is: MyApp.MixProject"

  """
  @spec in_project(atom, Path.t(), keyword, (module -> result)) :: result when result: term
  def in_project(app, path, post_config \\ [], fun)

  def in_project(app, ".", post_config, fun) when is_atom(app) do
    cached =
      try do
        load_project(app, post_config)
      rescue
        any ->
          Mix.shell().error("Error while loading project #{inspect(app)} at #{File.cwd!()}")
          reraise any, __STACKTRACE__
      end

    try do
      fun.(cached)
    after
      Mix.Project.pop()
    end
  end

  def in_project(app, path, post_config, fun) when is_atom(app) do
    File.cd!(path, fn ->
      in_project(app, ".", post_config, fun)
    end)
  end

  @doc """
  Returns the path where dependencies are stored for the given project.

  If no configuration is given, the one for the current project is used.

  The returned path will be expanded.

  ## Examples

      Mix.Project.deps_path()
      #=> "/path/to/project/deps"

  """
  @spec deps_path(keyword) :: Path.t()
  def deps_path(config \\ config()) do
    Path.expand(config[:deps_path])
  end

  @doc """
  Returns the full path of all dependencies as a map.

  ## Options

    * `:depth` - only return dependencies to the depth level,
      a depth of 1 will only return top-level dependencies
    * `:parents` - starts the dependency traversal from the
      given parents instead of the application root

  ## Examples

      Mix.Project.deps_paths()
      #=> %{foo: "deps/foo", bar: "custom/path/dep"}

  """
  @spec deps_paths(keyword) :: %{optional(atom) => Path.t()}
  def deps_paths(opts \\ []) do
    all_deps = Mix.Dep.cached()
    parents = opts[:parents]
    depth = opts[:depth]

    if parents || depth do
      parent_filter = if parents, do: &(&1.app in parents), else: & &1.top_level

      all_deps
      |> Enum.filter(parent_filter)
      |> deps_to_paths_map()
      |> deps_paths_depth(all_deps, 1, depth || :infinity)
    else
      deps_to_paths_map(all_deps)
    end
  end

  defp deps_to_paths_map(deps) do
    for %{app: app, opts: opts} <- deps,
        do: {app, opts[:dest]},
        into: %{}
  end

  defp deps_paths_depth(deps, _all_deps, depth, depth) do
    deps
  end

  defp deps_paths_depth(parents, all_deps, depth, target_depth) do
    children =
      for parent_dep <- all_deps,
          Map.has_key?(parents, parent_dep.app),
          %{app: app, opts: opts} <- parent_dep.deps,
          do: {app, opts[:dest]},
          into: %{}

    case Map.merge(parents, children) do
      ^parents -> parents
      new_parents -> deps_paths_depth(new_parents, all_deps, depth + 1, target_depth)
    end
  end

  @doc """
  Clears the dependency for the current environment.

  Useful when dependencies need to be reloaded due to change of global state.
  """
  @doc since: "1.7.0"
  @spec clear_deps_cache() :: :ok
  def clear_deps_cache() do
    Mix.Dep.clear_cached()
    :ok
  end

  @doc """
  Returns the build path for the given project.

  If no configuration is given, the one for the current project is used.

  The returned path will be expanded.

  ## Examples

      Mix.Project.build_path()
      #=> "/path/to/project/_build/shared"

  If `:build_per_environment` is set to `true`, it will create a new build per
  environment:

      Mix.env()
      #=> :dev
      Mix.Project.build_path()
      #=> "/path/to/project/_build/dev"

  """
  @spec build_path(keyword) :: Path.t()
  def build_path(config \\ config()) do
    System.get_env("MIX_BUILD_PATH") || config[:env_path] || env_path(config)
  end

  defp env_path(config) do
    build = config[:build_path] || "_build"

    case config[:build_per_environment] do
      true ->
        Path.expand("#{build}/#{Mix.env()}")

      false ->
        Path.expand("#{build}/shared")

      other ->
        Mix.raise("The :build_per_environment option should be a boolean, got: #{inspect(other)}")
    end
  end

  @doc """
  Returns the path where manifests are stored.

  By default they are stored in the app path inside
  the build directory. Umbrella applications have
  the manifest path set to the root of the build directory.
  Directories may be changed in future releases.

  The returned path will be expanded.

  ## Examples

      Mix.Project.manifest_path()
      #=> "/path/to/project/_build/shared/lib/app/.mix"

  """
  @spec manifest_path(keyword) :: Path.t()
  def manifest_path(config \\ config()) do
    app_path =
      config[:app_path] ||
        if app = config[:app] do
          Path.join([build_path(config), "lib", Atom.to_string(app)])
        else
          build_path(config)
        end

    Path.join(app_path, ".mix")
  end

  @doc """
  Returns the application path inside the build.

  The returned path will be expanded.

  ## Examples

      Mix.Project.app_path()
      #=> "/path/to/project/_build/shared/lib/app"

  """
  @spec app_path(keyword) :: Path.t()
  def app_path(config \\ config()) do
    config[:app_path] ||
      cond do
        app = config[:app] ->
          Path.join([build_path(config), "lib", Atom.to_string(app)])

        config[:apps_path] ->
          raise "trying to access Mix.Project.app_path for an umbrella project but umbrellas have no app"

        true ->
          Mix.raise(
            "Cannot access build without an application name, " <>
              "please ensure you are in a directory with a mix.exs file and it defines " <>
              "an :app name under the project configuration"
          )
      end
  end

  @doc """
  Returns the paths the given project compiles to.

  If no configuration is given, the one for the current project will be used.

  The returned path will be expanded.

  ## Examples

      Mix.Project.compile_path()
      #=> "/path/to/project/_build/dev/lib/app/ebin"

  """
  @spec compile_path(keyword) :: Path.t()
  def compile_path(config \\ config()) do
    Path.join(app_path(config), "ebin")
  end

  @doc """
  Returns the path where protocol consolidations are stored.

  The returned path will be expanded.

  ## Examples

      Mix.Project.consolidation_path()
      #=> "/path/to/project/_build/dev/lib/my_app/consolidated"

  Inside umbrellas:

      Mix.Project.consolidation_path()
      #=> "/path/to/project/_build/dev/consolidated"

  """
  def consolidation_path(config \\ config()) do
    if umbrella?(config) do
      Path.join(build_path(config), "consolidated")
    else
      Path.join(app_path(config), "consolidated")
    end
  end

  @doc """
  Compiles the given project.
  """
  @spec compile([term], keyword) :: term
  def compile(args, _config \\ []) do
    Mix.Task.run("compile", args)
  end

  @doc """
  Builds the project structure for the given application.

  ## Options

    * `:symlink_ebin` - symlink ebin instead of copying it

  """
  @spec build_structure(keyword, keyword) :: :ok
  def build_structure(config \\ config(), opts \\ []) do
    app = app_path(config)
    File.mkdir_p!(app)

    source = Path.expand("ebin")
    target = Path.join(app, "ebin")

    _ =
      cond do
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
  Ensures the project structure for the given project exists.

  In case it does exist, it is a no-op. Otherwise, it is built.
  """
  @spec ensure_structure(keyword, keyword) :: :ok
  def ensure_structure(config \\ config(), opts \\ []) do
    if File.exists?(app_path(config)) do
      :ok
    else
      build_structure(config, opts)
    end
  end

  @doc """
  Returns all load paths for the given project.
  """
  @spec load_paths(keyword) :: [Path.t()]
  def load_paths(config \\ config()) do
    if umbrella?(config) do
      []
    else
      [compile_path(config)]
    end
  end

  # Loads mix.exs in the current directory or loads the project from the
  # mixfile cache and pushes the project onto the project stack.
  defp load_project(app, post_config) do
    Mix.ProjectStack.post_config(post_config)

    if cached = Mix.ProjectStack.read_cache({:app, app}) do
      {project, file} = cached
      push(project, file, app)
      project
    else
      file = Path.expand("mix.exs")
      old_proj = get()

      {new_proj, file} =
        if File.regular?(file) do
          try do
            Code.compiler_options(relative_paths: false)
            _ = Code.compile_file(file)
            get()
          else
            ^old_proj -> Mix.raise("Could not find a Mix project at #{file}")
            new_proj -> {new_proj, file}
          after
            Code.compiler_options(relative_paths: true)
          end
        else
          push(nil, file, app)
          {nil, "nofile"}
        end

      Mix.ProjectStack.write_cache({:app, app}, {new_proj, file})
      new_proj
    end
  end

  defp default_config do
    [
      aliases: [],
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
      start_permanent: false
    ]
  end

  @private_config [:app_path, :build_scm, :env_path]
  defp get_project_config(nil), do: []
  defp get_project_config(atom), do: atom.project |> Keyword.drop(@private_config)
end
