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
           vsn: "0.6.0"]
        end
      end

  After being defined, the configuration for this project can be read
  as `Mix.Project.config/0`. Notice that `config/0` won't fail if a
  project is not defined; this allows many mix tasks to work
  even without a project.

  In case the developer needs a project or wants to access a special
  function in the project, he/she can call `Mix.Project.get!/0`
  which fails with `Mix.NoProjectError` in case a project is not
  defined.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @after_compile Mix.Project
    end
  end

  @private_config [:build_path, :app_path]

  # Invoked after each Mix.Project is compiled.
  @doc false
  def __after_compile__(env, _binary) do
    push env.module, env.file
  end

  # Push a project onto the project stack.
  # Only the top of the stack can be accessed.
  @doc false
  def push(atom, file \\ nil) when is_atom(atom) do
    file = file ||
           (atom && String.from_char_data!(atom.__info__(:compile)[:source]))

    config = default_config
             |> Keyword.merge(get_project_config(atom))
             |> Keyword.drop(@private_config)

    case Mix.ProjectStack.push(atom, config, file) do
      :ok ->
        :ok
      {:error, other} when is_binary(other) ->
        raise Mix.Error, message: "Trying to load #{inspect atom} from #{inspect file}" <>
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
    [build_path: build_path(config),
     build_per_environment: config[:build_per_environment],
     deps_path: deps_path(config)]
  end

  @doc """
  Retrieves the current project if there is one.

  Otherwise `nil` is returned. It may happen in cases
  there is no mixfile in the current directory.

  If you expect a project to be defined, i.e. it is a
  requirement of the current task, you should call
  `get!/0` instead.
  """
  def get do
    case Mix.ProjectStack.peek do
      {name, _config, _file} -> name
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
  def get! do
    get || raise Mix.NoProjectError
  end

  @doc """
  Returns the project configuration.

  If there is no project defined, it still returns a keyword
  list with default values. This allows many mix tasks to work
  without the need for an underlying project.

  Note this configuration is cached once the project is
  pushed into the stack. Calling it multiple times won't
  cause it to be recomputed.

  Do not use `Mix.Project.config` to rely on runtime configuration.
  Use it only to configure aspects of your project (like
  compilation directories) and not your application runtime.
  """
  def config do
    case Mix.ProjectStack.peek do
      {_name, config, _file} -> config
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
  def config_files do
    [Mix.Dep.Lock.manifest] ++
      case Mix.ProjectStack.peek do
        {_name, config, file} ->
          configs = (config[:config_path] || "config/config.exs")
                    |> Path.dirname
                    |> Path.join("*.*")
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
  def umbrella? do
    config[:apps_path] != nil
  end

  @doc """
  Runs the given `fun` inside the given project.

  This function changes the current working directory and
  loads the project at the given directory onto the project
  stack.

  A `post_config` can be passed that will be merged into
  the project configuration.
  """
  def in_project(app, path, post_config \\ [], fun)

  def in_project(app, ".", post_config, fun) do
    cached = load_project(app, post_config)
    result = try do
      fun.(cached)
    after
      Mix.Project.pop
    end
    result
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
  def deps_path(config \\ config()) do
    Path.expand config[:deps_path]
  end

  @doc """
  Returns the build path for this project.

  The returned path will be expanded.

  ## Examples

      Mix.Project.build_path
      #=> "/path/to/project/_build/shared"

  If :build_per_environment is set to true (the default), it
  will create a new build per environment:

      Mix.env
      #=> :dev
      Mix.Project.build_path
      #=> "/path/to/project/_build/dev"

  """
  def build_path(config \\ config()) do
    config[:build_path] || if config[:build_per_environment] do
      Path.expand("_build/#{Mix.env}")
    else
      Path.expand("_build/shared")
    end
  end

  @doc """
  The path to store manifests.

  By default they are stored in the app path
  inside the build directory but it may be changed
  in future releases.

  The returned path will be expanded.

  ## Examples

      Mix.Project.manifest_path
      #=> "/path/to/project/_build/shared/lib/app"

  """
  def manifest_path(config \\ config()) do
    app_path(config)
  end

  @doc """
  Returns the application path inside the build.

  The returned path will be expanded.

  ## Examples

      Mix.Project.app_path
      #=> "/path/to/project/_build/shared/lib/app"

  """
  def app_path(config \\ config()) do
    config[:app_path] || cond do
      app = config[:app] ->
        Path.join([build_path(config), "lib", Atom.to_string(app)])
      config[:apps_path] ->
        raise "Trying to access app_path for an umbrella project but umbrellas have no app"
      true ->
        raise Mix.Error, message: "Cannot access build without an application name, " <>
          "please ensure you are in a directory with a mix.exs file and it defines " <>
          "an :app name under the project configuration"
    end
  end

  @doc """
  Returns the paths this project compiles to.

  The returned path will be expanded.

  ## Examples

      Mix.Project.compile_path
      #=> "/path/to/project/_build/shared/lib/app/priv"

  """
  def compile_path(config \\ config()) do
    Path.join(app_path(config), "ebin")
  end

  @doc """
  Builds the project structure for the current application.

  ## Options

  * `:symlink_ebin` - Symlink ebin instead of copying it

  """
  def build_structure(config \\ config(), opts \\ []) do
    app = app_path(config)
    File.mkdir_p!(app)

    source = Path.expand("ebin")
    target = Path.join(app, "ebin")

    cond do
      opts[:symlink_ebin] ->
        Mix.Utils.symlink_or_copy(source, target)
      match?({:ok, _}, :file.read_link(target)) ->
        File.rm_rf!(target)
        File.mkdir_p!(target)
      true ->
        File.mkdir_p!(target)
    end

    Mix.Utils.symlink_or_copy(Path.expand("include"), Path.join(app, "include"))
    Mix.Utils.symlink_or_copy(Path.expand("priv"), Path.join(app, "priv"))
  end

  @doc """
  Returns all load paths for this project.
  """
  def load_paths do
    if umbrella? do
      []
    else
      [compile_path]
    end
  end

  # Loads mix.exs in the current directory or loads the project from the
  # mixfile cache and pushes the project to the project stack.
  defp load_project(app, post_config) do
    Mix.ProjectStack.post_config(post_config)

    if cached = Mix.ProjectStack.read_cache(app) do
      {project, file} = cached
      push(project, file)
      project
    else
      file = Path.expand("mix.exs")
      old_proj = get

      if File.regular?(file) do
        Code.load_file(file)
      end

      new_proj = get

      if old_proj == new_proj do
        file = "nofile"
        new_proj = nil
        push new_proj, file
      end

      Mix.ProjectStack.write_cache(app, {new_proj, file})
      new_proj
    end
  end

  defp default_config do
    [build_per_environment: true,
     default_task: "run",
     deps: [],
     deps_path: "deps",
     elixirc_paths: ["lib"],
     erlc_paths: ["src"],
     erlc_include_path: "include",
     erlc_options: [:debug_info],
     lockfile: "mix.lock",
     preferred_cli_env: %{"test" => :test}]
  end

  defp get_project_config(nil),  do: []
  defp get_project_config(atom), do: atom.project
end
