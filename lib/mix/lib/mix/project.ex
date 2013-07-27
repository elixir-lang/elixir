defmodule Mix.Project do
  @moduledoc """
  A module that provides conveniences for defining and working
  with projects.

  ## Examples

  In order to configure Mix, a developer needs to use
  `Mix.Project` in a module and define a function named
  `project` that returns a keyword list with configuration.

      defmodule MyApp do
        use Mix.Project

        def project do
          [
            app: :my_app,
            vsn: "0.6.0"
          ]
        end
      end

  After being defined, the configuration for this project can be read
  as `Mix.project/0`. Notice that `config/0` won't fail if a
  project is not defined; this allows many mix tasks to work
  even without a project.

  In case the developer needs a project or wants to access a special
  function in the project, he can call `Mix.Project.get!/0`
  which fails with `Mix.NoProjectError` in case a project is not
  defined.
  """

  alias Mix.Server.Project

  @doc false
  defmacro __using__(_) do
    quote do
      @after_compile Mix.Project
    end
  end

  # Invoked after each Mix.Project is compiled.
  @doc false
  def __after_compile__(env, _binary) do
    push env.module
  end

  # Push a project onto the project stack. Only
  # the top of the stack can be accessed.
  @doc false
  def push(atom) when is_atom(atom) do
    config = Keyword.merge default_config, get_project_config(atom)

    Mix.Server.cast({ :push_project, atom, config })
  end

  # Pops a project from the stack.
  @doc false
  def pop do
    Mix.Server.call(:pop_project)
  end

  # Registers post config.
  @doc false
  def post_config(config) do
    Mix.Server.cast({ :post_config, config })
  end

  @doc """
  Retrieves the current project, `nil` if there is no
  current project (i.e. there is no mixfile in the current
  project).

  If you expect a project to be defined, i.e. it is a
  requirement of the current task, you should call
  `get!/0` instead.
  """
  def get do
    case Mix.Server.call(:projects) do
      [Project[name: project]|_] -> project
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
  Returns the project configuration for the current environment.
  """
  def config do
    case Mix.Server.call(:projects) do
      [Project[name: name, config: config]|_] when name != nil -> config
      _ -> default_config
    end
  end

  @doc """
  Returns a list of project configuration files, for example,
  `mix.exs` and `mix.lock`. This function is usually used
  in compilation tasks to trigger a full recompilation
  whenever such configuration files change.
  """
  def config_files do
    opts     = []
    project  = get
    lockfile = config[:lockfile]

    if File.regular?(lockfile) do
      opts = [lockfile|opts]
    end

    if project && (source = project.__info__(:compile)[:source]) do
      opts = [:unicode.characters_to_binary(source)|opts]
    end

    opts
  end

  @doc """
  Returns `true` if project is an umbrella project.
  """
  def umbrella? do
    config[:apps_path] != nil
  end

  @doc """
  Runs `fun` in the current project.

  The goal of this function is to transparently abstract umbrella projects.
  So if you want to gather data from a project, like beam files, you can
  use this function to transparently go through the project, regardless
  if it is an umbrella project or not.
  """
  def recur(post_config // [], fun) do
    if apps_path = config[:apps_path] do
      paths = Path.wildcard(Path.join(apps_path, "*"))
      paths = Enum.filter(paths, File.dir?(&1))

      projects = Enum.map paths, fn path ->
        dir = Path.basename(path)
        app = dir |> String.downcase |> binary_to_atom
        { app, path }
      end

      projects = topsort_projects(projects, Path.expand(apps_path))

      results = Enum.map projects, fn { app, app_path } ->
        in_project(app, app_path, post_config, fun)
      end

      results
    else
      # Note that post_config isnt used for this case
      [fun.(get)]
    end
  end

  @doc """
  Runs the given `fun` inside the given project by changing
  the current working directory and loading the given project
  onto the project stack.
  """
  def in_project(app, app_path, post_config // [], fun)

  def in_project(app, ".", post_config, fun) do
    cached = load_project(app, post_config)
    result = try do
      fun.(cached)
    after
      Mix.Project.pop
    end
    result
  end

  def in_project(app, app_path, post_config, fun) do
    File.cd! app_path, fn ->
      in_project(app, ".", post_config, fun)
    end
  end

  @doc """
  Returns the paths this project compiles to,
  collecting all `:compile_path` in case of umbrella apps.
  """
  def compile_paths do
    recur(fn _ -> Path.expand config[:compile_path] end)
  end

  @doc """
  Returns all load paths for this project,
  collecting all `:load_paths` in case of umbrella apps.
  """
  def load_paths do
    paths =
      recur(fn _ ->
        Enum.map(config[:load_paths], Path.expand(&1))
      end) |> List.concat
    paths ++ compile_paths
  end

  # Loads mix.exs in the current directory or loads the project from the
  # mixfile cache and pushes the project to the project stack.
  defp load_project(app, post_config) do
    if cached = Mix.Server.call({ :mixfile_cache, app }) do
      post_config(post_config)
      push(cached)
      cached
    else
      old_proj = get

      if File.regular?("mix.exs") do
        post_config(post_config)
        Code.load_file "mix.exs"
      end

      new_proj = get

      if old_proj == new_proj do
        new_proj = nil
        push new_proj
      end

      Mix.Server.cast({ :mixfile_cache, app, new_proj })
      new_proj
    end
  end

  # Sort projects in dependency order
  defp topsort_projects(projects, apps_path) do
    graph = :digraph.new

    Enum.each projects, fn { app, app_path } ->
      :digraph.add_vertex(graph, app, app_path)
    end

    Enum.each projects, fn { app, app_path } ->
      in_project app, app_path, fn _ ->
        Enum.each Mix.Deps.children, fn dep ->
          if Mix.Deps.available?(dep) and Mix.Deps.in_umbrella?(dep, apps_path) do
            :digraph.add_edge(graph, dep.app, app)
          end
        end
      end
    end

    unless :digraph_utils.is_acyclic(graph) do
      raise Mix.Error, message: "Could not dependency sort umbrella projects. " <>
        "There are cycles in the dependency graph."
    end

    vertices = :digraph_utils.topsort(graph)
    projects = Enum.map vertices, fn app ->
      :digraph.vertex(graph, app)
    end
    :digraph.delete(graph)
    projects
  end

  defp default_config do
    [ compile_path: "ebin",
      default_task: "run",
      deps: [],
      deps_path: "deps",
      elixirc_exts: [:ex],
      elixirc_paths: ["lib"],
      elixirc_watch_exts: [:ex, :eex, :exs],
      erlc_paths: ["src"],
      erlc_include_path: "include",
      erlc_options: [:debug_info],
      load_paths: [],
      lockfile: "mix.lock",
      preferred_cli_env: [{ "test", :test }] ]
  end

  defp get_project_config(nil), do: []

  defp get_project_config(atom) do
    config = atom.project

    if env = config[:env][Mix.env] do
      config |> Keyword.delete(:env) |> Keyword.merge(env)
    else
      config
    end
  end
end
