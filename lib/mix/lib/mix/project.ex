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

  After defined, the configuration for this project can be read
  as `Mix.project/0`. Notice that config won't fail if a
  project is not defined, this allows many of mix tasks to work
  even without a project.

  In case the developer needs a project or want to access a special
  function in the project, he can access `Mix.Project.current/0`
  which fails with `Mix.NoProjectError` in case a project is not
  defined.
  """

  @doc false
  def behaviour_info(:callbacks) do
    [project: 0]
  end

  @doc false
  defmacro __using__(_) do
    quote do
      @after_compile Mix.Project
      @behaviour Mix.Project
    end
  end

  # Invoked after each Mix.Project is compiled.
  @doc false
  def after_compile(module, _binary) do
    push module
  end

  # Push a project into the project stack. Only
  # the top of the stack can be accessed.
  @doc false
  def push(atom) when is_atom(atom) do
    config = Keyword.merge default_config, get_project_config(atom)
    Mix.Server.cast({ :push_project, atom, config })
  end

  # Pops a project from the stack.
  @doc false
  def pop do
    Mix.Server.cast(:pop_project)
  end

  # Loads the mix.exs file in the current directory
  # and executes the given function. The project and
  # tasks stack are properly manipulated, no side-effects
  # should remain.
  @doc false
  def in_subproject(env, config, function) do
    old_env = Mix.env
    current = Mix.Project.current
    tasks   = Mix.Task.clear

    Mix.env(env)
    Mix.Server.cast({ :post_config, config })

    if File.regular?("mix.exs") do
      Code.load_file "mix.exs"
    end

    if current == Mix.Project.current do
      push nil
    end

    try do
      function.()
    after
      Mix.env(old_env)
      Mix.Project.pop
      Mix.Task.set_tasks(tasks)
    end
  end

  @doc """
  Retrieves the current project.

  This is usually called by tasks that needs additional
  functions on the project to be defined. Since such
  tasks usually depends on a project to be defined, this
  function raises `Mix.NoProjectError` in case no project
  is available.

  Use `defined?/0` if you need to check if a project is
  defined or not without raising an exception.
  """
  def current do
    case Mix.Server.call(:projects) do
      [{ h, _ }|_] when h != nil -> h
      _ -> raise Mix.NoProjectError
    end
  end

  @doc """
  Returns true if a current project is defined.
  """
  def defined? do
    case Mix.Server.call(:projects) do
      [{ h, _ }|_] when h != nil -> true
      _ -> false
    end
  end

  @doc """
  Returns the project configuration already
  considering the current environment.
  """
  def config do
    case Mix.Server.call(:projects) do
      [{ h, config }|_] when h != nil -> config
      _ -> default_config
    end
  end

  defp default_config do
    [ compile_path: "ebin",
      compile_first: [],
      compile_exts: [:ex, :eex],
      default_env: [test: :test],
      default_task: "test",
      deps_path: "deps",
      lockfile: "mix.lock",
      prepare_task: "compile",
      source_paths: ["lib"] ]
  end

  defp get_project_config(nil), do: []

  defp get_project_config(atom) do
    config = atom.project
    if env = config[:env][Mix.env] do
      config /> Keyword.delete(:env) /> Keyword.merge(env)
    else
      config
    end
  end
end
