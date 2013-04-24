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
  function in the project, he can access `Mix.Project.get!/0`
  which fails with `Mix.NoProjectError` in case a project is not
  defined.
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
    push env.module
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
    Mix.Server.call(:pop_project)
  end

  # Registers post config.
  @doc false
  def post_config(config) do
    Mix.Server.cast({ :post_config, config })
  end

  @doc """
  Refresh the project configuration. Usually required
  when the environment changes during a task.
  """
  def refresh do
    push pop
  end

  @doc """
  Retrieves the current project.

  This is usually called by tasks that needs additional
  functions on the project to be defined. Since such
  tasks usually depends on a project to be defined, this
  function raises `Mix.NoProjectError` in case no project
  is available.

  Returns nil if no project.
  """
  def get do
    case Mix.Server.call(:projects) do
      [{ h, _ }|_] -> h
      _ -> nil
    end
  end

  @doc """
  Same as `get/0` but raises an exception if no project.
  """
  def get! do
    get || raise Mix.NoProjectError
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

  @doc """
  Returns a list of project config files (mix.exs and mix.lock).
  """
  def config_files do
    opts     = []
    project  = get
    lockfile = config[:lockfile]

    if File.regular?(lockfile) do
      opts = [lockfile|opts]
    end

    if project do
      opts = [Mix.Utils.source(project)|opts]
    end

    opts
  end

  defp default_config do
    [ compile_path: "ebin",
      default_env: [test: :test],
      default_task: "run",
      deps_path: "deps",
      elixirc_exts: [:ex],
      elixirc_paths: ["lib"],
      elixirc_watch_exts: [:ex, :eex, :exs],
      lockfile: "mix.lock",
      erlc_paths: ["src"],
      erlc_include_path: "include",
      erlc_options: [:debug_info] ]
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
