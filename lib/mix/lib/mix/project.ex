defmodule Mix.Project do
  @moduledoc """
  A module that provides conveniences for defining and working
  with projects.

  ## Examples

  In order to configure Mix, a developer needs to use
  `Mix.Project` in a module and define a function named
  `project` that returns a keywords list with configuration.

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

  def behaviour_info(:callbacks) do
    [project: 0]
  end

  @doc false
  defmacro __using__(_) do
    Mix.Project.push __CALLER__.module
    quote do
      @behavior Mix.Project
    end
  end

  # Push a project into the project stack. Only
  # the top of the stack can be accessed.
  @doc false
  def push(atom) when is_atom(atom) do
    Mix.Server.cast({ :push_project, atom })
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
  def in_subproject(function) do
    current = Mix.Project.current
    tasks   = Mix.Task.clear

    if File.regular?("mix.exs") do
      Code.load_file "mix.exs"
    end

    if current == Mix.Project.current do
      Mix.Project.push nil
    end

    try do
      function.()
    after
      Mix.Project.pop
      Mix.Task.set_tasks(tasks)
    end
  end

  @doc """
  Retrieves the current project, raises an error
  if there is no project set.
  """
  def current do
    case Mix.Server.call(:projects) do
      [h|_] when h != nil -> h
      _ -> raise Mix.NoProjectError
    end
  end

  @doc """
  Returns true if a current project is defined.
  """
  def defined? do
    case Mix.Server.call(:projects) do
      [h|_] when h != nil -> true
      _ -> false
    end
  end
end