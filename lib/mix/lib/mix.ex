defmodule Mix do
  @doc """
  Starts the mix application and its dependencies.
  """
  def start do
    Enum.each [:elixir, :mix], :application.start(&1)
    :application.set_env(:mix, :project, [])
  end

  @doc """
  Loads the mixfile according to the given args and
  return a new args version that does not include
  the mixfile setting params. This is invoked from
  the command line and should not be invoked directly.
  """
  def load(args // System.argv) do
    file = "mix.exs"

    if File.regular?(file) do
      Code.require_file file
      unless project do
        raise "Mix.Project not specified in #{file} file"
      end
    else
      raise "Could not find #{file} file in the current directory: #{File.cwd!}"
    end

    args
  end

  @doc """
  Runs Mix according to the given arguments. This function
  is the main entry point for mix from the command line and
  should not be invoked directly. In order to run tasks in
  your Mix code, check `Mix.Tasks.run/2` instead.
  """
  def run(args)

  def run([h|t]) do
    run_task h, t
  end

  def run([]) do
    run_task Mix.Mixfile.default_task(Mix.Mixfile.get_project), []
  end

  defp run_task(name, args) do
    try do
      Mix.Tasks.run!(name, args)
    rescue
      exception in [Mix.NoTaskError, Mix.InvalidTaskError] ->
        IO.puts :stderr, exception.message
    end
  end

  @doc """
  Push a project into the project stack. Only
  the top of the stack can be accessed.
  """
  def push_project(atom) when is_atom(atom) do
    { :ok, stack } = :application.get_env(:mix, :project)
    :application.set_env(:mix, :project, [atom|stack])
  end

  @doc """
  Pops a project from the stack.
  """
  def pop_project do
    { :ok, [_|stack] } = :application.get_env(:mix, :project)
    :application.set_env(:mix, :project, stack)
  end

  @doc """
  Retrieves the current project. You can always
  expect it to return a module.
  """
  def project do
    { :ok, [h|_] } = :application.get_env(:mix, :project)
    h
  end
end