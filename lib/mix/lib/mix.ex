defmodule Mix do
  @doc """
  Starts the mix application and its dependencies.
  """
  def start do
    Enum.each [:elixir, :mix], :application.start(&1)
  end

  @doc """
  Runs Mix according to the given command line arguments.
  """
  def run(args // System.argv)

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
  Set the given module as the mixfile target.
  """
  def mixfile(atom) when is_atom(atom) do
    :application.set_env(:mix, :mixfile, atom)
  end

  @doc """
  Retrieves the current mixfile.
  """
  def mixfile do
    case :application.get_env(:mix, :mixfile) do
      { :ok, other } -> other
      :undefined     -> nil
    end
  end
end