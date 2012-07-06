defmodule Mix do
  @doc """
  Starts the mix application and its dependencies.
  """
  def start do
    Enum.each [:elixir, :mix], :application.start(&1)
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
      :undefined -> nil
      { :ok, other } -> other
    end
  end
end