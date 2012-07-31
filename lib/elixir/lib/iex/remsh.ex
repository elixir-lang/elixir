defmodule IEx.Remsh do
  @moduledoc """
  Helper function injected into connecting remote nodes
  to properly handle autocompletion.
  """
  def expand(node) do
    fn e ->
      case :rpc.call node, Elixir.IEx.Autocomplete, :expand, [e] do
        {:badrpc, _} -> {:no, '', []}
        r -> r 
      end
    end
  end
end
