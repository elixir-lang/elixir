defmodule IEx.Remsh do
  @moduledoc false

  @doc """
  Provides one helper function that is injected into connecting
  remote nodes to properly handle autocompletion. Elixir supports:

    * remsh from an elixir node to an elixir node
    * remsh from a plain erlang node to an elixir node (through the ^G menu)
    * remsh from an elixir node to a plain erlang node (and get an erl shell there)

  In order to get an Elixir shell from the ^G menu,
  you need to use 'Elixir.IEx' as the shell name (with quotes).

  Connecting an Elixir shell to a remote node without
  Elixir is **not** supported.
  """
  def expand(node) do
    fn e ->
      case :rpc.call node, IEx.Autocomplete, :expand, [e] do
        {:badrpc, _} -> {:no, '', []}
        r -> r
      end
    end
  end
end
