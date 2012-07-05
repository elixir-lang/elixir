defmodule Tuple do
  @doc """
  Simply invokes the Access protocol for the given tuple.
  Check `Access.Tuple` for more information.
  """
  def access(tuple, access) when is_tuple(tuple) do
    Access.Tuple.access(tuple, access)
  end
end