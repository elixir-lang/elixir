defmodule Dialyzer.WithThrowingElse do
  def with_throwing_else(map) do
    with {:ok, foo} <- Map.fetch(map, :foo),
         false <- Enum.empty?(foo) do
      foo
    else
      # several clauses but one is a no_return
      :error -> throw(:empty_map)
      true -> nil
    end
  end
end
