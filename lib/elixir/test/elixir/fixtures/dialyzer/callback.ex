defmodule Dialyzer.Callback do
  @callback required(atom) :: atom
  @callback required(list) :: list
end

defmodule Dialyzer.Callback.ImplAtom do
  @behaviour Dialyzer.Callback
  def required(:ok), do: :ok
end

defmodule Dialyzer.Callback.ImplList do
  @behaviour Dialyzer.Callback
  def required([a, b]), do: [b, a]
end
