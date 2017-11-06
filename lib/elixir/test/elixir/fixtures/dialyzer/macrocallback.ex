defmodule Dialyzer.Macrocallback do
  @macrocallback required(atom) :: Macro.t()
  @macrocallback optional(atom) :: Macro.t()
  @optional_callbacks [optional: 1]
end

defmodule Dialyzer.Macrocallback.Impl do
  @behaviour Dialyzer.Macrocallback
  defmacro required(var), do: Macro.expand(var, __CALLER__)
  defmacro optional(var), do: Macro.expand(var, __CALLER__)
end
