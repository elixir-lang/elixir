defmodule Module.Types.Expr do
  @moduledoc false

  # import Module.Types.Helpers

  def of_expr(_expr, _stack, context) do
    {:ok, :dynamic, context}
  end
end
