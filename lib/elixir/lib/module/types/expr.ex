defmodule Module.Types.Expr do
  @moduledoc false

  def of_expr(_expr, _stack, context) do
    {:ok, :dynamic, context}
  end
end
