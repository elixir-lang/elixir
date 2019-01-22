defmodule EEx.SmartEngine do
  @moduledoc """
  The default engine used by EEx.

  It includes assigns (like `@foo`) and possibly other
  conveniences in the future.

  ## Examples

      iex> EEx.eval_string("<%= @foo %>", assigns: [foo: 1])
      "1"

  In the example above, we can access the value `foo` under
  the binding `assigns` using `@foo`. This is useful because
  a template, after being compiled, can receive different
  assigns and would not require recompilation for each
  variable set.

  Assigns can also be used when compiled to a function:

      # sample.eex
      <%= @a + @b %>

      # sample.ex
      defmodule Sample do
        require EEx
        EEx.function_from_file(:def, :sample, "sample.eex", [:assigns])
      end

      # iex
      Sample.sample(a: 1, b: 2)
      #=> "3"

  """

  use EEx.Engine

  def handle_expr(buffer, mark, expr) do
    expr = Macro.prewalk(expr, &EEx.Engine.handle_assign/1)
    super(buffer, mark, expr)
  end
end
