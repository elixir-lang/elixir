defmodule EEx.TransformerEngine do
  @moduledoc false

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour EEx.Engine

      def handle_body(body) do
        EEx.Engine.handle_body(body)
      end

      def handle_text(buffer, text) do
        EEx.Engine.handle_text(buffer, text)
      end

      def handle_expr(buffer, mark, expr) do
        EEx.Engine.handle_expr(buffer, mark, transform(expr))
      end

      defp transform({a, b, c}) do
        {transform(a), b, transform(c)}
      end

      defp transform({a, b}) do
        {transform(a), transform(b)}
      end

      defp transform(list) when is_list(list) do
        for i <- list, do: transform(i)
      end

      defp transform(other) do
        other
      end

      defoverridable [transform: 1, handle_body: 1, handle_expr: 3, handle_text: 2]
    end
  end
end

defmodule EEx.AssignsEngine do
  @moduledoc false

  @doc false
  defmacro __using__(_) do
    quote unquote: false do
      defp transform({:@, line, [{name, _, atom}]}) when is_atom(name) and is_atom(atom) do
        quote do: Dict.get(var!(assigns), unquote(name))
      end

      defp transform(arg) do
        super(arg)
      end

      defoverridable [transform: 1]
    end
  end
end

defmodule EEx.SmartEngine do
  @moduledoc """
  The default engine used by EEx.

  It includes assigns (like `@foo`) and possibly other
  conveniences in the future.

  ## Examples

      iex> EEx.eval_string("<%= @foo %>", assigns: [foo: 1])
      "1"

  In the example above, we can access the value `foo` under
  the binding `assigns` using `@foo`. This is useful when
  a template, after compiled, may receive different assigns
  and the developer don't want to recompile it for each
  variable set.

  Assigns can also be used when compiled to a function:

      # sample.eex
      <%= @a + @b %>

      # sample.ex
      defmodule Sample do
        require EEx
        EEx.function_from_file :def, :sample, "sample.eex", [:assigns]
      end

      # iex
      Sample.sample(a: 1, b: 2) #=> "3"

  """

  use EEx.Engine

  def handle_expr(buffer, mark, expr) do
    expr = Macro.prewalk(expr, &EEx.Engine.handle_assign/1)
    super(buffer, mark, expr)
  end
end
