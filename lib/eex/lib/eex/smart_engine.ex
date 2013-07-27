defmodule EEx.TransformerEngine do
  @moduledoc """
  An abstract engine that is meant to be used and
  built upon in other modules. This engine implements
  the `EEx.Engine` behavior and provides a `transform`
  overridable directive that allows a developer to
  customize the expression returned by the engine.

  Check `EEx.AssignsEngine` and `EEx.SmartEngine` for
  examples of using this module.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @behavior EEx.Engine

      def handle_text(buffer, text) do
        EEx.Engine.handle_text(buffer, text)
      end

      def handle_expr(buffer, mark, expr) do
        EEx.Engine.handle_expr(buffer, mark, transform(expr))
      end

      defp transform({ a, b, c }) do
        { transform(a), b, transform(c) }
      end

      defp transform({ a, b }) do
        { transform(a), transform(b) }
      end

      defp transform(list) when is_list(list) do
        lc i inlist list, do: transform(i)
      end

      defp transform(other) do
        other
      end

      defoverridable [transform: 1, handle_expr: 3, handle_text: 2]
    end
  end
end

defmodule EEx.AssignsEngine do
  @moduledoc """
  An abstract engine that, when used with the
  `TransformerEngine`, allows a developer to access
  assigns using `@` as syntax.

  This engine is included by default on the SmartEngine.

  ## Examples

      defmodule MyEngine do
        use EEx.TransformerEngine
        use EEx.AssignsEngine
      end

      EEx.eval_string("<%= @foo %>", assigns: [foo: 1])
      #=> 1

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

  @doc false
  defmacro __using__(_) do
    quote unquote: false do
      defp transform({ :@, line, [{ name, _, atom }] }) when is_atom(name) and is_atom(atom) do
        quote do: Keyword.get(var!(assigns), unquote(name))
      end

      defp transform(arg) do
        super(arg)
      end

      defoverridable [transform: 1]
    end
  end
end

defmodule EEx.SmartEngine do
  use EEx.TransformerEngine
  use EEx.AssignsEngine

  @moduledoc """
  An engine meant for end-user usage that includes
  `EEx.AssignsEngine` and other conveniences. Read
  `EEx.AssignsEngine` for examples.
  """
end
