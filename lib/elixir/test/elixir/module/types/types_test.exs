Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.TypesTest do
  use ExUnit.Case, async: true
  alias Module.Types
  alias Module.Types.{Pattern, Expr}

  defmacro warning(patterns \\ [], guards \\ [], body) do
    min_line = min_line(patterns ++ guards ++ [body])
    patterns = reset_line(patterns, min_line)
    guards = reset_line(guards, min_line)
    body = reset_line(body, min_line)
    expr = TypeHelper.expand_expr(patterns, guards, body, __CALLER__)

    quote do
      Module.Types.TypesTest.__expr__(unquote(Macro.escape(expr)))
    end
  end

  defmacro generated(ast) do
    Macro.prewalk(ast, fn node -> Macro.update_meta(node, &([generated: true] ++ &1)) end)
  end

  def __expr__({patterns, guards, body}) do
    with {:ok, _types, context} <-
           Pattern.of_head(patterns, guards, TypeHelper.new_stack(), TypeHelper.new_context()),
         {:ok, _type, context} <- Expr.of_expr(body, :dynamic, TypeHelper.new_stack(), context) do
      case context.warnings do
        [warning] -> to_message(:warning, warning)
        _ -> :none
      end
    else
      {:error, {type, reason, context}} ->
        to_message(:error, {type, reason, context})
    end
  end

  defp reset_line(ast, min_line) do
    Macro.prewalk(ast, fn ast ->
      Macro.update_meta(ast, fn meta ->
        Keyword.update!(meta, :line, &(&1 - min_line + 1))
      end)
    end)
  end

  defp min_line(ast) do
    {_ast, min} =
      Macro.prewalk(ast, :infinity, fn
        {_fun, meta, _args} = ast, min -> {ast, min(min, Keyword.get(meta, :line, 1))}
        other, min -> {other, min}
      end)

    min
  end

  defp to_message(:warning, {module, warning, _location}) do
    warning
    |> module.format_warning()
    |> IO.iodata_to_binary()
  end

  defp to_message(:error, {type, reason, context}) do
    {Module.Types, error, _location} = Module.Types.error_to_warning(type, reason, context)

    error
    |> Module.Types.format_warning()
    |> IO.iodata_to_binary()
    |> String.trim_trailing("\nConflict found at")
  end

  test "expr_to_string/1" do
    assert Types.expr_to_string({1, 2}) == "{1, 2}"
    assert Types.expr_to_string(quote(do: Foo.bar(arg))) == "Foo.bar(arg)"
    assert Types.expr_to_string(quote(do: :erlang.band(a, b))) == "Bitwise.band(a, b)"
    assert Types.expr_to_string(quote(do: :erlang.orelse(a, b))) == "a or b"
    assert Types.expr_to_string(quote(do: :erlang."=:="(a, b))) == "a === b"
    assert Types.expr_to_string(quote(do: :erlang.list_to_atom(a))) == "List.to_atom(a)"
    assert Types.expr_to_string(quote(do: :maps.remove(a, b))) == "Map.delete(b, a)"
    assert Types.expr_to_string(quote(do: :erlang.element(1, a))) == "elem(a, 0)"
    assert Types.expr_to_string(quote(do: :erlang.element(:erlang.+(a, 1), b))) == "elem(b, a)"
  end

  test "undefined function warnings" do
    assert warning([], URI.unknown("foo")) ==
             "URI.unknown/1 is undefined or private"

    assert warning([], if(true, do: URI.unknown("foo"))) ==
             "URI.unknown/1 is undefined or private"

    assert warning([], try(do: :ok, after: URI.unknown("foo"))) ==
             "URI.unknown/1 is undefined or private"
  end
end
