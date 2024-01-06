Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.TypesTest do
  use ExUnit.Case, async: true
  alias Module.Types

  defmacro warning(patterns \\ [], guards \\ [], body) do
    min_line = min_line(patterns ++ guards ++ [body])
    patterns = reset_line(patterns, min_line)
    guards = reset_line(guards, min_line)
    body = reset_line(body, min_line)

    quote do
      unquote(TypeHelper.typecheck(patterns, guards, body, __CALLER__))
      |> Module.Types.TypesTest.__warning__()
    end
  end

  def __warning__(result) do
    context =
      case result do
        {:ok, _, context} -> context
        {:error, context} -> context
      end

    case context.warnings do
      [warning] -> to_message(warning)
      [] -> raise "no warnings"
      [_ | _] = warnings -> raise "too many warnings: #{inspect(warnings)}"
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

  defp to_message({module, warning, _location}) do
    warning
    |> module.format_warning()
    |> IO.iodata_to_binary()
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
    assert warning(URI.unknown("foo")) ==
             "URI.unknown/1 is undefined or private"

    assert warning(if(true, do: URI.unknown("foo"))) ==
             "URI.unknown/1 is undefined or private"

    assert warning(try(do: :ok, after: URI.unknown("foo"))) ==
             "URI.unknown/1 is undefined or private"
  end
end
