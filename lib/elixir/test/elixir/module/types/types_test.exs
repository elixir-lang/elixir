Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.TypesTest do
  use ExUnit.Case, async: true
  alias Module.Types

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
end
