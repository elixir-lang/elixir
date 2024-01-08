Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.HelpersTest do
  use ExUnit.Case, async: true
  import Module.Types.Helpers

  test "expr_to_string/1" do
    assert expr_to_string({1, 2}) == "{1, 2}"
    assert expr_to_string(quote(do: Foo.bar(arg))) == "Foo.bar(arg)"
    assert expr_to_string(quote(do: :erlang.band(a, b))) == "Bitwise.band(a, b)"
    assert expr_to_string(quote(do: :erlang.orelse(a, b))) == "a or b"
    assert expr_to_string(quote(do: :erlang."=:="(a, b))) == "a === b"
    assert expr_to_string(quote(do: :erlang.list_to_atom(a))) == "List.to_atom(a)"
    assert expr_to_string(quote(do: :maps.remove(a, b))) == "Map.delete(b, a)"
    assert expr_to_string(quote(do: :erlang.element(1, a))) == "elem(a, 0)"
    assert expr_to_string(quote(do: :erlang.element(:erlang.+(a, 1), b))) == "elem(b, a)"
  end
end
