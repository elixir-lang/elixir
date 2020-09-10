Code.require_file("../../test_helper.exs", __DIR__)

defmodule Module.Types.TypesTest do
  use ExUnit.Case, async: true
  alias Module.Types

  test "format_type/1" do
    assert Types.format_type(:binary, false) == "binary()"
    assert Types.format_type({:atom, true}, false) == "true"
    assert Types.format_type({:atom, :atom}, false) == ":atom"
    assert Types.format_type({:list, :binary}, false) == "[binary()]"
    assert Types.format_type({:tuple, []}, false) == "{}"
    assert Types.format_type({:tuple, [:integer]}, false) == "{integer()}"

    assert Types.format_type({:map, []}, true) == "map()"
    assert Types.format_type({:map, [{:required, {:atom, :foo}, :atom}]}, true) == "map()"

    assert Types.format_type({:map, []}, false) ==
             "%{}"

    assert Types.format_type({:map, [{:required, {:atom, :foo}, :atom}]}, false) ==
             "%{foo: atom()}"

    assert Types.format_type({:map, [{:required, :integer, :atom}]}, false) ==
             "%{integer() => atom()}"

    assert Types.format_type({:map, [{:optional, :integer, :atom}]}, false) ==
             "%{optional(integer()) => atom()}"

    assert Types.format_type({:map, [{:optional, {:atom, :foo}, :atom}]}, false) ==
             "%{optional(:foo) => atom()}"

    assert Types.format_type({:map, [{:required, {:atom, :__struct__}, {:atom, Struct}}]}, false) ==
             "%Struct{}"

    assert Types.format_type(
             {:map,
              [{:required, {:atom, :__struct__}, {:atom, Struct}}, {:required, :integer, :atom}]},
             false
           ) ==
             "%Struct{integer() => atom()}"
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
end
