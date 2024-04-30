Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.DescrTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr

  describe "union" do
    test "bitmap" do
      assert union(integer(), float()) == union(float(), integer())
    end

    test "term" do
      assert union(term(), float()) == term()
      assert union(term(), binary()) == term()
    end

    test "none" do
      assert union(none(), float()) == float()
      assert union(none(), binary()) == binary()
    end

    test "atom" do
      assert union(atom(), atom([:a])) == atom()
      assert union(atom([:a]), atom([:b])) == atom([:a, :b])
      assert union(atom([:a]), negation(atom([:b]))) == negation(atom([:b]))

      assert union(negation(atom([:a, :b])), negation(atom([:b, :c])))
             |> equal?(negation(atom([:b])))
    end

    test "all primitive types" do
      all = [
        atom(),
        integer(),
        float(),
        binary(),
        open_map(),
        non_empty_list(),
        empty_list(),
        tuple(),
        fun(),
        pid(),
        port(),
        reference()
      ]

      assert Enum.reduce(all, &union/2) == term()
    end

    test "dynamic" do
      assert equal?(union(dynamic(), dynamic()), dynamic())
      assert equal?(union(dynamic(), term()), term())
      assert equal?(union(term(), dynamic()), term())
      assert equal?(union(intersection(dynamic(), atom()), atom()), atom())
    end

    test "map" do
      assert equal?(union(open_map(), open_map()), open_map())
      assert equal?(union(closed_map(a: integer()), open_map()), open_map())
      assert equal?(union(closed_map(a: integer()), negation(closed_map(a: integer()))), term())

      a_integer_open = open_map(a: integer())
      assert equal?(union(closed_map(a: integer()), a_integer_open), a_integer_open)

      assert difference(open_map(a: integer()), closed_map(b: boolean()))
             |> equal?(open_map(a: integer()))
    end
  end

  describe "intersection" do
    test "bitmap" do
      assert intersection(integer(), union(integer(), float())) == integer()
      assert intersection(integer(), float()) == none()
    end

    test "term" do
      assert intersection(term(), term()) == term()
      assert intersection(term(), float()) == float()
      assert intersection(term(), binary()) == binary()
    end

    test "none" do
      assert intersection(none(), float()) == none()
      assert intersection(none(), binary()) == none()
    end

    test "atom" do
      assert intersection(atom(), atom()) == atom()
      assert intersection(atom(), atom([:a])) == atom([:a])
      assert intersection(atom([:a]), atom([:b])) == none()
      assert intersection(atom([:a]), negation(atom([:b]))) == atom([:a])
    end

    test "dynamic" do
      assert equal?(intersection(dynamic(), dynamic()), dynamic())
      assert equal?(intersection(dynamic(), term()), dynamic())
      assert equal?(intersection(term(), dynamic()), dynamic())
      assert empty?(intersection(dynamic(), none()))
      assert empty?(intersection(intersection(dynamic(), atom()), integer()))
    end

    test "map" do
      assert intersection(open_map(), open_map()) == open_map()
      assert equal?(intersection(closed_map(a: integer()), open_map()), closed_map(a: integer()))

      assert equal?(
               intersection(closed_map(a: integer()), open_map(a: integer())),
               closed_map(a: integer())
             )

      assert equal?(
               intersection(closed_map(a: integer()), closed_map(a: if_set(integer()))),
               closed_map(a: integer())
             )

      assert empty?(intersection(closed_map(a: integer()), closed_map(a: atom())))
    end
  end

  describe "difference" do
    test "bitmap" do
      assert difference(float(), integer()) == float()
      assert difference(union(float(), integer()), integer()) == float()
      assert difference(union(float(), integer()), binary()) == union(float(), integer())
    end

    test "term" do
      assert difference(float(), term()) == none()
      assert difference(integer(), term()) == none()
    end

    test "none" do
      assert difference(none(), integer()) == none()
      assert difference(none(), float()) == none()

      assert difference(integer(), none()) == integer()
      assert difference(float(), none()) == float()
    end

    test "atom" do
      assert difference(atom([:a]), atom()) == none()
      assert difference(atom([:a]), atom([:b])) == atom([:a])
    end

    test "dynamic" do
      assert equal?(dynamic(), difference(dynamic(), dynamic()))
      assert equal?(dynamic(), difference(term(), dynamic()))
      assert empty?(difference(dynamic(), term()))
      assert empty?(difference(none(), dynamic()))
    end

    test "map" do
      assert empty?(difference(open_map(), open_map()))
      assert empty?(difference(open_map(), term()))
      assert equal?(difference(open_map(), none()), open_map())
      assert empty?(difference(closed_map(a: integer()), open_map()))
      assert empty?(difference(closed_map(a: integer()), closed_map(a: integer())))
      assert empty?(difference(closed_map(a: integer()), open_map(a: integer())))
      assert empty?(difference(closed_map(a: integer()), open_map(b: if_set(integer()))))

      assert difference(closed_map(a: integer(), b: if_set(atom())), closed_map(a: integer()))
             |> difference(closed_map(a: integer(), b: atom()))
             |> empty?()

      assert difference(open_map(a: atom()), closed_map(b: integer()))
             |> equal?(open_map(a: atom()))
    end
  end

  describe "subtype" do
    test "bitmap" do
      assert subtype?(integer(), union(integer(), float()))
      assert subtype?(integer(), integer())
      assert subtype?(integer(), term())
      assert subtype?(none(), integer())
      assert subtype?(integer(), negation(float()))
    end

    test "atom" do
      assert subtype?(atom([:a]), atom())
      assert subtype?(atom([:a]), atom([:a]))
      assert subtype?(atom([:a]), term())
      assert subtype?(none(), atom([:a]))
      assert subtype?(atom([:a]), atom([:a, :b]))
      assert subtype?(atom([:a]), negation(atom([:b])))
    end

    test "dynamic" do
      assert subtype?(dynamic(), term())
      assert subtype?(dynamic(), dynamic())
      refute subtype?(term(), dynamic())
      assert subtype?(intersection(dynamic(), integer()), integer())
      assert subtype?(integer(), union(dynamic(), integer()))
    end

    test "map" do
      assert subtype?(open_map(), term())
      assert subtype?(closed_map(a: integer()), open_map())
      assert subtype?(closed_map(a: integer()), closed_map(a: integer()))
      assert subtype?(closed_map(a: integer()), open_map(a: integer()))
      assert subtype?(closed_map(a: integer(), b: atom()), open_map(a: integer()))
      assert subtype?(closed_map(a: integer()), closed_map(a: union(integer(), atom())))

      # optional
      refute subtype?(closed_map(a: if_set(integer())), closed_map(a: integer()))
      assert subtype?(closed_map(a: integer()), closed_map(a: if_set(integer())))
    end
  end

  describe "compatible" do
    test "intersection" do
      assert compatible?(integer(), intersection(dynamic(), integer()))
      refute compatible?(intersection(dynamic(), integer()), atom())
      refute compatible?(atom(), intersection(dynamic(), integer()))
      refute compatible?(atom(), intersection(dynamic(), atom([:foo, :bar])))
      assert compatible?(intersection(dynamic(), atom()), atom([:foo, :bar]))
      assert compatible?(atom([:foo, :bar]), intersection(dynamic(), atom()))
    end

    test "static" do
      refute compatible?(atom(), atom([:foo, :bar]))
      refute compatible?(union(integer(), atom()), integer())
      refute compatible?(none(), integer())
      refute compatible?(union(atom(), dynamic()), integer())
    end

    test "dynamic" do
      assert compatible?(dynamic(), term())
      assert compatible?(term(), dynamic())
      assert compatible?(dynamic(), integer())
      assert compatible?(integer(), dynamic())
    end

    test "map" do
      assert compatible?(closed_map(a: integer()), open_map())
      assert compatible?(intersection(dynamic(), open_map()), closed_map(a: integer()))
    end
  end

  describe "empty" do
    test "map" do
      assert intersection(closed_map(b: atom()), open_map(a: integer())) |> empty?
    end
  end

  describe "projections" do
    test "atom_fetch" do
      assert atom_fetch(term()) == :error
      assert atom_fetch(union(term(), dynamic(atom([:foo, :bar])))) == :error

      assert atom_fetch(atom()) == {:infinite, []}

      assert atom_fetch(atom([:foo, :bar])) ==
               {:finite, [:foo, :bar] |> :sets.from_list(version: 2) |> :sets.to_list()}

      assert atom_fetch(union(atom([:foo, :bar]), dynamic(atom()))) == {:infinite, []}
      assert atom_fetch(union(atom([:foo, :bar]), dynamic(term()))) == {:infinite, []}
    end

    test "map_fetch" do
      assert map_fetch(closed_map(a: integer()), :a) == {false, integer()}

      assert map_fetch(term(), :a) == :error
      assert map_fetch(union(open_map(), integer()), :a) == :error
      assert map_fetch(dynamic(), :a) == {true, dynamic()}

      assert intersection(dynamic(), open_map(a: integer()))
             |> map_fetch(:a) == {false, intersection(integer(), dynamic())}

      {false, value_type} =
        open_map(my_map: open_map(foo: integer()))
        |> intersection(open_map(my_map: open_map(bar: boolean())))
        |> map_fetch(:my_map)

      assert equal?(value_type, open_map(foo: integer(), bar: boolean()))

      assert map_fetch(union(closed_map(a: integer()), closed_map(a: atom())), :a) ==
               {false, union(integer(), atom())}

      assert map_fetch(union(closed_map(a: integer()), closed_map(b: atom())), :a) ==
               {true, integer()}

      {false, value_type} =
        closed_map(a: union(integer(), atom()))
        |> difference(open_map(a: integer()))
        |> map_fetch(:a)

      assert equal?(value_type, atom())

      {false, value_type} =
        closed_map(a: integer(), b: atom())
        |> difference(closed_map(a: integer(), b: atom([:foo])))
        |> map_fetch(:a)

      assert equal?(value_type, integer())

      {false, value_type} =
        closed_map(a: integer())
        |> difference(closed_map(a: atom()))
        |> map_fetch(:a)

      assert equal?(value_type, integer())

      {false, value_type} =
        open_map(a: integer(), b: atom())
        |> union(closed_map(a: tuple()))
        |> map_fetch(:a)

      assert equal?(value_type, union(integer(), tuple()))

      {false, value_type} =
        closed_map(a: atom())
        |> difference(closed_map(a: atom([:foo, :bar])))
        |> difference(closed_map(a: atom([:bar])))
        |> map_fetch(:a)

      assert equal?(value_type, intersection(atom(), negation(atom([:foo, :bar]))))

      assert closed_map(a: union(atom(), pid()), b: integer(), c: tuple())
             |> difference(open_map(a: atom(), b: integer()))
             |> difference(open_map(a: atom(), c: tuple()))
             |> map_fetch(:a) == {false, pid()}

      assert closed_map(a: union(atom([:foo]), pid()), b: integer(), c: tuple())
             |> difference(open_map(a: atom([:foo]), b: integer()))
             |> difference(open_map(a: atom(), c: tuple()))
             |> map_fetch(:a) == {false, pid()}

      assert closed_map(a: union(atom([:foo, :bar, :baz]), integer()))
             |> difference(open_map(a: atom([:foo, :bar])))
             |> difference(open_map(a: atom([:foo, :baz])))
             |> map_fetch(:a) == {false, integer()}
    end
  end

  describe "to_quoted" do
    test "bitmap" do
      assert union(integer(), union(float(), binary())) |> to_quoted_string() ==
               "binary() or float() or integer()"
    end

    test "none" do
      assert none() |> to_quoted_string() == "none()"
    end

    test "negation" do
      assert negation(negation(integer())) |> to_quoted_string() == "integer()"
      assert negation(negation(atom([:foo, :bar]))) |> to_quoted_string() == ":bar or :foo"
    end

    test "atom" do
      assert atom() |> to_quoted_string() == "atom()"
      assert atom([:a]) |> to_quoted_string() == ":a"
      assert atom([:a, :b]) |> to_quoted_string() == ":a or :b"
      assert difference(atom(), atom([:a])) |> to_quoted_string() == "atom() and not :a"

      assert atom([Elixir]) |> to_quoted_string() == "Elixir"
      assert atom([Foo.Bar]) |> to_quoted_string() == "Foo.Bar"
    end

    test "boolean" do
      assert boolean() |> to_quoted_string() == "boolean()"
      assert atom([true, false, :a]) |> to_quoted_string() == "boolean() or :a"
      assert atom([true, :a]) |> to_quoted_string() == ":a or true"
      assert difference(atom(), boolean()) |> to_quoted_string() == "atom() and not boolean()"
    end

    test "dynamic" do
      assert dynamic() |> to_quoted_string() == "dynamic()"
      assert intersection(binary(), dynamic()) |> to_quoted_string() == "binary()"

      assert intersection(union(binary(), pid()), dynamic()) |> to_quoted_string() ==
               "dynamic() and (binary() or pid())"

      assert intersection(atom(), dynamic()) |> to_quoted_string() == "dynamic() and atom()"

      assert union(atom([:foo, :bar]), dynamic()) |> to_quoted_string() ==
               "dynamic() or (:bar or :foo)"

      assert intersection(dynamic(), closed_map(a: integer())) |> to_quoted_string() ==
               "dynamic() and %{a: integer()}"
    end

    test "map" do
      assert empty_map() |> to_quoted_string() == "empty_map()"
      assert open_map() |> to_quoted_string() == "%{...}"

      assert closed_map(a: integer()) |> to_quoted_string() == "%{a: integer()}"
      assert open_map(a: float()) |> to_quoted_string() == "%{..., a: float()}"

      assert closed_map("Elixir.Foo.Bar": integer()) |> to_quoted_string() ==
               "%{Foo.Bar => integer()}"

      assert open_map("Elixir.Foo.Bar": float()) |> to_quoted_string() ==
               "%{..., Foo.Bar => float()}"

      # TODO: support this simplification
      # assert difference(open_map(), open_map(a: term())) |> to_quoted_string() ==
      #          "%{..., a: not_set()}"

      assert closed_map(a: integer(), b: atom()) |> to_quoted_string() ==
               "%{a: integer(), b: atom()}"

      assert open_map(a: float())
             |> difference(closed_map(a: float()))
             |> to_quoted_string() == "%{..., a: float()} and not %{a: float()}"

      assert difference(open_map(), empty_map()) |> to_quoted_string() ==
               "%{...} and not empty_map()"

      assert closed_map(foo: union(integer(), not_set())) |> to_quoted_string() ==
               "%{foo: if_set(integer())}"

      assert difference(open_map(a: integer()), closed_map(b: boolean())) |> to_quoted_string() ==
               "%{..., a: integer()}"

      assert open_map(a: integer(), b: atom())
             |> difference(open_map(b: atom()))
             |> union(open_map(a: integer()))
             |> to_quoted_string() == "%{..., a: integer()}"
    end

    test "structs" do
      assert open_map(__struct__: atom([URI])) |> to_quoted_string() == "%{..., __struct__: URI}"

      assert closed_map(__struct__: atom([URI])) |> to_quoted_string() ==
               "%URI{}"

      assert closed_map(__struct__: atom([URI]), path: atom([nil])) |> to_quoted_string() ==
               "%URI{path: nil}"

      assert closed_map(__struct__: atom([URI, Another])) |> to_quoted_string() ==
               "%{__struct__: Another or URI}"
    end
  end
end
