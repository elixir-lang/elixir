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
        map(),
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
      assert equal?(union(map(), map()), map())
      assert equal?(union(map(a: integer()), map()), map())
      assert equal?(union(map(a: integer()), negation(map(a: integer()))), term())

      a_integer_open = map([a: integer()], :open)
      assert equal?(union(map(a: integer()), a_integer_open), a_integer_open)

      assert difference(map([a: integer()], :open), map(b: boolean()))
             |> equal?(map([a: integer()], :open))
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
      assert intersection(map(), map()) == map()
      assert equal?(intersection(map(a: integer()), map()), map(a: integer()))

      a_integer_open = map([a: integer()], :open)
      assert equal?(intersection(map(a: integer()), a_integer_open), map(a: integer()))

      optional_a_integer_closed = map([a: if_set(integer())], :closed)
      assert equal?(intersection(map(a: integer()), optional_a_integer_closed), map(a: integer()))

      assert empty?(intersection(map(a: integer()), map(a: atom())))
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
      assert empty?(difference(map(), map()))
      assert empty?(difference(map(), term()))
      assert equal?(difference(map(), none()), map())
      assert empty?(difference(map(a: integer()), map()))
      assert empty?(difference(map(a: integer()), map(a: integer())))
      assert empty?(difference(map(a: integer()), map([a: integer()], :open)))

      assert difference(map(a: integer(), b: if_set(atom())), map(a: integer()))
             |> difference(map(a: integer(), b: atom()))
             |> empty?()

      assert difference(map([a: atom()], :open), map(b: integer()))
             |> equal?(map([a: atom()], :open))
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
      assert subtype?(map(), term())
      assert subtype?(map([a: integer()], :closed), map())
      assert subtype?(map([a: integer()], :closed), map([a: integer()], :closed))
      assert subtype?(map([a: integer()], :closed), map([a: integer()], :open))
      assert subtype?(map([a: integer(), b: atom()], :closed), map([a: integer()], :open))
      assert subtype?(map(a: integer()), map(a: union(integer(), atom())))

      # optional
      refute subtype?(map(a: if_set(integer())), map(a: integer()))
      assert subtype?(map(a: integer()), map(a: if_set(integer())))
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
      assert compatible?(map(a: integer()), map())
      assert compatible?(intersection(dynamic(), map()), map(a: integer()))
    end
  end

  describe "map operations" do
    test "get field" do
      assert map_get!(map(a: integer()), :a) == integer()
      assert map_get!(dynamic(), :a) == dynamic()

      assert intersection(dynamic(), map([a: integer()], :open))
             |> map_get!(:a) == intersection(integer(), dynamic())

      assert map([my_map: map([foo: integer()], :open)], :open)
             |> intersection(map([my_map: map([bar: boolean()], :open)], :open))
             |> map_get!(:my_map)
             |> equal?(map([foo: integer(), bar: boolean()], :open))

      assert map_get!(union(map(a: integer()), map(a: atom())), :a) == union(integer(), atom())
      assert map_get!(union(map(a: integer()), map(b: atom())), :a) == integer()
      assert map_get!(term(), :a) == term()

      assert map(a: union(integer(), atom()))
             |> difference(map([a: integer()], :open))
             |> map_get!(:a)
             |> equal?(atom())

      assert map(a: integer(), b: atom())
             |> difference(map(a: integer(), b: atom([:foo])))
             |> map_get!(:a)
             |> equal?(integer())

      assert map(a: integer())
             |> difference(map(a: atom()))
             |> map_get!(:a)
             |> equal?(integer())
    end

    test "key presence" do
      assert map_has_key?(map(a: integer()), :a)
      refute map_has_key?(map(a: integer()), :b)
      refute map_has_key?(map(), :a)
      refute map_has_key?(map(a: union(integer(), not_set())), :a)
      refute map_has_key?(union(map(a: integer()), map(b: atom())), :a)
      assert map_has_key?(union(map(a: integer()), map(a: atom())), :a)
      assert map_has_key?(intersection(dynamic(), map(a: integer())), :a)
      refute map_has_key?(intersection(dynamic(), map(a: integer())), :b)

      refute map_may_have_key?(map(foo: integer()), :bar)
      assert map_may_have_key?(map(foo: integer()), :foo)
      assert map_may_have_key?(dynamic(), :foo)
      refute map_may_have_key?(intersection(dynamic(), map([foo: not_set()], :open)), :foo)
    end

    test "type-checking map access" do
      # dynamic() and %{..., :a => integer(), b: not_set()}
      t = intersection(dynamic(), map([a: integer(), c: not_set()], :open))

      assert subtype?(map_get!(t, :a), integer())
      assert map_get!(t, :b) == dynamic()

      assert map_has_key?(t, :a)
      refute map_has_key?(t, :b)
      refute map_has_key?(t, :c)

      assert map_may_have_key?(t, :a)
      assert map_may_have_key?(t, :b)
      refute map_may_have_key?(t, :c)
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

      assert intersection(dynamic(), map(a: integer())) |> to_quoted_string() ==
               "dynamic() and %{:a => integer()}"
    end

    test "map" do
      assert map() |> to_quoted_string() == "%{...}"
      assert map(a: integer()) |> to_quoted_string() == "%{:a => integer()}"
      assert map([a: float()], :open) |> to_quoted_string() == "%{..., :a => float()}"

      # TODO: support this simplification
      # assert difference(map(), map([a: term()], :open)) |> to_quoted_string() ==
      #          "%{..., :a => not_set()}"

      assert map(a: integer(), b: atom()) |> to_quoted_string() ==
               "%{:a => integer(), :b => atom()}"

      assert map([a: float()], :open)
             |> difference(map([a: float()], :closed))
             |> to_quoted_string() == "%{..., :a => float()} and not %{:a => float()}"

      assert difference(map(), empty_map()) |> to_quoted_string() == "%{...} and not %{}"

      assert map(foo: union(integer(), not_set())) |> to_quoted_string() ==
               "%{:foo => if_set(integer())}"

      assert difference(map([a: integer()], :open), map(b: boolean())) |> to_quoted_string() ==
               "%{..., :a => integer()}"

      assert map([a: integer(), b: atom()], :open)
             |> difference(map([b: atom()], :open))
             |> union(map([a: integer()], :open))
             |> to_quoted_string() == "%{..., :a => integer()}"
    end
  end
end
