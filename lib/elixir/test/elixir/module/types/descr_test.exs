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
      assert union(negation(atom([:a, :b])), negation(atom([:b, :c]))) == negation(atom([:b]))
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
    end
  end
end
