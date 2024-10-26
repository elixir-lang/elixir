Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.PatternTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  describe "variables" do
    test "captures variables from simple assignment in head" do
      assert typecheck!([x = :foo], x) == dynamic(atom([:foo]))
      assert typecheck!([:foo = x], x) == dynamic(atom([:foo]))
    end

    test "captures variables from simple assignment in =" do
      assert typecheck!(
               (
                 x = :foo
                 x
               )
             ) == atom([:foo])
    end

    test "refines information across patterns" do
      assert typecheck!([%y{}, %x{}, x = y, x = Point], y) == dynamic(atom([Point]))
    end

    test "repeated refinements are ignored on reporting" do
      assert typeerror!([{name, arity}, arity = 123], hd(Atom.to_charlist(name))) == ~l"""
             incompatible types given to Kernel.hd/1:

                 hd(Atom.to_charlist(name))

             expected types:

                 non_empty_list(term(), term())

             but got types:

                 empty_list() or non_empty_list(integer())

             where "name" was given the type:

                 # type: dynamic()
                 # from: types_test.ex
                 {name, arity}
             """
    end

    test "errors on conflicting refinements" do
      assert typeerror!([a = b, a = :foo, b = :bar], {a, b}) ==
               ~l"""
               the following pattern will never match:

                   a = b

               where "a" was given the type:

                   # type: dynamic(:foo)
                   # from: types_test.ex:LINE-1
                   a = :foo

               where "b" was given the type:

                   # type: dynamic(:bar)
                   # from: types_test.ex:LINE-1
                   b = :bar
               """
    end
  end

  describe "structs" do
    test "variable name" do
      assert typecheck!([%x{}], x) == dynamic(atom())
    end

    test "variable name fields" do
      assert typecheck!([x = %_{}], x.__struct__) == dynamic(atom())
      assert typecheck!([x = %_{}], x) == dynamic(open_map(__struct__: atom()))

      assert typecheck!([x = %m{}, m = Point], x) ==
               dynamic(open_map(__struct__: atom([Point])))

      assert typecheck!([m = Point, x = %m{}], x) ==
               dynamic(open_map(__struct__: atom([Point])))

      assert typeerror!([m = 123], %^m{} = %Point{}) ==
               ~l"""
               incompatible types in expression:

                   %^m{}

               expected type:

                   atom()

               but got type:

                   integer()

               where "m" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   m = 123
               """
    end

    test "fields in guards" do
      assert typeerror!([x = %Point{}], x.foo_bar, :ok) ==
               ~l"""
               unknown key .foo_bar in expression:

                   x.foo_bar

               where "x" was given the type:

                   # type: dynamic(%Point{x: term(), y: term(), z: term()})
                   # from: types_test.ex:LINE-1
                   x = %Point{}
               """
    end
  end

  describe "maps" do
    test "fields in patterns" do
      assert typecheck!([x = %{foo: :bar}], x) == dynamic(open_map(foo: atom([:bar])))
      assert typecheck!([x = %{123 => 456}], x) == dynamic(open_map())
      assert typecheck!([x = %{123 => 456, foo: :bar}], x) == dynamic(open_map(foo: atom([:bar])))
    end

    test "fields in guards" do
      assert typecheck!([x = %{foo: :bar}], x.bar, x) == dynamic(open_map(foo: atom([:bar])))
    end
  end

  describe "tuples" do
    test "in patterns" do
      assert typecheck!([x = {:ok, 123}], x) == dynamic(tuple([atom([:ok]), integer()]))
      assert typecheck!([{:x, y} = {x, :y}], {x, y}) == dynamic(tuple([atom([:x]), atom([:y])]))
    end
  end

  describe "lists" do
    test "in patterns" do
      assert typecheck!([x = [1, 2, 3]], x) ==
               dynamic(non_empty_list(integer()))

      assert typecheck!([x = [1, 2, 3 | y], y = :foo], x) ==
               dynamic(non_empty_list(integer(), atom([:foo])))

      assert typecheck!([x = [1, 2, 3 | y], y = [1.0, 2.0, 3.0]], x) ==
               dynamic(non_empty_list(union(integer(), float())))

      assert typecheck!([x = [:ok | z]], {x, z}) ==
               dynamic(tuple([non_empty_list(term(), term()), term()]))

      assert typecheck!([x = [y | z]], {x, y, z}) ==
               dynamic(tuple([non_empty_list(term(), term()), term(), term()]))
    end

    test "in patterns through ++" do
      assert typecheck!([x = [] ++ []], x) == dynamic(empty_list())

      assert typecheck!([x = [] ++ y, y = :foo], x) ==
               dynamic(atom([:foo]))

      assert typecheck!([x = [1, 2, 3] ++ y, y = :foo], x) ==
               dynamic(non_empty_list(integer(), atom([:foo])))

      assert typecheck!([x = [1, 2, 3] ++ y, y = [1.0, 2.0, 3.0]], x) ==
               dynamic(non_empty_list(union(integer(), float())))
    end
  end

  describe "binaries" do
    test "ok" do
      assert typecheck!([<<x>>], x) == integer()
      assert typecheck!([<<x::float>>], x) == float()
      assert typecheck!([<<x::binary>>], x) == binary()
      assert typecheck!([<<x::utf8>>], x) == integer()
    end

    test "error" do
      assert typeerror!([<<x::binary-size(2), x::float>>], x) == ~l"""
             incompatible types assigned to "x":

                 binary() !~ float()

             where "x" was given the types:

                 # type: binary()
                 # from: types_test.ex:LINE
                 <<x::binary-size(2), ...>>

                 # type: float()
                 # from: types_test.ex:LINE
                 <<..., x::float>>
             """

      assert typeerror!([<<x::float, x>>], x) == ~l"""
             incompatible types assigned to "x":

                 float() !~ integer()

             where "x" was given the types:

                 # type: float()
                 # from: types_test.ex:LINE
                 <<x::float, ...>>

                 # type: integer()
                 # from: types_test.ex:LINE
                 <<..., x>>

             #{hints(:inferred_bitstring_spec)}
             """
    end

    test "size ok" do
      assert typecheck!([<<x, y, _::size(x - y)>>], :ok) == atom([:ok])
    end

    test "size error" do
      assert typeerror!([<<x::float, _::size(x)>>], :ok) ==
               ~l"""
               incompatible types in expression:

                   <<..., _::integer-size(x)>>

               expected type:

                   integer()

               but got type:

                   float()

               where "x" was given the type:

                   # type: float()
                   # from: types_test.ex:LINE-1
                   <<x::float, ...>>
               """
    end
  end

  describe "inference" do
    test "refines information across patterns" do
      result = [
        dynamic(open_map(__struct__: atom([Point]))),
        dynamic(open_map(__struct__: atom([Point]))),
        dynamic(atom([Point])),
        dynamic(atom([Point]))
      ]

      assert typeinfer!([%y{}, %x{}, x = y, x = Point]) == result
      assert typeinfer!([%x{}, %y{}, x = y, x = Point]) == result
      assert typeinfer!([%y{}, %x{}, x = y, y = Point]) == result
      assert typeinfer!([%x{}, %y{}, x = y, y = Point]) == result
    end
  end
end
