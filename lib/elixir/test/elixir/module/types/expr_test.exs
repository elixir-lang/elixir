Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.ExprTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  test "literal" do
    assert typecheck!(true) == atom([true])
    assert typecheck!(false) == atom([false])
    assert typecheck!(:foo) == atom([:foo])
    assert typecheck!(0) == integer()
    assert typecheck!(0.0) == float()
    assert typecheck!("foo") == binary()
    assert typecheck!([]) == empty_list()
    assert typecheck!([1, 2]) == non_empty_list()
    assert typecheck!({1, 2}) == tuple()
    assert typecheck!(%{}) == closed_map([])
  end

  describe "remotes" do
    test "dynamic calls" do
      assert typecheck!([%x{}], x.foo_bar()) == dynamic()
    end

    test "undefined function warnings" do
      assert typewarn!(URI.unknown("foo")) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}

      assert typewarn!(if(true, do: URI.unknown("foo"))) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}

      assert typewarn!(try(do: :ok, after: URI.unknown("foo"))) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}
    end

    test "calling a nullary function on non atoms" do
      assert typewarn!([<<x::integer>>], x.foo_bar()) ==
               {dynamic(),
                ~l"""
                expected a module (an atom) when invoking foo_bar/0 in expression:

                    x.foo_bar()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>

                #{hints(:dot)}

                typing violation found at:\
                """}
    end

    test "calling a function on non atoms with arguments" do
      assert typewarn!([<<x::integer>>], x.foo_bar(1, 2)) ==
               {dynamic(),
                ~l"""
                expected a module (an atom) when invoking foo_bar/2 in expression:

                    x.foo_bar(1, 2)

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>

                typing violation found at:\
                """}
    end

    test "capture a function with non atoms" do
      assert typewarn!([<<x::integer>>], &x.foo_bar/2) ==
               {dynamic(),
                ~l"""
                expected a module (an atom) when invoking foo_bar/2 in expression:

                    &x.foo_bar/2

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>

                typing violation found at:\
                """}
    end
  end

  describe "binaries" do
    test "warnings" do
      assert typewarn!([<<x::binary-size(2)>>], <<x::float>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<x::float>>

                expected type:

                    float() or integer()

                but got type:

                    binary()

                where "x" was given the type:

                    # type: binary()
                    # from: types_test.ex:LINE-2
                    <<x::binary-size(2)>>

                typing violation found at:\
                """}

      assert typewarn!([<<x::binary>>], <<x>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<x>>

                expected type:

                    integer()

                but got type:

                    binary()

                where "x" was given the type:

                    # type: binary()
                    # from: types_test.ex:LINE-2
                    <<x::binary>>

                #{hints(:inferred_bitstring_spec)}

                typing violation found at:\
                """}

      assert typewarn!([<<x>>], <<x::binary>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<x::binary>>

                expected type:

                    binary()

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x>>

                #{hints(:inferred_bitstring_spec)}

                typing violation found at:\
                """}
    end
  end

  describe "maps/structs" do
    test "creating maps" do
      assert typecheck!(%{foo: :bar}) == closed_map(foo: atom([:bar]))
      assert typecheck!(%{123 => 456}) == open_map()
      assert typecheck!(%{123 => 456, foo: :bar}) == open_map(foo: atom([:bar]))

      assert typecheck!(
               (
                 foo = :foo
                 %{foo => :first, foo => :second}
               )
             ) == closed_map(foo: atom([:second]))
    end

    test "creating structs" do
      assert typecheck!(%Point{}) ==
               closed_map(__struct__: atom([Point]), x: atom([nil]), y: atom([nil]), z: integer())

      assert typecheck!(%Point{x: :zero}) ==
               closed_map(
                 __struct__: atom([Point]),
                 x: atom([:zero]),
                 y: atom([nil]),
                 z: integer()
               )
    end

    test "updating structs" do
      assert typecheck!([x], %Point{x | x: :zero}) ==
               closed_map(__struct__: atom([Point]), x: atom([:zero]), y: dynamic(), z: dynamic())
    end

    test "nested map" do
      assert typecheck!([x = %{}], x.foo.bar) == dynamic()
    end

    test "accessing a field on not a map" do
      assert typewarn!([<<x::integer>>], x.foo_bar) ==
               {dynamic(),
                ~l"""
                expected a map or struct when accessing .foo_bar in expression:

                    x.foo_bar

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>

                #{hints(:dot)}

                typing violation found at:\
                """}
    end

    test "accessing an unknown field on struct" do
      assert typewarn!(%Point{}.foo_bar) ==
               {dynamic(),
                ~l"""
                unknown key .foo_bar in expression:

                    %Point{x: nil, y: nil, z: 0}.foo_bar

                the given type does not have the given key:

                    %Point{x: nil, y: nil, z: integer()}

                typing violation found at:\
                """}
    end

    test "accessing an unknown field on struct in a var" do
      assert typewarn!([x = %URI{}], x.foo_bar) ==
               {dynamic(),
                ~l"""
                unknown key .foo_bar in expression:

                    x.foo_bar

                where "x" was given the type:

                    # type: %URI{
                      authority: dynamic(),
                      fragment: dynamic(),
                      host: dynamic(),
                      path: dynamic(),
                      port: dynamic(),
                      query: dynamic(),
                      scheme: dynamic(),
                      userinfo: dynamic()
                    }
                    # from: types_test.ex:LINE-2
                    x = %URI{}

                typing violation found at:\
                """}
    end
  end

  describe "comparison" do
    test "works across numbers" do
      assert typecheck!([x = 123, y = 456.0], min(x, y)) == dynamic(union(integer(), float()))
      assert typecheck!([x = 123, y = 456.0], x < y) == boolean()
    end

    test "warns when comparison is constant" do
      assert typewarn!([x = :foo, y = 321], min(x, y)) ==
               {dynamic(union(integer(), atom([:foo]))),
                ~l"""
                comparison between incompatible types found:

                    min(x, y)

                while Elixir can compare across all types, you are comparing across types \
                which are always distinct, and the result is either always true or always false

                where "x" was given the type:

                    # type: :foo
                    # from: types_test.ex:LINE-2
                    x = :foo

                where "y" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    y = 321

                typing violation found at:\
                """}
    end

    test "warns on comparison with struct across dynamic call" do
      assert typewarn!([x = :foo, y = %Point{}, mod = Kernel], mod.<=(x, y)) ==
               {boolean(),
                ~l"""
                comparison with structs found:

                    mod.<=(x, y)

                comparison operators (>, <, >=, <=, min, and max) perform structural and not semantic comparison. Comparing with a struct won't give meaningful results. Struct that can be compared typically define a compare/2 function within their modules that can be used for semantic comparison

                where "mod" was given the type:

                    # type: Kernel
                    # from: types_test.ex:LINE-2
                    mod = Kernel

                where "x" was given the type:

                    # type: :foo
                    # from: types_test.ex:LINE-2
                    x = :foo

                where "y" was given the type:

                    # type: %Point{x: dynamic(), y: dynamic(), z: dynamic()}
                    # from: types_test.ex:LINE-2
                    y = %Point{}

                typing violation found at:\
                """}
    end
  end
end
