Code.require_file("type_helper.exs", __DIR__)

defmodule Point do
  defstruct [:x, :y, z: 0]
end

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
    assert typecheck!(%{}) == open_map()
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

                but got type:

                    integer()

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

                but got type:

                    integer()

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

    test "accessing a field on not a map" do
      assert typewarn!([<<x::integer>>], x.foo_bar) ==
               {dynamic(),
                ~l"""
                expected a map or struct when accessing .foo_bar in expression:

                    x.foo_bar

                but got type:

                    integer()

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
                missing key .foo_bar in expression:

                    %Point{x: nil, y: nil, z: 0}.foo_bar

                the given type does not have the given key:

                    %Point{x: nil, y: nil, z: integer()}

                #{hints(:dot)}

                typing violation found at:\
                """}
    end
  end
end
