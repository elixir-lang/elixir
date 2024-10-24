Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.ExprTest do
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr

  defmacro generated(x) do
    quote generated: true do
      unquote(x).foo()
    end
  end

  test "literal" do
    assert typecheck!(true) == atom([true])
    assert typecheck!(false) == atom([false])
    assert typecheck!(:foo) == atom([:foo])
    assert typecheck!(0) == integer()
    assert typecheck!(0.0) == float()
    assert typecheck!("foo") == binary()
    assert typecheck!([]) == empty_list()
    assert typecheck!(%{}) == closed_map([])
    assert typecheck!(& &1) == fun()
    assert typecheck!(fn -> :ok end) == fun()
  end

  test "generated" do
    assert typecheck!([x = 1], generated(x)) == dynamic()
  end

  describe "lists" do
    test "creating lists" do
      assert typecheck!([1, 2]) == non_empty_list(integer())
      assert typecheck!([1, 2 | 3]) == non_empty_list(integer(), integer())
      assert typecheck!([1, 2 | [3, 4]]) == non_empty_list(integer())
    end
  end

  describe "funs" do
    test "incompatible" do
      assert typewarn!([%x{}], x.(1, 2)) ==
               {dynamic(),
                ~l"""
                incompatible types in expression:

                    x

                expected type:

                    fun()

                but got type:

                    dynamic(atom())

                where "x" was given the type:

                    # type: dynamic(atom())
                    # from: types_test.ex:LINE-2
                    %x{}
                """}
    end
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
                """}
    end

    test "capture a function with non atoms" do
      assert typewarn!([<<x::integer>>], &x.foo_bar/2) ==
               {fun(),
                ~l"""
                expected a module (an atom) when invoking foo_bar/2 in expression:

                    &x.foo_bar/2

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>
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
                """}
    end

    test "size ok" do
      assert typecheck!([<<x, y>>, z], <<z::size(x - y)>>) == binary()
    end

    test "size error" do
      assert typewarn!([<<x::binary>>, y], <<y::size(x)>>) ==
               {binary(),
                ~l"""
                incompatible types in expression:

                    <<y::integer-size(x)>>

                expected type:

                    integer()

                but got type:

                    binary()

                where "x" was given the type:

                    # type: binary()
                    # from: types_test.ex:LINE-2
                    <<x::binary>>

                where "y" was given the type:

                    # type: dynamic()
                    # from: types_test.ex:LINE-2
                    y
                """}
    end
  end

  describe "tuples" do
    test "creating tuples" do
      assert typecheck!({:ok, 123}) == tuple([atom([:ok]), integer()])
      assert typecheck!([x], {:ok, x}) == dynamic(tuple([atom([:ok]), term()]))
    end

    test "elem/2" do
      assert typecheck!(elem({:ok, 123}, 0)) == atom([:ok])
      assert typecheck!(elem({:ok, 123}, 1)) == integer()
      assert typecheck!([x], elem({:ok, x}, 0)) == dynamic(atom([:ok]))
      assert typecheck!([x], elem({:ok, x}, 1)) == dynamic(term())

      assert typewarn!([<<x::integer>>], elem(x, 0)) ==
               {dynamic(),
                ~l"""
                expected a tuple in expression:

                    elem(x, 0)

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>
                """}

      assert typewarn!(elem({:ok, 123}, 2)) ==
               {dynamic(),
                ~l"""
                expected a tuple with at least 3 elements in expression:

                    elem({:ok, 123}, 2)

                the given type does not have the given index:

                    {:ok, integer()}
                """}
    end

    test "Tuple.insert_at/3" do
      assert typecheck!(Tuple.insert_at({}, 0, "foo")) == tuple([binary()])

      assert typecheck!(Tuple.insert_at({:ok, 123}, 0, "foo")) ==
               tuple([binary(), atom([:ok]), integer()])

      assert typecheck!(Tuple.insert_at({:ok, 123}, 1, "foo")) ==
               tuple([atom([:ok]), binary(), integer()])

      assert typecheck!(Tuple.insert_at({:ok, 123}, 2, "foo")) ==
               tuple([atom([:ok]), integer(), binary()])

      assert typewarn!([<<x::integer>>], Tuple.insert_at(x, 0, "foo")) ==
               {dynamic(),
                ~l"""
                expected a tuple in expression:

                    Tuple.insert_at(x, 0, "foo")

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>
                """}

      assert typewarn!(Tuple.insert_at({:ok, 123}, 3, "foo")) ==
               {dynamic(),
                ~l"""
                expected a tuple with at least 3 elements in expression:

                    Tuple.insert_at({:ok, 123}, 3, "foo")

                the given type does not have the given index:

                    {:ok, integer()}
                """}
    end

    test "Tuple.delete_at/2" do
      assert typecheck!(Tuple.delete_at({:ok, 123}, 0)) == tuple([integer()])
      assert typecheck!(Tuple.delete_at({:ok, 123}, 1)) == tuple([atom([:ok])])
      assert typecheck!([x], Tuple.delete_at({:ok, x}, 0)) == dynamic(tuple([term()]))
      assert typecheck!([x], Tuple.delete_at({:ok, x}, 1)) == dynamic(tuple([atom([:ok])]))

      assert typewarn!([<<x::integer>>], Tuple.delete_at(x, 0)) ==
               {dynamic(),
                ~l"""
                expected a tuple in expression:

                    Tuple.delete_at(x, 0)

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x::integer>>
                """}

      assert typewarn!(Tuple.delete_at({:ok, 123}, 2)) ==
               {dynamic(),
                ~l"""
                expected a tuple with at least 3 elements in expression:

                    Tuple.delete_at({:ok, 123}, 2)

                the given type does not have the given index:

                    {:ok, integer()}
                """}
    end
  end

  describe "maps/structs" do
    test "creating maps" do
      assert typecheck!(%{foo: :bar}) == closed_map(foo: atom([:bar]))
      assert typecheck!(%{123 => 456}) == open_map()
      assert typecheck!(%{123 => 456, foo: :bar}) == open_map(foo: atom([:bar]))
      assert typecheck!([x], %{key: x}) == dynamic(closed_map(key: term()))

      assert typecheck!(
               (
                 foo = :foo
                 %{foo => :first, foo => :second}
               )
             ) == closed_map(foo: atom([:second]))
    end

    test "creating structs" do
      assert typecheck!(%Point{}) ==
               dynamic(
                 closed_map(
                   __struct__: atom([Point]),
                   x: atom([nil]),
                   y: atom([nil]),
                   z: integer()
                 )
               )

      assert typecheck!(%Point{x: :zero}) ==
               dynamic(
                 closed_map(
                   __struct__: atom([Point]),
                   x: atom([:zero]),
                   y: atom([nil]),
                   z: integer()
                 )
               )
    end

    test "updating structs" do
      assert typecheck!([x], %Point{x | x: :zero}) ==
               dynamic(
                 closed_map(__struct__: atom([Point]), x: atom([:zero]), y: term(), z: term())
               )

      assert typewarn!([x = :foo], %Point{x | x: :zero}) ==
               {dynamic(),
                ~l"""
                incompatible types in struct update:

                    %Point{x | x: :zero}

                expected type:

                    dynamic(%Point{x: term(), y: term(), z: term()})

                but got type:

                    dynamic(:foo)

                where "x" was given the type:

                    # type: dynamic(:foo)
                    # from: types_test.ex:LINE-2
                    x = :foo
                """}
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
                """}
    end

    test "accessing an unknown field on struct with diagnostic" do
      {type, diagnostic} = typediag!(%Point{}.foo_bar)
      assert type == dynamic()
      assert diagnostic.span == {__ENV__.line - 2, 54}

      assert diagnostic.message == ~l"""
             unknown key .foo_bar in expression:

                 %Point{x: nil, y: nil, z: 0}.foo_bar

             the given type does not have the given key:

                 dynamic(%Point{x: nil, y: nil, z: integer()})
             """
    end

    test "accessing an unknown field on struct in a var with diagnostic" do
      {type, diagnostic} = typediag!([x = %URI{}], x.foo_bar)
      assert type == dynamic()
      assert diagnostic.span == {__ENV__.line - 2, 61}

      assert diagnostic.message == ~l"""
             unknown key .foo_bar in expression:

                 x.foo_bar

             where "x" was given the type:

                 # type: dynamic(%URI{
                   authority: term(),
                   fragment: term(),
                   host: term(),
                   path: term(),
                   port: term(),
                   query: term(),
                   scheme: term(),
                   userinfo: term()
                 })
                 # from: types_test.ex:LINE-4
                 x = %URI{}
             """

      assert [%{type: :variable, name: :x}] = diagnostic.details.typing_traces
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

                where "x" was given the type:

                    # type: dynamic(:foo)
                    # from: types_test.ex:LINE-2
                    x = :foo

                where "y" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    y = 321

                While Elixir can compare across all types, you are comparing across types \
                which are always distinct, and the result is either always true or always false
                """}
    end

    test "warns on comparison with struct across dynamic call" do
      assert typewarn!([x = :foo, y = %Point{}, mod = Kernel], mod.<=(x, y)) ==
               {boolean(),
                ~l"""
                comparison with structs found:

                    mod.<=(x, y)

                where "mod" was given the type:

                    # type: dynamic(Kernel)
                    # from: types_test.ex:LINE-2
                    mod = Kernel

                where "x" was given the type:

                    # type: dynamic(:foo)
                    # from: types_test.ex:LINE-2
                    x = :foo

                where "y" was given the type:

                    # type: dynamic(%Point{x: term(), y: term(), z: term()})
                    # from: types_test.ex:LINE-2
                    y = %Point{}

                Comparison operators (>, <, >=, <=, min, and max) perform structural and not semantic comparison. Comparing with a struct won't give meaningful results. Structs that can be compared typically define a compare/2 function within their modules that can be used for semantic comparison.
                """}
    end
  end

  describe "try" do
    test "warns on undefined exceptions" do
      assert typewarn!(
               try do
                 :ok
               rescue
                 e in UnknownError -> e
               end
             ) ==
               {dynamic(),
                "struct UnknownError is undefined (module UnknownError is not available or is yet to be defined)"}

      assert typewarn!(
               try do
                 :ok
               rescue
                 e in Enumerable -> e
               end
             ) ==
               {dynamic(),
                "struct Enumerable is undefined (there is such module but it does not define a struct)"}
    end

    test "defines unions of exceptions in rescue" do
      # TODO: check via the actual return type instead
      assert typewarn!(
               try do
                 :ok
               rescue
                 e in [SyntaxError, RuntimeError] ->
                   e.unknown
               end
             ) ==
               {dynamic(),
                ~l"""
                unknown key .unknown in expression:

                    e.unknown

                where "e" was given the type:

                    # type: dynamic(
                      %RuntimeError{__exception__: true, message: term()} or
                        %SyntaxError{
                          __exception__: true,
                          column: term(),
                          description: term(),
                          file: term(),
                          line: term(),
                          snippet: term()
                        }
                    )
                    # from: types_test.ex:LINE-5
                    rescue e in [SyntaxError, RuntimeError] ->
                """}
    end

    test "defines an open map of two fields in anonymous rescue" do
      # TODO: check via the actual return type instead
      assert typewarn!(
               try do
                 :ok
               rescue
                 e -> e.message
               end
             ) ==
               {dynamic(),
                ~l"""
                unknown key .message in expression:

                    e.message

                where "e" was given the type:

                    # type: %{..., __exception__: true, __struct__: atom()}
                    # from: types_test.ex:LINE-4
                    rescue e ->

                #{hints(:anonymous_rescue)}
                """}
    end
  end

  describe "comprehensions" do
    test "binary generators" do
      assert typewarn!([<<x>>], for(<<y <- x>>, do: y)) ==
               {dynamic(),
                ~l"""
                expected the right side of <- in a binary generator to be a binary:

                    x

                but got type:

                    integer()

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    <<x>>

                #{hints(:inferred_bitstring_spec)}
                """}
    end
  end
end
