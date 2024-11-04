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

      assert typecheck!([:ok, 123]) == non_empty_list(union(atom([:ok]), integer()))
      assert typecheck!([:ok | 123]) == non_empty_list(atom([:ok]), integer())
      assert typecheck!([x], [:ok, x]) == dynamic(non_empty_list(term()))
      assert typecheck!([x], [:ok | x]) == dynamic(non_empty_list(term(), term()))
    end

    test "hd" do
      assert typecheck!([x = [123, :foo]], hd(x)) == dynamic(union(atom([:foo]), integer()))
      assert typecheck!([x = [123 | :foo]], hd(x)) == dynamic(integer())

      assert typeerror!(hd([])) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.hd/1:

                   hd([])

               given types:

                   empty_list()

               but expected one of:

                   non_empty_list(term(), term())
               """

      assert typeerror!(hd(123)) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.hd/1:

                   hd(123)

               given types:

                   integer()

               but expected one of:

                   non_empty_list(term(), term())
               """
    end

    test "tl" do
      assert typecheck!([x = [123, :foo]], tl(x)) == dynamic(list(union(atom([:foo]), integer())))

      assert typecheck!([x = [123 | :foo]], tl(x)) ==
               dynamic(union(atom([:foo]), list(integer(), atom([:foo]))))

      assert typeerror!(tl([])) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.tl/1:

                   tl([])

               given types:

                   empty_list()

               but expected one of:

                   non_empty_list(term(), term())
               """

      assert typeerror!(tl(123)) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.tl/1:

                   tl(123)

               given types:

                   integer()

               but expected one of:

                   non_empty_list(term(), term())
               """
    end
  end

  describe "funs" do
    test "incompatible" do
      assert typeerror!([%x{}, a1, a2], x.(a1, a2)) == ~l"""
             expected a 2-arity function on call:

                 x.(a1, a2)

             but got type:

                 dynamic(atom())

             where "x" was given the type:

                 # type: dynamic(atom())
                 # from: types_test.ex:LINE
                 %x{}
             """
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

      # Check it also emits over a union
      assert typewarn!(
               [x = Atom, y = GenServer, z],
               (
                 mod =
                   cond do
                     z -> x
                     true -> y
                   end

                 mod.to_string(:atom)
               )
             ) ==
               {union(dynamic(), binary()), "GenServer.to_string/1 is undefined or private"}
    end

    test "calling a nullary function on non atoms" do
      assert typeerror!([<<x::integer>>], x.foo_bar()) ==
               ~l"""
               expected a module (an atom) when invoking foo_bar/0 in expression:

                   x.foo_bar()

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   <<x::integer>>

               #{hints(:dot)}
               """
    end

    test "calling a function on non atoms with arguments" do
      assert typeerror!([<<x::integer>>], x.foo_bar(1, 2)) ==
               ~l"""
               expected a module (an atom) when invoking foo_bar/2 in expression:

                   x.foo_bar(1, 2)

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   <<x::integer>>
               """

      assert typeerror!(
               [<<x::integer>>, y = Atom, z],
               (
                 mod =
                   cond do
                     z -> x
                     true -> y
                   end

                 mod.to_string(:atom)
               )
             ) ==
               ~l"""
               expected a module (an atom) when invoking to_string/1 in expression:

                   mod.to_string(:atom)

               where "mod" was given the type:

                   # type: dynamic(Atom or integer()) or integer()
                   # from: types_test.ex:LINE-9
                   mod =
                     cond do
                       z -> x
                       true -> y
                     end
               """
    end

    test "capture a function with non atoms" do
      assert typeerror!([<<x::integer>>], &x.foo_bar/2) ==
               ~l"""
               expected a module (an atom) when invoking foo_bar/2 in expression:

                   &x.foo_bar/2

               but got type:

                   integer()

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   <<x::integer>>
               """
    end
  end

  describe "binaries" do
    test "warnings" do
      assert typeerror!([<<x::binary-size(2)>>], <<x::float>>) ==
               ~l"""
               incompatible types in expression:

                   <<x::float>>

               got type:

                   binary()

               but expected type:

                   float() or integer()

               where "x" was given the type:

                   # type: binary()
                   # from: types_test.ex:LINE-1
                   <<x::binary-size(2)>>
               """

      assert typeerror!([<<x::binary>>], <<x>>) ==
               ~l"""
               incompatible types in expression:

                   <<x>>

               got type:

                   binary()

               but expected type:

                   integer()

               where "x" was given the type:

                   # type: binary()
                   # from: types_test.ex:LINE-1
                   <<x::binary>>

               #{hints(:inferred_bitstring_spec)}
               """

      assert typeerror!([<<x>>], <<x::binary>>) ==
               ~l"""
               incompatible types in expression:

                   <<x::binary>>

               got type:

                   integer()

               but expected type:

                   binary()

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   <<x>>

               #{hints(:inferred_bitstring_spec)}
               """
    end

    test "size ok" do
      assert typecheck!([<<x, y>>, z], <<z::size(x - y)>>) == binary()
    end

    test "size error" do
      assert typeerror!([<<x::binary>>, y], <<y::size(x)>>) ==
               ~l"""
               incompatible types in expression:

                   size(x)

               got type:

                   binary()

               but expected type:

                   integer()

               where "x" was given the type:

                   # type: binary()
                   # from: types_test.ex:LINE-1
                   <<x::binary>>
               """
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

      assert typeerror!([<<x::float>>], elem(x, 0)) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.elem/2:

                   elem(x, 0)

               given types:

                   float(), integer()

               but expected one of:

                   {...}, integer()

               where "x" was given the type:

                   # type: float()
                   # from: types_test.ex:LINE-1
                   <<x::float>>
               """

      assert typeerror!(elem({:ok, 123}, 2)) ==
               ~l"""
               expected a tuple with at least 3 elements in Kernel.elem/2:

                   elem({:ok, 123}, 2)

               the given type does not have the given index:

                   {:ok, integer()}
               """
    end

    test "Tuple.insert_at/3" do
      assert typecheck!(Tuple.insert_at({}, 0, "foo")) == tuple([binary()])

      assert typecheck!(Tuple.insert_at({:ok, 123}, 0, "foo")) ==
               tuple([binary(), atom([:ok]), integer()])

      assert typecheck!(Tuple.insert_at({:ok, 123}, 1, "foo")) ==
               tuple([atom([:ok]), binary(), integer()])

      assert typecheck!(Tuple.insert_at({:ok, 123}, 2, "foo")) ==
               tuple([atom([:ok]), integer(), binary()])

      assert typeerror!([<<x::float>>], Tuple.insert_at(x, 0, "foo")) |> strip_ansi() ==
               ~l"""
               incompatible types given to Tuple.insert_at/3:

                   Tuple.insert_at(x, 0, "foo")

               given types:

                   float(), integer(), binary()

               but expected one of:

                   {...}, integer(), term()

               where "x" was given the type:

                   # type: float()
                   # from: types_test.ex:LINE-1
                   <<x::float>>
               """

      assert typeerror!(Tuple.insert_at({:ok, 123}, 3, "foo")) ==
               ~l"""
               expected a tuple with at least 3 elements in Tuple.insert_at/3:

                   Tuple.insert_at({:ok, 123}, 3, "foo")

               the given type does not have the given index:

                   {:ok, integer()}
               """
    end

    test "Tuple.delete_at/2" do
      assert typecheck!(Tuple.delete_at({:ok, 123}, 0)) == tuple([integer()])
      assert typecheck!(Tuple.delete_at({:ok, 123}, 1)) == tuple([atom([:ok])])
      assert typecheck!([x], Tuple.delete_at({:ok, x}, 0)) == dynamic(tuple([term()]))
      assert typecheck!([x], Tuple.delete_at({:ok, x}, 1)) == dynamic(tuple([atom([:ok])]))

      assert typeerror!([<<x::float>>], Tuple.delete_at(x, 0)) |> strip_ansi() ==
               ~l"""
               incompatible types given to Tuple.delete_at/2:

                   Tuple.delete_at(x, 0)

               given types:

                   float(), integer()

               but expected one of:

                   {...}, integer()

               where "x" was given the type:

                   # type: float()
                   # from: types_test.ex:LINE-1
                   <<x::float>>
               """

      assert typeerror!(Tuple.delete_at({:ok, 123}, 2)) ==
               ~l"""
               expected a tuple with at least 3 elements in Tuple.delete_at/2:

                   Tuple.delete_at({:ok, 123}, 2)

               the given type does not have the given index:

                   {:ok, integer()}
               """
    end

    test "duplicate/2" do
      assert typecheck!(Tuple.duplicate(123, 0)) == tuple([])
      assert typecheck!(Tuple.duplicate(123, 1)) == tuple([integer()])
      assert typecheck!(Tuple.duplicate(123, 2)) == tuple([integer(), integer()])
      assert typecheck!([x], Tuple.duplicate(x, 2)) == dynamic(tuple([term(), term()]))
    end
  end

  describe "maps/structs" do
    test "creating closed maps" do
      assert typecheck!(%{foo: :bar}) == closed_map(foo: atom([:bar]))
      assert typecheck!([x], %{key: x}) == dynamic(closed_map(key: term()))
    end

    test "creating closed maps with dynamic keys" do
      assert typecheck!(
               (
                 foo = :foo
                 %{foo => :first, foo => :second}
               )
             ) == closed_map(foo: atom([:second]))

      assert typecheck!(
               (
                 foo_or_bar =
                   cond do
                     :rand.uniform() > 0.5 -> :foo
                     true -> :bar
                   end

                 %{foo_or_bar => :first, foo_or_bar => :second}
               )
             )
             |> equal?(
               closed_map(foo: atom([:second]))
               |> union(closed_map(bar: atom([:second])))
               |> union(closed_map(foo: atom([:first]), bar: atom([:second])))
               |> union(closed_map(bar: atom([:first]), foo: atom([:second])))
             )
    end

    test "creating open maps" do
      assert typecheck!(%{123 => 456}) == open_map()
      # Since key cannot override :foo, we preserve it
      assert typecheck!([key], %{key => 456, foo: :bar}) == dynamic(open_map(foo: atom([:bar])))
      # Since key can override :foo, we do not preserve it
      assert typecheck!([key], %{:foo => :bar, key => :baz}) == dynamic(open_map())
    end

    test "creating structs" do
      assert typecheck!(%Point{}) ==
               closed_map(
                 __struct__: atom([Point]),
                 x: atom([nil]),
                 y: atom([nil]),
                 z: integer()
               )

      assert typecheck!(%Point{x: :zero}) ==
               closed_map(
                 __struct__: atom([Point]),
                 x: atom([:zero]),
                 y: atom([nil]),
                 z: integer()
               )
    end

    test "updating to closed maps" do
      assert typecheck!([x], %{x | x: :zero}) ==
               dynamic(open_map(x: atom([:zero])))

      assert typecheck!([x], %{%{x | x: :zero} | y: :one}) ==
               dynamic(open_map(x: atom([:zero]), y: atom([:one])))

      assert typeerror!([x = :foo], %{x | x: :zero}) == ~l"""
             expected a map within map update syntax:

                 %{x | x: :zero}

             but got type:

                 dynamic(:foo)

             where "x" was given the type:

                 # type: dynamic(:foo)
                 # from: types_test.ex:LINE
                 x = :foo
             """
    end

    test "updating to open maps" do
      assert typecheck!(
               [key],
               (
                 x = %{foo: :bar}
                 %{x | key => :baz}
               )
             ) == dynamic(open_map())

      # Since key cannot override :foo, we preserve it
      assert typecheck!(
               [key],
               (
                 x = %{foo: :bar}
                 %{x | key => :baz, foo: :bat}
               )
             ) == dynamic(open_map(foo: atom([:bat])))

      # Since key can override :foo, we do not preserve it
      assert typecheck!(
               [key],
               (
                 x = %{foo: :bar}
                 %{x | :foo => :baz, key => :bat}
               )
             ) == dynamic(open_map())

      # The goal of this test is to verify we assert keys,
      # even if they may be overridden later.
      assert typeerror!(
               [key],
               (
                 x = %{key: :value}
                 %{x | :foo => :baz, key => :bat}
               )
             ) == ~l"""
             expected a map with key :foo in map update syntax:

                 %{x | :foo => :baz, key => :bat}

             but got type:

                 %{key: :value}

             where "key" was given the type:

                 # type: dynamic()
                 # from: types_test.ex:LINE-5
                 key

             where "x" was given the type:

                 # type: %{key: :value}
                 # from: types_test.ex:LINE-3
                 x = %{key: :value}
             """
    end

    test "updating structs" do
      assert typecheck!([x], %Point{x | x: :zero}) ==
               dynamic(open_map(__struct__: atom([Point]), x: atom([:zero])))

      assert typecheck!([x], %Point{%Point{x | x: :zero} | y: :one}) ==
               dynamic(open_map(__struct__: atom([Point]), x: atom([:zero]), y: atom([:one])))

      assert typeerror!(
               (
                 x = %{x: 0}
                 %Point{x | x: :zero}
               )
             ) ==
               ~l"""
               incompatible types in struct update:

                   %Point{x | x: :zero}

               expected type:

                   %Point{x: term(), y: term(), z: term()}

               but got type:

                   %{x: integer()}

               where "x" was given the type:

                   # type: %{x: integer()}
                   # from: types_test.ex:LINE-4
                   x = %{x: 0}
               """
    end

    test "nested map" do
      assert typecheck!([x = %{}], x.foo.bar) == dynamic()
    end

    test "accessing a field on not a map" do
      assert typeerror!([<<x::integer>>], x.foo_bar) ==
               ~l"""
               expected a map or struct when accessing .foo_bar in expression:

                   x.foo_bar

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   <<x::integer>>

               #{hints(:dot)}
               """
    end

    test "accessing an unknown field on struct with diagnostic" do
      {type, diagnostic} = typediag!(%Point{}.foo_bar)
      assert type == dynamic()
      assert diagnostic.span == {__ENV__.line - 2, 54}

      assert diagnostic.message == ~l"""
             unknown key .foo_bar in expression:

                 %Point{x: nil, y: nil, z: 0}.foo_bar

             the given type does not have the given key:

                 %Point{x: nil, y: nil, z: integer()}
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
                 # from: types_test.ex:LINE-4:41
                 x = %URI{}
             """

      assert [%{type: :variable, name: :x}] = diagnostic.details.typing_traces
    end
  end

  describe "comparison" do
    test "in static mode" do
      assert typecheck!([x = 123, y = 456.0], x < y) == boolean()
      assert typecheck!([x = 123, y = 456.0], x == y) == boolean()
    end

    test "in dynamic mode" do
      assert typedyn!([x = 123, y = 456.0], x < y) == dynamic(boolean())
      assert typedyn!([x = 123, y = 456.0], x == y) == dynamic(boolean())
      assert typedyn!(123 == 456) == boolean()
    end

    test "min/max" do
      assert typecheck!(min(123, 456.0)) == union(integer(), float())
      # min/max uses parametric types, which will carry dynamic regardless of being a strong arrow
      assert typecheck!([x = 123, y = 456.0], min(x, y)) == dynamic(union(integer(), float()))
      assert typedyn!([x = 123, y = 456.0], min(x, y)) == dynamic(union(integer(), float()))
    end

    test "warns when comparison is constant" do
      assert typewarn!([x = :foo, y = 321], min(x, y)) ==
               {dynamic(union(integer(), atom([:foo]))),
                ~l"""
                comparison between distinct types found:

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
                which are always disjoint, and the result is either always true or always false
                """}

      assert typewarn!([x = 123, y = 456.0], x === y) ==
               {boolean(),
                ~l"""
                comparison between distinct types found:

                    x === y

                where "x" was given the type:

                    # type: integer()
                    # from: types_test.ex:LINE-2
                    x = 123

                where "y" was given the type:

                    # type: float()
                    # from: types_test.ex:LINE-2
                    y = 456.0

                While Elixir can compare across all types, you are comparing across types \
                which are always disjoint, and the result is either always true or always false
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

  describe ":erlang rewrites" do
    test "Kernel.not/1" do
      assert typecheck!([x], not is_list(x)) == boolean()
      assert typedyn!([x], not is_list(x)) == dynamic(boolean())
    end

    test "Kernel.+/2" do
      assert typeerror!([x = :foo, y = 123], x + y) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.+/2:

                   x + y

               given types:

                   dynamic(:foo), integer()

               but expected one of:

                   #1
                   integer(), integer()

                   #2
                   integer(), float()

                   #3
                   float(), integer()

                   #4
                   float(), float()

               where "x" was given the type:

                   # type: dynamic(:foo)
                   # from: types_test.ex:LINE-1
                   x = :foo

               where "y" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   y = 123
               """
    end

    test "Integer.to_string/1" do
      assert typecheck!([x = 123], Integer.to_string(x)) == binary()
      assert typedyn!([x = 123], Integer.to_string(x)) == dynamic(binary())

      assert typeerror!([x = :foo], Integer.to_string(x)) |> strip_ansi() ==
               ~l"""
               incompatible types given to Integer.to_string/1:

                   Integer.to_string(x)

               given types:

                   dynamic(:foo)

               but expected one of:

                   integer()

               where "x" was given the type:

                   # type: dynamic(:foo)
                   # from: types_test.ex:LINE-1
                   x = :foo
               """
    end

    test "Bitwise.bnot/1" do
      assert typecheck!([x = 123], Bitwise.bnot(x)) == integer()
      assert typedyn!([x = 123], Bitwise.bnot(x)) == dynamic(integer())

      assert typeerror!([x = :foo], Bitwise.bnot(x)) |> strip_ansi() ==
               ~l"""
               incompatible types given to Bitwise.bnot/1:

                   Bitwise.bnot(x)

               given types:

                   dynamic(:foo)

               but expected one of:

                   integer()

               where "x" was given the type:

                   # type: dynamic(:foo)
                   # from: types_test.ex:LINE-1
                   x = :foo
               """
    end
  end

  describe "receive" do
    test "errors on bad timeout" do
      assert typeerror!(
               [x = :timeout],
               receive do
               after
                 x -> :ok
               end
             ) == ~l"""
             expected "after" timeout given to receive to be an integer:

                 x

             but got type:

                 dynamic(:timeout)

             where "x" was given the type:

                 # type: dynamic(:timeout)
                 # from: types_test.ex:LINE-5
                 x = :timeout
             """
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
                "struct UnknownError is undefined (module UnknownError is not available or is yet to be defined). " <>
                  "Make sure the module name is correct and has been specified in full (or that an alias has been defined)"}

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
      # TODO: we are validating the type through the exception but we should actually check the returned type
      assert typeerror!(
               try do
                 :ok
               rescue
                 e in [SyntaxError, RuntimeError] ->
                   e.unknown
               end
             ) ==
               ~l"""
               unknown key .unknown in expression:

                   e.unknown

               where "e" was given the type:

                   # type: %RuntimeError{__exception__: true, message: term()} or
                     %SyntaxError{
                       __exception__: true,
                       column: term(),
                       description: term(),
                       file: term(),
                       line: term(),
                       snippet: term()
                     }
                   # from: types_test.ex:LINE-4
                   rescue e in [SyntaxError, RuntimeError] ->
               """
    end

    test "defines an open map of two fields in anonymous rescue" do
      # TODO: we are validating the type through the exception but we should actually check the returned type
      assert typeerror!(
               try do
                 :ok
               rescue
                 e -> e.message
               end
             ) ==
               ~l"""
               unknown key .message in expression:

                   e.message

               where "e" was given the type:

                   # type: %{..., __exception__: true, __struct__: atom()}
                   # from: types_test.ex:LINE-3
                   rescue e ->

               #{hints(:anonymous_rescue)}
               """
    end

    test "matches on stacktrace" do
      # TODO: we are validating the type through the exception but we should actually check the returned type
      assert typeerror!(
               try do
                 :ok
               rescue
                 _ ->
                   [{_, _, args_or_arity, _} | _] = __STACKTRACE__
                   args_or_arity.fun()
               end
             ) =~ ~l"""
             expected a module (an atom) when invoking fun/0 in expression:

                 args_or_arity.fun()

             where "args_or_arity" was given the type:

                 # type: integer() or list(term())
                 # from: types_test.ex:LINE-3
                 [{_, _, args_or_arity, _} | _] = __STACKTRACE__
             """
    end
  end

  describe "cond" do
    test "always true" do
      assert typecheck!(
               cond do
                 true -> :ok
               end
             ) == atom([:ok])

      assert typecheck!(
               [x, y],
               cond do
                 y -> :y
                 x -> :x
               end
             ) == atom([:x, :y])

      assert typedyn!(
               [x, y],
               cond do
                 y -> :y
                 x -> :x
               end
             ) == dynamic(atom([:x, :y]))

      assert typewarn!(
               [x, y = {:foo, :bar}],
               cond do
                 y -> :y
                 x -> :x
               end
             ) ==
               {atom([:x, :y]),
                ~l"""
                this clause in cond will always match:

                    y

                since it has type:

                    dynamic({:foo, :bar})

                where "y" was given the type:

                    # type: dynamic({:foo, :bar})
                    # from: types_test.ex:LINE-7
                    y = {:foo, :bar}
                """}
    end

    test "always false" do
      assert typewarn!(
               [x, y = false],
               cond do
                 y -> :y
                 x -> :x
               end
             ) ==
               {atom([:x, :y]),
                ~l"""
                this clause in cond will never match:

                    y

                since it has type:

                    dynamic(false)

                where "y" was given the type:

                    # type: dynamic(false)
                    # from: types_test.ex:LINE-7
                    y = false
                """}
    end
  end

  describe "comprehensions" do
    test "binary generators" do
      assert typeerror!([<<x>>], for(<<y <- x>>, do: y)) ==
               ~l"""
               expected the right side of <- in a binary generator to be a binary:

                   x

               but got type:

                   integer()

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   <<x>>

               #{hints(:inferred_bitstring_spec)}
               """
    end
  end
end
