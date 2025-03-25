# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

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

    test "inference" do
      assert typecheck!(
               [x, y, z],
               (
                 List.to_integer([x, y | z])
                 {x, y, z}
               )
             ) == dynamic(tuple([integer(), integer(), list(integer())]))
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
               dynamic(union(atom([:foo]), non_empty_list(integer(), atom([:foo]))))

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
    test "infers funs" do
      assert typecheck!(
               [x],
               (
                 x.(1, 2)
                 x
               )
             ) == dynamic(fun())
    end

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

    test "infers atoms" do
      assert typecheck!(
               [x],
               (
                 x.foo_bar()
                 x
               )
             ) == dynamic(atom())

      assert typecheck!(
               [x],
               (
                 x.foo_bar(123)
                 x
               )
             ) == dynamic(atom())

      assert typecheck!(
               [x],
               (
                 &x.foo_bar/1
                 x
               )
             ) == dynamic(atom())
    end

    test "infers maps" do
      assert typecheck!(
               [x],
               (
                 :foo = x.foo_bar
                 123 = x.baz_bat
                 x
               )
             ) == dynamic(open_map(foo_bar: atom([:foo]), baz_bat: integer()))
    end

    test "infers args" do
      assert typecheck!(
               [x, y],
               (
                 z = Integer.to_string(x + y)
                 {x, y, z}
               )
             ) == dynamic(tuple([integer(), integer(), binary()]))
    end

    test "undefined function warnings" do
      assert typewarn!(URI.unknown("foo")) ==
               {dynamic(), "URI.unknown/1 is undefined or private"}

      assert typewarn!(if(:rand.uniform() > 0.5, do: URI.unknown("foo"))) ==
               {dynamic() |> union(atom([nil])), "URI.unknown/1 is undefined or private"}

      assert typewarn!(try(do: :ok, after: URI.unknown("foo"))) ==
               {atom([:ok]), "URI.unknown/1 is undefined or private"}

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

    test "calling a function with none()" do
      assert typeerror!(Integer.to_string(raise "oops")) |> strip_ansi() ==
               ~l"""
               incompatible types given to Integer.to_string/1:

                   Integer.to_string(raise RuntimeError.exception("oops"))

               given types:

                   none()

               the 1st argument is empty (often represented as none()), \
               most likely because it is the result of an expression that \
               always fails, such as a `raise` or a previous invalid call. \
               This causes any function called with this value to fail
               """
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
               [<<x::integer>>, y = SomeMod, z],
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

                   # type: dynamic(SomeMod) or integer()
                   # from: types_test.ex:LINE-9
                   mod =
                     cond do
                       z -> x
                       true -> y
                     end
               """
    end

    test "calling a function with invalid arguments on variables" do
      assert typeerror!(
               (
                 x = List
                 x.to_tuple(123)
               )
             )
             |> strip_ansi() ==
               ~l"""
               incompatible types given to List.to_tuple/1:

                   x.to_tuple(123)

               given types:

                   integer()

               but expected one of:

                   list(term())

               where "x" was given the type:

                   # type: List
                   # from: types_test.ex:LINE-5
                   x = List
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

    test "requires all combinations to be compatible (except refinements)" do
      assert typecheck!(
               [condition, arg],
               (
                 # While the code below may raise, it may also always succeed
                 # if condition and arg are passed in tandem. Therefore, we
                 # turn off refinement on dynamic calls.
                 mod = if condition, do: String, else: List
                 res = mod.to_integer(arg)
                 {arg, res}
               )
             ) == tuple([dynamic(), integer()])

      assert typeerror!(
               [condition],
               (
                 arg = if condition, do: "foo", else: [?f, ?o, ?o]
                 mod = if condition, do: String, else: List
                 mod.to_integer(arg)
               )
             )
             |> strip_ansi() == ~l"""
             incompatible types given to List.to_integer/1:

                 mod.to_integer(arg)
                 #=> invoked as List.to_integer/1

             given types:

                 binary() or non_empty_list(integer())

             but expected one of:

                 non_empty_list(integer())

             where "arg" was given the type:

                 # type: binary() or non_empty_list(integer())
                 # from: types_test.ex:LINE-5
                 arg =
                   if condition do
                     "foo"
                   else
                     ~c"foo"
                   end

             where "mod" was given the type:

                 # type: List or String
                 # from: types_test.ex:LINE-4
                 mod =
                   if condition do
                     String
                   else
                     List
                   end
             """
    end
  end

  describe "binaries" do
    test "inference" do
      assert typecheck!(
               [x, y],
               (
                 <<x::float-size(y)>>
                 {x, y}
               )
             ) == dynamic(tuple([union(float(), integer()), integer()]))
    end

    test "warnings" do
      assert typeerror!([<<x::binary-size(2)>>], <<x::float>>) ==
               ~l"""
               incompatible types in binary construction:

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
               incompatible types in binary construction:

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
               incompatible types in binary construction:

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
               expected an integer in binary size:

                   size(x)

               got type:

                   binary()

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

    test "inference" do
      assert typecheck!(
               [x, y],
               (
                 {:ok, :error} = {x, y}
                 {x, y}
               )
             ) == dynamic(tuple([atom([:ok]), atom([:error])]))
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

    test "Tuple.duplicate/2" do
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
      assert typecheck!(%{123 => 456}) == dynamic(open_map())
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

      assert typecheck!(
               (
                 foo_or_bar =
                   cond do
                     :rand.uniform() > 0.5 -> :key1
                     true -> :key2
                   end

                 x = %{key1: :one, key2: :two}
                 %{x | foo_or_bar => :one!, foo_or_bar => :two!}
               )
             )
             |> equal?(
               closed_map(key1: atom([:one]), key2: atom([:two!]))
               |> union(closed_map(key1: atom([:two!]), key2: atom([:one!])))
               |> union(closed_map(key1: atom([:one!]), key2: atom([:two!])))
               |> union(closed_map(key1: atom([:two!]), key2: atom([:two])))
             )

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

      assert typeerror!(
               (
                 x = %{}
                 %{x | x: :zero}
               )
             ) == ~l"""
             expected a map with key :x in map update syntax:

                 %{x | x: :zero}

             but got type:

                 empty_map()

             where "x" was given the type:

                 # type: empty_map()
                 # from: types_test.ex:LINE-3
                 x = %{}
             """

      # Assert we check all possible combinations
      assert typeerror!(
               (
                 foo_or_bar =
                   cond do
                     :rand.uniform() > 0.5 -> :foo
                     true -> :bar
                   end

                 x = %{foo: :baz}
                 %{x | foo_or_bar => :bat}
               )
             ) == ~l"""
             expected a map with key :bar in map update syntax:

                 %{x | foo_or_bar => :bat}

             but got type:

                 %{foo: :baz}

             where "foo_or_bar" was given the type:

                 # type: :bar or :foo
                 # from: types_test.ex:LINE-9
                 foo_or_bar =
                   cond do
                     :rand.uniform() > 0.5 -> :foo
                     true -> :bar
                   end

             where "x" was given the type:

                 # type: %{foo: :baz}
                 # from: types_test.ex:LINE-3
                 x = %{foo: :baz}
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

      # The goal of this assertion is to verify we assert keys,
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
      {type, [diagnostic]} = typediag!(%Point{}.foo_bar)
      assert type == dynamic()
      assert diagnostic.span == {__ENV__.line - 2, 56}

      assert diagnostic.message == ~l"""
             unknown key .foo_bar in expression:

                 %Point{x: nil, y: nil, z: 0}.foo_bar

             the given type does not have the given key:

                 %Point{x: nil, y: nil, z: integer()}
             """
    end

    test "accessing an unknown field on struct in a var with diagnostic" do
      {type, [diagnostic]} = typediag!([x = %URI{}], x.foo_bar)
      assert type == dynamic()
      assert diagnostic.span == {__ENV__.line - 2, 63}

      assert diagnostic.message == ~l"""
             unknown key .foo_bar in expression:

                 x.foo_bar

             the given type does not have the given key:

                 dynamic(%URI{
                   scheme: term(),
                   authority: term(),
                   userinfo: term(),
                   host: term(),
                   port: term(),
                   path: term(),
                   query: term(),
                   fragment: term()
                 })

             where "x" was given the type:

                 # type: dynamic(%URI{})
                 # from: types_test.ex:LINE-4:43
                 x = %URI{}
             """

      assert [%{type: :variable, name: :x}] = diagnostic.details.typing_traces
    end

    test "inspect struct definition" do
      assert typeerror!(
               (
                 p = %Point{x: 123}
                 Integer.to_string(p)
               )
             )
             |> strip_ansi() == ~l"""
             incompatible types given to Integer.to_string/1:

                 Integer.to_string(p)

             given types:

                 %Point{x: integer(), y: nil, z: integer()}

             but expected one of:

                 integer()

             where "p" was given the type:

                 # type: %Point{x: integer(), y: nil, z: integer()}
                 # from: types_test.ex:LINE-4
                 p = %Point{..., x: 123}
             """
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
      assert typedyn!([x = 123, y = 456], x == y) == dynamic(boolean())
    end

    test "using literals" do
      assert typecheck!(:foo == :bar) == boolean()
    end

    test "min/max" do
      assert typecheck!(min(123, 456.0)) == union(integer(), float())
      # min/max uses parametric types, which will carry dynamic regardless of being a strong arrow
      assert typecheck!([x = 123, y = 456.0], min(x, y)) == dynamic(union(integer(), float()))
      assert typedyn!([x = 123, y = 456.0], min(x, y)) == dynamic(union(integer(), float()))
    end

    test "warns when comparison is constant" do
      assert typeerror!([x = :foo, y = 321], min(x, y)) ==
               ~l"""
               comparison between distinct types found:

                   min(x, y)

               given types:

                   min(dynamic(:foo), integer())

               where "x" was given the type:

                   # type: dynamic(:foo)
                   # from: types_test.ex:LINE-1
                   x = :foo

               where "y" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   y = 321

               While Elixir can compare across all types, you are comparing across types \
               which are always disjoint, and the result is either always true or always false
               """

      assert typeerror!([x = 123, y = 456.0], x === y) ==
               ~l"""
               comparison between distinct types found:

                   x === y

               given types:

                   integer() === float()

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   x = 123

               where "y" was given the type:

                   # type: float()
                   # from: types_test.ex:LINE-1
                   y = 456.0

               While Elixir can compare across all types, you are comparing across types \
               which are always disjoint, and the result is either always true or always false
               """
    end

    test "warns on comparison with struct across dynamic call" do
      assert typeerror!([x = %Point{}, y = %Point{}, mod = Kernel], mod.<=(x, y)) ==
               ~l"""
               comparison with structs found:

                   mod.<=(x, y)

               given types:

                   dynamic(%Point{}) <= dynamic(%Point{})

               where "mod" was given the type:

                   # type: dynamic(Kernel)
                   # from: types_test.ex:LINE-1
                   mod = Kernel

               where "x" was given the type:

                   # type: dynamic(%Point{})
                   # from: types_test.ex:LINE-1
                   x = %Point{}

               where "y" was given the type:

                   # type: dynamic(%Point{})
                   # from: types_test.ex:LINE-1
                   y = %Point{}

               Comparison operators (>, <, >=, <=, min, and max) perform structural and not semantic comparison. Comparing with a struct won't give meaningful results. Structs that can be compared typically define a compare/2 function within their modules that can be used for semantic comparison.
               """

      assert typeerror!(
               [x = %Point{}, mod = Kernel, condition],
               (
                 y = if condition, do: 123, else: %Point{}
                 mod.<=(x, y)
               )
             ) =~ "comparison with structs found:"

      assert typecheck!(
               [x = 456, mod = Kernel, condition],
               (
                 y = if condition, do: 123, else: %Point{}
                 mod.<=(x, y)
               )
             ) == boolean()
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

  describe "case" do
    test "does not type check literals" do
      assert typecheck!(
               case :dev do
                 :dev -> :ok
                 :prod -> :error
               end
             ) == atom([:ok, :error])
    end

    test "resets branches" do
      assert typecheck!(
               [x],
               (
                 case :rand.uniform() do
                   y when y < 0.5 -> x.foo
                   y when y > 0.5 -> x.bar()
                 end

                 x
               )
             ) == dynamic()
    end

    test "returns unions of all clauses" do
      assert typecheck!(
               [x],
               case x do
                 :ok -> :ok
                 :error -> :error
               end
             ) == atom([:ok, :error])

      assert typedyn!(
               [x],
               case x do
                 :ok -> :ok
                 :error -> :error
               end
             ) == dynamic(atom([:ok, :error]))
    end

    test "reports error from clause that will never match" do
      assert typeerror!(
               [x],
               case Atom.to_string(x) do
                 :error -> :error
                 x -> x
               end
             ) == ~l"""
             the following clause will never match:

                 :error

             because it attempts to match on the result of:

                 Atom.to_string(x)

             which has type:

                 binary()
             """
    end

    test "reports errors from multiple clauses" do
      {type, [_, _]} =
        typediag!(
          [x],
          case Atom.to_string(x) do
            :ok -> :ok
            :error -> :error
          end
        )

      assert type == atom([:ok, :error])
    end
  end

  describe "conditionals" do
    test "if does not report on literals" do
      assert typecheck!(
               if true do
                 :ok
               end
             ) == atom([:ok, nil])
    end

    test "and does not report on literals" do
      assert typecheck!(false and true) == boolean()
    end

    test "and reports violations" do
      assert typeerror!([x = 123], x and true) =~ """
             the following conditional expression will always evaluate to integer():

                 x
             """
    end
  end

  describe "receive" do
    test "returns unions of all clauses" do
      assert typecheck!(
               receive do
                 :ok -> :ok
                 :error -> :error
               after
                 0 -> :timeout
               end
             ) == atom([:ok, :error, :timeout])

      assert typedyn!(
               receive do
                 :ok -> :ok
                 :error -> :error
               after
                 0 -> :timeout
               end
             ) == dynamic(atom([:ok, :error, :timeout]))
    end

    test "infers type for timeout" do
      assert typecheck!(
               [x],
               receive do
               after
                 x -> x
               end
             ) == dynamic(union(integer(), atom([:infinity])))
    end

    test "resets branches" do
      assert typecheck!(
               [x, timeout = :infinity],
               (
                 receive do
                   y when y > 0.5 -> x.foo
                   _ -> x.bar()
                 after
                   timeout -> <<^x::integer>> = :crypto.strong_rand_bytes(1)
                 end

                 x
               )
             ) == dynamic()
    end

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

      # Check for compatibility, not subtyping
      assert typeerror!(
               [<<x::integer, y::float>>],
               receive do
               after
                 if(:rand.uniform(), do: x, else: y) -> :ok
               end
             ) =~ "expected "
    after
      " timeout given to receive to be an integer"
    end
  end

  describe "try" do
    test "returns unions of all clauses" do
      assert typecheck!(
               try do
                 :do
               rescue
                 _ -> :rescue
               catch
                 :caught -> :caught1
                 :throw, :caught -> :caught2
               after
                 :not_used
               end
             ) == atom([:do, :caught1, :caught2, :rescue])

      assert typecheck!(
               [x],
               try do
                 x
               rescue
                 _ -> :rescue
               catch
                 :caught -> :caught1
                 :throw, :caught -> :caught2
               after
                 :not_used
               else
                 :match -> :else1
                 _ -> :else2
               end
             ) == atom([:caught1, :caught2, :rescue, :else1, :else2])
    end

    test "resets branches (except after)" do
      assert typecheck!(
               [x],
               (
                 try do
                   <<^x::float>> = :crypto.strong_rand_bytes(8)
                 rescue
                   ArgumentError -> x.foo
                 catch
                   _, _ -> x.bar()
                 after
                   <<^x::integer>> = :crypto.strong_rand_bytes(8)
                 end

                 x
               )
             ) == dynamic(integer())
    end

    test "reports error from clause that will never match" do
      assert typeerror!(
               [x],
               try do
                 Atom.to_string(x)
               rescue
                 _ -> :ok
               else
                 :error -> :error
                 x -> x
               end
             ) == ~l"""
             the following clause will never match:

                 :error

             it attempts to match on the result of the try do-block which has incompatible type:

                 binary()
             """
    end

    test "warns on undefined exceptions" do
      assert typewarn!(
               try do
                 :ok
               rescue
                 e in UnknownError -> e
               end
             ) ==
               {dynamic() |> union(atom([:ok])),
                "struct UnknownError is undefined (module UnknownError is not available or is yet to be defined). " <>
                  "Make sure the module name is correct and has been specified in full (or that an alias has been defined)"}

      assert typewarn!(
               try do
                 :ok
               rescue
                 e in Enumerable -> e
               end
             ) ==
               {dynamic() |> union(atom([:ok])),
                "struct Enumerable is undefined (there is such module but it does not define a struct)"}
    end

    test "defines unions of exceptions in rescue" do
      assert typecheck!(
               try do
                 raise "oops"
               rescue
                 e in [RuntimeError, ArgumentError] ->
                   e
               end
             ) ==
               dynamic(
                 union(
                   closed_map(
                     __struct__: atom([ArgumentError]),
                     __exception__: atom([true]),
                     message: term()
                   ),
                   closed_map(
                     __struct__: atom([RuntimeError]),
                     __exception__: atom([true]),
                     message: term()
                   )
                 )
               )
    end

    test "generates custom traces" do
      assert typeerror!(
               try do
                 raise "oops"
               rescue
                 e ->
                   Integer.to_string(e)
               end
             )
             |> strip_ansi() == ~l"""
             incompatible types given to Integer.to_string/1:

                 Integer.to_string(e)

             given types:

                 %{..., __exception__: true, __struct__: atom()}

             but expected one of:

                 integer()

             where "e" was given the type:

                 # type: %{..., __exception__: true, __struct__: atom()}
                 # from: types_test.ex
                 rescue e

             hint: when you rescue without specifying exception names, the variable is assigned a type of a struct but all of its fields are unknown. If you are trying to access an exception's :message key, either specify the exception names or use `Exception.message/1`.
             """
    end

    test "defines an open map of two fields in anonymous rescue" do
      assert typecheck!(
               try do
                 raise "oops"
               rescue
                 e -> e
               end
             ) ==
               open_map(
                 __struct__: atom(),
                 __exception__: atom([true])
               )
    end

    test "matches on stacktrace" do
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

    test "resets branches" do
      assert typecheck!(
               [x],
               (
                 cond do
                   :rand.uniform() > 0.5 -> x.foo
                   true -> x.bar()
                 end

                 x
               )
             ) == dynamic()
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

      # Check for compatibility, not subtyping
      assert typeerror!(
               [<<x::integer, y::binary>>],
               for(<<i <- if(:rand.uniform() > 0.5, do: x, else: y)>>, do: i)
             ) =~
               ~l"""
               expected the right side of <- in a binary generator to be a binary:

                   if :rand.uniform() > 0.5 do
                     x
                   else
                     y
                   end

               but got type:

                   binary() or integer()

               where "x" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-3
                   <<x::integer, ...>>

               where "y" was given the type:

                   # type: binary()
                   # from: types_test.ex:LINE-3
                   <<..., y::binary>>
               """
    end

    test "infers binary generators" do
      assert typecheck!(
               [x],
               (
                 for <<_ <- x>>, do: :ok
                 x
               )
             ) == dynamic(binary())
    end

    test ":into" do
      assert typecheck!([binary], for(<<x <- binary>>, do: x)) == list(integer())
      assert typecheck!([binary], for(<<x <- binary>>, do: x, into: [])) == list(integer())
      assert typecheck!([binary], for(<<x <- binary>>, do: x, into: "")) == binary()
      assert typecheck!([binary, other], for(<<x <- binary>>, do: x, into: other)) == dynamic()

      assert typecheck!([enum], for(x <- enum, do: x)) == list(dynamic())
      assert typecheck!([enum], for(x <- enum, do: x, into: [])) == list(dynamic())
      assert typecheck!([enum], for(x <- enum, do: x, into: "")) == binary()
      assert typecheck!([enum, other], for(x <- enum, do: x, into: other)) == dynamic()

      assert typecheck!(
               [binary],
               (
                 into = if :rand.uniform() > 0.5, do: [], else: "0"
                 for(<<x::float <- binary>>, do: x, into: into)
               )
             ) == union(binary(), list(float()))

      assert typecheck!(
               [binary, empty_list = []],
               (
                 into = if :rand.uniform() > 0.5, do: empty_list, else: "0"
                 for(<<x::float <- binary>>, do: x, into: into)
               )
             ) == dynamic(union(binary(), list(float())))
    end

    test ":reduce checks" do
      assert typecheck!(
               [list],
               for _ <- list, reduce: :ok do
                 :ok -> 1
                 _ -> 2.0
               end
             ) == union(atom([:ok]), union(integer(), float()))
    end

    test ":reduce inference" do
      assert typecheck!(
               [list, x],
               (
                 123 =
                   for _ <- list, reduce: x do
                     x -> x
                   end

                 x
               )
             ) == dynamic(integer())
    end
  end

  describe "info" do
    test "__info__/1" do
      assert typecheck!(GenServer.__info__(:functions)) == list(tuple([atom(), integer()]))

      assert typewarn!(:string.__info__(:functions)) ==
               {dynamic(), ":string.__info__/1 is undefined or private"}

      assert typecheck!([x], x.__info__(:functions)) == list(tuple([atom(), integer()]))

      assert typeerror!([x], x.__info__(:whatever)) |> strip_ansi() =~ """
             incompatible types given to __info__/1:

                 x.__info__(:whatever)

             given types:

                 :whatever
             """
    end

    test "__info__/1 for struct information" do
      assert typecheck!(GenServer.__info__(:struct)) == atom([nil])

      assert typecheck!(URI.__info__(:struct)) ==
               list(closed_map(default: if_set(term()), field: atom()))

      assert typecheck!([x], x.__info__(:struct)) ==
               list(closed_map(default: if_set(term()), field: atom())) |> union(atom([nil]))
    end

    test "behaviour_info/1" do
      assert typecheck!([x], x.behaviour_info(:callbacks)) == list(tuple([atom(), integer()]))

      assert typecheck!(GenServer.behaviour_info(:callbacks)) == list(tuple([atom(), integer()]))

      assert typewarn!(String.behaviour_info(:callbacks)) ==
               {dynamic(), "String.behaviour_info/1 is undefined or private"}
    end

    test "module_info/1" do
      assert typecheck!([x], x.module_info(:exports)) == list(tuple([atom(), integer()]))
      assert typecheck!(GenServer.module_info(:exports)) == list(tuple([atom(), integer()]))
    end

    test "module_info/0" do
      assert typecheck!([x], x.module_info()) |> subtype?(list(tuple([atom(), term()])))
      assert typecheck!(GenServer.module_info()) |> subtype?(list(tuple([atom(), term()])))
    end
  end
end
