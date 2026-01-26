# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

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
      assert typeerror!([{name, arity}, arity = 123], hd(Atom.to_charlist(name))) |> strip_ansi() ==
               ~l"""
               incompatible types given to Kernel.hd/1:

                   hd(Atom.to_charlist(name))

               given types:

                   empty_list() or non_empty_list(integer())

               but expected one of:

                   non_empty_list(term(), term())

               where "name" was given the type:

                   # type: dynamic(atom())
                   # from: types_test.ex:LINE-1
                   Atom.to_charlist(name)
               """
    end

    test "can be accessed even if they don't match" do
      assert typeerror!(
               (
                 # This will never match, info should not be "corrupted"
                 [info | _] = __ENV__.function
                 info
               )
             ) =~ "the following pattern will never match"
    end

    test "does not check underscore" do
      assert typecheck!(_ = raise("oops")) == none()
    end
  end

  describe "=" do
    test "precedence does not matter" do
      uri_type = typecheck!([x = %URI{}], x)

      assert typecheck!(
               (
                 x = %URI{} = URI.new!("/")
                 x
               )
             ) == uri_type

      assert typecheck!(
               (
                 %URI{} = x = URI.new!("/")
                 x
               )
             ) == uri_type
    end

    test "refines types" do
      assert typecheck!(
               [x, foo = :foo, bar = 123],
               (
                 {^foo, ^bar} = x
                 x
               )
             ) == dynamic(tuple([atom([:foo]), integer()]))
    end

    test "match propagation" do
      assert typecheck!([x = {:ok, y}], is_integer(y), x) ==
               dynamic(tuple([atom([:ok]), integer()]))

      assert typecheck!(
               [x = {:ok, y}],
               (
                 _ = Integer.to_string(y)
                 x
               )
             ) ==
               dynamic(tuple([atom([:ok]), integer()]))
    end

    test "reports incompatible types" do
      assert typeerror!([x = 123 = "123"], x) == ~l"""
             the following pattern will never match:

                 x = 123 = "123"
             """

      assert typeerror!([x = {:ok, _} = {:error, _, _}], x) == ~l"""
             the following pattern will never match:

                 x = {:ok, _} = {:error, _, _}
             """

      assert typeerror!([{x = {:ok, y} = {:error, z, w}}], {x, y, z, w}) == ~l"""
             the following pattern will never match:

                 {x = {:ok, y} = {:error, z, w}}
             """

      assert typeerror!([a = b, a = :foo, b = :bar], {a, b}) == ~l"""
             incompatible types assigned to "a":

                 dynamic(:foo) !~ dynamic(:bar)

             where "a" was given the types:

                 # type: dynamic(:foo)
                 # from: types_test.ex:LINE
                 a = :foo

                 # type: dynamic(:bar)
                 # from: types_test.ex:LINE
                 a = b
             """

      assert typeerror!([{x, _} = {y, _}, x = :foo, y = :bar], {x, y}) == ~l"""
             the following pattern will never match:

                 {x, _} = {y, _}

             where "x" was given the type:

                 # type: dynamic(:foo)
                 # from: types_test.ex:LINE
                 x = :foo

             where "y" was given the type:

                 # type: dynamic(:bar)
                 # from: types_test.ex:LINE
                 y = :bar
             """

      assert typeerror!([{:ok, x} = {:ok, y}], is_integer(x) and is_atom(y), {x, y}) == ~l"""
             the following pattern will never match:

                 {:ok, x} = {:ok, y}

             where "x" was given the type:

                 # type: integer()
                 # from: types_test.ex:LINE
                 is_integer(x)

             where "y" was given the type:

                 # type: dynamic(atom())
                 # from: types_test.ex:LINE
                 is_atom(y)
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
               expected an atom as struct name:

                   %^m{}

               got type:

                   integer()

               where "m" was given the type:

                   # type: integer()
                   # from: types_test.ex:LINE-1
                   m = 123
               """
    end
  end

  describe "maps" do
    test "atom keys in patterns" do
      assert typecheck!([x = %{foo: :bar}], x) == dynamic(open_map(foo: atom([:bar])))
      assert typecheck!([x = %{123 => 456}], x) == dynamic(open_map())
      assert typecheck!([x = %{123 => 456, foo: :bar}], x) == dynamic(open_map(foo: atom([:bar])))
      assert typecheck!([%{foo: :bar = x}], x) == dynamic(atom([:bar]))

      assert typecheck!(
               [
                 {:message, %{slug: slug}},
                 %{assigns: %{app: %{slug: slug} = app}} = root
               ],
               {root, app, slug}
             ) ==
               dynamic(
                 tuple([
                   open_map(assigns: open_map(app: open_map(slug: term()))),
                   open_map(slug: term()),
                   term()
                 ])
               )

      assert typecheck!(
               [
                 %{assigns: %{app: %{slug: slug} = app}} = root,
                 {:message, %{slug: slug}}
               ],
               {root, app, slug}
             ) ==
               dynamic(
                 tuple([
                   open_map(assigns: open_map(app: open_map(slug: term()))),
                   open_map(slug: term()),
                   term()
                 ])
               )
    end

    test "domain keys in patterns" do
      assert typecheck!([x = %{123 => 456}], x) == dynamic(open_map())
      assert typecheck!([x = %{123 => 456, foo: :bar}], x) == dynamic(open_map(foo: atom([:bar])))
      assert typecheck!([%{"123" => :bar = x}], x) == dynamic(atom([:bar]))
    end

    test "pinned variable key in patterns" do
      assert typecheck!(
               (
                 key = 123
                 %{^key => value} = %{123 => :value}
                 value
               )
             ) == atom([:value])

      assert typecheck!(
               [size, map],
               (
                 %{<<123::size(^size)>> => _} = map
                 {size, map}
               )
             ) == dynamic(tuple([integer(), open_map()]))

      assert(
        typecheck!(
          [key, map],
          (
            %{{:module, ^key} => _} = map
            {key, map}
          )
        ) == dynamic(tuple([term(), open_map()]))
      )
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

      assert typecheck!([x = [a = :a, b = :b, c = :c]], {x, a, b, c}) ==
               dynamic(
                 tuple([non_empty_list(atom([:a, :b, :c])), atom([:a]), atom([:b]), atom([:c])])
               )

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

    test "with lists inside tuples inside lists" do
      assert typecheck!([[node_1 = {[arg]}, node_2 = {[arg]}]], {node_1, node_2, arg})
             |> equal?(
               dynamic(
                 tuple([
                   tuple([non_empty_list(term())]),
                   tuple([non_empty_list(term())]),
                   term()
                 ])
               )
             )
    end

    test "regressions" do
      # This is a regression that happens because stacktrace may be
      # either three-element or four-element tuples, and it was failing
      # when we tried to access the fourth element
      assert typecheck!(
               try do
                 raise "oops"
               rescue
                 _ ->
                   [{__MODULE__, fun, _args_or_arity, info} | _] = __STACKTRACE__
                   {fun, length(info)}
               end
             ) == tuple([atom(), integer()])

      assert typecheck!(
               try do
                 raise "oops"
               rescue
                 _ ->
                   [tuple | _] = __STACKTRACE__
                   {__MODULE__, fun, _args_or_arity, info} = tuple
                   {fun, length(info)}
               end
             ) == tuple([atom(), integer()])
    end
  end

  describe "bitstrings" do
    test "alignment" do
      assert typecheck!([<<_>> = x], x) == dynamic(binary())
      assert typecheck!([<<_::1>> = x], x) == dynamic(difference(bitstring(), binary()))
      assert typecheck!([<<_::4, _::4>> = x], x) == dynamic(binary())
      assert typecheck!([<<size, _::size(size)>> = x], x) == dynamic(bitstring())
    end

    test "ok" do
      assert typecheck!([<<x>>], x) == integer()
      assert typecheck!([<<x::float>>], x) == float()
      assert typecheck!([<<x::binary>>], x) == binary()
      assert typecheck!([<<x::utf8>>], x) == integer()
    end

    test "nested" do
      assert typecheck!([<<0, <<x::binary>>::binary>>], x) == binary()

      assert typeerror!([<<0, <<x::bitstring>>::binary>>], x) == ~l"""
             incompatible types in binary matching:

                 <<..., <<x::bitstring>>::binary>>

             got type:

                 bitstring()

             but expected type:

                 binary()

             where "x" was given the type:

                 # type: bitstring()
                 # from: types_test.ex:LINE
                 <<x::bitstring>>
             """
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

    test "pin inference" do
      assert typecheck!(
               [x, y],
               (
                 <<^x>> = y
                 x
               )
             ) == dynamic(integer())
    end

    test "size ok" do
      assert typecheck!([<<x, y, _::size(x - y)>>], :ok) == atom([:ok])
    end

    test "size error" do
      assert typeerror!([<<x::float, _::size(x)>>], :ok) ==
               ~l"""
               expected an integer in binary size:

                   size(x)

               got type:

                   float()

               where "x" was given the type:

                   # type: float()
                   # from: types_test.ex:LINE-1
                   <<x::float, ...>>
               """
    end

    test "size pin inference" do
      assert typecheck!(
               [x, y],
               (
                 <<_::size(^x)>> = y
                 x
               )
             ) == dynamic(integer())
    end
  end

  describe "pin" do
    test "propagates across matches" do
      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case 123 do
                     ^y -> x
                   end
               end
             ) == dynamic(integer())

      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case 123 do
                     ^x -> y
                   end
               end
             ) == dynamic(integer())
    end

    test "propagates across guards" do
      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y when is_integer(y) -> x
               end
             ) == dynamic(integer())
    end

    test "propagates across matches and guards" do
      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case true do
                     true when is_integer(y) -> x
                   end
               end
             ) == dynamic(integer())

      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case true do
                     true when is_integer(x) -> y
                   end
               end
             ) == dynamic(integer())
    end

    test "propagates across binary size in match" do
      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case Process.get(:another) do
                     <<_::size(^y)>> -> x
                   end
               end
             ) == dynamic(integer())

      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case Process.get(:another) do
                     <<_::size(^x)>> -> y
                   end
               end
             ) == dynamic(integer())
    end

    test "propagates across binary size in match inside a key" do
      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case Process.get(:another) do
                     %{<<32::size(^y)>> => _} -> x
                   end
               end
             ) == dynamic(integer())

      assert typecheck!(
               [x],
               case Process.get(:unused) do
                 ^x = y ->
                   case Process.get(:another) do
                     %{<<32::size(^x)>> => _} -> y
                   end
               end
             ) == dynamic(integer())
    end
  end

  describe "guards" do
    test "not" do
      assert typecheck!([x], not x, x) == dynamic(atom([false]))

      assert typecheck!([x], not x.foo, x) == dynamic(open_map(foo: atom([false])))

      assert typeerror!([x], not length(x), x) |> strip_ansi() == ~l"""
             incompatible types given to Kernel.not/1:

                 not length(x)

             given types:

                 integer()

             but expected one of:

                 #1
                 true

                 #2
                 false

             where "x" was given the type:

                 # type: dynamic()
                 # from: types_test.ex:LINE
                 x
             """
    end

    test "is_struct/1" do
      assert typecheck!([x], is_struct(x), x) == dynamic(open_map(__struct__: atom()))
      assert typecheck!([x], is_struct(x, URI), x) == dynamic(open_map(__struct__: atom([URI])))
    end

    test "is_binary/1" do
      assert typecheck!([x], is_binary(x), x) == dynamic(binary())
      assert typecheck!([x], not is_binary(x), x) == dynamic(negation(binary()))

      assert typecheck!([x], is_bitstring(x), x) == dynamic(bitstring())
      assert typecheck!([x], not is_bitstring(x), x) == dynamic(negation(bitstring()))
    end

    test "is_function/2" do
      assert typecheck!([x], is_function(x, 3), x) == dynamic(fun(3))
      assert typecheck!([x], not is_function(x, 3), x) == dynamic(negation(fun(3)))
    end

    test "is_map_key/2" do
      assert typecheck!([x], is_map_key(x, :foo), x) == dynamic(open_map(foo: term()))
      assert typecheck!([x], not is_map_key(x, :foo), x) == dynamic(open_map(foo: not_set()))
    end

    test "elem" do
      assert typecheck!([x], elem(x, 1), x) ==
               dynamic(open_tuple([term(), atom([true])]))

      assert typecheck!([x], not elem(x, 1), x) ==
               dynamic(open_tuple([term(), atom([false])]))

      assert typecheck!([x], is_integer(elem(x, 1)), x) ==
               dynamic(open_tuple([term(), integer()]))
    end

    test "map.field" do
      assert typecheck!([x = %{foo: :bar}], x.bar, x) ==
               dynamic(open_map(foo: atom([:bar]), bar: atom([true])))

      assert typecheck!([x = %{foo: :bar}], not x.bar, x) ==
               dynamic(open_map(foo: atom([:bar]), bar: atom([false])))

      assert typeerror!([x = %Point{}], x.foo_bar, :ok) ==
               ~l"""
               unknown key .foo_bar in expression:

                   x.foo_bar

               the given type does not have the given key:

                   dynamic(%Point{x: term(), y: term(), z: term()})

               where "x" was given the type:

                   # type: dynamic(%Point{})
                   # from: types_test.ex:LINE-1
                   x = %Point{}
               """
    end

    test "when checks" do
      assert typecheck!([x], is_binary(x) when is_atom(x), x) == dynamic(union(binary(), atom()))

      assert typecheck!([x], is_binary(x) when map_size(x) >= 0, x) ==
               dynamic(union(binary(), open_map()))

      assert typecheck!([x], tuple_size(x) >= 0 when map_size(x) >= 0, x) ==
               dynamic(union(tuple(), open_map()))

      assert typecheck!([x, y], is_binary(x) when is_atom(y), {x, y}) ==
               dynamic(tuple([term(), term()]))
    end

    test "conditional checks (andalso/orelse)" do
      assert typecheck!([x], is_binary(x) or is_atom(x), x) == dynamic(union(binary(), atom()))

      assert typecheck!([x], is_binary(x) or map_size(x) >= 0, x) ==
               dynamic(union(binary(), open_map()))

      assert typecheck!([x, y], is_binary(x) or is_atom(y), {x, y}) ==
               dynamic(tuple([term(), term()]))

      assert typecheck!([x, y], is_binary(x) or map_size(y) >= 0, {x, y}) ==
               dynamic(tuple([term(), term()]))

      assert typecheck!([x], not (is_pid(x) and is_atom(x)), x) |> equal?(dynamic(term()))

      assert typecheck!([x, y], not (is_pid(x) and is_atom(y)), {x, y}) ==
               dynamic(tuple([term(), term()]))

      # Error
      assert typeerror!([x], is_pid(x) and is_atom(x), x) == ~l"""
             this guard will never succeed:

                 is_pid(x) and is_atom(x)

             because it returns type:

                 false

             where "x" was given the type:

                 # type: pid()
                 # from: types_test.ex:LINE
                 is_pid(x)
             """

      assert typeerror!([x], (is_binary(x) or is_atom(x)) and is_pid(x), x) == ~l"""
             this guard will never succeed:

                 (is_binary(x) or is_atom(x)) and is_pid(x)

             because it returns type:

                 false

             where "x" was given the type:

                 # type: dynamic(atom() or binary())
                 # from: types_test.ex:LINE
                 is_binary(x) or is_atom(x)
             """
    end

    test "conditional checks (and/or)" do
      assert typecheck!([x], :erlang.or(is_binary(x), is_atom(x)), x) ==
               dynamic(union(binary(), atom()))

      assert typecheck!([x], :erlang.or(is_binary(x), map_size(x) >= 0), x) ==
               dynamic(open_map())

      assert typecheck!([x, y], :erlang.or(is_binary(x), is_atom(y)), {x, y}) ==
               dynamic(tuple([term(), term()]))

      assert typecheck!([x, y], :erlang.or(is_binary(x), map_size(y) >= 0), {x, y}) ==
               dynamic(tuple([term(), open_map()]))

      assert typecheck!([x], not :erlang.and(is_pid(x), is_atom(x)), x) |> equal?(dynamic(term()))

      assert typecheck!([x, y], not :erlang.and(is_pid(x), is_atom(y)), {x, y}) ==
               dynamic(tuple([term(), term()]))

      # Error
      assert typeerror!([x], :erlang.and(is_pid(x), is_atom(x)), x) == ~l"""
             this guard will never succeed:

                 :erlang.and(is_pid(x), is_atom(x))

             because it returns type:

                 false

             where "x" was given the type:

                 # type: pid()
                 # from: types_test.ex:LINE
                 is_pid(x)
             """

      assert typeerror!([x], :erlang.and(:erlang.or(is_binary(x), is_atom(x)), is_pid(x)), x) ==
               ~l"""
               this guard will never succeed:

                   :erlang.and(:erlang.or(is_binary(x), is_atom(x)), is_pid(x))

               because it returns type:

                   false

               where "x" was given the type:

                   # type: dynamic(atom() or binary())
                   # from: types_test.ex:LINE-1
                   :erlang.or(is_binary(x), is_atom(x))
               """
    end

    test "domain checks" do
      # Regular domain check
      assert typecheck!([x, z], length(x) == z, x) == dynamic(list(term()))

      # erlang-or propagates
      assert typecheck!([x, y, z], :erlang.or(length(x) == z, map_size(y) == z), {x, y}) ==
               dynamic(tuple([list(term()), open_map()]))

      # erlang-and propagates
      assert typecheck!([x, y, z], :erlang.and(length(x) == z, map_size(y) == z), {x, y}) ==
               dynamic(tuple([list(term()), open_map()]))

      # or with mixed checks
      assert typecheck!([x, z], length(x) == z or is_map(x), x) ==
               dynamic(list(term()))

      # or does not propagate
      assert typecheck!([x, y, z], length(x) == z or map_size(y) == z, {x, y}) ==
               dynamic(tuple([list(term()), term()]))

      # and propagates
      assert typecheck!([x, y, z], length(x) == z and map_size(y) == z, {x, y}) ==
               dynamic(tuple([list(term()), open_map()]))

      # not or does propagate
      assert typecheck!([x, y, z], not (length(x) == z or map_size(y) == z), {x, y}) ==
               dynamic(tuple([list(term()), open_map()]))

      # not and does not propagate
      assert typecheck!([x, y, z], not (length(x) == z and map_size(y) == z), {x, y}) ==
               dynamic(tuple([list(term()), term()]))
    end

    test "errors in guards" do
      assert typeerror!([x = {}], is_integer(x), x) == ~l"""
             this guard will never succeed:

                 is_integer(x)

             because it returns type:

                 false

             where "x" was given the type:

                 # type: dynamic({})
                 # from: types_test.ex:LINE
                 x = {}
             """
    end
  end

  describe "equality in guards" do
    test "with non-singleton literals" do
      assert typecheck!([x], x == "foo", x) == dynamic(binary())
      assert typecheck!([x], x === "foo", x) == dynamic(binary())
      assert typecheck!([x], not (x == "foo"), x) == dynamic()
      assert typecheck!([x], not (x === "foo"), x) == dynamic()

      assert typecheck!([x], x != "foo", x) == dynamic()
      assert typecheck!([x], x !== "foo", x) == dynamic()
      assert typecheck!([x], not (x != "foo"), x) == dynamic(binary())
      assert typecheck!([x], not (x !== "foo"), x) == dynamic(binary())
    end

    test "with binaries" do
      assert typecheck!([sep, token], token == <<sep::utf8>>, {sep, token}) ==
               dynamic(tuple([integer(), binary()]))
    end

    test "with number literals" do
      assert typecheck!([x], x == 1, x) == dynamic(union(integer(), float()))
      assert typecheck!([x], x === 1, x) == dynamic(integer())
      assert typecheck!([x], not (x == 1), x) == dynamic()
      assert typecheck!([x], not (x === 1), x) == dynamic()

      assert typecheck!([x], x != 1, x) == dynamic()
      assert typecheck!([x], x !== 1, x) == dynamic()
      assert typecheck!([x], not (x != 1), x) == dynamic(union(integer(), float()))
      assert typecheck!([x], not (x !== 1), x) == dynamic(integer())

      assert typecheck!([x], x == 1.0, x) == dynamic(union(integer(), float()))
      assert typecheck!([x], x === 1.0, x) == dynamic(float())
      assert typecheck!([x], not (x == 1.0), x) == dynamic()
      assert typecheck!([x], not (x === 1.0), x) == dynamic()

      assert typecheck!([x], x != 1.0, x) == dynamic()
      assert typecheck!([x], x !== 1.0, x) == dynamic()
      assert typecheck!([x], not (x != 1.0), x) == dynamic(union(integer(), float()))
      assert typecheck!([x], not (x !== 1.0), x) == dynamic(float())
    end

    test "with singleton literals" do
      assert typecheck!([x], x == :foo, x) == dynamic(atom([:foo]))
      assert typecheck!([x], x === :foo, x) == dynamic(atom([:foo]))
      assert typecheck!([x], not (x == :foo), x) == dynamic(negation(atom([:foo])))
      assert typecheck!([x], not (x === :foo), x) == dynamic(negation(atom([:foo])))

      assert typecheck!([x], x != :foo, x) == dynamic(negation(atom([:foo])))
      assert typecheck!([x], x !== :foo, x) == dynamic(negation(atom([:foo])))
      assert typecheck!([x], not (x != :foo), x) == dynamic(atom([:foo]))
      assert typecheck!([x], not (x !== :foo), x) == dynamic(atom([:foo]))

      assert typecheck!([x], x == [], x) == dynamic(empty_list())
      assert typecheck!([x], x === [], x) == dynamic(empty_list())
      assert typecheck!([x], not (x == []), x) == dynamic(negation(empty_list()))
      assert typecheck!([x], not (x === []), x) == dynamic(negation(empty_list()))

      assert typecheck!([x], x != [], x) == dynamic(negation(empty_list()))
      assert typecheck!([x], x !== [], x) == dynamic(negation(empty_list()))
      assert typecheck!([x], not (x != []), x) == dynamic(empty_list())
      assert typecheck!([x], not (x !== []), x) == dynamic(empty_list())
    end

    test "with singleton literals and composite types" do
      assert typecheck!([x], x.key == :ok, x) == dynamic(open_map(key: atom([:ok])))
      assert typecheck!([x], hd(x) == :ok, x) == dynamic(non_empty_list(term(), term()))
      assert typecheck!([x], elem(x, 0) == :ok, x) == dynamic(open_tuple([atom([:ok])]))
    end

    test "with expressions" do
      # With numbers
      assert typecheck!([x, y], x == y and y === 42, {x, y}) ==
               dynamic(tuple([union(integer(), float()), integer()]))

      assert typecheck!([x, y], x == y and x === 42, {x, y}) ==
               dynamic(tuple([integer(), union(integer(), float())]))

      assert typecheck!([x, y], x != y and y === 42, {x, y}) ==
               dynamic(tuple([term(), integer()]))

      assert typecheck!([x, y], x === y and y === 42, {x, y}) ==
               dynamic(tuple([integer(), integer()]))

      assert typecheck!([x, y], x === y and x === 42, {x, y}) ==
               dynamic(tuple([integer(), integer()]))

      assert typecheck!([x, y], x !== y and y === 42, {x, y}) ==
               dynamic(tuple([term(), integer()]))

      # With non-singleton
      assert typecheck!([x, y], x == y and y === "42", {x, y}) ==
               dynamic(tuple([binary(), binary()]))

      assert typecheck!([x, y], x == y and x === "42", {x, y}) ==
               dynamic(tuple([binary(), binary()]))

      assert typecheck!([x, y], x != y and y === "42", {x, y}) ==
               dynamic(tuple([term(), binary()]))

      # With singleton
      assert typecheck!([x, y], x == y and y === :ok, {x, y}) ==
               dynamic(tuple([atom([:ok]), atom([:ok])]))

      assert typecheck!([x, y], x == y and x === :ok, {x, y}) ==
               dynamic(tuple([atom([:ok]), atom([:ok])]))

      assert typecheck!([x, y], x != y and y === :ok, {x, y}) ==
               dynamic(tuple([term(), atom([:ok])]))

      # With composite types
      assert typecheck!([x, y], x == {:ok, y} and y === 42, {x, y}) ==
               dynamic(tuple([tuple([atom([:ok]), union(integer(), float())]), integer()]))

      assert typecheck!([x, y], x != {:ok, y} and y === 42, {x, y}) ==
               dynamic(tuple([term(), integer()]))

      assert typecheck!([x, y], x == elem(y, 0) and y === {1, :ok}, {x, y}) ==
               dynamic(tuple([union(integer(), float()), tuple([integer(), atom([:ok])])]))

      assert typecheck!([x, y], x != elem(y, 0) and y === {1, :ok}, {x, y}) ==
               dynamic(tuple([term(), tuple([integer(), atom([:ok])])]))

      assert typecheck!([x, y], x == elem(y, 1) and y === {1, :ok}, {x, y}) ==
               dynamic(tuple([atom([:ok]), tuple([integer(), atom([:ok])])]))

      assert typecheck!([x, y], x != elem(y, 1) and y === {1, :ok}, {x, y}) ==
               dynamic(tuple([term(), tuple([integer(), atom([:ok])])]))
    end

    test "warnings" do
      assert typeerror!([x = {}], x == 0, x) =~ ~l"""
             comparison between distinct types found:

                 x == 0

             given types:

                 dynamic({}) == integer()
             """

      assert typeerror!([x = {}], x != 0, x) =~ ~l"""
             comparison between distinct types found:

                 x != 0

             given types:

                 dynamic({}) != integer()
             """

      assert typeerror!([x = {}], x == :foo, x) =~ ~l"""
             comparison between distinct types found:

                 x == :foo

             given types:

                 dynamic({}) == :foo
             """

      assert typeerror!([x = {}], not (x != :foo), x) =~ ~l"""
             comparison between distinct types found:

                 x != :foo

             given types:

                 dynamic({}) != :foo
             """

      # We cannot warn in this case because the inference itself will lead to disjoint types
      assert typecheck!([x = {}], not (x == :foo), x) == dynamic(tuple([]))
      assert typecheck!([x = {}], x != :foo, x) == dynamic(tuple([]))
    end
  end

  describe "comparison in guards" do
    test "length equality" do
      assert typecheck!([x], length(x) != 0, x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], not (length(x) != 0), x) == dynamic(empty_list())

      assert typecheck!([x], 0 != length(x), x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], not (0 != length(x)), x) == dynamic(empty_list())
    end

    test "length ordered" do
      assert typecheck!([x], length(x) < 0, x) == dynamic(list(term()))
      assert typecheck!([x], length(x) >= 0, x) == dynamic(list(term()))
      assert typecheck!([x], length(x) <= 0, x) == dynamic(empty_list())

      assert typecheck!([x], 0 <= length(x), x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], 0 >= length(x), x) == dynamic(list(term()))
      assert typecheck!([x], 0 < length(x), x) == dynamic(list(term()))
      assert typecheck!([x], 0 > length(x), x) == dynamic(empty_list())

      assert typecheck!([x], not (length(x) > 0), x) == dynamic(empty_list())
      assert typecheck!([x], not (length(x) < 0), x) == dynamic(list(term()))
      assert typecheck!([x], not (length(x) >= 0), x) == dynamic(list(term()))
      assert typecheck!([x], not (length(x) <= 0), x) == dynamic(non_empty_list(term()))

      assert typecheck!([x], length(x) < 1, x) == dynamic(empty_list())

      assert typecheck!([x], length(x) > 2, x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], length(x) < 2, x) == dynamic(list(term()))
      assert typecheck!([x], length(x) >= 2, x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], length(x) <= 2, x) == dynamic(list(term()))

      assert typecheck!([x], 2 <= length(x), x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], 2 >= length(x), x) == dynamic(list(term()))
      assert typecheck!([x], 2 < length(x), x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], 2 > length(x), x) == dynamic(list(term()))

      assert typecheck!([x], not (length(x) > 2), x) == dynamic(list(term()))
      assert typecheck!([x], not (length(x) < 2), x) == dynamic(non_empty_list(term()))
      assert typecheck!([x], not (length(x) >= 2), x) == dynamic(list(term()))
      assert typecheck!([x], not (length(x) <= 2), x) == dynamic(non_empty_list(term()))
    end

    @non_empty_map difference(open_map(), empty_map())

    test "map_size equality" do
      assert typecheck!([x], map_size(x) == 0, x) == dynamic(empty_map())
      assert typecheck!([x], map_size(x) != 0, x) == dynamic(@non_empty_map)
      assert typecheck!([x], not (map_size(x) == 0), x) == dynamic(@non_empty_map)
      assert typecheck!([x], not (map_size(x) != 0), x) == dynamic(empty_map())

      assert typecheck!([x], 0 == map_size(x), x) == dynamic(empty_map())
      assert typecheck!([x], 0 != map_size(x), x) == dynamic(@non_empty_map)
      assert typecheck!([x], not (0 == map_size(x)), x) == dynamic(@non_empty_map)
      assert typecheck!([x], not (0 != map_size(x)), x) == dynamic(empty_map())
    end

    test "map_size ordered" do
      assert typecheck!([x], map_size(x) > 0, x) == dynamic(@non_empty_map)
      assert typecheck!([x], map_size(x) < 0, x) == dynamic(open_map())
      assert typecheck!([x], map_size(x) >= 0, x) == dynamic(open_map())
      assert typecheck!([x], map_size(x) <= 0, x) == dynamic(empty_map())

      assert typecheck!([x], 0 <= map_size(x), x) == dynamic(@non_empty_map)
      assert typecheck!([x], 0 >= map_size(x), x) == dynamic(open_map())
      assert typecheck!([x], 0 < map_size(x), x) == dynamic(open_map())
      assert typecheck!([x], 0 > map_size(x), x) == dynamic(empty_map())

      assert typecheck!([x], not (map_size(x) > 0), x) == dynamic(empty_map())
      assert typecheck!([x], not (map_size(x) < 0), x) == dynamic(open_map())
      assert typecheck!([x], not (map_size(x) >= 0), x) == dynamic(open_map())
      assert typecheck!([x], not (map_size(x) <= 0), x) == dynamic(@non_empty_map)

      assert typecheck!([x], map_size(x) < 1, x) == dynamic(empty_map())

      assert typecheck!([x], map_size(x) > 2, x) == dynamic(@non_empty_map)
      assert typecheck!([x], map_size(x) < 2, x) == dynamic(open_map())
      assert typecheck!([x], map_size(x) >= 2, x) == dynamic(@non_empty_map)
      assert typecheck!([x], map_size(x) <= 2, x) == dynamic(open_map())

      assert typecheck!([x], 2 <= map_size(x), x) == dynamic(@non_empty_map)
      assert typecheck!([x], 2 >= map_size(x), x) == dynamic(open_map())
      assert typecheck!([x], 2 < map_size(x), x) == dynamic(@non_empty_map)
      assert typecheck!([x], 2 > map_size(x), x) == dynamic(open_map())

      assert typecheck!([x], not (map_size(x) > 2), x) == dynamic(open_map())
      assert typecheck!([x], not (map_size(x) < 2), x) == dynamic(@non_empty_map)
      assert typecheck!([x], not (map_size(x) >= 2), x) == dynamic(open_map())
      assert typecheck!([x], not (map_size(x) <= 2), x) == dynamic(@non_empty_map)
    end

    @non_empty_tuple difference(open_tuple([]), tuple([]))
    @non_binary_tuple difference(open_tuple([]), tuple([term(), term()]))

    @open_binary_tuple open_tuple([term(), term()])
    @open_ternary_tuple open_tuple([term(), term(), term()])
    @non_open_binary_tuple difference(open_tuple([]), open_tuple([term(), term()]))
    @non_open_ternary_tuple difference(open_tuple([]), open_tuple([term(), term(), term()]))

    test "tuple_size equality" do
      assert typecheck!([x], tuple_size(x) == 0, x) == dynamic(tuple([]))
      assert typecheck!([x], tuple_size(x) != 0, x) == dynamic(@non_empty_tuple)
      assert typecheck!([x], not (tuple_size(x) == 0), x) == dynamic(@non_empty_tuple)
      assert typecheck!([x], not (tuple_size(x) != 0), x) == dynamic(tuple([]))

      assert typecheck!([x], 0 == tuple_size(x), x) == dynamic(tuple([]))
      assert typecheck!([x], 0 != tuple_size(x), x) == dynamic(@non_empty_tuple)
      assert typecheck!([x], not (0 == tuple_size(x)), x) == dynamic(@non_empty_tuple)
      assert typecheck!([x], not (0 != tuple_size(x)), x) == dynamic(tuple([]))

      assert typecheck!([x], tuple_size(x) == 2, x) == dynamic(tuple([term(), term()]))
      assert typecheck!([x], tuple_size(x) != 2, x) == dynamic(@non_binary_tuple)
      assert typecheck!([x], not (tuple_size(x) == 2), x) == dynamic(@non_binary_tuple)
      assert typecheck!([x], not (tuple_size(x) != 2), x) == dynamic(tuple([term(), term()]))

      assert typecheck!([x], 2 == tuple_size(x), x) == dynamic(tuple([term(), term()]))
      assert typecheck!([x], 2 != tuple_size(x), x) == dynamic(@non_binary_tuple)
      assert typecheck!([x], not (2 == tuple_size(x)), x) == dynamic(@non_binary_tuple)
      assert typecheck!([x], not (2 != tuple_size(x)), x) == dynamic(tuple([term(), term()]))
    end

    test "tuple_size ordered" do
      assert typecheck!([x], tuple_size(x) > 0, x) == dynamic(open_tuple([term()]))
      assert typecheck!([x], tuple_size(x) < 0, x) == dynamic(open_tuple([]))
      assert typecheck!([x], tuple_size(x) >= 0, x) == dynamic(open_tuple([]))
      assert typecheck!([x], tuple_size(x) <= 0, x) == dynamic(tuple([]))

      assert typecheck!([x], 0 <= tuple_size(x), x) == dynamic(open_tuple([term()]))
      assert typecheck!([x], 0 >= tuple_size(x), x) == dynamic(open_tuple([]))
      assert typecheck!([x], 0 < tuple_size(x), x) == dynamic(open_tuple([]))
      assert typecheck!([x], 0 > tuple_size(x), x) == dynamic(tuple([]))

      assert typecheck!([x], not (tuple_size(x) > 0), x) == dynamic(tuple([]))
      assert typecheck!([x], not (tuple_size(x) < 0), x) == dynamic(open_tuple([]))
      assert typecheck!([x], not (tuple_size(x) >= 0), x) == dynamic(open_tuple([]))
      assert typecheck!([x], not (tuple_size(x) <= 0), x) == dynamic(open_tuple([term()]))

      assert typecheck!([x], tuple_size(x) > 2, x) == dynamic(@open_ternary_tuple)
      assert typecheck!([x], tuple_size(x) < 2, x) == dynamic(@non_open_binary_tuple)
      assert typecheck!([x], tuple_size(x) >= 2, x) == dynamic(@open_binary_tuple)
      assert typecheck!([x], tuple_size(x) <= 2, x) == dynamic(@non_open_ternary_tuple)

      assert typecheck!([x], 2 <= tuple_size(x), x) == dynamic(@open_ternary_tuple)
      assert typecheck!([x], 2 >= tuple_size(x), x) == dynamic(@non_open_binary_tuple)
      assert typecheck!([x], 2 < tuple_size(x), x) == dynamic(@open_binary_tuple)
      assert typecheck!([x], 2 > tuple_size(x), x) == dynamic(@non_open_ternary_tuple)

      assert typecheck!([x], not (tuple_size(x) > 2), x) == dynamic(@non_open_ternary_tuple)
      assert typecheck!([x], not (tuple_size(x) < 2), x) == dynamic(@open_binary_tuple)
      assert typecheck!([x], not (tuple_size(x) >= 2), x) == dynamic(@non_open_binary_tuple)
      assert typecheck!([x], not (tuple_size(x) <= 2), x) == dynamic(@open_ternary_tuple)
    end
  end

  describe "precision" do
    test "literals in patterns" do
      assert precise?([:ok])
      refute precise?([123])
      refute precise?([123.0])
      refute precise?(["string"])
    end

    test "variables in patterns" do
      assert precise?([x])
      assert precise?([x, y])
      refute precise?([x, x])
    end

    test "bitstrings in patterns" do
      assert precise?([<<_::binary>>])
      assert precise?([<<_::bytes>>])
      assert precise?([<<_::bitstring>>])
      assert precise?([<<_::bits>>])
      assert precise?([<<(<<_::binary>>)::bits>>])

      refute precise?([<<>>])
      refute precise?([<<1, _::binary>>])
      refute precise?([<<1, _::bitstring>>])
      refute precise?([<<_::binary-size(8)>>])
      refute precise?([<<_::bitstring-size(8)>>])
      refute precise?([<<(<<123>>)::bits>>])
    end

    test "tuples in patterns" do
      assert precise?([{:ok, _}])
      refute precise?([{:ok, 123}])
    end

    test "maps in patterns" do
      assert precise?([%{ok: _}])
      assert precise?([%URI{path: _}])

      refute precise?([%{ok: 123}])
      refute precise?([%{"foo" => _}])
    end

    test "lists in patterns" do
      assert precise?([[]])
      assert precise?([[_ | _]])
      assert precise?([x, [y | z]])

      refute precise?([[x | x]])
      refute precise?([x, [x | y]])

      refute precise?([[:ok | _]])
      refute precise?([[_ | :ok]])
    end

    test "guards" do
      assert precise?([x], is_integer(x))
      assert precise?([x], not is_integer(x))
      assert precise?([x], is_integer(x) or is_boolean(x))
      assert precise?([x], x == :ok or x == :error)
      assert precise?([x], x.key == :ok)
      assert precise?([x], elem(x, 0) == :ok)
      assert precise?([x], :erlang.map_get(:key, x) == :ok)
      assert precise?([x], x != :ok)
      assert precise?([x], not (x == :ok))
      assert precise?([x], x.key != :ok)
      assert precise?([x], not (x.key != :ok))

      refute precise?([x, y], x == y)
      refute precise?([x], x == 123)
      refute precise?([x], x == 123.0)
      refute precise?([x, y], x == hd(y))
      refute precise?([x], hd(x) == :ok)
    end

    test "sized guards" do
      # Tuples: everything goes
      assert precise?([x], tuple_size(x) == 0)
      assert precise?([x], tuple_size(x) != 0)
      assert precise?([x], tuple_size(x) > 0)
      assert precise?([x], tuple_size(x) >= 0)
      assert precise?([x], tuple_size(x) > 10)
      assert precise?([x], tuple_size(x) < 10)

      # Lists: only when compared to 0
      assert precise?([x], not (length(x) == 0))
      assert precise?([x], length(x) != 0)
      assert precise?([x], not (length(x) > 0))
      assert precise?([x], length(x) >= 0)
      assert precise?([x], length(x) < 1)

      refute precise?([x], length(x) == 1)
      refute precise?([x], length(x) != 1)
      refute precise?([x], length(x) > 1)
      refute precise?([x], length(x) <= 3)

      # Maps: only when compared to 0
      assert precise?([x], map_size(x) == 0)
      assert precise?([x], map_size(x) != 0)
      assert precise?([x], map_size(x) > 0)
      assert precise?([x], map_size(x) >= 0)
      assert precise?([x], map_size(x) < 1)

      refute precise?([x], map_size(x) == 1)
      refute precise?([x], map_size(x) != 1)
      refute precise?([x], map_size(x) > 1)
      refute precise?([x], map_size(x) <= 3)
    end
  end
end
