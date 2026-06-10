# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("type_helper.exs", __DIR__)

defmodule NoFieldsStruct do
  defstruct []
end

defmodule Decimal do
  defstruct [:sign, :coef, :exp]
end

defmodule Module.Types.DescrTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr
  defmacro domain_key(arg) when is_atom(arg), do: [arg]

  defp number(), do: opt_union(integer(), float())
  defp empty_tuple(), do: tuple([])
  defp tuple_of_size_at_least(n) when is_integer(n), do: open_tuple(List.duplicate(term(), n))
  defp tuple_of_size(n) when is_integer(n) and n >= 0, do: tuple(List.duplicate(term(), n))

  defp list(elem_type, tail_type),
    do: opt_union(empty_list(), non_empty_list(elem_type, tail_type))

  defp map_with_default(descr), do: open_map([{to_domain_keys(:term), descr}])

  defp projected_negative_map(size) do
    Enum.reduce(1..size, open_map(k: open_map(), x: term()), fn index, acc ->
      opt_difference(
        acc,
        open_map([
          {:k, open_map([{:"value#{index}", integer()}])},
          {:"field#{index}", integer()}
        ])
      )
    end)
  end

  defp projected_negative_tuple(size) do
    Enum.reduce(1..size, open_tuple([open_tuple([]), term()]), fn index, acc ->
      opt_difference(
        acc,
        open_tuple([
          open_tuple([atom([:"value#{index}"])]),
          integer()
        ])
      )
    end)
  end

  describe "union" do
    test "bitmap" do
      assert opt_union(integer(), float()) == opt_union(float(), integer())
    end

    test "term" do
      assert opt_union(term(), float()) == term()
      assert opt_union(term(), binary()) == term()
      assert opt_union(term(), if_set(binary())) == if_set(term())
    end

    test "none" do
      assert opt_union(none(), float()) == float()
      assert opt_union(none(), binary()) == binary()
    end

    test "atom" do
      assert opt_union(atom(), atom([:a])) == atom()
      assert opt_union(atom([:a]), atom([:b])) == atom([:a, :b])
      assert opt_union(atom([:a]), opt_negation(atom([:b]))) == opt_negation(atom([:b]))

      assert opt_union(opt_negation(atom([:a, :b])), opt_negation(atom([:b, :c])))
             |> equal?(opt_negation(atom([:b])))
    end

    test "all primitive types" do
      all = [
        atom(),
        integer(),
        float(),
        bitstring(),
        open_map(),
        non_empty_list(term(), term()),
        empty_list(),
        tuple(),
        fun(),
        pid(),
        port(),
        reference()
      ]

      assert Enum.reduce(all, &opt_union/2) |> equal?(term())
    end

    test "dynamic" do
      assert equal?(opt_union(dynamic(), dynamic()), dynamic())
      assert equal?(opt_union(dynamic(), term()), term())
      assert equal?(opt_union(term(), dynamic()), term())

      assert equal?(opt_union(dynamic(atom()), atom()), atom())
      refute equal?(opt_union(dynamic(atom()), atom()), dynamic(atom()))

      assert equal?(
               opt_union(term(), dynamic(if_set(integer()))),
               opt_union(term(), dynamic(not_set()))
             )

      refute equal?(
               opt_union(term(), dynamic(if_set(integer()))),
               dynamic(opt_union(term(), not_set()))
             )
    end

    test "tuple" do
      assert equal?(opt_union(tuple(), tuple()), tuple())

      t = tuple([integer(), atom()])
      assert equal?(opt_union(t, t), t)

      assert opt_union(tuple([integer(), atom()]), tuple([float(), atom()]))
             |> equal?(tuple([opt_union(integer(), float()), atom()]))

      assert opt_union(tuple([integer(), atom()]), tuple([integer(), binary()]))
             |> equal?(tuple([integer(), opt_union(atom(), binary())]))

      assert open_tuple([atom()])
             |> opt_union(tuple([atom(), integer()]))
             |> equal?(open_tuple([atom()]))

      assert tuple([opt_union(integer(), atom())])
             |> opt_difference(open_tuple([atom()]))
             |> equal?(tuple([integer()]))

      # Test union of open tuple
      # We assert using == on purpose as we want to return open tuples
      assert opt_union(open_tuple([]), tuple([atom()])) == open_tuple([])

      assert opt_union(open_tuple([]), opt_difference(open_tuple([]), tuple([atom()]))) ==
               open_tuple([])

      assert opt_union(opt_difference(open_tuple([]), tuple([atom()])), open_tuple([])) ==
               open_tuple([])
    end

    test "map" do
      assert equal?(opt_union(open_map(), open_map()), open_map())
      assert equal?(opt_union(closed_map(a: integer()), open_map()), open_map())

      assert equal?(
               opt_union(closed_map(a: integer()), opt_negation(closed_map(a: integer()))),
               term()
             )

      a_integer_open = open_map(a: integer())
      assert equal?(opt_union(closed_map(a: integer()), a_integer_open), a_integer_open)

      closed = closed_map(a: integer(), b: atom())
      open = open_map(a: integer(), b: boolean())

      assert subtype?(closed, opt_union(closed, open))
      assert subtype?(open, opt_union(closed, open))
      assert subtype?(closed, opt_union(open, closed))
      assert subtype?(open, opt_union(open, closed))

      # Domain key types
      atom_to_atom = open_map([{domain_key(:atom), atom()}])
      atom_to_integer = open_map([{domain_key(:atom), integer()}])

      # Test union identity and different type maps
      assert opt_union(atom_to_atom, atom_to_atom) == atom_to_atom

      # Test subtype relationships with domain key maps
      refute open_map([{domain_key(:atom), opt_union(atom(), integer())}])
             |> subtype?(opt_union(atom_to_atom, atom_to_integer))

      assert opt_union(atom_to_atom, atom_to_integer)
             |> subtype?(open_map([{domain_key(:atom), opt_union(atom(), integer())}]))

      # Test unions with empty map
      assert opt_union(empty_map(), open_map([{domain_key(:integer), atom()}]))
             |> equal?(open_map([{domain_key(:integer), atom()}]))

      # Test union of open map
      # We assert using == on purpose as we want to return open maps
      assert opt_union(open_map(), open_map([{domain_key(:integer), atom()}])) == open_map()

      assert opt_union(open_map(), opt_difference(open_map(), closed_map(foo: atom()))) ==
               open_map()

      assert opt_union(opt_difference(open_map(), closed_map(foo: atom())), open_map()) ==
               open_map()

      # Ensure no duplicate, no matter the order
      assert opt_union(
               open_map(a: integer()),
               open_map(a: number(), b: binary())
             )
             |> opt_union(open_map(a: integer())) ==
               opt_union(
                 open_map(a: number(), b: binary()),
                 open_map(a: integer())
               )
               |> opt_union(open_map(a: integer()))
    end

    test "list" do
      assert opt_union(list(term()), list(term())) |> equal?(list(term()))
      assert opt_union(list(integer()), list(term())) |> equal?(list(term()))

      assert opt_union(opt_difference(list(term()), list(integer())), list(integer()))
             |> equal?(list(term()))
    end

    test "fun" do
      assert equal?(opt_union(fun(), fun()), fun())
      assert equal?(opt_union(fun(), none_fun(1)), fun())

      dynamic_fun = opt_intersection(fun(), dynamic())
      assert equal?(opt_union(dynamic_fun, fun()), fun())
    end

    test "optimizations (maps)" do
      # The tests are checking the actual implementation, not the semantics.
      # This is why we are using structural comparisons.
      # It's fine to remove these if the implementation changes, but breaking
      # these might have an important impact on compile times.

      # Optimization one: same tags, all but one key are structurally equal
      assert opt_union(
               open_map(a: float(), b: atom()),
               open_map(a: integer(), b: atom())
             )
             |> equal?(open_map(a: opt_union(float(), integer()), b: atom()))

      assert opt_union(
               closed_map(a: float(), b: atom()),
               closed_map(a: integer(), b: atom())
             ) == closed_map(a: opt_union(float(), integer()), b: atom())

      # Optimization two: we can tell that one map is a subtype of the other:
      assert opt_union(
               closed_map(a: term(), b: term()),
               closed_map(a: float(), b: binary())
             ) == closed_map(a: term(), b: term())

      assert opt_union(
               open_map(a: term()),
               closed_map(a: float(), b: binary())
             ) == open_map(a: term())

      assert opt_union(
               closed_map(a: float(), b: binary()),
               open_map(a: term())
             ) == open_map(a: term())

      assert opt_union(
               closed_map(a: term(), b: tuple([term(), term()])),
               closed_map(a: float(), b: tuple([atom(), binary()]))
             ) == closed_map(a: term(), b: tuple([term(), term()]))
    end

    test "optimizations (tuples)" do
      # Optimization one: same tags, all but one key are structurally equal
      assert opt_union(
               open_tuple([float(), atom()]),
               open_tuple([integer(), atom()])
             ) == open_tuple([opt_union(float(), integer()), atom()])

      assert opt_union(
               tuple([float(), atom()]),
               tuple([integer(), atom()])
             ) == tuple([opt_union(float(), integer()), atom()])

      # Optimization two: we can tell that one tuple is a subtype of the other
      assert opt_union(
               tuple([term(), term()]),
               tuple([float(), binary()])
             ) == tuple([term(), term()])

      assert opt_union(
               open_tuple([term()]),
               tuple([float(), binary()])
             ) == open_tuple([term()])

      assert opt_union(
               tuple([float(), binary()]),
               open_tuple([term()])
             ) == open_tuple([term()])
    end
  end

  describe "if_set" do
    test "preserves static parts alongside dynamic term" do
      type = opt_union(atom([:value]), dynamic()) |> if_set()

      assert equal?(type, opt_union(if_set(atom([:value])), dynamic(if_set(term()))))
      refute equal?(type, dynamic(if_set(term())))
    end
  end

  describe "intersection" do
    test "bitmap" do
      assert opt_intersection(integer(), opt_union(integer(), float())) == integer()
      assert opt_intersection(integer(), float()) == none()
    end

    test "term" do
      assert opt_intersection(term(), term()) == term()
      assert opt_intersection(term(), float()) == float()
      assert opt_intersection(term(), binary()) == binary()
    end

    test "none" do
      assert opt_intersection(none(), float()) == none()
      assert opt_intersection(none(), binary()) == none()
    end

    test "atom" do
      assert opt_intersection(atom(), atom()) == atom()
      assert opt_intersection(atom(), atom([:a])) == atom([:a])
      assert opt_intersection(atom([:a]), atom([:b])) == none()
      assert opt_intersection(atom([:a]), opt_negation(atom([:b]))) == atom([:a])
    end

    test "dynamic" do
      assert equal?(opt_intersection(dynamic(), dynamic()), dynamic())
      assert equal?(opt_intersection(dynamic(), term()), dynamic())
      assert equal?(opt_intersection(term(), dynamic()), dynamic())
      assert empty?(opt_intersection(dynamic(), none()))
      assert empty?(opt_intersection(opt_intersection(dynamic(), atom()), integer()))

      assert empty?(opt_intersection(dynamic(not_set()), term()))
      refute empty?(opt_intersection(dynamic(if_set(integer())), term()))

      # Check for structural equivalence
      assert opt_intersection(dynamic(not_set()), term()) == none()
      assert equal?(opt_intersection(if_set(dynamic(integer())), term()), dynamic(integer()))

      assert equal?(
               opt_intersection(if_set(opt_union(atom(), dynamic())), term()),
               opt_union(atom(), dynamic())
             )
    end

    test "tuple" do
      assert empty?(opt_intersection(open_tuple([atom()]), open_tuple([integer()])))

      assert opt_intersection(open_tuple([atom()]), tuple([term(), integer()]))
             |> equal?(tuple([atom(), integer()]))

      assert opt_intersection(tuple([term(), integer()]), tuple([atom(), term()]))
             |> equal?(tuple([atom(), integer()]))

      empty_field =
        closed_map(key: atom([:value]))
        |> opt_difference(open_map(key: atom(), optional: if_set(atom())))

      assert empty?(empty_field)
      refute empty_field == none()

      assert opt_intersection(open_tuple([integer()]), tuple([integer(), empty_field]))
             |> equal?(none())

      assert opt_intersection(tuple([integer(), empty_field]), open_tuple([integer()]))
             |> equal?(none())

      assert opt_intersection(tuple(), tuple([integer(), empty_field])) |> equal?(none())
    end

    test "map" do
      assert opt_intersection(open_map(), open_map()) == open_map()

      assert equal?(
               opt_intersection(closed_map(a: integer()), open_map()),
               closed_map(a: integer())
             )

      assert equal?(
               opt_intersection(closed_map(a: integer()), open_map(a: integer())),
               closed_map(a: integer())
             )

      assert opt_intersection(closed_map(a: integer()), open_map(b: not_set()))
             |> equal?(closed_map(a: integer()))

      assert opt_intersection(closed_map(a: integer()), open_map(b: if_set(integer())))
             |> equal?(closed_map(a: integer()))

      assert equal?(
               opt_intersection(closed_map(a: integer()), closed_map(a: if_set(integer()))),
               closed_map(a: integer())
             )

      assert empty?(opt_intersection(closed_map(a: integer()), closed_map(a: atom())))

      # Maps leaves are actually optimized, so some of the code branches
      # can only be tested through negations. This is the intersection between
      # open_map(a: integer()) and open_map(b: integer())
      a_and_b =
        opt_negation(
          opt_union(opt_negation(open_map(a: integer())), opt_negation(open_map(b: integer())))
        )

      assert equal?(
               # The additional parts we are intersecting are empty
               opt_intersection(
                 opt_union(a_and_b, closed_map(c: float())),
                 opt_union(a_and_b, closed_map(d: float()))
               ),
               a_and_b
             )

      # This is a regression triggered by an optimization
      assert opt_intersection(
               closed_map(tag: atom([true]), halted: atom([true]), assigns: term()),
               opt_union(
                 closed_map(tag: atom([true]), halted: atom([true]), assigns: term()),
                 closed_map(tag: atom([true]), halted: term(), assigns: open_map())
               )
             )
             |> equal?(closed_map(tag: atom([true]), halted: atom([true]), assigns: term()))
    end

    # Closed maps with not set keys should have no impact
    test "map closed with not set keys" do
      assert opt_intersection(closed_map(a: integer()), closed_map(a: integer(), b: not_set())) ==
               closed_map(a: integer())

      assert opt_intersection(closed_map(a: integer(), b: not_set()), closed_map(a: integer())) ==
               closed_map(a: integer())

      assert opt_intersection(closed_map(a: integer()), closed_map(a: not_set())) ==
               none()

      assert opt_intersection(
               closed_map(a: integer(), b: not_set()),
               closed_map(a: integer(), c: not_set())
             ) ==
               closed_map(a: integer())

      assert opt_intersection(empty_map(), closed_map(a: if_set(integer()))) == empty_map()
      assert opt_intersection(closed_map(a: if_set(integer())), empty_map()) == empty_map()
      refute disjoint?(empty_map(), closed_map(a: if_set(integer())))
    end

    test "map with domain keys" do
      # %{..., int => t1, atom => t2} and %{int => t3}
      # intersection is %{int => t1 and t3, atom => none}
      map1 = open_map([{domain_key(:integer), integer()}, {domain_key(:atom), atom()}])
      map2 = closed_map([{domain_key(:integer), number()}])

      intersection = opt_intersection(map1, map2)

      expected =
        closed_map([{domain_key(:integer), integer()}, {domain_key(:atom), none()}])

      assert equal?(intersection, expected)

      # %{..., int => t1, atom => t2} and %{int => t3, pid => t4}
      # intersection is %{int =>t1 and t3, atom => none, pid => t4}
      map1 = open_map([{domain_key(:integer), integer()}, {domain_key(:atom), atom()}])
      map2 = closed_map([{domain_key(:integer), float()}, {domain_key(:pid), binary()}])

      intersection = opt_intersection(map1, map2)

      expected =
        closed_map([
          {domain_key(:integer), opt_intersection(integer(), float())},
          {domain_key(:atom), none()},
          {domain_key(:pid), binary()}
        ])

      assert equal?(intersection, expected)

      # %{..., int => t1, string => t3} and %{int => t4}
      # intersection is %{int => t1 and t4, string => none}
      map1 = open_map([{domain_key(:integer), integer()}, {domain_key(:binary), binary()}])
      map2 = closed_map([{domain_key(:integer), float()}])

      intersection = opt_intersection(map1, map2)

      assert equal?(
               intersection,
               closed_map([
                 {domain_key(:integer), opt_intersection(integer(), float())},
                 {domain_key(:binary), none()}
               ])
             )

      assert subtype?(empty_map(), closed_map([{domain_key(:integer), atom()}]))

      t1 = closed_map([{domain_key(:integer), atom()}])
      t2 = closed_map([{domain_key(:integer), binary()}])
      assert equal?(opt_intersection(t1, t2), empty_map())

      t1 = closed_map([{domain_key(:integer), atom()}])
      t2 = closed_map([{domain_key(:atom), term()}])
      refute empty?(opt_intersection(t1, t2))
      assert equal?(opt_intersection(t1, t2), empty_map())
    end

    test "list" do
      assert opt_intersection(list(term()), list(term())) == list(term())
      assert opt_intersection(list(integer()), list(integer())) == list(integer())
      assert opt_intersection(list(integer()), list(number())) == list(integer())
      assert opt_intersection(list(integer()), list(atom())) == empty_list()

      # Empty list intersections
      assert opt_intersection(empty_list(), list(term())) == empty_list()
      assert opt_intersection(empty_list(), list(integer())) == empty_list()
      assert opt_intersection(empty_list(), empty_list()) == empty_list()

      # List with any type
      assert opt_intersection(list(term()), list(integer())) == list(integer())
      assert opt_intersection(list(term()), list(integer())) == list(integer())

      # Intersection with more specific types
      assert opt_intersection(list(integer()), list(atom([:a, :b]))) == empty_list()

      # Intersection with union types
      assert opt_intersection(list(opt_union(integer(), atom())), list(number())) ==
               list(integer())

      # Intersection with dynamic
      assert equal?(
               opt_intersection(dynamic(list(term())), list(integer())),
               dynamic(list(integer()))
             )

      assert equal?(opt_intersection(dynamic(list(term())), list(term())), dynamic(list(term())))

      # Nested list intersections
      assert opt_intersection(list(list(integer())), list(list(number()))) ==
               list(list(integer()))

      assert opt_intersection(list(list(integer())), list(list(atom()))) == list(empty_list())

      # Intersection with non-list types
      assert opt_intersection(list(integer()), integer()) == none()

      # Tests for list with last element
      assert opt_intersection(list(float(), atom()), list(number(), term())) ==
               list(float(), atom())

      assert opt_intersection(list(number(), atom()), list(float(), boolean())) ==
               list(float(), boolean())

      assert opt_intersection(list(integer(), float()), list(number(), integer())) == empty_list()

      # Empty list with last element
      assert opt_intersection(empty_list(), list(integer(), atom())) == empty_list()
      assert opt_intersection(list(integer(), atom()), empty_list()) == empty_list()

      # List with any type and specific last element
      assert opt_intersection(list(term(), atom()), list(float(), boolean())) ==
               list(float(), boolean())

      assert opt_intersection(list(term(), term()), list(float(), atom())) ==
               list(float(), atom())

      # Nested lists with last element
      assert opt_intersection(list(list(integer()), atom()), list(list(number()), boolean())) ==
               list(list(integer()), boolean())

      assert list(list(integer(), atom()), float())
             |> opt_intersection(list(list(number(), boolean()), integer())) == empty_list()

      # Union types in last element
      assert opt_intersection(
               list(integer(), opt_union(atom(), binary())),
               list(number(), atom())
             ) ==
               list(integer(), atom())

      # Dynamic with last element
      assert opt_intersection(dynamic(list(term(), atom())), list(integer(), boolean()))
             |> equal?(dynamic(list(integer(), boolean())))

      # Intersection with proper list (should result in empty list)
      assert opt_intersection(list(integer(), atom()), list(integer())) == empty_list()
    end

    test "function" do
      assert not empty?(opt_intersection(opt_negation(none_fun(2)), opt_negation(none_fun(3))))
    end
  end

  describe "difference" do
    test "bitmap" do
      assert opt_difference(float(), integer()) == float()
      assert opt_difference(opt_union(float(), integer()), integer()) == float()

      assert opt_difference(opt_union(float(), integer()), binary()) ==
               opt_union(float(), integer())
    end

    test "term" do
      assert opt_difference(float(), term()) == none()
      assert opt_difference(integer(), term()) == none()
    end

    test "none" do
      assert opt_difference(none(), integer()) == none()
      assert opt_difference(none(), float()) == none()

      assert opt_difference(integer(), none()) == integer()
      assert opt_difference(float(), none()) == float()
    end

    test "atom" do
      assert opt_difference(atom([:a]), atom()) == none()
      assert opt_difference(atom([:a]), atom([:b])) == atom([:a])
    end

    test "dynamic" do
      assert equal?(dynamic(), opt_difference(dynamic(), dynamic()))
      assert equal?(dynamic(), opt_difference(term(), dynamic()))
      assert empty?(opt_difference(dynamic(), term()))
      assert empty?(opt_difference(none(), dynamic()))
      assert empty?(opt_difference(dynamic(integer()), integer()))
    end

    test "tuple" do
      assert empty?(opt_difference(open_tuple([atom()]), open_tuple([term()])))
      refute empty?(opt_difference(tuple(), empty_tuple()))
      refute tuple_of_size_at_least(2) |> opt_difference(tuple_of_size(2)) |> empty?()
      assert tuple_of_size_at_least(2) |> opt_difference(tuple_of_size_at_least(1)) |> empty?()
      assert tuple_of_size_at_least(3) |> opt_difference(tuple_of_size_at_least(3)) |> empty?()
      refute tuple_of_size_at_least(2) |> opt_difference(tuple_of_size_at_least(3)) |> empty?()
      refute tuple([term(), term()]) |> opt_difference(tuple([atom(), term()])) |> empty?()
      refute tuple([term(), term()]) |> opt_difference(tuple([atom()])) |> empty?()
      assert tuple([term(), term()]) |> opt_difference(tuple([term(), term()])) |> empty?()

      assert tuple_of_size_at_least(2)
             |> opt_difference(tuple_of_size(2))
             |> opt_difference(tuple_of_size_at_least(3))
             |> empty?()

      assert tuple([term(), term()])
             |> opt_difference(tuple([atom()]))
             |> opt_difference(open_tuple([term()]))
             |> opt_difference(empty_tuple())
             |> empty?()

      refute opt_difference(tuple(), empty_tuple())
             |> opt_difference(open_tuple([term(), term()]))
             |> empty?()

      assert opt_difference(open_tuple([term()]), open_tuple([term(), term()]))
             |> opt_difference(tuple([term()]))
             |> empty?()

      assert tuple([opt_union(atom(), integer()), term()])
             |> opt_difference(open_tuple([atom(), term()]))
             |> opt_difference(open_tuple([integer(), term()]))
             |> empty?()

      assert tuple([opt_union(atom(), integer()), term()])
             |> opt_difference(open_tuple([atom(), term()]))
             |> equal?(tuple([integer(), term()]))

      assert tuple([term(), opt_union(atom(), integer()), term()])
             |> opt_difference(open_tuple([term(), integer()]))
             |> equal?(tuple([term(), atom(), term()]))

      assert opt_difference(tuple(), open_tuple([term(), term()]))
             |> equal?(opt_union(tuple([term()]), tuple([])))
    end

    test "tuple optimizations" do
      # We do direct assertions because we want to check how it works underneath
      assert opt_difference(tuple([]), tuple([atom()])) == tuple([])
      assert opt_difference(tuple([]), open_tuple([atom()])) == tuple([])
      assert opt_difference(open_tuple([atom()]), tuple([])) == open_tuple([atom()])
      assert opt_difference(tuple([atom([:ok])]), tuple([atom([:error])])) == tuple([atom([:ok])])
      assert opt_difference(tuple([atom([:ok])]), tuple([integer()])) == tuple([atom([:ok])])
      assert opt_difference(tuple([integer()]), tuple([atom()])) == tuple([integer()])
    end

    test "map" do
      assert opt_difference(open_map(), open_map()) == none()
      assert opt_difference(open_map(), term()) == none()
      assert opt_difference(open_map(), none()) == open_map()

      assert empty?(opt_difference(closed_map(a: integer()), open_map()))

      assert opt_difference(closed_map(a: integer(), b: if_set(atom())), closed_map(a: integer()))
             |> opt_difference(closed_map(a: integer(), b: atom()))
             |> empty?()

      refute empty?(opt_difference(open_map(), empty_map()))

      # Difference with single field closed map on rhs
      assert opt_difference(closed_map(a: integer()), closed_map(a: integer())) == none()

      assert opt_difference(open_map(a: atom()), closed_map(b: integer()))
             |> equal?(open_map(a: atom()))

      assert opt_difference(open_map(a: integer()), closed_map(b: boolean()))
             |> equal?(open_map(a: integer()))

      # Difference with single field open map on rhs (they are optimized)
      assert opt_difference(closed_map(a: integer()), open_map(a: integer())) == none()
      assert opt_difference(closed_map(a: integer()), open_map(a: if_set(integer()))) == none()

      assert opt_difference(closed_map(a: integer()), open_map(b: integer())) ==
               closed_map(a: integer())

      assert opt_difference(closed_map(a: integer()), open_map(b: if_set(integer()))) == none()
    end

    test "map double negation with redundant empty map" do
      type =
        closed_map(a: atom())
        |> opt_union(open_map(a: if_set(integer())))
        |> opt_union(empty_map())

      assert opt_negation(opt_negation(type)) |> equal?(type)
    end

    test "map (struct optimizations)" do
      # We do direct assertions because we want to check how it works underneath
      atom_foo = atom([:foo])
      atom_bar = atom([:bar])

      assert opt_difference(open_map(__struct__: atom_foo), open_map(__struct__: atom_bar)) ==
               open_map(__struct__: atom_foo)

      assert opt_difference(closed_map(__struct__: atom_foo), open_map(__struct__: atom_bar)) ==
               closed_map(__struct__: atom_foo)

      assert opt_difference(open_map(__struct__: atom_foo), closed_map(__struct__: atom_bar)) ==
               open_map(__struct__: atom_foo)

      assert opt_difference(closed_map(__struct__: atom_foo), closed_map(__struct__: atom_bar)) ==
               closed_map(__struct__: atom_foo)

      assert opt_difference(closed_map(__struct__: atom_foo), closed_map(__struct__: term())) ==
               none()

      assert opt_difference(closed_map(__struct__: atom()), closed_map(__struct__: atom_bar)) ==
               closed_map(__struct__: opt_difference(atom(), atom_bar))

      # Explicitly assert we keep it as cascading differences
      assert %{map: {_, {_, :closed, _}, :bdd_bot, :bdd_bot, _}} =
               opt_difference(
                 opt_difference(
                   open_map(value: term()),
                   closed_map(__struct__: atom_foo, value: term())
                 ),
                 closed_map(__struct__: atom_bar, name: term())
               )
    end

    test "map with domain keys" do
      # Non-overlapping domain keys
      t1 = closed_map([{domain_key(:integer), atom()}])
      t2 = closed_map([{domain_key(:atom), binary()}])

      assert equal?(opt_difference(t1, t2) |> opt_union(empty_map()), t1)
      assert empty?(opt_difference(t1, t1))

      # %{atom() => t1} and not %{atom() => t2} is not %{atom() => t1 and not t2}
      t3 = closed_map([{domain_key(:integer), atom()}])
      t4 = closed_map([{domain_key(:integer), atom([:ok])}])
      assert subtype?(opt_difference(t3, t4), t3)

      refute opt_difference(t3, t4)
             |> equal?(closed_map([{domain_key(:integer), opt_difference(atom(), atom([:ok]))}]))

      # Difference with a non-domain key map
      t5 = closed_map([{domain_key(:integer), opt_union(atom(), integer())}])
      t6 = closed_map(a: atom())
      assert equal?(opt_difference(t5, t6), t5)

      # Removing atom keys from a map with defined atom keys
      a_number = closed_map(a: number())
      a_number_and_pids = closed_map([{:a, number()}, {domain_key(:atom), pid()}])
      atom_to_float = closed_map([{domain_key(:atom), float()}])
      atom_to_term = closed_map([{domain_key(:atom), term()}])
      atom_to_pid = closed_map([{domain_key(:atom), pid()}])
      t_diff = opt_difference(a_number, atom_to_float)

      # Removing atom keys that map to float, make the :a key point to integer only.
      assert map_fetch_key(t_diff, :a) == {false, integer()}
      # %{a => number, atom => pid} and not %{atom => float} gives numbers on :a
      assert map_fetch_key(opt_difference(a_number_and_pids, atom_to_float), :a) ==
               {false, number()}

      assert map_fetch_key(t_diff, :foo) == :badkey

      assert subtype?(a_number, atom_to_term)
      refute subtype?(a_number, atom_to_float)

      # Removing all atom keys from map %{:a => type} means there is nothing left.
      assert empty?(opt_difference(a_number, atom_to_term))
      refute empty?(opt_intersection(atom_to_term, a_number))
      assert empty?(opt_intersection(atom_to_pid, a_number))

      # (%{:a => number} and not %{:a => float}) is %{:a => integer}
      assert equal?(opt_difference(a_number, atom_to_float), closed_map(a: integer()))
    end

    test "list" do
      # Basic list type differences
      assert opt_difference(list(term()), empty_list()) == non_empty_list(term())
      assert opt_difference(list(integer()), list(term())) |> empty?()

      assert opt_difference(list(integer()), list(float()))
             |> equal?(non_empty_list(integer()))

      # All list of integers and floats, minus all lists of integers, is NOT all lists of floats
      refute opt_difference(list(opt_union(integer(), float())), list(integer()))
             |> equal?(non_empty_list(float()))

      # Interactions with empty_list()
      assert opt_difference(empty_list(), list(term())) == none()
      assert opt_difference(list(integer()), empty_list()) == non_empty_list(integer())

      # Nested list structures
      assert opt_difference(list(list(integer())), list(list(float())))
             |> equal?(opt_difference(list(list(integer())), list(empty_list())))

      # Lists with union types
      refute opt_difference(list(opt_union(integer(), float())), list(integer())) == list(float())
      refute opt_difference(list(opt_union(atom(), binary())), list(atom())) == list(binary())

      # Tests for list with last element
      assert opt_difference(list(integer(), atom()), list(number(), term())) |> empty?()

      assert opt_difference(
               list(atom(), term()),
               opt_difference(list(atom(), term()), list(atom()))
             )
             |> equal?(list(atom()))

      assert opt_difference(list(integer(), float()), list(number(), integer()))
             |> equal?(non_empty_list(integer(), opt_difference(float(), integer())))

      # Empty list with last element
      assert opt_difference(empty_list(), list(integer(), atom())) == none()

      assert opt_difference(list(integer(), atom()), empty_list()) ==
               non_empty_list(integer(), atom())

      # List with any type and specific last element
      assert opt_difference(list(term(), term()), list(term(), integer()))
             |> equal?(
               non_empty_list(
                 term(),
                 opt_negation(opt_union(integer(), non_empty_list(term(), term())))
               )
             )

      # Nested lists with last element
      # "lists of (lists of integers), ending with atom"
      # minus
      # "lists of (lists of numbers), ending with boolean"
      # gives:
      # "non empty lists of (lists of integers), ending with (atom and not boolean)"

      assert opt_difference(list(list(integer()), atom()), list(list(number()), boolean()))
             |> equal?(non_empty_list(list(integer()), opt_difference(atom(), boolean())))

      # Union types in last element
      assert opt_difference(list(integer(), opt_union(atom(), binary())), list(number(), atom()))
             |> equal?(
               opt_union(
                 non_empty_list(integer(), binary()),
                 non_empty_list(opt_difference(integer(), number()), opt_union(atom(), binary()))
               )
             )

      # Dynamic with last element
      assert equal?(
               opt_difference(dynamic(list(term(), atom())), list(integer(), term())),
               dynamic(opt_difference(list(term(), atom()), list(integer(), term())))
             )

      # Difference with proper list
      assert opt_difference(list(integer(), atom()), list(integer()))
             |> equal?(non_empty_list(integer(), atom()))
    end

    test "fun" do
      for arity <- [0, 1, 2, 3] do
        assert empty?(opt_difference(none_fun(arity), none_fun(arity)))
      end

      assert empty?(opt_difference(fun(), fun()))
      assert empty?(opt_difference(none_fun(3), fun()))
      refute empty?(opt_difference(fun(), none_fun(1)))
      refute empty?(opt_difference(none_fun(2), none_fun(3)))
      assert empty?(opt_intersection(none_fun(2), none_fun(3)))

      f1f2 = opt_union(none_fun(1), none_fun(2))
      assert f1f2 |> opt_difference(none_fun(1)) |> opt_difference(none_fun(2)) |> empty?()
      assert none_fun(1) |> opt_difference(opt_difference(f1f2, none_fun(2))) |> empty?()
      assert f1f2 |> opt_difference(none_fun(1)) |> equal?(none_fun(2))

      assert fun([integer()], term()) |> opt_difference(fun([none()], term())) |> empty?()
    end
  end

  describe "creation" do
    test "map hoists dynamic" do
      assert dynamic(open_map(a: integer())) == open_map(a: dynamic(integer()))

      assert opt_union(
               open_map(a: binary()),
               dynamic(open_map(a: opt_union(integer(), binary())))
             ) ==
               open_map(a: dynamic(integer()) |> opt_union(binary()))

      # For domains too
      t1 = dynamic(open_map([{domain_key(:integer), integer()}]))
      t2 = open_map([{domain_key(:integer), dynamic(integer())}])
      assert t1 == t2

      # if_set on dynamic fields also must work
      t1 = dynamic(open_map(a: if_set(integer())))
      t2 = open_map(a: if_set(dynamic(integer())))
      assert opt_union(open_map(a: not_set()), t1) == t2
    end

    test "structural types preserve static part of gradual elements" do
      static = atom([:ok])
      gradual = opt_union(static, dynamic(integer()))
      upper_bound = upper_bound(gradual)
      x = atom([:x])
      head = atom([:head])

      for {descr, static_descr, dynamic_descr} <- [
            {tuple([gradual, x]), tuple([static, x]), tuple([upper_bound, x])},
            {open_tuple([gradual]), open_tuple([static]), open_tuple([upper_bound])},
            {closed_map(a: gradual), closed_map(a: static), closed_map(a: upper_bound)},
            {open_map(a: gradual), open_map(a: static), open_map(a: upper_bound)},
            {closed_map([{domain_key(:integer), gradual}]),
             closed_map([{domain_key(:integer), static}]),
             closed_map([{domain_key(:integer), upper_bound}])},
            {non_empty_list(gradual), non_empty_list(static), non_empty_list(upper_bound)},
            {non_empty_list(head, gradual), non_empty_list(head, static),
             non_empty_list(head, upper_bound)}
          ] do
        assert descr == Map.put(static_descr, :dynamic, dynamic_descr)
        assert subtype?(static_descr, descr)
      end
    end
  end

  describe "subtype" do
    test "bitmap" do
      assert subtype?(integer(), opt_union(integer(), float()))
      assert subtype?(integer(), integer())
      assert subtype?(integer(), term())
      assert subtype?(none(), integer())
      assert subtype?(integer(), opt_negation(float()))
    end

    test "atom" do
      assert subtype?(atom([:a]), atom())
      assert subtype?(atom([:a]), atom([:a]))
      assert subtype?(atom([:a]), term())
      assert subtype?(none(), atom([:a]))
      assert subtype?(atom([:a]), atom([:a, :b]))
      assert subtype?(atom([:a]), opt_negation(atom([:b])))
    end

    test "dynamic" do
      assert subtype?(dynamic(), term())
      assert subtype?(dynamic(), dynamic())
      refute subtype?(term(), dynamic())
      assert subtype?(opt_intersection(dynamic(), integer()), integer())
      assert subtype?(integer(), opt_union(dynamic(), integer()))
    end

    test "tuple" do
      assert subtype?(empty_tuple(), tuple())
      assert subtype?(tuple([integer(), atom()]), tuple())
      refute subtype?(empty_tuple(), open_tuple([term()]))
      assert subtype?(tuple([integer(), atom()]), tuple([term(), term()]))
      refute subtype?(tuple([integer(), atom()]), tuple([integer(), integer()]))
      refute subtype?(tuple([integer(), atom()]), tuple([atom(), atom()]))

      assert subtype?(tuple([integer(), atom()]), open_tuple([integer(), atom()]))
      refute subtype?(tuple([term()]), open_tuple([term(), term()]))
      refute subtype?(tuple([integer(), atom()]), open_tuple([integer(), integer()]))
      refute subtype?(open_tuple([integer(), atom()]), tuple([integer(), integer()]))
    end

    test "map" do
      assert subtype?(open_map(), term())
      assert subtype?(closed_map(a: integer()), open_map())
      assert subtype?(closed_map(a: integer()), closed_map(a: integer()))
      assert subtype?(closed_map(a: integer()), open_map(a: integer()))
      assert subtype?(closed_map(a: integer(), b: atom()), open_map(a: integer()))
      assert subtype?(closed_map(a: integer()), closed_map(a: opt_union(integer(), atom())))

      # optional
      refute subtype?(closed_map(a: if_set(integer())), closed_map(a: integer()))
      assert subtype?(closed_map(a: integer()), closed_map(a: if_set(integer())))
      refute subtype?(closed_map(a: if_set(term())), closed_map(a: term()))
      assert subtype?(closed_map(a: term()), closed_map(a: if_set(term())))

      # With domains
      t1 = closed_map([{domain_key(:integer), number()}])
      t2 = closed_map([{domain_key(:integer), integer()}])

      assert subtype?(t2, t1)

      t1_minus_t2 = opt_difference(t1, t2)
      refute empty?(t1_minus_t2)

      assert subtype?(map_with_default(number()), open_map())
      t = opt_difference(open_map(), map_with_default(number()))
      refute empty?(t)
      refute subtype?(open_map(), map_with_default(number()))
      assert subtype?(map_with_default(integer()), map_with_default(number()))
      refute subtype?(map_with_default(float()), map_with_default(atom()))

      assert equal?(
               opt_intersection(map_with_default(number()), map_with_default(float())),
               map_with_default(float())
             )
    end

    test "optional" do
      refute subtype?(if_set(none()), term())
      refute subtype?(if_set(term()), term())
      assert subtype?(if_set(term()), if_set(term()))
      refute subtype?(if_set(term()), if_set(dynamic(term())))
    end

    test "list" do
      refute subtype?(non_empty_list(integer()), opt_difference(list(number()), list(integer())))
      assert subtype?(list(term(), boolean()), list(term(), atom()))
      assert subtype?(list(integer()), list(term()))
      assert subtype?(list(term()), list(term(), term()))
    end

    test "fun" do
      assert equal?(fun([], term()), fun([], term()))
      refute equal?(fun([], integer()), fun([], atom()))
      refute subtype?(fun([none()], term()), fun([integer()], integer()))

      # Difference with argument/return type variations
      int_to_atom = fun([integer()], atom())
      num_to_atom = fun([number()], atom())
      int_to_bool = fun([integer()], boolean())

      # number->atom is a subtype of int->atom
      assert subtype?(num_to_atom, int_to_atom)
      refute subtype?(int_to_atom, num_to_atom)
      assert subtype?(int_to_bool, int_to_atom)
      refute subtype?(int_to_bool, num_to_atom)

      # Multi-arity
      f1 = fun([integer(), atom()], boolean())
      f2 = fun([number(), atom()], boolean())

      # (int,atom)->boolean is a subtype of (number,atom)->boolean
      # since number is a supertype of int
      assert subtype?(f2, f1)
      # f1 is not a subtype of f2
      refute subtype?(f1, f2)

      # Unary functions / Output covariance
      assert subtype?(fun([], float()), fun([], term()))
      refute subtype?(fun([], term()), fun([], float()))

      # Contravariance of domain
      refute subtype?(fun([integer()], boolean()), fun([number()], boolean()))
      assert subtype?(fun([number()], boolean()), fun([integer()], boolean()))

      # Nested function types
      higher_order = fun([fun([integer()], atom())], boolean())
      specific = fun([fun([number()], atom())], boolean())

      assert subtype?(higher_order, specific)
      refute subtype?(specific, higher_order)

      ## Multi-arity
      f = fun([none(), integer()], atom())
      assert subtype?(f, f)
      assert subtype?(f, fun([none(), integer()], term()))
      assert subtype?(fun([none(), number()], atom()), f)
      assert subtype?(fun([tuple(), number()], atom()), f)

      # (none, float -> atom) is not a subtype of (none, integer -> atom)
      # because float has an empty intersection with integer.
      # it's only possible to find this out by doing the
      # intersection one by one.
      refute subtype?(fun([none(), float()], atom()), f)
      refute subtype?(fun([pid(), float()], atom()), f)
      # A function with the wrong arity is refused
      refute subtype?(fun([none()], atom()), f)
    end
  end

  describe "compatible" do
    test "intersection" do
      assert compatible?(integer(), opt_intersection(dynamic(), integer()))
      refute compatible?(opt_intersection(dynamic(), integer()), atom())
      refute compatible?(atom(), opt_intersection(dynamic(), integer()))
      refute compatible?(atom(), opt_intersection(dynamic(), atom([:foo, :bar])))
      assert compatible?(opt_intersection(dynamic(), atom()), atom([:foo, :bar]))
      assert compatible?(atom([:foo, :bar]), opt_intersection(dynamic(), atom()))
    end

    test "static" do
      refute compatible?(atom(), atom([:foo, :bar]))
      refute compatible?(opt_union(integer(), atom()), integer())
      refute compatible?(none(), integer())
      refute compatible?(opt_union(atom(), dynamic()), integer())
    end

    test "dynamic" do
      assert compatible?(dynamic(), term())
      assert compatible?(term(), dynamic())
      assert compatible?(dynamic(), integer())
      assert compatible?(integer(), dynamic())
    end

    test "tuple" do
      assert compatible?(dynamic(tuple()), tuple([integer(), atom()]))
    end

    test "map" do
      assert compatible?(closed_map(a: integer()), open_map())
      assert compatible?(opt_intersection(dynamic(), open_map()), closed_map(a: integer()))
    end

    test "list" do
      assert compatible?(dynamic(), list(term()))
      assert compatible?(dynamic(list(term())), list(integer()))
      assert compatible?(dynamic(list(term(), term())), list(integer(), integer()))
    end
  end

  describe "empty?" do
    test "tuple" do
      assert tuple([none()]) |> empty?()
      assert open_tuple([integer(), none()]) |> empty?()

      assert opt_intersection(tuple([integer(), atom()]), open_tuple([atom()])) |> empty?()
      refute open_tuple([integer(), integer()]) |> opt_difference(empty_tuple()) |> empty?()

      refute open_tuple([integer(), integer()])
             |> opt_difference(open_tuple([atom()]))
             |> empty?()

      refute open_tuple([term()]) |> opt_difference(tuple([term()])) |> empty?()

      assert opt_difference(tuple(), empty_tuple())
             |> opt_difference(open_tuple([term()]))
             |> empty?()

      assert opt_difference(tuple(), open_tuple([term()]))
             |> opt_difference(empty_tuple())
             |> empty?()

      refute open_tuple([term()])
             |> opt_difference(tuple([term()]))
             |> opt_difference(tuple([term()]))
             |> empty?()

      assert tuple([integer(), opt_union(integer(), atom())])
             |> opt_difference(tuple([integer(), integer()]))
             |> opt_difference(tuple([integer(), atom()]))
             |> empty?()
    end

    test "map" do
      assert open_map(a: none()) |> empty?()
      assert closed_map(a: integer(), b: none()) |> empty?()
      assert opt_intersection(closed_map(b: atom()), open_map(a: integer())) |> empty?()
    end

    test "fun" do
      refute empty?(fun())
      refute empty?(none_fun(1))
      refute empty?(fun([integer()], atom()))

      assert empty?(opt_intersection(none_fun(1), none_fun(2)))
      refute empty?(opt_intersection(fun(), none_fun(1)))
      assert empty?(opt_difference(none_fun(1), opt_union(none_fun(1), none_fun(2))))
    end
  end

  describe "function creation" do
    test "fun_from_non_overlapping_clauses" do
      assert fun_from_non_overlapping_clauses([{[integer()], atom()}, {[float()], binary()}]) ==
               opt_intersection(fun([integer()], atom()), fun([float()], binary()))
    end

    test "fun_from_inferred_clauses" do
      # No overlap
      assert fun_from_inferred_clauses([{[integer()], atom()}, {[float()], binary()}])
             |> equal?(
               opt_intersection(
                 fun_from_non_overlapping_clauses([{[integer()], atom()}, {[float()], binary()}]),
                 fun([number()], dynamic())
               )
             )

      # Subsets
      assert fun_from_inferred_clauses([{[integer()], atom()}, {[number()], binary()}])
             |> equal?(
               fun_from_non_overlapping_clauses([
                 {[integer()], dynamic(opt_union(atom(), binary()))},
                 {[number()], dynamic(opt_union(atom(), binary()))}
               ])
             )

      assert fun_from_inferred_clauses([{[number()], binary()}, {[integer()], atom()}])
             |> equal?(
               fun_from_non_overlapping_clauses([
                 {[integer()], dynamic(opt_union(atom(), binary()))},
                 {[number()], dynamic(opt_union(atom(), binary()))}
               ])
             )

      # Partial
      assert fun_from_inferred_clauses([
               {[opt_union(integer(), pid())], atom()},
               {[opt_union(float(), pid())], binary()}
             ])
             |> equal?(
               fun_from_non_overlapping_clauses([
                 {[opt_union(integer(), pid())], dynamic(opt_union(atom(), binary()))},
                 {[opt_union(float(), pid())], dynamic(opt_union(atom(), binary()))}
               ])
             )

      # Difference
      assert fun_from_inferred_clauses([
               {[integer(), opt_union(pid(), atom())], atom()},
               {[number(), pid()], binary()}
             ])
             |> equal?(
               fun_from_non_overlapping_clauses([
                 {[integer(), opt_union(pid(), atom())], dynamic(opt_union(atom(), binary()))},
                 {[number(), pid()], dynamic(opt_union(atom(), binary()))}
               ])
             )
    end
  end

  describe "function application" do
    defp none_fun(arity), do: %{fun: {:union, %{arity => :bdd_top}}}

    test "non funs" do
      assert fun_apply(term(), [integer()]) == :badfun
      assert fun_apply(integer(), [integer()]) == :badfun
      assert fun_apply(opt_union(integer(), none_fun(1)), [integer()]) == :badfun
      assert fun_apply(opt_union(integer(), fun([integer()], atom())), [integer()]) == :badfun
      assert fun_apply(opt_union(integer(), dynamic()), [integer()]) == :badfun
    end

    test "static" do
      # Full static
      assert fun_apply(fun(), [integer()]) == {:badarg, [none()], false}

      assert fun_apply(opt_difference(fun(), none_fun(2)), [integer()]) ==
               {:badarg, [none()], false}

      # Basic function application scenarios
      assert fun_apply(fun([integer()], atom()), [integer()]) == {:ok, atom()}
      assert fun_apply(fun([integer()], atom()), [float()]) == {:badarg, [integer()], false}
      assert fun_apply(fun([integer()], atom()), [term()]) == {:badarg, [integer()], false}

      # Union argument type: domain is int | float
      assert fun_apply(fun([opt_union(integer(), float())], atom()), [integer()]) == {:ok, atom()}
      assert fun_apply(fun([opt_union(integer(), float())], atom()), [float()]) == {:ok, atom()}

      assert fun_apply(fun([opt_union(integer(), float())], atom()), [atom()]) ==
               {:badarg, [opt_union(integer(), float())], false}

      # 2-arity function
      assert fun_apply(fun([integer(), atom()], binary()), [integer(), atom()]) == {:ok, binary()}

      assert fun_apply(fun([integer(), atom()], binary()), [boolean(), atom()]) ==
               {:badarg, [integer(), atom()], false}

      # Return types
      assert fun_apply(fun([integer()], none()), [integer()]) == {:ok, none()}
      assert fun_apply(fun([integer()], term()), [integer()]) == {:ok, term()}

      # Dynamic args
      assert fun_apply(fun([term()], term()), [dynamic()]) == {:ok, term()}

      assert fun_apply(fun([integer()], atom()), [dynamic(integer())])
             |> elem(1)
             |> equal?(atom())

      assert fun_apply(fun([integer()], atom()), [dynamic(float())]) ==
               {:badarg, [integer()], false}

      assert fun_apply(fun([integer()], atom()), [dynamic(term())]) == {:ok, dynamic()}

      # Arity mismatches
      assert fun_apply(fun([integer()], integer()), [term(), term()]) == {:badarity, [1]}
      assert fun_apply(fun([integer(), atom()], boolean()), [integer()]) == {:badarity, [2]}

      # Union of two different arities: always badarity regardless of which arity is called
      fun_mixed = opt_union(fun([integer()], integer()), fun([integer(), atom()], boolean()))
      assert fun_apply(fun_mixed, [integer()]) == {:badarity, [1, 2]}
      assert fun_apply(fun_mixed, [integer(), atom()]) == {:badarity, [2, 1]}

      # Function intersection tests (no overlap)
      fun0 = opt_intersection(fun([integer()], atom()), fun([float()], binary()))
      assert fun_apply(fun0, [integer()]) == {:ok, atom()}
      assert fun_apply(fun0, [float()]) == {:ok, binary()}
      assert fun_apply(fun0, [number()]) == {:ok, opt_union(atom(), binary())}

      assert fun_apply(fun0, [dynamic(integer())]) |> elem(1) |> equal?(atom())
      assert fun_apply(fun0, [dynamic(float())]) |> elem(1) |> equal?(binary())

      assert fun_apply(fun0, [dynamic(number())])
             |> elem(1)
             |> equal?(opt_union(atom(), binary()))

      assert fun_apply(fun0, [dynamic()]) == {:ok, dynamic()}

      # Function intersection tests (overlap)
      fun1 = opt_intersection(fun([integer()], atom()), fun([number()], term()))
      assert fun_apply(fun1, [integer()]) == {:ok, atom()}
      assert fun_apply(fun1, [float()]) == {:ok, term()}

      # Function intersection with unions
      fun2 =
        opt_intersection(
          fun([opt_union(integer(), atom())], term()),
          fun([opt_union(integer(), pid())], atom())
        )

      assert fun_apply(fun2, [integer()]) == {:ok, atom()}
      assert fun_apply(fun2, [atom()]) == {:ok, term()}
      assert fun_apply(fun2, [pid()]) == {:ok, atom()}

      # Function intersection with same domain, different codomains
      assert fun([integer()], term())
             |> opt_intersection(fun([integer()], atom()))
             |> fun_apply([integer()]) == {:ok, atom()}

      # Function intersection with singleton atoms
      fun3 =
        opt_intersection(fun([atom([:ok])], atom([:success])), fun([atom([:ok])], atom([:done])))

      assert fun_apply(fun3, [atom([:ok])]) == {:ok, none()}
    end

    test "static with dynamic signature" do
      assert fun_apply(fun([dynamic()], term()), [dynamic()]) == {:ok, term()}
      assert fun_apply(fun([integer()], dynamic()), [integer()]) == {:ok, dynamic()}

      assert fun_apply(fun([dynamic()], integer()), [dynamic()]) ==
               {:ok, opt_union(integer(), dynamic())}

      assert fun_apply(fun([dynamic(), atom()], float()), [dynamic(), atom()]) ==
               {:ok, opt_union(float(), dynamic())}

      fun = fun([dynamic(integer())], atom())
      assert fun_apply(fun, [dynamic(integer())]) == {:ok, opt_union(atom(), dynamic())}
      assert fun_apply(fun, [dynamic(number())]) == {:ok, dynamic()}
      assert fun_apply(fun, [integer()]) == {:ok, dynamic()}
      assert fun_apply(fun, [float()]) == {:badarg, [dynamic(integer())], false}
    end

    defp dynamic_fun(args, return), do: dynamic(fun(args, return))

    test "dynamic" do
      # Full dynamic
      assert fun_apply(dynamic(), [integer()]) == {:ok, dynamic()}
      assert fun_apply(dynamic(none_fun(1)), [integer()]) == {:ok, dynamic()}
      assert fun_apply(opt_difference(dynamic(), none_fun(2)), [integer()]) == {:ok, dynamic()}

      # Basic function application scenarios
      assert fun_apply(dynamic_fun([integer()], atom()), [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(dynamic_fun([integer()], atom()), [float()]) == {:ok, dynamic()}
      assert fun_apply(dynamic_fun([integer()], atom()), [term()]) == {:ok, dynamic()}
      assert fun_apply(dynamic_fun([integer()], none()), [integer()]) == {:ok, dynamic(none())}

      assert fun_apply(dynamic(fun([integer()], integer())), [none()]) ==
               {:badarg, [none()], true}

      assert fun_apply(dynamic_fun([integer()], term()), [integer()]) == {:ok, dynamic()}

      # Dynamic return and dynamic args
      assert fun_apply(dynamic_fun([term()], term()), [dynamic()]) == {:ok, dynamic()}

      fun = dynamic_fun([integer()], binary())
      assert fun_apply(fun, [integer()]) == {:ok, dynamic(binary())}
      assert fun_apply(fun, [dynamic(integer())]) == {:ok, dynamic(binary())}
      assert fun_apply(fun, [dynamic(atom())]) == {:ok, dynamic()}

      # Arity mismatches
      assert fun_apply(dynamic_fun([integer()], integer()), [term(), term()]) == {:badarity, [1]}

      assert fun_apply(dynamic_fun([integer(), atom()], boolean()), [integer()]) ==
               {:badarity, [2]}

      # Union of two dynamic functions with different arities: the call may succeed,
      # so we pick the matching-arity arrows and wrap in dynamic().
      fun_dyn_mixed =
        opt_union(
          dynamic_fun([integer()], integer()),
          dynamic_fun([integer(), atom()], boolean())
        )

      # picks arity-1 arrows → dynamic(integer())
      assert fun_apply(fun_dyn_mixed, [integer()]) == {:ok, dynamic(integer())}
      # picks arity-2 arrows → dynamic(boolean())
      assert fun_apply(fun_dyn_mixed, [integer(), atom()]) == {:ok, dynamic(boolean())}
      # no matching arity → badarity (no dynamic escape here)
      assert fun_apply(fun_dyn_mixed, [integer(), atom(), float()]) == {:badarity, [1, 2]}
      # arg outside arity-1 domain but dynamic-compatible → dynamic()
      assert fun_apply(fun_dyn_mixed, [atom()]) == {:ok, dynamic()}

      # Function intersection tests
      fun0 = opt_intersection(dynamic_fun([integer()], atom()), dynamic_fun([float()], binary()))
      assert fun_apply(fun0, [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(fun0, [float()]) == {:ok, dynamic(binary())}
      assert fun_apply(fun0, [dynamic(integer())]) == {:ok, dynamic(atom())}
      assert fun_apply(fun0, [dynamic(float())]) == {:ok, dynamic(binary())}
      assert fun_apply(fun0, [dynamic(number())]) == {:ok, dynamic(opt_union(binary(), atom()))}

      # Function intersection with subset domain
      fun1 = opt_intersection(dynamic_fun([integer()], atom()), dynamic_fun([number()], term()))
      assert fun_apply(fun1, [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(fun1, [float()]) == {:ok, dynamic()}
      assert fun_apply(fun1, [dynamic(integer())]) == {:ok, dynamic(atom())}
      assert fun_apply(fun1, [dynamic(float())]) == {:ok, dynamic()}

      # Function intersection with same domain, different codomains
      assert dynamic_fun([integer()], term())
             |> opt_intersection(dynamic_fun([integer()], atom()))
             |> fun_apply([integer()]) == {:ok, dynamic(atom())}

      # Function intersection with overlapping domains
      fun2 =
        opt_intersection(
          dynamic_fun([opt_union(integer(), atom())], term()),
          dynamic_fun([opt_union(integer(), pid())], atom())
        )

      assert fun_apply(fun2, [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(fun2, [atom()]) == {:ok, dynamic()}
      assert fun_apply(fun2, [pid()]) |> elem(1) |> equal?(dynamic(atom()))

      assert fun_apply(fun2, [dynamic(integer())]) == {:ok, dynamic(atom())}
      assert fun_apply(fun2, [dynamic(atom())]) == {:ok, dynamic()}
      assert fun_apply(fun2, [dynamic(pid())]) |> elem(1) |> equal?(dynamic(atom()))

      # Function intersection with singleton atoms
      fun3 =
        opt_intersection(
          dynamic_fun([atom([:ok])], atom([:success])),
          dynamic_fun([atom([:ok])], atom([:done]))
        )

      assert fun_apply(fun3, [atom([:ok])]) == {:ok, dynamic(none())}

      # Testing the special case of uplifiting both the function and argument
      # when the function is purely dynamic
      fun4 =
        opt_intersection(
          dynamic_fun([integer()], integer()),
          dynamic_fun([boolean()], boolean())
        )

      # dynamic(int->int and bool->bool) applied to dynamic(int)
      assert fun_apply(fun4, [dynamic(integer())]) == {:ok, dynamic(integer())}

      # float escapes the domain so the result is dynamic()
      arg = dynamic(opt_union(integer(), float()))
      assert fun_apply(fun4, [arg]) == {:ok, dynamic()}

      assert fun_apply(dynamic(), [integer()]) == {:ok, dynamic()}
    end

    test "static and dynamic" do
      # Bad arity
      fun_arities =
        opt_union(
          fun([atom()], integer()),
          dynamic_fun([integer(), float()], binary())
        )

      assert fun_arities
             |> fun_apply([atom()])
             |> elem(1)
             |> equal?(integer())

      assert fun_arities |> fun_apply([integer(), float()]) == {:badarity, [1]}

      # Bad argument
      fun_args =
        opt_union(
          fun([atom()], integer()),
          dynamic_fun([integer()], binary())
        )

      assert fun_args |> fun_apply([atom()]) == {:ok, dynamic()}
      assert fun_args |> fun_apply([integer()]) == {:badarg, [dynamic(atom())], false}

      # ((bool->bool) or dyn(int->int))
      # booleans work, but not integers
      fun_mixed_gdom = opt_union(fun([boolean()], boolean()), dynamic_fun([integer()], integer()))
      assert fun_apply(fun_mixed_gdom, [boolean()]) == {:ok, dynamic()}

      assert fun_apply(fun_mixed_gdom, [dynamic(boolean())]) ==
               {:ok, opt_union(dynamic(), boolean())}

      assert fun_apply(fun_mixed_gdom, [integer()]) == {:badarg, [dynamic(boolean())], false}

      assert fun_apply(fun_mixed_gdom, [dynamic(integer())]) ==
               {:badarg, [dynamic(boolean())], false}

      # Badfun
      assert opt_union(
               fun([atom()], integer()),
               dynamic_fun([integer()], binary()) |> opt_intersection(none_fun(2))
             )
             |> fun_apply([atom()])
             |> elem(1)
             |> equal?(integer())

      assert opt_union(
               fun([atom()], integer()) |> opt_intersection(none_fun(2)),
               dynamic_fun([integer()], binary())
             )
             |> fun_apply([integer()]) == {:ok, dynamic(binary())}

      # Applying (dynamic or int) -> bool to (dynamic and float).
      # The domain is
      #   gdom((dynamic or int) -> bool) = dom(int -> bool) or dynamic and dom(term -> bool)
      #                                  = int or dynamic and term = int or dynamic

      # The domain check dynamic and float <= int or dynamic succeeds.
      # The static application (term -> bool) o float = bool is well-defined.
      # The dynamic application (int -> bool) o float is not well-defined (float not <: int),
      # but since it is dynamic it returns term wrapped in dynamic, which is dynamic.
      # Result: bool or dynamic.
      fun_type = fun([opt_union(dynamic(), integer())], boolean())
      arg = dynamic(float())

      # Application yields bool or dynamic
      assert {:ok, result} = fun_apply(fun_type, [arg])
      assert equal?(opt_union(boolean(), dynamic()), result)
    end
  end

  describe "singleton?" do
    test "non-singleton?" do
      refute singleton?(term())
      refute singleton?(none())
      refute singleton?(dynamic())
      refute singleton?(integer())
      refute singleton?(float())
      refute singleton?(pid())
      refute singleton?(reference())
      refute singleton?(fun(1))
      refute singleton?(non_empty_list(atom([:foo])))
    end

    @disguised_empty_map closed_map(key: atom([:value]))
                         |> opt_difference(open_map(key: atom(), optional: if_set(atom())))

    test "atoms" do
      assert singleton?(atom([:foo]))
      refute singleton?(atom([:foo, :bar]))
      assert singleton?(atom([:foo]) |> opt_union(@disguised_empty_map))
      refute singleton?(atom() |> opt_difference(atom([:foo])))
    end

    test "empty list" do
      assert singleton?(empty_list())
      refute singleton?(non_empty_list(term()))
      refute singleton?(opt_union(empty_list(), atom([:foo])))
      assert singleton?(opt_union(empty_list(), @disguised_empty_map))
    end

    test "maps" do
      assert singleton?(empty_map())
      assert singleton?(closed_map(key: atom([:value])))
      assert singleton?(closed_map(key: atom([:value])) |> opt_union(@disguised_empty_map))
      refute singleton?(closed_map(key: binary()))
      refute singleton?(closed_map(key: if_set(atom([:value]))))
      refute singleton?(closed_map(__struct__: :term))
      refute singleton?(open_map())
      refute singleton?(open_map(key: atom([:value])))

      refute singleton?(
               opt_union(closed_map(key: atom([:value])), closed_map(other: atom([:value])))
             )
    end

    test "tuples" do
      assert singleton?(tuple([]))
      assert singleton?(tuple([atom([:foo])]))
      refute singleton?(tuple([binary()]))
      refute singleton?(open_tuple([]))
      refute singleton?(opt_union(tuple([atom([:value])]), tuple([atom([:other_value])])))
      refute singleton?(opt_union(tuple([atom([:value])]), closed_map(other: atom([:value]))))

      # Both BDD lines produce the same singleton tuple, so the tuple DNF must not duplicate it.
      a = tuple([opt_union(integer(), atom([:ok])), atom([:x])])
      b = tuple([integer(), atom([:x, :y])])
      c = tuple([integer(), opt_union(atom([:x]), binary())])

      t = opt_union(opt_difference(a, b), opt_difference(a, c))
      # Semantically t ~= {:ok, :x}, confirmed by equal?
      assert equal?(t, tuple([atom([:ok]), atom([:x])]))
      assert singleton?(t)
    end
  end

  describe "projections" do
    test "booleaness" do
      for type <- [none(), open_map(), opt_negation(boolean()), opt_difference(atom(), boolean())] do
        assert booleaness(type) == :none
        assert booleaness(dynamic(type)) == :none
      end

      for type <- [term(), dynamic(), atom(), boolean()] do
        assert booleaness(type) == :maybe_both
        assert booleaness(dynamic(type)) == :maybe_both
      end

      assert booleaness(atom([false])) == {false, :always}
      assert booleaness(dynamic(atom([false]))) == {false, :always}
      assert booleaness(dynamic(atom([false, :other]))) == {false, :maybe}
      assert booleaness(opt_negation(atom([false]))) == {true, :maybe}

      assert booleaness(atom([true])) == {true, :always}
      assert booleaness(dynamic(atom([true]))) == {true, :always}
      assert booleaness(dynamic(atom([true, :other]))) == {true, :maybe}
      assert booleaness(opt_negation(atom([true]))) == {false, :maybe}
    end

    test "truthiness" do
      for type <- [term(), none(), atom(), boolean(), opt_union(atom([false]), integer())] do
        assert truthiness(type) == :undefined
        assert truthiness(dynamic(type)) == :undefined
      end

      for type <- [atom([false]), atom([nil]), atom([nil, false]), atom([false, nil])] do
        assert truthiness(type) == :always_false
        assert truthiness(dynamic(type)) == :always_false
      end

      assert equal?(
               opt_intersection(opt_union(atom(), dynamic()), opt_union(atom(), dynamic())),
               opt_union(atom(), dynamic())
             )

      for type <-
            [
              opt_negation(atom()),
              atom([true]),
              opt_negation(atom([false, nil])),
              atom([:ok]),
              integer()
            ] do
        assert truthiness(type) == :always_true
        assert truthiness(dynamic(type)) == :always_true
      end

      assert truthiness(opt_union(atom([true]), integer())) == :always_true

      empty_descr =
        opt_difference(tuple([number(), integer()]), open_tuple([float(), term()]))
        |> opt_difference(tuple([integer(), integer()]))

      assert empty?(empty_descr)

      assert truthiness(empty_descr) == :undefined
      assert truthiness(dynamic(empty_descr)) == :undefined
      assert truthiness(opt_union(atom([nil]), empty_descr)) == :always_false

      assert truthiness(opt_union(atom([false]), empty_descr)) == :always_false
    end

    test "atom_fetch" do
      assert atom_fetch(term()) == :error
      assert atom_fetch(opt_union(term(), dynamic(atom([:foo, :bar])))) == :error

      assert atom_fetch(atom()) == {:infinite, []}
      assert atom_fetch(dynamic()) == {:infinite, []}

      assert atom_fetch(atom([:foo, :bar])) ==
               {:finite, [:foo, :bar] |> :sets.from_list(version: 2) |> :sets.to_list()}

      assert atom_fetch(opt_union(atom([:foo, :bar]), dynamic(atom()))) == {:infinite, []}
      assert atom_fetch(opt_union(atom([:foo, :bar]), dynamic(term()))) == {:infinite, []}
    end

    test "list_hd" do
      assert list_hd(none()) == :badnonemptylist
      assert list_hd(term()) == :badnonemptylist
      assert list_hd(list(term())) == :badnonemptylist
      assert list_hd(empty_list()) == :badnonemptylist
      assert list_hd(non_empty_list(term())) == {:ok, term()}
      assert list_hd(non_empty_list(integer())) == {:ok, integer()}
      assert list_hd(opt_difference(list(number()), list(integer()))) == {:ok, number()}
      assert list_hd(non_empty_list(atom(), float())) == {:ok, atom()}

      assert list_hd(dynamic()) == {:ok, dynamic()}
      assert list_hd(dynamic(list(integer()))) == {:ok, dynamic(integer())}
      assert list_hd(opt_union(dynamic(), atom())) == :badnonemptylist
      assert list_hd(opt_union(dynamic(), list(term()))) == :badnonemptylist

      assert list_hd(opt_difference(list(number()), list(number()))) == :badnonemptylist
      assert list_hd(dynamic(opt_difference(list(number()), list(number())))) == :badnonemptylist

      assert list_hd(opt_union(dynamic(list(float())), non_empty_list(atom()))) ==
               {:ok, opt_union(dynamic(float()), atom())}

      # If term() is in the tail, it means list(term()) is in the tail
      # and therefore any term can be returned from hd.
      assert list_hd(non_empty_list(atom(), term())) == {:ok, term()}
      assert list_hd(non_empty_list(atom(), opt_negation(list(term(), term())))) == {:ok, atom()}
    end

    test "list_of" do
      assert list_of(term()) == :badproperlist
      assert list_of(none()) == :badproperlist
      assert list_of(empty_list()) == {true, none()}
      assert list_of(opt_union(empty_list(), integer())) == :badproperlist
      assert list_of(non_empty_list(integer())) == {false, integer()}
      assert list_of(non_empty_list(integer(), atom())) == :badproperlist
      assert list_of(non_empty_list(integer(), term())) == :badproperlist
      assert list_of(non_empty_list(integer(), list(term()))) == {false, term()}
      assert list_of(list(integer()) |> opt_union(list(integer(), integer()))) == :badproperlist
      assert list_of(list(integer()) |> opt_union(integer())) == :badproperlist
      assert list_of(dynamic(list(integer()))) == {true, dynamic(integer())}
      assert list_of(dynamic(list(integer(), atom()))) == {true, nil}
      assert list_of(dynamic(non_empty_list(integer(), atom()))) == :badproperlist
      assert list_of(dynamic(opt_union(empty_list(), integer()))) == {true, nil}

      # A list that the difference resolves to nothing
      list_with_tail =
        non_empty_list(atom(), opt_union(integer(), empty_list()))
        |> opt_difference(non_empty_list(atom([:ok]), integer()))
        |> opt_difference(non_empty_list(atom(), term()))

      assert list_of(list_with_tail) == :badproperlist
    end

    test "list_tl" do
      assert list_tl(none()) == :badnonemptylist
      assert list_tl(term()) == :badnonemptylist
      assert list_tl(empty_list()) == :badnonemptylist
      assert list_tl(list(integer())) == :badnonemptylist
      assert list_tl(opt_difference(list(number()), list(number()))) == :badnonemptylist

      assert list_tl(non_empty_list(integer())) == {:ok, list(integer())}

      assert list_tl(non_empty_list(integer(), atom())) ==
               {:ok, opt_union(atom(), non_empty_list(integer(), atom()))}

      # The tail of either a (non empty) list of integers with an atom tail or a (non empty) list
      # of tuples with a float tail is either an atom, or a float, or a (possibly empty) list of
      # integers with an atom tail, or a (possibly empty) list of tuples with a float tail.
      assert list_tl(
               opt_union(non_empty_list(integer(), atom()), non_empty_list(tuple(), float()))
             ) ==
               {:ok,
                opt_union(atom(), float())
                |> opt_union(non_empty_list(integer(), atom()))
                |> opt_union(non_empty_list(tuple(), float()))}

      assert list_tl(dynamic()) == {:ok, dynamic()}
      assert list_tl(dynamic(list(integer()))) == {:ok, dynamic(list(integer()))}

      assert list_tl(dynamic(list(integer(), atom()))) ==
               {:ok, dynamic(opt_union(atom(), list(integer(), atom())))}
    end

    test "tuple_fetch" do
      assert tuple_fetch(term(), 0) == :badtuple
      assert tuple_fetch(integer(), 0) == :badtuple
      assert tuple_fetch(tuple([none(), atom()]), 1) == :badtuple
      assert tuple_fetch(tuple([none()]), 0) == :badtuple

      assert tuple_fetch(tuple([integer(), atom()]), 0) == {false, integer()}
      assert tuple_fetch(tuple([integer(), atom()]), 1) == {false, atom()}
      assert tuple_fetch(tuple([integer(), atom()]), 2) == :badindex

      assert tuple_fetch(open_tuple([integer(), atom()]), 0) == {false, integer()}
      assert tuple_fetch(open_tuple([integer(), atom()]), 1) == {false, atom()}
      assert tuple_fetch(open_tuple([integer(), atom()]), 2) == :badindex

      assert tuple_fetch(tuple([integer(), atom()]), -1) == :badindex
      assert tuple_fetch(empty_tuple(), 0) == :badindex
      assert opt_difference(tuple(), tuple()) |> tuple_fetch(0) == :badtuple

      assert tuple([atom()]) |> opt_difference(empty_tuple()) |> tuple_fetch(0) ==
               {false, atom()}

      assert opt_difference(tuple([opt_union(integer(), atom())]), open_tuple([atom()]))
             |> tuple_fetch(0) == {false, integer()}

      assert tuple_fetch(opt_union(tuple([integer(), atom()]), dynamic(open_tuple([atom()]))), 1)
             |> Kernel.then(fn {opt, ty} -> opt and equal?(ty, opt_union(atom(), dynamic())) end)

      assert tuple_fetch(opt_union(tuple([integer()]), tuple([atom()])), 0) ==
               {false, opt_union(integer(), atom())}

      assert tuple([integer(), atom(), opt_union(atom(), integer())])
             |> opt_difference(tuple([integer(), term(), atom()]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple([integer(), atom(), opt_union(opt_union(atom(), integer()), list(term()))])
             |> opt_difference(tuple([integer(), term(), atom()]))
             |> opt_difference(open_tuple([term(), atom(), list(term())]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple([integer(), atom(), integer()])
             |> opt_difference(tuple([integer(), term(), integer()]))
             |> tuple_fetch(1) == :badtuple

      assert tuple([integer(), atom(), integer()])
             |> opt_difference(tuple([integer(), term(), atom()]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple_fetch(tuple(), 0) == :badindex

      assert tuple_fetch(projected_negative_tuple(200), 1) == {false, term()}
    end

    test "tuple_fetch with dynamic" do
      assert tuple_fetch(dynamic(), 0) == {true, dynamic()}
      assert tuple_fetch(dynamic(empty_tuple()), 0) == :badindex
      assert tuple_fetch(dynamic(tuple([integer(), atom()])), 2) == :badindex
      assert tuple_fetch(opt_union(dynamic(), integer()), 0) == :badtuple
      assert tuple_fetch(tuple([none()]), 0) == :badtuple

      assert tuple_fetch(dynamic(tuple()), 0)
             |> Kernel.then(fn {opt, type} -> opt and equal?(type, dynamic()) end)

      assert tuple_fetch(opt_union(dynamic(), open_tuple([atom()])), 0) ==
               {true, opt_union(atom(), dynamic())}
    end

    test "tuple_delete_at" do
      assert tuple_delete_at(tuple([integer(), atom()]), 3) == :badindex
      assert tuple_delete_at(tuple([integer(), atom()]), -1) == :badindex
      assert tuple_delete_at(empty_tuple(), 0) == :badindex
      assert tuple_delete_at(integer(), 0) == :badtuple
      assert tuple_delete_at(term(), 0) == :badtuple
      assert tuple_delete_at(tuple([none()]), 0) == :badtuple

      # Test deleting an element from a closed tuple
      assert tuple_delete_at(tuple([integer(), atom(), boolean()]), 1) ==
               tuple([integer(), boolean()])

      # Test deleting the last element from a closed tuple
      assert tuple_delete_at(tuple([integer(), atom()]), 1) ==
               tuple([integer()])

      # Test deleting from an open tuple
      assert tuple_delete_at(open_tuple([integer(), atom(), boolean()]), 1) ==
               open_tuple([integer(), boolean()])

      # Test deleting from a dynamic tuple
      assert tuple_delete_at(dynamic(tuple([integer(), atom()])), 1) ==
               dynamic(tuple([integer()]))

      assert tuple_delete_at(dynamic(tuple([integer(), atom()])), 2) == :badindex

      # Test deleting from a union of tuples
      assert tuple_delete_at(opt_union(tuple([integer(), atom()]), tuple([float(), binary()])), 1)
             |> equal?(tuple([opt_union(integer(), float())]))

      # Test deleting from an intersection of tuples
      assert opt_intersection(tuple([integer(), atom()]), tuple([term(), boolean()]))
             |> tuple_delete_at(1) == tuple([integer()])

      # Test deleting from a difference of tuples
      assert opt_difference(tuple([integer(), atom(), boolean()]), tuple([term(), term()]))
             |> tuple_delete_at(1)
             |> equal?(tuple([integer(), boolean()]))

      assert opt_difference(
               open_tuple([open_tuple([]), term()]),
               open_tuple([open_tuple([atom([:value])]), integer()])
             )
             |> tuple_delete_at(1)
             |> equal?(open_tuple([open_tuple([])]))

      # Test deleting from a complex union involving dynamic
      assert opt_union(tuple([integer(), atom()]), dynamic(tuple([float(), binary()])))
             |> tuple_delete_at(1)
             |> equal?(opt_union(tuple([integer()]), dynamic(tuple([float()]))))

      # Successfully deleting at position `index` in a tuple means that the dynamic
      # values that succeed are intersected with tuples of size at least `index`
      assert dynamic(tuple()) |> tuple_delete_at(0) == dynamic(tuple())
      assert dynamic(term()) |> tuple_delete_at(0) == dynamic(tuple())

      assert dynamic(opt_union(tuple(), integer()))
             |> tuple_delete_at(1)
             |> equal?(dynamic(tuple_of_size_at_least(1)))
    end

    test "tuple_insert_at" do
      assert tuple_insert_at(tuple([integer(), atom()]), 3, boolean()) == :badindex
      assert tuple_insert_at(tuple([integer(), atom()]), -1, boolean()) == :badindex
      assert tuple_insert_at(integer(), 0, boolean()) == :badtuple
      assert tuple_insert_at(term(), 0, boolean()) == :badtuple
      assert tuple_insert_at(tuple([none()]), 0, boolean()) == :badtuple

      # Out-of-bounds in a union
      assert opt_union(tuple([integer(), atom()]), tuple([float()]))
             |> tuple_insert_at(2, boolean()) == :badindex

      # Test inserting into a closed tuple
      assert tuple_insert_at(tuple([integer(), atom()]), 1, boolean()) ==
               tuple([integer(), boolean(), atom()])

      # Test inserting at the beginning of a tuple
      assert tuple_insert_at(tuple([integer(), atom()]), 0, boolean()) ==
               tuple([boolean(), integer(), atom()])

      # Test inserting at the end of a tuple
      assert tuple_insert_at(tuple([integer(), atom()]), 2, boolean()) ==
               tuple([integer(), atom(), boolean()])

      # Test inserting into an empty tuple
      assert tuple_insert_at(empty_tuple(), 0, integer()) == tuple([integer()])

      # Test inserting into an open tuple
      assert tuple_insert_at(open_tuple([integer(), atom()]), 1, boolean()) ==
               open_tuple([integer(), boolean(), atom()])

      # Test inserting a dynamic type
      assert tuple_insert_at(tuple([integer(), atom()]), 1, dynamic()) ==
               dynamic(tuple([integer(), term(), atom()]))

      assert tuple_insert_at(
               tuple([boolean()]),
               1,
               opt_union(dynamic(integer()), atom([:inserted]))
             )
             |> equal?(
               opt_union(
                 tuple([boolean(), atom([:inserted])]),
                 dynamic(tuple([boolean(), integer()]))
               )
             )

      # Test inserting into a dynamic tuple
      assert tuple_insert_at(dynamic(tuple([integer(), atom()])), 1, boolean())
             |> equal?(dynamic(tuple([integer(), boolean(), atom()])))

      assert tuple_insert_at(dynamic(tuple([integer(), atom()])), 3, boolean()) == :badindex

      # Test inserting into a union of tuples
      assert tuple_insert_at(opt_union(tuple([integer()]), tuple([atom()])), 0, boolean()) ==
               opt_union(tuple([boolean(), integer()]), tuple([boolean(), atom()]))

      assert opt_difference(tuple(), empty_tuple())
             |> tuple_insert_at(1, boolean())
             |> equal?(open_tuple([term(), boolean()]))

      inserted = tuple_insert_at(projected_negative_tuple(200), 1, atom([:inserted]))
      assert tuple_fetch(inserted, 1) == {false, atom([:inserted])}

      # Test inserting into a difference of tuples
      assert opt_difference(tuple([integer(), atom(), boolean()]), tuple([term(), term()]))
             |> tuple_insert_at(1, float())
             |> equal?(tuple([integer(), float(), atom(), boolean()]))

      # Test inserting into a complex union involving dynamic
      assert opt_union(tuple([integer(), atom()]), dynamic(tuple([float(), binary()])))
             |> tuple_insert_at(1, boolean())
             |> equal?(
               opt_union(
                 tuple([integer(), boolean(), atom()]),
                 dynamic(tuple([float(), boolean(), binary()]))
               )
             )

      # If you successfully intersect at position index in a type, then the dynamic values
      # that succeed are intersected with tuples of size at least index
      assert dynamic(opt_union(tuple(), integer()))
             |> tuple_insert_at(1, boolean())
             |> equal?(dynamic(open_tuple([term(), boolean()])))

      # Errors must propagate even when the inserted value is dynamic
      assert tuple_insert_at(integer(), 0, dynamic()) == :badtuple
      assert tuple_insert_at(tuple([atom([:ok])]), 2, dynamic()) == :badindex
    end

    test "tuple_replace_at" do
      assert tuple_replace_at(tuple([integer(), atom()]), 2, boolean()) == :badindex
      assert tuple_replace_at(tuple([integer(), atom()]), -1, boolean()) == :badindex
      assert tuple_replace_at(empty_tuple(), 0, boolean()) == :badindex
      assert tuple_replace_at(integer(), 0, boolean()) == :badtuple
      assert tuple_replace_at(term(), 0, boolean()) == :badtuple
      assert tuple_replace_at(tuple([none()]), 0, boolean()) == :badtuple

      # Out-of-bounds in a union
      assert opt_union(tuple([integer(), atom()]), tuple([float()]))
             |> tuple_replace_at(1, boolean()) == :badindex

      # Test replacing an element in a closed tuple
      assert tuple_replace_at(tuple([integer(), atom(), boolean()]), 1, float()) ==
               tuple([integer(), float(), boolean()])

      # Test replacing the first element of a closed tuple
      assert tuple_replace_at(tuple([integer(), atom()]), 0, boolean()) ==
               tuple([boolean(), atom()])

      # Test replacing the last element of a closed tuple
      assert tuple_replace_at(tuple([integer(), atom()]), 1, boolean()) ==
               tuple([integer(), boolean()])

      # Test replacing in an open tuple
      assert tuple_replace_at(open_tuple([integer(), atom(), boolean()]), 1, float()) ==
               open_tuple([integer(), float(), boolean()])

      # Test replacing with a dynamic type
      assert tuple_replace_at(tuple([integer(), atom()]), 1, dynamic()) ==
               dynamic(tuple([integer(), term()]))

      # Test replacing in a dynamic tuple
      assert tuple_replace_at(dynamic(tuple([integer(), atom()])), 1, boolean())
             |> equal?(dynamic(tuple([integer(), boolean()])))

      # Test replacing in a union of tuples
      assert tuple_replace_at(opt_union(tuple([integer()]), tuple([atom()])), 0, boolean()) ==
               tuple([boolean()])

      # Test replacing in an intersection of tuples
      assert opt_intersection(tuple([integer(), atom()]), tuple([term(), boolean()]))
             |> tuple_replace_at(1, float()) == tuple([integer(), float()])

      # Replacing in a difference where the negation actually constrains the
      # positive (not just by arity). The replaced position drops its negative
      # constraint, the other positions keep theirs.
      assert opt_difference(tuple([atom(), atom()]), tuple([atom([:a]), term()]))
             |> tuple_replace_at(0, boolean())
             |> equal?(tuple([boolean(), atom()]))

      assert opt_difference(tuple([atom(), atom()]), tuple([atom([:a]), term()]))
             |> tuple_replace_at(1, boolean())
             |> equal?(opt_difference(tuple([atom(), boolean()]), tuple([atom([:a]), boolean()])))

      # Errors must propagate even when the replacement value is dynamic
      assert tuple_replace_at(integer(), 0, dynamic()) == :badtuple
      assert tuple_replace_at(term(), 0, dynamic()) == :badtuple
      assert tuple_replace_at(tuple([atom([:ok])]), 1, dynamic()) == :badindex
      assert tuple_replace_at(empty_tuple(), 0, dynamic()) == :badindex

      # Out-of-bounds writes to a dynamic fixed-size tuple must fail with :badindex
      assert tuple_replace_at(dynamic(tuple([atom([:ok]), term()])), 2, binary()) ==
               :badindex

      assert tuple_replace_at(dynamic(tuple([atom([:ok])])), 1, binary()) == :badindex

      # Test replacing in a complex union involving dynamic
      assert opt_union(tuple([integer(), atom()]), dynamic(tuple([float(), binary()])))
             |> tuple_replace_at(1, boolean())
             |> equal?(
               opt_union(
                 tuple([integer(), boolean()]),
                 dynamic(tuple([float(), boolean()]))
               )
             )

      # Successfully replacing at position `index` in a tuple means that the dynamic
      # values that succeed are intersected with tuples of size at least `index + 1`
      assert dynamic(tuple())
             |> tuple_replace_at(0, boolean())
             |> equal?(dynamic(open_tuple([boolean()])))

      assert dynamic(term())
             |> tuple_replace_at(0, boolean())
             |> equal?(dynamic(open_tuple([boolean()])))

      assert dynamic(opt_union(tuple(), integer()))
             |> tuple_replace_at(1, boolean())
             |> equal?(dynamic(open_tuple([term(), boolean()])))
    end

    test "tuple_values" do
      assert tuple_values(term()) == :badtuple
      assert tuple_values(dynamic()) == dynamic()
      assert tuple_values(integer()) == :badtuple
      assert tuple_values(tuple([none()])) == :badtuple
      assert tuple_values(tuple([])) == none()
      assert tuple_values(tuple()) == term()
      assert tuple_values(open_tuple([integer()])) == term()
      assert tuple_values(tuple([integer(), atom()])) == opt_union(integer(), atom())

      assert tuple_values(opt_union(tuple([float(), pid()]), tuple([reference()]))) ==
               opt_union(float(), opt_union(pid(), reference()))

      assert tuple_values(opt_difference(tuple([number(), atom()]), tuple([float(), term()]))) ==
               opt_union(integer(), atom())

      assert opt_union(tuple([atom([:ok])]), open_tuple([integer()]))
             |> opt_difference(open_tuple([term(), term()]))
             |> tuple_values() == opt_union(atom([:ok]), integer())

      assert tuple_values(
               opt_difference(tuple([number(), atom()]), tuple([float(), atom([:ok])]))
             ) ==
               opt_union(number(), atom())

      assert tuple_values(dynamic(tuple())) == dynamic()
      assert tuple_values(dynamic(tuple([integer()]))) == dynamic(integer())

      assert tuple_values(opt_union(dynamic(tuple([integer()])), tuple([atom()]))) ==
               opt_union(dynamic(integer()), atom())

      assert tuple_values(opt_union(dynamic(tuple()), integer())) == :badtuple
      assert tuple_values(dynamic(opt_union(integer(), tuple([atom()])))) == dynamic(atom())

      assert tuple_values(opt_union(dynamic(tuple([integer()])), tuple([integer()])))
             |> equal?(integer())
    end

    test "map_to_list" do
      assert map_to_list(:term) == :badmap
      assert map_to_list(integer()) == :badmap
      assert map_to_list(opt_union(open_map(), integer())) == :badmap
      assert map_to_list(none()) == :badmap
      assert map_to_list(dynamic()) == {:ok, dynamic(list(tuple([term(), term()])))}

      # A non existent map type is refused
      assert open_map()
             |> opt_difference(open_map(a: if_set(term()), c: if_set(term())))
             |> map_to_list() == :badmap

      assert map_to_list(empty_map()) == {:ok, empty_list()}
      assert map_to_list(open_map()) == {:ok, list(tuple([term(), term()]))}

      assert map_to_list(closed_map(a: integer())) ==
               {:ok, non_empty_list(tuple([atom([:a]), integer()]))}

      assert map_to_list(closed_map(a: term())) ==
               {:ok, non_empty_list(tuple([atom([:a]), term()]))}

      assert map_to_list(closed_map(a: integer(), b: atom())) ==
               {:ok,
                non_empty_list(
                  tuple([atom([:a]), integer()])
                  |> opt_union(tuple([atom([:b]), atom()]))
                )}

      assert map_to_list(opt_union(closed_map(a: float()), closed_map(b: pid()))) ==
               {:ok,
                non_empty_list(
                  tuple([atom([:a]), float()])
                  |> opt_union(tuple([atom([:b]), pid()]))
                )}

      # Test with struct-like descrs
      assert map_to_list(closed_map(__struct__: term())) ==
               {:ok, non_empty_list(tuple([atom([:__struct__]), term()]))}

      # Test with domain keys
      assert map_to_list(closed_map([{domain_key(:integer), binary()}])) ==
               {:ok, list(tuple([integer(), binary()]))}

      assert map_to_list(closed_map([{domain_key(:tuple), binary()}])) ==
               {:ok, list(tuple([tuple(), binary()]))}

      # Test with both atom keys and domain keys
      map_with_both =
        closed_map([
          {:a, atom([:ok])},
          {:b, float()},
          {domain_key(:integer), binary()},
          {domain_key(:tuple), pid()}
        ])

      assert map_to_list(map_with_both) ==
               {:ok,
                non_empty_list(
                  tuple([atom([:a]), atom([:ok])])
                  |> opt_union(tuple([atom([:b]), float()]))
                  |> opt_union(tuple([integer(), binary()]))
                  |> opt_union(tuple([tuple(), pid()]))
                )}

      # Test open maps - should return list of key-value tuples
      assert map_to_list(open_map()) == {:ok, list(tuple([term(), term()]))}
      assert map_to_list(open_map(a: integer())) == {:ok, non_empty_list(tuple([term(), term()]))}

      {:ok, list} = map_to_list(open_map([{domain_key(:integer), binary()}]))

      assert list(
               Enum.reduce(
                 [binary(), float(), pid(), port(), reference()] ++
                   [fun(), atom(), tuple(), open_map(), list(term(), term())],
                 tuple([integer(), binary()]),
                 fn domain, acc -> opt_union(acc, tuple([domain, term()])) end
               )
             )
             |> equal?(list)

      # Test with multiple domain keys
      multiple_domains =
        closed_map([
          {domain_key(:integer), atom([:int])},
          {domain_key(:float), atom([:float])},
          {domain_key(:atom), binary()},
          {domain_key(:binary), integer()},
          {domain_key(:tuple), float()}
        ])

      assert map_to_list(multiple_domains) ==
               {:ok,
                list(
                  tuple([integer(), atom([:int])])
                  |> opt_union(tuple([float(), atom([:float])]))
                  |> opt_union(tuple([atom(), binary()]))
                  |> opt_union(tuple([binary(), integer()]))
                  |> opt_union(tuple([tuple(), float()]))
                )}

      # Test dynamic maps
      assert map_to_list(dynamic(open_map())) ==
               {:ok, dynamic(list(tuple([term(), term()])))}

      assert map_to_list(dynamic(closed_map(a: integer()))) ==
               {:ok, dynamic(non_empty_list(tuple([atom([:a]), integer()])))}

      assert map_to_list(opt_union(dynamic(closed_map(a: integer())), closed_map(b: atom()))) ==
               {:ok,
                opt_union(
                  non_empty_list(tuple([atom([:b]), atom()])),
                  dynamic(
                    non_empty_list(
                      opt_union(
                        tuple([atom([:a]), integer()]),
                        tuple([atom([:b]), atom()])
                      )
                    )
                  )
                )}

      # A static integer is refused
      assert map_to_list(opt_union(dynamic(open_map()), integer())) == :badmap

      # Test with negations
      assert map_to_list(
               opt_difference(closed_map(a: integer(), b: atom()), closed_map(a: integer()))
             ) ==
               {:ok,
                non_empty_list(
                  tuple([atom([:a]), integer()])
                  |> opt_union(tuple([atom([:b]), atom()]))
                )}

      # If a key is removed entirely by a negation, it should not appear in the result
      assert closed_map(a: if_set(integer()), b: atom())
             |> opt_difference(closed_map(a: integer(), b: term()))
             |> map_to_list() ==
               {:ok, non_empty_list(tuple([atom([:b]), atom()]))}
    end

    test "domain_to_args" do
      # take complex tuples, normalize them, and check if they are still equal
      complex_tuples = [
        tuple([term(), atom(), number()])
        |> opt_difference(tuple([atom(), atom(), float()])),
        # overlapping union and difference producing multiple variants
        opt_difference(
          tuple([opt_union(atom(), pid()), opt_union(integer(), float())]),
          tuple([opt_union(atom(), pid()), float()])
        )
      ]

      Enum.each(complex_tuples, fn domain ->
        args = domain_to_args(domain)

        assert Enum.reduce(args, none(), &opt_union(args_to_domain(&1), &2))
               |> equal?(domain)
      end)
    end

    test "map_fetch_key" do
      assert map_fetch_key(term(), :a) == :badmap
      assert map_fetch_key(opt_union(open_map(), integer()), :a) == :badmap
      assert map_fetch_key(opt_difference(open_map(), open_map()), :a) == :badmap

      assert map_fetch_key(opt_difference(closed_map(a: integer()), closed_map(a: term())), :a) ==
               :badmap

      assert map_fetch_key(open_map(), :a) == :badkey
      assert map_fetch_key(open_map(a: not_set()), :a) == :badkey

      assert map_fetch_key(opt_union(closed_map(a: integer()), closed_map(b: atom())), :a) ==
               :badkey

      assert map_fetch_key(closed_map(a: integer()), :a) == {false, integer()}

      assert map_fetch_key(opt_union(closed_map(a: integer()), closed_map(a: atom())), :a) ==
               {false, opt_union(integer(), atom())}

      {false, value_type} =
        open_map(my_map: open_map(foo: integer()))
        |> opt_intersection(open_map(my_map: open_map(bar: boolean())))
        |> map_fetch_key(:my_map)

      assert equal?(value_type, open_map(foo: integer(), bar: boolean()))

      {false, value_type} =
        closed_map(a: opt_union(integer(), atom()))
        |> opt_difference(open_map(a: integer()))
        |> map_fetch_key(:a)

      assert equal?(value_type, atom())

      {false, value_type} =
        closed_map(a: integer(), b: atom())
        |> opt_difference(closed_map(a: integer(), b: atom([:foo])))
        |> map_fetch_key(:a)

      assert equal?(value_type, integer())

      {false, value_type} =
        closed_map(a: integer())
        |> opt_difference(closed_map(a: atom()))
        |> map_fetch_key(:a)

      assert equal?(value_type, integer())

      {false, value_type} =
        open_map(a: integer(), b: atom())
        |> opt_union(closed_map(a: tuple()))
        |> map_fetch_key(:a)

      assert equal?(value_type, opt_union(integer(), tuple()))

      {false, value_type} =
        closed_map(a: atom())
        |> opt_difference(closed_map(a: atom([:foo, :bar])))
        |> opt_difference(closed_map(a: atom([:bar])))
        |> map_fetch_key(:a)

      assert equal?(value_type, opt_intersection(atom(), opt_negation(atom([:foo, :bar]))))

      assert closed_map(a: opt_union(atom([:ok]), pid()), b: integer(), c: tuple())
             |> opt_difference(open_map(a: atom([:ok]), b: integer()))
             |> opt_difference(open_map(a: atom(), c: tuple()))
             |> map_fetch_key(:a) == {false, pid()}

      assert closed_map(a: opt_union(atom([:foo]), pid()), b: integer(), c: tuple())
             |> opt_difference(open_map(a: atom([:foo]), b: integer()))
             |> opt_difference(open_map(a: atom(), c: tuple()))
             |> map_fetch_key(:a) == {false, pid()}

      assert closed_map(a: opt_union(atom([:foo, :bar, :baz]), integer()))
             |> opt_difference(open_map(a: atom([:foo, :bar])))
             |> opt_difference(open_map(a: atom([:foo, :baz])))
             |> map_fetch_key(:a) == {false, integer()}
    end

    # Times out without a projection-only map_fetch_key path
    test "map_fetch_key with projected negative maps" do
      assert map_fetch_key(projected_negative_map(100), :k) == {false, open_map()}
    end

    test "map_fetch_key with dynamic" do
      assert map_fetch_key(dynamic(), :a) == {true, dynamic()}
      assert map_fetch_key(opt_union(dynamic(), integer()), :a) == :badmap
      assert map_fetch_key(opt_union(dynamic(open_map(a: integer())), integer()), :a) == :badmap
      assert map_fetch_key(opt_union(dynamic(integer()), integer()), :a) == :badmap

      assert opt_intersection(dynamic(), open_map(a: integer()))
             |> map_fetch_key(:a) == {false, opt_intersection(integer(), dynamic())}

      {false, type} = opt_union(dynamic(integer()), open_map(a: integer())) |> map_fetch_key(:a)
      assert equal?(type, integer())

      assert opt_union(dynamic(integer()), open_map(a: if_set(integer()))) |> map_fetch_key(:a) ==
               :badkey

      assert opt_union(dynamic(open_map(a: atom())), open_map(a: integer()))
             |> map_fetch_key(:a) == {false, opt_union(dynamic(atom()), integer())}
    end

    test "map_fetch_key with domain keys" do
      integer_to_atom = open_map([{domain_key(:integer), atom()}])
      assert map_fetch_key(integer_to_atom, :foo) == :badkey

      # the key :a is for sure of type pid and exists in type
      # %{atom() => pid()} and not %{:a => not_set()}
      t1 = closed_map([{domain_key(:atom), pid()}])
      t2 = closed_map(a: not_set())
      t3 = open_map(a: not_set())

      # Indeed, t2 is equivalent to the empty map
      assert map_fetch_key(opt_difference(t1, t2), :a) == :badkey
      assert map_fetch_key(opt_difference(t1, t3), :a) == {false, pid()}

      t4 = closed_map([{domain_key(:pid), atom()}])
      assert map_fetch_key(opt_difference(t1, t4) |> opt_difference(t3), :a) == {false, pid()}

      assert map_fetch_key(closed_map([{domain_key(:atom), pid()}]), :a) == :badkey

      assert map_fetch_key(dynamic(closed_map([{domain_key(:atom), pid()}])), :a) ==
               {true, dynamic(pid())}

      assert closed_map([{domain_key(:atom), number()}])
             |> opt_difference(open_map(a: if_set(integer())))
             |> map_fetch_key(:a) == {false, float()}

      assert closed_map([{domain_key(:atom), number()}])
             |> opt_difference(closed_map(b: if_set(integer())))
             |> map_fetch_key(:a) == :badkey
    end
  end

  describe "numberize" do
    test "with static" do
      assert numberize(term()) == term()

      assert open_map(list: list(integer(), atom()), tuple: tuple([float(), binary(), integer()]))
             |> numberize() ==
               open_map(
                 list: list(number(), atom()),
                 tuple: tuple([number(), binary(), number()])
               )
    end

    test "with dynamic" do
      assert numberize(dynamic()) == dynamic()

      assert dynamic(list(binary(), float())) |> numberize() ==
               dynamic(list(binary(), number()))
    end
  end

  describe "map_get" do
    test "with domain keys" do
      assert map_get(term(), term()) == :badmap

      map_type = closed_map([{domain_key(:tuple), binary()}])
      assert map_get(map_type, tuple()) == {:ok, binary()}

      # Type with all domain types
      # %{:bar => :ok, integer() => :int, float() => :float, atom() => binary(), binary() => integer(), tuple() => float(), map() => pid(), reference() => port(), pid() => boolean()}
      all_domains =
        closed_map([
          {:bar, atom([:ok])},
          {domain_key(:integer), atom([:int])},
          {domain_key(:float), atom([:float])},
          {domain_key(:atom), binary()},
          {domain_key(:binary), integer()},
          {domain_key(:tuple), float()},
          {domain_key(:map), pid()},
          {domain_key(:reference), port()},
          {domain_key(:pid), reference()},
          {domain_key(:port), boolean()}
        ])

      assert map_get(all_domains, atom([:bar])) == {:ok, atom([:ok])}

      assert map_get(all_domains, integer()) == {:ok, atom([:int])}
      assert map_get(all_domains, number()) == {:ok, atom([:int, :float])}

      assert map_get(all_domains, empty_list()) == :error
      assert map_get(all_domains, atom([:foo])) == {:ok, binary()}
      assert map_get(all_domains, binary()) == {:ok, integer()}
      assert map_get(all_domains, tuple([integer(), atom()])) == {:ok, float()}
      assert map_get(all_domains, empty_map()) == {:ok, pid()}

      # Union
      assert map_get(all_domains, opt_union(tuple(), empty_map())) ==
               {:ok, opt_union(float(), pid())}

      # Removing all maps with tuple keys
      t_no_tuple = opt_difference(all_domains, closed_map([{domain_key(:tuple), float()}]))
      t_really_no_tuple = opt_difference(all_domains, open_map([{domain_key(:tuple), float()}]))
      assert subtype?(all_domains, open_map())
      # It's only closed maps, so it should not change
      assert map_get(t_no_tuple, tuple()) == {:ok, float()}
      # This time we actually removed all tuple to float keys
      assert map_get(t_really_no_tuple, tuple()) == :error

      t1 = closed_map([{domain_key(:tuple), integer()}])
      t2 = closed_map([{domain_key(:tuple), float()}])
      t3 = opt_union(t1, t2)
      assert map_get(t3, tuple()) == {:ok, number()}
    end

    test "with dynamic" do
      assert map_get(dynamic(), term()) == {:ok, dynamic()}
    end

    test "with atom fall back" do
      map = closed_map([{:a, atom([:a])}, {:b, atom([:b])}, {domain_key(:atom), pid()}])

      assert map_get(map, atom([:a, :b])) ==
               {:ok, atom([:a, :b])}

      assert map_get(map, atom([:a, :c])) ==
               {:ok, opt_union(atom([:a]), pid())}

      assert map_get(map, atom() |> opt_difference(atom([:a, :b]))) ==
               {:ok, pid()}

      assert map_get(map, atom() |> opt_difference(atom([:a]))) ==
               {:ok, opt_union(atom([:b]), pid())}

      assert map_get(closed_map(a: atom([:a]), b: atom([:b])), atom()) ==
               {:ok, atom([:a, :b])}

      assert map_get(closed_map([{domain_key(:atom), integer()}]), atom([:a, :b])) ==
               {:ok, integer()}

      # Have one of the keys be a __struct__
      map = closed_map([{:a, atom([:a])}, {:__struct__, term()}, {domain_key(:atom), pid()}])
      {:ok, term} = map_get(map, atom() |> opt_difference(atom([:a])))
      assert equal?(term, term())

      base = open_map([{domain_key(:atom), term()}])
      bad = open_map(a: if_set(opt_negation(integer())))
      map = opt_negation(opt_union(opt_negation(base), bad))

      assert equal?(map, open_map(a: integer()))

      {:ok, type} = map_get(map, atom())
      assert equal?(type, term())

      {:ok, type} = map_get(map, atom([:a]))
      assert equal?(type, integer())

      map = closed_map([{:a, term()}, {domain_key(:atom), integer()}])

      {:ok, type} = map_get(map, atom())
      assert equal?(type, term())

      {:ok, type} = map_get(map, atom([:a]))
      assert equal?(type, term())

      {:ok, type} = map_get(map, opt_difference(atom(), atom([:a])))
      assert equal?(type, integer())

      map =
        closed_map([{:a, term()}, {domain_key(:atom), integer()}])
        |> opt_difference(open_map(a: opt_negation(pid())))

      {:ok, type} = map_get(map, atom())
      assert equal?(type, opt_union(integer(), pid()))

      {:ok, type} = map_get(map, atom([:a]))
      assert equal?(type, pid())

      {:ok, type} = map_get(map, opt_difference(atom(), atom([:a])))
      assert equal?(type, integer())

      map =
        closed_map([{:a, term()}, {:b, binary()}, {domain_key(:atom), integer()}])
        |> opt_difference(open_map(a: opt_negation(pid())))

      {:ok, type} = map_get(map, atom())
      assert equal?(type, opt_union(opt_union(integer(), pid()), binary()))

      {:ok, type} = map_get(map, atom([:a, :b]))
      assert equal?(type, opt_union(pid(), binary()))

      {:ok, type} = map_get(map, opt_difference(atom(), atom([:a, :b])))
      assert equal?(type, integer())
    end

    test "with lists" do
      # Verify that empty_list() bitmap type maps to :list domain (not :empty_list domain)
      map_with_list_domain = closed_map([{domain_key(:list), atom([:empty])}])

      # empty_list() should access the :list domain
      assert map_get(map_with_list_domain, empty_list()) == {:ok, atom([:empty])}

      # non_empty_list() should also access the :list domain
      assert map_get(map_with_list_domain, non_empty_list(integer())) ==
               {:ok, atom([:empty])}

      # list() should also access the :list domain
      assert map_get(map_with_list_domain, list(integer())) ==
               {:ok, atom([:empty])}

      # If I create a map and instantiate both empty_list() and non_empty_list(integer()), it should return the union of the two types
      map =
        closed_map([{domain_key(:list), atom([:empty])}, {domain_key(:list), atom([:non_empty])}])

      assert map_get(map, empty_list()) == {:ok, atom([:empty, :non_empty])}

      assert map_get(map, non_empty_list(integer())) ==
               {:ok, atom([:empty, :non_empty])}

      assert map_get(map, list(integer())) == {:ok, atom([:empty, :non_empty])}
    end

    # Times out without a projection-only map_get path
    test "with projected negative maps" do
      assert map_get(projected_negative_map(100), atom([:k])) == {:ok, open_map()}
    end
  end

  describe "map_update" do
    test "with static atom keys" do
      assert map_update(open_map(key: binary()), atom([:key]), integer()) ==
               {binary(), open_map(key: integer()), []}

      assert map_update(dynamic(open_map(key: binary())), atom([:key]), integer()) ==
               {dynamic(binary()), dynamic(open_map(key: integer())), []}

      # Optional fail for static maps
      assert map_update(open_map(key: if_set(atom([:value]))), atom([:key]), integer()) ==
               {:error, [badkey: :key]}

      # ...unless forcing
      assert map_update(
               open_map(key: if_set(atom([:value]))),
               atom([:key]),
               integer(),
               true,
               true
             ) ==
               {atom([:value]), open_map(key: integer()), []}

      # But optional does not fail for dynamic ones
      assert map_update(dynamic(open_map(key: if_set(atom([:value])))), atom([:key]), integer()) ==
               {dynamic(atom([:value])), dynamic(open_map(key: integer())), []}

      assert map_update(dynamic(), atom([:key]), integer()) ==
               {dynamic(), dynamic(open_map(key: integer())), []}

      # Empty value fails for static maps
      assert map_update(closed_map(key: not_set()), atom([:key]), integer()) ==
               {:error, [badkey: :key]}

      # ...unless forcing
      assert map_update(closed_map(key: not_set()), atom([:key]), integer(), true, true) ==
               {none(), closed_map(key: integer()), []}

      # When putting multiple keys, we don't know which one will be set
      assert map_update(open_map(key1: atom(), key2: binary()), atom([:key1, :key2]), integer()) ==
               {opt_union(atom(), binary()),
                opt_union(
                  open_map(key1: atom(), key2: integer()),
                  open_map(key1: integer(), key2: binary())
                ), []}

      # When putting multiple keys, all have to be set
      assert map_update(open_map(key1: atom(), key2: binary()), atom([:key1, :key3]), integer()) ==
               {term(),
                opt_union(
                  open_map(key1: integer(), key2: binary()),
                  open_map(key1: atom(), key2: binary(), key3: integer())
                ), [badkey: :key3]}

      # ...unless forcing
      assert map_update(
               open_map(key1: atom(), key2: binary()),
               atom([:key1, :key3]),
               integer(),
               true,
               true
             ) ==
               {term(),
                opt_union(
                  open_map(key1: integer(), key2: binary()),
                  open_map(key1: atom(), key2: binary(), key3: integer())
                ), []}

      # ...unless dynamic
      assert map_update(dynamic(open_map()), atom([:key1, :key2]), integer()) ==
               {dynamic(),
                dynamic(opt_union(open_map(key1: integer()), open_map(key2: integer()))), []}

      # A "none" map
      assert open_map()
             |> opt_difference(open_map(a: if_set(term()), c: if_set(term())))
             |> map_update(atom([:b]), integer()) == {:error, [badkey: :b]}

      # ... even when forcing
      assert open_map()
             |> opt_difference(open_map(a: if_set(term()), c: if_set(term())))
             |> map_update(atom([:b]), integer(), true, true) == {none(), none(), []}
    end

    # Times out without a projection-aware map_update path
    test "with projected negative maps" do
      assert map_update(projected_negative_map(100), atom([:k]), binary()) ==
               {open_map(), open_map(k: binary(), x: term()), []}
    end

    test "with non-empty open maps does not call the callback with none from absent branches" do
      # This is a test of the map_update_fun/5 with forced?: false parameter.
      # We check that it does not call its typed_fun argument with `none()`
      # due to the key being absent in the map.
      type = dynamic(opt_difference(open_map(), empty_map()))

      fun = fn _optional?, value ->
        send(self(), :callback_invoked)
        value
      end

      assert map_update_fun(type, binary(), fun, false, false) == {dynamic(none()), type, []}
      refute_received :callback_invoked
    end

    test "with dynamic atom keys" do
      assert {type, descr, errors} =
               map_update(closed_map(key: atom([:value])), dynamic(), atom([:new_value]))

      assert equal?(type, atom([:value]))
      assert equal?(descr, closed_map(key: atom([:value, :new_value])))
      assert errors == []

      assert {type, descr, errors} =
               map_update(
                 dynamic(closed_map(key: atom([:value]))),
                 dynamic(),
                 atom([:new_value])
               )

      assert equal?(type, dynamic(atom([:value])))
      assert equal?(descr, dynamic(closed_map(key: atom([:value, :new_value]))))
      assert errors == []

      # Check struct fields
      assert {type, descr, errors} =
               map_update(open_map(__struct__: term()), dynamic(atom()), integer())

      assert type == term()
      assert equal?(descr, open_map(__struct__: term()))
      assert errors == []

      # When precise dynamic keys are given, at least one must succeed
      assert map_update(
               closed_map(key1: atom(), key2: binary()),
               dynamic(atom([:key1, :key3])),
               integer()
             ) ==
               {atom(), closed_map(key1: integer(), key2: binary()), []}

      assert map_update(
               closed_map(key1: atom(), key2: binary()),
               dynamic(atom([:key3, :key4])),
               integer()
             ) == {:error, []}

      # ...unless forcing
      assert map_update(
               closed_map(key1: atom(), key2: binary()),
               dynamic(atom([:key3, :key4])),
               integer(),
               true,
               true
             ) ==
               {none(),
                opt_union(
                  closed_map(key1: atom(), key2: binary(), key3: integer()),
                  closed_map(key1: atom(), key2: binary(), key4: integer())
                ), []}

      # ...unless open
      assert map_update(
               open_map(key1: atom(), key2: binary()),
               dynamic(atom([:key1, :key3])),
               integer()
             ) ==
               {term(),
                opt_union(
                  open_map(key1: integer(), key2: binary()),
                  open_map(key1: atom(), key2: binary(), key3: integer())
                ), []}

      assert map_update(
               open_map(key1: atom(), key2: binary()),
               dynamic(atom([:key3, :key4])),
               integer()
             ) == {:error, []}
    end

    test "with domain keys" do
      map =
        closed_map([
          {domain_key(:integer), binary()},
          {domain_key(:pid), binary()},
          {domain_key(:port), binary()}
        ])

      assert map_update(map, none(), integer()) ==
               {:error, []}

      assert map_update(map, integer(), integer()) ==
               {binary(),
                closed_map([
                  {domain_key(:integer), opt_union(integer(), binary())},
                  {domain_key(:pid), binary()},
                  {domain_key(:port), binary()}
                ]), []}

      assert map_update(map, opt_union(pid(), integer()), integer()) ==
               {binary(),
                closed_map([
                  {domain_key(:integer), opt_union(integer(), binary())},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()}
                ]), []}

      assert map_update(map, opt_union(pid(), reference()), integer()) ==
               {binary(),
                closed_map([
                  {domain_key(:integer), binary()},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()}
                ]), [baddomain: reference()]}

      assert map_update(
               map,
               opt_union(pid(), dynamic(opt_union(reference(), integer()))),
               integer()
             ) ==
               {binary(),
                closed_map([
                  {domain_key(:integer), opt_union(integer(), binary())},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()}
                ]), []}

      assert map_update(
               map,
               opt_union(pid(), dynamic(opt_union(reference(), binary()))),
               integer()
             ) ==
               {binary(),
                closed_map([
                  {domain_key(:integer), binary()},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()}
                ]), []}

      assert map_update(map, dynamic(opt_union(reference(), binary())), integer()) ==
               {:error, []}

      # ... unless forcing
      assert map_update(map, dynamic(opt_union(reference(), binary())), integer(), true, true) ==
               {none(),
                closed_map([
                  {domain_key(:integer), binary()},
                  {domain_key(:pid), binary()},
                  {domain_key(:port), binary()},
                  {domain_key(:binary), integer()},
                  {domain_key(:reference), integer()}
                ]), []}

      # Putting dynamic atom over record keys
      assert {type, descr, errors} =
               map_update(closed_map(key1: binary(), key2: pid()), atom(), integer())

      assert equal?(type, opt_union(binary(), pid()))

      assert equal?(
               descr,
               opt_union(
                 closed_map(key1: binary(), key2: integer()),
                 closed_map(key1: opt_union(integer(), binary()), key2: pid())
               )
             )

      assert errors == [baddomain: atom()]

      # ... unless forcing
      assert map_update(closed_map(key1: binary(), key2: pid()), atom(), integer(), true, true) ==
               {opt_union(binary(), pid()),
                [
                  closed_map([{domain_key(:atom), integer()}, key1: binary(), key2: pid()]),
                  closed_map(key1: integer(), key2: pid()),
                  closed_map(key1: binary(), key2: integer())
                ]
                |> Enum.reduce(&opt_union/2), [baddomain: atom()]}

      assert {type, descr, errors} =
               map_update(closed_map(key1: binary(), key2: pid()), dynamic(atom()), integer())

      assert equal?(type, opt_union(binary(), pid()))

      assert equal?(
               descr,
               opt_union(
                 closed_map(key1: binary(), key2: integer()),
                 closed_map(key1: opt_union(integer(), binary()), key2: pid())
               )
             )

      assert errors == []

      # A "none()" map
      assert open_map()
             |> opt_difference(open_map(a: if_set(term()), c: if_set(term())))
             |> map_update(binary(), integer()) == {:error, [baddomain: binary()]}

      # ... even when forcing
      {type, descr, errors} =
        open_map()
        |> opt_difference(open_map(a: if_set(term()), c: if_set(term())))
        |> map_update(binary(), integer(), true, true)

      assert empty?(type)
      assert empty?(descr)
      assert errors == [baddomain: binary()]
    end

    test "with mixed keys" do
      assert map_update(dynamic(), opt_union(atom([:key]), binary()), integer()) ==
               {dynamic(), dynamic(open_map()), []}

      # When precise dynamic keys are given, at least one must succeed
      assert map_update(
               closed_map([{:key, atom()}, {domain_key(:integer), binary()}]),
               dynamic(opt_union(atom([:key]), integer())),
               integer()
             ) ==
               {opt_union(atom(), binary()),
                opt_union(
                  closed_map([{:key, integer()}, {domain_key(:integer), binary()}]),
                  closed_map([
                    {:key, atom()},
                    {domain_key(:integer), opt_union(binary(), integer())}
                  ])
                ), []}

      # Negated keys
      assert {type, descr, errors} =
               map_update(
                 closed_map(key1: binary(), key2: binary()),
                 opt_difference(atom(), atom([:key1])),
                 integer()
               )

      assert equal?(type, binary())
      assert equal?(descr, closed_map(key1: binary(), key2: opt_union(integer(), binary())))
      assert errors == [baddomain: atom()]

      assert map_update(
               closed_map([key1: binary(), key2: binary()] ++ [{domain_key(:atom), pid()}]),
               opt_difference(atom(), atom([:key1])),
               integer()
             ) ==
               {opt_union(binary(), pid()),
                opt_union(
                  closed_map([{domain_key(:atom), pid()}, key1: binary(), key2: integer()]),
                  closed_map([
                    {domain_key(:atom), opt_union(pid(), integer())},
                    key1: binary(),
                    key2: binary()
                  ])
                ), []}

      # Missing keys
      assert map_update(
               closed_map([{:key, atom()}, {domain_key(:integer), binary()}]),
               dynamic(opt_union(atom([:other_key]), pid())),
               integer()
             ) == {:error, []}

      # ...unless forcing
      assert map_update(
               closed_map([{:key, atom()}, {domain_key(:integer), binary()}]),
               dynamic(opt_union(atom([:other_key]), pid())),
               integer(),
               true,
               true
             ) ==
               {none(),
                opt_union(
                  closed_map([
                    {:key, atom()},
                    {domain_key(:integer), binary()},
                    {domain_key(:pid), integer()}
                  ]),
                  closed_map([
                    {:key, atom()},
                    {:other_key, integer()},
                    {domain_key(:integer), binary()}
                  ])
                ), []}

      # Popping dynamic keys
      non_struct_map = opt_difference(open_map(), open_map(__struct__: atom()))
      {type, descr, []} = map_update(non_struct_map, dynamic(), not_set(), true, true)
      assert type == term()
      assert equal?(descr, open_map(__struct__: if_set(opt_negation(atom()))))
    end
  end

  describe "map_put" do
    test "with static atom keys" do
      assert map_put(open_map(key: binary()), atom([:key]), integer()) ==
               {:ok, open_map(key: integer())}

      assert map_put(dynamic(open_map(key: binary())), atom([:key]), integer()) ==
               {:ok, dynamic(open_map(key: integer()))}

      # Optional does not fail on put keys
      assert map_put(open_map(key: if_set(atom([:value]))), atom([:key]), integer()) ==
               {:ok, open_map(key: integer())}

      # But optional does not fail for dynamic ones
      assert map_put(dynamic(open_map(key: if_set(atom([:value])))), atom([:key]), integer()) ==
               {:ok, dynamic(open_map(key: integer()))}

      assert map_put(dynamic(), atom([:key]), integer()) ==
               {:ok, dynamic(open_map(key: integer()))}

      # Empty value does not fail for put
      assert map_put(closed_map(key: not_set()), atom([:key]), integer()) ==
               {:ok, closed_map(key: integer())}

      # When putting multiple keys, we don't know which one will be set
      assert map_put(open_map(key1: atom(), key2: binary()), atom([:key1, :key2]), integer()) ==
               {:ok,
                opt_union(
                  open_map(key1: atom(), key2: integer()),
                  open_map(key1: integer(), key2: binary())
                )}

      # When putting multiple keys, set even missing keys
      assert map_put(open_map(key1: atom(), key2: binary()), atom([:key1, :key3]), integer()) ==
               {:ok,
                opt_union(
                  open_map(key1: atom(), key2: binary(), key3: integer()),
                  open_map(key1: integer(), key2: binary())
                )}

      assert map_put(dynamic(open_map()), atom([:key1, :key2]), integer()) ==
               {:ok, dynamic(opt_union(open_map(key1: integer()), open_map(key2: integer())))}
    end

    test "with dynamic/term as key-value" do
      assert map_put(closed_map(key: atom([:value])), dynamic(), dynamic()) ==
               {:ok, dynamic(open_map())}

      assert map_put(closed_map(key: atom([:value])), dynamic(), term()) ==
               {:ok, open_map()}

      assert map_put(closed_map(key: atom([:value])), term(), dynamic()) ==
               {:ok, dynamic(open_map())}

      assert map_put(closed_map(key: atom([:value])), term(), term()) ==
               {:ok, open_map()}

      assert map_put(dynamic(closed_map(key: atom([:value]))), term(), term()) ==
               {:ok, dynamic(open_map())}
    end

    test "with dynamic atom keys" do
      assert map_put(
               open_map(key1: atom(), key2: binary()),
               dynamic(atom([:key1, :key3])),
               integer()
             ) ==
               {:ok,
                opt_union(
                  open_map(key1: atom(), key2: binary(), key3: integer()),
                  open_map(key1: integer(), key2: binary())
                )}

      assert map_put(
               open_map(key1: atom(), key2: binary()),
               dynamic(atom([:key3, :key4])),
               integer()
             ) ==
               {:ok,
                opt_union(
                  open_map(key1: atom(), key2: binary(), key3: integer()),
                  open_map(key1: atom(), key2: binary(), key4: integer())
                )}
    end

    test "with domain keys" do
      map =
        closed_map([
          {domain_key(:integer), binary()},
          {domain_key(:pid), binary()},
          {domain_key(:port), binary()}
        ])

      assert map_put(map, integer(), integer()) ==
               {:ok,
                closed_map([
                  {domain_key(:integer), opt_union(integer(), binary())},
                  {domain_key(:pid), binary()},
                  {domain_key(:port), binary()}
                ])}

      assert map_put(map, opt_union(pid(), integer()), integer()) ==
               {:ok,
                closed_map([
                  {domain_key(:integer), opt_union(integer(), binary())},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()}
                ])}

      assert map_put(map, opt_union(pid(), reference()), integer()) ==
               {:ok,
                closed_map([
                  {domain_key(:integer), binary()},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()},
                  {domain_key(:reference), integer()}
                ])}

      assert map_put(map, opt_union(pid(), dynamic(opt_union(reference(), integer()))), integer()) ==
               {:ok,
                closed_map([
                  {domain_key(:integer), opt_union(integer(), binary())},
                  {domain_key(:pid), opt_union(integer(), binary())},
                  {domain_key(:port), binary()},
                  {domain_key(:reference), integer()}
                ])}

      assert map_put(map, dynamic(opt_union(reference(), binary())), integer()) ==
               {:ok,
                closed_map([
                  {domain_key(:integer), binary()},
                  {domain_key(:pid), binary()},
                  {domain_key(:port), binary()},
                  {domain_key(:reference), integer()},
                  {domain_key(:binary), integer()}
                ])}

      # Putting dynamic atom over record keys
      assert map_put(closed_map(key1: binary(), key2: binary()), atom(), integer()) ==
               {:ok,
                [
                  closed_map(key1: binary(), key2: integer()),
                  closed_map(key1: integer(), key2: binary()),
                  closed_map([{domain_key(:atom), integer()}, key1: binary(), key2: binary()])
                ]
                |> Enum.reduce(&opt_union/2)}
    end

    test "with mixed keys" do
      assert map_put(dynamic(), opt_union(atom([:key]), binary()), integer()) ==
               {:ok, dynamic(open_map())}

      # When precise dynamic keys are given, at least one must succeed
      assert map_put(
               closed_map([{:key, atom()}, {domain_key(:integer), binary()}]),
               dynamic(opt_union(atom([:key]), integer())),
               integer()
             ) ==
               {:ok,
                opt_union(
                  closed_map([{:key, integer()}, {domain_key(:integer), binary()}]),
                  closed_map([
                    {:key, atom()},
                    {domain_key(:integer), opt_union(binary(), integer())}
                  ])
                )}

      assert map_put(
               closed_map([{:key, atom()}, {domain_key(:integer), binary()}]),
               dynamic(opt_union(atom([:other_key]), pid())),
               integer()
             ) ==
               {:ok,
                opt_union(
                  closed_map([
                    {:key, atom()},
                    {:other_key, integer()},
                    {domain_key(:integer), binary()}
                  ]),
                  closed_map([
                    {:key, atom()},
                    {domain_key(:integer), binary()},
                    {domain_key(:pid), integer()}
                  ])
                )}

      # Negated keys
      assert map_put(
               closed_map(key1: binary(), key2: binary()),
               opt_difference(atom(), atom([:key1])),
               integer()
             ) ==
               {:ok,
                opt_union(
                  closed_map(key1: binary(), key2: integer()),
                  closed_map([{domain_key(:atom), integer()}, key1: binary(), key2: binary()])
                )}

      assert map_put(
               closed_map([key1: binary(), key2: binary()] ++ [{domain_key(:atom), pid()}]),
               opt_difference(atom(), atom([:key1])),
               integer()
             ) ==
               {:ok,
                opt_union(
                  closed_map([{domain_key(:atom), pid()}, key1: binary(), key2: integer()]),
                  closed_map([
                    {domain_key(:atom), opt_union(integer(), pid())},
                    key1: binary(),
                    key2: binary()
                  ])
                )}
    end

    # Times out without proper map_put
    test "with projected negative maps" do
      map = projected_negative_map(100)

      assert map_put(map, atom([:k]), binary()) == {:ok, open_map(k: binary(), x: term())}

      map =
        opt_difference(open_map(k: integer(), x: term()), open_map(k: integer(), a: integer()))

      {:ok, type} = map_put(map, atom([:k]), binary())

      assert equal?(
               type,
               opt_difference(
                 open_map(k: binary(), x: term()),
                 open_map(k: binary(), a: integer())
               )
             )
    end

    test "with projected negative maps and no popped value projection" do
      # map_put/3 passes nil as the popped value accumulator because it only needs the map side.
      map =
        projected_negative_map(100)
        |> opt_difference(open_map(k: atom(), x: term()))

      assert map_put(map, atom([:k]), binary()) == {:ok, open_map(k: binary(), x: term())}
    end
  end

  describe "disjoint" do
    test "optional" do
      assert disjoint?(term(), if_set(none()))
      assert disjoint?(term(), if_set(none()) |> opt_union(non_empty_list(none())))
    end

    test "map" do
      refute disjoint?(open_map(), open_map(a: integer()))
    end
  end

  describe "to_quoted" do
    test "none" do
      assert none() |> to_quoted_string() == "none()"
      assert dynamic(none()) |> to_quoted_string() == "none()"
    end

    test "bitmap" do
      assert opt_union(pid(), bitstring()) |> to_quoted_string() ==
               "bitstring() or pid()"

      assert opt_union(integer(), opt_union(float(), binary())) |> to_quoted_string() ==
               "binary() or float() or integer()"

      assert opt_difference(bitstring(), binary()) |> opt_union(integer()) |> to_quoted_string() ==
               "(bitstring() and not binary()) or integer()"
    end

    test "bitmap (negation)" do
      assert opt_union(pid(), bitstring()) |> opt_negation() |> to_quoted_string() ==
               "not bitstring() and not pid()"

      assert opt_difference(bitstring(), binary())
             |> opt_union(integer())
             |> opt_negation()
             |> to_quoted_string() ==
               "not (bitstring() and not binary()) and not integer()"
    end

    test "atom" do
      assert atom() |> to_quoted_string() == "atom()"
      assert atom([:a]) |> to_quoted_string() == ":a"
      assert atom([:a, :b]) |> to_quoted_string() == ":a or :b"
      assert opt_difference(atom(), atom([:a])) |> to_quoted_string() == "atom() and not :a"

      assert atom([Elixir]) |> to_quoted_string() == "Elixir"
      assert atom([Foo.Bar]) |> to_quoted_string() == "Foo.Bar"
    end

    test "boolean" do
      assert boolean() |> to_quoted_string() == "boolean()"
      assert atom([true, false, :a]) |> to_quoted_string() == ":a or boolean()"
      assert atom([true, :a]) |> to_quoted_string() == ":a or true"
      assert opt_difference(atom(), boolean()) |> to_quoted_string() == "atom() and not boolean()"
    end

    test "atom/boolean (negation)" do
      assert atom() |> opt_negation() |> to_quoted_string() == "not atom()"
      assert atom([:a, :b]) |> opt_negation() |> to_quoted_string() == "not :a and not :b"
      assert boolean() |> opt_negation() |> to_quoted_string() == "not boolean()"

      assert atom([true, false, :a]) |> opt_negation() |> to_quoted_string() ==
               "not :a and not boolean()"
    end

    test "dynamic" do
      assert dynamic() |> to_quoted_string() == "dynamic()"

      assert opt_intersection(atom(), dynamic()) |> to_quoted_string() == "dynamic(atom())"

      assert dynamic(opt_union(atom(), integer())) |> opt_union(integer()) |> to_quoted_string() ==
               "dynamic(atom()) or integer()"

      assert opt_intersection(binary(), dynamic()) |> to_quoted_string() ==
               "binary()"

      assert opt_intersection(bitstring(), dynamic()) |> to_quoted_string() ==
               "dynamic(bitstring())"

      assert opt_intersection(bitstring_no_binary(), dynamic()) |> to_quoted_string() ==
               "bitstring() and not binary()"

      assert opt_intersection(opt_union(binary(), pid()), dynamic()) |> to_quoted_string() ==
               "dynamic(binary() or pid())"

      assert opt_union(atom([:foo, :bar]), dynamic()) |> to_quoted_string() ==
               "dynamic() or :bar or :foo"

      assert opt_intersection(dynamic(), closed_map(a: integer())) |> to_quoted_string() ==
               "dynamic(%{a: integer()})"
    end

    test "dynamic (negation)" do
      assert dynamic(opt_negation(integer())) |> to_quoted_string() == "dynamic(not integer())"

      assert opt_negation(dynamic(integer())) |> to_quoted_string() ==
               "dynamic() or not integer()"

      assert opt_union(atom(), dynamic(integer())) |> opt_negation() |> to_quoted_string() ==
               "dynamic(not atom()) or (not atom() and not integer())"

      assert dynamic(opt_union(atom(), integer()))
             |> opt_negation()
             |> opt_union(integer())
             |> to_quoted_string() ==
               "dynamic() or not atom()"
    end

    test "list" do
      assert non_empty_list(none()) |> to_quoted_string() == "none()"
      assert list(term()) |> to_quoted_string() == "list(term())"
      assert list(integer()) |> to_quoted_string() == "list(integer())"

      assert list(term()) |> opt_difference(empty_list()) |> to_quoted_string() ==
               "non_empty_list(term())"

      assert list(term()) |> opt_difference(list(integer())) |> to_quoted_string() ==
               "non_empty_list(term()) and not non_empty_list(integer())"

      assert list(term())
             |> opt_difference(list(integer()))
             |> opt_difference(list(atom()))
             |> to_quoted_string() ==
               "non_empty_list(term()) and not (non_empty_list(integer()) or non_empty_list(atom()))"

      assert list(term(), integer()) |> to_quoted_string() ==
               "empty_list() or non_empty_list(term(), integer())"

      assert opt_difference(list(term(), atom()), list(term(), boolean())) |> to_quoted_string() ==
               "non_empty_list(term(), atom() and not boolean())"

      assert list(term(), term()) |> to_quoted_string() ==
               "empty_list() or non_empty_list(term(), term())"

      # Test normalization

      # Remove duplicates
      assert opt_union(list(integer()), list(integer())) |> to_quoted_string() ==
               "list(integer())"

      # Merge subtypes
      assert opt_union(list(float(), pid()), list(number(), pid())) |> to_quoted_string() ==
               "empty_list() or non_empty_list(float() or integer(), pid())"

      # Merge last element types
      assert opt_union(list(atom([:ok]), integer()), list(atom([:ok]), float()))
             |> to_quoted_string() ==
               "empty_list() or non_empty_list(:ok, float() or integer())"

      assert opt_union(dynamic(list(integer(), float())), dynamic(list(integer(), pid())))
             |> to_quoted_string() ==
               "dynamic(empty_list() or non_empty_list(integer(), float() or pid()))"

      list_with_tail =
        non_empty_list(atom(), opt_union(integer(), empty_list()))
        |> opt_difference(non_empty_list(atom([:ok]), integer()))
        |> opt_difference(non_empty_list(atom(), integer()))

      # Check that simplifications occur.
      assert to_quoted_string(list_with_tail) == "non_empty_list(atom())"
    end

    test "list (negation)" do
      assert list(term()) |> opt_negation() |> to_quoted_string() == "not list(term())"
      assert list(opt_negation(integer())) |> to_quoted_string() == "list(not integer())"

      assert list(term()) |> opt_difference(empty_list()) |> opt_negation() |> to_quoted_string() ==
               "not non_empty_list(term())"

      assert non_empty_list(integer(), integer()) |> opt_negation() |> to_quoted_string() ==
               "not non_empty_list(integer(), integer())"
    end

    test "tuple" do
      assert tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom()}"

      assert tuple([integer(), dynamic(atom())]) |> to_quoted_string() ==
               "dynamic({integer(), atom()})"

      assert open_tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom(), ...}"

      assert opt_union(tuple([integer(), atom()]), open_tuple([atom()])) |> to_quoted_string() ==
               "{atom(), ...} or {integer(), atom()}"

      assert opt_difference(tuple([integer(), atom()]), open_tuple([atom()]))
             |> to_quoted_string() ==
               "{integer(), atom()}"

      assert tuple([closed_map(a: integer()), open_map()]) |> to_quoted_string() ==
               "{%{a: integer()}, map()}"

      assert opt_union(tuple([integer(), atom()]), tuple([integer(), atom()]))
             |> to_quoted_string() ==
               "{integer(), atom()}"

      assert opt_union(tuple([integer(), atom()]), tuple([float(), atom()])) |> to_quoted_string() ==
               "{float() or integer(), atom()}"

      assert opt_union(tuple([integer(), atom()]), tuple([float(), atom()]))
             |> opt_union(tuple([pid(), pid(), port()]))
             |> opt_union(tuple([pid(), pid(), atom()]))
             |> to_quoted_string() ==
               "{float() or integer(), atom()} or {pid(), pid(), atom() or port()}"

      assert opt_union(open_tuple([integer()]), open_tuple([float()])) |> to_quoted_string() ==
               "{float() or integer(), ...}"

      # {:ok, {term(), integer()}} or {:ok, {term(), float()}} or {:exit, :kill} or {:exit, :timeout}
      assert tuple([atom([:ok]), tuple([term(), empty_list()])])
             |> opt_union(tuple([atom([:ok]), tuple([term(), open_map()])]))
             |> opt_union(tuple([atom([:exit]), atom([:kill])]))
             |> opt_union(tuple([atom([:exit]), atom([:timeout])]))
             |> to_quoted_string() ==
               "{:exit, :kill or :timeout} or {:ok, {term(), empty_list() or map()}}"

      # Detection of duplicates
      assert tuple([atom([:ok]), term()])
             |> opt_union(tuple([atom([:ok]), term()]))
             |> to_quoted_string() == "{:ok, term()}"

      assert tuple([closed_map(a: integer(), b: atom()), open_map()])
             |> opt_union(tuple([closed_map(a: integer(), b: atom()), open_map()]))
             |> to_quoted_string() ==
               "{%{a: integer(), b: atom()}, map()}"

      # Nested fusion
      assert tuple([closed_map(a: integer(), b: atom()), open_map()])
             |> opt_union(tuple([closed_map(a: float(), b: atom()), open_map()]))
             |> to_quoted_string() ==
               "{%{a: float() or integer(), b: atom()}, map()}"

      # Complex simplification of map/tuple combinations. Initial type is:
      # ```
      #   dynamic(
      #     :error or
      #     ({%Decimal{coef: :inf, exp: integer(), sign: integer()}, binary()} or
      #       {%Decimal{coef: :NaN, exp: integer(), sign: integer()}, binary()} or
      #       {%Decimal{coef: integer(), exp: integer(), sign: integer()}, term()} or
      #       {%Decimal{coef: :inf, exp: integer(), sign: integer()} or
      #           %Decimal{coef: :NaN, exp: integer(), sign: integer()} or
      #           %Decimal{coef: integer(), exp: integer(), sign: integer()}, term()})
      #   )
      # ```
      decimal_inf =
        closed_map(
          __struct__: atom([Decimal]),
          coef: atom([:inf]),
          exp: integer(),
          sign: integer()
        )

      decimal_nan =
        closed_map(
          __struct__: atom([Decimal]),
          coef: atom([:NaN]),
          exp: integer(),
          sign: integer()
        )

      decimal_int =
        closed_map(
          __struct__: atom([Decimal]),
          coef: integer(),
          exp: integer(),
          sign: integer()
        )

      assert atom([:error])
             |> opt_union(
               tuple([decimal_inf, binary()])
               |> opt_union(
                 tuple([decimal_nan, binary()])
                 |> opt_union(
                   tuple([decimal_int, term()])
                   |> opt_union(
                     tuple([opt_union(decimal_inf, opt_union(decimal_nan, decimal_int)), term()])
                   )
                 )
               )
             )
             |> dynamic()
             |> to_quoted_string() ==
               """
               dynamic(
                 :error or {%Decimal{sign: integer(), coef: :NaN or :inf or integer(), exp: integer()}, term()}
               )\
               """
    end

    test "tuple (negation)" do
      assert tuple([integer()]) |> opt_negation() |> to_quoted_string() == "not {integer()}"
      assert tuple([opt_negation(integer())]) |> to_quoted_string() == "{not integer()}"
    end

    test "fun" do
      assert fun() |> to_quoted_string() == "fun()"
      assert none_fun(1) |> to_quoted_string() == "(none() -> term())"

      assert none_fun(1)
             |> opt_intersection(none_fun(2))
             |> to_quoted_string() == "none()"

      assert fun([integer(), float()], boolean()) |> to_quoted_string() ==
               "(integer(), float() -> boolean())"

      assert fun([integer()], boolean())
             |> opt_union(fun([float()], boolean()))
             |> to_quoted_string() ==
               "(float() -> boolean()) or (integer() -> boolean())"

      assert fun([integer()], boolean())
             |> opt_intersection(fun([float()], boolean()))
             |> to_quoted_string() ==
               "(float() -> boolean()) and (integer() -> boolean())"

      # Thanks to lazy BDDs, consecutive union of functions come out as the original union
      assert fun([integer()], integer())
             |> opt_union(fun([float()], float()))
             |> opt_union(fun([pid()], pid()))
             |> to_quoted_string() ==
               "(integer() -> integer()) or (pid() -> pid()) or (float() -> float())"

      assert fun(3) |> to_quoted_string() == "(none(), none(), none() -> term())"

      assert opt_intersection(fun(), opt_negation(fun())) |> to_quoted_string() == "none()"

      assert opt_intersection(fun(), opt_negation(fun(3))) |> to_quoted_string() ==
               "fun() and not (none(), none(), none() -> term())"
    end

    test "fun with optimized intersections" do
      assert fun([integer()], atom()) |> opt_intersection(none_fun(1)) |> to_quoted_string() ==
               "(integer() -> atom())"

      assert fun([integer()], atom())
             |> opt_difference(none_fun(2))
             |> opt_intersection(none_fun(1))
             |> to_quoted_string() ==
               "(integer() -> atom())"
    end

    test "fun with dynamic signatures" do
      assert fun([dynamic(integer())], float()) |> to_quoted_string() ==
               "(dynamic(integer()) -> float())"

      assert fun([dynamic(atom())], float()) |> to_quoted_string() ==
               "(dynamic(atom()) -> float())"

      assert fun([integer(), float()], dynamic(atom())) |> to_quoted_string() ==
               "(integer(), float() -> dynamic(atom()))"

      domain_part = fun([dynamic(atom()) |> opt_union(integer()), binary()], float())

      assert domain_part |> to_quoted_string() ==
               "(dynamic(atom()) or integer(), binary() -> float())"

      codomain_part = fun([pid(), float()], dynamic(atom()) |> opt_union(integer()))

      assert codomain_part |> to_quoted_string() ==
               "(pid(), float() -> dynamic(atom()) or integer())"

      assert opt_union(domain_part, codomain_part) |> to_quoted_string() ==
               """
               (pid(), float() -> dynamic(atom()) or integer()) or
                 (dynamic(atom()) or integer(), binary() -> float())\
               """

      assert opt_intersection(domain_part, codomain_part) |> to_quoted_string() ==
               """
               (pid(), float() -> dynamic(atom()) or integer()) and
                 (dynamic(atom()) or integer(), binary() -> float())\
               """
    end

    test "fun (negation)" do
      assert fun([integer()], atom()) |> opt_negation() |> to_quoted_string() ==
               "not (integer() -> atom())"
    end

    test "map as records" do
      assert empty_map() |> to_quoted_string() == "empty_map()"
      assert open_map() |> to_quoted_string() == "map()"

      assert closed_map(a: integer()) |> to_quoted_string() == "%{a: integer()}"
      assert open_map(a: float()) |> to_quoted_string() == "%{..., a: float()}"

      assert closed_map("Elixir.Foo.Bar": integer()) |> to_quoted_string() ==
               "%{Foo.Bar => integer()}"

      assert open_map("Elixir.Foo.Bar": float()) |> to_quoted_string() ==
               "%{..., Foo.Bar => float()}"

      assert opt_difference(open_map(), open_map(a: term())) |> to_quoted_string() ==
               "%{..., a: not_set()}"

      assert closed_map(a: integer(), b: atom()) |> to_quoted_string() ==
               "%{a: integer(), b: atom()}"

      assert open_map(a: float())
             |> opt_difference(closed_map(a: float()))
             |> to_quoted_string() == "%{..., a: float()} and not %{a: float()}"

      assert opt_difference(open_map(), empty_map()) |> to_quoted_string() ==
               "map() and not empty_map()"

      assert closed_map(foo: opt_union(integer(), not_set())) |> to_quoted_string() ==
               "%{foo: if_set(integer())}"

      # Test normalization
      assert open_map(a: integer(), b: atom())
             |> opt_difference(open_map(b: atom()))
             |> opt_union(open_map(a: integer()))
             |> to_quoted_string() == "%{..., a: integer()}"

      assert opt_union(open_map(a: integer()), open_map(a: integer())) |> to_quoted_string() ==
               "%{..., a: integer()}"

      assert opt_difference(open_map(a: number(), b: atom()), open_map(a: integer()))
             |> to_quoted_string() == "%{..., a: float(), b: atom()}"

      # Basic map fusion
      assert opt_union(closed_map(a: integer()), closed_map(a: integer())) |> to_quoted_string() ==
               "%{a: integer()}"

      assert opt_union(closed_map(a: integer()), closed_map(a: float())) |> to_quoted_string() ==
               "%{a: float() or integer()}"

      # Nested fusion
      assert opt_union(closed_map(a: integer(), b: atom()), closed_map(a: float(), b: atom()))
             |> opt_union(closed_map(x: pid(), y: pid(), z: port()))
             |> opt_union(closed_map(x: pid(), y: pid(), z: atom()))
             |> to_quoted_string() ==
               "%{a: float() or integer(), b: atom()} or %{x: pid(), y: pid(), z: atom() or port()}"

      # Open map fusion
      assert opt_union(open_map(a: integer()), open_map(a: float())) |> to_quoted_string() ==
               "%{..., a: float() or integer()}"

      # Fusing complex nested maps with unions
      assert closed_map(
               status: atom([:ok]),
               data: closed_map(value: term(), count: empty_list())
             )
             |> opt_union(
               closed_map(
                 status: atom([:ok]),
                 data: closed_map(value: term(), count: open_map())
               )
             )
             |> opt_union(closed_map(status: atom([:error]), reason: atom([:timeout])))
             |> opt_union(closed_map(status: atom([:error]), reason: atom([:crash])))
             |> to_quoted_string() ==
               "%{data: %{count: empty_list() or map(), value: term()}, status: :ok} or\n  %{reason: :crash or :timeout, status: :error}"

      # Difference and union tests
      assert closed_map(status: atom([:ok]), value: term())
             |> opt_difference(closed_map(status: atom([:ok]), value: float()))
             |> opt_union(
               closed_map(status: atom([:ok]), value: term())
               |> opt_difference(closed_map(status: atom([:ok]), value: integer()))
             )
             |> to_quoted_string() ==
               "%{status: :ok, value: term()}"

      # Nested map fusion
      assert closed_map(data: closed_map(x: integer(), y: atom()), meta: open_map())
             |> opt_union(closed_map(data: closed_map(x: float(), y: atom()), meta: open_map()))
             |> to_quoted_string() ==
               "%{data: %{x: float() or integer(), y: atom()}, meta: map()}"

      # Test complex combinations
      assert opt_intersection(
               open_map(a: number(), b: atom()),
               open_map(a: integer(), c: boolean())
             )
             |> opt_union(opt_difference(open_map(x: atom()), open_map(x: boolean())))
             |> to_quoted_string() ==
               "%{..., a: integer(), b: atom(), c: boolean()} or %{..., x: atom() and not boolean()}"

      assert closed_map(a: number(), b: atom(), c: pid())
             |> opt_difference(closed_map(a: integer(), b: atom(), c: pid()))
             |> to_quoted_string() == "%{a: float(), b: atom(), c: pid()}"

      # No simplification compared to above, as it is an open map
      assert open_map(a: number(), b: atom())
             |> opt_difference(closed_map(a: integer(), b: atom()))
             |> to_quoted_string() ==
               "%{..., a: float() or integer(), b: atom()} and not %{a: integer(), b: atom()}"

      # Remark: this simplification is order dependent. Having the first difference
      # after the second gives a different result.
      assert open_map(a: number(), b: atom(), c: opt_union(pid(), port()))
             |> opt_difference(open_map(a: integer(), b: atom(), c: opt_union(pid(), port())))
             |> opt_difference(open_map(a: float(), b: atom(), c: pid()))
             |> to_quoted_string() == "%{..., a: float(), b: atom(), c: port()}"

      assert open_map(a: number(), b: atom(), c: opt_union(pid(), port()))
             |> opt_difference(open_map(a: float(), b: atom(), c: pid()))
             |> opt_difference(open_map(a: integer(), b: atom(), c: opt_union(pid(), port())))
             |> to_quoted_string() == "%{..., a: float(), b: atom(), c: port()}"
    end

    test "map as dictionaries" do
      assert closed_map([{domain_key(:integer), integer()}])
             |> to_quoted_string() == "%{integer() => integer()}"

      assert closed_map([{domain_key(:integer), not_set()}, {:float, float()}])
             |> to_quoted_string() == "%{integer() => not_set(), float: float()}"
    end

    test "map (negation)" do
      assert open_map(a: integer()) |> opt_negation() |> to_quoted_string() ==
               "not (map() and not %{..., a: if_set(not integer())})"

      assert open_map(a: opt_negation(integer())) |> to_quoted_string() ==
               "%{..., a: not integer()}"

      assert closed_map(a: integer()) |> opt_negation() |> to_quoted_string() ==
               "not %{a: integer()}"

      assert closed_map(a: opt_negation(integer())) |> to_quoted_string() ==
               "%{a: not integer()}"
    end

    test "structs" do
      assert open_map(__struct__: term()) |> to_quoted_string() ==
               "%{..., __struct__: term()}"

      assert open_map(__struct__: atom([URI])) |> to_quoted_string() ==
               "%{..., __struct__: URI}"

      assert closed_map(__struct__: atom([URI])) |> to_quoted_string() ==
               "%{__struct__: URI}"

      assert closed_map(__struct__: atom([NoFieldsStruct])) |> to_quoted_string() ==
               "%NoFieldsStruct{}"

      assert closed_map(__struct__: atom([URI, Another])) |> to_quoted_string() ==
               "%{__struct__: Another or URI}"

      assert closed_map(__struct__: atom([Decimal]), coef: term(), exp: term(), sign: term())
             |> to_quoted_string(collapse_structs: false) ==
               "%Decimal{sign: term(), coef: term(), exp: term()}"

      assert closed_map(__struct__: atom([Decimal]), coef: term(), exp: term(), sign: term())
             |> to_quoted_string() ==
               "%Decimal{}"

      assert closed_map(__struct__: atom([Decimal]), coef: term(), exp: term(), sign: integer())
             |> to_quoted_string() ==
               "%Decimal{sign: integer()}"

      # Does not fuse structs
      assert opt_union(closed_map(__struct__: atom([Foo])), closed_map(__struct__: atom([Bar])))
             |> to_quoted_string() ==
               "%{__struct__: Bar} or %{__struct__: Foo}"

      # Properly format non_struct_map
      assert open_map(__struct__: if_set(opt_negation(atom()))) |> to_quoted_string() ==
               "non_struct_map()"
    end
  end

  describe "performance" do
    test "tuple difference" do
      # Large difference with no duplicates
      descr1 =
        opt_union(
          atom([:ignored, :reset]),
          tuple([atom([:font_style]), atom([:italic])])
        )

      descr2 =
        opt_union(
          atom([:ignored, :reset]),
          opt_union(
            tuple([atom([:font_style]), atom([:italic])]),
            Enum.reduce(
              for elem1 <- 1..5, elem2 <- 1..5 do
                tuple([atom([:"f#{elem1}"]), atom([:"s#{elem2}"])])
              end,
              &opt_union/2
            )
          )
        )

      assert subtype?(descr1, descr2)
      refute subtype?(descr2, descr1)
    end

    test "map difference" do
      # Create a large map with various types
      map1 =
        open_map([
          {:id, integer()},
          {:name, binary()},
          {:age, opt_union(integer(), atom())},
          {:email, binary()},
          {:active, boolean()},
          {:tags, list(atom())}
        ])

      # Create another large map with some differences and many more entries
      map2 =
        open_map(
          [
            {:id, integer()},
            {:name, binary()},
            {:age, integer()},
            {:email, binary()},
            {:active, boolean()},
            {:tags, non_empty_list(atom())},
            {:meta,
             open_map([
               {:created_at, binary()},
               {:updated_at, binary()},
               {:status, atom()}
             ])},
            {:permissions, tuple([atom(), integer(), atom()])},
            {:profile,
             open_map([
               {:bio, binary()},
               {:interests, non_empty_list(binary())},
               {:social_media,
                open_map([
                  {:twitter, binary()},
                  {:instagram, binary()},
                  {:linkedin, binary()}
                ])}
             ])},
            {:notifications, boolean()}
          ] ++
            Enum.map(1..50, fn i ->
              {:"field_#{i}", atom([:"value_#{i}"])}
            end)
        )

      refute subtype?(map1, map2)
      assert subtype?(map2, map1)
    end

    test "map intersection and then difference" do
      actual = open_map(__struct__: atom(), __exception__: atom([true]))

      expected =
        for i <- 1..50 do
          name = :"name_#{i}"
          closed_map([__struct__: atom([name])] ++ [{name, binary()}])
        end
        |> Enum.reduce(&opt_union/2)

      common = opt_intersection(actual, expected)
      opt_difference(actual, common)
    end

    test "struct difference" do
      entries =
        [
          closed_map(__struct__: atom([MapSet]), map: term()),
          closed_map(__struct__: atom([Jason.OrderedObject]), values: term()),
          closed_map(__struct__: atom([GenEvent.Stream]), timeout: term(), manager: term()),
          closed_map(__struct__: atom([HashDict]), size: term(), root: term()),
          closed_map(__struct__: atom([HashSet]), size: term(), root: term()),
          closed_map(
            __struct__: atom([IO.Stream]),
            raw: term(),
            device: term(),
            line_or_bytes: term()
          ),
          closed_map(__struct__: atom([Range]), first: term(), last: term(), step: term()),
          closed_map(
            __struct__: atom([Stream]),
            enum: term(),
            done: term(),
            funs: term(),
            accs: term()
          ),
          closed_map(
            __struct__: atom([Req.Response.Async]),
            pid: term(),
            ref: term(),
            stream_fun: term(),
            cancel_fun: term()
          ),
          closed_map(
            __struct__: atom([Postgrex.Stream]),
            options: term(),
            params: term(),
            query: term(),
            conn: term()
          ),
          closed_map(
            __struct__: atom([DBConnection.PrepareStream]),
            opts: term(),
            params: term(),
            query: term(),
            conn: term()
          ),
          closed_map(
            __struct__: atom([DBConnection.Stream]),
            opts: term(),
            params: term(),
            query: term(),
            conn: term()
          ),
          closed_map(
            __struct__: atom([Ecto.Adapters.SQL.Stream]),
            meta: term(),
            opts: term(),
            params: term(),
            statement: term()
          ),
          closed_map(
            __struct__: atom([Date.Range]),
            first: term(),
            last: term(),
            step: term(),
            first_in_iso_days: term(),
            last_in_iso_days: term()
          ),
          closed_map(
            __struct__: atom([File.Stream]),
            node: term(),
            raw: term(),
            path: term(),
            modes: term(),
            line_or_bytes: term()
          ),
          closed_map(
            __struct__: atom([Phoenix.LiveView.LiveStream]),
            name: term(),
            ref: term(),
            inserts: term(),
            deletes: term(),
            reset?: term(),
            dom_id: term(),
            consumable?: term()
          )
        ]

      range =
        closed_map(__struct__: atom([Range]), first: integer(), last: integer(), step: integer())

      assert subtype?(range, Enum.reduce(entries, &opt_union/2))
    end
  end
end
