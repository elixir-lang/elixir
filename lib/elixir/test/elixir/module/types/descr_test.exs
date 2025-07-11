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

  import Module.Types.Descr, except: [fun: 1]

  defmacrop domain_key(key), do: {:domain_key, key}

  defp number(), do: union(integer(), float())
  defp empty_tuple(), do: tuple([])
  defp tuple_of_size_at_least(n) when is_integer(n), do: open_tuple(List.duplicate(term(), n))
  defp tuple_of_size(n) when is_integer(n) and n >= 0, do: tuple(List.duplicate(term(), n))
  defp list(elem_type, tail_type), do: union(empty_list(), non_empty_list(elem_type, tail_type))
  defp map_with_default(descr), do: open_map([], if_set(descr))
  defp nil_or_type(type), do: union(type, atom([nil]))

  describe "union" do
    test "bitmap" do
      assert union(integer(), float()) == union(float(), integer())
    end

    test "term" do
      assert union(term(), float()) == term()
      assert union(term(), binary()) == term()
      assert union(term(), if_set(binary())) == if_set(term())
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
        open_map(),
        non_empty_list(term(), term()),
        empty_list(),
        tuple(),
        fun(),
        pid(),
        port(),
        reference()
      ]

      assert Enum.reduce(all, &union/2) |> equal?(term())
    end

    test "dynamic" do
      assert equal?(union(dynamic(), dynamic()), dynamic())
      assert equal?(union(dynamic(), term()), term())
      assert equal?(union(term(), dynamic()), term())

      assert equal?(union(dynamic(atom()), atom()), atom())
      refute equal?(union(dynamic(atom()), atom()), dynamic(atom()))

      assert equal?(union(term(), dynamic(if_set(integer()))), union(term(), dynamic(not_set())))
      refute equal?(union(term(), dynamic(if_set(integer()))), dynamic(union(term(), not_set())))
    end

    test "tuple" do
      assert equal?(union(tuple(), tuple()), tuple())

      t = tuple([integer(), atom()])
      assert equal?(union(t, t), t)

      assert union(tuple([integer(), atom()]), tuple([float(), atom()]))
             |> equal?(tuple([union(integer(), float()), atom()]))

      assert union(tuple([integer(), atom()]), tuple([integer(), binary()]))
             |> equal?(tuple([integer(), union(atom(), binary())]))

      assert open_tuple([atom()])
             |> union(tuple([atom(), integer()]))
             |> equal?(open_tuple([atom()]))

      assert tuple([union(integer(), atom())])
             |> difference(open_tuple([atom()]))
             |> equal?(tuple([integer()]))
    end

    test "map" do
      assert equal?(union(open_map(), open_map()), open_map())
      assert equal?(union(closed_map(a: integer()), open_map()), open_map())
      assert equal?(union(closed_map(a: integer()), negation(closed_map(a: integer()))), term())

      a_integer_open = open_map(a: integer())
      assert equal?(union(closed_map(a: integer()), a_integer_open), a_integer_open)

      # Domain key types
      atom_to_atom = open_map([{domain_key(:atom), atom()}])
      atom_to_integer = open_map([{domain_key(:atom), integer()}])

      # Test union identity and different type maps
      assert union(atom_to_atom, atom_to_atom) == atom_to_atom

      # Test subtype relationships with domain key maps
      refute open_map([{domain_key(:atom), union(atom(), integer())}])
             |> subtype?(union(atom_to_atom, atom_to_integer))

      assert union(atom_to_atom, atom_to_integer)
             |> subtype?(open_map([{domain_key(:atom), union(atom(), integer())}]))

      # Test unions with empty and open maps
      assert union(empty_map(), open_map([{domain_key(:integer), atom()}]))
             |> equal?(open_map([{domain_key(:integer), atom()}]))

      assert union(open_map(), open_map([{domain_key(:integer), atom()}])) == open_map()

      # Test union of open map and map with domain key
      assert union(open_map(), open_map([{domain_key(:integer), atom()}])) == open_map()
    end

    test "list" do
      assert union(list(term()), list(term())) |> equal?(list(term()))
      assert union(list(integer()), list(term())) |> equal?(list(term()))

      assert union(difference(list(term()), list(integer())), list(integer()))
             |> equal?(list(term()))
    end

    test "fun" do
      assert equal?(union(fun(), fun()), fun())
      assert equal?(union(fun(), none_fun(1)), fun())

      dynamic_fun = intersection(fun(), dynamic())
      assert equal?(union(dynamic_fun, fun()), fun())
    end

    test "optimizations (maps)" do
      # The tests are checking the actual implementation, not the semantics.
      # This is why we are using structural comparisons.
      # It's fine to remove these if the implementation changes, but breaking
      # these might have an important impact on compile times.

      # Optimization one: same tags, all but one key are structurally equal
      assert union(
               open_map(a: float(), b: atom()),
               open_map(a: integer(), b: atom())
             ) == open_map(a: union(float(), integer()), b: atom())

      assert union(
               closed_map(a: float(), b: atom()),
               closed_map(a: integer(), b: atom())
             ) == closed_map(a: union(float(), integer()), b: atom())

      # Optimization two: we can tell that one map is a subtype of the other:

      assert union(
               closed_map(a: term(), b: term()),
               closed_map(a: float(), b: binary())
             ) == closed_map(a: term(), b: term())

      assert union(
               open_map(a: term()),
               closed_map(a: float(), b: binary())
             ) == open_map(a: term())

      assert union(
               closed_map(a: float(), b: binary()),
               open_map(a: term())
             ) == open_map(a: term())

      assert union(
               closed_map(a: term(), b: tuple([term(), term()])),
               closed_map(a: float(), b: tuple([atom(), binary()]))
             ) == closed_map(a: term(), b: tuple([term(), term()]))
    end

    test "optimizations (tuples)" do
      # Optimization one: same tags, all but one key are structurally equal
      assert union(
               open_tuple([float(), atom()]),
               open_tuple([integer(), atom()])
             ) == open_tuple([union(float(), integer()), atom()])

      assert union(
               tuple([float(), atom()]),
               tuple([integer(), atom()])
             ) == tuple([union(float(), integer()), atom()])

      # Optimization two: we can tell that one tuple is a subtype of the other:

      assert union(
               tuple([term(), term()]),
               tuple([float(), binary()])
             ) == tuple([term(), term()])

      assert union(
               open_tuple([term()]),
               tuple([float(), binary()])
             ) == open_tuple([term()])

      assert union(
               tuple([float(), binary()]),
               open_tuple([term()])
             ) == open_tuple([term()])
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

      assert empty?(intersection(dynamic(not_set()), term()))
      refute empty?(intersection(dynamic(if_set(integer())), term()))

      # Check for structural equivalence
      assert intersection(dynamic(not_set()), term()) == none()
    end

    test "tuple" do
      assert empty?(intersection(open_tuple([atom()]), open_tuple([integer()])))

      assert intersection(open_tuple([atom()]), tuple([term(), integer()]))
             |> equal?(tuple([atom(), integer()]))

      assert intersection(tuple([term(), integer()]), tuple([atom(), term()]))
             |> equal?(tuple([atom(), integer()]))
    end

    test "map" do
      assert intersection(open_map(), open_map()) == open_map()
      assert equal?(intersection(closed_map(a: integer()), open_map()), closed_map(a: integer()))

      assert equal?(
               intersection(closed_map(a: integer()), open_map(a: integer())),
               closed_map(a: integer())
             )

      assert intersection(closed_map(a: integer()), open_map(b: not_set())) ==
               closed_map(a: integer())

      assert intersection(closed_map(a: integer()), open_map(b: if_set(integer()))) ==
               closed_map(a: integer())

      assert equal?(
               intersection(closed_map(a: integer()), closed_map(a: if_set(integer()))),
               closed_map(a: integer())
             )

      assert empty?(intersection(closed_map(a: integer()), closed_map(a: atom())))
    end

    test "map with domain keys" do
      # %{..., int => t1, atom => t2} and %{int => t3}
      # intersection is %{int => t1 and t3, atom => none}
      map1 = open_map([{domain_key(:integer), integer()}, {domain_key(:atom), atom()}])
      map2 = closed_map([{domain_key(:integer), number()}])

      intersection = intersection(map1, map2)

      expected =
        closed_map([{domain_key(:integer), integer()}, {domain_key(:atom), none()}])

      assert equal?(intersection, expected)

      # %{..., int => t1, atom => t2} and %{int => t3, pid => t4}
      # intersection is %{int =>t1 and t3, atom => none, pid => t4}
      map1 = open_map([{domain_key(:integer), integer()}, {domain_key(:atom), atom()}])
      map2 = closed_map([{domain_key(:integer), float()}, {domain_key(:pid), binary()}])

      intersection = intersection(map1, map2)

      expected =
        closed_map([
          {domain_key(:integer), intersection(integer(), float())},
          {domain_key(:atom), none()},
          {domain_key(:pid), binary()}
        ])

      assert equal?(intersection, expected)

      # %{..., int => t1, string => t3} and %{int => t4}
      # intersection is %{int => t1 and t4, string => none}
      map1 = open_map([{domain_key(:integer), integer()}, {domain_key(:binary), binary()}])
      map2 = closed_map([{domain_key(:integer), float()}])

      intersection = intersection(map1, map2)

      assert equal?(
               intersection,
               closed_map([
                 {domain_key(:integer), intersection(integer(), float())},
                 {domain_key(:binary), none()}
               ])
             )

      assert subtype?(empty_map(), closed_map([{domain_key(:integer), atom()}]))

      t1 = closed_map([{domain_key(:integer), atom()}])
      t2 = closed_map([{domain_key(:integer), binary()}])

      assert equal?(intersection(t1, t2), empty_map())

      t1 = closed_map([{domain_key(:integer), atom()}])
      t2 = closed_map([{domain_key(:atom), term()}])

      # their intersection is the empty map
      refute empty?(intersection(t1, t2))
      assert equal?(intersection(t1, t2), empty_map())
    end

    test "list" do
      assert intersection(list(term()), list(term())) == list(term())
      assert intersection(list(integer()), list(integer())) == list(integer())
      assert intersection(list(integer()), list(number())) == list(integer())
      assert intersection(list(integer()), list(atom())) == empty_list()

      # Empty list intersections
      assert intersection(empty_list(), list(term())) == empty_list()
      assert intersection(empty_list(), list(integer())) == empty_list()
      assert intersection(empty_list(), empty_list()) == empty_list()

      # List with any type
      assert intersection(list(term()), list(integer())) == list(integer())
      assert intersection(list(term()), list(integer())) == list(integer())

      # Intersection with more specific types
      assert intersection(list(integer()), list(atom([:a, :b]))) == empty_list()

      # Intersection with union types
      assert intersection(list(union(integer(), atom())), list(number())) == list(integer())

      # Intersection with dynamic
      assert equal?(
               intersection(dynamic(list(term())), list(integer())),
               dynamic(list(integer()))
             )

      assert equal?(intersection(dynamic(list(term())), list(term())), dynamic(list(term())))

      # Nested list intersections
      assert intersection(list(list(integer())), list(list(number()))) == list(list(integer()))
      assert intersection(list(list(integer())), list(list(atom()))) == list(empty_list())

      # Intersection with non-list types
      assert intersection(list(integer()), integer()) == none()

      # Tests for list with last element
      assert intersection(list(float(), atom()), list(number(), term())) == list(float(), atom())

      assert intersection(list(number(), atom()), list(float(), boolean())) ==
               list(float(), boolean())

      assert intersection(list(integer(), float()), list(number(), integer())) == empty_list()

      # Empty list with last element
      assert intersection(empty_list(), list(integer(), atom())) == empty_list()
      assert intersection(list(integer(), atom()), empty_list()) == empty_list()

      # List with any type and specific last element
      assert intersection(list(term(), atom()), list(float(), boolean())) ==
               list(float(), boolean())

      assert intersection(list(term(), term()), list(float(), atom())) == list(float(), atom())

      # Nested lists with last element
      assert intersection(list(list(integer()), atom()), list(list(number()), boolean())) ==
               list(list(integer()), boolean())

      assert list(list(integer(), atom()), float())
             |> intersection(list(list(number(), boolean()), integer())) == empty_list()

      # Union types in last element
      assert intersection(list(integer(), union(atom(), binary())), list(number(), atom())) ==
               list(integer(), atom())

      # Dynamic with last element
      assert intersection(dynamic(list(term(), atom())), list(integer(), boolean()))
             |> equal?(dynamic(list(integer(), boolean())))

      # Intersection with proper list (should result in empty list)
      assert intersection(list(integer(), atom()), list(integer())) == empty_list()
    end

    test "function" do
      assert not empty?(intersection(negation(none_fun(2)), negation(none_fun(3))))
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
      assert empty?(difference(dynamic(integer()), integer()))
    end

    test "tuple" do
      assert empty?(difference(open_tuple([atom()]), open_tuple([term()])))
      refute empty?(difference(tuple(), empty_tuple()))
      refute tuple_of_size_at_least(2) |> difference(tuple_of_size(2)) |> empty?()
      assert tuple_of_size_at_least(2) |> difference(tuple_of_size_at_least(1)) |> empty?()
      assert tuple_of_size_at_least(3) |> difference(tuple_of_size_at_least(3)) |> empty?()
      refute tuple_of_size_at_least(2) |> difference(tuple_of_size_at_least(3)) |> empty?()
      refute tuple([term(), term()]) |> difference(tuple([atom(), term()])) |> empty?()
      refute tuple([term(), term()]) |> difference(tuple([atom()])) |> empty?()
      assert tuple([term(), term()]) |> difference(tuple([term(), term()])) |> empty?()

      # {term(), term(), ...} and not ({term(), term(), term(), ...} or {term(), term()})
      assert tuple_of_size_at_least(2)
             |> difference(tuple_of_size(2))
             |> difference(tuple_of_size_at_least(3))
             |> empty?()

      assert tuple([term(), term()])
             |> difference(tuple([atom()]))
             |> difference(open_tuple([term()]))
             |> difference(empty_tuple())
             |> empty?()

      refute difference(tuple(), empty_tuple())
             |> difference(open_tuple([term(), term()]))
             |> empty?()

      assert difference(open_tuple([term()]), open_tuple([term(), term()]))
             |> difference(tuple([term()]))
             |> empty?()

      assert open_tuple([atom()])
             |> difference(tuple([integer(), integer()]))
             |> equal?(open_tuple([atom()]))

      assert tuple([union(atom(), integer()), term()])
             |> difference(open_tuple([atom(), term()]))
             |> equal?(tuple([integer(), term()]))

      assert tuple([union(atom(), integer()), term()])
             |> difference(open_tuple([atom(), term()]))
             |> difference(open_tuple([integer(), term()]))
             |> empty?()

      assert tuple([term(), union(atom(), integer()), term()])
             |> difference(open_tuple([term(), integer()]))
             |> equal?(tuple([term(), atom(), term()]))

      assert difference(tuple(), open_tuple([term(), term()]))
             |> equal?(union(tuple([term()]), tuple([])))
    end

    test "map" do
      assert empty?(difference(open_map(), open_map()))
      assert empty?(difference(open_map(), term()))
      assert equal?(difference(open_map(), none()), open_map())
      assert empty?(difference(closed_map(a: integer()), open_map()))
      assert empty?(difference(closed_map(a: integer()), closed_map(a: integer())))
      assert empty?(difference(closed_map(a: integer()), open_map(a: integer())))
      assert empty?(difference(closed_map(a: integer()), open_map(b: if_set(integer()))))

      assert difference(closed_map(a: integer(), b: if_set(atom())), closed_map(a: integer()))
             |> difference(closed_map(a: integer(), b: atom()))
             |> empty?()

      assert difference(open_map(a: atom()), closed_map(b: integer()))
             |> equal?(open_map(a: atom()))

      refute empty?(difference(open_map(), empty_map()))

      assert difference(open_map(a: integer()), closed_map(b: boolean()))
             |> equal?(open_map(a: integer()))
    end

    test "map with domain keys" do
      # Non-overlapping domain keys
      t1 = closed_map([{domain_key(:integer), atom()}])
      t2 = closed_map([{domain_key(:atom), binary()}])
      assert equal?(difference(t1, t2) |> union(empty_map()), t1)
      assert empty?(difference(t1, t1))

      # %{atom() => t1} and not %{atom() => t2} is not %{atom() => t1 and not t2}
      t3 = closed_map([{domain_key(:integer), atom()}])
      t4 = closed_map([{domain_key(:integer), atom([:ok])}])
      assert subtype?(difference(t3, t4), t3)

      refute difference(t3, t4)
             |> equal?(closed_map([{domain_key(:integer), difference(atom(), atom([:ok]))}]))

      # Difference with a non-domain key map
      t5 = closed_map([{domain_key(:integer), union(atom(), integer())}])
      t6 = closed_map(a: atom())
      assert equal?(difference(t5, t6), t5)

      # Removing atom keys from a map with defined atom keys
      a_number = closed_map(a: number())
      a_number_and_pids = closed_map([{:a, number()}, {domain_key(:atom), pid()}])
      atom_to_float = closed_map([{domain_key(:atom), float()}])
      atom_to_term = closed_map([{domain_key(:atom), term()}])
      atom_to_pid = closed_map([{domain_key(:atom), pid()}])
      t_diff = difference(a_number, atom_to_float)

      # Removing atom keys that map to float, make the :a key point to integer only.
      assert map_fetch(t_diff, :a) == {false, integer()}
      # %{a => number, atom => pid} and not %{atom => float} gives numbers on :a
      assert map_fetch(difference(a_number_and_pids, atom_to_float), :a) == {false, number()}

      assert map_fetch(t_diff, :foo) == :badkey

      assert subtype?(a_number, atom_to_term)
      refute subtype?(a_number, atom_to_float)

      # Removing all atom keys from map %{:a => type} means there is nothing left.
      assert empty?(difference(a_number, atom_to_term))
      refute empty?(intersection(atom_to_term, a_number))
      assert empty?(intersection(atom_to_pid, a_number))

      # (%{:a => number} and not %{:a => float}) is %{:a => integer}
      assert equal?(difference(a_number, atom_to_float), closed_map(a: integer()))
    end

    test "list" do
      # Basic list type differences
      assert difference(list(term()), empty_list()) == non_empty_list(term())
      assert difference(list(integer()), list(term())) == none()

      assert difference(list(integer()), list(float()))
             |> equal?(non_empty_list(integer()))

      # All list of integers and floats, minus all lists of integers, is NOT all lists of floats
      refute difference(list(union(integer(), float())), list(integer()))
             |> equal?(non_empty_list(float()))

      # Interactions with empty_list()
      assert difference(empty_list(), list(term())) == none()
      assert difference(list(integer()), empty_list()) == non_empty_list(integer())

      # Nested list structures
      assert difference(list(list(integer())), list(list(float())))
             |> equal?(difference(list(list(integer())), list(empty_list())))

      # Lists with union types
      refute difference(list(union(integer(), float())), list(integer())) == list(float())
      refute difference(list(union(atom(), binary())), list(atom())) == list(binary())

      # Tests for list with last element
      assert difference(list(integer(), atom()), list(number(), term())) == none()

      assert difference(
               list(atom(), term()),
               difference(list(atom(), term()), list(atom()))
             )
             |> equal?(list(atom()))

      assert difference(list(integer(), float()), list(number(), integer()))
             |> equal?(non_empty_list(integer(), difference(float(), integer())))

      # Empty list with last element
      assert difference(empty_list(), list(integer(), atom())) == none()

      assert difference(list(integer(), atom()), empty_list()) ==
               non_empty_list(integer(), atom())

      # List with any type and specific last element
      assert difference(list(term(), term()), list(term(), integer()))
             |> equal?(
               non_empty_list(term(), negation(union(integer(), non_empty_list(term(), term()))))
             )

      # Nested lists with last element
      assert difference(list(list(integer()), atom()), list(list(number()), boolean()))
             |> equal?(
               union(
                 non_empty_list(list(integer()), difference(atom(), boolean())),
                 non_empty_list(difference(list(integer()), list(number())), atom())
               )
             )

      # Union types in last element
      assert difference(list(integer(), union(atom(), binary())), list(number(), atom()))
             |> equal?(
               union(
                 non_empty_list(integer(), binary()),
                 non_empty_list(difference(integer(), number()), union(atom(), binary()))
               )
             )

      # Dynamic with last element
      assert equal?(
               difference(dynamic(list(term(), atom())), list(integer(), term())),
               dynamic(difference(list(term(), atom()), list(integer(), term())))
             )

      # Difference with proper list
      assert difference(list(integer(), atom()), list(integer())) ==
               non_empty_list(integer(), atom())
    end

    test "fun" do
      for arity <- [0, 1, 2, 3] do
        assert empty?(difference(none_fun(arity), none_fun(arity)))
      end

      assert empty?(difference(fun(), fun()))
      assert empty?(difference(none_fun(3), fun()))
      refute empty?(difference(fun(), none_fun(1)))
      refute empty?(difference(none_fun(2), none_fun(3)))
      assert empty?(intersection(none_fun(2), none_fun(3)))

      f1f2 = union(none_fun(1), none_fun(2))
      assert f1f2 |> difference(none_fun(1)) |> difference(none_fun(2)) |> empty?()
      assert none_fun(1) |> difference(difference(f1f2, none_fun(2))) |> empty?()
      assert f1f2 |> difference(none_fun(1)) |> equal?(none_fun(2))

      assert fun([integer()], term()) |> difference(fun([none()], term())) |> empty?()
    end
  end

  describe "creation" do
    test "map hoists dynamic" do
      assert dynamic(open_map(a: integer())) == open_map(a: dynamic(integer()))

      assert dynamic(open_map(a: union(integer(), binary()))) ==
               open_map(a: dynamic(integer()) |> union(binary()))

      # For domains too
      t1 = dynamic(open_map([{domain_key(:integer), integer()}]))
      t2 = open_map([{domain_key(:integer), dynamic(integer())}])
      assert t1 == t2

      # if_set on dynamic fields also must work
      t1 = dynamic(open_map(a: if_set(integer())))
      t2 = open_map(a: if_set(dynamic(integer())))
      assert t1 == t2
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
      assert subtype?(closed_map(a: integer()), closed_map(a: union(integer(), atom())))

      # optional
      refute subtype?(closed_map(a: if_set(integer())), closed_map(a: integer()))
      assert subtype?(closed_map(a: integer()), closed_map(a: if_set(integer())))
      refute subtype?(closed_map(a: if_set(term())), closed_map(a: term()))
      assert subtype?(closed_map(a: term()), closed_map(a: if_set(term())))

      # With domains
      t1 = closed_map([{domain_key(:integer), number()}])
      t2 = closed_map([{domain_key(:integer), integer()}])

      assert subtype?(t2, t1)

      t1_minus_t2 = difference(t1, t2)
      refute empty?(t1_minus_t2)

      assert subtype?(map_with_default(number()), open_map())
      t = difference(open_map(), map_with_default(number()))
      refute empty?(t)
      refute subtype?(open_map(), map_with_default(number()))
      assert subtype?(map_with_default(integer()), map_with_default(number()))
      refute subtype?(map_with_default(float()), map_with_default(atom()))

      assert equal?(
               intersection(map_with_default(number()), map_with_default(float())),
               map_with_default(float())
             )
    end

    test "optional" do
      refute subtype?(if_set(none()), term())
      refute subtype?(if_set(term()), term())
      assert subtype?(if_set(term()), if_set(term()))
    end

    test "list" do
      refute subtype?(non_empty_list(integer()), difference(list(number()), list(integer())))
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

    test "tuple" do
      assert compatible?(dynamic(tuple()), tuple([integer(), atom()]))
    end

    test "map" do
      assert compatible?(closed_map(a: integer()), open_map())
      assert compatible?(intersection(dynamic(), open_map()), closed_map(a: integer()))
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

      assert intersection(tuple([integer(), atom()]), open_tuple([atom()])) |> empty?()
      refute open_tuple([integer(), integer()]) |> difference(empty_tuple()) |> empty?()
      refute open_tuple([integer(), integer()]) |> difference(open_tuple([atom()])) |> empty?()
      refute open_tuple([term()]) |> difference(tuple([term()])) |> empty?()
      assert difference(tuple(), empty_tuple()) |> difference(open_tuple([term()])) |> empty?()
      assert difference(tuple(), open_tuple([term()])) |> difference(empty_tuple()) |> empty?()

      refute open_tuple([term()])
             |> difference(tuple([term()]))
             |> difference(tuple([term()]))
             |> empty?()

      assert tuple([integer(), union(integer(), atom())])
             |> difference(tuple([integer(), integer()]))
             |> difference(tuple([integer(), atom()]))
             |> empty?()
    end

    test "map" do
      assert open_map(a: none()) |> empty?()
      assert closed_map(a: integer(), b: none()) |> empty?()
      assert intersection(closed_map(b: atom()), open_map(a: integer())) |> empty?()
    end

    test "fun" do
      refute empty?(fun())
      refute empty?(none_fun(1))
      refute empty?(fun([integer()], atom()))

      assert empty?(intersection(none_fun(1), none_fun(2)))
      refute empty?(intersection(fun(), none_fun(1)))
      assert empty?(difference(none_fun(1), union(none_fun(1), none_fun(2))))
    end
  end

  describe "function creation" do
    test "fun_from_non_overlapping_clauses" do
      assert fun_from_non_overlapping_clauses([{[integer()], atom()}, {[float()], binary()}]) ==
               intersection(fun([integer()], atom()), fun([float()], binary()))
    end

    test "fun_from_inferred_clauses" do
      # No overlap
      assert fun_from_inferred_clauses([{[integer()], atom()}, {[float()], binary()}])
             |> equal?(
               intersection(
                 fun_from_non_overlapping_clauses([{[integer()], atom()}, {[float()], binary()}]),
                 fun([number()], dynamic())
               )
             )

      # Subsets
      assert fun_from_inferred_clauses([{[integer()], atom()}, {[number()], binary()}])
             |> equal?(
               intersection(
                 fun_from_non_overlapping_clauses([
                   {[integer()], union(atom(), binary())},
                   {[float()], binary()}
                 ]),
                 fun([number()], dynamic())
               )
             )

      assert fun_from_inferred_clauses([{[number()], binary()}, {[integer()], atom()}])
             |> equal?(
               intersection(
                 fun_from_non_overlapping_clauses([
                   {[integer()], union(atom(), binary())},
                   {[float()], binary()}
                 ]),
                 fun([number()], dynamic())
               )
             )

      # Partial
      assert fun_from_inferred_clauses([
               {[union(integer(), pid())], atom()},
               {[union(float(), pid())], binary()}
             ])
             |> equal?(
               intersection(
                 fun_from_non_overlapping_clauses([
                   {[integer()], atom()},
                   {[float()], binary()},
                   {[pid()], union(atom(), binary())}
                 ]),
                 fun([union(number(), pid())], dynamic())
               )
             )

      # Difference
      assert fun_from_inferred_clauses([
               {[integer(), union(pid(), atom())], atom()},
               {[number(), pid()], binary()}
             ])
             |> equal?(
               intersection(
                 fun_from_non_overlapping_clauses([
                   {[float(), pid()], binary()},
                   {[integer(), atom()], atom()},
                   {[integer(), pid()], union(atom(), binary())}
                 ]),
                 fun_from_non_overlapping_clauses([
                   {[integer(), union(pid(), atom())], dynamic()},
                   {[number(), pid()], dynamic()}
                 ])
               )
             )
    end
  end

  describe "function application" do
    defp none_fun(arity), do: fun(List.duplicate(none(), arity), term())

    test "non funs" do
      assert fun_apply(term(), [integer()]) == :badfun
      assert fun_apply(union(integer(), none_fun(1)), [integer()]) == :badfun
    end

    test "static" do
      # Full static
      assert fun_apply(fun(), [integer()]) == {:badarg, [none()]}
      assert fun_apply(difference(fun(), none_fun(2)), [integer()]) == {:badarg, [none()]}

      # Basic function application scenarios
      assert fun_apply(fun([integer()], atom()), [integer()]) == {:ok, atom()}
      assert fun_apply(fun([integer()], atom()), [float()]) == {:badarg, [integer()]}
      assert fun_apply(fun([integer()], atom()), [term()]) == {:badarg, [integer()]}

      # Return types
      assert fun_apply(fun([integer()], none()), [integer()]) == {:ok, none()}
      assert fun_apply(fun([integer()], term()), [integer()]) == {:ok, term()}

      # Dynamic args
      assert fun_apply(fun([term()], term()), [dynamic()]) == {:ok, term()}

      assert fun_apply(fun([integer()], atom()), [dynamic(integer())])
             |> elem(1)
             |> equal?(atom())

      assert fun_apply(fun([integer()], atom()), [dynamic(float())]) == {:badarg, [integer()]}
      assert fun_apply(fun([integer()], atom()), [dynamic(term())]) == {:ok, dynamic()}

      # Arity mismatches
      assert fun_apply(fun([integer()], integer()), [term(), term()]) == {:badarity, [1]}
      assert fun_apply(fun([integer(), atom()], boolean()), [integer()]) == {:badarity, [2]}

      # Function intersection tests (no overlap)
      fun0 = intersection(fun([integer()], atom()), fun([float()], binary()))
      assert fun_apply(fun0, [integer()]) == {:ok, atom()}
      assert fun_apply(fun0, [float()]) == {:ok, binary()}
      assert fun_apply(fun0, [number()]) == {:ok, union(atom(), binary())}

      assert fun_apply(fun0, [dynamic(integer())]) |> elem(1) |> equal?(atom())
      assert fun_apply(fun0, [dynamic(float())]) |> elem(1) |> equal?(binary())
      assert fun_apply(fun0, [dynamic(number())]) |> elem(1) |> equal?(union(atom(), binary()))
      assert fun_apply(fun0, [dynamic()]) == {:ok, dynamic()}

      # Function intersection tests (overlap)
      fun1 = intersection(fun([integer()], atom()), fun([number()], term()))
      assert fun_apply(fun1, [integer()]) == {:ok, atom()}
      assert fun_apply(fun1, [float()]) == {:ok, term()}

      # Function intersection with unions
      fun2 =
        intersection(
          fun([union(integer(), atom())], term()),
          fun([union(integer(), pid())], atom())
        )

      assert fun_apply(fun2, [integer()]) == {:ok, atom()}
      assert fun_apply(fun2, [atom()]) == {:ok, term()}
      assert fun_apply(fun2, [pid()]) == {:ok, atom()}

      # Function intersection with same domain, different codomains
      assert fun([integer()], term())
             |> intersection(fun([integer()], atom()))
             |> fun_apply([integer()]) == {:ok, atom()}

      # Function intersection with singleton atoms
      fun3 = intersection(fun([atom([:ok])], atom([:success])), fun([atom([:ok])], atom([:done])))
      assert fun_apply(fun3, [atom([:ok])]) == {:ok, none()}
    end

    test "static with dynamic signature" do
      assert fun_apply(fun([dynamic()], term()), [dynamic()]) == {:ok, term()}
      assert fun_apply(fun([integer()], dynamic()), [integer()]) == {:ok, dynamic()}

      assert fun_apply(fun([dynamic()], integer()), [dynamic()]) ==
               {:ok, union(integer(), dynamic())}

      assert fun_apply(fun([dynamic(), atom()], float()), [dynamic(), atom()]) ==
               {:ok, union(float(), dynamic())}

      fun = fun([dynamic(integer())], atom())
      assert fun_apply(fun, [dynamic(integer())]) == {:ok, union(atom(), dynamic())}
      assert fun_apply(fun, [dynamic(number())]) == {:ok, dynamic()}
      assert fun_apply(fun, [integer()]) == {:ok, dynamic()}
      assert fun_apply(fun, [float()]) == {:badarg, [dynamic(integer())]}
    end

    defp dynamic_fun(args, return), do: dynamic(fun(args, return))

    test "dynamic" do
      # Full dynamic
      assert fun_apply(dynamic(), [integer()]) == {:ok, dynamic()}
      assert fun_apply(dynamic(none_fun(1)), [integer()]) == {:ok, dynamic()}
      assert fun_apply(difference(dynamic(), none_fun(2)), [integer()]) == {:ok, dynamic()}

      # Basic function application scenarios
      assert fun_apply(dynamic_fun([integer()], atom()), [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(dynamic_fun([integer()], atom()), [float()]) == {:ok, dynamic()}
      assert fun_apply(dynamic_fun([integer()], atom()), [term()]) == {:ok, dynamic()}
      assert fun_apply(dynamic_fun([integer()], none()), [integer()]) == {:ok, dynamic(none())}
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

      # Function intersection tests
      fun0 = intersection(dynamic_fun([integer()], atom()), dynamic_fun([float()], binary()))
      assert fun_apply(fun0, [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(fun0, [float()]) == {:ok, dynamic(binary())}
      assert fun_apply(fun0, [dynamic(integer())]) == {:ok, dynamic(atom())}
      assert fun_apply(fun0, [dynamic(float())]) == {:ok, dynamic(binary())}
      assert fun_apply(fun0, [dynamic(number())]) == {:ok, dynamic(union(binary(), atom()))}

      # Function intersection with subset domain
      fun1 = intersection(dynamic_fun([integer()], atom()), dynamic_fun([number()], term()))
      assert fun_apply(fun1, [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(fun1, [float()]) == {:ok, dynamic()}
      assert fun_apply(fun1, [dynamic(integer())]) == {:ok, dynamic(atom())}
      assert fun_apply(fun1, [dynamic(float())]) == {:ok, dynamic()}

      # Function intersection with same domain, different codomains
      assert dynamic_fun([integer()], term())
             |> intersection(dynamic_fun([integer()], atom()))
             |> fun_apply([integer()]) == {:ok, dynamic(atom())}

      # Function intersection with overlapping domains
      fun2 =
        intersection(
          dynamic_fun([union(integer(), atom())], term()),
          dynamic_fun([union(integer(), pid())], atom())
        )

      assert fun_apply(fun2, [integer()]) == {:ok, dynamic(atom())}
      assert fun_apply(fun2, [atom()]) == {:ok, dynamic()}
      assert fun_apply(fun2, [pid()]) |> elem(1) |> equal?(dynamic(atom()))

      assert fun_apply(fun2, [dynamic(integer())]) == {:ok, dynamic(atom())}
      assert fun_apply(fun2, [dynamic(atom())]) == {:ok, dynamic()}
      assert fun_apply(fun2, [dynamic(pid())]) |> elem(1) |> equal?(dynamic(atom()))

      # Function intersection with singleton atoms
      fun3 =
        intersection(
          dynamic_fun([atom([:ok])], atom([:success])),
          dynamic_fun([atom([:ok])], atom([:done]))
        )

      assert fun_apply(fun3, [atom([:ok])]) == {:ok, dynamic(none())}
    end

    test "static and dynamic" do
      # Bad arity
      fun_arities =
        union(
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
        union(
          fun([atom()], integer()),
          dynamic_fun([integer()], binary())
        )

      assert fun_args |> fun_apply([atom()]) == {:ok, dynamic()}
      assert fun_args |> fun_apply([integer()]) == {:badarg, [dynamic(atom())]}

      # Badfun
      assert union(
               fun([atom()], integer()),
               dynamic_fun([integer()], binary()) |> intersection(none_fun(2))
             )
             |> fun_apply([atom()])
             |> elem(1)
             |> equal?(integer())

      assert union(
               fun([atom()], integer()) |> intersection(none_fun(2)),
               dynamic_fun([integer()], binary())
             )
             |> fun_apply([integer()]) == {:ok, dynamic(binary())}
    end
  end

  describe "projections" do
    test "truthiness" do
      for type <- [term(), none(), atom(), boolean(), union(atom([false]), integer())] do
        assert truthiness(type) == :undefined
        assert truthiness(dynamic(type)) == :undefined
      end

      for type <- [atom([false]), atom([nil]), atom([nil, false]), atom([false, nil])] do
        assert truthiness(type) == :always_false
        assert truthiness(dynamic(type)) == :always_false
      end

      for type <-
            [negation(atom()), atom([true]), negation(atom([false, nil])), atom([:ok]), integer()] do
        assert truthiness(type) == :always_true
        assert truthiness(dynamic(type)) == :always_true
      end
    end

    test "atom_fetch" do
      assert atom_fetch(term()) == :error
      assert atom_fetch(union(term(), dynamic(atom([:foo, :bar])))) == :error

      assert atom_fetch(atom()) == {:infinite, []}
      assert atom_fetch(dynamic()) == {:infinite, []}

      assert atom_fetch(atom([:foo, :bar])) ==
               {:finite, [:foo, :bar] |> :sets.from_list(version: 2) |> :sets.to_list()}

      assert atom_fetch(union(atom([:foo, :bar]), dynamic(atom()))) == {:infinite, []}
      assert atom_fetch(union(atom([:foo, :bar]), dynamic(term()))) == {:infinite, []}
    end

    test "list_hd" do
      assert list_hd(none()) == :badnonemptylist
      assert list_hd(term()) == :badnonemptylist
      assert list_hd(list(term())) == :badnonemptylist
      assert list_hd(empty_list()) == :badnonemptylist
      assert list_hd(non_empty_list(term())) == {false, term()}
      assert list_hd(non_empty_list(integer())) == {false, integer()}
      assert list_hd(difference(list(number()), list(integer()))) == {false, number()}

      assert list_hd(dynamic()) == {true, dynamic()}
      assert list_hd(dynamic(list(integer()))) == {true, dynamic(integer())}
      assert list_hd(union(dynamic(), atom())) == :badnonemptylist
      assert list_hd(union(dynamic(), list(term()))) == :badnonemptylist

      assert list_hd(difference(list(number()), list(number()))) == :badnonemptylist
      assert list_hd(dynamic(difference(list(number()), list(number())))) == :badnonemptylist

      assert list_hd(union(dynamic(list(float())), non_empty_list(atom()))) ==
               {true, union(dynamic(float()), atom())}

      # If term() is in the tail, it means list(term()) is in the tail
      # and therefore any term can be returned from hd.
      assert list_hd(non_empty_list(atom(), term())) == {false, term()}
      assert list_hd(non_empty_list(atom(), negation(list(term(), term())))) == {false, atom()}
    end

    test "list_tl" do
      assert list_tl(none()) == :badnonemptylist
      assert list_tl(term()) == :badnonemptylist
      assert list_tl(empty_list()) == :badnonemptylist
      assert list_tl(list(integer())) == :badnonemptylist
      assert list_tl(difference(list(number()), list(number()))) == :badnonemptylist

      assert list_tl(non_empty_list(integer())) == {false, list(integer())}

      assert list_tl(non_empty_list(integer(), atom())) ==
               {false, union(atom(), non_empty_list(integer(), atom()))}

      # The tail of either a (non empty) list of integers with an atom tail or a (non empty) list
      # of tuples with a float tail is either an atom, or a float, or a (possibly empty) list of
      # integers with an atom tail, or a (possibly empty) list of tuples with a float tail.
      assert list_tl(union(non_empty_list(integer(), atom()), non_empty_list(tuple(), float()))) ==
               {false,
                atom()
                |> union(float())
                |> union(
                  union(non_empty_list(integer(), atom()), non_empty_list(tuple(), float()))
                )}

      assert list_tl(dynamic()) == {true, dynamic()}
      assert list_tl(dynamic(list(integer()))) == {true, dynamic(list(integer()))}

      assert list_tl(dynamic(list(integer(), atom()))) ==
               {true, dynamic(union(atom(), list(integer(), atom())))}
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
      assert difference(tuple(), tuple()) |> tuple_fetch(0) == :badtuple

      assert tuple([atom()]) |> difference(empty_tuple()) |> tuple_fetch(0) ==
               {false, atom()}

      assert difference(tuple([union(integer(), atom())]), open_tuple([atom()]))
             |> tuple_fetch(0) == {false, integer()}

      assert tuple_fetch(union(tuple([integer(), atom()]), dynamic(open_tuple([atom()]))), 1)
             |> Kernel.then(fn {opt, ty} -> opt and equal?(ty, union(atom(), dynamic())) end)

      assert tuple_fetch(union(tuple([integer()]), tuple([atom()])), 0) ==
               {false, union(integer(), atom())}

      assert tuple([integer(), atom(), union(atom(), integer())])
             |> difference(tuple([integer(), term(), atom()]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple([integer(), atom(), union(union(atom(), integer()), list(term()))])
             |> difference(tuple([integer(), term(), atom()]))
             |> difference(open_tuple([term(), atom(), list(term())]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple([integer(), atom(), integer()])
             |> difference(tuple([integer(), term(), integer()]))
             |> tuple_fetch(1) == :badtuple

      assert tuple([integer(), atom(), integer()])
             |> difference(tuple([integer(), term(), atom()]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple_fetch(tuple(), 0) == :badindex
    end

    test "tuple_fetch with dynamic" do
      assert tuple_fetch(dynamic(), 0) == {true, dynamic()}
      assert tuple_fetch(dynamic(empty_tuple()), 0) == :badindex
      assert tuple_fetch(dynamic(tuple([integer(), atom()])), 2) == :badindex
      assert tuple_fetch(union(dynamic(), integer()), 0) == :badtuple

      assert tuple_fetch(dynamic(tuple()), 0)
             |> Kernel.then(fn {opt, type} -> opt and equal?(type, dynamic()) end)

      assert tuple_fetch(union(dynamic(), open_tuple([atom()])), 0) ==
               {true, union(atom(), dynamic())}
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

      # Test deleting from a union of tuples
      assert tuple_delete_at(union(tuple([integer(), atom()]), tuple([float(), binary()])), 1)
             |> equal?(tuple([union(integer(), float())]))

      # Test deleting from an intersection of tuples
      assert intersection(tuple([integer(), atom()]), tuple([term(), boolean()]))
             |> tuple_delete_at(1) == tuple([integer()])

      # Test deleting from a difference of tuples
      assert difference(tuple([integer(), atom(), boolean()]), tuple([term(), term()]))
             |> tuple_delete_at(1)
             |> equal?(tuple([integer(), boolean()]))

      # Test deleting from a complex union involving dynamic
      assert union(tuple([integer(), atom()]), dynamic(tuple([float(), binary()])))
             |> tuple_delete_at(1)
             |> equal?(union(tuple([integer()]), dynamic(tuple([float()]))))

      # Successfully deleting at position `index` in a tuple means that the dynamic
      # values that succeed are intersected with tuples of size at least `index`
      assert dynamic(tuple()) |> tuple_delete_at(0) == dynamic(tuple())
      assert dynamic(term()) |> tuple_delete_at(0) == dynamic(tuple())

      assert dynamic(union(tuple(), integer()))
             |> tuple_delete_at(1)
             |> equal?(dynamic(tuple_of_size_at_least(1)))
    end

    test "tuple_insert_at" do
      assert tuple_insert_at(tuple([integer(), atom()]), 3, boolean()) == :badindex
      assert tuple_insert_at(tuple([integer(), atom()]), -1, boolean()) == :badindex
      assert tuple_insert_at(integer(), 0, boolean()) == :badtuple
      assert tuple_insert_at(term(), 0, boolean()) == :badtuple

      # Out-of-bounds in a union
      assert union(tuple([integer(), atom()]), tuple([float()]))
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

      # Test inserting into a dynamic tuple
      assert tuple_insert_at(dynamic(tuple([integer(), atom()])), 1, boolean()) ==
               dynamic(tuple([integer(), boolean(), atom()]))

      # Test inserting into a union of tuples
      assert tuple_insert_at(union(tuple([integer()]), tuple([atom()])), 0, boolean()) ==
               union(tuple([boolean(), integer()]), tuple([boolean(), atom()]))

      # Test inserting into a difference of tuples
      assert difference(tuple([integer(), atom(), boolean()]), tuple([term(), term()]))
             |> tuple_insert_at(1, float())
             |> equal?(tuple([integer(), float(), atom(), boolean()]))

      # Test inserting into a complex union involving dynamic
      assert union(tuple([integer(), atom()]), dynamic(tuple([float(), binary()])))
             |> tuple_insert_at(1, boolean())
             |> equal?(
               union(
                 tuple([integer(), boolean(), atom()]),
                 dynamic(tuple([float(), boolean(), binary()]))
               )
             )

      # If you successfully intersect at position index in a type, then the dynamic values
      # that succeed are intersected with tuples of size at least index
      assert dynamic(union(tuple(), integer()))
             |> tuple_insert_at(1, boolean())
             |> equal?(dynamic(open_tuple([term(), boolean()])))
    end

    test "tuple_values" do
      assert tuple_values(integer()) == :badtuple
      assert tuple_values(tuple([none()])) == :badtuple
      assert tuple_values(tuple([])) == none()
      assert tuple_values(tuple()) == term()
      assert tuple_values(open_tuple([integer()])) == term()
      assert tuple_values(tuple([integer(), atom()])) == union(integer(), atom())

      assert tuple_values(union(tuple([float(), pid()]), tuple([reference()]))) ==
               union(float(), union(pid(), reference()))

      assert tuple_values(difference(tuple([number(), atom()]), tuple([float(), term()]))) ==
               union(integer(), atom())

      assert union(tuple([atom([:ok])]), open_tuple([integer()]))
             |> difference(open_tuple([term(), term()]))
             |> tuple_values() == union(atom([:ok]), integer())

      assert tuple_values(difference(tuple([number(), atom()]), tuple([float(), atom([:ok])]))) ==
               union(number(), atom())

      assert tuple_values(dynamic(tuple())) == dynamic()
      assert tuple_values(dynamic(tuple([integer()]))) == dynamic(integer())

      assert tuple_values(union(dynamic(tuple([integer()])), tuple([atom()]))) ==
               union(dynamic(integer()), atom())

      assert tuple_values(union(dynamic(tuple()), integer())) == :badtuple
      assert tuple_values(dynamic(union(integer(), tuple([atom()])))) == dynamic(atom())

      assert tuple_values(union(dynamic(tuple([integer()])), tuple([integer()])))
             |> equal?(integer())
    end

    test "domain_to_args" do
      # take complex tuples, normalize them, and check if they are still equal
      complex_tuples = [
        tuple([term(), atom(), number()])
        |> difference(tuple([atom(), atom(), float()])),
        # overlapping union and difference producing multiple variants
        difference(
          tuple([union(atom(), pid()), union(integer(), float())]),
          tuple([union(atom(), pid()), float()])
        )
      ]

      Enum.each(complex_tuples, fn domain ->
        args = domain_to_args(domain)

        assert Enum.reduce(args, none(), &union(args_to_domain(&1), &2))
               |> equal?(domain)
      end)
    end

    test "map_fetch" do
      assert map_fetch(term(), :a) == :badmap
      assert map_fetch(union(open_map(), integer()), :a) == :badmap

      assert map_fetch(open_map(), :a) == :badkey
      assert map_fetch(open_map(a: not_set()), :a) == :badkey
      assert map_fetch(union(closed_map(a: integer()), closed_map(b: atom())), :a) == :badkey
      assert map_fetch(difference(closed_map(a: integer()), closed_map(a: term())), :a) == :badkey

      assert map_fetch(closed_map(a: integer()), :a) == {false, integer()}

      assert map_fetch(union(closed_map(a: integer()), closed_map(a: atom())), :a) ==
               {false, union(integer(), atom())}

      {false, value_type} =
        open_map(my_map: open_map(foo: integer()))
        |> intersection(open_map(my_map: open_map(bar: boolean())))
        |> map_fetch(:my_map)

      assert equal?(value_type, open_map(foo: integer(), bar: boolean()))

      {false, value_type} =
        closed_map(a: union(integer(), atom()))
        |> difference(open_map(a: integer()))
        |> map_fetch(:a)

      assert equal?(value_type, atom())

      {false, value_type} =
        closed_map(a: integer(), b: atom())
        |> difference(closed_map(a: integer(), b: atom([:foo])))
        |> map_fetch(:a)

      assert equal?(value_type, integer())

      {false, value_type} =
        closed_map(a: integer())
        |> difference(closed_map(a: atom()))
        |> map_fetch(:a)

      assert equal?(value_type, integer())

      {false, value_type} =
        open_map(a: integer(), b: atom())
        |> union(closed_map(a: tuple()))
        |> map_fetch(:a)

      assert equal?(value_type, union(integer(), tuple()))

      {false, value_type} =
        closed_map(a: atom())
        |> difference(closed_map(a: atom([:foo, :bar])))
        |> difference(closed_map(a: atom([:bar])))
        |> map_fetch(:a)

      assert equal?(value_type, intersection(atom(), negation(atom([:foo, :bar]))))

      assert closed_map(a: union(atom([:ok]), pid()), b: integer(), c: tuple())
             |> difference(open_map(a: atom([:ok]), b: integer()))
             |> difference(open_map(a: atom(), c: tuple()))
             |> map_fetch(:a) == {false, pid()}

      assert closed_map(a: union(atom([:foo]), pid()), b: integer(), c: tuple())
             |> difference(open_map(a: atom([:foo]), b: integer()))
             |> difference(open_map(a: atom(), c: tuple()))
             |> map_fetch(:a) == {false, pid()}

      assert closed_map(a: union(atom([:foo, :bar, :baz]), integer()))
             |> difference(open_map(a: atom([:foo, :bar])))
             |> difference(open_map(a: atom([:foo, :baz])))
             |> map_fetch(:a) == {false, integer()}
    end

    test "map_fetch with dynamic" do
      assert map_fetch(dynamic(), :a) == {true, dynamic()}
      assert map_fetch(union(dynamic(), integer()), :a) == :badmap
      assert map_fetch(union(dynamic(open_map(a: integer())), integer()), :a) == :badmap
      assert map_fetch(union(dynamic(integer()), integer()), :a) == :badmap

      assert intersection(dynamic(), open_map(a: integer()))
             |> map_fetch(:a) == {false, intersection(integer(), dynamic())}

      {false, type} = union(dynamic(integer()), open_map(a: integer())) |> map_fetch(:a)
      assert equal?(type, integer())

      assert union(dynamic(integer()), open_map(a: if_set(integer()))) |> map_fetch(:a) == :badkey

      assert union(dynamic(open_map(a: atom())), open_map(a: integer()))
             |> map_fetch(:a) == {false, union(dynamic(atom()), integer())}
    end

    test "map_fetch with domain keys" do
      integer_to_atom = open_map([{domain_key(:integer), atom()}])
      assert map_fetch(integer_to_atom, :foo) == :badkey

      # the key :a is for sure of type pid and exists in type
      # %{atom() => pid()} and not %{:a => not_set()}
      t1 = closed_map([{domain_key(:atom), pid()}])
      t2 = closed_map(a: not_set())
      t3 = open_map(a: not_set())

      # Indeed, t2 is equivalent to the empty map
      assert map_fetch(difference(t1, t2), :a) == :badkey
      assert map_fetch(difference(t1, t3), :a) == {false, pid()}

      t4 = closed_map([{domain_key(:pid), atom()}])
      assert map_fetch(difference(t1, t4) |> difference(t3), :a) == {false, pid()}

      assert map_fetch(closed_map([{domain_key(:atom), pid()}]), :a) == :badkey

      assert map_fetch(dynamic(closed_map([{domain_key(:atom), pid()}])), :a) ==
               {true, dynamic(pid())}

      assert closed_map([{domain_key(:atom), number()}])
             |> difference(open_map(a: if_set(integer())))
             |> map_fetch(:a) == {false, float()}

      assert closed_map([{domain_key(:atom), number()}])
             |> difference(closed_map(b: if_set(integer())))
             |> map_fetch(:a) == :badkey
    end

    test "map_get with domain keys" do
      assert map_get(term(), term()) == :badmap

      map_type = closed_map([{domain_key(:tuple), binary()}])
      assert map_get(map_type, tuple()) == {:ok, nil_or_type(binary())}

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

      assert map_get(all_domains, atom([:bar])) == {:ok_present, atom([:ok])}

      assert map_get(all_domains, integer()) == {:ok, atom([:int]) |> nil_or_type()}
      assert map_get(all_domains, number()) == {:ok, atom([:int, :float]) |> nil_or_type()}

      assert map_get(all_domains, empty_list()) == {:ok_absent, atom([nil])}
      assert map_get(all_domains, atom([:foo])) == {:ok, binary() |> nil_or_type()}
      assert map_get(all_domains, binary()) == {:ok, integer() |> nil_or_type()}
      assert map_get(all_domains, tuple([integer(), atom()])) == {:ok, nil_or_type(float())}
      assert map_get(all_domains, empty_map()) == {:ok, pid() |> nil_or_type()}

      # Union
      assert map_get(all_domains, union(tuple(), empty_map())) ==
               {:ok, union(float(), pid() |> nil_or_type())}

      # Removing all maps with tuple keys
      t_no_tuple = difference(all_domains, closed_map([{domain_key(:tuple), float()}]))
      t_really_no_tuple = difference(all_domains, open_map([{domain_key(:tuple), float()}]))
      assert subtype?(all_domains, open_map())
      # It's only closed maps, so it should not change
      assert map_get(t_no_tuple, tuple()) == {:ok, float() |> nil_or_type()}
      # This time we actually removed all tuple to float keys
      assert map_get(t_really_no_tuple, tuple()) == {:ok_absent, atom([nil])}

      t1 = closed_map([{domain_key(:tuple), integer()}])
      t2 = closed_map([{domain_key(:tuple), float()}])
      t3 = union(t1, t2)
      assert map_get(t3, tuple()) == {:ok, number() |> nil_or_type()}
    end

    test "map_get with dynamic" do
      {_answer, type_selected} = map_get(dynamic(), term())
      assert equal?(type_selected, dynamic() |> nil_or_type())
    end

    test "map_get with atom fall back" do
      map = closed_map([{:a, atom([:a])}, {:b, atom([:b])}, {domain_key(:atom), pid()}])
      assert map_get(map, atom([:a, :b])) == {:ok_present, atom([:a, :b])}
      assert map_get(map, atom([:a, :c])) == {:ok, union(atom([:a]), pid() |> nil_or_type())}
      assert map_get(map, atom() |> difference(atom([:a, :b]))) == {:ok, pid() |> nil_or_type()}

      assert map_get(map, atom() |> difference(atom([:a]))) ==
               {:ok, union(atom([:b]), pid() |> nil_or_type())}
    end

    test "map_delete" do
      assert map_delete(term(), :a) == :badmap
      assert map_delete(integer(), :a) == :badmap
      assert map_delete(union(open_map(), integer()), :a) == :badmap
      assert map_delete(closed_map(a: integer(), b: atom()), :a) == {:ok, closed_map(b: atom())}
      assert map_delete(empty_map(), :a) == {:ok, empty_map()}

      assert map_delete(closed_map(a: if_set(integer()), b: atom()), :a) ==
               {:ok, closed_map(b: atom())}

      # Deleting a non-existent key
      assert map_delete(closed_map(a: integer(), b: atom()), :c) ==
               {:ok, closed_map(a: integer(), b: atom())}

      # Deleting from a dynamic map
      assert map_delete(dynamic(), :a) == {:ok, dynamic(open_map(a: not_set()))}

      # Deleting from an open map
      {:ok, type} = map_delete(open_map(a: integer(), b: atom()), :a)
      assert equal?(type, open_map(a: not_set(), b: atom()))

      # Deleting from a union of maps
      {:ok, type} = map_delete(union(closed_map(a: integer()), closed_map(b: atom())), :a)
      assert equal?(type, union(empty_map(), closed_map(b: atom())))

      # Deleting from a gradual map
      {:ok, type} = map_delete(union(dynamic(), closed_map(a: integer())), :a)
      assert equal?(type, union(dynamic(open_map(a: not_set())), empty_map()))

      {:ok, type} = map_delete(dynamic(open_map(a: not_set())), :b)
      assert equal?(type, dynamic(open_map(a: not_set(), b: not_set())))

      # Deleting from an intersection of maps
      {:ok, type} = map_delete(intersection(open_map(a: integer()), open_map(b: atom())), :a)
      assert equal?(type, open_map(a: not_set(), b: atom()))

      # Deleting from a difference of maps
      {:ok, type} =
        map_delete(
          difference(closed_map(a: integer(), b: atom()), closed_map(a: integer())),
          :b
        )

      assert equal?(type, closed_map(a: integer()))

      {:ok, type} = map_delete(difference(open_map(), open_map(a: not_set())), :a)
      assert equal?(type, open_map(a: not_set()))
    end

    test "map_delete with atom fallback" do
      assert closed_map([{:a, integer()}, {:b, atom()}, {domain_key(:atom), pid()}])
             |> map_delete(:a) ==
               {:ok, closed_map([{:a, not_set()}, {:b, atom()}, {domain_key(:atom), pid()}])}
    end

    test "map_take" do
      assert map_take(term(), :a) == :badmap
      assert map_take(integer(), :a) == :badmap
      assert map_take(union(open_map(), integer()), :a) == :badmap

      assert map_take(closed_map(a: integer(), b: atom()), :a) ==
               {integer(), closed_map(b: atom())}

      # Deleting a non-existent key
      assert map_take(empty_map(), :a) == :badkey
      assert map_take(closed_map(a: integer(), b: atom()), :c) == :badkey
      assert map_take(closed_map(a: if_set(integer()), b: atom()), :a) == :badkey

      # Deleting from a dynamic map
      assert map_take(dynamic(), :a) == {dynamic(), dynamic(open_map(a: not_set()))}

      # Deleting from an open map
      {value, type} = map_take(open_map(a: integer(), b: atom()), :a)
      assert value == integer()
      assert equal?(type, open_map(a: not_set(), b: atom()))

      # Deleting from a union of maps
      union = union(closed_map(a: integer()), closed_map(b: atom()))
      assert map_take(union, :a) == :badkey
      {value, type} = map_take(dynamic(union), :a)
      assert value == dynamic(integer())
      assert equal?(type, dynamic(union(empty_map(), closed_map(b: atom()))))

      # Deleting from a gradual map
      {value, type} = map_take(union(dynamic(), closed_map(a: integer())), :a)
      assert value == union(dynamic(), integer())
      assert equal?(type, union(dynamic(open_map(a: not_set())), empty_map()))

      {value, type} = map_take(dynamic(open_map(a: not_set())), :b)
      assert equal?(value, dynamic())
      assert equal?(type, dynamic(open_map(a: not_set(), b: not_set())))

      # Deleting from an intersection of maps
      {value, type} = map_take(intersection(open_map(a: integer()), open_map(b: atom())), :a)
      assert value == integer()
      assert equal?(type, open_map(a: not_set(), b: atom()))

      # Deleting from a difference of maps
      {value, type} =
        map_take(difference(closed_map(a: integer(), b: atom()), closed_map(a: integer())), :b)

      assert value == atom()
      assert equal?(type, closed_map(a: integer()))

      {value, type} = map_take(difference(open_map(), open_map(a: not_set())), :a)
      assert equal?(value, term())
      assert equal?(type, open_map(a: not_set()))
    end

    test "map_fetch_and_put" do
      assert map_fetch_and_put(term(), :a, integer()) == :badmap
      assert map_fetch_and_put(open_map(), :a, integer()) == :badkey
    end

    test "map_put" do
      assert map_put(term(), :a, integer()) == :badmap
      assert map_put(integer(), :a, integer()) == :badmap
      assert map_put(dynamic(integer()), :a, atom()) == :badmap
      assert map_put(union(integer(), dynamic()), :a, atom()) == :badmap
      assert map_put(empty_map(), :a, integer()) == {:ok, closed_map(a: integer())}

      # Replace an existing key in a closed map
      assert map_put(closed_map(a: integer()), :a, atom()) == {:ok, closed_map(a: atom())}

      # Add a new key to a closed map
      assert map_put(closed_map(a: integer()), :b, atom()) ==
               {:ok, closed_map(a: integer(), b: atom())}

      # Replace an existing key in an open map
      assert map_put(open_map(a: integer()), :a, atom()) ==
               {:ok, open_map(a: atom())}

      # Add a new key to an open map
      assert map_put(open_map(a: integer()), :b, atom()) ==
               {:ok, open_map(a: integer(), b: atom())}

      # Put a key-value pair in a union of maps
      {:ok, type} =
        union(closed_map(a: integer()), closed_map(b: atom())) |> map_put(:c, boolean())

      assert equal?(
               type,
               union(
                 closed_map(a: integer(), c: boolean()),
                 closed_map(b: atom(), c: boolean())
               )
             )

      # Put a key-value pair in a dynamic map
      assert map_put(dynamic(open_map()), :a, integer()) ==
               {:ok, dynamic(open_map(a: integer()))}

      # Put a key-value pair in an intersection of maps
      {:ok, type} =
        intersection(open_map(a: integer()), open_map(b: atom())) |> map_put(:c, boolean())

      assert equal?(type, open_map(a: integer(), b: atom(), c: boolean()))

      # Put a key-value pair in a difference of maps
      {:ok, type} = difference(open_map(), closed_map(a: integer())) |> map_put(:b, atom())
      assert equal?(type, difference(open_map(b: atom()), closed_map(a: integer())))

      # Put a new key-value pair with dynamic type
      # Note: setting a field to a dynamic type makes the whole map become dynamic.
      assert map_put(open_map(), :a, dynamic()) == {:ok, dynamic(open_map(a: term()))}

      # Put a key-value pair in a map with optional fields
      {:ok, type} = closed_map(a: if_set(integer())) |> map_put(:b, atom())
      assert equal?(type, closed_map(a: if_set(integer()), b: atom()))

      # Fetching on a key-value pair that was put to a given type returns {false, type}
      {:ok, map} = map_put(union(dynamic(), empty_map()), :a, atom())
      {false, type} = map_fetch(map, :a)
      assert equal?(type, atom())
    end

    test "map_put with domain keys" do
      # Using a literal key or an expression of that singleton key is the same
      assert map_refresh(empty_map(), atom([:a]), integer()) == {:ok, closed_map(a: integer())}

      # Several keys
      assert map_refresh(empty_map(), atom([:a, :b]), integer()) ==
               {:ok, closed_map(a: if_set(integer()), b: if_set(integer()))}

      assert map_refresh(empty_map(), integer(), integer()) ==
               {:ok, closed_map([{domain_key(:integer), integer()}])}

      assert map_refresh(closed_map([{domain_key(:integer), integer()}]), integer(), float()) ==
               {:ok, closed_map([{domain_key(:integer), number()}])}

      assert map_refresh(open_map(), integer(), integer()) == {:ok, open_map()}

      # TODO: Revisit this
      # {:ok, type} = map_refresh(empty_map(), integer(), dynamic())
      # assert equal?(type, dynamic(closed_map([{domain_key(:integer), term()}])))

      # Adding a key of type float to a dynamic only guarantees that we have a map
      # as we cannot express "has at least one key of type float => float"
      {:ok, type} = map_refresh(dynamic(), float(), float())
      assert equal?(type, dynamic(open_map()))

      assert closed_map([{domain_key(:integer), integer()}])
             |> difference(open_map())
             |> empty?()

      assert closed_map([{domain_key(:integer), integer()}])
             |> difference(open_map())
             |> map_refresh(integer(), float()) == :badmap

      assert map_refresh(empty_map(), number(), float()) ==
               {:ok,
                closed_map([
                  {domain_key(:integer), float()},
                  {domain_key(:float), float()}
                ])}

      # Tricky cases with atoms:
      # We add one atom fields that maps to an integer, which is not :a. So we do not touch
      # :a, add integer to :b, and add a domain field.
      assert map_refresh(
               closed_map(a: pid(), b: pid()),
               atom() |> difference(atom([:a])),
               integer()
             ) ==
               {:ok,
                closed_map([
                  {:a, pid()},
                  {:b, union(pid(), integer())},
                  {domain_key(:atom), integer()}
                ])}

      assert map_refresh(empty_map(), term(), integer()) == {:ok, map_with_default(integer())}
    end
  end

  describe "disjoint" do
    test "optional" do
      assert disjoint?(term(), if_set(none()))
      assert disjoint?(term(), if_set(none()) |> union(non_empty_list(none())))
    end

    test "map" do
      refute disjoint?(open_map(), open_map(a: integer()))
    end
  end

  describe "to_quoted" do
    test "bitmap" do
      assert union(integer(), union(float(), binary())) |> to_quoted_string() ==
               "binary() or float() or integer()"
    end

    test "none" do
      assert none() |> to_quoted_string() == "none()"
      assert dynamic(none()) |> to_quoted_string() == "none()"
    end

    test "negation" do
      assert negation(negation(integer())) |> to_quoted_string() == "integer()"
      assert negation(negation(atom([:foo, :bar]))) |> to_quoted_string() == ":bar or :foo"
      assert negation(negation(list(term()))) |> to_quoted_string() == "list(term())"
    end

    test "atom" do
      assert atom() |> to_quoted_string() == "atom()"
      assert atom([:a]) |> to_quoted_string() == ":a"
      assert atom([:a, :b]) |> to_quoted_string() == ":a or :b"
      assert difference(atom(), atom([:a])) |> to_quoted_string() == "atom() and not :a"

      assert atom([Elixir]) |> to_quoted_string() == "Elixir"
      assert atom([Foo.Bar]) |> to_quoted_string() == "Foo.Bar"
    end

    test "boolean" do
      assert boolean() |> to_quoted_string() == "boolean()"
      assert atom([true, false, :a]) |> to_quoted_string() == ":a or boolean()"
      assert atom([true, :a]) |> to_quoted_string() == ":a or true"
      assert difference(atom(), boolean()) |> to_quoted_string() == "atom() and not boolean()"
    end

    test "dynamic" do
      assert dynamic() |> to_quoted_string() == "dynamic()"

      assert dynamic(union(atom(), integer())) |> union(integer()) |> to_quoted_string() ==
               "dynamic(atom()) or integer()"

      assert intersection(binary(), dynamic()) |> to_quoted_string() == "binary()"

      assert intersection(union(binary(), pid()), dynamic()) |> to_quoted_string() ==
               "dynamic(binary() or pid())"

      assert intersection(atom(), dynamic()) |> to_quoted_string() == "dynamic(atom())"

      assert union(atom([:foo, :bar]), dynamic()) |> to_quoted_string() ==
               "dynamic() or :bar or :foo"

      assert intersection(dynamic(), closed_map(a: integer())) |> to_quoted_string() ==
               "dynamic(%{a: integer()})"
    end

    test "lists" do
      assert list(term()) |> to_quoted_string() == "list(term())"
      assert list(integer()) |> to_quoted_string() == "list(integer())"

      assert list(term()) |> difference(empty_list()) |> to_quoted_string() ==
               "non_empty_list(term())"

      assert list(term()) |> difference(list(integer())) |> to_quoted_string() ==
               "non_empty_list(term()) and not non_empty_list(integer())"

      assert list(term())
             |> difference(list(integer()))
             |> difference(list(atom()))
             |> to_quoted_string() ==
               "non_empty_list(term()) and not (non_empty_list(atom()) or non_empty_list(integer()))"

      assert list(term(), integer()) |> to_quoted_string() ==
               "empty_list() or non_empty_list(term(), integer())"

      assert difference(list(term(), atom()), list(term(), boolean())) |> to_quoted_string() ==
               "non_empty_list(term(), atom() and not boolean())"

      assert list(term(), term()) |> to_quoted_string() ==
               "empty_list() or non_empty_list(term(), term())"

      assert non_empty_list(term(), difference(term(), list(term()))) |> to_quoted_string() ==
               "improper_list()"

      # Test normalization

      # Remove duplicates
      assert union(list(integer()), list(integer())) |> to_quoted_string() == "list(integer())"

      # Merge subtypes
      assert union(list(float(), pid()), list(number(), pid())) |> to_quoted_string() ==
               "empty_list() or non_empty_list(float() or integer(), pid())"

      # Merge last element types
      assert union(list(atom([:ok]), integer()), list(atom([:ok]), float()))
             |> to_quoted_string() ==
               "empty_list() or non_empty_list(:ok, float() or integer())"

      assert union(dynamic(list(integer(), float())), dynamic(list(integer(), pid())))
             |> to_quoted_string() ==
               "dynamic(empty_list() or non_empty_list(integer(), float() or pid()))"
    end

    test "tuples" do
      assert tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom()}"

      assert tuple([integer(), dynamic(atom())]) |> to_quoted_string() ==
               "dynamic({integer(), atom()})"

      assert open_tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom(), ...}"

      assert union(tuple([integer(), atom()]), open_tuple([atom()])) |> to_quoted_string() ==
               "{atom(), ...} or {integer(), atom()}"

      assert difference(tuple([integer(), atom()]), open_tuple([atom()])) |> to_quoted_string() ==
               "{integer(), atom()}"

      assert tuple([closed_map(a: integer()), open_map()]) |> to_quoted_string() ==
               "{%{a: integer()}, map()}"

      assert union(tuple([integer(), atom()]), tuple([integer(), atom()])) |> to_quoted_string() ==
               "{integer(), atom()}"

      assert union(tuple([integer(), atom()]), tuple([float(), atom()])) |> to_quoted_string() ==
               "{float() or integer(), atom()}"

      assert union(tuple([integer(), atom()]), tuple([float(), atom()]))
             |> union(tuple([pid(), pid(), port()]))
             |> union(tuple([pid(), pid(), atom()]))
             |> to_quoted_string() ==
               "{float() or integer(), atom()} or {pid(), pid(), atom() or port()}"

      assert union(open_tuple([integer()]), open_tuple([float()])) |> to_quoted_string() ==
               "{float() or integer(), ...}"

      # {:ok, {term(), integer()}} or {:ok, {term(), float()}} or {:exit, :kill} or {:exit, :timeout}
      assert tuple([atom([:ok]), tuple([term(), empty_list()])])
             |> union(tuple([atom([:ok]), tuple([term(), open_map()])]))
             |> union(tuple([atom([:exit]), atom([:kill])]))
             |> union(tuple([atom([:exit]), atom([:timeout])]))
             |> to_quoted_string() ==
               "{:exit, :kill or :timeout} or {:ok, {term(), empty_list() or map()}}"

      # Detection of duplicates
      assert tuple([atom([:ok]), term()])
             |> union(tuple([atom([:ok]), term()]))
             |> to_quoted_string() == "{:ok, term()}"

      assert tuple([closed_map(a: integer(), b: atom()), open_map()])
             |> union(tuple([closed_map(a: integer(), b: atom()), open_map()]))
             |> to_quoted_string() ==
               "{%{a: integer(), b: atom()}, map()}"

      # Nested fusion
      assert tuple([closed_map(a: integer(), b: atom()), open_map()])
             |> union(tuple([closed_map(a: float(), b: atom()), open_map()]))
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
             |> union(
               tuple([decimal_inf, binary()])
               |> union(
                 tuple([decimal_nan, binary()])
                 |> union(
                   tuple([decimal_int, term()])
                   |> union(tuple([union(decimal_inf, union(decimal_nan, decimal_int)), term()]))
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

    test "function" do
      assert fun() |> to_quoted_string() == "fun()"
      assert none_fun(1) |> to_quoted_string() == "(none() -> term())"

      assert none_fun(1)
             |> intersection(none_fun(2))
             |> to_quoted_string() == "none()"

      assert fun([integer(), float()], boolean()) |> to_quoted_string() ==
               "(integer(), float() -> boolean())"

      assert fun([integer()], boolean())
             |> union(fun([float()], boolean()))
             |> to_quoted_string() ==
               "(integer() -> boolean()) or (float() -> boolean())"

      assert fun([integer()], boolean())
             |> intersection(fun([float()], boolean()))
             |> to_quoted_string() ==
               "(integer() -> boolean()) and (float() -> boolean())"
    end

    test "function with optimized intersections" do
      assert fun([integer()], atom()) |> intersection(none_fun(1)) |> to_quoted_string() ==
               "(integer() -> atom())"

      assert fun([integer()], atom())
             |> difference(none_fun(2))
             |> intersection(none_fun(1))
             |> to_quoted_string() ==
               "(integer() -> atom())"
    end

    test "function with dynamic signatures" do
      assert fun([dynamic(integer())], float()) |> to_quoted_string() ==
               "(dynamic(integer()) -> float())"

      assert fun([dynamic(atom())], float()) |> to_quoted_string() ==
               "(dynamic(atom()) -> float())"

      assert fun([integer(), float()], dynamic(atom())) |> to_quoted_string() ==
               "(integer(), float() -> dynamic(atom()))"

      domain_part = fun([dynamic(atom()) |> union(integer()), binary()], float())

      assert domain_part |> to_quoted_string() ==
               "(dynamic(atom()) or integer(), binary() -> float())"

      codomain_part = fun([pid(), float()], dynamic(atom()) |> union(integer()))

      assert codomain_part |> to_quoted_string() ==
               "(pid(), float() -> dynamic(atom()) or integer())"

      assert union(domain_part, codomain_part) |> to_quoted_string() ==
               """
               (dynamic(atom()) or integer(), binary() -> float()) or
                 (pid(), float() -> dynamic(atom()) or integer())\
               """

      assert intersection(domain_part, codomain_part) |> to_quoted_string() ==
               """
               (dynamic(atom()) or integer(), binary() -> float()) and
                 (pid(), float() -> dynamic(atom()) or integer())\
               """
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

      assert difference(open_map(), open_map(a: term())) |> to_quoted_string() ==
               "%{..., a: not_set()}"

      assert closed_map(a: integer(), b: atom()) |> to_quoted_string() ==
               "%{a: integer(), b: atom()}"

      assert open_map(a: float())
             |> difference(closed_map(a: float()))
             |> to_quoted_string() == "%{..., a: float()} and not %{a: float()}"

      assert difference(open_map(), empty_map()) |> to_quoted_string() ==
               "map() and not empty_map()"

      assert closed_map(foo: union(integer(), not_set())) |> to_quoted_string() ==
               "%{foo: if_set(integer())}"

      # Test normalization
      assert open_map(a: integer(), b: atom())
             |> difference(open_map(b: atom()))
             |> union(open_map(a: integer()))
             |> to_quoted_string() == "%{..., a: integer()}"

      assert union(open_map(a: integer()), open_map(a: integer())) |> to_quoted_string() ==
               "%{..., a: integer()}"

      assert difference(open_map(a: number(), b: atom()), open_map(a: integer()))
             |> to_quoted_string() == "%{..., a: float(), b: atom()}"

      # Basic map fusion
      assert union(closed_map(a: integer()), closed_map(a: integer())) |> to_quoted_string() ==
               "%{a: integer()}"

      assert union(closed_map(a: integer()), closed_map(a: float())) |> to_quoted_string() ==
               "%{a: float() or integer()}"

      # Nested fusion
      assert union(closed_map(a: integer(), b: atom()), closed_map(a: float(), b: atom()))
             |> union(closed_map(x: pid(), y: pid(), z: port()))
             |> union(closed_map(x: pid(), y: pid(), z: atom()))
             |> to_quoted_string() ==
               "%{a: float() or integer(), b: atom()} or %{x: pid(), y: pid(), z: atom() or port()}"

      # Open map fusion
      assert union(open_map(a: integer()), open_map(a: float())) |> to_quoted_string() ==
               "%{..., a: float() or integer()}"

      # Fusing complex nested maps with unions
      assert closed_map(
               status: atom([:ok]),
               data: closed_map(value: term(), count: empty_list())
             )
             |> union(
               closed_map(
                 status: atom([:ok]),
                 data: closed_map(value: term(), count: open_map())
               )
             )
             |> union(closed_map(status: atom([:error]), reason: atom([:timeout])))
             |> union(closed_map(status: atom([:error]), reason: atom([:crash])))
             |> to_quoted_string() ==
               "%{data: %{count: empty_list() or map(), value: term()}, status: :ok} or\n  %{reason: :crash or :timeout, status: :error}"

      # Difference and union tests
      assert closed_map(status: atom([:ok]), value: term())
             |> difference(closed_map(status: atom([:ok]), value: float()))
             |> union(
               closed_map(status: atom([:ok]), value: term())
               |> difference(closed_map(status: atom([:ok]), value: integer()))
             )
             |> to_quoted_string() ==
               "%{status: :ok, value: term()}"

      # Nested map fusion
      assert closed_map(data: closed_map(x: integer(), y: atom()), meta: open_map())
             |> union(closed_map(data: closed_map(x: float(), y: atom()), meta: open_map()))
             |> to_quoted_string() ==
               "%{data: %{x: float() or integer(), y: atom()}, meta: map()}"

      # Test complex combinations
      assert intersection(
               open_map(a: number(), b: atom()),
               open_map(a: integer(), c: boolean())
             )
             |> union(difference(open_map(x: atom()), open_map(x: boolean())))
             |> to_quoted_string() ==
               "%{..., a: integer(), b: atom(), c: boolean()} or %{..., x: atom() and not boolean()}"

      assert closed_map(a: number(), b: atom(), c: pid())
             |> difference(closed_map(a: integer(), b: atom(), c: pid()))
             |> to_quoted_string() == "%{a: float(), b: atom(), c: pid()}"

      # No simplification compared to above, as it is an open map
      assert open_map(a: number(), b: atom())
             |> difference(closed_map(a: integer(), b: atom()))
             |> to_quoted_string() ==
               "%{..., a: float() or integer(), b: atom()} and not %{a: integer(), b: atom()}"

      # Remark: this simplification is order dependent. Having the first difference
      # after the second gives a different result.
      assert open_map(a: number(), b: atom(), c: union(pid(), port()))
             |> difference(open_map(a: float(), b: atom(), c: pid()))
             |> difference(open_map(a: integer(), b: atom(), c: union(pid(), port())))
             |> to_quoted_string() == "%{..., a: float(), b: atom(), c: port()}"
    end

    test "maps as dictionaries" do
      assert closed_map([{domain_key(:integer), integer()}])
             |> to_quoted_string() == "%{integer() => if_set(integer())}"

      assert closed_map([{domain_key(:integer), integer()}, {:float, float()}])
             |> to_quoted_string() == "%{integer() => if_set(integer()), float: float()}"
    end

    test "structs" do
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
      assert union(closed_map(__struct__: atom([Foo])), closed_map(__struct__: atom([Bar])))
             |> to_quoted_string() ==
               "%{__struct__: Bar} or %{__struct__: Foo}"

      # Properly format non_struct_map
      assert open_map(__struct__: if_set(negation(atom()))) |> to_quoted_string() ==
               "non_struct_map()"
    end
  end

  describe "performance" do
    test "tuple difference" do
      # Large difference with no duplicates
      descr1 =
        union(
          atom([:ignored, :reset]),
          tuple([atom([:font_style]), atom([:italic])])
        )

      descr2 =
        union(
          atom([:ignored, :reset]),
          union(
            tuple([atom([:font_style]), atom([:italic])]),
            Enum.reduce(
              for elem1 <- 1..5, elem2 <- 1..5 do
                tuple([atom([:"f#{elem1}"]), atom([:"s#{elem2}"])])
              end,
              &union/2
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
          {:age, union(integer(), atom())},
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
  end
end
