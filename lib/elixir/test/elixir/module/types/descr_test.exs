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
      assert equal?(union(intersection(dynamic(), atom()), atom()), atom())
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

      assert difference(open_map(a: integer()), closed_map(b: boolean()))
             |> equal?(open_map(a: integer()))
    end

    test "list" do
      assert union(list(term()), list(term())) |> equal?(list(term()))
      assert union(list(integer()), list(term())) |> equal?(list(term()))

      assert union(difference(list(term()), list(integer())), list(integer()))
             |> equal?(list(term()))
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

      assert equal?(
               intersection(closed_map(a: integer()), closed_map(a: if_set(integer()))),
               closed_map(a: integer())
             )

      assert empty?(intersection(closed_map(a: integer()), closed_map(a: atom())))
    end

    defp number(), do: union(integer(), float())

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

    defp empty_tuple(), do: tuple([])
    defp tuple_of_size_at_least(n) when is_integer(n), do: open_tuple(List.duplicate(term(), n))
    defp tuple_of_size(n) when is_integer(n) and n >= 0, do: tuple(List.duplicate(term(), n))

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
             |> empty?

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
    end

    defp improper_list(type), do: difference(list(type, term()), list(type))

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
      assert difference(list(atom(), term()), improper_list(atom())) |> equal?(list(atom()))

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
  end

  describe "creation" do
    test "map hoists dynamic" do
      assert dynamic(open_map(a: integer())) == open_map(a: dynamic(integer()))

      assert dynamic(open_map(a: union(integer(), binary()))) ==
               open_map(a: dynamic(integer()) |> union(binary()))
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
    end

    test "list" do
      refute subtype?(non_empty_list(integer()), difference(list(number()), list(integer())))
      assert subtype?(list(term(), boolean()), list(term(), atom()))
      assert subtype?(list(integer()), list(term()))
      assert subtype?(list(term()), list(term(), term()))
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

  describe "empty" do
    test "tuple" do
      assert intersection(tuple([integer(), atom()]), open_tuple([atom()])) |> empty?
      refute open_tuple([integer(), integer()]) |> difference(empty_tuple()) |> empty?
      refute open_tuple([integer(), integer()]) |> difference(open_tuple([atom()])) |> empty?
      refute open_tuple([term()]) |> difference(tuple([term()])) |> empty?
      assert difference(tuple(), empty_tuple()) |> difference(open_tuple([term()])) |> empty?
      assert difference(tuple(), open_tuple([term()])) |> difference(empty_tuple()) |> empty?

      refute open_tuple([term()])
             |> difference(tuple([term()]))
             |> difference(tuple([term()]))
             |> empty?

      assert tuple([integer(), union(integer(), atom())])
             |> difference(tuple([integer(), integer()]))
             |> difference(tuple([integer(), atom()]))
             |> empty?
    end

    test "map" do
      assert intersection(closed_map(b: atom()), open_map(a: integer())) |> empty?()
    end
  end

  describe "queries" do
    test "atom_type?" do
      assert atom_type?(term(), :foo)
      assert atom_type?(dynamic(), :foo)

      assert atom_type?(atom([:foo, :bar]), :foo)
      refute atom_type?(atom([:foo, :bar]), :baz)
      assert atom_type?(negation(atom([:foo, :bar])), :baz)

      refute atom_type?(union(atom([:foo, :bar]), integer()), :baz)
      refute atom_type?(dynamic(union(atom([:foo, :bar]), integer())), :baz)
    end
  end

  describe "projections" do
    test "fun_fetch" do
      assert fun_fetch(term(), 1) == :error
      assert fun_fetch(union(term(), dynamic(fun())), 1) == :error
      assert fun_fetch(fun(), 1) == :ok
      assert fun_fetch(dynamic(), 1) == :ok
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

      assert list_hd(union(dynamic(list(float())), non_empty_list(atom()))) ==
               {true, union(dynamic(float()), atom())}

      # If term() is in the tail, it means list(term()) is in the tail
      # and therefore any term can be returned from hd.
      assert list_hd(non_empty_list(atom(), term())) == {false, term()}
      assert list_hd(non_empty_list(atom(), negation(list(term(), term())))) == {false, atom()}
    end

    test "list_tl" do
      assert list_tl(term()) == :badnonemptylist
      assert list_tl(empty_list()) == :badnonemptylist
      assert list_tl(list(integer())) == :badnonemptylist
      assert list_tl(non_empty_list(integer())) == {false, list(integer())}

      assert list_tl(non_empty_list(integer(), atom())) ==
               {false, union(atom(), list(integer(), atom()))}

      # The tail of either a (non empty) list of integers with an atom tail or a (non empty) list
      # of tuples with a float tail is either an atom, or a float, or a (possibly empty) list of
      # integers with an atom tail, or a (possibly empty) list of tuples with a float tail.
      assert list_tl(union(non_empty_list(integer(), atom()), non_empty_list(tuple(), float()))) ==
               {false,
                atom()
                |> union(float())
                |> union(union(list(integer(), atom()), list(tuple(), float())))}

      assert list_tl(dynamic()) == {true, dynamic()}
      assert list_tl(dynamic(list(integer()))) == {true, dynamic(list(integer()))}

      assert list_tl(dynamic(list(integer(), atom()))) ==
               {true, dynamic(union(atom(), list(integer(), atom())))}
    end

    test "tuple_fetch" do
      assert tuple_fetch(term(), 0) == :badtuple
      assert tuple_fetch(integer(), 0) == :badtuple

      assert tuple_fetch(tuple([integer(), atom()]), 0) == {false, integer()}
      assert tuple_fetch(tuple([integer(), atom()]), 1) == {false, atom()}
      assert tuple_fetch(tuple([integer(), atom()]), 2) == :badindex

      assert tuple_fetch(open_tuple([integer(), atom()]), 0) == {false, integer()}
      assert tuple_fetch(open_tuple([integer(), atom()]), 1) == {false, atom()}
      assert tuple_fetch(open_tuple([integer(), atom()]), 2) == :badindex

      assert tuple_fetch(tuple([integer(), atom()]), -1) == :badindex
      assert tuple_fetch(empty_tuple(), 0) == :badindex
      assert difference(tuple(), tuple()) |> tuple_fetch(0) == :badindex

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
             |> tuple_fetch(1) == :badindex

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
      assert tuple_delete_at(union(tuple([integer(), atom()]), tuple([float(), binary()])), 1) ==
               union(tuple([integer()]), tuple([float()]))

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

      # Succesfully deleting at position `index` in a tuple means that the dynamic
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

      # If you succesfully intersect at position index in a type, then the dynamic values
      # that succeed are intersected with tuples of size at least index
      assert dynamic(union(tuple(), integer()))
             |> tuple_insert_at(1, boolean())
             |> equal?(dynamic(open_tuple([term(), boolean()])))
    end

    test "map_fetch" do
      assert map_fetch(term(), :a) == :badmap
      assert map_fetch(union(open_map(), integer()), :a) == :badmap

      assert map_fetch(closed_map(a: integer()), :a) == {false, integer()}

      assert map_fetch(union(closed_map(a: integer()), closed_map(b: atom())), :a) ==
               :badkey

      assert map_fetch(difference(closed_map(a: integer()), closed_map(a: term())), :a) ==
               :badkey

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

      assert closed_map(a: union(atom(), pid()), b: integer(), c: tuple())
             |> difference(open_map(a: atom(), b: integer()))
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

    test "map_delete" do
      assert map_delete(term(), :a) == :badmap
      assert map_delete(integer(), :a) == :badmap
      assert map_delete(union(open_map(), integer()), :a) == :badmap
      assert map_delete(closed_map(a: integer(), b: atom()), :a) == closed_map(b: atom())
      assert map_delete(empty_map(), :a) == empty_map()
      assert map_delete(closed_map(a: if_set(integer()), b: atom()), :a) == closed_map(b: atom())

      # Deleting a non-existent key
      assert map_delete(closed_map(a: integer(), b: atom()), :c) ==
               closed_map(a: integer(), b: atom())

      # Deleting from an open map
      assert map_delete(open_map(a: integer(), b: atom()), :a)
             |> equal?(open_map(a: not_set(), b: atom()))

      # Deleting from a union of maps
      assert map_delete(union(closed_map(a: integer()), closed_map(b: atom())), :a)
             |> equal?(union(empty_map(), closed_map(b: atom())))

      # Deleting from a dynamic map
      assert map_delete(dynamic(), :a) == dynamic(open_map(a: not_set()))

      # Deleting from a gradual map
      assert map_delete(union(dynamic(), closed_map(a: integer())), :a)
             |> equal?(union(dynamic(open_map(a: not_set())), empty_map()))

      assert map_delete(dynamic(open_map(a: not_set())), :b)
             |> equal?(dynamic(open_map(a: not_set(), b: not_set())))

      # Deleting from an intersection of maps
      assert map_delete(intersection(open_map(a: integer()), open_map(b: atom())), :a) ==
               open_map(a: not_set(), b: atom())

      # Deleting from a difference of maps
      assert difference(closed_map(a: integer(), b: atom()), closed_map(a: integer()))
             |> map_delete(:b)
             |> equal?(closed_map(a: integer()))

      assert difference(open_map(), open_map(a: not_set()))
             |> map_delete(:a) == open_map(a: not_set())
    end
  end

  test "map_put" do
    assert map_put(term(), :a, integer()) == :badmap
    assert map_put(integer(), :a, integer()) == :badmap
    assert map_put(dynamic(integer()), :a, atom()) == :badmap
    assert map_put(union(integer(), dynamic()), :a, atom()) == :badmap
    assert map_put(empty_map(), :a, integer()) == closed_map(a: integer())

    # Replace an existing key in a closed map
    assert map_put(closed_map(a: integer()), :a, atom()) == closed_map(a: atom())

    # Add a new key to a closed map
    assert map_put(closed_map(a: integer()), :b, atom()) == closed_map(a: integer(), b: atom())

    # Replace an existing key in an open map
    assert map_put(open_map(a: integer()), :a, atom()) == open_map(a: atom())

    # Add a new key to an open map
    assert map_put(open_map(a: integer()), :b, atom()) == open_map(a: integer(), b: atom())

    # Put a key-value pair in a union of maps
    assert union(closed_map(a: integer()), closed_map(b: atom()))
           |> map_put(:c, boolean())
           |> equal?(
             union(closed_map(a: integer(), c: boolean()), closed_map(b: atom(), c: boolean()))
           )

    # Put a key-value pair in a dynamic map
    assert map_put(dynamic(open_map()), :a, integer()) == dynamic(open_map(a: integer()))

    # Put a key-value pair in an intersection of maps
    assert intersection(open_map(a: integer()), open_map(b: atom()))
           |> map_put(:c, boolean())
           |> equal?(open_map(a: integer(), b: atom(), c: boolean()))

    # Put a key-value pair in a difference of maps
    assert difference(open_map(), closed_map(a: integer()))
           |> map_put(:b, atom())
           |> equal?(difference(open_map(b: atom()), closed_map(a: integer())))

    # Put a new key-value pair with dynamic type
    # Note: setting a field to a dynamic type makes the whole map become dynamic.
    assert map_put(open_map(), :a, dynamic()) == dynamic(open_map(a: term()))

    # Put a key-value pair in a map with optional fields
    assert map_put(closed_map(a: if_set(integer())), :b, atom())
           |> equal?(closed_map(a: if_set(integer()), b: atom()))

    # Fetching on a key-value pair that was put to a given type returns {false, type}
    {false, type} = union(dynamic(), empty_map()) |> map_put(:a, atom()) |> map_fetch(:a)
    assert equal?(type, atom())
  end

  describe "disjoint" do
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
    end

    test "negation" do
      assert negation(negation(integer())) |> to_quoted_string() == "integer()"
      assert negation(negation(atom([:foo, :bar]))) |> to_quoted_string() == ":bar or :foo"

      assert negation(negation(list(term()))) |> to_quoted_string() ==
               "empty_list() or non_empty_list(term())"
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
      assert atom([true, false, :a]) |> to_quoted_string() == "boolean() or :a"
      assert atom([true, :a]) |> to_quoted_string() == ":a or true"
      assert difference(atom(), boolean()) |> to_quoted_string() == "atom() and not boolean()"
    end

    test "dynamic" do
      assert dynamic() |> to_quoted_string() == "dynamic()"
      assert intersection(binary(), dynamic()) |> to_quoted_string() == "binary()"

      assert intersection(union(binary(), pid()), dynamic()) |> to_quoted_string() ==
               "dynamic(binary() or pid())"

      assert intersection(atom(), dynamic()) |> to_quoted_string() == "dynamic(atom())"

      assert union(atom([:foo, :bar]), dynamic()) |> to_quoted_string() ==
               "dynamic() or (:bar or :foo)"

      assert intersection(dynamic(), closed_map(a: integer())) |> to_quoted_string() ==
               "dynamic(%{a: integer()})"
    end

    test "lists" do
      assert list(term()) |> to_quoted_string() == "empty_list() or non_empty_list(term())"
      assert list(integer()) |> to_quoted_string() == "empty_list() or non_empty_list(integer())"

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
    end

    test "tuples" do
      assert tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom()}"

      assert tuple([integer(), dynamic(atom())]) |> to_quoted_string() ==
               "dynamic({integer(), atom()})"

      assert open_tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom(), ...}"

      assert union(tuple([integer(), atom()]), open_tuple([atom()])) |> to_quoted_string() ==
               "{integer(), atom()} or {atom(), ...}"

      assert difference(tuple([integer(), atom()]), open_tuple([atom()])) |> to_quoted_string() ==
               "{integer(), atom()} and not {atom(), ...}"

      assert tuple([closed_map(a: integer()), open_map()]) |> to_quoted_string() ==
               "{%{a: integer()}, %{...}}"
    end

    test "map" do
      assert empty_map() |> to_quoted_string() == "empty_map()"
      assert open_map() |> to_quoted_string() == "%{...}"

      assert closed_map(a: integer()) |> to_quoted_string() == "%{a: integer()}"
      assert open_map(a: float()) |> to_quoted_string() == "%{..., a: float()}"

      assert closed_map("Elixir.Foo.Bar": integer()) |> to_quoted_string() ==
               "%{Foo.Bar => integer()}"

      assert open_map("Elixir.Foo.Bar": float()) |> to_quoted_string() ==
               "%{..., Foo.Bar => float()}"

      # TODO: support this simplification
      # assert difference(open_map(), open_map(a: term())) |> to_quoted_string() ==
      #          "%{..., a: not_set()}"

      assert closed_map(a: integer(), b: atom()) |> to_quoted_string() ==
               "%{a: integer(), b: atom()}"

      assert open_map(a: float())
             |> difference(closed_map(a: float()))
             |> to_quoted_string() == "%{..., a: float()} and not %{a: float()}"

      assert difference(open_map(), empty_map()) |> to_quoted_string() ==
               "%{...} and not empty_map()"

      assert closed_map(foo: union(integer(), not_set())) |> to_quoted_string() ==
               "%{foo: if_set(integer())}"

      assert difference(open_map(a: integer()), closed_map(b: boolean())) |> to_quoted_string() ==
               "%{..., a: integer()}"

      assert open_map(a: integer(), b: atom())
             |> difference(open_map(b: atom()))
             |> union(open_map(a: integer()))
             |> to_quoted_string() == "%{..., a: integer()}"
    end

    test "structs" do
      assert open_map(__struct__: atom([URI])) |> to_quoted_string() == "%{..., __struct__: URI}"

      assert closed_map(__struct__: atom([URI])) |> to_quoted_string() ==
               "%URI{}"

      assert closed_map(__struct__: atom([URI]), path: atom([nil])) |> to_quoted_string() ==
               "%URI{path: nil}"

      assert closed_map(__struct__: atom([URI, Another])) |> to_quoted_string() ==
               "%{__struct__: Another or URI}"
    end
  end
end
