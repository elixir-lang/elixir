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
        non_empty_list(),
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
    defp tuple_of_size(n) when is_integer(n), do: tuple(List.duplicate(term(), n))

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

    test "tuple_fetch" do
      assert tuple_fetch(term(), 0) == :badtuple
      assert tuple_fetch(integer(), 0) == :badtuple
      assert tuple_fetch(tuple([integer(), atom()]), 0) == {false, integer()}
      assert tuple_fetch(tuple([integer(), atom()]), 1) == {false, atom()}
      assert tuple_fetch(tuple([integer(), atom()]), 2) == :badindex
      assert tuple_fetch(tuple([integer(), atom()]), -1) == :badindex
      assert tuple_fetch(empty_tuple(), 0) == :badindex
      assert difference(tuple(), tuple()) |> tuple_fetch(0) == :badindex
      assert tuple([atom()]) |> difference(empty_tuple()) |> tuple_fetch(0) == {false, atom()}

      assert difference(tuple([union(integer(), atom())]), open_tuple([atom()]))
             |> tuple_fetch(0) == {false, integer()}

      assert tuple_fetch(union(tuple([integer(), atom()]), dynamic(open_tuple([atom()]))), 1)
             |> Kernel.then(fn {opt, ty} -> opt and equal?(ty, union(atom(), dynamic())) end)

      assert tuple_fetch(union(tuple([integer()]), tuple([atom()])), 0) ==
               {false, union(integer(), atom())}

      assert tuple([integer(), atom(), union(atom(), integer())])
             |> difference(tuple([integer(), term(), atom()]))
             |> tuple_fetch(2) == {false, integer()}

      assert tuple([integer(), atom(), union(union(atom(), integer()), list())])
             |> difference(tuple([integer(), term(), atom()]))
             |> difference(open_tuple([term(), atom(), list()]))
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

  test "map_update" do
    assert map_update(empty_map(), :a, term()) == :badkey
    assert map_update(open_map(), :a, term()) == :badkey
    assert map_update(term(), :a, term()) == :badmap
    assert map_update(closed_map(a: integer()), :b, atom()) == :badkey
    assert map_update(open_map(a: integer()), :b, float()) == :badkey
    assert map_update(closed_map(a: if_set(integer())), :b, atom()) == :badkey
    assert map_update(union(dynamic(), empty_map()), :a, atom()) == :badkey
    assert map_update(closed_map(a: integer()), :a, atom()) == closed_map(a: atom())
    assert map_update(open_map(a: integer()), :a, atom()) |> equal?(open_map(a: atom()))
    assert map_update(dynamic(open_map()), :a, integer()) == dynamic(open_map(a: integer()))

    assert closed_map(a: if_set(atom()), b: float())
           |> union(open_map(a: atom()))
           |> map_update(:a, integer()) ==
             :badkey

    assert closed_map(a: if_set(atom()), b: float())
           |> union(open_map(a: atom()))
           |> difference(open_map(a: if_set(float())))
           |> map_update(:a, integer())
           |> map_fetch(:a) == {false, integer()}

    assert map_update(difference(open_map(), open_map(a: not_set())), :a, fun()) ==
             open_map(a: fun())

    # Update a key-value pair with dynamic type
    # Note: setting a field to a dynamic type makes the whole map become dynamic.
    assert map_update(open_map(a: atom()), :a, dynamic()) |> equal?(dynamic(open_map(a: term())))
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

    test "tuples" do
      assert tuple([integer(), atom()]) |> to_quoted_string() == "{integer(), atom()}"
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
