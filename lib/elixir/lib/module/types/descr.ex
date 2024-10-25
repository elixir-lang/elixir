defmodule Module.Types.Descr do
  @moduledoc false

  # The descr contains a set-theoretic implementation of types.
  # Types are represented as maps of non-overlapping unions.
  # A bitmap is used to represent non-divisible types. All other
  # types require specific data structures.

  # Vocabulary:
  #
  # * DNF - disjunctive normal form which is a pair of unions and negations.
  #   In the case of maps, we augment each pair with the open/closed tag.

  import Bitwise

  @bit_binary 1 <<< 0
  @bit_empty_list 1 <<< 1
  @bit_integer 1 <<< 2
  @bit_float 1 <<< 3
  @bit_pid 1 <<< 4
  @bit_port 1 <<< 5
  @bit_reference 1 <<< 6

  @bit_fun 1 <<< 7
  @bit_top (1 <<< 8) - 1

  @bit_number @bit_integer ||| @bit_float
  @bit_optional 1 <<< 8

  @atom_top {:negation, :sets.new(version: 2)}
  @tuple_top [{:open, [], []}]
  @map_top [{:open, %{}, []}]
  @map_empty [{:closed, %{}, []}]
  @none %{}
  @empty_list %{bitmap: @bit_empty_list}
  # A definition of non empty list that does not use negation. Includes empty list.
  @not_non_empty_list %{bitmap: @bit_top, atom: @atom_top, tuple: @tuple_top, map: @map_top}
  @non_empty_list_top [{:term, @not_non_empty_list, []}]

  # Type definitions

  defguard is_descr(descr) when is_map(descr) or descr == :term

  def dynamic(), do: %{dynamic: :term}
  def none(), do: @none
  def term(), do: :term

  defp unfold(:term), do: unfolded_term()
  defp unfold(other), do: other

  defp unfolded_term,
    do: %{
      bitmap: @bit_top,
      atom: @atom_top,
      tuple: @tuple_top,
      map: @map_top,
      list: @non_empty_list_top
    }

  def atom(as), do: %{atom: atom_new(as)}
  def atom(), do: %{atom: @atom_top}
  def binary(), do: %{bitmap: @bit_binary}
  def closed_map(pairs), do: map_descr(:closed, pairs)
  def empty_list(), do: %{bitmap: @bit_empty_list}
  def empty_map(), do: %{map: @map_empty}
  def integer(), do: %{bitmap: @bit_integer}
  def float(), do: %{bitmap: @bit_float}
  def fun(), do: %{bitmap: @bit_fun}
  def list(type, tail \\ @empty_list), do: list_descr(type, tail, true)
  def non_empty_list(type, tail \\ @empty_list), do: list_descr(type, tail, false)
  def open_map(), do: %{map: @map_top}
  def open_map(pairs), do: map_descr(:open, pairs)
  def open_tuple(elements, _fallback \\ term()), do: tuple_descr(:open, elements)
  def pid(), do: %{bitmap: @bit_pid}
  def port(), do: %{bitmap: @bit_port}
  def reference(), do: %{bitmap: @bit_reference}
  def tuple(), do: %{tuple: @tuple_top}
  def tuple(elements), do: tuple_descr(:closed, elements)

  @boolset :sets.from_list([true, false], version: 2)
  def boolean(), do: %{atom: {:union, @boolset}}

  # Map helpers
  #
  # `not_set()` is a special base type that represents an not_set field in a map.
  # E.g., `%{a: integer(), b: not_set(), ...}` represents a map with an integer
  # field `a` and an not_set field `b`, and possibly other fields.
  #
  # The `if_set()` modifier is syntactic sugar for specifying the key as a union
  # of the key type and `not_set()`. For example, `%{:foo => if_set(integer())}`
  # is equivalent to `%{:foo => integer() or not_set()}`.
  #
  # `not_set()` has no meaning outside of map types.

  @not_set %{bitmap: @bit_optional}
  @term_or_optional %{
    bitmap: @bit_top ||| @bit_optional,
    atom: @atom_top,
    tuple: @tuple_top,
    map: @map_top,
    list: @non_empty_list_top
  }

  def not_set(), do: @not_set
  defp term_or_optional(), do: @term_or_optional

  def if_set(:term), do: term_or_optional()
  def if_set(type), do: Map.update(type, :bitmap, @bit_optional, &(&1 ||| @bit_optional))

  defguardp is_optional(map)
            when is_map(map) and
                   ((is_map_key(map, :bitmap) and (map.bitmap &&& @bit_optional) != 0) or
                      (is_map_key(map, :dynamic) and is_map(map.dynamic) and
                         is_map_key(map.dynamic, :bitmap) and
                         (map.dynamic.bitmap &&& @bit_optional) != 0))

  defguardp is_optional_static(map)
            when is_map(map) and is_map_key(map, :bitmap) and (map.bitmap &&& @bit_optional) != 0

  defp descr_key?(:term, _key), do: true
  defp descr_key?(descr, key), do: is_map_key(descr, key)

  ## Set operations

  def term_type?(:term), do: true
  def term_type?(descr), do: subtype_static(unfolded_term(), Map.delete(descr, :dynamic))

  def dynamic_term_type?(descr), do: descr == %{dynamic: :term}

  def gradual?(:term), do: false
  def gradual?(descr), do: is_map_key(descr, :dynamic)

  @doc """
  Make a whole type dynamic.

  It is an optimized version of `intersection(dynamic(), type)`.
  """
  def dynamic(descr) do
    case descr do
      %{dynamic: dynamic} -> %{dynamic: dynamic}
      _ -> %{dynamic: descr}
    end
  end

  @doc """
  Computes the union of two descrs.
  """
  def union(:term, other) when not is_optional(other), do: :term
  def union(other, :term) when not is_optional(other), do: :term
  def union(none, other) when none == %{}, do: other
  def union(other, none) when none == %{}, do: other

  def union(left, right) do
    left = unfold(left)
    right = unfold(right)
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(right, :dynamic, right)
        symmetrical_merge(left, right_with_dynamic, &union/3)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(left, :dynamic, left)
        symmetrical_merge(left_with_dynamic, right, &union/3)

      true ->
        symmetrical_merge(left, right, &union/3)
    end
  end

  @compile {:inline, union: 3}
  defp union(:bitmap, v1, v2), do: bitmap_union(v1, v2)
  defp union(:atom, v1, v2), do: atom_union(v1, v2)
  defp union(:dynamic, v1, v2), do: dynamic_union(v1, v2)
  defp union(:map, v1, v2), do: map_union(v1, v2)
  defp union(:tuple, v1, v2), do: tuple_union(v1, v2)
  defp union(:list, v1, v2), do: list_union(v1, v2)

  @doc """
  Computes the intersection of two descrs.
  """
  def intersection(:term, other) when not is_optional(other), do: other
  def intersection(other, :term) when not is_optional(other), do: other
  def intersection(%{dynamic: :term}, other) when not is_optional(other), do: dynamic(other)
  def intersection(other, %{dynamic: :term}) when not is_optional(other), do: dynamic(other)

  def intersection(left, right) do
    left = unfold(left)
    right = unfold(right)
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(right, :dynamic, right)
        symmetrical_intersection(left, right_with_dynamic, &intersection/3)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(left, :dynamic, left)
        symmetrical_intersection(left_with_dynamic, right, &intersection/3)

      true ->
        symmetrical_intersection(left, right, &intersection/3)
    end
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, intersection: 3}
  defp intersection(:bitmap, v1, v2), do: bitmap_intersection(v1, v2)
  defp intersection(:atom, v1, v2), do: atom_intersection(v1, v2)
  defp intersection(:dynamic, v1, v2), do: dynamic_intersection(v1, v2)
  defp intersection(:map, v1, v2), do: map_intersection(v1, v2)
  defp intersection(:tuple, v1, v2), do: tuple_intersection(v1, v2)
  defp intersection(:list, v1, v2), do: list_intersection(v1, v2)

  @doc """
  Computes the difference between two types.
  """
  def difference(other, :term) when not is_optional(other), do: none()

  def difference(left, right) do
    left = unfold(left)
    right = unfold(right)

    if gradual?(left) or gradual?(right) do
      {left_dynamic, left_static} = Map.pop(left, :dynamic, left)
      {right_dynamic, right_static} = Map.pop(right, :dynamic, right)
      dynamic_part = difference_static(left_dynamic, right_static)

      if empty?(dynamic_part),
        do: none(),
        else: Map.put(difference_static(left_static, right_dynamic), :dynamic, dynamic_part)
    else
      difference_static(left, right)
    end
  end

  # For static types, the difference is component-wise.
  defp difference_static(left, :term) when not is_optional_static(left), do: none()

  defp difference_static(left, right) do
    iterator_difference(:maps.next(:maps.iterator(unfold(right))), unfold(left))
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, difference: 3}
  defp difference(:bitmap, v1, v2), do: bitmap_difference(v1, v2)
  defp difference(:atom, v1, v2), do: atom_difference(v1, v2)
  defp difference(:dynamic, v1, v2), do: dynamic_difference(v1, v2)
  defp difference(:map, v1, v2), do: map_difference(v1, v2)
  defp difference(:tuple, v1, v2), do: tuple_difference(v1, v2)
  defp difference(:list, v1, v2), do: list_difference(v1, v2)

  @doc """
  Compute the negation of a type.
  """
  def negation(:term), do: none()
  def negation(%{} = descr), do: difference(unfolded_term(), descr)

  @doc """
  Check if a type is empty.

  For gradual types, check that the upper bound (the dynamic part) is empty.
  Stop as soon as one non-empty component is found. Simpler components
  (bitmap, atom) are checked first for speed since, if they are present,
  the type is non-empty as we normalize then during construction.
  """
  def empty?(:term), do: false

  def empty?(%{} = descr) do
    case Map.get(descr, :dynamic, descr) do
      :term ->
        false

      value when value == @none ->
        true

      descr ->
        not Map.has_key?(descr, :bitmap) and not Map.has_key?(descr, :atom) and
          (not Map.has_key?(descr, :tuple) or tuple_empty?(descr.tuple)) and
          (not Map.has_key?(descr, :map) or map_empty?(descr.map)) and
          (not Map.has_key?(descr, :list) or list_empty?(descr.list))
    end
  end

  @doc """
  Converts a descr to its quoted representation.
  """
  def to_quoted(descr) do
    if term_type?(descr) do
      {:term, [], []}
    else
      case Enum.flat_map(descr, fn {key, value} -> to_quoted(key, value) end) do
        [] -> {:none, [], []}
        unions -> unions |> Enum.sort() |> Enum.reduce(&{:or, [], [&2, &1]})
      end
    end
  end

  @compile {:inline, to_quoted: 2}
  defp to_quoted(:bitmap, val), do: bitmap_to_quoted(val)
  defp to_quoted(:atom, val), do: atom_to_quoted(val)
  defp to_quoted(:dynamic, descr), do: dynamic_to_quoted(descr)
  defp to_quoted(:map, dnf), do: map_to_quoted(dnf)
  defp to_quoted(:tuple, dnf), do: tuple_to_quoted(dnf)
  defp to_quoted(:list, dnf), do: list_to_quoted(dnf)

  @doc """
  Converts a descr to its quoted string representation.
  """
  def to_quoted_string(descr) do
    descr
    |> to_quoted()
    |> Code.Formatter.to_algebra()
    |> Inspect.Algebra.format(98)
    |> IO.iodata_to_binary()
  end

  ## Type relations

  @doc """
  Check if a type is a subtype of another.

  If     `left  = (left_dyn  and dynamic()) or left_static`
  and    `right = (right_dyn and dynamic()) or right_static`

  then the gradual subtyping relation defined in Definition 6.5 page 125 of
  the thesis https://vlanvin.fr/papers/thesis.pdf is:

  `left <= right` if and only if
    - `left_static <= right_static`
    - `left_dyn <= right_dyn`

  Because of the dynamic/static invariant in the `descr`, subtyping can be
  simplified in several cases according to which type is gradual or not.
  """
  def subtype?(left, :term) when not is_optional(left), do: true

  def subtype?(left, right) do
    left = unfold(left)
    right = unfold(right)
    is_grad_left = gradual?(left)
    is_grad_right = gradual?(right)

    cond do
      is_grad_left and not is_grad_right ->
        left_dynamic = Map.get(left, :dynamic)
        subtype_static(left_dynamic, right)

      is_grad_right and not is_grad_left ->
        right_static = Map.delete(right, :dynamic)
        subtype_static(left, right_static)

      true ->
        subtype_static(left, right)
    end
  end

  defp subtype_static(same, same), do: true
  defp subtype_static(left, right), do: empty?(difference_static(left, right))

  @doc """
  Check if a type is equal to another.

  It is currently not optimized. Only to be used in tests.
  """
  def equal?(left, right) do
    left == right or (subtype?(left, right) and subtype?(right, left))
  end

  @doc """
  Check if two types are disjoint.
  """
  def disjoint?(left, right), do: empty?(intersection(left, right))

  @doc """
  Checks if a type is a compatible subtype of another.

  If `input_type` has a static part (i.e., values that are known to appear and
  need to be handled), then to be compatible it should be a subtype of the
  the dynamic part of `expected_type` (that is, the largest allowed type at
  runtime).

  If `input_type` is a dynamic type, then we check that the two can intersect
  at runtime, i.e. it is possible to get valid inputs at runtime.

  The function is used, in gradual mode, to type an operator that expects a given
  type. For instance, `+` expects `integer() or float()` inputs. Compatible inputs
  include `dynamic()`, `integer()`, but also `dynamic() and (integer() or atom())`.
  Incompatible subtypes include `integer() or list()`, `dynamic() and atom()`.
  """
  def compatible?(left, :term) when not is_optional(left), do: true

  def compatible?(left, right) do
    left = unfold(left)
    right = unfold(right)
    {left_dynamic, left_static} = Map.pop(left, :dynamic, left)
    right_dynamic = Map.get(right, :dynamic, right)

    if empty?(left_static) do
      not disjoint?(left_dynamic, right_dynamic)
    else
      subtype_static(left_static, right_dynamic)
    end
  end

  ## Bitmaps

  @doc """
  Optimized version of `not empty?(intersection(empty_list(), type))`.
  """
  def empty_list_type?(:term), do: true
  def empty_list_type?(%{dynamic: :term}), do: true

  def empty_list_type?(%{dynamic: %{bitmap: bitmap}}) when (bitmap &&& @bit_empty_list) != 0,
    do: true

  def empty_list_type?(%{bitmap: bitmap}) when (bitmap &&& @bit_empty_list) != 0, do: true
  def empty_list_type?(_), do: false

  @doc """
  Optimized version of `not empty?(intersection(binary(), type))`.
  """
  def binary_type?(:term), do: true
  def binary_type?(%{dynamic: :term}), do: true
  def binary_type?(%{dynamic: %{bitmap: bitmap}}) when (bitmap &&& @bit_binary) != 0, do: true
  def binary_type?(%{bitmap: bitmap}) when (bitmap &&& @bit_binary) != 0, do: true
  def binary_type?(_), do: false

  @doc """
  Optimized version of `not empty?(intersection(integer(), type))`.
  """
  def integer_type?(:term), do: true
  def integer_type?(%{dynamic: :term}), do: true
  def integer_type?(%{dynamic: %{bitmap: bitmap}}) when (bitmap &&& @bit_integer) != 0, do: true
  def integer_type?(%{bitmap: bitmap}) when (bitmap &&& @bit_integer) != 0, do: true
  def integer_type?(_), do: false

  @doc """
  Optimized version of `not empty?(intersection(float(), type))`.
  """
  def float_type?(:term), do: true
  def float_type?(%{dynamic: :term}), do: true
  def float_type?(%{dynamic: %{bitmap: bitmap}}) when (bitmap &&& @bit_float) != 0, do: true
  def float_type?(%{bitmap: bitmap}) when (bitmap &&& @bit_float) != 0, do: true
  def float_type?(_), do: false

  @doc """
  Optimized version of `not empty?(intersection(integer() or float(), type))`.
  """
  def number_type?(:term), do: true
  def number_type?(%{dynamic: :term}), do: true
  def number_type?(%{dynamic: %{bitmap: bitmap}}) when (bitmap &&& @bit_number) != 0, do: true
  def number_type?(%{bitmap: bitmap}) when (bitmap &&& @bit_number) != 0, do: true
  def number_type?(_), do: false

  @doc """
  Optimized version of `not empty?(intersection(list(), type))`.
  """
  def list_type?(:term), do: true
  def list_type?(%{dynamic: :term}), do: true
  def list_type?(%{dynamic: %{list: _}}), do: true
  def list_type?(%{list: _}), do: true
  def list_type?(_), do: false

  defp bitmap_union(v1, v2), do: v1 ||| v2
  defp bitmap_intersection(v1, v2), do: v1 &&& v2
  defp bitmap_difference(v1, v2), do: v1 - (v1 &&& v2)

  defp bitmap_to_quoted(val) do
    pairs =
      [
        binary: @bit_binary,
        empty_list: @bit_empty_list,
        integer: @bit_integer,
        float: @bit_float,
        pid: @bit_pid,
        port: @bit_port,
        reference: @bit_reference,
        fun: @bit_fun
      ]

    for {type, mask} <- pairs,
        (mask &&& val) !== 0,
        do: {type, [], []}
  end

  ## Funs

  @doc """
  Checks there is a function type (and only functions) with said arity.
  """
  def fun_fetch(:term, _arity), do: :error

  def fun_fetch(%{} = descr, _arity) do
    {static_or_dynamic, static} = Map.pop(descr, :dynamic, descr)

    if fun_only?(static) do
      case static_or_dynamic do
        :term -> :ok
        %{bitmap: bitmap} when (bitmap &&& @bit_fun) != 0 -> :ok
        %{} -> :error
      end
    else
      :error
    end
  end

  defp fun_only?(descr), do: empty?(difference(descr, fun()))

  ## Atoms

  # The atom component of a type consists of pairs `{tag, set}` where `set` is a
  # set of atoms.
  # If `tag = :union` the pair represents the union of the atoms in `set`.
  # Else, if `tag = :negation`, it represents every atom except those in `set`.
  #
  # Example:
  #   - `{:union, :sets.from_list([:a, :b])}` represents type `:a or :b`
  #   - `{:negation, :sets.from_list([:c, :d])}` represents type `atom() \ (:c or :d)
  #
  # `{:negation, :sets.new()}` is the `atom()` top type, as it is the difference
  # of `atom()` with an empty list.
  #
  # `{:union, :sets.new()}` is the empty type for atoms, as it is the union of
  # an empty list of atoms. It is simplified to `0` in set operations, and the key
  # is removed from the map.

  @doc """
  Optimized version of `not empty?(intersection(atom([atom]), type))`.
  """
  def atom_type?(:term, _atom), do: true

  def atom_type?(%{} = descr, atom) do
    case Map.get(descr, :dynamic, descr) do
      :term -> true
      %{atom: {:union, set}} -> :sets.is_element(atom, set)
      %{atom: {:negation, set}} -> not :sets.is_element(atom, set)
      %{} -> false
    end
  end

  @doc """
  Returns a set of all known atoms.

  Returns `{:finite or :infinite, known_set}` if it is an atom,
  `:error` otherwise. Notice `known_set` may be empty in infinite
  cases, due to negations.
  """
  def atom_fetch(:term), do: :error

  def atom_fetch(%{} = descr) do
    {static_or_dynamic, static} = Map.pop(descr, :dynamic, descr)

    if atom_only?(static) do
      case static_or_dynamic do
        :term -> {:infinite, []}
        %{atom: {:union, set}} -> {:finite, :sets.to_list(set)}
        %{atom: {:negation, _}} -> {:infinite, []}
        %{} -> :error
      end
    else
      :error
    end
  end

  defp atom_only?(descr), do: empty?(Map.delete(descr, :atom))
  defp atom_new(as) when is_list(as), do: {:union, :sets.from_list(as, version: 2)}

  defp atom_intersection({tag1, s1}, {tag2, s2}) do
    {tag, s} =
      case {tag1, tag2} do
        {:union, :union} -> {:union, :sets.intersection(s1, s2)}
        {:negation, :negation} -> {:negation, :sets.union(s1, s2)}
        {:union, :negation} -> {:union, :sets.subtract(s1, s2)}
        {:negation, :union} -> {:union, :sets.subtract(s2, s1)}
      end

    if tag == :union and :sets.size(s) == 0, do: 0, else: {tag, s}
  end

  defp atom_union({:union, s1}, {:union, s2}), do: {:union, :sets.union(s1, s2)}
  defp atom_union({:negation, s1}, {:negation, s2}), do: {:negation, :sets.intersection(s1, s2)}
  defp atom_union({:union, s1}, {:negation, s2}), do: {:negation, :sets.subtract(s2, s1)}
  defp atom_union({:negation, s1}, {:union, s2}), do: {:negation, :sets.subtract(s1, s2)}

  defp atom_difference({tag1, s1}, {tag2, s2}) do
    {tag, s} =
      case {tag1, tag2} do
        {:union, :union} -> {:union, :sets.subtract(s1, s2)}
        {:negation, :negation} -> {:union, :sets.subtract(s2, s1)}
        {:union, :negation} -> {:union, :sets.intersection(s1, s2)}
        {:negation, :union} -> {:negation, :sets.union(s1, s2)}
      end

    if tag == :union and :sets.size(s) == 0, do: 0, else: {tag, s}
  end

  defp literal_to_quoted(lit) do
    if is_atom(lit) and Macro.classify_atom(lit) == :alias do
      segments =
        case Atom.to_string(lit) do
          "Elixir" ->
            [:"Elixir"]

          "Elixir." <> segments ->
            segments
            |> String.split(".")
            |> Enum.map(&String.to_atom/1)
        end

      {:__aliases__, [], segments}
    else
      {:__block__, [], [lit]}
    end
  end

  defp atom_to_quoted({:union, a}) do
    if :sets.is_subset(@boolset, a) do
      :sets.subtract(a, @boolset)
      |> :sets.to_list()
      |> Enum.sort()
      |> Enum.reduce({:boolean, [], []}, &{:or, [], [&2, literal_to_quoted(&1)]})
    else
      :sets.to_list(a)
      |> Enum.sort()
      |> Enum.map(&literal_to_quoted/1)
      |> Enum.reduce(&{:or, [], [&2, &1]})
    end
    |> List.wrap()
  end

  defp atom_to_quoted({:negation, a}) do
    if :sets.size(a) == 0 do
      {:atom, [], []}
    else
      atom_to_quoted({:union, a})
      |> Kernel.then(&{:and, [], [{:atom, [], []}, {:not, [], &1}]})
    end
    |> List.wrap()
  end

  ## List

  # Represents both list and improper list simultaneously using a pair {list_type, last_type}.
  #
  # For proper lists, the last_type is empty_list().
  # In general, list(term(), term()) is interpreted as {term(), term()}
  # and not non_empty_list(term(), term()).
  #
  # Note: A type being none() is handled separately.
  defp list_descr(list_type, last_type, empty?) do
    {list_dynamic?, list_type} = list_pop_dynamic(list_type)
    {last_dynamic?, last_type} = list_pop_dynamic(last_type)

    list_part =
      if last_type == :term do
        list_new(term(), term())
      else
        case :maps.take(:list, last_type) do
          :error ->
            list_new(list_type, last_type)

          {dnf, last_type} ->
            {list_type, last_type} =
              Enum.reduce(dnf, {list_type, last_type}, fn {head, tail, _}, {acc_head, acc_tail} ->
                {union(head, acc_head), union(tail, acc_tail)}
              end)

            list_new(list_type, last_type)
        end
      end

    list_descr =
      if empty?, do: %{list: list_part, bitmap: @bit_empty_list}, else: %{list: list_part}

    case list_dynamic? or last_dynamic? do
      true -> %{dynamic: list_descr}
      false -> list_descr
    end
  end

  defp list_new(list_type, last_type) do
    [{list_type, last_type, []}]
  end

  defp list_pop_dynamic(:term), do: {false, :term}

  defp list_pop_dynamic(descr) do
    case :maps.take(:dynamic, descr) do
      :error -> {false, descr}
      {dynamic, _} -> {true, dynamic}
    end
  end

  defp list_tail_unfold(:term), do: @not_non_empty_list
  defp list_tail_unfold(other), do: Map.delete(other, :list)

  defp list_union(dnf1, dnf2), do: dnf1 ++ dnf2

  defp list_intersection(dnf1, dnf2) do
    for {list_type1, last_type1, negs1} <- dnf1,
        {list_type2, last_type2, negs2} <- dnf2,
        reduce: [] do
      acc ->
        inter = intersection(list_type1, list_type2)
        last = intersection(last_type1, last_type2)

        if empty?(inter) or empty?(last) do
          acc
        else
          [{inter, last, negs1 ++ negs2} | acc]
        end
    end
    |> case do
      [] -> 0
      dnf -> dnf
    end
  end

  # Computes the difference between two DNF (Disjunctive Normal Form) list types.
  # It progressively subtracts each type in dnf2 from all types in dnf1.
  # The algorithm handles three cases:
  # 1. Disjoint types: keeps the original type from dnf1
  # 2. Subtype relationship:
  #    a) If dnf2 type is a supertype, keeps only the negations
  #    b) If only the last type differs, subtracts it
  # 3. Base case: adds dnf2 type to negations of dnf1 type
  # The result may be larger than the initial dnf1, which is maintained in the accumulator.
  defp list_difference(dnf1, dnf2) do
    Enum.reduce(dnf2, dnf1, fn {t2, last2, negs2}, acc_dnf1 ->
      last2 = list_tail_unfold(last2)

      Enum.flat_map(acc_dnf1, fn {t1, last1, negs1} ->
        last1 = list_tail_unfold(last1)

        i = intersection(t1, t2)
        l = intersection(last1, last2)

        new_negs =
          Enum.reduce(negs2, [], fn {nt, nlast}, nacc ->
            t = intersection(t1, nt)
            last = intersection(last1, nlast)
            if empty?(t) or empty?(last), do: nacc, else: [{t, last, negs1} | nacc]
          end)

        cond do
          empty?(i) or empty?(l) -> [{t1, last1, negs1}]
          subtype?(t1, t2) and subtype?(last1, last2) -> new_negs
          subtype?(t1, t2) -> [{t1, difference(last1, last2), negs1} | new_negs]
          true -> [{t1, last1, [{t2, last2} | negs1]} | new_negs]
        end
      end)
    end)
  end

  defp list_empty?(dnf) do
    Enum.all?(dnf, fn {list_type, last_type, negs} ->
      last_type = list_tail_unfold(last_type)

      empty?(list_type) or empty?(last_type) or
        Enum.reduce_while(negs, last_type, fn {neg_type, neg_last}, acc_last_type ->
          if subtype?(list_type, neg_type) and subtype?(acc_last_type, neg_last) do
            {:halt, nil}
          else
            {:cont, difference(acc_last_type, neg_last)}
          end
        end)
        |> is_nil()
    end)
  end

  defp list_only?(descr), do: subtype?(Map.delete(descr, :list), empty_list())

  @doc """
  Returns the head of a list.

  Errors on an empty list.
  On a non empty list of integers, it returns the first integer.
  On a non empty list of integers, with an atom head element, it returns the atom.
  """
  def list_hd(:term), do: :badnonemptylist

  def list_hd(%{} = descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        has_empty = empty_list_type?(descr)
        is_list_type = list_only?(descr)

        if is_list_type and not has_empty do
          {false, list_hd_static(descr)}
        else
          :badnonemptylist
        end

      {dynamic, static} ->
        has_empty = empty_list_type?(static)
        only_list = list_only?(static)
        is_dynamic_list = list_type?(dynamic)

        if is_dynamic_list and only_list and not has_empty do
          {is_dynamic_list, union(dynamic(list_hd_static(dynamic)), list_hd_static(static))}
        else
          :badnonemptylist
        end
    end
  end

  defp list_hd_static(:term), do: :term

  defp list_hd_static(descr) do
    case descr do
      %{list: [{type, _last, _negs}]} ->
        type

      %{list: dnf} ->
        Enum.reduce(dnf, none(), fn {type, _, _}, acc -> union(type, acc) end)

      %{} ->
        none()
    end
  end

  @doc """
  Returns the tail of a list.

  Errors on a possibly empty list.
  On a non empty list of integers, it returns a (possibly empty) list of integers.
  On a non empty list of integers, with an atom tail element, it returns either an atom,
  or a (possibly empty) list of integers with an atom tail element.
  """
  def list_tl(:term), do: :badnonemptylist

  def list_tl(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        has_empty = empty_list_type?(descr)
        is_list_type = list_only?(descr)

        if is_list_type and not has_empty do
          {false, list_tl_static(descr)}
        else
          :badnonemptylist
        end

      {dynamic, static} ->
        has_empty = empty_list_type?(static)
        only_list = list_only?(static)
        is_dynamic_list = list_type?(dynamic)

        if is_dynamic_list and only_list and not has_empty do
          {is_dynamic_list, union(dynamic(list_tl_static(dynamic)), list_tl_static(static))}
        else
          :badnonemptylist
        end
    end
  end

  defp list_tl_static(:term), do: :term

  defp list_tl_static(descr) do
    case descr do
      %{list: dnf} ->
        Enum.reduce(dnf, %{list: dnf, bitmap: @bit_empty_list}, fn {_, last, _}, acc ->
          union(last, acc)
        end)

      %{bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0 ->
        empty_list()

      %{} ->
        none()
    end
  end

  defp list_to_quoted(dnf) do
    dnf = list_normalize(dnf)

    for {list_type, last_type, negs} <- dnf, reduce: [] do
      acc ->
        arguments =
          if subtype?(last_type, @empty_list) do
            [to_quoted(list_type)]
          else
            [to_quoted(list_type), to_quoted(last_type)]
          end

        if negs == [] do
          [{:non_empty_list, [], arguments} | acc]
        else
          negs
          |> Enum.map(fn {ty, lst} ->
            args =
              if subtype?(lst, @empty_list) do
                [to_quoted(ty)]
              else
                [to_quoted(ty), to_quoted(lst)]
              end

            {:non_empty_list, [], args}
          end)
          |> Enum.reduce(&{:or, [], [&2, &1]})
          |> Kernel.then(
            &[
              {:and, [],
               [
                 {:non_empty_list, [], arguments},
                 {:not, [], [&1]}
               ]}
              | acc
            ]
          )
        end
    end
  end

  # TODO: Eliminate empty lists from the union
  defp list_normalize(dnf), do: dnf
  #   Enum.filter(dnf, fn {list_type, last_type, negs} ->
  #     not Enum.any?(negs, fn neg -> subtype?(list_type, neg) end)
  #   end)
  # end

  ## Dynamic
  #
  # A type with a dynamic component is a gradual type; without, it is a static
  # type. The dynamic component itself is a static type; hence, any gradual type
  # can be written using a pair of static types as the union:
  #
  # `type = (dynamic_component and dynamic()) or static_component`
  #
  # where the `static_component` is simply the rest of the `descr`, and `dynamic()`
  # is the base type that can represent any value at runtime. The dynamic and
  # static parts can be considered separately for a mixed-typed analysis. For
  # example, the type
  #
  # `type = (dynamic() and integer()) or boolean()`
  #
  # denotes an expression that evaluates to booleans or integers; however, there is
  # uncertainty about the source of these integers. In static mode, the
  # type-checker refuses to apply a function of type `boolean() -> boolean()` to
  # this argument (since the argument may turn out to be an integer()), but in
  # dynamic mode, it considers the type obtained by replacing `dynamic()` with
  # `none()` (that is, `boolean()`), accepts the application, but types it with a
  # type that contains `dynamic()`.
  #
  # When pattern matching on an expression of type `type`, the static part (here,
  # booleans) has to be handled exhaustively. In contrast, the dynamic part can
  # produce potential warnings in specific user-induced conditions, such as asking
  # for stronger enforcement of static types.
  #
  # During construction and through set operations, we maintain the invariant that
  # the dynamic component is a supertype of the static one, formally
  # `dynamic_component >= static_component`
  #
  # With this invariant, the dynamic component always represents every value that
  # a given gradual type can take at runtime, allowing us to simplify set operations,
  # compared, for example, to keeping only the extra dynamic type that can obtained
  # on top of the static type. Though, the latter may be used for printing purposes.
  #
  # There are two ways for a descr to represent a static type: either the
  # `:dynamic` field is not_set, or it contains a type equal to the static component
  # (that is, there are no extra dynamic values).

  defp dynamic_union(:term, other) when not is_optional_static(other), do: :term
  defp dynamic_union(other, :term) when not is_optional_static(other), do: :term

  defp dynamic_union(left, right),
    do: symmetrical_merge(unfold(left), unfold(right), &union/3)

  defp dynamic_intersection(:term, other) when not is_optional_static(other), do: other
  defp dynamic_intersection(other, :term) when not is_optional_static(other), do: other

  defp dynamic_intersection(left, right),
    do: symmetrical_intersection(unfold(left), unfold(right), &intersection/3)

  defp dynamic_difference(left, right), do: difference_static(left, right)

  defp dynamic_to_quoted(descr) do
    cond do
      term_type?(descr) -> [{:dynamic, [], []}]
      single = indivisible_bitmap(descr) -> [single]
      true -> [{:dynamic, [], [to_quoted(descr)]}]
    end
  end

  defp indivisible_bitmap(descr) do
    with %{bitmap: bitmap} when map_size(descr) == 1 <- descr,
         [single] <- bitmap_to_quoted(bitmap) do
      single
    else
      _ -> nil
    end
  end

  ## Map
  #
  # Maps are in disjunctive normal form (DNF), that is, a list (union) of pairs
  # `{map_literal, negated_literals}` where `map_literal` is a map type literal
  # and `negated_literals` is a list of map type literals that are negated from it.
  # Each pair is augmented with the :open or :closed tag.
  #
  # For instance, the type `%{..., a: integer()} and not %{b: atom()}` can be represented
  # by the DNF containing one pair of shape:
  #
  #     {:open, %{a => integer()}, [{:closed, %{b => atom()}}]}
  #
  # The goal of keeping symbolic negations is to avoid distributing difference on
  # every member of a union which creates a lot of map literals in the union and
  # requires emptiness checks to avoid creating empty maps.
  #
  # For instance, the difference between `%{...}` and `%{a: atom(), b: integer()}`
  # is the union of `%{..., a: atom(), b: if_set(not integer())}` and
  # `%{..., a: if_set(not atom()), b: integer()}`. For maps with more keys,
  # each key in a negated literal may create a new union when eliminated.

  defp map_descr(tag, fields) do
    case map_descr_pairs(fields, [], false) do
      {fields, true} ->
        %{dynamic: %{map: map_new(tag, fields |> Enum.reverse() |> :maps.from_list())}}

      {_, false} ->
        %{map: map_new(tag, :maps.from_list(fields))}
    end
  end

  defp map_descr_pairs([{key, :term} | rest], acc, dynamic?) do
    map_descr_pairs(rest, [{key, :term} | acc], dynamic?)
  end

  defp map_descr_pairs([{key, value} | rest], acc, dynamic?) do
    case :maps.take(:dynamic, value) do
      :error -> map_descr_pairs(rest, [{key, value} | acc], dynamic?)
      {dynamic, _static} -> map_descr_pairs(rest, [{key, dynamic} | acc], true)
    end
  end

  defp map_descr_pairs([], acc, dynamic?) do
    {acc, dynamic?}
  end

  defp tag_to_type(:open), do: term_or_optional()
  defp tag_to_type(:closed), do: not_set()

  defp map_new(tag, fields = %{}), do: [{tag, fields, []}]

  @doc """
  Fetches the type of the value returned by accessing `key` on `map`
  with the assumption that the descr is exclusively a map (or dynamic).

  It returns a two element tuple or `:error`. The first element says
  if the type is dynamically optional or not, the second element is
  the type. In static mode, optional keys are not allowed.
  """
  def map_fetch(:term, _key), do: :badmap

  def map_fetch(%{} = descr, key) when is_atom(key) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and map_only?(descr) do
          {static_optional?, static_type} = map_fetch_static(descr, key)

          if static_optional? or empty?(static_type) do
            :badkey
          else
            {false, static_type}
          end
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          {dynamic_optional?, dynamic_type} = map_fetch_static(dynamic, key)
          {static_optional?, static_type} = map_fetch_static(static, key)

          if static_optional? or empty?(dynamic_type) do
            :badkey
          else
            {dynamic_optional?, union(dynamic(dynamic_type), static_type)}
          end
        else
          :badmap
        end
    end
  end

  defp map_only?(descr), do: empty?(Map.delete(descr, :map))

  defp map_fetch_static(:term, _key), do: {true, term()}

  defp map_fetch_static(descr, key) when is_atom(key) do
    case descr do
      # Optimization: if the key does not exist in the map,
      # avoid building if_set/not_set pairs and return the
      # popped value directly.
      %{map: [{tag, fields, []}]} when not is_map_key(fields, key) ->
        case tag do
          :open -> {true, term()}
          :closed -> {true, none()}
        end

      %{map: map} ->
        map_get(map, key) |> pop_optional_static()

      %{} ->
        {false, none()}
    end
  end

  @doc """
  Adds a `key` of a given type, assuming that the descr is exclusively
  a map (or dynamic).
  """
  def map_put(:term, _key, _type), do: :badmap
  def map_put(descr, key, :term) when is_atom(key), do: map_put_static_value(descr, key, :term)

  def map_put(descr, key, type) when is_atom(key) do
    case :maps.take(:dynamic, type) do
      :error -> map_put_static_value(descr, key, type)
      {dynamic, _static} -> dynamic(map_put_static_value(descr, key, dynamic))
    end
  end

  defp map_put_static_value(descr, key, type) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if map_only?(descr) do
          map_put_static_descr(descr, key, type)
        else
          :badmap
        end

      {dynamic, static} when static == @none ->
        if descr_key?(dynamic, :map) do
          dynamic(map_put_static_descr(dynamic, key, type))
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          dynamic = map_put_static_descr(dynamic, key, type)
          static = map_put_static_descr(static, key, type)
          union(dynamic(dynamic), static)
        else
          :badmap
        end
    end
  end

  # Directly inserts a key of a given type into every positive and negative map
  defp map_put_static_descr(descr, key, type) do
    case map_delete_static(descr, key) do
      %{map: dnf} = descr ->
        dnf =
          Enum.map(dnf, fn {tag, fields, negs} ->
            {tag, Map.put(fields, key, type),
             Enum.map(negs, fn {neg_tag, neg_fields} ->
               {neg_tag, Map.put(neg_fields, key, type)}
             end)}
          end)

        %{descr | map: dnf}

      %{} ->
        descr
    end
  end

  defp pop_optional_static(type) do
    case type do
      %{bitmap: @bit_optional} ->
        {true, Map.delete(type, :bitmap)}

      %{bitmap: bitmap} when (bitmap &&& @bit_optional) != 0 ->
        {true, %{type | bitmap: bitmap - @bit_optional}}

      _ ->
        {false, type}
    end
  end

  # Union is list concatenation
  defp map_union(dnf1, dnf2), do: dnf1 ++ dnf2

  # Given two unions of maps, intersects each pair of maps.
  defp map_intersection(dnf1, dnf2) do
    for {tag1, pos1, negs1} <- dnf1,
        {tag2, pos2, negs2} <- dnf2,
        reduce: [] do
      acc ->
        try do
          {tag, fields} = map_literal_intersection(tag1, pos1, tag2, pos2)
          [{tag, fields, negs1 ++ negs2} | acc]
        catch
          :empty -> acc
        end
    end
    |> case do
      [] -> 0
      acc -> acc
    end
  end

  # Intersects two map literals; throws if their intersection is empty.
  # Both open: the result is open.
  defp map_literal_intersection(:open, map1, :open, map2) do
    new_fields =
      symmetrical_merge(map1, map2, fn _, type1, type2 ->
        non_empty_intersection!(type1, type2)
      end)

    {:open, new_fields}
  end

  # Both closed: the result is closed.
  defp map_literal_intersection(:closed, map1, :closed, map2) do
    new_fields =
      symmetrical_intersection(map1, map2, fn _, type1, type2 ->
        non_empty_intersection!(type1, type2)
      end)

    if map_size(new_fields) < map_size(map1) or map_size(new_fields) < map_size(map2) do
      throw(:empty)
    end

    {:closed, new_fields}
  end

  # Open and closed: result is closed, all fields from open should be in closed, except not_set ones.
  defp map_literal_intersection(:open, open, :closed, closed) do
    :maps.iterator(open) |> :maps.next() |> map_literal_intersection_loop(closed)
  end

  defp map_literal_intersection(:closed, closed, :open, open) do
    :maps.iterator(open) |> :maps.next() |> map_literal_intersection_loop(closed)
  end

  defp map_literal_intersection_loop(:none, acc), do: {:closed, acc}

  defp map_literal_intersection_loop({key, type1, iterator}, acc) do
    case acc do
      %{^key => type2} ->
        acc = %{acc | key => non_empty_intersection!(type1, type2)}
        :maps.next(iterator) |> map_literal_intersection_loop(acc)

      _ ->
        # If the key is marked as not_set in the open map, we can ignore it.
        if type1 == @not_set do
          :maps.next(iterator) |> map_literal_intersection_loop(acc)
        else
          throw(:empty)
        end
    end
  end

  defp non_empty_intersection!(type1, type2) do
    type = intersection(type1, type2)
    if empty?(type), do: throw(:empty), else: type
  end

  defp map_difference(dnf1, dnf2) do
    Enum.reduce(dnf2, dnf1, fn
      # Optimization: we are removing an open map with one field.
      {:open, fields2, []}, dnf1 when map_size(fields2) == 1 ->
        Enum.reduce(dnf1, [], fn {tag1, fields1, negs1}, acc ->
          {key, value} = Enum.at(fields2, 0)
          t_diff = difference(Map.get(fields1, key, tag_to_type(tag1)), value)

          if empty?(t_diff) do
            acc
          else
            [{tag1, Map.put(fields1, key, t_diff), negs1} | acc]
          end
        end)

      {tag2, fields2, negs2}, dnf1 ->
        Enum.reduce(dnf1, [], fn {tag1, fields1, negs1}, acc ->
          acc = [{tag1, fields1, [{tag2, fields2} | negs1]} | acc]

          Enum.reduce(negs2, acc, fn {neg_tag2, neg_fields2}, acc ->
            try do
              {tag, fields} = map_literal_intersection(tag1, fields1, neg_tag2, neg_fields2)
              [{tag, fields, negs1} | acc]
            catch
              :empty -> acc
            end
          end)
        end)
    end)
    |> case do
      [] -> 0
      acc -> acc
    end
  end

  @doc """
  Removes a key from a map type.

  ## Algorithm

  1. Split the map type based on the presence of the key.
  2. Take the second part of the split, which represents the union of all
     record types where the key has been explicitly removed.
  3. Intersect this with an open record type where the key is explicitly absent.
     This step eliminates the key from open record types where it was implicitly present.
  """
  def map_delete(:term, _key), do: :badmap

  def map_delete(descr, key) when is_atom(key) do
    case :maps.take(:dynamic, descr) do
      :error ->
        # Note: the empty typ is not a valid input
        if descr_key?(descr, :map) and map_only?(descr) do
          map_delete_static(descr, key)
          |> intersection(open_map([{key, not_set()}]))
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          dynamic_result = map_delete_static(dynamic, key)
          static_result = map_delete_static(static, key)

          union(dynamic(dynamic_result), static_result)
          |> intersection(open_map([{key, not_set()}]))
        else
          :badmap
        end
    end
  end

  # Takes a static map type and removes a key from it.
  defp map_delete_static(%{map: dnf}, key) do
    Enum.reduce(dnf, none(), fn
      # Optimization: if there are no negatives, we can directly remove the key.
      {tag, fields, []}, acc ->
        union(acc, %{map: map_new(tag, :maps.remove(key, fields))})

      {tag, fields, negs}, acc ->
        {fst, snd} = map_pop_key(tag, fields, key)

        union(
          acc,
          case map_split_negative(negs, key) do
            :empty ->
              none()

            negative ->
              negative |> pair_make_disjoint() |> pair_eliminate_negations_snd(fst, snd)
          end
        )
    end)
  end

  defp map_delete_static(:term, key), do: open_map([{key, not_set()}])

  # If there is no map part to this static type, there is nothing to delete.
  defp map_delete_static(_type, _key), do: none()

  # Emptiness checking for maps.
  #
  # Short-circuits if it finds a non-empty map literal in the union.
  # Since the algorithm is recursive, we implement the short-circuiting
  # as throw/catch.
  defp map_empty?(dnf) do
    Enum.all?(dnf, fn {tag, pos, negs} -> map_empty?(tag, pos, negs) end)
  end

  defp map_empty?(_, _, []), do: false
  defp map_empty?(_, _, [{:open, neg_fields} | _]) when neg_fields == %{}, do: true
  defp map_empty?(:open, fs, [{:closed, _} | negs]), do: map_empty?(:open, fs, negs)

  defp map_empty?(tag, fields, [{neg_tag, neg_fields} | negs]) do
    (Enum.all?(neg_fields, fn {neg_key, neg_type} ->
       cond do
         # Keys that are present in the negative map, but not in the positive one
         is_map_key(fields, neg_key) ->
           true

         # The key is not shared between positive and negative maps,
         # if the negative type is optional, then there may be a value in common
         tag == :closed ->
           is_optional_static(neg_type)

         # There may be value in common
         tag == :open ->
           diff = difference(term_or_optional(), neg_type)
           empty?(diff) or map_empty?(tag, Map.put(fields, neg_key, diff), negs)
       end
     end) and
       Enum.all?(fields, fn {key, type} ->
         case neg_fields do
           %{^key => neg_type} ->
             diff = difference(type, neg_type)
             empty?(diff) or map_empty?(tag, Map.put(fields, key, diff), negs)

           %{} ->
             cond do
               neg_tag == :open ->
                 true

               neg_tag == :closed and not is_optional_static(type) ->
                 false

               true ->
                 # an absent key in a open negative map can be ignored
                 diff = difference(type, tag_to_type(neg_tag))
                 empty?(diff) or map_empty?(tag, Map.put(fields, key, diff), negs)
             end
         end
       end)) or map_empty?(tag, fields, negs)
  end

  # Takes a map dnf and returns the union of types it can take for a given key.
  # If the key may be undefined, it will contain the `not_set()` type.
  defp map_get(dnf, key) do
    Enum.reduce(dnf, none(), fn
      # Optimization: if there are no negatives,
      # we can return the value directly.
      {_tag, %{^key => value}, []}, acc ->
        value |> union(acc)

      # Optimization: if there are no negatives
      # and the key does not exist, return the default one.
      {tag, %{}, []}, acc ->
        tag_to_type(tag) |> union(acc)

      {tag, fields, negs}, acc ->
        {fst, snd} = map_pop_key(tag, fields, key)

        case map_split_negative(negs, key) do
          :empty -> none()
          negative -> negative |> pair_make_disjoint() |> pair_eliminate_negations_fst(fst, snd)
        end
        |> union(acc)
    end)
  end

  defp map_pop_key(tag, fields, key) do
    case :maps.take(key, fields) do
      {value, fields} -> {value, %{map: map_new(tag, fields)}}
      :error -> {tag_to_type(tag), %{map: map_new(tag, fields)}}
    end
  end

  defp map_split_negative(negs, key) do
    Enum.reduce_while(negs, [], fn
      # A negation with an open map means the whole thing is empty.
      {:open, fields}, _acc when map_size(fields) == 0 -> {:halt, :empty}
      {tag, fields}, neg_acc -> {:cont, [map_pop_key(tag, fields, key) | neg_acc]}
    end)
  end

  # Use heuristics to normalize a map dnf for pretty printing.
  # TODO: eliminate some simple negations, those which have only zero or one key in common.
  defp map_normalize(dnf) do
    dnf
    |> Enum.reject(&map_empty?([&1]))
    |> Enum.map(fn {tag, fields, negs} ->
      {tag, fields, Enum.reject(negs, &map_empty_negation?(tag, fields, &1))}
    end)
  end

  # Adapted from `map_empty?` to remove useless negations.
  defp map_empty_negation?(tag, fields, {neg_tag, neg_fields}) do
    (tag == :closed and
       Enum.any?(neg_fields, fn {neg_key, neg_type} ->
         not is_map_key(fields, neg_key) and not is_optional_static(neg_type)
       end)) or
      (neg_tag == :closed and
         Enum.any?(fields, fn {key, type} ->
           not is_map_key(neg_fields, key) and not is_optional_static(type)
         end))
  end

  defp map_to_quoted(dnf) do
    dnf
    |> map_normalize()
    |> Enum.map(&map_each_to_quoted/1)
    |> case do
      [] -> []
      dnf -> Enum.reduce(dnf, &{:or, [], [&2, &1]}) |> List.wrap()
    end
  end

  defp map_each_to_quoted({tag, positive_map, negative_maps}) do
    case negative_maps do
      [] ->
        map_literal_to_quoted({tag, positive_map})

      _ ->
        negative_maps
        |> Enum.map(&map_literal_to_quoted/1)
        |> Enum.reduce(&{:or, [], [&2, &1]})
        |> Kernel.then(
          &{:and, [], [map_literal_to_quoted({tag, positive_map}), {:not, [], [&1]}]}
        )
    end
  end

  def map_literal_to_quoted({:closed, fields}) when map_size(fields) == 0 do
    {:empty_map, [], []}
  end

  def map_literal_to_quoted({tag, fields}) do
    case tag do
      :closed ->
        with %{__struct__: struct_descr} <- fields,
             {_, [struct]} <- atom_fetch(struct_descr) do
          {:%, [],
           [
             literal_to_quoted(struct),
             {:%{}, [], map_fields_to_quoted(tag, Map.delete(fields, :__struct__))}
           ]}
        else
          _ -> {:%{}, [], map_fields_to_quoted(tag, fields)}
        end

      :open ->
        {:%{}, [], [{:..., [], nil} | map_fields_to_quoted(tag, fields)]}
    end
  end

  defp map_fields_to_quoted(tag, map) do
    sorted = Enum.sort(map)
    keyword? = Inspect.List.keyword?(sorted)

    for {key, type} <- sorted,
        not (tag == :open and is_optional_static(type) and term_type?(type)) do
      key =
        if keyword? do
          {:__block__, [format: :keyword], [key]}
        else
          literal_to_quoted(key)
        end

      cond do
        not is_optional_static(type) -> {key, to_quoted(type)}
        empty?(type) -> {key, {:not_set, [], []}}
        true -> {key, {:if_set, [], [to_quoted(type)]}}
      end
    end
  end

  ## Tuple

  # Represents tuple types in two forms:
  # 1. Closed tuples: Fixed-length tuples with specific element types
  #    Example: {integer(), atom()}
  # 2. Open tuples: Variable-length tuples with a minimum set of element types
  #    Example: {atom(), boolean(), ...}
  #
  # Internal representation:
  # - Closed tuple: {:closed, [element_type, ...]}
  # - Open tuple:   {:open, [element_type, ...]}
  #
  # Examples:
  # - {integer(), atom()} is encoded as {:closed, [integer(), atom()]}
  # - {atom(), boolean(), ...} is encoded as {:open, [atom(), boolean()]}

  defp tuple_descr(tag, fields) do
    case tuple_descr(fields, [], false) do
      {fields, true} -> %{dynamic: %{tuple: tuple_new(tag, Enum.reverse(fields))}}
      {_, false} -> %{tuple: tuple_new(tag, fields)}
    end
  end

  defp tuple_descr([:term | rest], acc, dynamic?) do
    tuple_descr(rest, [:term | acc], dynamic?)
  end

  defp tuple_descr([value | rest], acc, dynamic?) do
    case :maps.take(:dynamic, value) do
      :error -> tuple_descr(rest, [value | acc], dynamic?)
      {dynamic, _static} -> tuple_descr(rest, [dynamic | acc], true)
    end
  end

  defp tuple_descr([], acc, dynamic?) do
    {acc, dynamic?}
  end

  defp tuple_new(tag, elements), do: [{tag, elements, []}]

  defp tuple_intersection(dnf1, dnf2) do
    for {tag1, elements1, negs1} <- dnf1,
        {tag2, elements2, negs2} <- dnf2,
        reduce: [] do
      acc ->
        try do
          {tag, fields} = tuple_literal_intersection(tag1, elements1, tag2, elements2)
          [{tag, fields, negs1 ++ negs2} | acc]
        catch
          :empty -> acc
        end
    end
    |> case do
      [] -> 0
      acc -> acc
    end
  end

  defp tuple_literal_intersection(tag1, elements1, tag2, elements2) do
    n = length(elements1)
    m = length(elements2)

    cond do
      (tag1 == :closed and n < m) or (tag2 == :closed and n > m) -> throw(:empty)
      tag1 == :open and tag2 == :open -> {:open, zip_intersection(elements1, elements2, [])}
      true -> {:closed, zip_intersection(elements1, elements2, [])}
    end
  end

  # Intersects two lists of types, and _appends_ the extra elements to the result.
  defp zip_intersection([], types2, acc), do: Enum.reverse(acc, types2)
  defp zip_intersection(types1, [], acc), do: Enum.reverse(acc, types1)

  defp zip_intersection([type1 | rest1], [type2 | rest2], acc) do
    zip_intersection(rest1, rest2, [non_empty_intersection!(type1, type2) | acc])
  end

  defp tuple_difference(dnf1, dnf2) do
    Enum.reduce(dnf2, dnf1, fn {tag2, elements2, negs2}, dnf1 ->
      Enum.reduce(dnf1, [], fn {tag1, elements1, negs1}, acc ->
        acc = [{tag1, elements1, [{tag2, elements2} | negs1]}] ++ acc

        Enum.reduce(negs2, acc, fn {neg_tag2, neg_elements2}, acc ->
          try do
            {tag, fields} = tuple_literal_intersection(tag1, elements1, neg_tag2, neg_elements2)
            [{tag, fields, negs1}] ++ acc
          catch
            :empty -> acc
          end
        end)
      end)
    end)
    |> case do
      [] -> 0
      acc -> acc
    end
  end

  defp tuple_union(left, right), do: left ++ right

  defp tuple_to_quoted(dnf) do
    dnf
    |> tuple_simplify()
    |> Enum.map(&tuple_each_to_quoted/1)
    |> case do
      [] -> []
      dnf -> Enum.reduce(dnf, &{:or, [], [&2, &1]}) |> List.wrap()
    end
  end

  defp tuple_each_to_quoted({tag, positive_map, negative_maps}) do
    case negative_maps do
      [] ->
        tuple_literal_to_quoted({tag, positive_map})

      _ ->
        negative_maps
        |> Enum.map(&tuple_literal_to_quoted/1)
        |> Enum.reduce(&{:or, [], [&2, &1]})
        |> Kernel.then(
          &{:and, [], [tuple_literal_to_quoted({tag, positive_map}), {:not, [], [&1]}]}
        )
    end
  end

  defp tuple_literal_to_quoted({:closed, []}), do: {:{}, [], []}

  defp tuple_literal_to_quoted({tag, elements}) do
    case tag do
      :closed -> {:{}, [], Enum.map(elements, &to_quoted/1)}
      :open -> {:{}, [], Enum.map(elements, &to_quoted/1) ++ [{:..., [], nil}]}
    end
  end

  # Pads a list of elements with term().
  defp tuple_fill(elements, desired_length) do
    pad_length = desired_length - length(elements)

    if pad_length < 0 do
      raise ArgumentError, "tuple_fill: elements are longer than the desired length"
    else
      elements ++ List.duplicate(term(), pad_length)
    end
  end

  # Check if a tuple represented in DNF is empty
  defp tuple_empty?(dnf) do
    Enum.all?(dnf, fn {tag, pos, negs} -> tuple_empty?(tag, pos, negs) end)
  end

  # No negations, so not empty
  defp tuple_empty?(_, _, []), do: false
  # Open empty negation makes it empty
  defp tuple_empty?(_, _, [{:open, []} | _]), do: true
  # Open positive can't be emptied by a single closed negative
  defp tuple_empty?(:open, _, [{:closed, _}]), do: false

  defp tuple_empty?(tag, elements, [{neg_tag, neg_elements} | negs]) do
    n = length(elements)
    m = length(neg_elements)

    # Scenarios where the difference is guaranteed to be empty:
    # 1. When removing larger tuples from a fixed-size positive tuple
    # 2. When removing smaller tuples from larger tuples
    if (tag == :closed and n < m) or (neg_tag == :closed and n > m) do
      tuple_empty?(tag, elements, negs)
    else
      tuple_elements([], tag, elements, neg_elements, negs) and
        tuple_compatibility(n, m, tag, elements, neg_tag, negs)
    end
  end

  # Recursively check elements for emptiness
  defp tuple_elements(_, _, _, [], _), do: true

  defp tuple_elements(acc, tag, elements, [neg_type | neg_elements], negs) do
    # Handles the case where {tag, elements} is an open tuple, like {:open, []}
    {ty, elements} = List.pop_at(elements, 0, term())
    diff = difference(ty, neg_type)

    (empty?(diff) or tuple_empty?(tag, Enum.reverse(acc, [diff | elements]), negs)) and
      tuple_elements([ty | acc], tag, elements, neg_elements, negs)
  end

  # Determines if the set difference is empty when:
  # - Positive tuple: {tag, elements} of size n
  # - Negative tuple: open or closed tuples of size m
  defp tuple_compatibility(n, m, tag, elements, neg_tag, negs) do
    # The tuples to consider are all those of size n to m - 1, and if the negative tuple is
    # closed, we also need to consider tuples of size greater than m + 1.
    tag == :closed or
      (Enum.all?(n..(m - 1)//1, &tuple_empty?(:closed, tuple_fill(elements, &1), negs)) and
         (neg_tag == :open or tuple_empty?(:open, tuple_fill(elements, m + 1), negs)))
  end

  @doc """
  Fetches the type of the value returned by accessing `index` on `tuple`
  with the assumption that the descr is exclusively a tuple (or dynamic).

  Returns one of:

  - `{false, type}` if the element is always accessible and has the given `type`.
  - `{true, type}` if the element is dynamically optional and has the given `type`.
  - `:badindex` if the index is never accessible in the tuple type.
  - `:badtuple` if the descr is not a tuple type.

  ## Examples

      iex> tuple_fetch(tuple([integer(), atom()]), 0)
      {false, integer()}

      iex> tuple_fetch(union(tuple([integer()]), tuple([integer(), atom()])), 1)
      {true, atom()}

      iex> tuple_fetch(dynamic(), 0)
      {true, dynamic()}

      iex> tuple_fetch(integer(), 0)
      :badtuple

  """
  def tuple_fetch(_, index) when index < 0, do: :badindex
  def tuple_fetch(:term, _key), do: :badtuple

  def tuple_fetch(%{} = descr, key) when is_integer(key) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :tuple) and tuple_only?(descr) do
          {static_optional?, static_type} = tuple_fetch_static(descr, key)

          # If I access a static tuple at a "open position", we have two options:
          #
          # 1. Do not allow the access and return :badindex,
          #    you must use dynamic for these cases (this is what we chose for maps)
          #
          # 2. Allow the access and return the static type
          #
          # The trouble with allowing the access is that it is a potential runtime
          # error not being caught by the type system.
          #
          # Furthermore, our choice here, needs to be consistent with elem/put_elem
          # when the index is the `integer()` type. If we choose to return `:badindex`,
          # then all elem/put_elem with an `integer()` and the tuple is not dynamic
          # should also be a static typing error. We chose to go with 1.
          if static_optional? or empty?(static_type) do
            :badindex
          else
            {false, static_type}
          end
        else
          :badtuple
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :tuple) and tuple_only?(static) do
          {dynamic_optional?, dynamic_type} = tuple_fetch_static(dynamic, key)
          {static_optional?, static_type} = tuple_fetch_static(static, key)

          if empty?(dynamic_type) do
            :badindex
          else
            {static_optional? or dynamic_optional?, union(dynamic(dynamic_type), static_type)}
          end
        else
          :badtuple
        end
    end
  end

  defp tuple_only?(descr), do: empty?(Map.delete(descr, :tuple))

  defp tuple_fetch_static(descr, index) when is_integer(index) do
    case descr do
      :term ->
        {true, term()}

      %{tuple: tuple} ->
        tuple_get(tuple, index)
        |> pop_optional_static()

      %{} ->
        {false, none()}
    end
  end

  defp tuple_get(dnf, index) do
    Enum.reduce(dnf, none(), fn
      # Optimization: if there are no negatives, just return the type at that index.
      {tag, elements, []}, acc ->
        Enum.at(elements, index, tag_to_type(tag)) |> union(acc)

      {tag, elements, negs}, acc ->
        {fst, snd} = tuple_pop_index(tag, elements, index)

        case tuple_split_negative(negs, index) do
          :empty -> none()
          negative -> negative |> pair_make_disjoint() |> pair_eliminate_negations_fst(fst, snd)
        end
        |> union(acc)
    end)
  end

  defp tuple_pop_index(tag, elements, index) do
    case List.pop_at(elements, index) do
      {nil, _} -> {tag_to_type(tag), %{tuple: [{tag, elements, []}]}}
      {type, rest} -> {type, %{tuple: [{tag, rest, []}]}}
    end
  end

  defp tuple_split_negative(negs, index) do
    Enum.reduce_while(negs, [], fn
      {:open, []}, _acc -> {:halt, :empty}
      {tag, elements}, acc -> {:cont, [tuple_pop_index(tag, elements, index) | acc]}
    end)
  end

  # Use heuristics to simplify a tuple dnf for pretty printing.
  defp tuple_simplify(dnf) do
    for {tag, elements, negs} <- dnf,
        not tuple_empty?([{tag, elements, negs}]) do
      n = length(elements)
      {tag, elements, Enum.reject(negs, &tuple_empty_negation?(tag, n, &1))}
    end
  end

  @doc """
  Delete an element from the tuple.

  It returns the same as `tuple_fetch/2`.
  """
  # Same as tuple_delete but checks if the index is out of range.
  def tuple_delete_at(:term, _key), do: :badtuple

  def tuple_delete_at(descr, index) when is_integer(index) and index >= 0 do
    case :maps.take(:dynamic, descr) do
      :error ->
        # Note: the empty type is not a valid input
        is_proper_tuple? = descr_key?(descr, :tuple) and tuple_only?(descr)
        is_proper_size? = tuple_of_size_at_least_static?(descr, index + 1)

        cond do
          is_proper_tuple? and is_proper_size? -> tuple_delete_static(descr, index)
          is_proper_tuple? -> :badindex
          true -> :badtuple
        end

      {dynamic, static} ->
        is_proper_tuple? = descr_key?(dynamic, :tuple) and tuple_only?(static)
        is_proper_size? = tuple_of_size_at_least_static?(static, index + 1)

        cond do
          is_proper_tuple? and is_proper_size? ->
            static_result = tuple_delete_static(static, index)

            # Prune for dynamic values make the intersection succeed
            dynamic_result =
              intersection(dynamic, tuple_of_size_at_least(index))
              |> tuple_delete_static(index)

            union(dynamic(dynamic_result), static_result)

          # Highlight the case where the issue is an index out of range from the tuple
          is_proper_tuple? ->
            :badindex

          true ->
            :badtuple
        end
    end
  end

  def tuple_delete_at(_, _), do: :badindex

  # Takes a static map type and removes an index from it.
  defp tuple_delete_static(%{tuple: dnf}, index) do
    Enum.reduce(dnf, none(), fn
      # Optimization: if there are no negatives, we can directly remove the element
      {tag, elements, []}, acc ->
        union(acc, %{tuple: tuple_new(tag, List.delete_at(elements, index))})

      {tag, elements, negs}, acc ->
        {fst, snd} = tuple_pop_index(tag, elements, index)

        union(
          acc,
          case tuple_split_negative(negs, index) do
            :empty -> none()
            negative -> negative |> pair_make_disjoint() |> pair_eliminate_negations_snd(fst, snd)
          end
        )
    end)
  end

  # If there is no map part to this static type, there is nothing to delete.
  defp tuple_delete_static(_type, _key), do: none()

  @doc """
  Insert an element at the tuple.

  It returns the same as `tuple_fetch/2`. Notice, however, the range for indexes is inclusive.
  """
  def tuple_insert_at(:term, _key, _type), do: :badtuple

  def tuple_insert_at(descr, index, type) when is_integer(index) and index >= 0 do
    case :maps.take(:dynamic, unfold(type)) do
      :error -> tuple_insert_at_checked(descr, index, type)
      {dynamic, _static} -> dynamic(tuple_insert_at_checked(descr, index, dynamic))
    end
  end

  def tuple_insert_at(_, _, _), do: :badindex

  defp tuple_insert_at_checked(descr, index, type) do
    case :maps.take(:dynamic, descr) do
      :error ->
        # Note: the empty type is not a valid input
        is_proper_tuple? = descr_key?(descr, :tuple) and tuple_only?(descr)
        is_proper_size? = index == 0 or tuple_of_size_at_least_static?(descr, index)

        cond do
          is_proper_tuple? and is_proper_size? -> tuple_insert_static(descr, index, type)
          is_proper_tuple? -> :badindex
          true -> :badtuple
        end

      {dynamic, static} ->
        is_proper_tuple? = descr_key?(dynamic, :tuple) and tuple_only?(static)
        is_proper_size? = index == 0 or tuple_of_size_at_least_static?(static, index)

        cond do
          is_proper_tuple? and is_proper_size? ->
            static_result = tuple_insert_static(static, index, type)

            # Prune for dynamic values that make the intersection succeed
            dynamic_result =
              intersection(dynamic, tuple_of_size_at_least(index))
              |> tuple_insert_static(index, type)

            union(dynamic(dynamic_result), static_result)

          # Highlight the case where the issue is an index out of range from the tuple
          is_proper_tuple? ->
            :badindex

          true ->
            :badtuple
        end
    end
  end

  defp tuple_insert_static(descr, _, _) when descr == @none, do: none()

  defp tuple_insert_static(descr, index, type) do
    Map.update!(descr, :tuple, fn dnf ->
      Enum.map(dnf, fn {tag, elements, negs} ->
        {tag, List.insert_at(elements, index, type),
         Enum.map(negs, fn {neg_tag, neg_elements} ->
           {neg_tag, List.insert_at(neg_elements, index, type)}
         end)}
      end)
    end)
  end

  # Remove useless negations, which denote tuples of incompatible sizes.
  defp tuple_empty_negation?(tag, n, {neg_tag, neg_elements}) do
    m = length(neg_elements)
    (tag == :closed and n < m) or (neg_tag == :closed and n > m)
  end

  defp tuple_of_size_at_least(n) when is_integer(n) and n >= 0 do
    open_tuple(List.duplicate(term(), n))
  end

  defp tuple_of_size_at_least_static?(descr, index) do
    case descr do
      %{tuple: dnf} ->
        Enum.all?(dnf, fn
          {_, elements, []} -> length(elements) >= index
          entry -> subtype?(%{tuple: [entry]}, tuple_of_size_at_least(index))
        end)

      %{} ->
        true
    end
  end

  ## Pairs
  #
  # To simplify disjunctive normal forms of e.g., map types, it is useful to
  # convert them into disjunctive normal forms of pairs of types, and define
  # normalization algorithms on pairs.
  #
  # The algorithms take a line, a list of pairs `{positive, negative}` where
  # `positive` is a list of literals and `negative` is a list of negated literals.
  # Positive pairs can all be intersected component-wise. Negative ones are
  # eliminated iteratively.

  # Eliminates negations from `{t, s} and not negative` where `negative` is a
  # union of pairs disjoint on their first component.
  #
  # Formula:
  #   {t, s} and not (union<i=1..n> {t_i, s_i})
  #       = union<i=1..n> {t and t_i, s and not s_i}
  #            or {t and not (union{i=1..n} t_i), s}
  #
  # This eliminates all top-level negations and produces a union of pairs that
  # are disjoint on their first component. The function `pair_eliminate_negations_fst`
  # is optimized to only keep the first component out of those pairs.
  defp pair_eliminate_negations_fst(negative, t, s) do
    {pair_union, diff_of_t_i} =
      Enum.reduce(negative, {none(), t}, fn {t_i, s_i}, {accu, diff_of_t_i} ->
        i = intersection(t, t_i)

        if empty?(i) do
          {accu, diff_of_t_i}
        else
          diff_of_t_i = difference(diff_of_t_i, t_i)
          s_diff = difference(s, s_i)

          if empty?(s_diff),
            do: {accu, diff_of_t_i},
            else: {union(i, accu), diff_of_t_i}
        end
      end)

    union(pair_union, diff_of_t_i)
  end

  # The formula above is symmetric with respect to the first and second components.
  # Hence the following also holds true:
  #
  #    {t, s} and not (union<i=1..n> {t_i, s_i})
  #           = union<i=1..n> {t and not t_i, s and s_i}
  #            or {t, s and not (union{i=1..n} s_i)}
  #
  # which is used to in the following function, optimized to keep the second component.
  defp pair_eliminate_negations_snd(negative, t, s) do
    {pair_union, diff_of_s_i} =
      Enum.reduce(negative, {none(), s}, fn {t_i, s_i}, {accu, diff_of_s_i} ->
        i = intersection(s, s_i)

        if empty?(i) do
          {accu, diff_of_s_i}
        else
          diff_of_s_i = difference(diff_of_s_i, s_i)
          t_diff = difference(t, t_i)

          if empty?(t_diff),
            do: {accu, diff_of_s_i},
            else: {union(i, accu), diff_of_s_i}
        end
      end)

    union(diff_of_s_i, pair_union)
  end

  # Makes a union of pairs into an equivalent union of disjoint pairs.
  #
  # Inserts a pair of types {fst, snd} into a list of pairs that are disjoint
  # on their first component. The invariant on `acc` is that its elements are
  # two-to-two disjoint with the first argument's `pairs`.
  #
  # To insert {fst, snd} into a disjoint pairs list, we go through the list to find
  # each pair whose first element has a non-empty intersection with `fst`. Then
  # we decompose {fst, snd} over each such pair to produce disjoint ones, and add
  # the decompositions into the accumulator.
  defp pair_make_disjoint(pairs) do
    Enum.reduce(pairs, [], fn {t1, t2}, acc -> add_pair_to_disjoint_list(acc, t1, t2, []) end)
  end

  defp add_pair_to_disjoint_list([], fst, snd, acc), do: [{fst, snd} | acc]

  defp add_pair_to_disjoint_list([{s1, s2} | pairs], fst, snd, acc) do
    x = intersection(fst, s1)

    if empty?(x) do
      add_pair_to_disjoint_list(pairs, fst, snd, [{s1, s2} | acc])
    else
      fst_diff = difference(fst, s1)
      s1_diff = difference(s1, fst)
      empty_fst_diff = empty?(fst_diff)
      empty_s1_diff = empty?(s1_diff)

      cond do
        # if fst is a subtype of s1, the disjointness invariant ensures we can
        # add those two pairs and end the recursion
        empty_fst_diff and empty_s1_diff ->
          [{x, union(snd, s2)} | pairs ++ acc]

        empty_fst_diff ->
          [{s1_diff, s2}, {x, union(snd, s2)} | pairs ++ acc]

        empty_s1_diff ->
          add_pair_to_disjoint_list(pairs, fst_diff, snd, [{x, union(snd, s2)} | acc])

        true ->
          # case where, when comparing {fst, snd} and {s1, s2}, both (fst and not s1)
          # and (s1 and not fst) are non empty. that is, there is something in fst
          # that is not in s1, and something in s1 that is not in fst
          add_pair_to_disjoint_list(pairs, fst_diff, snd, [
            {s1_diff, s2},
            {x, union(snd, s2)} | acc
          ])
      end
    end
  end

  ## Map helpers

  defp symmetrical_merge(left, right, fun) do
    # Erlang maps:merge_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_merge(:maps.next(:maps.iterator(right)), left, fun)
    else
      iterator_merge(:maps.next(:maps.iterator(left)), right, fun)
    end
  end

  defp iterator_merge({key, v1, iterator}, map, fun) do
    acc =
      case map do
        %{^key => v2} -> %{map | key => fun.(key, v1, v2)}
        %{} -> Map.put(map, key, v1)
      end

    iterator_merge(:maps.next(iterator), acc, fun)
  end

  defp iterator_merge(:none, map, _fun), do: map

  defp symmetrical_intersection(left, right, fun) do
    # Erlang maps:intersect_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_intersection(:maps.next(:maps.iterator(right)), left, [], fun)
    else
      iterator_intersection(:maps.next(:maps.iterator(left)), right, [], fun)
    end
  end

  defp iterator_intersection({key, v1, iterator}, map, acc, fun) do
    acc =
      case map do
        %{^key => v2} ->
          case fun.(key, v1, v2) do
            0 -> acc
            value when value == @none -> acc
            value -> [{key, value} | acc]
          end

        %{} ->
          acc
      end

    iterator_intersection(:maps.next(iterator), map, acc, fun)
  end

  defp iterator_intersection(:none, _map, acc, _fun), do: :maps.from_list(acc)

  defp iterator_difference({key, v2, iterator}, map) do
    acc =
      case map do
        %{^key => v1} ->
          case difference(key, v1, v2) do
            0 -> Map.delete(map, key)
            [] -> Map.delete(map, key)
            value -> %{map | key => value}
          end

        %{} ->
          map
      end

    iterator_difference(:maps.next(iterator), acc)
  end

  defp iterator_difference(:none, map), do: map
end
