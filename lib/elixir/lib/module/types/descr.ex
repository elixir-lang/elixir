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

  # TODO: When we convert from AST to descr, we need to normalize
  # the dynamic type.
  import Bitwise

  @bit_binary 1 <<< 0
  @bit_empty_list 1 <<< 1
  @bit_integer 1 <<< 2
  @bit_float 1 <<< 3
  @bit_pid 1 <<< 4
  @bit_port 1 <<< 5
  @bit_reference 1 <<< 6

  @bit_non_empty_list 1 <<< 7
  @bit_tuple 1 <<< 8
  @bit_fun 1 <<< 9
  @bit_top (1 <<< 10) - 1
  @bit_optional 1 <<< 10

  @atom_top {:negation, :sets.new(version: 2)}
  @map_top [{:open, %{}, []}]

  # Guard helpers

  @term %{bitmap: @bit_top, atom: @atom_top, map: @map_top}
  @none %{}
  @dynamic %{dynamic: @term}

  # Type definitions

  def dynamic(), do: @dynamic
  def term(), do: @term
  def none(), do: @none

  def atom(as), do: %{atom: atom_new(as)}
  def atom(), do: %{atom: @atom_top}
  def binary(), do: %{bitmap: @bit_binary}
  def empty_list(), do: %{bitmap: @bit_empty_list}
  def integer(), do: %{bitmap: @bit_integer}
  def float(), do: %{bitmap: @bit_float}
  def fun(), do: %{bitmap: @bit_fun}
  def map(pairs, open_or_closed), do: %{map: map_new(open_or_closed, pairs)}
  def map(pairs), do: %{map: map_new(:closed, pairs)}
  def map(), do: %{map: @map_top}
  def empty_map(), do: map([])
  def non_empty_list(), do: %{bitmap: @bit_non_empty_list}
  def pid(), do: %{bitmap: @bit_pid}
  def port(), do: %{bitmap: @bit_port}
  def reference(), do: %{bitmap: @bit_reference}
  def tuple(), do: %{bitmap: @bit_tuple}

  @boolset :sets.from_list([true, false], version: 2)
  def boolean(), do: %{atom: {:union, @boolset}}

  # Map helpers

  @not_set %{bitmap: @bit_optional}
  @term_or_not_set %{bitmap: @bit_top ||| @bit_optional, atom: @atom_top, map: @map_top}

  def not_set(), do: @not_set
  def if_set(type), do: Map.update(type, :bitmap, @bit_optional, &(&1 ||| @bit_optional))
  defp term_or_not_set(), do: @term_or_not_set

  ## Set operations

  def term?(descr), do: subtype_static(@term, Map.delete(descr, :dynamic))
  def gradual?(descr), do: is_map_key(descr, :dynamic)

  @doc """
  Computes the union of two descrs.
  """
  def union(%{} = left, %{} = right) do
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(right, :dynamic, right)
        union_static(left, right_with_dynamic)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(left, :dynamic, left)
        union_static(left_with_dynamic, right)

      true ->
        union_static(left, right)
    end
  end

  defp union_static(left, right) do
    # Erlang maps:merge_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_union(:maps.next(:maps.iterator(right)), left)
    else
      iterator_union(:maps.next(:maps.iterator(left)), right)
    end
  end

  @compile {:inline, union: 3}
  defp union(:bitmap, v1, v2), do: bitmap_union(v1, v2)
  defp union(:atom, v1, v2), do: atom_union(v1, v2)
  defp union(:dynamic, v1, v2), do: dynamic_union(v1, v2)
  defp union(:map, v1, v2), do: map_union(v1, v2)

  @doc """
  Computes the intersection of two descrs.
  """
  def intersection(%{} = left, %{} = right) do
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(right, :dynamic, right)
        intersection_static(left, right_with_dynamic)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(left, :dynamic, left)
        intersection_static(left_with_dynamic, right)

      true ->
        intersection_static(left, right)
    end
  end

  defp intersection_static(left, right) do
    # Erlang maps:intersect_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_intersection(:maps.next(:maps.iterator(right)), left, [])
    else
      iterator_intersection(:maps.next(:maps.iterator(left)), right, [])
    end
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, intersection: 3}
  defp intersection(:bitmap, v1, v2), do: bitmap_intersection(v1, v2)
  defp intersection(:atom, v1, v2), do: atom_intersection(v1, v2)
  defp intersection(:dynamic, v1, v2), do: dynamic_intersection(v1, v2)
  defp intersection(:map, v1, v2), do: map_intersection(v1, v2)

  @doc """
  Computes the difference between two types.
  """
  def difference(left = %{}, right = %{}) do
    if gradual?(left) or gradual?(right) do
      {left_dynamic, left_static} = Map.pop(left, :dynamic, left)
      {right_dynamic, right_static} = Map.pop(right, :dynamic, right)
      dynamic_part = difference_static(left_dynamic, right_static)

      if empty?(dynamic_part),
        do: @none,
        else: Map.put(difference_static(left_static, right_dynamic), :dynamic, dynamic_part)
    else
      difference_static(left, right)
    end
  end

  # For static types, the difference is component-wise.
  defp difference_static(left, right) do
    iterator_difference(:maps.next(:maps.iterator(right)), left)
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  @compile {:inline, difference: 3}
  defp difference(:bitmap, v1, v2), do: bitmap_difference(v1, v2)
  defp difference(:atom, v1, v2), do: atom_difference(v1, v2)
  defp difference(:dynamic, v1, v2), do: dynamic_difference(v1, v2)
  defp difference(:map, v1, v2), do: map_difference(v1, v2)

  @doc """
  Compute the negation of a type.
  """
  def negation(%{} = descr), do: difference(term(), descr)

  @doc """
  Check if a type is empty.

  For gradual types, check that the upper bound (the dynamic part) is empty.
  Stop as soon as one non-empty component is found. Simpler components
  (bitmap, atom) are checked first for speed since, if they are present,
  the type is non-empty as we normalize then during construction.
  """
  def empty?(%{} = descr) do
    descr = Map.get(descr, :dynamic, descr)

    descr == @none or
      (not Map.has_key?(descr, :bitmap) and not Map.has_key?(descr, :atom) and
         (not Map.has_key?(descr, :map) or map_empty?(descr.map)))
  end

  @doc """
  Converts a descr to its quoted representation.
  """
  def to_quoted(%{} = descr) do
    case Enum.flat_map(descr, fn {key, value} -> to_quoted(key, value) end) do
      [] -> {:none, [], []}
      unions -> unions |> Enum.sort() |> Enum.reduce(&{:or, [], [&2, &1]})
    end
  end

  @compile {:inline, to_quoted: 2}
  defp to_quoted(:bitmap, val), do: bitmap_to_quoted(val)
  defp to_quoted(:atom, val), do: atom_to_quoted(val)
  defp to_quoted(:dynamic, descr), do: dynamic_to_quoted(descr)
  defp to_quoted(:map, dnf), do: map_to_quoted(dnf)

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

  ## Iterator helpers

  defp iterator_union({key, v1, iterator}, map) do
    acc =
      case map do
        %{^key => v2} -> %{map | key => union(key, v1, v2)}
        %{} -> Map.put(map, key, v1)
      end

    iterator_union(:maps.next(iterator), acc)
  end

  defp iterator_union(:none, map), do: map

  defp iterator_intersection({key, v1, iterator}, map, acc) do
    acc =
      case map do
        %{^key => v2} ->
          case intersection(key, v1, v2) do
            0 -> acc
            [] -> acc
            value -> [{key, value} | acc]
          end

        %{} ->
          acc
      end

    iterator_intersection(:maps.next(iterator), map, acc)
  end

  defp iterator_intersection(:none, _map, acc), do: :maps.from_list(acc)

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
  def subtype?(%{} = left, %{} = right) do
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

  defp subtype_static(left, right), do: empty?(difference_static(left, right))

  @doc """
  Check if a type is equal to another.

  It is currently not optimized. Only to be used in tests.
  """
  def equal?(left, right) do
    left == right or (subtype?(left, right) and subtype?(right, left))
  end

  @doc """
  Check if two types have a non-empty intersection.
  """
  def intersect?(left, right), do: not empty?(intersection(left, right))

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
  def compatible?(input_type, expected_type) do
    {input_dynamic, input_static} = Map.pop(input_type, :dynamic, input_type)
    expected_dynamic = Map.get(expected_type, :dynamic, expected_type)

    if empty?(input_static) do
      intersect?(input_dynamic, expected_dynamic)
    else
      subtype_static(input_static, expected_dynamic)
    end
  end

  ## Bitmaps

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
        non_empty_list: @bit_non_empty_list,
        tuple: @bit_tuple,
        fun: @bit_fun
      ]

    for {type, mask} <- pairs,
        (mask &&& val) !== 0,
        do: {type, [], []}
  end

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

  defp literal(lit), do: {:__block__, [], [lit]}

  defp atom_to_quoted({:union, a}) do
    if :sets.is_subset(@boolset, a) do
      :sets.subtract(a, @boolset)
      |> :sets.to_list()
      |> Enum.sort()
      |> Enum.reduce({:boolean, [], []}, &{:or, [], [&2, literal(&1)]})
    else
      :sets.to_list(a)
      |> Enum.sort()
      |> Enum.map(&literal/1)
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

  defp dynamic_intersection(left, right) do
    inter = intersection_static(left, right)
    if empty?(inter), do: 0, else: inter
  end

  defp dynamic_difference(left, right) do
    diff = difference_static(left, right)
    if empty?(diff), do: 0, else: diff
  end

  defp dynamic_union(left, right), do: union_static(left, right)

  defp dynamic_to_quoted(%{} = descr) do
    cond do
      term?(descr) -> [{:dynamic, [], []}]
      single = indivisible_bitmap(descr) -> [single]
      true -> [{:and, [], [{:dynamic, [], []}, to_quoted(descr)]}]
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
  # A map literal is a pair `{:open | :closed, %{keys => value_types}}`.
  #
  # For instance, the type `%{..., a: integer()} and not %{b: atom()}` can be represented
  # by the DNF containing one pair, where the positive literal is `{:open, %{a => integer()}}`
  # and the negated literal is `{:closed, %{b => atom()}}`.
  #
  # The goal of keeping symbolic negations is to avoid distributing difference on
  # every member of a union which creates a lot of map literals in the union and
  # requires emptiness checks to avoid creating empty maps.
  #
  # For instance, the difference between `%{...}` and `%{a: atom(), b: integer()}`
  # is the union of `%{..., a: atom(), b: if_set(not integer())}` and
  # `%{..., a: if_set(not atom()), b: integer()}`; for maps with more keys,
  # each key in a negated literal may create a new union when eliminated.
  #
  # Set-theoretic operations take two DNFs (lists) and return a DNF (list).
  # Simplifications can be done to prune the latter.

  ## Not_set

  # `not_set()` is a special base type that represents an not_set field in a map.
  # E.g., `%{a: integer(), b: not_set(), ...}` represents a map with an integer
  # field `a` and an not_set field `b`, and possibly other fields.

  # The `if_set()` modifier is syntactic sugar for specifying the key as a union
  # of the key type and `not_set()`. For example, `%{:foo => if_set(integer())}`
  # is equivalent to `%{:foo => integer() or not_set()}`.

  # `not_set()` has no meaning outside of map types.

  defp optional?(%{bitmap: bitmap}) when (bitmap &&& @bit_optional) != 0, do: true
  defp optional?(_), do: false

  defp remove_not_set(type) do
    case type do
      %{bitmap: @bit_optional} -> Map.delete(type, :bitmap)
      %{bitmap: bitmap} -> %{type | bitmap: bitmap &&& ~~~@bit_optional}
      _ -> type
    end
  end

  # Create a DNF from a specification of a map type.
  defp map_new(open_or_closed, pairs), do: [{open_or_closed, Map.new(pairs), []}]

  defp map_descr(tag, fields), do: %{map: [{tag, fields, []}]}

  @doc """
  Gets the type of the value returned by accessing `key` on `map`.
  Does not guarantee the key exists. To do that, use `map_has_key?`.
  """
  def map_get!(%{} = descr, key) do
    if not gradual?(descr) do
      map_get_static(descr, key)
    else
      {dynamic, static} = Map.pop(descr, :dynamic)
      dynamic_value_type = map_get_static(dynamic, key)
      static_value_type = map_get_static(static, key)
      union(intersection(dynamic(), dynamic_value_type), static_value_type)
    end
  end

  @doc """
  Check that a key is present.
  """
  def map_has_key?(descr, key) do
    subtype?(descr, map([{key, term()}], :open))
  end

  # Assuming `descr` is a static type. Accessing `key` will, if it succeeds,
  # return a value of the type returned. To guarantee that the key is always
  # present, use `map_has_key?`. To guarantee that the key may be present
  # use `map_may_have_key?`. If key is never present, result will be `none()`.
  defp map_get_static(descr, key) when is_atom(key) do
    descr_map = intersection(descr, map())

    if empty?(descr_map) do
      none()
    else
      map_split_on_key(descr_map.map, key)
      |> Enum.reduce(none(), fn typeof_key, union -> union(typeof_key, union) end)
      |> remove_not_set()
    end
  end

  @doc """
  Check that a key may be present.
  """
  def map_may_have_key?(descr, key) do
    compatible?(descr, map([{key, term()}], :open))
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
  end

  # Intersects two map literals; throws if their intersection is empty.
  defp map_literal_intersection(tag1, map1, tag2, map2) do
    default1 = if tag1 == :open, do: term_or_not_set(), else: not_set()
    default2 = if tag2 == :open, do: term_or_not_set(), else: not_set()

    # if any intersection of values is empty, the whole intersection is empty
    new_fields =
      (for {key, value_type} <- map1 do
         value_type2 = Map.get(map2, key, default2)
         t = intersection(value_type, value_type2)
         if empty?(t), do: throw(:empty), else: {key, t}
       end ++
         for {key, value_type} <- map2, not is_map_key(map1, key) do
           t = intersection(default1, value_type)
           if empty?(t), do: throw(:empty), else: {key, t}
         end)
      |> Map.new()

    case {tag1, tag2} do
      {:open, :open} -> {:open, new_fields}
      _ -> {:closed, new_fields}
    end
  end

  defp map_difference(dnf1, dnf2) do
    Enum.reduce(dnf2, dnf1, fn {tag2, fields2, negs2}, dnf1 ->
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
  end

  # Emptiness checking for maps. Short-circuits if it finds a non-empty map literal in the union.
  defp map_empty?(dnf) do
    try do
      for {tag, pos, negs} <- dnf do
        map_empty?(tag, pos, negs)
      end

      true
    catch
      :not_empty -> false
    end
  end

  defp map_empty?(_, _, []), do: throw(:not_empty)
  defp map_empty?(_, _, [{:open, neg_fields} | _]) when neg_fields == %{}, do: true
  defp map_empty?(:open, fs, [{:closed, _} | negs]), do: map_empty?(:open, fs, negs)

  defp map_empty?(tag, fields, [{neg_tag, neg_fields} | negs]) do
    try do
      # keys that are present in the negative map, but not in the positive one
      for {neg_key, neg_type} <- neg_fields, not is_map_key(fields, neg_key) do
        cond do
          # key is required, and the positive map is closed: empty intersection
          tag == :closed and not optional?(neg_type) ->
            throw(:no_intersection)

          # if the positive map is open
          tag == :open ->
            diff = difference(term_or_not_set(), neg_type)
            empty?(diff) or map_empty?(tag, Map.put(fields, neg_key, diff), negs)
        end
      end

      for {key, type} <- fields do
        case neg_fields do
          %{^key => neg_type} ->
            diff = difference(type, neg_type)
            empty?(diff) or map_empty?(tag, Map.put(fields, key, diff), negs)

          %{} ->
            if neg_tag == :closed and not optional?(type) do
              throw(:no_intersection)
            else
              # an absent key in a open negative map can be ignored
              default2 = if neg_tag == :open, do: @term_or_not_set, else: @not_set
              diff = difference(type, default2)
              empty?(diff) or map_empty?(tag, Map.put(fields, key, diff), negs)
            end
        end
      end
    catch
      :no_intersection -> map_empty?(tag, fields, negs)
    end
  end

  # Takes a map bdd and a key, and returns an equivalent dnf of pairs, in which
  # the type of the key in the map can be found in the first element of the pair.
  # See `split_line_on_key/5`.
  defp map_split_on_key(dnf, key) do
    Enum.flat_map(dnf, fn {tag, fields, negs} ->
      {fst, snd} =
        case single_split({tag, fields}, key) do
          # { .. } the open map in a positive intersection can be ignored
          :no_split -> {term_or_not_set(), term_or_not_set()}
          # {typeof l, rest} is added to the positive accumulator
          {value_type, rest_of_map} -> {value_type, rest_of_map}
        end

      case split_negative(negs, key, []) do
        :no_split -> []
        negative -> make_pairs_disjoint(negative) |> eliminate_negations(fst, snd)
      end
    end)
  end

  # Splits a map literal on a key. This means that given a map literal, compute
  # the pair of types `{value_type, rest_of_map}` where `value_type` is the type
  # associated with `key`, and `rest_of_map` is obtained by removing `key`.
  defp single_split({tag, fields}, key) do
    {value_type, rest_of_map} = Map.pop(fields, key)

    cond do
      value_type != nil -> {value_type, map_descr(tag, rest_of_map)}
      tag == :closed -> {not_set(), map_descr(tag, rest_of_map)}
      # case where there is an open map with no keys { .. }
      map_size(fields) == 0 -> :no_split
      true -> {term_or_not_set(), map_descr(tag, rest_of_map)}
    end
  end

  # Given a line, that is, a list `positive` of map literals and `negative` of
  # negated map literals, and a `key`, splits every map literal on the key and
  # outputs a DNF of pairs, that is, a list (union) of (intersections) of pairs.
  defp split_negative([], _key, neg_acc), do: neg_acc

  defp split_negative([map_literal | negative], key, neg_acc) do
    case single_split(map_literal, key) do
      # an intersection that contains %{...} is empty, so we discard it entirely
      :no_split ->
        :no_split

      # {typeof l, rest_of_map} is added to the negative accumulator
      {value_type, rest_of_map} ->
        split_negative(negative, key, [{value_type, rest_of_map} | neg_acc])
    end
  end

  defp map_to_quoted(dnf) do
    map_normalize(dnf)
    |> case do
      [] -> []
      x -> Enum.map(x, &map_line_to_quoted/1) |> Enum.reduce(&{:or, [], [&2, &1]}) |> List.wrap()
    end
  end

  # Use heuristics to normalize a map dnf for pretty printing.
  # TODO: eliminate some simple negations, those which have only zero or one key in common.
  defp map_normalize(dnf) do
    dnf
    |> Enum.filter(&(not map_empty?([&1])))
    |> Enum.map(fn {tag, fields, negs} ->
      {tag, fields, filter_empty_negations(tag, fields, negs)}
    end)
  end

  # Adapted from `map_empty?` to remove useless negations.
  defp filter_empty_negations(_tag, _fields, []), do: []

  defp filter_empty_negations(tag, fields, [{neg_tag, neg_fields} | negs]) do
    try do
      for {neg_key, neg_type} when not is_map_key(fields, neg_key) <- neg_fields do
        # key is required, and the positive map is closed: empty intersection
        if tag == :closed and not optional?(neg_type), do: throw(:no_intersection)
      end

      for {key, type} when not is_map_key(neg_fields, key) <- fields,
          # key is required, and the negative map is closed: empty intersection
          not optional?(type) and neg_tag == :closed do
        throw(:no_intersection)
      end

      [{neg_tag, neg_fields} | filter_empty_negations(tag, fields, negs)]
    catch
      :no_intersection -> filter_empty_negations(tag, fields, negs)
    end
  end

  defp map_line_to_quoted({tag, positive_map, negative_maps}) do
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

  def map_literal_to_quoted({tag, fields}) do
    case tag do
      :closed -> {:%{}, [], fields_to_quoted(tag, fields)}
      :open -> {:%{}, [], [{:..., [], nil} | fields_to_quoted(tag, fields)]}
    end
  end

  defp fields_to_quoted(tag, map) do
    for {key, type} <- Enum.sort(map),
        not (tag == :open and optional?(type) and term?(type)) do
      cond do
        optional?(type) and empty?(type) -> {literal(key), {:not_set, [], []}}
        optional?(type) -> {literal(key), {:if_set, [], [to_quoted(type)]}}
        true -> {literal(key), to_quoted(type)}
      end
    end
  end

  ## Pairs
  #
  # To simplify disjunctive normal forms of e.g., map types, it is useful to
  # convert them into disjunctive normal forms of pairs of types, and define
  # normalization algorithms on pairs.

  # Takes a DNF of pairs and simplifies it into a equivalent single list (union)
  # of type pairs. The `term` argument should be either `@term_or_not_set` (for
  # map value types) or `@term` in general.
  # Remark: all lines of a pair dnf are naturally disjoint, because choosing a
  # different edge means intersection with a literal or its negation.

  # A line is a list of pairs `{positive, negative}` where `positive` is a list of
  # literals and `negative` is a list of negated literals. Positive pairs can
  # all be intersected component-wise. Negative ones are eliminated iteratively.

  # Eliminates negations from `{t, s} and not negative` where `negative` is a
  # union of pairs disjoint on their first component.
  # Formula:
  #   {t, s} and not (union<i=1..n> {t_i, s_i})
  #       = union<i=1..n> {t and t_i, s and not s_i}
  #            or {t and not (union{i=1..n} t_i), s}
  # This eliminates all top-level negations and produces a union of pairs that
  # are disjoint on their first component.
  defp eliminate_negations(negative, t, s) do
    {pair_union, diff_of_t_i} =
      Enum.reduce(
        negative,
        {[], t},
        fn {t_i, s_i}, {accu, diff_of_t_i} ->
          i = intersection(t, t_i)

          if empty?(i) do
            {accu, diff_of_t_i}
          else
            diff_of_t_i = difference(diff_of_t_i, t_i)
            s_diff = difference(s, s_i)

            if empty?(s_diff),
              do: {accu, diff_of_t_i},
              else: {[i | accu], diff_of_t_i}
          end
        end
      )

    [diff_of_t_i | pair_union]
  end

  # Inserts a pair of types {fst, snd} into a list of pairs that are disjoint
  # on their first component. The invariant on `acc` is that its elements are
  # two-to-two disjoint with the first argument's `pairs`.
  #
  # To insert {fst, snd} into a disjoint pairs list, we go through the list to find
  # each pair whose first element has a non-empty intersection with `fst`. Then
  # we decompose {fst, snd} over each such pair to produce disjoint ones, and add
  # the decompositions into the accumulator.
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

  # Makes a union of pairs into an equivalent union of disjoint pairs.
  defp make_pairs_disjoint(pairs) do
    Enum.reduce(pairs, [], fn {t1, t2}, acc -> add_pair_to_disjoint_list(acc, t1, t2, []) end)
  end
end
