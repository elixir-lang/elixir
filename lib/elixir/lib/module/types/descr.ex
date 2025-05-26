# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

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
  @bit_top (1 <<< 7) - 1
  @bit_number @bit_integer ||| @bit_float

  @fun_top :fun_top
  @atom_top {:negation, :sets.new(version: 2)}
  @map_top [{:open, %{}, []}]
  @non_empty_list_top [{:term, :term, []}]
  @tuple_top [{:open, [], []}]
  @map_empty [{:closed, %{}, []}]

  @none %{}
  @term %{
    bitmap: @bit_top,
    atom: @atom_top,
    tuple: @tuple_top,
    map: @map_top,
    list: @non_empty_list_top,
    fun: @fun_top
  }
  @empty_list %{bitmap: @bit_empty_list}
  @not_non_empty_list Map.delete(@term, :list)
  @not_list Map.replace!(@not_non_empty_list, :bitmap, @bit_top - @bit_empty_list)

  @empty_intersection [0, @none]
  @empty_difference [0, []]

  # Type definitions

  defguard is_descr(descr) when is_map(descr) or descr == :term

  defp descr_key?(:term, _key), do: true
  defp descr_key?(descr, key), do: is_map_key(descr, key)

  def dynamic(), do: %{dynamic: :term}
  def none(), do: @none
  def term(), do: :term

  @compile {:inline, unfold: 1}
  defp unfold(:term), do: unfolded_term()
  defp unfold(other), do: other
  defp unfolded_term, do: @term

  def atom(as), do: %{atom: atom_new(as)}
  def atom(), do: %{atom: @atom_top}
  def binary(), do: %{bitmap: @bit_binary}
  def closed_map(pairs), do: map_descr(:closed, pairs)
  def empty_list(), do: %{bitmap: @bit_empty_list}
  def empty_map(), do: %{map: @map_empty}
  def integer(), do: %{bitmap: @bit_integer}
  def float(), do: %{bitmap: @bit_float}
  def list(type), do: list_descr(type, @empty_list, true)
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

  def fun(), do: %{fun: @fun_top}

  @doc """
  Creates a function type with the given arguments and return type.

  ## Examples

      fun([integer()], atom())
      #=> (integer -> atom)

      fun([integer(), float()], boolean())
      #=> (integer, float -> boolean)
  """
  def fun(args, return) when is_list(args), do: fun_descr(args, return)

  @doc """
  Creates the top function type for the given arity,
  where all arguments are none() and return is term().

  ## Examples

      fun(1)
      #=> (none -> term)

      fun(2)
      #=> Creates (none, none) -> term
  """
  def fun(arity) when is_integer(arity) and arity >= 0 do
    fun(List.duplicate(none(), arity), term())
  end

  @doc """
  Tuples represent function domains, using unions to combine parameters.

  Example: for functions (integer, float ->:ok) and (float, integer -> :error)
  domain isn't {integer|float,integer|float} as that would incorrectly accept {float,float}
  Instead, it is {integer,float} or {float,integer}

  Made public for testing.
  """
  def domain_descr(types) when is_list(types), do: tuple(types)

  ## Optional

  # `not_set()` is a special base type that represents an not_set field in a map.
  # E.g., `%{a: integer(), b: not_set(), ...}` represents a map with an integer
  # field `a` and an not_set field `b`, and possibly other fields.
  #
  # The `if_set()` modifier is syntactic sugar for specifying the key as a union
  # of the key type and `not_set()`. For example, `%{:foo => if_set(integer())}`
  # is equivalent to `%{:foo => integer() or not_set()}`.
  #
  # `not_set()` has no meaning outside of map types.

  @not_set %{optional: 1}
  @term_or_optional Map.put(@term, :optional, 1)
  @term_or_dynamic_optional Map.put(@term, :dynamic, %{optional: 1})
  @not_atom_or_optional Map.delete(@term_or_optional, :atom)

  def not_set(), do: @not_set
  def if_set(:term), do: term_or_optional()
  def if_set(type), do: Map.put(type, :optional, 1)
  defp term_or_optional(), do: @term_or_optional

  @compile {:inline,
            keep_optional: 1, remove_optional: 1, remove_optional_static: 1, optional_to_term: 1}
  defp keep_optional(descr) do
    case descr do
      %{dynamic: %{optional: 1}} -> %{dynamic: %{optional: 1}}
      %{optional: 1} -> %{optional: 1}
      _ -> %{}
    end
  end

  defp remove_optional(descr) do
    case descr do
      %{dynamic: %{optional: _} = dynamic} when map_size(dynamic) == 1 ->
        Map.delete(descr, :dynamic)

      %{dynamic: %{optional: _} = dynamic} ->
        %{descr | dynamic: Map.delete(dynamic, :optional)}

      _ ->
        remove_optional_static(descr)
    end
  end

  defp remove_optional_static(descr) do
    case descr do
      %{optional: _} -> Map.delete(descr, :optional)
      descr -> descr
    end
  end

  defp optional_to_term(descr) do
    case descr do
      %{dynamic: %{optional: _}} -> @term_or_dynamic_optional
      %{optional: _} -> term_or_optional()
      _ -> :term
    end
  end

  defp pop_optional_static(:term), do: {false, :term}

  defp pop_optional_static(type) do
    case :maps.take(:optional, type) do
      :error -> {false, type}
      {1, type} -> {true, type}
    end
  end

  ## Set operations

  @doc """
  Returns true if the type has a gradual part.
  """
  def gradual?(:term), do: false
  def gradual?(descr), do: is_map_key(descr, :dynamic)

  @doc """
  Returns true if the type only has a gradual part.
  """
  def only_gradual?(%{dynamic: _} = descr), do: map_size(descr) == 1
  def only_gradual?(_), do: false

  @doc """
  Make a whole type dynamic.

  It is an optimized version of `intersection(dynamic(), type)`.
  """
  @compile {:inline, dynamic: 1}
  def dynamic(descr) do
    case descr do
      %{dynamic: dynamic} -> %{dynamic: dynamic}
      _ -> %{dynamic: descr}
    end
  end

  @compile {:inline, maybe_union: 2}
  defp maybe_union(nil, _fun), do: nil
  defp maybe_union(descr, fun), do: union(descr, fun.())

  @doc """
  Computes the union of two descrs.
  """
  def union(:term, other), do: optional_to_term(other)
  def union(other, :term), do: optional_to_term(other)
  def union(none, other) when none == @none, do: other
  def union(other, none) when none == @none, do: other

  def union(left, right) do
    left = unfold(left)
    right = unfold(right)
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

  @compile {:inline, union_static: 2}
  defp union_static(left, right) do
    symmetrical_merge(left, right, &union/3)
  end

  defp union(:atom, v1, v2), do: atom_union(v1, v2)
  defp union(:bitmap, v1, v2), do: v1 ||| v2
  defp union(:dynamic, v1, v2), do: dynamic_union(v1, v2)
  defp union(:list, v1, v2), do: list_union(v1, v2)
  defp union(:map, v1, v2), do: map_union(v1, v2)
  defp union(:optional, 1, 1), do: 1
  defp union(:tuple, v1, v2), do: tuple_union(v1, v2)
  defp union(:fun, v1, v2), do: fun_union(v1, v2)

  @doc """
  Computes the intersection of two descrs.
  """
  def intersection(:term, other), do: remove_optional(other)
  def intersection(other, :term), do: remove_optional(other)
  def intersection(%{dynamic: :term}, other), do: dynamic(remove_optional(other))
  def intersection(other, %{dynamic: :term}), do: dynamic(remove_optional(other))

  def intersection(left, right) do
    left = unfold(left)
    right = unfold(right)
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

  @compile {:inline, intersection_static: 2}
  defp intersection_static(left, right) do
    symmetrical_intersection(left, right, &intersection/3)
  end

  # Returning 0 from the callback is taken as none() for that subtype.
  defp intersection(:atom, v1, v2), do: atom_intersection(v1, v2)
  defp intersection(:bitmap, v1, v2), do: v1 &&& v2
  defp intersection(:dynamic, v1, v2), do: dynamic_intersection(v1, v2)
  defp intersection(:list, v1, v2), do: list_intersection(v1, v2)
  defp intersection(:map, v1, v2), do: map_intersection(v1, v2)
  defp intersection(:optional, 1, 1), do: 1
  defp intersection(:tuple, v1, v2), do: tuple_intersection(v1, v2)
  defp intersection(:fun, v1, v2), do: fun_intersection(v1, v2)

  @doc """
  Computes the difference between two types.
  """
  def difference(left, :term), do: keep_optional(left)

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
  defp difference_static(left, :term), do: keep_optional(left)

  defp difference_static(left, right) do
    iterator_difference_static(:maps.next(:maps.iterator(unfold(right))), unfold(left))
  end

  defp iterator_difference_static({key, v2, iterator}, map) do
    acc =
      case map do
        %{^key => v1} ->
          value = difference(key, v1, v2)

          if value in @empty_difference do
            Map.delete(map, key)
          else
            %{map | key => value}
          end

        %{} ->
          map
      end

    iterator_difference_static(:maps.next(iterator), acc)
  end

  defp iterator_difference_static(:none, map), do: map

  # Returning 0 from the callback is taken as none() for that subtype.
  defp difference(:atom, v1, v2), do: atom_difference(v1, v2)
  defp difference(:bitmap, v1, v2), do: v1 - (v1 &&& v2)
  defp difference(:dynamic, v1, v2), do: dynamic_difference(v1, v2)
  defp difference(:list, v1, v2), do: list_difference(v1, v2)
  defp difference(:map, v1, v2), do: map_difference(v1, v2)
  defp difference(:optional, 1, 1), do: 0
  defp difference(:tuple, v1, v2), do: tuple_difference(v1, v2)
  defp difference(:fun, v1, v2), do: fun_difference(v1, v2)

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
    case :maps.get(:dynamic, descr, _default = descr) do
      :term ->
        false

      value when value == @none ->
        true

      descr ->
        not Map.has_key?(descr, :atom) and
          not Map.has_key?(descr, :bitmap) and
          not Map.has_key?(descr, :optional) and
          (not Map.has_key?(descr, :map) or map_empty?(descr.map)) and
          (not Map.has_key?(descr, :list) or list_empty?(descr.list)) and
          (not Map.has_key?(descr, :tuple) or tuple_empty?(descr.tuple)) and
          (not Map.has_key?(descr, :fun) or fun_empty?(descr.fun))
    end
  end

  # For atom, bitmap, and optional, if the key is present,
  # then they are not empty,
  defp empty_key?(:map, value), do: map_empty?(value)
  defp empty_key?(:list, value), do: list_empty?(value)
  defp empty_key?(:tuple, value), do: tuple_empty?(value)
  defp empty_key?(_, _value), do: false

  @doc """
  Converts a descr to its quoted representation.

  ## Options

    * `:collapse_structs` - do not show struct fields that match
      their default type
  """
  def to_quoted(descr, opts \\ []) do
    if term_type?(descr) do
      {:term, [], []}
    else
      # Dynamic always come first for visibility
      {dynamic, descr} =
        case :maps.take(:dynamic, descr) do
          :error -> {[], descr}
          {:term, descr} -> {to_quoted(:dynamic, :term, opts), descr}
          {dynamic, descr} -> {to_quoted(:dynamic, difference(dynamic, descr), opts), descr}
        end

      # Merge empty list and list together if they both exist
      {extra, descr} =
        case descr do
          %{list: list, bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0 ->
            descr = descr |> Map.delete(:list) |> Map.replace!(:bitmap, bitmap - @bit_empty_list)
            {list_to_quoted(list, true, opts), descr}

          %{} ->
            {[], descr}
        end

      unions =
        dynamic ++
          Enum.sort(
            extra ++ Enum.flat_map(descr, fn {key, value} -> to_quoted(key, value, opts) end)
          )

      case unions do
        [] -> {:none, [], []}
        unions -> Enum.reduce(unions, &{:or, [], [&2, &1]})
      end
    end
  end

  defp to_quoted(:atom, val, _opts), do: atom_to_quoted(val)
  defp to_quoted(:bitmap, val, _opts), do: bitmap_to_quoted(val)
  defp to_quoted(:dynamic, descr, opts), do: dynamic_to_quoted(descr, opts)
  defp to_quoted(:map, dnf, opts), do: map_to_quoted(dnf, opts)
  defp to_quoted(:list, dnf, opts), do: list_to_quoted(dnf, false, opts)
  defp to_quoted(:tuple, dnf, opts), do: tuple_to_quoted(dnf, opts)
  defp to_quoted(:fun, dnf, opts), do: fun_to_quoted(dnf, opts)

  @doc """
  Converts a descr to its quoted string representation.
  """
  def to_quoted_string(descr, opts \\ []) do
    descr
    |> to_quoted(opts)
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
  def subtype?(left, right) do
    left = unfold(left)
    right = unfold(right)
    is_grad_left = gradual?(left)
    is_grad_right = gradual?(right)

    cond do
      is_grad_left and not is_grad_right ->
        left_dynamic = Map.get(left, :dynamic)
        subtype_static?(left_dynamic, right)

      is_grad_right and not is_grad_left ->
        right_static = Map.delete(right, :dynamic)
        subtype_static?(left, right_static)

      true ->
        subtype_static?(left, right)
    end
  end

  defp subtype_static?(same, same), do: true
  defp subtype_static?(left, right), do: empty?(difference_static(left, right))

  @doc """
  Check if a type is equal to another.

  It is currently not optimized. Only to be used in tests.
  """
  def equal?(left, right) do
    left == right or (subtype?(left, right) and subtype?(right, left))
  end

  @doc """
  Check if two types are disjoint.

  This reimplements intersection/2 but aborts as it finds a disjoint part.
  """
  def disjoint?(:term, other), do: empty?(remove_optional(other))
  def disjoint?(other, :term), do: empty?(remove_optional(other))
  def disjoint?(%{dynamic: :term}, other), do: empty?(remove_optional(other))
  def disjoint?(other, %{dynamic: :term}), do: empty?(remove_optional(other))

  def disjoint?(left, right) do
    left = unfold(left)
    right = unfold(right)
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(right, :dynamic, right)
        not non_disjoint_intersection?(left, right_with_dynamic)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(left, :dynamic, left)
        not non_disjoint_intersection?(left_with_dynamic, right)

      true ->
        not non_disjoint_intersection?(left, right)
    end
  end

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
  def compatible?(left, right) do
    {left_dynamic, left_static} =
      case left do
        :term -> {:term, :term}
        _ -> Map.pop(left, :dynamic, left)
      end

    right_dynamic =
      case right do
        %{dynamic: dynamic} -> dynamic
        _ -> right
      end

    if empty?(left_static) do
      not disjoint?(left_dynamic, right_dynamic)
    else
      subtype_static?(left_static, right_dynamic)
    end
  end

  @doc """
  Returns the intersection between two types
  only if they are compatible. Otherwise returns `:error`.

  This finds the intersection between the arguments and the
  domain of a function. It is used to refine dynamic types
  as we traverse the program.
  """
  def compatible_intersection(left, right) do
    {left_dynamic, left_static} =
      case left do
        :term -> {:term, :term}
        _ -> Map.pop(left, :dynamic, left)
      end

    right_dynamic =
      case right do
        %{dynamic: dynamic} -> dynamic
        _ -> right
      end

    cond do
      empty?(left_static) ->
        dynamic = intersection_static(unfold(left_dynamic), unfold(right_dynamic))
        if empty?(dynamic), do: {:error, left}, else: {:ok, dynamic(dynamic)}

      subtype_static?(left_static, right_dynamic) ->
        dynamic = intersection_static(unfold(left_dynamic), unfold(right_dynamic))
        {:ok, union(dynamic(dynamic), left_static)}

      true ->
        {:error, left}
    end
  end

  @doc """
  Optimized version of `not empty?(term(), type)`.
  """
  def term_type?(:term), do: true
  def term_type?(descr), do: subtype_static?(unfolded_term(), Map.delete(descr, :dynamic))

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

  ## Bitmaps

  defp bitmap_to_quoted(val) do
    pairs =
      [
        binary: @bit_binary,
        empty_list: @bit_empty_list,
        integer: @bit_integer,
        float: @bit_float,
        pid: @bit_pid,
        port: @bit_port,
        reference: @bit_reference
      ]

    for {type, mask} <- pairs,
        (mask &&& val) !== 0,
        do: {type, [], []}
  end

  ## Funs

  @doc """
  Checks if a function type with the specified arity exists in the descriptor.

  1. If there is no dynamic component:
     - The static part must be a non-empty function type of the given arity

  2. If there is a dynamic component:
     - Either the static part is a non-empty function type of the given arity, or
     - The static part is empty and the dynamic part contains functions of the given arity
  """
  def fun_fetch(:term, _arity), do: :error

  def fun_fetch(%{} = descr, arity) when is_integer(arity) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if not empty?(descr) and fun_only?(descr, arity), do: :ok, else: :error

      {dynamic, static} ->
        empty_static? = empty?(static)

        cond do
          not empty_static? -> if fun_only?(static, arity), do: :ok, else: :error
          empty_static? and not empty?(intersection(dynamic, fun(arity))) -> :ok
          true -> :error
        end
    end
  end

  defp fun_only?(descr), do: empty?(Map.delete(descr, :fun))
  defp fun_only?(descr, arity), do: empty?(difference(descr, fun(arity)))

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

  @false_or_nil_atoms [
    :sets.from_list([false, nil], version: 2),
    :sets.from_list([nil], version: 2),
    :sets.from_list([false], version: 2)
  ]

  @doc """
  Compute the truthiness of an element.

  It is either :undefined, :always_true, or :always_false.
  """
  def truthiness(:term), do: :undefined

  def truthiness(%{} = descr) do
    descr = Map.get(descr, :dynamic, descr)

    case descr do
      :term ->
        :undefined

      %{atom: {:union, set}}
      when map_size(descr) == 1 and set in @false_or_nil_atoms ->
        :always_false

      %{atom: {:union, set}}
      when map_size(descr) == 1 and not is_map_key(set, false) and not is_map_key(set, nil) ->
        :always_true

      %{atom: {:negation, %{nil => _, false => _}}} ->
        :always_true

      %{atom: _} ->
        :undefined

      _ when map_size(descr) == 0 ->
        :undefined

      _ ->
        :always_true
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
      entries =
        :sets.subtract(a, @boolset)
        |> :sets.to_list()
        |> Enum.map(&literal_to_quoted/1)

      [{:boolean, [], []} | entries]
    else
      :sets.to_list(a)
      |> Enum.map(&literal_to_quoted/1)
    end
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

  ## Functions

  # Function types are represented using Binary Decision Diagrams (BDDs) for efficient
  # handling of unions, intersections, and negations.
  #
  # Currently, they only represent weak types. It is yet to be decided how all of the
  # types will be encoded in the descr.

  ### Key concepts:

  # * BDD structure: A tree with function nodes and :fun_top/:fun_bottom leaves.
  #   Paths to :fun_top represent valid function types. Nodes are positive when
  #   following a left branch (e.g. (int, float -> bool) and negative otherwise.

  # * Function variance:
  #   - Contravariance in arguments: If s <: t, then (t → r) <: (s → r)
  #   - Covariance in returns: If s <: t, then (u → s) <: (u → t)

  # * Representation:
  #   - fun(): Top function type (leaf 1)
  #   - Function literals: {[t1, ..., tn], t} where [t1, ..., tn] are argument types and t is return type
  #   - Normalized form for function applications: {domain, arrows, arity} is produced by `fun_normalize/1`

  # * Examples:
  #   - fun([integer()], atom()): A function from integer to atom
  #   - intersection(fun([integer()], atom()), fun([float()], boolean())): A function handling both signatures

  # Note: Function domains are expressed as tuple types. We use separate representations
  # rather than unary functions with tuple domains to handle special cases like representing
  # functions of a specific arity (e.g., (none,none->term) for arity 2).
  defp fun_new(inputs, output), do: {{inputs, output}, :fun_top, :fun_bottom}

  # Creates a function type from a list of inputs and an output
  # where the inputs and/or output may be dynamic.
  #
  # For function (t → s) with dynamic components:
  # - Static part: (upper_bound(t) → lower_bound(s))
  # - Dynamic part: dynamic(lower_bound(t) → upper_bound(s))
  #
  # When handling dynamic types:
  # - `upper_bound(t)` extracts the upper bound (most general type) of a gradual type.
  #   For `dynamic(integer())`, it is `integer()`.
  # - `lower_bound(t)` extracts the lower bound (most specific type) of a gradual type.
  defp fun_descr(args, output) when is_list(args) do
    dynamic_arguments? = are_arguments_dynamic?(args)
    dynamic_output? = match?(%{dynamic: _}, output)

    if dynamic_arguments? or dynamic_output? do
      input_static = if dynamic_arguments?, do: materialize_arguments(args, :up), else: args
      input_dynamic = if dynamic_arguments?, do: materialize_arguments(args, :down), else: args

      output_static = if dynamic_output?, do: lower_bound(output), else: output
      output_dynamic = if dynamic_output?, do: upper_bound(output), else: output

      %{
        fun: fun_new(input_static, output_static),
        dynamic: %{fun: fun_new(input_dynamic, output_dynamic)}
      }
    else
      # No dynamic components, use standard function type
      %{fun: fun_new(args, output)}
    end
  end

  # Gets the upper bound of a gradual type.
  defp upper_bound(%{dynamic: dynamic}), do: dynamic
  defp upper_bound(static), do: static

  # Gets the lower bound of a gradual type.
  defp lower_bound(:term), do: :term
  defp lower_bound(type), do: Map.delete(type, :dynamic)

  @doc """
  Calculates the domain of a function type.

  For a function type, the domain is the set of valid input types.

  Returns:
  - `:badfun` if the type is not a function type
  - A tuple type representing the domain for valid function types

  Handles both static and dynamic function types:
  1. For static functions, returns their exact domain
  2. For dynamic functions, computes domain based on both static and dynamic parts

  Formula is dom(t) = dom(upper_bound(t)) ∪ dynamic(dom(lower_bound(t))).
  See Definition 6.15 in https://vlanvin.fr/papers/thesis.pdf.

  ## Examples
      iex> fun_domain(fun([integer()], atom()))
      domain_repr([integer()])

      iex> fun_domain(fun([integer(), float()], boolean()))
      domain_repr([integer(), float()])
  """
  def fun_domain(:term), do: :badfun

  def fun_domain(type) do
    result =
      case :maps.take(:dynamic, type) do
        :error ->
          # Static function type
          with true <- fun_only?(type), {:ok, domain} <- fun_domain_static(type) do
            domain
          else
            _ -> :badfun
          end

        {dynamic, static} when static == @none ->
          with {:ok, domain} <- fun_domain_static(dynamic), do: domain

        {dynamic, static} ->
          with true <- fun_only?(static),
               {:ok, static_domain} <- fun_domain_static(static),
               {:ok, dynamic_domain} <- fun_domain_static(dynamic) do
            union(dynamic_domain, dynamic(static_domain))
          else
            _ -> :badfun
          end
      end

    case result do
      :badfun -> :badfun
      result -> if empty?(result), do: :badfun, else: result
    end
  end

  # Returns {:ok, domain} if the domain of the static type is well-defined.
  # For that, it has to contain a non-empty function type.
  # Otherwise, returns :badfun.
  defp fun_domain_static(%{fun: bdd}) do
    case fun_normalize(bdd) do
      {domain, _, _} -> {:ok, domain}
      _ -> {:ok, none()}
    end
  end

  defp fun_domain_static(:term), do: :badfun
  defp fun_domain_static(%{}), do: {:ok, none()}

  @doc """
  Applies a function type to a list of argument types.

  Returns the result type if the application is valid, or `:badarg` if not.

  Handles both static and dynamic function types:
  1. For static functions: checks exact argument types
  2. For dynamic functions: computes result based on both static and dynamic parts
  3. For mixed static/dynamic: computes all valid combinations

  # Function application formula for dynamic types:
  #   τ◦τ′ = (lower_bound(τ) ◦ upper_bound(τ′)) ∨ (dynamic(upper_bound(τ) ◦ lower_bound(τ′)))
  #
  # Where:
  # - τ is a dynamic function type
  # - τ′ are the arguments
  # - ◦ is function application
  #
  # For more details, see Definition 6.15 in https://vlanvin.fr/papers/thesis.pdf

  ## Examples
      iex> fun_apply(fun([integer()], atom()), [integer()])
      atom()

      iex> fun_apply(fun([integer()], atom()), [float()])
      :badarg

      iex> fun_apply(fun([dynamic()], atom()), [dynamic()])
      atom()
  """
  def fun_apply(fun, arguments) do
    if empty?(domain_descr(arguments)) do
      :badarg
    else
      case :maps.take(:dynamic, fun) do
        :error -> fun_apply_with_strategy(fun, nil, arguments)
        {fun_dynamic, fun_static} -> fun_apply_with_strategy(fun_static, fun_dynamic, arguments)
      end
    end
  end

  defp fun_apply_with_strategy(fun_static, fun_dynamic, arguments) do
    args_dynamic? = are_arguments_dynamic?(arguments)

    # For non-dynamic function and arguments, just return the static result
    if fun_dynamic == nil and not args_dynamic? do
      with {:ok, type} <- fun_apply_static(fun_static, arguments), do: type
    else
      # For dynamic cases, combine static and dynamic results
      {static_args, dynamic_args} =
        if args_dynamic?,
          do: {materialize_arguments(arguments, :up), materialize_arguments(arguments, :down)},
          else: {arguments, arguments}

      dynamic_fun = fun_dynamic || fun_static

      with {:ok, res1} <- fun_apply_static(fun_static, static_args),
           {:ok, res2} <- fun_apply_static(dynamic_fun, dynamic_args) do
        union(res1, dynamic(res2))
      else
        _ -> :badarg
      end
    end
  end

  # Materializes arguments using the specified direction (up or down)
  defp materialize_arguments(arguments, :up), do: Enum.map(arguments, &upper_bound/1)
  defp materialize_arguments(arguments, :down), do: Enum.map(arguments, &lower_bound/1)

  defp are_arguments_dynamic?(arguments), do: Enum.any?(arguments, &match?(%{dynamic: _}, &1))

  defp fun_apply_static(%{fun: fun_bdd}, arguments) do
    type_args = domain_descr(arguments)

    case fun_normalize(fun_bdd) do
      {domain, arrows, arity} when arity == length(arguments) ->
        cond do
          empty?(type_args) ->
            # Opti: short-circuits when inner loop is none() or outer loop is term()
            result =
              Enum.reduce_while(arrows, none(), fn intersection_of_arrows, acc ->
                Enum.reduce_while(intersection_of_arrows, term(), fn
                  {_dom, _ret}, acc when acc == @none -> {:halt, acc}
                  {_dom, ret}, acc -> {:cont, intersection(acc, ret)}
                end)
                |> case do
                  :term -> {:halt, :term}
                  inner -> {:cont, union(inner, acc)}
                end
              end)

            {:ok, result}

          subtype?(type_args, domain) ->
            result =
              Enum.reduce(arrows, none(), fn intersection_of_arrows, acc ->
                aux_apply(acc, type_args, term(), intersection_of_arrows)
              end)

            {:ok, result}

          true ->
            :badarg
        end

      {_, _, arity} ->
        {:badarity, arity}

      :badfun ->
        :badfun
    end
  end

  # Helper function for function application that handles the application of
  # function arrows to input types.

  # This function recursively processes a list of function arrows (an intersection),
  # applying each arrow to the input type and accumulating the result.

  # ## Parameters

  # - result: The accumulated result type so far
  # - input: The input type being applied to the function
  # - rets_reached: The intersection of return types reached so far
  # - arrow_intersections: The list of function arrows to process

  # For more details, see Definitions 2.20 or 6.11 in https://vlanvin.fr/papers/thesis.pdf
  defp aux_apply(result, _input, rets_reached, []) do
    if subtype?(rets_reached, result), do: result, else: union(result, rets_reached)
  end

  defp aux_apply(result, input, returns_reached, [{dom, ret} | arrow_intersections]) do
    # Calculate the part of the input not covered by this arrow's domain
    dom_subtract = difference(input, domain_descr(dom))

    # Refine the return type by intersecting with this arrow's return type
    ret_refine = intersection(returns_reached, ret)

    # Phase 1: Domain partitioning
    # If the input is not fully covered by the arrow's domain, then the result type should be
    # _augmented_ with the outputs obtained by applying the remaining arrows to the non-covered
    # parts of the domain.
    #
    # e.g. (integer()->atom()) and (float()->pid()) when applied to number() should unite
    # both atoms and pids in the result.
    result =
      if empty?(dom_subtract) do
        result
      else
        aux_apply(result, dom_subtract, returns_reached, arrow_intersections)
      end

    # 2. Return type refinement
    # The result type is also refined (intersected) in the sense that, if several arrows match
    # the same part of the input, then the result type is an intersection of the return types of
    # those arrows.

    # e.g. (integer()->atom()) and (integer()->pid()) when applied to integer()
    # should result in (atom() ∩ pid()), which is none().
    aux_apply(result, input, ret_refine, arrow_intersections)
  end

  # Takes all the paths from the root to the leaves finishing with a 1,
  # and compile into tuples of positive and negative nodes. Positive nodes are
  # those followed by a left path, negative nodes are those followed by a right path.
  def fun_get(bdd), do: fun_get([], [], [], bdd)

  def fun_get(acc, pos, neg, bdd) do
    case bdd do
      :fun_bottom -> acc
      :fun_top -> [{pos, neg} | acc]
      {fun, left, right} -> fun_get(fun_get(acc, [fun | pos], neg, left), pos, [fun | neg], right)
    end
  end

  # Transforms a binary decision diagram (BDD) into the canonical form {domain, arrows, arity}:
  #
  # 1. **domain**: The union of all domains from positive functions in the BDD
  # 2. **arrows**: List of lists, where each inner list contains an intersection of function arrows
  # 3. **arity**: Function arity (number of parameters)
  #
  ## Return Values
  #
  # - `{domain, arrows, arity}` for valid function BDDs
  # - `:badfun` if the BDD represents an empty function type
  #
  # ## Internal Use
  #
  # This function is used internally by `fun_apply`, `fun_domain`, and others to
  # ensure consistent handling of function types in all operations.
  defp fun_normalize(bdd) do
    {domain, arrows, arity} =
      fun_get(bdd)
      |> Enum.reduce({term(), [], nil}, fn {pos_funs, neg_funs}, {domain, arrows, arity} ->
        # Skip empty function intersections
        if fun_empty?(pos_funs, neg_funs) do
          {domain, arrows, arity}
        else
          # Determine arity from first positive function or keep existing
          new_arity = arity || pos_funs |> List.first() |> elem(0) |> length()

          # Calculate domain from all positive functions
          path_domain =
            Enum.reduce(pos_funs, none(), fn {args, _}, acc ->
              union(acc, domain_descr(args))
            end)

          {intersection(domain, path_domain), [pos_funs | arrows], new_arity}
        end
      end)

    if arrows == [], do: :badfun, else: {domain, arrows, arity}
  end

  # Checks if a function type is empty.
  #
  # A function type is empty if:
  # 1. It is the empty type (0)
  # 2. For each path in the BDD (Binary Decision Diagram) from root to leaf ending in 1,
  #    the intersection of positive functions and the negation of negative functions is empty.
  #
  # For example:
  # - `fun(1)` is not empty
  # - `fun(1) and not fun(1)` is empty
  # - `fun(integer() -> atom()) and not fun(none() -> term())` is empty
  # - `fun(integer() -> atom()) and not fun(atom() -> integer())` is not empty
  defp fun_empty?(bdd) do
    case bdd do
      :fun_bottom -> true
      :fun_top -> false
      bdd -> fun_get(bdd) |> Enum.all?(fn {posits, negats} -> fun_empty?(posits, negats) end)
    end
  end

  # Checks if a function type represented by positive and negative function literals is empty.

  # A function type {positives, negatives} is empty if either:
  # 1. The positive functions have different arities (incompatible function types)
  # 2. There exists a negative function that negates the whole positive intersection

  ## Examples
  #
  # - `{[fun(1)], []}` is not empty
  # - `{[fun(1), fun(2)], []}` is empty (different arities)
  # - `{[fun(integer() -> atom())], [fun(none() -> term())]}` is empty
  # - `{[], _}` (representing the top function type fun()) is never empty
  #
  defp fun_empty?([], _), do: false

  defp fun_empty?(positives, negatives) do
    case fetch_arity_and_domain(positives) do
      # If there are functions with different arities in positives, then the function type is empty
      {:empty, _} ->
        true

      {positive_arity, positive_domain} ->
        # Check if any negative function negates the whole positive intersection
        # e.g. (integer() -> atom()) is negated by:
        #
        # * (none() -> term())
        # * (none() -> atom())
        # * (integer() -> term())
        # * (integer() -> atom())
        Enum.any?(negatives, fn {neg_arguments, neg_return} ->
          # Filter positives to only those with matching arity, then check if the negative
          # function's domain is a supertype of the positive domain and if the phi function
          # determines emptiness.
          length(neg_arguments) == positive_arity and
            subtype?(domain_descr(neg_arguments), positive_domain) and
            phi_starter(neg_arguments, negation(neg_return), positives)
        end)
    end
  end

  # Checks the list of arrows positives and returns {:empty, nil} if there exists two arrows with
  # different arities. Otherwise, it returns {arity, domain} with domain the union of all domains of
  # the arrows in positives.
  defp fetch_arity_and_domain(positives) do
    positives
    |> Enum.reduce_while({:empty, none()}, fn
      {args, _}, {:empty, _} ->
        {:cont, {length(args), domain_descr(args)}}

      {args, _}, {arity, dom} when length(args) == arity ->
        {:cont, {arity, union(dom, domain_descr(args))}}

      {_args, _}, {_arity, _} ->
        {:halt, {:empty, none()}}
    end)
  end

  # Implements the Φ (phi) function for determining function subtyping relationships.
  #
  # ## Algorithm
  #
  # For inputs t₁...tₙ, booleans b₁...bₙ, negated return type t, and set of arrow types P:
  #
  # Φ((b₁,t₁)...(bₙ,tₙ), (b,t), ∅) = (∃j ∈ [1,n]. bⱼ and tⱼ ≤ ∅) ∨ (b and t ≤ ∅)
  #
  # Φ((b₁,t₁)...(bₙ,tₙ), t, {(t'₁...t'ₙ) → t'} ∪ P) =
  #       Φ((b₁,t₁)...(bₙ,tₙ), (true,t ∧ t'), P) ∧
  #         ∀j ∈ [1,n]. Φ((b₁,t₁)...(true,tⱼ∖t'ⱼ)...(bₙ,tₙ), (b,t), P)
  #
  # Returns true if the intersection of the positives is a subtype of (t1,...,tn)->(not t).
  #
  # See [Castagna and Lanvin (2024)](https://arxiv.org/abs/2408.14345), Theorem 4.2.
  defp phi_starter(arguments, return, positives) do
    n = length(arguments)
    # Arity mismatch: if there is one positive function with a different arity,
    # then it cannot be a subtype of the (arguments->type) functions.
    if Enum.any?(positives, fn {args, _ret} -> length(args) != n end) do
      false
    else
      arguments = Enum.map(arguments, &{false, &1})
      phi(arguments, {false, return}, positives)
    end
  end

  defp phi(args, {b, t}, []) do
    Enum.any?(args, fn {bool, typ} -> bool and empty?(typ) end) or (b and empty?(t))
  end

  defp phi(args, {b, ret}, [{arguments, return} | rest_positive]) do
    phi(args, {true, intersection(ret, return)}, rest_positive) and
      Enum.all?(Enum.with_index(arguments), fn {type, index} ->
        List.update_at(args, index, fn {_, arg} -> {true, difference(arg, type)} end)
        |> phi({b, ret}, rest_positive)
      end)
  end

  defp fun_union(bdd1, bdd2) do
    case {bdd1, bdd2} do
      {:fun_top, _} -> :fun_top
      {_, :fun_top} -> :fun_top
      {:fun_bottom, bdd} -> bdd
      {bdd, :fun_bottom} -> bdd
      {{fun, l1, r1}, {fun, l2, r2}} -> {fun, fun_union(l1, l2), fun_union(r1, r2)}
      # Note: this is a deep merge, that goes down bdd1 to insert bdd2 into it.
      # It is the same as going down bdd1 to insert bdd1 into it.
      # Possible opti: insert into the bdd with smallest height
      {{fun, l, r}, bdd} -> {fun, fun_union(l, bdd), fun_union(r, bdd)}
    end
  end

  defp fun_intersection(bdd1, bdd2) do
    case {bdd1, bdd2} do
      # Base cases
      {_, :fun_bottom} -> :fun_bottom
      {:fun_bottom, _} -> :fun_bottom
      {:fun_top, bdd} -> bdd
      {bdd, :fun_top} -> bdd
      # Optimizations
      # If intersecting with a single positive or negative function, we insert
      # it at the root instead of merging the trees (this avoids going down the
      # whole bdd).
      {bdd, {fun, :fun_top, :fun_bottom}} -> {fun, bdd, :fun_bottom}
      {bdd, {fun, :fun_bottom, :fun_top}} -> {fun, :fun_bottom, bdd}
      {{fun, :fun_top, :fun_bottom}, bdd} -> {fun, bdd, :fun_bottom}
      {{fun, :fun_bottom, :fun_top}, bdd} -> {fun, :fun_bottom, bdd}
      # General cases
      {{fun, l1, r1}, {fun, l2, r2}} -> {fun, fun_intersection(l1, l2), fun_intersection(r1, r2)}
      {{fun, l, r}, bdd} -> {fun, fun_intersection(l, bdd), fun_intersection(r, bdd)}
    end
  end

  defp fun_difference(bdd1, bdd2) do
    case {bdd1, bdd2} do
      {:fun_bottom, _} -> :fun_bottom
      {_, :fun_top} -> :fun_bottom
      {bdd, :fun_bottom} -> bdd
      {:fun_top, {fun, l, r}} -> {fun, fun_difference(:fun_top, l), fun_difference(:fun_top, r)}
      {{fun, l1, r1}, {fun, l2, r2}} -> {fun, fun_difference(l1, l2), fun_difference(r1, r2)}
      {{fun, l, r}, bdd} -> {fun, fun_difference(l, bdd), fun_difference(r, bdd)}
    end
  end

  # Converts a function BDD (Binary Decision Diagram) to its quoted representation.
  defp fun_to_quoted(:fun, _opts), do: [{:fun, [], []}]

  defp fun_to_quoted(bdd, opts) do
    arrows = fun_get(bdd)

    for {positives, negatives} <- arrows, not fun_empty?(positives, negatives) do
      fun_intersection_to_quoted(positives, opts)
    end
    |> case do
      [] -> []
      multiple -> [Enum.reduce(multiple, &{:or, [], [&2, &1]})]
    end
  end

  defp fun_intersection_to_quoted(intersection, opts) do
    intersection
    |> Enum.map(fn {args, ret} ->
      {:__block__, [],
       [[{:->, [], [Enum.map(args, &to_quoted(&1, opts)), to_quoted(ret, opts)]}]]}
    end)
    |> case do
      [] -> {:fun, [], []}
      multiple -> Enum.reduce(multiple, &{:and, [], [&2, &1]})
    end
  end

  ## List

  # Represents both list and improper list simultaneously using a pair
  # `{list_type, last_type}`.
  #
  # We compute if it is a proper or improper list based if the last_type
  # is an empty_list() or a list(). In particular, the last_type may be
  # stored as `:term` for optimization purposes. This is ok because operations
  # like `tl` effectively return the list itself plus the union of the tail
  # (and if the tail includes the list itself, they are equivalent). And,
  # for other operations like difference, we expand the tail_type back into
  # `not non_empty_list()` via `list_tail_unfold/1`. Overall, this simplifies
  # the code because we don't need to special case `not non_empty_list()`.
  #
  # none() types can be given and, while stored, it means the list type is empty.
  defp list_descr(list_type, last_type, empty?) do
    {list_dynamic?, list_type} = list_pop_dynamic(list_type)
    {last_dynamic?, last_type} = list_pop_dynamic(last_type)

    list_part =
      if last_type == :term do
        list_new(:term, :term)
      else
        case :maps.take(:list, last_type) do
          :error ->
            list_new(list_type, last_type)

          {dnf, last_type} ->
            # It is safe to discard the negations for the tail because
            # `list(term()) and not list(integer())` means a list
            # of all terms except lists where all of them are integer,
            # which means the head is still a term().
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

  defp list_union(dnf1, dnf2), do: dnf1 ++ (dnf2 -- dnf1)

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

        new_negs =
          Enum.reduce(negs2, [], fn {nt, nlast}, nacc ->
            t = intersection(t1, nt)
            last = intersection(last1, nlast)
            if empty?(t) or empty?(last), do: nacc, else: [{t, last, negs1} | nacc]
          end)

        i = intersection(t1, t2)
        l = intersection(last1, last2)

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

  defp non_empty_list_only?(descr), do: empty?(Map.delete(descr, :list))

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
        static_value = list_hd_static(descr)

        if non_empty_list_only?(descr) and not empty?(static_value) do
          {false, static_value}
        else
          :badnonemptylist
        end

      {dynamic, static} ->
        dynamic_value = list_hd_static(dynamic)

        if non_empty_list_only?(static) and not empty?(dynamic_value) do
          {true, union(dynamic(dynamic_value), list_hd_static(static))}
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
        static_value = list_tl_static(descr)

        if non_empty_list_only?(descr) and not empty?(static_value) do
          {false, static_value}
        else
          :badnonemptylist
        end

      {dynamic, static} ->
        dynamic_value = list_tl_static(dynamic)

        if non_empty_list_only?(static) and not empty?(dynamic_value) do
          {true, union(dynamic(dynamic_value), list_tl_static(static))}
        else
          :badnonemptylist
        end
    end
  end

  defp list_tl_static(:term), do: :term

  defp list_tl_static(%{list: dnf} = descr) do
    initial =
      case descr do
        %{bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0 ->
          %{list: dnf, bitmap: @bit_empty_list}

        %{} ->
          %{list: dnf}
      end

    Enum.reduce(dnf, initial, fn {_, last, _}, acc ->
      union(last, acc)
    end)
  end

  defp list_tl_static(%{}), do: none()

  defp list_improper_static?(:term), do: false
  defp list_improper_static?(%{bitmap: bitmap}) when (bitmap &&& @bit_empty_list) != 0, do: false
  defp list_improper_static?(term), do: equal?(term, @not_list)

  defp list_to_quoted(dnf, empty?, opts) do
    dnf = list_normalize(dnf)

    {unions, list_rendered?} =
      Enum.reduce(dnf, {[], false}, fn {list_type, last_type, negs}, {acc, list_rendered?} ->
        {name, arguments, list_rendered?} =
          cond do
            list_type == term() and list_improper_static?(last_type) ->
              {:improper_list, [], list_rendered?}

            subtype?(last_type, @empty_list) ->
              name = if empty?, do: :list, else: :non_empty_list
              {name, [to_quoted(list_type, opts)], empty?}

            true ->
              args = [to_quoted(list_type, opts), to_quoted(last_type, opts)]
              {:non_empty_list, args, list_rendered?}
          end

        acc =
          if negs == [] do
            [{name, [], arguments} | acc]
          else
            negs
            |> non_empty_map_or(fn {ty, lst} ->
              args =
                if subtype?(lst, @empty_list) do
                  [to_quoted(ty, opts)]
                else
                  [to_quoted(ty, opts), to_quoted(lst, opts)]
                end

              {name, [], args}
            end)
            |> Kernel.then(
              &[
                {:and, [], [{name, [], arguments}, {:not, [], [&1]}]}
                | acc
              ]
            )
          end

        {acc, list_rendered?}
      end)

    if empty? and not list_rendered? do
      [{:empty_list, [], []} | unions]
    else
      unions
    end
  end

  # Eliminate empty lists from the union, and redundant types (that are subtypes of others,
  # or that can be merged with others).
  defp list_normalize(dnf) do
    Enum.reduce(dnf, [], fn {lt, last, negs}, acc ->
      if list_literal_empty?(lt, last, negs),
        do: acc,
        else: add_to_list_normalize(acc, lt, last, negs)
    end)
  end

  defp list_literal_empty?(list_type, last_type, negations) do
    empty?(list_type) or empty?(last_type) or
      Enum.any?(negations, fn {neg_type, neg_last} ->
        subtype?(list_type, neg_type) and subtype?(last_type, neg_last)
      end)
  end

  # Inserts a list type into a list of non-subtype list types.
  # If the {list_type, last_type} is a subtype of an existing type, the negs
  # are added to that type.
  # If one list member is a subtype of {list_type, last_type}, it is replaced
  # and its negations are added to the new type.
  # If the type of elements are the same, the last types are merged.
  defp add_to_list_normalize([{t, l, n} | rest], list, last, negs) do
    cond do
      subtype?(list, t) and subtype?(last, l) -> [{t, l, n ++ negs} | rest]
      subtype?(t, list) and subtype?(l, last) -> [{list, last, n ++ negs} | rest]
      equal?(t, list) -> [{t, union(l, last), n ++ negs} | rest]
      true -> [{t, l, n} | add_to_list_normalize(rest, list, last, negs)]
    end
  end

  defp add_to_list_normalize([], list, last, negs), do: [{list, last, negs}]

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

  defp dynamic_union(:term, other), do: optional_to_term(other)
  defp dynamic_union(other, :term), do: optional_to_term(other)

  defp dynamic_union(left, right),
    do: symmetrical_merge(unfold(left), unfold(right), &union/3)

  defp dynamic_intersection(:term, other), do: remove_optional_static(other)
  defp dynamic_intersection(other, :term), do: remove_optional_static(other)

  defp dynamic_intersection(left, right),
    do: symmetrical_intersection(unfold(left), unfold(right), &intersection/3)

  defp dynamic_difference(left, right) do
    case difference_static(left, right) do
      value when value == @none -> 0
      value -> value
    end
  end

  defp dynamic_to_quoted(descr, opts) do
    cond do
      term_type?(descr) ->
        [{:dynamic, [], []}]

      single = indivisible_bitmap(descr) ->
        [single]

      true ->
        case to_quoted(descr, opts) do
          {:none, _meta, []} = none -> [none]
          descr -> [{:dynamic, [], [descr]}]
        end
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

  defguardp is_optional_static(map)
            when is_map(map) and is_map_key(map, :optional)

  defp map_new(tag, fields = %{}), do: [{tag, fields, []}]

  defp map_only?(descr), do: empty?(Map.delete(descr, :map))

  defp map_union(dnf1, dnf2) do
    # Union is just concatenation, but we rely on some optimization strategies to
    # avoid the list to grow when possible

    # first pass trying to identify patterns where two maps can be fused as one
    with [map1] <- dnf1,
         [map2] <- dnf2,
         optimized when optimized != nil <- maybe_optimize_map_union(map1, map2) do
      [optimized]
    else
      # otherwise we just concatenate and remove structural duplicates
      _ -> dnf1 ++ (dnf2 -- dnf1)
    end
  end

  defp maybe_optimize_map_union({tag1, pos1, []} = map1, {tag2, pos2, []} = map2) do
    case map_union_optimization_strategy(tag1, pos1, tag2, pos2) do
      :all_equal ->
        map1

      :any_map ->
        {:open, %{}, []}

      {:one_key_difference, key, v1, v2} ->
        new_pos = Map.put(pos1, key, union(v1, v2))
        {tag1, new_pos, []}

      :left_subtype_of_right ->
        map2

      :right_subtype_of_left ->
        map1

      nil ->
        nil
    end
  end

  defp maybe_optimize_map_union(_, _), do: nil

  defp map_union_optimization_strategy(tag1, pos1, tag2, pos2)
  defp map_union_optimization_strategy(tag, pos, tag, pos), do: :all_equal
  defp map_union_optimization_strategy(:open, empty, _, _) when empty == %{}, do: :any_map
  defp map_union_optimization_strategy(_, _, :open, empty) when empty == %{}, do: :any_map

  defp map_union_optimization_strategy(tag, pos1, tag, pos2)
       when map_size(pos1) == map_size(pos2) do
    :maps.iterator(pos1)
    |> :maps.next()
    |> do_map_union_optimization_strategy(pos2, :all_equal)
  end

  defp map_union_optimization_strategy(:open, pos1, _, pos2)
       when map_size(pos1) <= map_size(pos2) do
    :maps.iterator(pos1)
    |> :maps.next()
    |> do_map_union_optimization_strategy(pos2, :right_subtype_of_left)
  end

  defp map_union_optimization_strategy(_, pos1, :open, pos2)
       when map_size(pos1) >= map_size(pos2) do
    :maps.iterator(pos2)
    |> :maps.next()
    |> do_map_union_optimization_strategy(pos1, :right_subtype_of_left)
    |> case do
      :right_subtype_of_left -> :left_subtype_of_right
      nil -> nil
    end
  end

  defp map_union_optimization_strategy(_, _, _, _), do: nil

  defp do_map_union_optimization_strategy(:none, _, status), do: status

  defp do_map_union_optimization_strategy({key, v1, iterator}, pos2, status) do
    with %{^key => v2} <- pos2,
         next_status when next_status != nil <- map_union_next_strategy(key, v1, v2, status) do
      do_map_union_optimization_strategy(:maps.next(iterator), pos2, next_status)
    else
      _ -> nil
    end
  end

  defp map_union_next_strategy(key, v1, v2, status)

  # structurally equal values do not impact the ongoing strategy
  defp map_union_next_strategy(_key, same, same, status), do: status

  defp map_union_next_strategy(key, v1, v2, :all_equal) do
    if key != :__struct__, do: {:one_key_difference, key, v1, v2}
  end

  defp map_union_next_strategy(_key, v1, v2, {:one_key_difference, _, d1, d2}) do
    # we have at least two key differences now, we switch strategy
    # if both are subtypes in one direction, keep checking
    cond do
      subtype?(d1, d2) and subtype?(v1, v2) -> :left_subtype_of_right
      subtype?(d2, d1) and subtype?(v2, v1) -> :right_subtype_of_left
      true -> nil
    end
  end

  defp map_union_next_strategy(_key, v1, v2, :left_subtype_of_right) do
    if subtype?(v1, v2), do: :left_subtype_of_right
  end

  defp map_union_next_strategy(_key, v1, v2, :right_subtype_of_left) do
    if subtype?(v2, v1), do: :right_subtype_of_left
  end

  # Given two unions of maps, intersects each pair of maps.
  defp map_intersection(dnf1, dnf2) do
    for {tag1, pos1, negs1} <- dnf1,
        {tag2, pos2, negs2} <- dnf2,
        reduce: [] do
      acc ->
        try do
          {tag, fields} = map_literal_intersection(tag1, pos1, tag2, pos2)
          entry = {tag, fields, negs1 ++ negs2}

          # Imagine a, b, c, where a is closed and b and c are open with
          # no keys in common. The result in both cases will be a and we
          # want to avoid adding duplicates, especially as intersection
          # is a cartesian product.
          case :lists.member(entry, acc) do
            true -> acc
            false -> [entry | acc]
          end
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
        # If the key is optional in the open map, we can ignore it
        case type1 do
          %{optional: 1} -> :maps.next(iterator) |> map_literal_intersection_loop(acc)
          _ -> throw(:empty)
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
          {key, value, _rest} = :maps.next(:maps.iterator(fields2))
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

  # Optimization: if the key does not exist in the map, avoid building
  # if_set/not_set pairs and return the popped value directly.
  defp map_fetch_static(%{map: [{tag, fields, []}]}, key) when not is_map_key(fields, key) do
    case tag do
      :open -> {true, term()}
      :closed -> {true, none()}
    end
  end

  # Takes a map dnf and returns the union of types it can take for a given key.
  # If the key may be undefined, it will contain the `not_set()` type.
  defp map_fetch_static(%{map: dnf}, key) do
    dnf
    |> Enum.reduce(none(), fn
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
          :empty ->
            acc

          negative ->
            negative
            |> pair_make_disjoint()
            |> pair_eliminate_negations_fst(fst, snd)
            |> union(acc)
        end
    end)
    |> pop_optional_static()
  end

  defp map_fetch_static(%{}, _key), do: {false, none()}
  defp map_fetch_static(:term, _key), do: {true, term()}

  @doc """
  Fetches and puts a `key` of a given type, assuming that the descr is exclusively
  a map (or dynamic).
  """
  def map_fetch_and_put(:term, _key, _type), do: :badmap

  def map_fetch_and_put(descr, key, :term) when is_atom(key),
    do: map_fetch_and_put_shared(descr, key, :term)

  def map_fetch_and_put(descr, key, type) when is_atom(key) do
    case :maps.take(:dynamic, type) do
      :error -> map_fetch_and_put_shared(descr, key, type)
      {dynamic, _static} -> map_fetch_and_put_shared(dynamic(descr), key, dynamic)
    end
  end

  defp map_fetch_and_put_shared(descr, key, type) do
    map_take(descr, key, none(), &map_put_static(&1, key, type))
  end

  @doc """
  Puts a `key` of a given type, assuming that the descr is exclusively
  a map (or dynamic).
  """
  def map_put(:term, _key, _type), do: :badmap
  def map_put(descr, key, :term) when is_atom(key), do: map_put_shared(descr, key, :term)

  def map_put(descr, key, type) when is_atom(key) do
    case :maps.take(:dynamic, type) do
      :error -> map_put_shared(descr, key, type)
      {dynamic, _static} -> map_put_shared(dynamic(descr), key, dynamic)
    end
  end

  defp map_put_shared(descr, key, type) do
    with {nil, descr} <- map_take(descr, key, nil, &map_put_static(&1, key, type)) do
      {:ok, descr}
    end
  end

  # Directly inserts a key of a given type into every positive and negative map.
  defp map_put_static(%{map: dnf} = descr, key, type) do
    dnf =
      Enum.map(dnf, fn {tag, fields, negs} ->
        {tag, Map.put(fields, key, type),
         Enum.map(negs, fn {neg_tag, neg_fields} ->
           {neg_tag, Map.put(neg_fields, key, type)}
         end)}
      end)

    %{descr | map: dnf}
  end

  defp map_put_static(descr, _key, _type), do: descr

  @doc """
  Removes a key from a map type.
  """
  def map_delete(descr, key) do
    # We pass nil as the initial value so we can avoid computing the unions.
    with {nil, descr} <-
           map_take(descr, key, nil, &intersection_static(&1, open_map([{key, not_set()}]))) do
      {:ok, descr}
    end
  end

  @doc """
  Removes a key from a map type and return its type.

  ## Algorithm

  1. Split the map type based on the presence of the key.
  2. Take the second part of the split, which represents the union of all
     record types where the key has been explicitly removed.
  3. Intersect this with an open record type where the key is explicitly absent.
     This step eliminates the key from open record types where it was implicitly present.
  """
  def map_take(descr, key) do
    map_take(descr, key, none(), &intersection_static(&1, open_map([{key, not_set()}])))
  end

  @compile {:inline, map_take: 4}
  defp map_take(:term, _key, _initial, _updater), do: :badmap

  defp map_take(descr, key, initial, updater) when is_atom(key) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and map_only?(descr) do
          {optional?, taken, result} = map_take_static(descr, key, initial)

          cond do
            taken == nil -> {nil, updater.(result)}
            optional? or empty?(taken) -> :badkey
            true -> {taken, updater.(result)}
          end
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          {_, dynamic_taken, dynamic_result} = map_take_static(dynamic, key, initial)
          {static_optional?, static_taken, static_result} = map_take_static(static, key, initial)
          result = union(dynamic(updater.(dynamic_result)), updater.(static_result))

          cond do
            static_taken == nil and dynamic_taken == nil -> {nil, result}
            static_optional? or empty?(dynamic_taken) -> :badkey
            true -> {union(dynamic(dynamic_taken), static_taken), result}
          end
        else
          :badmap
        end
    end
  end

  # Takes a static map type and removes a key from it.
  # This allows the key to be put or deleted later on.
  defp map_take_static(%{map: [{tag, fields, []}]} = descr, key, initial)
       when not is_map_key(fields, key) do
    case tag do
      :open -> {true, maybe_union(initial, fn -> term() end), descr}
      :closed -> {true, initial, descr}
    end
  end

  defp map_take_static(%{map: dnf}, key, initial) do
    {value, map} =
      Enum.reduce(dnf, {initial, none()}, fn
        # Optimization: if there are no negatives, we can directly remove the key.
        {tag, fields, []}, {value, map} ->
          {fst, snd} = map_pop_key(tag, fields, key)
          {maybe_union(value, fn -> fst end), union(map, snd)}

        {tag, fields, negs}, {value, map} ->
          {fst, snd} = map_pop_key(tag, fields, key)

          case map_split_negative(negs, key) do
            :empty ->
              {value, map}

            negative ->
              disjoint = pair_make_disjoint(negative)

              {maybe_union(value, fn -> pair_eliminate_negations_fst(disjoint, fst, snd) end),
               disjoint |> pair_eliminate_negations_snd(fst, snd) |> union(map)}
          end
      end)

    if value == nil do
      {false, value, map}
    else
      {optional?, value} = pop_optional_static(value)
      {optional?, value, map}
    end
  end

  # If there is no map part to this static type, there is nothing to delete.
  defp map_take_static(%{}, _key, initial), do: {false, initial, none()}

  defp map_take_static(:term, _key, initial) do
    {true, maybe_union(initial, fn -> term() end), open_map()}
  end

  # Short-circuits if it finds a non-empty map literal in the union.
  # Since the algorithm is recursive, we implement the short-circuiting
  # as throw/catch.
  defp map_empty?(dnf) do
    Enum.all?(dnf, fn {tag, pos, negs} -> map_empty?(tag, pos, negs) end)
  end

  defp map_empty?(_, pos, []), do: Enum.any?(Map.to_list(pos), fn {_, v} -> empty?(v) end)
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
  defp map_normalize(dnfs) do
    for dnf <- dnfs, not map_empty?([dnf]) do
      {tag, fields, negs} = dnf

      {fields, negs} =
        Enum.reduce(negs, {fields, []}, fn neg = {neg_tag, neg_fields}, {acc_fields, acc_negs} ->
          if map_empty_negation?(tag, acc_fields, neg) do
            {acc_fields, acc_negs}
          else
            case map_all_but_one(tag, acc_fields, neg_tag, neg_fields) do
              {:one, diff_key} ->
                {Map.update!(acc_fields, diff_key, &difference(&1, neg_fields[diff_key])),
                 acc_negs}

              _ ->
                {acc_fields, [neg | acc_negs]}
            end
          end
        end)

      {tag, fields, negs}
    end
    |> map_fusion()
  end

  # Given a dnf, fuse maps when possible
  # e.g. %{a: integer(), b: atom()} or %{a: float(), b: atom()} into %{a: number(), b: atom()}
  defp map_fusion(dnf) do
    # Steps:
    # 1. Group maps by tags and keys
    # 2. Try fusions for each group until no fusion is found
    # 3. Merge the groups back into a dnf
    {without_negs, with_negs} = Enum.split_with(dnf, fn {_tag, _fields, negs} -> negs == [] end)

    without_negs =
      without_negs
      |> Enum.group_by(fn {tag, fields, _} -> {tag, Map.keys(fields)} end)
      |> Enum.flat_map(fn {_, maps} -> map_non_negated_fuse(maps) end)

    without_negs ++ with_negs
  end

  defp map_non_negated_fuse(maps) do
    Enum.reduce(maps, [], fn map, acc ->
      map_fuse_with_first_fusible(map, acc)
    end)
  end

  defp map_fuse_with_first_fusible(map, []), do: [map]

  defp map_fuse_with_first_fusible(map, [candidate | rest]) do
    if fused = maybe_optimize_map_union(map, candidate) do
      # we found a fusible candidate, we're done
      [fused | rest]
    else
      [candidate | map_fuse_with_first_fusible(map, rest)]
    end
  end

  # If all fields are the same except one, we can optimize map difference.
  defp map_all_but_one(tag1, fields1, tag2, fields2) do
    with true <- {tag1, tag2} != {:open, :closed},
         true <- map_size(fields1) == map_size(fields2),
         keys = :maps.keys(fields1),
         true <- Enum.all?(keys, fn key -> is_map_key(fields2, key) end),
         1 <-
           Enum.count_until(
             keys,
             fn key -> Map.fetch!(fields1, key) != Map.fetch!(fields2, key) end,
             _limit = 2
           ) do
      {:one, Enum.find(keys, &(Map.fetch!(fields1, &1) != Map.fetch!(fields2, &1)))}
    else
      _ -> :no
    end
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

  defp map_to_quoted(dnf, opts) do
    dnf
    |> map_normalize()
    |> Enum.map(&map_each_to_quoted(&1, opts))
  end

  defp map_each_to_quoted({tag, positive_map, negative_maps}, opts) do
    case negative_maps do
      [] ->
        map_literal_to_quoted({tag, positive_map}, opts)

      _ ->
        negative_maps
        |> non_empty_map_or(&map_literal_to_quoted(&1, opts))
        |> Kernel.then(
          &{:and, [], [map_literal_to_quoted({tag, positive_map}, opts), {:not, [], [&1]}]}
        )
    end
  end

  def map_literal_to_quoted({:closed, fields}, _opts) when map_size(fields) == 0 do
    {:empty_map, [], []}
  end

  def map_literal_to_quoted({:open, fields}, _opts) when map_size(fields) == 0 do
    {:map, [], []}
  end

  def map_literal_to_quoted({:open, %{__struct__: @not_atom_or_optional} = fields}, _opts)
      when map_size(fields) == 1 do
    {:non_struct_map, [], []}
  end

  def map_literal_to_quoted({tag, fields}, opts) do
    case tag do
      :closed ->
        with %{__struct__: struct_descr} <- fields,
             {_, [struct]} <- atom_fetch(struct_descr),
             [_ | _] = info <- maybe_struct(struct),
             true <- map_size(fields) == length(info) + 1,
             true <- Enum.all?(info, &is_map_key(fields, &1.field)) do
          collapse? = Keyword.get(opts, :collapse_structs, true)

          fields =
            for %{field: field} <- info,
                type = Map.fetch!(fields, field),
                # TODO: This should consider the struct default type
                not collapse? or type != term(),
                do: {field, type}

          {:%, [],
           [
             literal_to_quoted(struct),
             {:%{}, [], map_fields_to_quoted(tag, fields, opts)}
           ]}
        else
          _ -> {:%{}, [], map_fields_to_quoted(tag, Enum.sort(fields), opts)}
        end

      :open ->
        fields = Map.to_list(fields)
        {:%{}, [], [{:..., [], nil} | map_fields_to_quoted(tag, Enum.sort(fields), opts)]}
    end
  end

  defp maybe_struct(struct) do
    try do
      struct.__info__(:struct)
    rescue
      _ -> nil
    end
  end

  defp map_fields_to_quoted(tag, sorted, opts) do
    keyword? = Inspect.List.keyword?(sorted)

    for {key, type} <- sorted,
        not (tag == :open and is_optional_static(type) and term_type?(type)) do
      key =
        if keyword? do
          {:__block__, [format: :keyword], [key]}
        else
          literal_to_quoted(key)
        end

      {optional?, type} = pop_optional_static(type)

      cond do
        not optional? -> {key, to_quoted(type, opts)}
        empty?(type) -> {key, {:not_set, [], []}}
        true -> {key, {:if_set, [], [to_quoted(type, opts)]}}
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
        case tuple_literal_intersection(tag1, elements1, tag2, elements2) do
          {tag, elements} ->
            entry = {tag, elements, negs1 ++ negs2}

            case :lists.member(entry, acc) do
              true -> acc
              false -> [entry | acc]
            end

          :empty ->
            acc
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
      (tag1 == :closed and n < m) or (tag2 == :closed and n > m) ->
        :empty

      tag1 == :open and tag2 == :open ->
        try do
          {:open, zip_non_empty_intersection!(elements1, elements2, [])}
        catch
          :empty -> :empty
        end

      true ->
        try do
          {:closed, zip_non_empty_intersection!(elements1, elements2, [])}
        catch
          :empty -> :empty
        end
    end
  end

  # Intersects two lists of types, and _appends_ the extra elements to the result.
  defp zip_non_empty_intersection!([], types2, acc), do: Enum.reverse(acc, types2)
  defp zip_non_empty_intersection!(types1, [], acc), do: Enum.reverse(acc, types1)

  defp zip_non_empty_intersection!([type1 | rest1], [type2 | rest2], acc) do
    zip_non_empty_intersection!(rest1, rest2, [non_empty_intersection!(type1, type2) | acc])
  end

  defp tuple_difference(dnf1, dnf2) do
    Enum.reduce(dnf2, dnf1, fn {tag2, elements2, negs2}, dnf1 ->
      Enum.reduce(dnf1, [], fn {tag1, elements1, negs1}, acc ->
        # Prune negations that have no values in common
        acc =
          case tuple_literal_intersection(tag1, elements1, tag2, elements2) do
            :empty -> [{tag1, elements1, negs1}] ++ acc
            _ -> [{tag1, elements1, [{tag2, elements2} | negs1]}] ++ acc
          end

        Enum.reduce(negs2, acc, fn {neg_tag2, neg_elements2}, inner_acc ->
          case tuple_literal_intersection(tag1, elements1, neg_tag2, neg_elements2) do
            :empty -> inner_acc
            {tag, fields} -> [{tag, fields, negs1} | inner_acc]
          end
        end)
      end)
    end)
    |> case do
      [] -> 0
      acc -> acc
    end
  end

  defp tuple_union(dnf1, dnf2) do
    # Union is just concatenation, but we rely on some optimization strategies to
    # avoid the list to grow when possible

    # first pass trying to identify patterns where two maps can be fused as one
    with [tuple1] <- dnf1,
         [tuple2] <- dnf2,
         optimized when optimized != nil <- maybe_optimize_tuple_union(tuple1, tuple2) do
      [optimized]
    else
      # otherwise we just concatenate and remove structural duplicates
      _ -> dnf1 ++ (dnf2 -- dnf1)
    end
  end

  defp maybe_optimize_tuple_union({tag1, pos1, []} = tuple1, {tag2, pos2, []} = tuple2) do
    case tuple_union_optimization_strategy(tag1, pos1, tag2, pos2) do
      :all_equal ->
        tuple1

      {:one_index_difference, index, v1, v2} ->
        new_pos = List.replace_at(pos1, index, union(v1, v2))
        {tag1, new_pos, []}

      :left_subtype_of_right ->
        tuple2

      :right_subtype_of_left ->
        tuple1

      nil ->
        nil
    end
  end

  defp maybe_optimize_tuple_union(_, _), do: nil

  defp tuple_union_optimization_strategy(tag1, pos1, tag2, pos2)
  defp tuple_union_optimization_strategy(tag, pos, tag, pos), do: :all_equal

  # might be one extra loop but cheap and avoids doing deep subtype comparisons
  defp tuple_union_optimization_strategy(:closed, pos1, :closed, pos2)
       when length(pos1) != length(pos2),
       do: nil

  defp tuple_union_optimization_strategy(tag1, pos1, tag2, pos2) do
    status =
      case {tag1, tag2} do
        {:open, :closed} -> :right_subtype_of_left
        {:closed, :open} -> :left_subtype_of_right
        {same, same} -> :all_equal
      end

    do_tuple_union_optimization_strategy(tag1, pos1, tag2, pos2, 0, status)
  end

  defp do_tuple_union_optimization_strategy(_tag1, [], _tag2, [], _i, status), do: status

  defp do_tuple_union_optimization_strategy(:open, [], _tag2, _pos2, _i, status)
       when status in [:all_equal, :right_subtype_of_left],
       do: :right_subtype_of_left

  defp do_tuple_union_optimization_strategy(_tag1, _pos1, :open, [], _i, status)
       when status in [:all_equal, :left_subtype_of_right],
       do: :left_subtype_of_right

  defp do_tuple_union_optimization_strategy(tag1, [v1 | pos1], tag2, [v2 | pos2], i, status) do
    if next_status = tuple_union_next_strategy(i, v1, v2, status) do
      do_tuple_union_optimization_strategy(tag1, pos1, tag2, pos2, i + 1, next_status)
    end
  end

  defp do_tuple_union_optimization_strategy(_tag1, _pos1, _tag2, _pos2, _i, _status), do: nil

  defp tuple_union_next_strategy(index, v1, v2, status)

  # structurally equal values do not impact the ongoing strategy
  defp tuple_union_next_strategy(_index, same, same, status), do: status

  defp tuple_union_next_strategy(index, v1, v2, :all_equal) do
    {:one_index_difference, index, v1, v2}
  end

  defp tuple_union_next_strategy(_index, v1, v2, {:one_index_difference, _, d1, d2}) do
    # we have at least two differences now, we switch strategy
    # if both are subtypes in one direction, keep checking
    cond do
      subtype?(d1, d2) and subtype?(v1, v2) -> :left_subtype_of_right
      subtype?(d2, d1) and subtype?(v2, v1) -> :right_subtype_of_left
      true -> nil
    end
  end

  defp tuple_union_next_strategy(_index, v1, v2, :left_subtype_of_right) do
    if subtype?(v1, v2), do: :left_subtype_of_right
  end

  defp tuple_union_next_strategy(_index, v1, v2, :right_subtype_of_left) do
    if subtype?(v2, v1), do: :right_subtype_of_left
  end

  defp tuple_to_quoted(dnf, opts) do
    dnf
    |> tuple_simplify()
    |> tuple_fusion()
    |> Enum.map(&tuple_each_to_quoted(&1, opts))
  end

  # Given a dnf of tuples, fuses the tuple unions when possible,
  # e.g. {integer(), atom()} or {float(), atom()} into {number(), atom()}
  # The negations of two fused tuples are just concatenated.
  defp tuple_fusion(dnf) do
    # Steps:
    # 1. Consider tuples without negations apart from those with
    # 2. Group tuples by size and tag
    # 3. Try fusions for each group until no fusion is found
    # 4. Merge the groups back into a dnf
    {without_negs, with_negs} = Enum.split_with(dnf, fn {_tag, _elems, negs} -> negs == [] end)

    without_negs =
      without_negs
      |> Enum.group_by(fn {tag, elems, _} -> {tag, length(elems)} end)
      |> Enum.flat_map(fn {_, tuples} -> tuple_non_negated_fuse(tuples) end)

    without_negs ++ with_negs
  end

  defp tuple_non_negated_fuse(tuples) do
    Enum.reduce(tuples, [], fn tuple, acc ->
      tuple_fuse_with_first_fusible(tuple, acc)
    end)
  end

  defp tuple_fuse_with_first_fusible(tuple, []), do: [tuple]

  defp tuple_fuse_with_first_fusible(tuple, [candidate | rest]) do
    if fused = maybe_optimize_tuple_union(tuple, candidate) do
      # we found a fusible candidate, we're done
      [fused | rest]
    else
      [candidate | tuple_fuse_with_first_fusible(tuple, rest)]
    end
  end

  defp tuple_each_to_quoted({tag, positive_tuple, negative_tuples}, opts) do
    case negative_tuples do
      [] ->
        tuple_literal_to_quoted({tag, positive_tuple}, opts)

      _ ->
        negative_tuples
        |> non_empty_map_or(&tuple_literal_to_quoted(&1, opts))
        |> Kernel.then(
          &{:and, [], [tuple_literal_to_quoted({tag, positive_tuple}, opts), {:not, [], [&1]}]}
        )
    end
  end

  defp tuple_literal_to_quoted({:closed, []}, _opts), do: {:{}, [], []}

  defp tuple_literal_to_quoted({tag, elements}, opts) do
    case tag do
      :closed -> {:{}, [], Enum.map(elements, &to_quoted(&1, opts))}
      :open -> {:{}, [], Enum.map(elements, &to_quoted(&1, opts)) ++ [{:..., [], nil}]}
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

  # No negations, so not empty unless there's an empty type
  defp tuple_empty?(_, pos, []), do: Enum.any?(pos, &empty?/1)
  # Open empty negation makes it empty
  defp tuple_empty?(_, _, [{:open, []} | _]), do: true
  # Open positive can't be emptied by a single closed negative
  defp tuple_empty?(:open, pos, [{:closed, _}]), do: Enum.any?(pos, &empty?/1)

  defp tuple_empty?(tag, elements, [{neg_tag, neg_elements} | negs]) do
    n = length(elements)
    m = length(neg_elements)

    # Scenarios where the difference is guaranteed to be empty:
    # 1. When removing larger tuples from a fixed-size positive tuple
    # 2. When removing smaller tuples from larger tuples
    if (tag == :closed and n < m) or (neg_tag == :closed and n > m) do
      tuple_empty?(tag, elements, negs)
    else
      tuple_elements_empty?([], tag, elements, neg_elements, negs) and
        tuple_compatibility(n, m, tag, elements, neg_tag, negs)
    end
  end

  # Recursively check elements for emptiness
  defp tuple_elements_empty?(_, _, _, [], _), do: true

  defp tuple_elements_empty?(acc, tag, elements, [neg_type | neg_elements], negs) do
    # Handles the case where {tag, elements} is an open tuple, like {:open, []}
    {ty, elements} = List.pop_at(elements, 0, term())
    diff = difference(ty, neg_type)

    (empty?(diff) or tuple_empty?(tag, Enum.reverse(acc, [diff | elements]), negs)) and
      tuple_elements_empty?([ty | acc], tag, elements, neg_elements, negs)
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
          :empty ->
            acc

          negative ->
            negative
            |> pair_make_disjoint()
            |> pair_eliminate_negations_fst(fst, snd)
            |> union(acc)
        end
    end)
  end

  def tuple_values(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if tuple_only?(descr) do
          process_tuples_values(Map.get(descr, :tuple, []))
        else
          :badtuple
        end

      {dynamic, static} ->
        if tuple_only?(static) and descr_key?(dynamic, :tuple) do
          dynamic(process_tuples_values(Map.get(dynamic, :tuple, [])))
          |> union(process_tuples_values(Map.get(static, :tuple, [])))
        else
          :badtuple
        end
    end
  end

  defp process_tuples_values(dnf) do
    tuple_reduce(dnf, none(), &union/2, fn tag, elements ->
      cond do
        Enum.any?(elements, &empty?/1) -> none()
        tag == :open -> term()
        tag == :closed -> Enum.reduce(elements, none(), &union/2)
      end
    end)
  end

  defp tuple_reduce(dnf, initial, join, transform) do
    Enum.reduce(dnf, initial, fn {tag, elements, negs}, acc ->
      join.(acc, tuple_reduce(tag, elements, negs, initial, join, transform))
    end)
  end

  defp tuple_reduce(tag, elements, [], _init, _join, transform), do: transform.(tag, elements)
  defp tuple_reduce(_tag, _elements, [{:open, []} | _], initial, _join, _transform), do: initial

  defp tuple_reduce(tag, elements, [{neg_tag, neg_elements} | negs], initial, join, transform) do
    n = length(elements)
    m = length(neg_elements)

    if (tag == :closed and n < m) or (neg_tag == :closed and n > m) do
      tuple_reduce(tag, elements, negs, initial, join, transform)
    else
      # Those two functions eliminate the negations, transforming into
      # a union of tuples to compute their values.
      elim_content([], tag, elements, neg_elements, negs, initial, join, transform)
      |> join.(elim_size(n, m, tag, elements, neg_tag, negs, initial, join, transform))
    end
  end

  # Eliminates negations according to tuple content.
  # This means that there are no more neg_elements to subtract -- end the recursion.
  defp elim_content(_acc, _tag, _elements, [], _, initial, _join, _transform), do: initial

  # Subtracts each element of a negative tuple to build a new tuple with the difference.
  # Example: {number(), atom()} and not {float(), :foo} contains types {integer(), :foo}
  # as well as {float(), atom() and not :foo}
  # Same process as tuple_elements_empty?
  defp elim_content(acc, tag, elements, [neg_type | neg_elements], negs, init, join, transform) do
    {ty, elements} = List.pop_at(elements, 0, term())
    diff = difference(ty, neg_type)

    if empty?(diff) do
      init
    else
      tuple_reduce(tag, Enum.reverse(acc, [diff | elements]), negs, init, join, transform)
    end
    |> join.(elim_content([ty | acc], tag, elements, neg_elements, negs, init, join, transform))
  end

  # Eliminates negations according to size
  # Example: {integer(), ...} and not {term(), term(), ...} contains {integer()}
  defp elim_size(_, _, :closed, _, _, _, initial, _join, _transfo), do: initial

  defp elim_size(n, m, tag, elements, neg_tag, negs, initial, join, transform) do
    n..(m - 1)//1
    |> Enum.reduce(initial, fn i, acc ->
      tuple_reduce(:closed, tuple_fill(elements, i), negs, initial, join, transform)
      |> join.(acc)
    end)
    |> join.(
      if neg_tag == :open do
        initial
      else
        tuple_reduce(tag, tuple_fill(elements, m + 1), negs, initial, join, transform)
      end
    )
  end

  @doc """
  Converts a tuple type to a simple union by eliminating negations.

  Takes a tuple type with complex negations and simplifies it to a union of
  positive tuple literals only.

  For static tuple types: eliminates all negations from the DNF representation.
  For gradual tuple types: processes both dynamic and static components separately,
  then combines them.

  Uses `tuple_reduce/4` with concatenation as the join function and a transform
  that is simply the identity.

  Returns the descriptor unchanged for non-tuple types, or a descriptor with
  simplified tuple DNF containing only positive literals. If simplification
  results in an empty tuple list, removes the `:tuple` key entirely.
  """
  def tuple_elim_negations(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        tuple_elim_negations_static(descr)

      {dynamic, static} ->
        tuple_elim_negations_static(static)
        |> union(dynamic(tuple_elim_negations_static(dynamic)))
    end
  end

  # Call tuple_reduce to build the simple union of tuples that come from each map literal.
  # Thus, initial is `[]`, join is concatenation, and the transform of a map literal
  # with no negations is just to keep the map literal as is.
  defp tuple_elim_negations_static(%{tuple: dnf} = descr) do
    case tuple_reduce(dnf, [], &Kernel.++/2, fn tag, elements -> [{tag, elements, []}] end) do
      [] -> Map.delete(descr, :tuple)
      new_dnf -> %{descr | tuple: new_dnf}
    end
  end

  defp tuple_elim_negations_static(descr), do: descr

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

        case tuple_split_negative(negs, index) do
          :empty ->
            acc

          negative ->
            negative
            |> pair_make_disjoint()
            |> pair_eliminate_negations_snd(fst, snd)
            |> union(acc)
        end
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
    tuple_descr(:open, List.duplicate(term(), n))
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
        # if fst is a subtype of s1, the disjointedness invariant ensures we can
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
          value = fun.(key, v1, v2)

          if value in @empty_intersection do
            acc
          else
            [{key, value} | acc]
          end

        %{} ->
          acc
      end

    iterator_intersection(:maps.next(iterator), map, acc, fun)
  end

  defp iterator_intersection(:none, _map, acc, _fun), do: :maps.from_list(acc)

  defp non_disjoint_intersection?(left, right) do
    # Erlang maps:intersect_with/3 has to preserve the order in combiner.
    # We don't care about the order, so we have a faster implementation.
    if map_size(left) > map_size(right) do
      iterator_non_disjoint_intersection?(:maps.next(:maps.iterator(right)), left)
    else
      iterator_non_disjoint_intersection?(:maps.next(:maps.iterator(left)), right)
    end
  end

  defp iterator_non_disjoint_intersection?({key, v1, iterator}, map) do
    with %{^key => v2} <- map,
         value when value not in @empty_intersection <- intersection(key, v1, v2),
         false <- empty_key?(key, value) do
      true
    else
      _ -> iterator_non_disjoint_intersection?(:maps.next(iterator), map)
    end
  end

  defp iterator_non_disjoint_intersection?(:none, _map), do: false

  defp non_empty_map_or([head | tail], fun) do
    Enum.reduce(tail, fun.(head), &{:or, [], [&2, fun.(&1)]})
  end
end
