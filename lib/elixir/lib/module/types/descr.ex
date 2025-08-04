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
  # * BDD - binary decision diagram which is a set-theoretic representation of types as a tree.
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

  defmacrop map_literal(tag, fields), do: {:{}, [], [{tag, fields}, :bdd_top, :bdd_bot]}
  defmacrop tuple_literal(tag, elements), do: {:{}, [], [{tag, elements}, :bdd_top, :bdd_bot]}
  defmacrop list_literal(list, last), do: {:{}, [], [{list, last}, :bdd_top, :bdd_bot]}

  defmacrop domain_key(key), do: {:domain_key, key}

  @domain_key_types [
    {:domain_key, :binary},
    {:domain_key, :empty_list},
    {:domain_key, :integer},
    {:domain_key, :float},
    {:domain_key, :pid},
    {:domain_key, :port},
    {:domain_key, :reference},
    {:domain_key, :fun},
    {:domain_key, :atom},
    {:domain_key, :tuple},
    {:domain_key, :map},
    {:domain_key, :list}
  ]

  @fun_top :bdd_top
  @atom_top {:negation, :sets.new(version: 2)}
  @map_top {{:open, %{}}, :bdd_top, :bdd_bot}
  @non_empty_list_top {{:term, :term}, :bdd_top, :bdd_bot}
  @tuple_top {{:open, []}, :bdd_top, :bdd_bot}
  @map_empty {{:closed, %{}}, :bdd_top, :bdd_bot}

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

  @not_set %{optional: 1}
  @term_or_optional Map.put(@term, :optional, 1)
  @term_or_dynamic_optional Map.put(@term, :dynamic, %{optional: 1})
  @not_atom_or_optional Map.delete(@term_or_optional, :atom)

  @empty_intersection [0, [], :bdd_bot]
  @empty_difference [0, [], :bdd_bot]

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
  def closed_map(pairs), do: map_descr(:closed, pairs, @term_or_optional, false)
  def empty_list(), do: %{bitmap: @bit_empty_list}
  def empty_map(), do: %{map: @map_empty}
  def integer(), do: %{bitmap: @bit_integer}
  def float(), do: %{bitmap: @bit_float}
  def list(type), do: list_descr(type, @empty_list, true)
  def non_empty_list(type, tail \\ @empty_list), do: list_descr(type, tail, false)
  def open_map(), do: %{map: @map_top}
  def open_map(pairs), do: map_descr(:open, pairs, @term_or_optional, false)
  def open_map(pairs, default), do: map_descr(:open, pairs, if_set(default), true)
  def open_tuple(elements, _fallback \\ term()), do: tuple_descr(:open, elements)
  def pid(), do: %{bitmap: @bit_pid}
  def port(), do: %{bitmap: @bit_port}
  def reference(), do: %{bitmap: @bit_reference}
  def tuple(), do: %{tuple: @tuple_top}
  def tuple(elements), do: tuple_descr(:closed, elements)

  @boolset :sets.from_list([true, false], version: 2)
  def boolean(), do: %{atom: {:union, @boolset}}

  ## Function constructors

  @doc """
  The top function type.
  """
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

  This function cannot be applied to, unless it is made dynamic.

  ## Examples

      fun(1)
      #=> (none -> term)

      fun(2)
      #=> Creates (none, none -> term)
  """
  def fun(arity) when is_integer(arity) and arity >= 0 do
    fun(List.duplicate(none(), arity), term())
  end

  @doc """
  Creates a function from non-overlapping function clauses.
  """
  def fun_from_non_overlapping_clauses([{args, return} | clauses]) do
    Enum.reduce(clauses, fun(args, return), fn {args, return}, acc ->
      intersection(acc, fun(args, return))
    end)
  end

  @doc """
  Creates a function from overlapping function clauses.
  """
  def fun_from_inferred_clauses(args_clauses) do
    domain_clauses =
      Enum.reduce(args_clauses, [], fn {args, return}, acc ->
        domain = args |> Enum.map(&upper_bound/1) |> args_to_domain()
        pivot_overlapping_clause(domain, upper_bound(return), acc)
      end)

    funs =
      for {domain, return} <- domain_clauses,
          args <- domain_to_args(domain),
          do: fun(args, dynamic(return))

    Enum.reduce(funs, &intersection/2)
  end

  # Inserts a new arrow `{domain, return}` into `acc`, a list whose arrows
  # have disjoint domains.
  #
  # To preserve that invariant we compare the new arrow with every function in
  # the accumulator, `{acc_domain, acc_return}`:
  #
  #   * If `intersection(domain, acc_domain)` is empty, the arrows do not overlap.
  #     We keep the current arrow and recurse on the tail.
  #
  #   * Otherwise, the domains overlap. We partition them into:
  #
  #       common = intersection(domain, acc_domain)   # shared part
  #       diff   = difference(domain, acc_domain)     # only in new arrow
  #       left   = difference(acc_domain, domain)     # only in existing arrow
  #
  #     We emit `{common, union(return, acc_return)}` for the shared part,
  #     keep `{left, acc_return}` (if any), and continue inserting `diff`
  #     into the remainder of the list to handle further overlaps.

  defp pivot_overlapping_clause(domain, return, [{acc_domain, acc_return} | acc]) do
    common = intersection(domain, acc_domain)

    if empty?(common) do
      [{acc_domain, acc_return} | pivot_overlapping_clause(domain, return, acc)]
    else
      diff = difference(domain, acc_domain)

      rest =
        if empty?(diff) do
          []
        else
          pivot_overlapping_clause(diff, return, acc)
        end

      [{common, union(return, acc_return)} | rest]
      |> prepend_to_unless_empty(difference(acc_domain, domain), acc_return)
    end
  end

  defp pivot_overlapping_clause(domain, return, []) do
    [{domain, return}]
  end

  defp prepend_to_unless_empty(acc, domain, return) do
    if empty?(domain), do: acc, else: [{domain, return} | acc]
  end

  @doc """
  Converts a list of arguments into a domain.

  Tuples represent function domains, using unions to combine parameters.

  Example: for functions (integer(), float() -> :ok) and (float(), integer() -> :error)
  domain isn't `{integer() or float(), integer() or float()}` as that would incorrectly
  accept `{float(), float()}`, instead it is `{integer(), float()} or {float(), integer()}`.
  """
  def args_to_domain(types) when is_list(types), do: tuple(types)

  @doc """
  Converts the domain to a list of arguments.

  The domain is expected to be closed tuples. They may have complex negations
  which are then simplified to a union of positive tuple literals only.

  * For static tuple types: eliminates all negations from the DNF representation.

  * For gradual tuple types: processes both dynamic and static components separately,
    then combines them.

  The list of arguments can be flattened into a broad domain by calling:

      |> Enum.zip_with(fn types -> Enum.reduce(types, &union/2) end)
  """
  def domain_to_args(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        unwrap_domain_tuple(descr, fn {:closed, elems} -> elems end)

      {dynamic, static} ->
        unwrap_domain_tuple(static, fn {:closed, elems} -> elems end) ++
          unwrap_domain_tuple(dynamic, fn {:closed, elems} -> Enum.map(elems, &dynamic/1) end)
    end
  end

  defp unwrap_domain_tuple(%{tuple: bdd} = descr, transform) when map_size(descr) == 1 do
    tuple_bdd_to_dnf_no_negations(bdd) |> Enum.map(transform)
  end

  defp unwrap_domain_tuple(descr, _transform) when descr == %{}, do: []

  defp domain_to_flat_args(domain, arity) do
    case domain_to_args(domain) do
      [] -> List.duplicate(none(), arity)
      args -> Enum.zip_with(args, fn types -> Enum.reduce(types, &union/2) end)
    end
  end

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
  def not_set(), do: @not_set

  def if_set(:term), do: term_or_optional()

  # If type contains a :dynamic part, :optional gets added there.
  def if_set(type) do
    case type do
      %{dynamic: dyn} -> Map.put(%{type | dynamic: Map.put(dyn, :optional, 1)}, :optional, 1)
      _ -> Map.put(type, :optional, 1)
    end
  end

  defp term_or_optional(), do: @term_or_optional

  @compile {:inline,
            keep_optional: 1, remove_optional: 1, remove_optional_static: 1, optional_to_term: 1}
  defp keep_optional(descr) do
    case descr do
      %{dynamic: %{optional: 1}} -> %{dynamic: %{optional: 1}}
      %{optional: 1} -> %{optional: 1}
      _ -> @none
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
  defp intersection(:list, v1, v2), do: list_intersection(v1, v2)
  defp intersection(:map, v1, v2), do: map_intersection(v1, v2)
  defp intersection(:optional, 1, 1), do: 1
  defp intersection(:tuple, v1, v2), do: tuple_intersection(v1, v2)

  defp intersection(:fun, v1, v2) do
    bdd = fun_intersection(v1, v2)
    if bdd == :bdd_bot, do: 0, else: bdd
  end

  defp intersection(:dynamic, v1, v2) do
    descr = dynamic_intersection(v1, v2)
    if descr == @none, do: 0, else: descr
  end

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

      Map.put(difference_static(left_static, right_dynamic), :dynamic, dynamic_part)
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

  # This function is designed to compute the difference during subtyping efficiently.
  # Do not use it for anything else.
  defp empty_difference_subtype?(%{dynamic: dyn_left} = left, %{dynamic: dyn_right} = right) do
    # Dynamic will either exist on both sides or on none
    empty_difference_subtype?(dyn_left, dyn_right) and
      empty_difference_subtype?(Map.delete(left, :dynamic), Map.delete(right, :dynamic))
  end

  defp empty_difference_subtype?(left, :term), do: keep_optional(left) == @none

  defp empty_difference_subtype?(left, right) do
    iterator_empty_difference_subtype?(:maps.next(:maps.iterator(unfold(left))), unfold(right))
  end

  defp iterator_empty_difference_subtype?({key, v1, iterator}, map) do
    case map do
      %{^key => v2} ->
        value = difference(key, v1, v2)
        value in @empty_difference or empty_key?(key, value)

      %{} ->
        empty_key?(key, v1)
    end and
      iterator_empty_difference_subtype?(:maps.next(iterator), map)
  end

  defp iterator_empty_difference_subtype?(:none, _map), do: true

  # Returning 0 from the callback is taken as none() for that subtype.
  defp difference(:atom, v1, v2), do: atom_difference(v1, v2)
  defp difference(:bitmap, v1, v2), do: v1 - (v1 &&& v2)
  defp difference(:list, v1, v2), do: list_difference(v1, v2)
  defp difference(:map, v1, v2), do: map_difference(v1, v2)
  defp difference(:optional, 1, 1), do: 0
  defp difference(:tuple, v1, v2), do: tuple_difference(v1, v2)

  defp difference(:fun, v1, v2) do
    bdd = fun_difference(v1, v2)
    if bdd == :bdd_bot, do: 0, else: bdd
  end

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
    case :maps.get(:dynamic, descr, descr) do
      :term ->
        false

      value when value == @none ->
        true

      descr ->
        not Map.has_key?(descr, :atom) and
          not Map.has_key?(descr, :bitmap) and
          not Map.has_key?(descr, :optional) and
          (not Map.has_key?(descr, :tuple) or tuple_empty?(descr.tuple)) and
          (not Map.has_key?(descr, :map) or map_empty?(descr.map)) and
          (not Map.has_key?(descr, :list) or list_empty?(descr.list)) and
          (not Map.has_key?(descr, :fun) or fun_empty?(descr.fun))
    end
  end

  defp empty_or_optional?(type), do: empty?(remove_optional(type))

  # For atom, bitmap, tuple, and optional, if the key is present,
  # then they are not empty,
  defp empty_key?(:fun, value), do: fun_empty?(value)
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
      non_term_type_to_quoted(descr, opts)
    end
  end

  defp non_term_type_to_quoted(descr, opts) do
    {dynamic, static, extra} =
      case :maps.take(:dynamic, descr) do
        :error ->
          {%{}, descr, []}

        {:term, static} ->
          {:term, static, []}

        {dynamic, static} ->
          # Computing term_type?(difference(dynamic, static)) can be
          # expensive, so we check for term type before hand and check
          # for :term exclusively in dynamic_to_quoted/2.
          if term_type?(dynamic) do
            {:term, static, []}
          else
            # Denormalize functions before we do the difference
            {static, dynamic, extra} = fun_denormalize(static, dynamic, opts)
            {difference(dynamic, static), static, extra}
          end
      end

    # Merge empty list and list together if they both exist
    {extra, static} =
      case static do
        %{list: list, bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0 ->
          static =
            static
            |> Map.delete(:list)
            |> Map.replace!(:bitmap, bitmap - @bit_empty_list)

          {list_to_quoted(list, true, opts) ++ extra, static}

        %{} ->
          {extra, static}
      end

    # Dynamic always come first for visibility
    unions =
      to_quoted(:dynamic, dynamic, opts) ++
        Enum.sort(
          extra ++ Enum.flat_map(static, fn {key, value} -> to_quoted(key, value, opts) end)
        )

    case unions do
      [] -> {:none, [], []}
      unions -> Enum.reduce(unions, &{:or, [], [&2, &1]})
    end
  end

  defp to_quoted(:atom, val, _opts), do: atom_to_quoted(val)
  defp to_quoted(:bitmap, val, _opts), do: bitmap_to_quoted(val)
  defp to_quoted(:dynamic, descr, opts), do: dynamic_to_quoted(descr, opts)
  defp to_quoted(:map, bdd, opts), do: map_to_quoted(bdd, opts)
  defp to_quoted(:list, bdd, opts), do: list_to_quoted(bdd, false, opts)
  defp to_quoted(:tuple, bdd, opts), do: tuple_to_quoted(bdd, opts)
  defp to_quoted(:fun, bdd, opts), do: fun_to_quoted(bdd, opts)

  @doc """
  Converts a descr to its quoted string representation.

  ## Options

    * `:collapse_structs` - do not show struct fields that match
      their default type
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
  defp subtype_static?(left, right), do: empty_difference_subtype?(left, right)

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

  # * BDD structure: A tree with function nodes and :bdd_top/:bdd_bot leaves.
  #   Paths to :bdd_top represent valid function types. Nodes are positive when
  #   following a left branch (e.g. (int, float -> bool) and negative otherwise.

  # * Function variance:
  #   - Contravariance in arguments: If s <: t, then (t → r) <: (s → r)
  #   - Covariance in returns: If s <: t, then (u → s) <: (u → t)

  # * Representation:
  #   - fun(): Top function type (leaf 1)
  #   - Function literals: {[t1, ..., tn], t} where [t1, ..., tn] are argument types and t is return type
  #   - Normalized form for function applications: {domain, arrows} is produced by `fun_normalize/3`

  # * Examples:
  #   - fun([integer()], atom()): A function from integer to atom
  #   - intersection(fun([integer()], atom()), fun([float()], boolean())): A function handling both signatures

  # Note: Function domains are expressed as tuple types. We use separate representations
  # rather than unary functions with tuple domains to handle special cases like representing
  # functions of a specific arity (e.g., (none,none->term) for arity 2).
  defp fun_new(inputs, output), do: {{inputs, output}, :bdd_top, :bdd_bot}

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
    dynamic_arguments? = any_dynamic?(args)
    dynamic_output? = match?(%{dynamic: _}, output)

    if dynamic_arguments? or dynamic_output? do
      input_static = if dynamic_arguments?, do: Enum.map(args, &upper_bound/1), else: args
      input_dynamic = if dynamic_arguments?, do: Enum.map(args, &lower_bound/1), else: args

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
  Applies a function type to a list of argument types.

  Returns `{:ok, result}` if the application is valid
  or one `{:badarg, to_succeed_domain}`, `:badfun`,
  `{:badarity, arities}` if not.

  Note the domain returned by `:badarg` is not the strong
  domain, but the domain that must be satisfied for the
  function application to succeed.

  Handles both static and dynamic function types:

  1. For static functions: checks exact argument types
  2. For dynamic functions: computes result based on both static and dynamic parts
  3. For mixed static/dynamic: computes all valid combinations

  ## Function application formula for dynamic types

      τ◦τ′ = (lower_bound(τ) ◦ upper_bound(τ′)) ∨ (dynamic(upper_bound(τ) ◦ lower_bound(τ′)))

  Where:

  - τ is a dynamic function type
  - τ′ are the arguments
  - ◦ is function application

  For more details, see Definition 6.15 in https://vlanvin.fr/papers/thesis.pdf

  ## Examples

      iex> fun_apply(fun([integer()], atom()), [integer()])
      {:ok, atom()}

      iex> fun_apply(fun([integer()], atom()), [float()])
      :badarg

      iex> fun_apply(fun([dynamic()], atom()), [dynamic()])
      {:ok, atom()}
  """
  def fun_apply(:term, _arguments) do
    :badfun
  end

  def fun_apply(fun, arguments) do
    case :maps.take(:dynamic, fun) do
      :error ->
        if fun_only?(fun) do
          fun_apply_with_strategy(fun, nil, arguments)
        else
          :badfun
        end

      # Optimize the cases where dynamic closes over all function types
      {:term, fun_static} when fun_static == %{} ->
        {:ok, dynamic()}

      {%{fun: @fun_top}, fun_static} when fun_static == %{} ->
        {:ok, dynamic()}

      {fun_dynamic, fun_static} ->
        if fun_only?(fun_static) do
          fun_apply_with_strategy(fun_static, fun_dynamic, arguments)
        else
          :badfun
        end
    end
  end

  defp fun_only?(descr), do: empty?(Map.delete(descr, :fun))

  defp fun_apply_with_strategy(fun_static, fun_dynamic, arguments) do
    args_dynamic? = any_dynamic?(arguments)
    args_domain = args_to_domain(arguments)
    static? = fun_dynamic == nil and not args_dynamic?
    arity = length(arguments)

    with {:ok, domain, static_arrows, dynamic_arrows} <-
           fun_normalize_both(fun_static, fun_dynamic, arity) do
      cond do
        Enum.any?(arguments, &empty?/1) ->
          {:badarg, domain_to_flat_args(domain, arity)}

        not subtype?(args_domain, domain) ->
          if static? or not compatible?(args_domain, domain) do
            {:badarg, domain_to_flat_args(domain, arity)}
          else
            {:ok, dynamic()}
          end

        static? ->
          {:ok, fun_apply_static(arguments, static_arrows)}

        static_arrows == [] ->
          # TODO: We need to validate this within the theory
          arguments = Enum.map(arguments, &upper_bound/1)
          {:ok, dynamic(fun_apply_static(arguments, dynamic_arrows))}

        true ->
          # For dynamic cases, combine static and dynamic results
          arguments = Enum.map(arguments, &upper_bound/1)

          {:ok,
           union(
             fun_apply_static(arguments, static_arrows),
             dynamic(fun_apply_static(arguments, dynamic_arrows))
           )}
      end
    end
  end

  defp any_dynamic?(arguments), do: Enum.any?(arguments, &match?(%{dynamic: _}, &1))

  defp fun_normalize_both(fun_static, fun_dynamic, arity) do
    case fun_normalize(fun_static, arity, :static) do
      {:ok, static_domain, static_arrows} when fun_dynamic == nil ->
        {:ok, static_domain, static_arrows, static_arrows}

      {:ok, static_domain, static_arrows} when fun_dynamic != nil ->
        case fun_normalize(fun_dynamic, arity, :dynamic) do
          {:ok, dynamic_domain, dynamic_arrows} ->
            domain = union(dynamic_domain, dynamic(static_domain))
            {:ok, domain, static_arrows, dynamic_arrows}

          _ ->
            {:ok, static_domain, static_arrows, static_arrows}
        end

      :badfun ->
        case fun_normalize(fun_dynamic, arity, :dynamic) do
          {:ok, dynamic_domain, dynamic_arrows} ->
            {:ok, union(dynamic_domain, dynamic()), [], dynamic_arrows}

          error ->
            error
        end

      error ->
        error
    end
  end

  # Transforms a binary decision diagram (BDD) into the canonical `domain-arrows` pair:
  #
  # 1. **domain**: The union of all domains from positive functions in the BDD
  # 2. **arrows**: List of lists, where each inner list contains an intersection of function arrows
  #
  # ## Return Values
  #
  # - `{:ok, domain, arrows}` for valid function BDDs
  # - `{:badarity, supported_arities}` if the given arity is not supported
  # - `:badfun` if the BDD represents an empty function type
  #
  # ## Internal Use
  #
  # This function is used internally by `fun_apply_*`, and others to
  # ensure consistent handling of function types in all operations.
  defp fun_normalize(:term, arity, mode) do
    fun_normalize(%{fun: @fun_top}, arity, mode)
  end

  defp fun_normalize(%{fun: bdd}, arity, mode) do
    {domain, arrows, bad_arities} =
      Enum.reduce(fun_get(bdd), {term(), [], []}, fn
        {pos_funs, neg_funs}, {domain, arrows, bad_arities} ->
          arrow_arity =
            case pos_funs do
              [{args, _} | _] -> length(args)
              _ -> arity
            end

          cond do
            arrow_arity != arity ->
              {domain, arrows, [arrow_arity | bad_arities]}

            fun_empty?(pos_funs, neg_funs) ->
              {domain, arrows, bad_arities}

            true ->
              # Calculate domain from all positive functions
              path_domain =
                Enum.reduce(pos_funs, none(), fn {args, _}, acc ->
                  union(acc, args_to_domain(args))
                end)

              {intersection(domain, path_domain), [pos_funs | arrows], bad_arities}
          end
      end)

    case {arrows, bad_arities} do
      {[], []} ->
        :badfun

      {arrows, [_ | _] = bad_arities} when mode == :static or arrows == [] ->
        {:badarity, Enum.uniq(bad_arities)}

      {_, _} ->
        {:ok, domain, arrows}
    end
  end

  defp fun_normalize(%{}, _arity, _mode) do
    :badfun
  end

  defp fun_apply_static(arguments, arrows) do
    type_args = args_to_domain(arguments)

    Enum.reduce(arrows, none(), fn intersection_of_arrows, acc ->
      aux_apply(acc, type_args, term(), intersection_of_arrows)
    end)
  end

  defp apply_disjoint(arguments, arrows) do
    type_args = args_to_domain(arguments)

    Enum.reduce(arrows, none(), fn {args, ret}, acc_return ->
      dom = args_to_domain(args)

      if empty?(intersection(dom, type_args)) do
        acc_return
      else
        union(acc_return, ret)
      end
    end)
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

  defp aux_apply(result, input, returns_reached, [{args, ret} | arrow_intersections]) do
    # Calculate the part of the input not covered by this arrow's domain
    dom_subtract = difference(input, args_to_domain(args))

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

    # Refine the return type by intersecting with this arrow's return type
    ret_refine = intersection(returns_reached, ret)

    # e.g. (integer()->atom()) and (integer()->pid()) when applied to integer()
    # should result in (atom() ∩ pid()), which is none().
    aux_apply(result, input, ret_refine, arrow_intersections)
  end

  # Takes all the paths from the root to the leaves finishing with a 1,
  # and compile into tuples of positive and negative nodes. Positive nodes are
  # those followed by a left path, negative nodes are those followed by a right path.
  defp fun_get(bdd), do: fun_get([], [], [], bdd)

  defp fun_get(acc, pos, neg, bdd) do
    case bdd do
      :bdd_bot -> acc
      :bdd_top -> [{pos, neg} | acc]
      {fun, left, right} -> fun_get(fun_get(acc, [fun | pos], neg, left), pos, [fun | neg], right)
    end
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
      :bdd_bot -> true
      :bdd_top -> false
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
            subtype?(args_to_domain(neg_arguments), positive_domain) and
            phi_starter(neg_arguments, neg_return, positives)
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
        {:cont, {length(args), args_to_domain(args)}}

      {args, _}, {arity, dom} when length(args) == arity ->
        {:cont, {arity, union(dom, args_to_domain(args))}}

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
    # Optimization: When all positive functions have non-empty domains,
    # we can simplify the phi function check to a direct subtyping test.
    # This avoids the expensive recursive phi computation by checking only that applying the
    # input to the positive intersection yields a subtype of the return
    case disjoint_non_empty_domains?({arguments, return}, positives) do
      :disjoint_non_empty ->
        apply_disjoint(arguments, positives) |> subtype?(return)

      :non_empty ->
        fun_apply_static(arguments, [positives]) |> subtype?(return)

      _ ->
        n = length(arguments)
        # Arity mismatch: functions with different arities cannot be subtypes
        # of the target function type (arguments -> return)
        if Enum.any?(positives, fn {args, _ret} -> length(args) != n end) do
          false
        else
          # Initialize memoization cache for the recursive phi computation
          arguments = Enum.map(arguments, &{false, &1})
          {result, _cache} = phi(arguments, {false, negation(return)}, positives, %{})
          result
        end
    end
  end

  defp phi(args, {b, t}, [], cache) do
    result = Enum.any?(args, fn {bool, typ} -> bool and empty?(typ) end) or (b and empty?(t))
    {result, Map.put(cache, {args, {b, t}, []}, result)}
  end

  defp phi(args, {b, ret}, [{arguments, return} | rest_positive], cache) do
    # Create cache key from function arguments
    cache_key = {args, {b, ret}, [{arguments, return} | rest_positive]}

    case cache do
      %{^cache_key => value} ->
        {value, cache}

      %{} ->
        # Compute result and cache it
        {result1, cache} = phi(args, {true, intersection(ret, return)}, rest_positive, cache)

        if not result1 do
          cache = Map.put(cache, cache_key, false)
          {false, cache}
        else
          {_index, result2, cache} =
            Enum.reduce_while(arguments, {0, true, cache}, fn
              type, {index, acc_result, acc_cache} ->
                {new_result, new_cache} =
                  args
                  |> List.update_at(index, fn {_, arg} -> {true, difference(arg, type)} end)
                  |> phi({b, ret}, rest_positive, acc_cache)

                if new_result do
                  {:cont, {index + 1, acc_result and new_result, new_cache}}
                else
                  {:halt, {index + 1, false, new_cache}}
                end
            end)

          result = result1 and result2
          cache = Map.put(cache, cache_key, result)
          {result, cache}
        end
    end
  end

  defp disjoint_non_empty_domains?({arguments, return}, positives) do
    b1 = all_disjoint_arguments?(positives)
    b2 = all_non_empty_arguments?([{arguments, return} | positives])

    cond do
      b1 and b2 -> :disjoint_non_empty
      b2 -> :non_empty
      true -> nil
    end
  end

  defp all_non_empty_arguments?(positives) do
    Enum.all?(positives, fn {args, _ret} ->
      Enum.all?(args, fn arg -> not empty?(arg) end)
    end)
  end

  # For two arguments to be disjoint, one of their types must be disjoint.
  defp disjoint_arguments?(args1, args2) do
    Enum.any?(Enum.zip(args1, args2), fn {t1, t2} -> disjoint?(t1, t2) end)
  end

  defp all_disjoint_arguments?([]), do: true

  defp all_disjoint_arguments?([{args, _} | rest]) do
    Enum.all?(rest, fn {args_rest, _} -> disjoint_arguments?(args, args_rest) end) and
      all_disjoint_arguments?(rest)
  end

  defp fun_union(bdd1, bdd2) do
    case {bdd1, bdd2} do
      {:bdd_top, _} -> :bdd_top
      {_, :bdd_top} -> :bdd_top
      {:bdd_bot, bdd} -> bdd
      {bdd, :bdd_bot} -> bdd
      {{fun, l1, r1}, {fun, l2, r2}} -> {fun, fun_union(l1, l2), fun_union(r1, r2)}
      # Note: this is a deep merge, that goes down bdd1 to insert bdd2 into it.
      # It is the same as going down bdd1 to insert bdd1 into it.
      # Possible opti: insert into the bdd with smallest height
      {{fun, l, r}, bdd} -> {fun, fun_union(l, bdd), fun_union(r, bdd)}
    end
  end

  defp is_fun_top?(bdd, {{args, return}, :bdd_top, :bdd_bot}) do
    return == :term and Enum.all?(args, &(&1 == %{})) and
      matching_arity_left?(bdd, length(args))
  end

  defp is_fun_top?(_, _), do: false

  defp fun_intersection(bdd1, bdd2) do
    cond do
      # If intersecting with the top type for that arity, no-op
      is_tuple(bdd2) and is_fun_top?(bdd2, bdd1) -> bdd2
      is_tuple(bdd1) and is_fun_top?(bdd1, bdd2) -> bdd1
      true -> fun_bdd_intersection(bdd1, bdd2)
    end
  end

  defp fun_bdd_intersection(bdd1, bdd2) do
    case {bdd1, bdd2} do
      # Base cases
      {_, :bdd_bot} ->
        :bdd_bot

      {:bdd_bot, _} ->
        :bdd_bot

      {:bdd_top, bdd} ->
        bdd

      {bdd, :bdd_top} ->
        bdd

      # Optimizations
      # If intersecting with a single positive or negative function, we insert
      # it at the root instead of merging the trees (this avoids going down the
      # whole bdd).
      {bdd, {fun, :bdd_top, :bdd_bot}} ->
        {fun, bdd, :bdd_bot}

      {bdd, {fun, :bdd_bot, :bdd_top}} ->
        {fun, :bdd_bot, bdd}

      {{fun, :bdd_top, :bdd_bot}, bdd} ->
        {fun, bdd, :bdd_bot}

      {{fun, :bdd_bot, :bdd_top}, bdd} ->
        {fun, :bdd_bot, bdd}

      # General cases
      {{fun, l1, r1}, {fun, l2, r2}} ->
        {fun, fun_bdd_intersection(l1, l2), fun_bdd_intersection(r1, r2)}

      {{fun, l, r}, bdd} ->
        {fun, fun_bdd_intersection(l, bdd), fun_bdd_intersection(r, bdd)}
    end
  end

  defp matching_arity_left?({{args, _return}, l, r}, arity) do
    length(args) == arity and matching_arity_left?(l, arity) and matching_arity_right?(r, arity)
  end

  defp matching_arity_left?(_, _arity), do: true

  defp matching_arity_right?({_, l, r}, arity) do
    matching_arity_left?(l, arity) and matching_arity_right?(r, arity)
  end

  defp matching_arity_right?(_, _arity), do: true

  defp fun_difference(bdd1, bdd2), do: bdd_difference(bdd1, bdd2)

  # Converts the static and dynamic parts of descr to its quoted
  # representation. The goal here is to the opposite of fun_descr
  # and put static and dynamic parts back together to improve
  # pretty printing.
  defp fun_denormalize(%{fun: static_bdd} = static, %{fun: dynamic_bdd} = dynamic, opts) do
    static_pos = fun_get_pos(static_bdd)
    dynamic_pos = fun_get_pos(dynamic_bdd)

    if static_pos != [] and dynamic_pos != [] do
      {static_pos, dynamic_pos} = fun_denormalize_pos(static_pos, dynamic_pos)

      quoted =
        if dynamic_pos == [] do
          fun_pos_to_quoted(static_pos, opts)
        else
          {:or, [],
           [
             {:dynamic, [], [fun_pos_to_quoted(dynamic_pos, opts)]},
             fun_pos_to_quoted(static_pos, opts)
           ]}
        end

      {Map.delete(static, :fun), Map.delete(dynamic, :fun), [quoted]}
    else
      {static, dynamic, []}
    end
  end

  defp fun_denormalize(static, dynamic, _opts) do
    {static, dynamic, []}
  end

  defp fun_denormalize_pos(static_unions, dynamic_unions) do
    Enum.map_reduce(static_unions, dynamic_unions, fn
      # Handle fun() types accordingly
      [], dynamic_unions ->
        {[], List.delete(dynamic_unions, [])}

      static_intersections, dynamic_unions ->
        case pivot(dynamic_unions, [], &fun_denormalize_intersections(static_intersections, &1)) do
          {match, dynamic_unions} -> {match, dynamic_unions}
          :error -> {static_intersections, dynamic_unions}
        end
    end)
  end

  defp fun_denormalize_intersections(statics, dynamics) do
    if length(statics) == length(dynamics) do
      fun_denormalize_intersections(statics, dynamics, [])
    else
      :error
    end
  end

  # We assume those pairs are always formed in the same order
  defp fun_denormalize_intersections(
         [{static_args, static_return} | statics],
         [{dynamic_args, dynamic_return} | dynamics],
         acc
       ) do
    if subtype?(static_return, dynamic_return) and args_subtype?(dynamic_args, static_args) do
      args =
        Enum.zip_with(static_args, dynamic_args, fn static_arg, dynamic_arg ->
          union(dynamic(static_arg), dynamic_arg)
        end)

      return = union(dynamic(dynamic_return), static_return)
      fun_denormalize_intersections(statics, dynamics, [{args, return} | acc])
    else
      :error
    end
  end

  defp fun_denormalize_intersections([], [], acc), do: {:ok, acc}

  defp arrow_subtype?(left_args, left_return, right_args, right_return) do
    subtype?(left_return, right_return) and args_subtype?(right_args, left_args)
  end

  defp args_subtype?(left_args, right_args) do
    Enum.zip_reduce(left_args, right_args, true, fn left, right, acc ->
      acc and subtype?(left, right)
    end)
  end

  defp pivot([head | tail], acc, fun) do
    case fun.(head) do
      {:ok, value} -> {value, acc ++ tail}
      :error -> pivot(tail, [head | acc], fun)
    end
  end

  defp pivot([], _acc, _fun), do: :error

  # Converts a function BDD (Binary Decision Diagram) to its quoted representation
  defp fun_to_quoted(bdd, opts) do
    case fun_get_pos(bdd) do
      [] -> []
      pos -> [fun_pos_to_quoted(pos, opts)]
    end
  end

  defp fun_get_pos(bdd) do
    for {pos, negs} <- fun_get(bdd), not fun_empty?(pos, negs) do
      fun_filter_subset(pos, [])
    end
  end

  defp fun_filter_subset([], acc), do: acc

  defp fun_filter_subset([{args, return} | tail], acc) do
    # If another arrow is a subset of the current one, we skip it
    if Enum.any?(tail, fn {other_args, other_return} ->
         arrow_subtype?(other_args, other_return, args, return)
       end) or
         Enum.any?(acc, fn {other_args, other_return} ->
           arrow_subtype?(other_args, other_return, args, return)
         end) do
      fun_filter_subset(tail, acc)
    else
      fun_filter_subset(tail, [{args, return} | acc])
    end
  end

  defp fun_pos_to_quoted([_ | _] = pos, opts) do
    opts = Keyword.put(opts, :skip_dynamic_for_indivisible, false)

    pos
    |> Enum.sort()
    |> Enum.map(&fun_intersection_to_quoted(&1, opts))
    |> Enum.reduce(&{:or, [], [&2, &1]})
  end

  defp fun_intersection_to_quoted(intersection, opts) do
    intersection
    |> Enum.sort()
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

  # Represents list and improper list simultaneously as a BDD with nodes of the form
  # `{list_type, last_type}`, where `list_type` is the type of elements in the list,
  # and `last_type` is the type of the last element (so `[]` if the list is proper,
  # and anything else but a list if the list is improper).

  # The last_type may be stored as `:term` for optimization purposes. This is ok
  # because operations like `tl` effectively return the list itself plus the union of
  # the tail (and if the tail includes the list itself, they are equivalent). And, for other
  # operations like difference, we expand the tail_type back into
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

          {bdd, last_type_no_list} ->
            # `last_type` may itself represent one or more list types.
            # Our goal is to fold those list types into `list_type` while retaining the
            # possible type of the final element (which can be `[]` or any non-list value).
            #
            # The list types inside `last_type` are stored in a BDD that includes possible
            # negations, so we must evaluate each node with its sign taken into account.
            #
            # A negation only matters when the negated list type is a supertype of the
            # corresponding positive list type; in that case we subtract the negated
            # variant from the positive one.
            {list_type, last_type} =
              list_get_pos(bdd)
              |> Enum.reduce({list_type, last_type_no_list}, fn {head, tail},
                                                                {acc_head, acc_tail} ->
                tail = list_tail_unfold(tail)
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
    {{list_type, last_type}, :bdd_top, :bdd_bot}
  end

  # Takes all the lines from the root to the leaves finishing with a 1,
  # and compile into tuples of positive and negative nodes. Positive nodes are
  # those followed by a left path, negative nodes are those followed by a right path.
  defp list_get(bdd), do: list_get([], {:term, :term}, [], bdd)

  defp list_get(acc, {list_acc, tail_acc} = pos, negs, bdd) do
    case bdd do
      :bdd_bot ->
        acc

      :bdd_top ->
        if list_empty?(list_acc, tail_acc, negs), do: acc, else: [{pos, negs} | acc]

      {{list, tail} = list_type, left, right} ->
        new_pos = {intersection(list_acc, list), intersection(tail_acc, tail)}
        list_get(list_get(acc, new_pos, negs, left), pos, [list_type | negs], right)
    end
  end

  # Takes all the lines from the root to the leaves finishing with a 1,
  # and compile into tuples of positive and negative nodes. Keep only the non-empty positives,
  # and include the impact of negations on the last type.
  # To see if a negation changes the last type or the list type, we just need to check
  # if the negative list type is a supertype of the positive list type. In that case,
  # we can remove the negative last type from the positive one.
  # (If this subtracted type was empty, the whole type would be empty)
  defp list_get_pos(bdd), do: list_get_pos(:term, :term, bdd, [])

  defp list_get_pos(list_acc, last_acc, bdd, lines_acc) do
    case bdd do
      :bdd_bot ->
        lines_acc

      :bdd_top ->
        [{list_acc, last_acc} | lines_acc]

      {{list, last}, left, right} ->
        # Case 1: count the list_type negatively. Check condition when it affects the positive one.
        lines_acc =
          if subtype?(list_acc, list) do
            last = difference(last_acc, last)
            if empty?(last), do: lines_acc, else: list_get_pos(list_acc, last, right, lines_acc)
          else
            list_get_pos(list_acc, last_acc, right, lines_acc)
          end

        # Case 2: count the list_type positively.
        list_acc = intersection(list_acc, list)
        last_acc = intersection(last_acc, last)

        if empty?(list_acc) or empty?(last_acc) do
          lines_acc
        else
          list_get_pos(list_acc, last_acc, left, lines_acc)
        end
    end
  end

  # Takes all the lines from the root to the leaves finishing with a 1, computes the intersection
  # of the positives, and calls the condition on the result. Checks it is true for all of them.
  # As if calling Enum.all? on all the lines of the bdd.
  defp list_all?(bdd, condition), do: list_all?({:term, :term}, [], bdd, condition)

  defp list_all?({list_acc, tail_acc} = pos, negs, bdd, condition) do
    case bdd do
      :bdd_bot ->
        true

      :bdd_top ->
        condition.(list_acc, tail_acc, negs)

      {{list, tail} = list_type, left, right} ->
        list_all?(
          {intersection(list_acc, list), intersection(tail_acc, tail)},
          negs,
          left,
          condition
        ) and
          list_all?(pos, [list_type | negs], right, condition)
    end
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

  @compile {:inline, list_union: 2}
  defp list_union(bdd1, bdd2), do: bdd_union(bdd1, bdd2)

  defp is_list_top?({{list, tail}, :bdd_top, :bdd_bot}) do
    list == :term and tail == :term
  end

  defp is_list_top?(_), do: false

  defp list_intersection(list_literal(list1, last1), list_literal(list2, last2)) do
    try do
      list = non_empty_intersection!(list1, list2)
      last = non_empty_intersection!(last1, last2)
      list_literal(list, last)
    catch
      :empty -> :bdd_bot
    end
  end

  defp list_intersection(bdd1, bdd2) do
    cond do
      is_list_top?(bdd1) and is_tuple(bdd2) -> bdd2
      is_list_top?(bdd2) and is_tuple(bdd1) -> bdd1
      true -> bdd_intersection(bdd1, bdd2)
    end
    |> case do
      {_, :bdd_bot, :bdd_bot} -> :bdd_bot
      bdd -> bdd
    end
  end

  # Computes the difference between two BDD (Binary Decision Diagram) list types.
  # It progressively subtracts each type in bdd2 from all types in bdd1.
  # The algorithm handles three cases:
  # 1. Disjoint types: keeps the original type from bdd1
  # 2. Subtype relationship:
  #    a) If bdd2 type is a supertype, keeps only the negations
  #    b) If only the last type differs, subtracts it
  # 3. Base case: adds bdd2 type to negations of bdd1 type
  # The result may be larger than the initial bdd1, which is maintained in the accumulator.
  defp list_difference(list_literal(list1, last1) = bdd1, list_literal(list2, last2) = bdd2) do
    list = intersection(list1, list2)
    last = intersection(last1, last2)

    cond do
      empty?(list) or empty?(last) -> list_literal(list1, last1)
      subtype?(list1, list2) and subtype?(last1, last2) -> :bdd_bot
      equal?(list1, list2) -> list_literal(list1, difference(last1, last2))
      true -> bdd_difference(bdd1, bdd2)
    end
  end

  defp list_difference(bdd1, bdd2) do
    bdd_difference(bdd1, bdd2)
    |> case do
      {_, :bdd_bot, :bdd_bot} -> :bdd_bot
      bdd -> bdd
    end
  end

  defp list_empty?(@non_empty_list_top), do: false
  defp list_empty?(bdd), do: list_all?(bdd, &list_empty?/3)

  defp list_empty?(list_type, last_type, negs) do
    last_type = list_tail_unfold(last_type)
    # To make a list {list, last} empty with some negative lists:
    # 1. Ignore negative lists which do not have a list type that is a supertype of the positive one.
    # 2. Each of the list supertypes:
    #     a. either completely covers the type, if its last type is a supertype of the positive one,
    #     b. or it removes part of the last type.
    empty?(list_type) or empty?(last_type) or
      Enum.reduce_while(negs, last_type, fn {neg_type, neg_last}, acc_last_type ->
        if subtype?(list_type, neg_type) do
          d = difference(acc_last_type, neg_last)
          if empty?(d), do: {:halt, nil}, else: {:cont, d}
        else
          {:cont, acc_last_type}
        end
      end)
      |> is_nil()
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

  defp list_hd_static(%{list: bdd}) do
    list_get_pos(bdd)
    |> Enum.reduce(none(), fn {list, _}, acc ->
      union(list, acc)
    end)
  end

  defp list_hd_static(%{}), do: none()

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

  defp list_tl_static(%{list: bdd} = descr) do
    initial =
      case descr do
        %{bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0 ->
          %{list: bdd, bitmap: @bit_empty_list}

        %{} ->
          %{list: bdd}
      end

    list_get_pos(bdd) |> Enum.reduce(initial, fn {_, tail}, acc -> union(tail, acc) end)
  end

  defp list_tl_static(%{}), do: none()

  defp list_improper_static?(:term), do: false
  defp list_improper_static?(%{bitmap: bitmap}) when (bitmap &&& @bit_empty_list) != 0, do: false
  defp list_improper_static?(term), do: equal?(term, @not_list)

  defp list_to_quoted(bdd, empty?, opts) do
    dnf = list_normalize(bdd)

    {unions, list_rendered?} =
      dnf
      |> Enum.reduce({[], false}, fn {list_type, last_type, negs}, {acc, list_rendered?} ->
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
  defp list_normalize(bdd) do
    list_get(bdd)
    |> Enum.reduce([], fn {{list, last}, negs}, acc ->
      # First, try to eliminate the negations from the existing type.
      {list, last, negs} =
        Enum.uniq(negs)
        |> Enum.reduce({list, last, []}, fn {nlist, nlast}, {acc_list, acc_last, acc_negs} ->
          last = list_tail_unfold(last)
          new_list = intersection(list, nlist)
          new_last = intersection(last, nlast)

          cond do
            # No intersection between the list and the negative
            empty?(new_list) or empty?(new_last) -> {acc_list, acc_last, acc_negs}
            subtype?(list, nlist) -> {acc_list, difference(acc_last, nlast), acc_negs}
            true -> {acc_list, acc_last, [{nlist, nlast} | acc_negs]}
          end
        end)

      add_to_list_normalize(acc, list, last, negs |> Enum.reverse())
    end)
  end

  # List of possible union merges:
  # Case 1: when a list is a subtype of another
  # Case 2: when two lists have the same list type, then the last types are united
  defp add_to_list_normalize([{t, l, []} = cur | rest], list, last, []) do
    cond do
      subtype?(list, t) and subtype?(last, l) -> [cur | rest]
      subtype?(t, list) and subtype?(l, last) -> [{list, last, []} | rest]
      equal?(t, list) -> [{t, union(l, last), []} | rest]
      true -> [cur | add_to_list_normalize(rest, list, last, [])]
    end
  end

  # Case 3: when a list with negations is united with one of its negations
  defp add_to_list_normalize([{t, l, n} = cur | rest], list, last, []) do
    case pop_elem({list, last}, n) do
      {true, n1} -> [{t, l, n1} | rest]
      {false, _} -> [cur | add_to_list_normalize(rest, list, last, n)]
    end
  end

  defp add_to_list_normalize(rest, list, last, negs), do: [{list, last, negs} | rest]

  defp pop_elem(elem, list) do
    case :lists.delete(elem, list) do
      ^list -> {false, list}
      new_list -> {true, new_list}
    end
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

  defp dynamic_union(:term, other), do: optional_to_term(other)
  defp dynamic_union(other, :term), do: optional_to_term(other)

  defp dynamic_union(left, right),
    do: symmetrical_merge(unfold(left), unfold(right), &union/3)

  defp dynamic_intersection(:term, other), do: remove_optional_static(other)
  defp dynamic_intersection(other, :term), do: remove_optional_static(other)

  defp dynamic_intersection(left, right),
    do: symmetrical_intersection(unfold(left), unfold(right), &intersection/3)

  defp dynamic_to_quoted(descr, opts) do
    cond do
      # We check for :term literally instead of using term_type?
      # because we check for term_type? in to_quoted before we
      # compute the difference(dynamic, static).
      descr == :term ->
        [{:dynamic, [], []}]

      single = indivisible_bitmap(descr, opts) ->
        [single]

      empty?(descr) ->
        []

      true ->
        case non_term_type_to_quoted(descr, opts) do
          {:none, _meta, []} = none -> [none]
          descr -> [{:dynamic, [], [descr]}]
        end
    end
  end

  defp indivisible_bitmap(descr, opts) do
    with true <- Keyword.get(opts, :skip_dynamic_for_indivisible, true),
         %{bitmap: bitmap} when map_size(descr) == 1 <- descr,
         [single] <- bitmap_to_quoted(bitmap) do
      single
    else
      _ -> nil
    end
  end

  ## Map
  #
  # Maps are represented as BDDs, that is, a tree of pairs `{tag_or_domain, fields}`
  # where `tag_or_domain` is either :closed or :open, or a map from domain keys
  # (@domain_key_types) to types, and `fields` is a map of atom keys (:foo, :bar, ...)
  # to types.
  #
  # For instance, the type `%{..., a: integer()} and not %{b: atom()}` can be represented
  # by the BDD containing one pair of shape:
  #
  #     {{:open, %{:a => integer()}}, {{:closed, %{:b => atom()}}, :bdd_bot, :bdd_top}, :bdd_bot}
  #
  # which can be seen as:
  #
  #     └─ %{..., a: integer()}
  #      ├─ %{b: atom()}
  #      │  ├─ :bdd_bot
  #      │  └─ :bdd_top
  #      └─ :bdd_bot
  #
  # and is interpreted as the intersection of `%{..., a: integer()}` with
  # `not %{b: atom()}`, since the only path from the root to the leaves which
  # ends with `:bdd_top` is the one which takes the first (left) branch after
  # `%{..., a: integer()}`, and the second (right) branch after `%{b: atom()}`.
  #
  # This representation keeps negations symbolic, and avoids distributing difference on
  # every member of a union which creates a lot of map literals in the union and
  # requires emptiness checks to avoid creating empty maps.
  #
  # For instance, the difference between `%{...}` and `%{a: atom(), b: integer()}`
  # is the union of `%{..., a: atom(), b: if_set(not integer())}` and
  # `%{..., a: if_set(not atom()), b: integer()}`. For maps with more keys,
  # each key in a negated literal may create a new union when eliminated.
  #
  # Instead of a tag :open or :closed, we can use a map of domains which
  # specifies for each defined key domain (@domain_key_types) the type associated with
  # those keys.
  #
  # For instance, the type `%{atom() => if_set(integer())}` is the type of maps where atom keys
  # map to integers, without any non-atom keys. It is represented using the map literal
  # `{%{atom: if_set(integer())}, [], []}`, with no defined keys or negations.
  #
  # The type `%{..., atom() => integer()}` represents maps with atom keys bound to integers,
  # and other keys bound to any type. It will be represented using a map domain that maps
  # atom to `if_set(integer())`, and every other domain key to `term_or_optional()`.

  defp map_descr(tag, pairs, default, force?) do
    {fields, domains, dynamic?} = map_descr_pairs(pairs, [], %{}, false)

    map_new =
      if domains != %{} or force? do
        domains =
          if tag == :open do
            Enum.reduce(@domain_key_types, domains, &Map.put_new(&2, &1, default))
          else
            domains
          end

        map_new(domains, fields)
      else
        map_new(tag, fields)
      end

    case dynamic? do
      true -> %{dynamic: %{map: map_new}}
      false -> %{map: map_new}
    end
  end

  # TODO: Double check if we indeed want the union here
  # when we start using domain types from Elixir itself
  defp map_put_domain(domain, key, value) do
    Map.update(domain, key, if_set(value), &union(&1, value))
  end

  defp map_descr_pairs([{key, :term} | rest], fields, domain, dynamic?) do
    case is_atom(key) do
      true -> map_descr_pairs(rest, [{key, :term} | fields], domain, dynamic?)
      false -> map_descr_pairs(rest, fields, map_put_domain(domain, key, :term), dynamic?)
    end
  end

  defp map_descr_pairs([{key, value} | rest], fields, domain, dynamic?) do
    {value, dynamic?} =
      case :maps.take(:dynamic, value) do
        :error -> {value, dynamic?}
        {dynamic, _static} -> {dynamic, true}
      end

    case is_atom(key) do
      true -> map_descr_pairs(rest, [{key, value} | fields], domain, dynamic?)
      false -> map_descr_pairs(rest, fields, map_put_domain(domain, key, value), dynamic?)
    end
  end

  defp map_descr_pairs([], fields, domain, dynamic?) do
    {fields |> Enum.reverse() |> :maps.from_list(), domain, dynamic?}
  end

  defp tuple_tag_to_type(:open), do: term_or_optional()
  defp tuple_tag_to_type(:closed), do: not_set()

  # Gets the default type associated to atom keys in a map.
  defp map_key_tag_to_type(:open), do: term_or_optional()
  defp map_key_tag_to_type(:closed), do: not_set()
  defp map_key_tag_to_type(domains = %{}), do: Map.get(domains, domain_key(:atom), not_set())

  defguardp is_optional_static(map)
            when is_map(map) and is_map_key(map, :optional)

  defp map_new(tag, fields = %{}), do: {{tag, fields}, :bdd_top, :bdd_bot}

  defp map_only?(descr), do: empty?(Map.delete(descr, :map))

  defp non_empty_map_only?(descr) do
    case :maps.take(:map, descr) do
      :error -> false
      {map_bdd, rest} -> empty?(rest) and not map_empty?(map_bdd)
    end
  end

  def map_union(map_literal(tag1, fields1), map_literal(tag2, fields2)) do
    case maybe_optimize_map_union({tag1, fields1, []}, {tag2, fields2, []}) do
      {tag, fields, []} -> map_literal(tag, fields)
      nil -> {{tag1, fields1}, :bdd_top, {{tag2, fields2}, :bdd_top, :bdd_bot}}
    end
  end

  def map_union(bdd1, bdd2), do: bdd_union(bdd1, bdd2)

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

  defp is_map_top?(map_literal(:open, fields)) when map_size(fields) == 0, do: true
  defp is_map_top?(_), do: false

  defp map_intersection(map_literal(tag1, fields1), map_literal(tag2, fields2)) do
    try do
      map = map_literal_intersection(tag1, fields1, tag2, fields2)
      {map, :bdd_top, :bdd_bot}
    catch
      :empty -> :bdd_bot
    end
  end

  defp map_intersection(bdd1, bdd2) do
    # If intersecting with the top map type, no-op
    cond do
      is_map_top?(bdd1) and is_tuple(bdd2) -> bdd2
      is_map_top?(bdd2) and is_tuple(bdd1) -> bdd1
      true -> bdd_intersection(bdd1, bdd2)
    end
  end

  # Optimizations on single maps.
  defp map_difference(map_literal(tag, fields) = map1, map_literal(neg_tag, neg_fields) = map2) do
    # Case 1: we are removing an open map with one field. Just do the difference of that field.
    if neg_tag == :open and map_size(neg_fields) == 1 do
      {key, value, _rest} = :maps.next(:maps.iterator(neg_fields))
      t_diff = difference(Map.get(fields, key, map_key_tag_to_type(tag)), value)

      if empty?(t_diff) do
        :bdd_bot
      else
        map_literal(tag, Map.put(fields, key, t_diff))
      end
    else
      # Case 2: the maps have all but one key in common. Do the difference of that key.
      case map_all_but_one(tag, fields, neg_tag, neg_fields) do
        {:one, diff_key} ->
          map_literal(tag, Map.update!(fields, diff_key, &difference(&1, neg_fields[diff_key])))

        _ ->
          bdd_difference(map1, map2)
      end
    end
  end

  defp map_difference(bdd1, bdd2), do: bdd_difference(bdd1, bdd2)

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

  # At least one tag is a tag-domain pair.
  defp map_literal_intersection(tag_or_domains1, map1, tag_or_domains2, map2) do
    # For a closed map with domains intersected with an open map with domains:
    # 1. The result is closed (more restrictive)
    # 2. We need to check each domain in the open map against the closed map
    default1 = map_key_tag_to_type(tag_or_domains1)
    default2 = map_key_tag_to_type(tag_or_domains2)

    # Compute the new domain
    tag_or_domains = map_domain_intersection(tag_or_domains1, tag_or_domains2)

    # Go over all fields in map1 and map2 with default atom types atom1 and atom2
    # 1. If key is in both maps, compute non empty intersection (:error if it is none)
    # 2. If key is only in map1, compute non empty intersection with atom2
    # 3. If key is only in map2, compute non empty intersection with atom1
    # We do that by computing intersection on all key labels in both map1 and map2,
    # using default values when a key is not present.
    {tag_or_domains,
     symmetrical_merge(map1, default1, map2, default2, fn _key, v1, v2 ->
       non_empty_intersection!(v1, v2)
     end)}
  end

  # Compute the intersection of two tags or tag-domain pairs.
  defp map_domain_intersection(:closed, _), do: :closed
  defp map_domain_intersection(_, :closed), do: :closed
  defp map_domain_intersection(:open, tag_or_domains), do: tag_or_domains
  defp map_domain_intersection(tag_or_domains, :open), do: tag_or_domains

  defp map_domain_intersection(domains1 = %{}, domains2 = %{}) do
    new_domains =
      for {domain_key(_) = domain_key, type1} <- domains1, reduce: %{} do
        acc_domains ->
          case domains2 do
            %{^domain_key => type2} ->
              inter = intersection(type1, type2)

              if empty_or_optional?(inter) do
                acc_domains
              else
                Map.put(acc_domains, domain_key, inter)
              end

            _ ->
              acc_domains
          end
      end

    # If the explicit domains are empty, use simple atom tags
    if map_size(new_domains) == 0, do: :closed, else: new_domains
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

  # Takes all the lines from the root to the leaves finishing with a 1,
  # and compile into tuples of positive and negative nodes. Positive nodes are
  # those followed by a left path, negative nodes are those followed by a right path.
  def map_bdd_get(bdd), do: map_bdd_get([], {:open, %{}}, [], bdd)

  defp map_bdd_get(acc, {tag, fields} = map, negs, bdd) do
    case bdd do
      :bdd_bot ->
        acc

      :bdd_top ->
        if map_empty?(tag, fields, negs), do: acc, else: [{tag, fields, negs} | acc]

      {{next_tag, next_fields} = next_map, left, right} ->
        acc =
          try do
            new_pos = map_literal_intersection(tag, fields, next_tag, next_fields)
            map_bdd_get(acc, new_pos, negs, left)
          catch
            :empty -> acc
          end

        map_bdd_get(acc, map, [next_map | negs], right)
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
        if descr_key?(descr, :map) and non_empty_map_only?(descr) do
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
  defp map_fetch_static(%{map: {{tag_or_domains, fields}, :bdd_top, :bdd_bot}}, key)
       when not is_map_key(fields, key) do
    map_key_tag_to_type(tag_or_domains) |> pop_optional_static()
  end

  # Takes a map dnf and returns the union of types it can take for a given key.
  # If the key may be undefined, it will contain the `not_set()` type.
  defp map_fetch_static(%{map: bdd}, key) do
    map_bdd_get(bdd)
    |> Enum.reduce(none(), fn
      # Optimization: if there are no negatives and key exists, return its value
      {_tag, %{^key => value}, []}, acc ->
        value |> union(acc)

      # Optimization: if there are no negatives and the key does not exist, return the default one.
      {tag, %{}, []}, acc ->
        map_key_tag_to_type(tag) |> union(acc)

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

  @doc """
  Refreshes the type of map after assuming some type was given to a key of a given type.
  Assuming that the descr is exclusively a map (or dynamic).
  """
  # TODO: Figure out how this operation will be used from Elixir
  def map_refresh(:term, _key, _type), do: :badmap

  def map_refresh(descr, key_descr, type) do
    {dynamic_descr, static_descr} = Map.pop(descr, :dynamic)
    key_descr = unfold(key_descr)
    type = unfold(type)

    cond do
      # Either 1) static part is a map, or 2) static part is empty and dynamic part contains maps
      not map_only?(static_descr) ->
        :badmap

      empty?(static_descr) and not (not is_nil(dynamic_descr) and descr_key?(dynamic_descr, :map)) ->
        :badmap

      # Either of those three types could be dynamic.
      not (not is_nil(dynamic_descr) or Map.has_key?(key_descr, :dynamic) or
               Map.has_key?(type, :dynamic)) ->
        map_refresh_static(descr, key_descr, type)

      true ->
        # If one of those is dynamic, we just compute the union
        {descr_dynamic, descr_static} = Map.pop(descr, :dynamic, descr)
        {key_dynamic, key_static} = Map.pop(key_descr, :dynamic, key_descr)
        {type_dynamic, type_static} = Map.pop(type, :dynamic, type)

        with {:ok, new_static} <- map_refresh_static(descr_static, key_static, type_static),
             {:ok, new_dynamic} <- map_refresh_static(descr_dynamic, key_dynamic, type_dynamic) do
          {:ok, union(new_static, dynamic(new_dynamic))}
        end
    end
  end

  def map_refresh_static(%{map: _} = descr, key_descr = %{}, type) do
    # Check if descr is a valid map,
    case atom_fetch(key_descr) do
      # If the key_descr is a singleton, we directly put the type into the map.
      {:finite, [single_key]} ->
        map_put(descr, single_key, type)

      # In this case, we iterate on key_descr to add type to each key type it covers.
      # Since we do not know which key will be used, we do the union with previous types.
      _ ->
        new_descr =
          key_descr
          |> covered_key_types()
          |> Enum.reduce(descr, fn
            {:atom, atom_key}, acc ->
              map_refresh_atom(acc, atom_key, type)

            domain_key, acc ->
              map_refresh_domain(acc, domain_key, type)
          end)

        {:ok, new_descr}
    end
  end

  def map_refresh_static(:term, _key_descr, _type), do: {:ok, open_map()}
  def map_refresh_static(_, _, _), do: {:ok, none()}

  @doc """
  Updates a key in a map type by fetching its current type, unioning it with a
  `new_additional_type`, and then putting the resulting union type back.

  Returns:
    - `{:ok, new_map_descr}`: If successful.
    - `:badmap`: If the input `descr` is not a valid map type.
    - `:badkey`: If the key is considered invalid during the take operation (e.g.,
      an optional key that resolves to an empty type).
  """
  # TODO: Figure out how this operation will be used from Elixir
  def map_refresh_key(descr, key, new_additional_type) when is_atom(key) do
    case map_fetch(descr, key) do
      :badmap ->
        :badmap

      # Key is not present: we just add the new one and make it optional.
      :badkey ->
        with {:ok, descr} <- map_put(descr, key, if_set(new_additional_type)) do
          descr
        end

      {_optional?, current_key_type} ->
        type_to_put = union(current_key_type, new_additional_type)

        case map_fetch_and_put(descr, key, type_to_put) do
          {_taken_type, new_map_descr} -> new_map_descr
          # Propagates :badmap or :badkey from map_fetch_and_put
          error -> error
        end
    end
  end

  def map_refresh_domain(%{map: {{tag, fields}, :bdd_top, :bdd_bot}}, domain, type) do
    %{map: {{map_refresh_tag(tag, domain, type), fields}, :bdd_top, :bdd_bot}}
  end

  def map_refresh_domain(%{map: bdd}, domain, type) do
    # For negations, we count on the idea that a negation will not remove any
    # type from a domain unless it completely cancels out the type.
    # So for any non-empty map dnf, we just update the domain with the new type,
    # as well as its negations to keep them accurate.
    %{map: bdd_map(bdd, fn {tag, fields} -> {map_refresh_tag(tag, domain, type), fields} end)}
  end

  def map_refresh_atom(descr = %{map: bdd}, atom_key, type) do
    case atom_key do
      {:union, keys} ->
        keys
        |> :sets.to_list()
        |> Enum.reduce(descr, fn key, acc -> map_refresh_key(acc, key, type) end)

      {:negation, keys} ->
        # 1) Fetch all the possible keys in the bdd
        # 2) Get them all, except the ones in neg_atoms
        considered_keys = map_fetch_all_key_names(bdd) |> :sets.subtract(keys)

        considered_keys
        |> :sets.to_list()
        |> Enum.reduce(descr, fn key, acc -> map_refresh_key(acc, key, type) end)
        |> map_refresh_domain(domain_key(:atom), type)
    end
  end

  def map_refresh_tag(tag_or_domains, domain_key, type) do
    case tag_or_domains do
      :open -> :open
      :closed -> %{domain_key => if_set(type)}
      domains = %{} -> Map.update(domains, domain_key, if_set(type), &union(&1, type))
    end
  end

  defp map_put_shared(descr, key, type) do
    with {nil, descr} <- map_take(descr, key, nil, &map_put_static(&1, key, type)) do
      {:ok, descr}
    end
  end

  # Directly inserts a key of a given type into every positive and negative map.
  defp map_put_static(%{map: bdd} = descr, key, type) do
    bdd = bdd_map(bdd, fn {tag, fields} -> {tag, Map.put(fields, key, type)} end)

    %{descr | map: bdd}
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
  Computes the union of types for keys matching `key_type` within the `map_type`.

  This generalizes `map_fetch/2` (which operates on a single literal key) to
  work with a key type (e.g., `atom()`, `integer()`, `:a or :b`). It's based
  on the map-selection operator t.[t'] described in Section 4.2 of "Typing Records,
  Maps, and Structs" (Castagna et al., ICFP 2023).

  ## Return Values

  The function returns a tuple indicating the outcome and the resulting type union:

  * `{:ok, type}`: Standard success. `type` is the resulting union of types
    found for the matching keys. This covers two sub-cases:
      * **Keys definitely exist:** If `disjoint?(type, not_set())` is true,
        all keys matching `key_type` are guaranteed to exist.
      * **Keys may exist:** If `type` includes `not_set()`, some keys
        matching `key_type` might exist (contributing their types) while
        others might be absent (contributing `not_set()`).

  * `{:ok_absent, type}`: Success, but the resulting `type` is `none()` or a
    subtype of `not_set()`. This indicates that no key matching `key_type`
    can exist with a value other than `not_set()`. The caller may wish to
    issue a warning, as this often implies selecting a field that is
    effectively undefined.

  # TODO: implement/decide if worth it (it's from the paper)
  * `{:ok_spillover, type}`: Success, and `type` is the resulting union.
    However, this indicates that the `key_type` included keys not explicitly
    covered by the `map_type`'s fields or domain specifications. The
    projection relied on the map's default behavior (e.g., the `term()`
    value type for unspecified keys in an open map). The caller may wish to
    issue a warning, as this could conceal issues like selecting keys
    not intended by the map's definition.

  * `:badmap`: The input `map_type` was invalid (e.g., not a map type or
    a dynamic type wrapping a map type).

  * `:badkeytype`: The input `key_type` was invalid (e.g., not a subtype
    of the allowed key types like `atom()`, `integer()`, etc.).
  """
  # TODO: Figure out how to use this operation from Elixir
  def map_get(:term, _key_descr), do: :badmap

  def map_get(%{} = descr, key_descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and map_only?(descr) do
          {optional?, type_selected} = map_get_static(descr, key_descr) |> pop_optional_static()

          cond do
            empty?(type_selected) -> {:ok_absent, atom([nil])}
            optional? -> {:ok, nil_or_type(type_selected)}
            true -> {:ok_present, type_selected}
          end
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          {optional_dynamic?, dynamic_type} =
            map_get_static(dynamic, key_descr) |> pop_optional_static()

          {optional_static?, static_type} =
            map_get_static(static, key_descr) |> pop_optional_static()

          type_selected = union(dynamic(dynamic_type), static_type)

          cond do
            empty?(type_selected) -> {:ok_absent, atom([nil])}
            optional_dynamic? or optional_static? -> {:ok, nil_or_type(type_selected)}
            true -> {:ok_present, type_selected}
          end
        else
          :badmap
        end
    end
  end

  # Returns the list of key types that are covered by the key_descr.
  # E.g., for `{atom([:ok]), term} or integer()` it returns `[:tuple, :integer]`.
  # We treat bitmap types as a separate key type.
  defp covered_key_types(:term), do: @domain_key_types

  defp covered_key_types(key_descr) do
    for {type_kind, type} <- key_descr, reduce: [] do
      acc ->
        cond do
          type_kind == :atom -> [{:atom, type} | acc]
          type_kind == :bitmap -> bitmap_to_domain_keys(type) ++ acc
          not empty?(%{type_kind => type}) -> [domain_key(type_kind) | acc]
          true -> acc
        end
    end
  end

  defp bitmap_to_domain_keys(bitmap) do
    [
      if((bitmap &&& @bit_binary) != 0, do: domain_key(:binary)),
      if((bitmap &&& @bit_empty_list) != 0, do: domain_key(:empty_list)),
      if((bitmap &&& @bit_integer) != 0, do: domain_key(:integer)),
      if((bitmap &&& @bit_float) != 0, do: domain_key(:float)),
      if((bitmap &&& @bit_pid) != 0, do: domain_key(:pid)),
      if((bitmap &&& @bit_port) != 0, do: domain_key(:port)),
      if((bitmap &&& @bit_reference) != 0, do: domain_key(:reference))
    ]
    |> Enum.reject(&is_nil/1)
  end

  defp nil_or_type(type), do: union(type, atom([nil]))

  defp unfold_domains(:closed), do: %{}

  defp unfold_domains(:open),
    do: Map.new(@domain_key_types, fn domain_key -> {domain_key, @term_or_optional} end)

  defp unfold_domains(domains = %{}), do: domains

  defp map_get_static(%{map: {{tag_or_domains, fields}, :bdd_top, :bdd_bot}}, key_descr) do
    # For each non-empty kind of type in the key_descr, we add the corresponding key domain in a union.
    domains = unfold_domains(tag_or_domains)

    key_descr
    |> covered_key_types()
    |> Enum.reduce(none(), fn
      # Note: we could stop if we reach term_or_optional()
      {:atom, atom_type}, acc ->
        map_get_atom(map_literal(domains, fields), atom_type) |> union(acc)

      key_type, acc ->
        Map.get(domains, key_type, not_set()) |> union(acc)
    end)
  end

  defp map_get_static(%{map: bdd}, key_descr) do
    key_descr
    |> covered_key_types()
    |> Enum.reduce(none(), fn
      {:atom, atom_type}, acc ->
        map_get_atom(bdd, atom_type) |> union(acc)

      domain_key, acc ->
        map_get_domain(bdd, domain_key) |> union(acc)
    end)
  end

  defp map_get_static(%{}, _key), do: not_set()
  defp map_get_static(:term, _key), do: term_or_optional()

  # Given a map dnf return the union of types for a given atom type. Handles two cases:
  # 1. A union of atoms (e.g., `{:union, atoms}`):
  #    - Iterates through each atom in the union.
  #    - Fetches the type for each atom and combines them into a union.
  #
  # 2. A negation of atoms (e.g., `{:negation, atoms}`):
  #    - Fetches all possible keys in the map's DNF.
  #    - Excludes the negated atoms from the considered keys.
  #    - Includes the domain of all atoms in the map's DNF.
  #
  # Example:
  #   Fetching a key of type `atom() and not (:a)` from a map of type
  #   `%{a: atom(), b: float(), atom() => pid()}`
  #   would return either `nil` or `float()` (key `:b`) or `pid()` (key `atom()`), but not `atom()` (key `:a`).
  defp map_get_atom(bdd, atom_type) do
    case atom_type do
      {:union, atoms} ->
        atoms
        |> :sets.to_list()
        |> Enum.reduce(none(), fn atom, acc ->
          {static_optional?, type} = map_fetch_static(%{map: bdd}, atom)

          if static_optional? do
            union(type, acc) |> nil_or_type() |> if_set()
          else
            union(type, acc)
          end
        end)

      {:negation, atoms} ->
        # 1) Fetch all the possible keys in the bdd
        # 2) Get them all, except the ones in neg_atoms
        possible_keys = map_fetch_all_key_names(bdd)
        considered_keys = :sets.subtract(possible_keys, atoms)

        considered_keys
        |> :sets.to_list()
        |> Enum.reduce(none(), fn atom, acc ->
          {static_optional?, type} = map_fetch_static(%{map: bdd}, atom)

          if static_optional? do
            union(type, acc) |> nil_or_type() |> if_set()
          else
            union(type, acc)
          end
        end)
        |> union(map_get_domain(bdd, domain_key(:atom)))
    end
  end

  # Fetch all present keys in a map dnf (including negated ones).
  defp map_fetch_all_key_names(bdd) do
    map_bdd_get(bdd)
    |> Enum.reduce(:sets.new(version: 2), fn {_tag, fields, negs}, acc ->
      keys = :sets.from_list(Map.keys(fields))

      # Add all the negative keys
      # Example: %{...} and not %{a: not_set()} makes key :a present in the map
      Enum.reduce(negs, keys, fn {_tag, neg_fields}, acc ->
        :sets.from_list(Map.keys(neg_fields)) |> :sets.union(acc)
      end)
      |> :sets.union(acc)
    end)
  end

  # Take a map dnf and return the union of types for the given key domain.
  defp map_get_domain(bdd, domain_key(_) = domain_key) do
    map_bdd_get(bdd)
    |> Enum.reduce(none(), fn
      {tag, _fields, []}, acc when is_atom(tag) ->
        map_key_tag_to_type(tag) |> union(acc)

      # Optimization: if there are no negatives and domains exists, return its value
      {%{^domain_key => value}, _fields, []}, acc ->
        value |> union(acc)

      # Optimization: if there are no negatives and the key does not exist, return the default type.
      {domains = %{}, _fields, []}, acc ->
        map_key_tag_to_type(domains) |> union(acc)

      {tag_or_domains, fields, negs}, acc ->
        {fst, snd} = map_pop_domain(tag_or_domains, fields, domain_key)

        case map_split_negative_domain(negs, domain_key) do
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
          {optional?, taken, result} =
            map_take_static(descr, key, initial)

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
            static_taken == nil and dynamic_taken == nil ->
              {nil, result}

            static_optional? or empty?(dynamic_taken) ->
              :badkey

            true ->
              {union(dynamic(dynamic_taken), static_taken), result}
          end
        else
          :badmap
        end
    end
  end

  # Takes a static map type and removes a key from it.
  # This allows the key to be put or deleted later on.
  defp map_take_static(%{map: {{tag, fields}, :bdd_top, :bdd_bot}} = descr, key, initial)
       when not is_map_key(fields, key) do
    case tag do
      :open -> {true, maybe_union(initial, fn -> term() end), descr}
      :closed -> {true, initial, descr}
    end
  end

  defp map_take_static(%{map: bdd}, key, initial) do
    {value, map} =
      map_bdd_get(bdd)
      |> Enum.reduce({initial, none()}, fn
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
  defp map_empty?(bdd), do: map_bdd_get(bdd) == []

  defp map_empty?(_, pos, []), do: Enum.any?(Map.to_list(pos), fn {_, v} -> empty?(v) end)
  defp map_empty?(_, _, [{:open, neg_fields} | _]) when neg_fields == %{}, do: true
  defp map_empty?(:open, fs, [{:closed, _} | negs]), do: map_empty?(:open, fs, negs)

  defp map_empty?(tag, fields, [{neg_tag, neg_fields} | negs]) do
    if map_check_domain_keys(tag, neg_tag) do
      atom_default = map_key_tag_to_type(tag)
      neg_atom_default = map_key_tag_to_type(neg_tag)

      (Enum.all?(Map.to_list(neg_fields), fn {neg_key, neg_type} ->
         cond do
           # Ignore keys present in both maps; will be handled below
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

           true ->
             diff = difference(atom_default, neg_type)
             empty?(diff) or map_empty?(tag, Map.put(fields, neg_key, diff), negs)
         end
       end) and
         Enum.all?(Map.to_list(fields), fn {key, type} ->
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
                   diff = difference(type, neg_atom_default)
                   empty?(diff) or map_empty?(tag, Map.put(fields, key, diff), negs)
               end
           end
         end)) or map_empty?(tag, fields, negs)
    else
      map_empty?(tag, fields, negs)
    end
  end

  # Verify the domain condition from equation (22) in paper ICFP'23 https://www.irif.fr/~gc/papers/icfp23.pdf
  # which is that every domain key type in the positive map is a subtype
  # of the corresponding domain key type in the negative map.
  defp map_check_domain_keys(:closed, _), do: true
  defp map_check_domain_keys(_, :open), do: true

  # An open map is a subtype iff the negative domains are all present as term_or_optional()
  defp map_check_domain_keys(:open, neg_domains) do
    map_size(neg_domains) == length(@domain_key_types) and
      Enum.all?(neg_domains, fn {domain_key(_), type} -> subtype?(term_or_optional(), type) end)
  end

  # A positive domains is smaller than a closed map iff all its keys are empty or optional
  defp map_check_domain_keys(pos_domains, :closed) do
    Enum.all?(pos_domains, fn {domain_key(_), type} -> empty_or_optional?(type) end)
  end

  # Component-wise comparison of domains
  defp map_check_domain_keys(pos_domains, neg_domains) do
    Enum.all?(pos_domains, fn {domain_key(_) = domain_key, type} ->
      subtype?(type, Map.get(neg_domains, domain_key, not_set()))
    end)
  end

  defp map_pop_key(tag, fields, key) do
    case :maps.take(key, fields) do
      {value, fields} -> {value, %{map: map_new(tag, fields)}}
      :error -> {map_key_tag_to_type(tag), %{map: map_new(tag, fields)}}
    end
  end

  # Pop a domain type, e.g. popping integers from %{integer() => binary()}
  # returns {if_set(binary()), %{integer() => if_set(binary()}}
  # If the domain is not present, use the tag to type as default.
  defp map_pop_domain(domains = %{}, fields, domain_key) do
    case :maps.take(domain_key, domains) do
      {value, domains} -> {if_set(value), %{map: map_new(domains, fields)}}
      :error -> {map_key_tag_to_type(domains), %{map: map_new(domains, fields)}}
    end
  end

  # Atom case
  defp map_pop_domain(tag, fields, _domain_key),
    do: {map_key_tag_to_type(tag), %{map: map_new(tag, fields)}}

  defp map_split_negative(negs, key) do
    Enum.reduce_while(negs, [], fn
      # A negation with an open map means the whole thing is empty.
      {:open, fields}, _acc when map_size(fields) == 0 -> {:halt, :empty}
      {tag, fields}, neg_acc -> {:cont, [map_pop_key(tag, fields, key) | neg_acc]}
    end)
  end

  defp map_split_negative_domain(negs, domain_key) do
    Enum.reduce_while(negs, [], fn
      {:open, fields}, _acc when map_size(fields) == 0 -> {:halt, :empty}
      {tag, fields}, neg_acc -> {:cont, [map_pop_domain(tag, fields, domain_key) | neg_acc]}
    end)
  end

  # Use heuristics to normalize a map dnf for pretty printing.
  defp map_normalize(bdd) do
    map_bdd_get(bdd)
    |> Enum.map(fn {tag, fields, negs} ->
      map_eliminate_while_negs_decrease(tag, fields, negs)
    end)
    |> map_fusion()
  end

  # Continue to eliminate negations while length of list of negs decreases
  defp map_eliminate_while_negs_decrease(tag, fields, []), do: {tag, fields, []}

  defp map_eliminate_while_negs_decrease(tag, fields, negs) do
    n = length(negs)
    {fields, negs} = maybe_eliminate_map_negations(tag, fields, negs)

    if length(negs) < n do
      map_eliminate_while_negs_decrease(tag, fields, negs)
    else
      {tag, fields, negs}
    end
  end

  defp maybe_eliminate_map_negations(tag, fields, negs) do
    Enum.reduce(negs, {fields, []}, fn neg = {neg_tag, neg_fields}, {acc_fields, acc_negs} ->
      # If the intersection with the negative is empty, we can remove the negative.
      empty_intersection? =
        try do
          _ = map_literal_intersection(tag, acc_fields, neg_tag, neg_fields)
          false
        catch
          :empty -> true
        end

      if empty_intersection? do
        {acc_fields, acc_negs}
      else
        case map_all_but_one(tag, acc_fields, neg_tag, neg_fields) do
          {:one, diff_key} ->
            {Map.update!(acc_fields, diff_key, &difference(&1, neg_fields[diff_key])), acc_negs}

          _ ->
            {acc_fields, [neg | acc_negs]}
        end
      end
    end)
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
    case maybe_optimize_map_union(map, candidate) do
      nil -> [candidate | map_fuse_with_first_fusible(map, rest)]
      # we found a fusible candidate, we're done
      fused -> [fused | rest]
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

  defp map_to_quoted(bdd, opts) do
    map_normalize(bdd)
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

  def map_literal_to_quoted({domains = %{}, fields}, _opts)
      when map_size(domains) == 0 and map_size(fields) == 0 do
    {:empty_map, [], []}
  end

  def map_literal_to_quoted({:open, %{__struct__: @not_atom_or_optional} = fields}, _opts)
      when map_size(fields) == 1 do
    {:non_struct_map, [], []}
  end

  def map_literal_to_quoted({domains = %{}, fields}, opts) do
    domain_fields =
      for {domain_key(domain_type), value_type} <- domains do
        {{domain_type, [], []}, map_value_to_quoted(value_type, opts)}
      end

    regular_fields_quoted = map_fields_to_quoted(:closed, Enum.sort(fields), opts)
    {:%{}, [], domain_fields ++ regular_fields_quoted}
  end

  def map_literal_to_quoted({tag, fields}, opts) do
    case tag do
      :closed ->
        with %{__struct__: struct_descr} <- fields,
             {_, [struct]} <- atom_fetch(struct_descr),
             info when is_list(info) <- maybe_struct(struct),
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
          _ ->
            {:%{}, [], map_fields_to_quoted(tag, Enum.sort(fields), opts)}
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

      {key, map_value_to_quoted(type, opts)}
    end
  end

  defp map_value_to_quoted(type, opts) do
    {optional?, type} = pop_optional_static(type)

    cond do
      not optional? -> to_quoted(type, opts)
      empty?(type) -> {:not_set, [], []}
      true -> {:if_set, [], [to_quoted(type, opts)]}
    end
  end

  ## Tuple

  # Represents tuple types as a BDD with nodes of the following forms:
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
      :empty -> %{}
      {fields, true} -> %{dynamic: %{tuple: tuple_new(tag, Enum.reverse(fields))}}
      {_, false} -> %{tuple: tuple_new(tag, fields)}
    end
  end

  defp tuple_descr([:term | rest], acc, dynamic?) do
    tuple_descr(rest, [:term | acc], dynamic?)
  end

  defp tuple_descr([value | rest], acc, dynamic?) do
    # Check if the static part is empty
    static_empty? =
      case value do
        # Has dynamic component, check static separately
        %{dynamic: _} -> false
        _ -> empty?(value)
      end

    if static_empty? do
      :empty
    else
      case :maps.take(:dynamic, value) do
        :error ->
          tuple_descr(rest, [value | acc], dynamic?)

        {dynamic, _static} ->
          # Check if dynamic component is empty
          if empty?(dynamic) do
            :empty
          else
            tuple_descr(rest, [dynamic | acc], true)
          end
      end
    end
  end

  defp tuple_descr([], acc, dynamic?) do
    {acc, dynamic?}
  end

  defp tuple_new(tag, elements), do: tuple_literal(tag, elements)

  defp tuple_intersection(tuple_literal(tag1, elements1), tuple_literal(tag2, elements2)) do
    case tuple_literal_intersection(tag1, elements1, tag2, elements2) do
      {tag, elements} -> tuple_literal(tag, elements)
      :empty -> :bdd_bot
    end
  end

  defp tuple_intersection(bdd1, bdd2), do: bdd_intersection(bdd1, bdd2)

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

  defp tuple_difference(bdd1, bdd2), do: bdd_difference(bdd1, bdd2)

  defp tuple_empty?(bdd), do: tuple_bdd_to_dnf(bdd) == []

  # No negations, so not empty unless there's an empty type
  # Note: since the extraction from the BDD is done in a way that guarantees that
  # the elements are non-empty, we can avoid checking for empty types there.
  # Otherwise, tuple_empty?(_, elements, []) would be Enum.any?(elements, &empty?/1)
  defp tuple_empty?(_, _, []), do: false
  # Open empty negation makes it empty
  defp tuple_empty?(_, _, [{:open, []} | _]), do: true
  # Open positive can't be emptied by a single closed negative
  defp tuple_empty?(:open, _pos, [{:closed, _}]), do: false

  defp tuple_empty?(tag, elements, [{neg_tag, neg_elements} | negs]) do
    n = length(elements)
    m = length(neg_elements)

    # Scenarios where the difference is guaranteed to be empty:
    # 1. When removing larger tuples from a fixed-size positive tuple
    # 2. When removing smaller tuples from larger tuples
    if (tag == :closed and n < m) or (neg_tag == :closed and n > m) do
      tuple_empty?(tag, elements, negs)
    else
      case element_intersection(elements, neg_elements) do
        :empty ->
          tuple_empty?(tag, elements, negs)

        _ ->
          tuple_elements_empty?([], tag, elements, neg_elements, negs) and
            tuple_compatibility(n, m, tag, elements, neg_tag, negs)
      end
    end
  end

  # Recursively check elements for emptiness
  defp tuple_elements_empty?(_, _, _, [], _), do: true

  defp tuple_elements_empty?(acc_meet, tag, elements, [neg_type | neg_elements], negs) do
    # Handles the case where {tag, elements} is an open tuple, like {:open, []}
    {ty, elements} = List.pop_at(elements, 0, term())
    diff = difference(ty, neg_type)
    meet = intersection(ty, neg_type)

    # In this case, there is no intersection between the positive and this negative.
    # So we should just "go next"
    (empty?(diff) or tuple_empty?(tag, Enum.reverse(acc_meet, [diff | elements]), negs)) and
      tuple_elements_empty?([meet | acc_meet], tag, elements, neg_elements, negs)
  end

  # Function that, given two tuples {tag1, elements1} and {tag2, elements2}, computes the
  # intersection of all their elements (with default term() if one is open).
  # If any intersection is empty, it return :empty. Else, it should return the list of them.
  defp element_intersection(elements1, elements2) do
    elements1 =
      if length(elements1) < length(elements2),
        do: tuple_fill(elements1, length(elements2)),
        else: elements1

    elements2 =
      if length(elements1) > length(elements2),
        do: tuple_fill(elements2, length(elements1)),
        else: elements2

    Enum.reduce_while(Enum.zip(elements1, elements2), [], fn {type1, type2}, acc ->
      case intersection(type1, type2) do
        :empty -> {:halt, :empty}
        meet -> {:cont, [meet | acc]}
      end
    end)
    |> case do
      :empty -> :empty
      list -> Enum.reverse(list)
    end
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

  defp tuple_eliminate_negations(tag, elements, negs) do
    Enum.reduce(negs, [{tag, elements}], fn {neg_tag, neg_elements}, acc ->
      Enum.flat_map(acc, fn {tag, elements} ->
        tuple_eliminate_single_negation(tag, elements, {neg_tag, neg_elements})
      end)
    end)
  end

  defp empty_element_intersection?(elements1, elements2) do
    Enum.any?(Enum.zip(elements1, elements2), fn {type1, type2} ->
      empty?(intersection(type1, type2))
    end)
  end

  # Important: this generates DISJOINT tuples.
  defp tuple_eliminate_single_negation(tag, elements, {neg_tag, neg_elements}) do
    n = length(elements)
    m = length(neg_elements)

    # Scenarios where the difference is guaranteed to be empty:
    # 1. When removing larger tuples from a fixed-size positive tuple
    # 2. When removing smaller tuples from larger tuples
    # 3. When there is no intersection between the elements of the two tuples
    if (tag == :closed and n < m) or (neg_tag == :closed and n > m) or
         empty_element_intersection?(elements, neg_elements) do
      [{tag, elements}]
    else
      tuple_dnf_union(
        tuple_elim_content([], tag, elements, neg_elements),
        tuple_elim_size(n, m, tag, elements, neg_tag)
      )
    end
  end

  # Build a disjoint disjunction for {t1,...,tn} /\ not {u1,...,un}
  # by splitting on the *earliest* index where they differ.
  #
  # Invariant:
  # - `acc` holds the prefix we've already forced to *match* the negative tuple,
  #   stored in reverse order (so we can `Enum.reverse/2` at the end when we emit).
  # - `elements` are the yet-unprocessed positive tuple element types.
  # - `neg_elements` are the corresponding negative tuple element types.
  #
  # For position i:
  #   - One branch mismatches here:  (ti \ ui), with earlier positions fixed to intersection(tj, uj).
  #   - The other recursive path forces match here: intersection(ti, ui) and continues to i+1.
  #
  # This yields disjoint branches like:
  #   {t1 /\ not u1, t2, t3, ...} OR {t1 /\ u1, t2 /\ not u2, t3, ...} OR {t1/\u1, t2/\u2, t3/\ not u3, ...} ...
  defp tuple_elim_content(acc, tag, elements, [neg_type | neg_elements]) do
    # If `elements` is shorter, default to top type `term()` (as in your example).
    {ty, rest} = List.pop_at(elements, 0, term())

    # ti \ ui  (i.e., ti and not ui)
    diff = difference(ty, neg_type)
    # ti /\ ui
    meet = intersection(ty, neg_type)

    # Branch where the earliest difference is *here*.
    # Earlier positions are the accumulated matches in `acc`;
    # later positions remain unconstrained (`rest` as-is).
    here_branch =
      if empty?(diff) do
        []
      else
        [{tag, Enum.reverse(acc, [diff | rest])}]
      end

    # Branches where we force equality here and defer the first difference to later positions.
    later_branches =
      if empty?(meet) do
        []
      else
        tuple_elim_content([meet | acc], tag, rest, neg_elements)
      end

    here_branch ++ later_branches
  end

  # No more negative elements to process: there is no “all-equal” branch to add,
  # because we’re constructing {t} ant not {u}, which must differ somewhere.
  defp tuple_elim_content(_acc, _tag, _elements, []) do
    []
  end

  # Eliminates negations according to size
  # Example: {integer(), ...} and not {term(), term(), ...} contains {integer()}
  # The tuples to consider are all those of size n to m - 1, and if the negative tuple is
  # closed, we also need to consider tuples of size greater than m + 1.
  defp tuple_elim_size(_, _, :closed, _, _), do: []

  defp tuple_elim_size(n, m, :open, elements, neg_tag) do
    n..(m - 1)//1
    |> Enum.reduce([], fn i, acc ->
      [{:closed, tuple_fill(elements, i)} | acc]
    end)
    |> Kernel.++(
      if neg_tag == :open do
        []
      else
        [{:open, tuple_fill(elements, m + 1)}]
      end
    )
  end

  defp tuple_dnf_union(dnf1, dnf2) do
    # Union of tuple DNFs is just concatenation, but we rely on some optimization strategies to
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

  defp tuple_union(
         tuple_literal(tag1, elements1) = tuple1,
         tuple_literal(tag2, elements2) = tuple2
       ) do
    case maybe_optimize_tuple_union({tag1, elements1}, {tag2, elements2}) do
      {tag, elements} -> tuple_literal(tag, elements)
      nil -> bdd_union(tuple1, tuple2)
    end
  end

  defp tuple_union(bdd1, bdd2), do: bdd_union(bdd1, bdd2)

  defp maybe_optimize_tuple_union({tag1, pos1} = tuple1, {tag2, pos2} = tuple2) do
    case tuple_union_optimization_strategy(tag1, pos1, tag2, pos2) do
      :all_equal ->
        tuple1

      {:one_index_difference, index, v1, v2} ->
        new_pos = List.replace_at(pos1, index, union(v1, v2))
        {tag1, new_pos}

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

  defp tuple_to_quoted(bdd, opts) do
    tuple_bdd_to_dnf_no_negations(bdd)
    |> tuple_fusion()
    |> Enum.map(&tuple_literal_to_quoted(&1, opts))
  end

  # Transforms a bdd into a dnf (union of tuples with negations)
  def tuple_bdd_to_dnf(bdd),
    do: tuple_bdd_get([], {:open, []}, [], bdd, &{&1, &2, &3}, &[&1 | &2])

  # Transforms a bdd into a positive normal form (union of tuples with no negations)
  # Note: it is important to compose the results with tuple_dnf_union/2 to avoid duplicates
  defp tuple_bdd_to_dnf_no_negations(bdd),
    do: tuple_bdd_get([], {:open, []}, [], bdd, &tuple_eliminate_negations/3, &tuple_dnf_union/2)

  defp tuple_bdd_get(acc, {tag, elements} = tuple, negs, bdd, transform, compose) do
    case bdd do
      :bdd_bot ->
        acc

      :bdd_top ->
        if tuple_empty?(tag, elements, negs) do
          acc
        else
          compose.(transform.(tag, elements, negs), acc)
        end

      {{next_tag, next_elements} = next_tuple, left, right} ->
        acc =
          case tuple_literal_intersection(tag, elements, next_tag, next_elements) do
            :empty ->
              acc

            new_tuple ->
              tuple_bdd_get(acc, new_tuple, negs, left, transform, compose)
          end

        tuple_bdd_get(acc, tuple, [next_tuple | negs], right, transform, compose)
    end
  end

  # Given a union of tuples, fuses the tuple unions when possible,
  # e.g. {integer(), atom()} or {float(), atom()} into {number(), atom()}
  # The negations of two fused tuples are just concatenated.
  defp tuple_fusion(dnf_no_negations) do
    # Steps:
    # 1. Consider tuples without negations apart from those with
    # 2. Group tuples by size and tag
    # 3. Try fusions for each group until no fusion is found
    # 4. Merge the groups back into a dnf
    dnf_no_negations
    |> Enum.group_by(fn {tag, elems} -> {tag, length(elems)} end)
    |> Enum.flat_map(fn {_, tuples} -> tuple_non_negated_fuse(tuples) end)
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
        if descr_key?(descr, :tuple) and non_empty_tuple_only?(descr) do
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

  defp non_empty_tuple_only?(descr) do
    case :maps.take(:tuple, descr) do
      :error -> false
      {tuple_bdd, rest} -> empty?(rest) and not tuple_empty?(tuple_bdd)
    end
  end

  defp tuple_fetch_static(descr, index) when is_integer(index) do
    case descr do
      :term -> {true, term()}
      %{tuple: tuple} -> tuple_get(tuple, index) |> pop_optional_static()
      %{} -> {false, none()}
    end
  end

  defp tuple_get(bdd, index) do
    tuple_bdd_to_dnf_no_negations(bdd)
    |> Enum.reduce(none(), fn
      {tag, elements}, acc -> Enum.at(elements, index, tuple_tag_to_type(tag)) |> union(acc)
    end)
  end

  @doc """
  Returns all of the values that are part of a tuple.
  """
  def tuple_values(descr) when descr == %{}, do: :badtuple

  def tuple_values(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if tuple_only?(descr) do
          process_tuples_values(Map.get(descr, :tuple, :bdd_bot))
        else
          :badtuple
        end

      {dynamic, static} ->
        if tuple_only?(static) and descr_key?(dynamic, :tuple) do
          dynamic(process_tuples_values(Map.get(dynamic, :tuple, :bdd_bot)))
          |> union(process_tuples_values(Map.get(static, :tuple, :bdd_bot)))
        else
          :badtuple
        end
    end
  end

  defp process_tuples_values(bdd) do
    tuple_bdd_to_dnf_no_negations(bdd)
    |> Enum.reduce(none(), fn {tag, elements}, acc ->
      cond do
        Enum.any?(elements, &empty?/1) -> none()
        tag == :open -> term()
        tag == :closed -> Enum.reduce(elements, none(), &union/2)
      end
      |> union(acc)
    end)
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
  defp tuple_delete_static(%{tuple: bdd}, index) do
    %{tuple: bdd_map(bdd, fn {tag, elements} -> {tag, List.delete_at(elements, index)} end)}
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
    Map.update!(descr, :tuple, fn bdd ->
      bdd_map(bdd, fn {tag, elements} ->
        {tag, List.insert_at(elements, index, type)}
      end)
    end)
  end

  defp tuple_of_size_at_least(n) when is_integer(n) and n >= 0 do
    %{tuple: tuple_new(:open, List.duplicate(term(), n))}
  end

  defp tuple_of_size_at_least_static?(descr, index) do
    case descr do
      %{tuple: bdd} ->
        tuple_bdd_to_dnf_no_negations(bdd)
        |> Enum.all?(fn {_, elements} -> length(elements) >= index end)

      %{} ->
        true
    end
  end

  ## BDD helpers

  # Leaf cases
  defp bdd_intersection(_, :bdd_bot), do: :bdd_bot
  defp bdd_intersection(:bdd_bot, _), do: :bdd_bot
  defp bdd_intersection(:bdd_top, other), do: other
  defp bdd_intersection(other, :bdd_top), do: other

  # Internal-node cases
  # Keeping the invariant that literals are ordered ensures nodes are not duplicated down
  defp bdd_intersection({lit, l1, r1}, {lit, l2, r2}),
    do: {lit, bdd_intersection(l1, l2), bdd_intersection(r1, r2)}

  defp bdd_intersection({lit1, l1, r1}, {lit2, _, _} = bdd2) when lit1 < lit2,
    do: {lit1, bdd_intersection(l1, bdd2), bdd_intersection(r1, bdd2)}

  defp bdd_intersection({lit1, _, _} = bdd1, {lit2, l2, r2}) when lit1 > lit2,
    do: {lit2, bdd_intersection(bdd1, l2), bdd_intersection(bdd1, r2)}

  defp bdd_difference(:bdd_bot, _), do: :bdd_bot
  defp bdd_difference(other, :bdd_bot), do: other
  defp bdd_difference(:bdd_top, other), do: bdd_negation(other)
  defp bdd_difference(_, :bdd_top), do: :bdd_bot

  defp bdd_difference({lit, l1, r1}, {lit, l2, r2}),
    do: {lit, bdd_difference(l1, l2), bdd_difference(r1, r2)}

  defp bdd_difference({lit1, l1, r1}, {lit2, _, _} = bdd2) when lit1 < lit2,
    do: {lit1, bdd_difference(l1, bdd2), bdd_difference(r1, bdd2)}

  defp bdd_difference({lit1, _, _} = bdd1, {lit2, l2, r2}) when lit1 > lit2,
    do: {lit2, bdd_difference(bdd1, l2), bdd_difference(bdd1, r2)}

  defp bdd_negation(:bdd_bot), do: :bdd_top
  defp bdd_negation(:bdd_top), do: :bdd_bot
  defp bdd_negation({lit, l, r}), do: {lit, bdd_negation(l), bdd_negation(r)}

  defp bdd_union(:bdd_top, _), do: :bdd_top
  defp bdd_union(_, :bdd_top), do: :bdd_top
  defp bdd_union(:bdd_bot, other), do: other
  defp bdd_union(other, :bdd_bot), do: other
  defp bdd_union({map, l1, r1}, {map, l2, r2}), do: {map, bdd_union(l1, l2), bdd_union(r1, r2)}

  # Maintaining the invariant that literals are ordered ensures they are not duplicated down the tree
  defp bdd_union({lit1, l1, r1}, {lit2, _, _} = bdd2) when lit1 < lit2,
    do: {lit1, bdd_union(l1, bdd2), bdd_union(r1, bdd2)}

  defp bdd_union({lit1, _, _} = bdd1, {lit2, l2, r2}) when lit1 > lit2,
    do: {lit2, bdd_union(bdd1, l2), bdd_union(bdd1, r2)}

  defp bdd_map(bdd, fun) do
    case bdd do
      :bdd_bot -> :bdd_bot
      :bdd_top -> :bdd_top
      {literal, left, right} -> {fun.(literal), bdd_map(left, fun), bdd_map(right, fun)}
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

  # Erlang maps:merge_with/3 has to preserve the order in combiner.
  # We don't care about the order, so we have a faster implementation.
  defp symmetrical_merge(left, right, fun) do
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

  # Perform a symmetrical merge with default values
  defp symmetrical_merge(left, left_default, right, right_default, fun) do
    iterator = :maps.next(:maps.iterator(left))
    iterator_merge_left(iterator, left_default, right, right_default, %{}, fun)
  end

  defp iterator_merge_left({key, v1, iterator}, v1_default, map, v2_default, acc, fun) do
    value =
      case map do
        %{^key => v2} -> fun.(key, v1, v2)
        %{} -> fun.(key, v1, v2_default)
      end

    acc = Map.put(acc, key, value)
    iterator_merge_left(:maps.next(iterator), v1_default, map, v2_default, acc, fun)
  end

  defp iterator_merge_left(:none, v1_default, map, _v2_default, acc, fun) do
    iterator_merge_right(:maps.next(:maps.iterator(map)), v1_default, acc, fun)
  end

  defp iterator_merge_right({key, v2, iterator}, v1_default, acc, fun) do
    acc =
      case acc do
        %{^key => _} -> acc
        %{} -> Map.put(acc, key, fun.(key, v1_default, v2))
      end

    iterator_merge_right(:maps.next(iterator), v1_default, acc, fun)
  end

  defp iterator_merge_right(:none, _v1_default, acc, _fun) do
    acc
  end

  # Erlang maps:intersect_with/3 has to preserve the order in combiner.
  # We don't care about the order, so we have a faster implementation.
  defp symmetrical_intersection(left, right, fun) do
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
