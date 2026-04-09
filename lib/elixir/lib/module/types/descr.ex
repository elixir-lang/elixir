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
  # * DNF - disjunctive normal form which is a pair of unions and negations
  # * BDD - binary decision diagram which is a set-theoretic representation of types as a tree

  import Bitwise

  @bit_binary 0b1
  @bit_bitstring_no_binary 0b10
  @bit_empty_list 0b100
  @bit_integer 0b1000
  @bit_float 0b10000
  @bit_pid 0b100000
  @bit_port 0b1000000
  @bit_reference 0b10000000
  @bit_top 0b11111111

  # We use two bits to represent bitstrings and binaries,
  # which must be looked at together
  # bitstring = 0b11
  # bitstring and not binary = 0b10
  # binary = 0b01
  # none = 0b00
  @bit_bitstring @bit_binary ||| @bit_bitstring_no_binary
  @bit_number @bit_integer ||| @bit_float

  defmacro bdd_leaf(arg1, arg2), do: {arg1, arg2}

  # Map fields and domains are stored as orddicts (sorted key-value lists).
  @fields_new []
  defguardp is_fields_empty(fields) when fields == []
  defguardp fields_size(fields) when length(fields)

  @domain_key_types :lists.sort(
                      [:binary, :integer, :float, :pid, :port, :reference] ++
                        [:fun, :atom, :tuple, :map, :list]
                    )

  # Remark: those are explicit BDD constructors. The functional constructors are `bdd_new/1` and `bdd_new/3`.
  @fun_top {:negation, %{}}
  @atom_top {:negation, :sets.new(version: 2)}
  @map_top {:open, @fields_new}
  @non_empty_list_top {:term, :term}
  @tuple_top {:open, []}
  @map_empty {:closed, @fields_new}

  # The top BDD for each arity.
  @fun_bdd_top :bdd_top

  @none %{}
  @term %{
    bitmap: @bit_top,
    atom: @atom_top,
    tuple: @tuple_top,
    map: @map_top,
    list: @non_empty_list_top,
    fun: @fun_top
  }
  @list_top %{bitmap: @bit_empty_list, list: @non_empty_list_top}
  @empty_list %{bitmap: @bit_empty_list}
  @not_non_empty_list Map.delete(@term, :list)

  @not_set %{optional: 1}
  @term_or_optional Map.put(@term, :optional, 1)
  @term_or_dynamic_optional Map.put(@term, :dynamic, %{optional: 1})
  @not_atom_or_optional Map.delete(@term_or_optional, :atom)

  @empty_intersection [0, :bdd_bot]
  @empty_difference [0, :bdd_bot]

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
  def bitstring(), do: %{bitmap: @bit_bitstring}
  def bitstring_no_binary(), do: %{bitmap: @bit_bitstring_no_binary}
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

  @doc """
  Gets the upper bound of a gradual type.

  This is the same as removing the gradual type.
  """
  def upper_bound(%{dynamic: dynamic}), do: dynamic
  def upper_bound(static), do: static

  @doc """
  Gets the lower bound of a gradual type.

  This is the same as getting the static part.
  Note this is not generally safe and changes the representation of the type.
  """
  def lower_bound(:term), do: :term
  def lower_bound(type), do: Map.delete(type, :dynamic)

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
    %{fun: {:union, %{arity => @fun_bdd_top}}}
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
        return = upper_bound(return)
        pivot_overlapping_clause(domain, return, return, acc)
      end)

    funs =
      for {domain, _return, union} <- domain_clauses,
          args <- domain_to_args(domain),
          do: fun(args, dynamic(union))

    Enum.reduce(funs, &intersection/2)
  end

  # If you have a function with multiple clauses, they may overlap,
  # and we need to find the correct return type. This can be done
  # in two different algorithms: a precise and a fast one.
  # Consider the inferred clauses:
  #
  #     (integer() or atom() -> :foo) and (integer() or float() -> :bar)
  #
  # The precise algorithm works by finding the intersections between
  # domains, assigning it the union, and then computing the leftovers
  # on each side. The trouble of this approach is that it builds
  # differences, which can be expensive, and can lead to several new
  # clauses. For example, the clause above would emit:
  #
  #     (integer() -> :foo or :bar) and (atom() -> :foo) and (float() -> :bar)
  #
  # Due to performance constraints, we chose the fast approach, which
  # leads to broad return types, but this is acceptable because we mark
  # all return types as dynamic anyway. Therefore we infer the type:
  #
  #     (integer() or atom() -> :foo or :bar) and (integer() or float() -> :foo or :bar)
  #
  defp pivot_overlapping_clause(domain, return, union, [{acc_domain, acc_return, acc_union} | acc]) do
    if disjoint?(domain, acc_domain) do
      [
        {acc_domain, acc_return, acc_union}
        | pivot_overlapping_clause(domain, return, union, acc)
      ]
    else
      [
        {acc_domain, acc_return, union(return, acc_union)}
        | pivot_overlapping_clause(domain, return, union(acc_return, union), acc)
      ]
    end
  end

  defp pivot_overlapping_clause(domain, return, union, []) do
    [{domain, return, union}]
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
  Converts a list of arguments into a static domain.

  If a dynamic type is given, we get its upper bound.
  """
  def args_to_static_domain(types) when is_list(types) do
    %{tuple: tuple_new(:closed, Enum.map(types, &upper_bound/1))}
  end

  @doc """
  Converts the domain to a list of arguments.

  The domain is expected to be closed tuples. They may have complex negations
  which are then simplified to a union of positive tuple literals only.

  * For static tuple types: eliminates all negations from the DNF representation.

  * For gradual tuple types: processes both dynamic and static components separately,
    then combines them.

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

  @doc """
  Converts the domain to a flatten list of arguments.
  """
  def domain_to_flat_args(domain, arity_or_args) do
    case domain_to_args(domain) do
      [] when is_integer(arity_or_args) -> List.duplicate(none(), arity_or_args)
      [] when is_list(arity_or_args) -> Enum.map(arity_or_args, fn _ -> none() end)
      args -> Enum.zip_with(args, fn types -> Enum.reduce(types, &union/2) end)
    end
  end

  defp unwrap_domain_tuple(%{tuple: bdd} = descr, transform) when map_size(descr) == 1 do
    tuple_bdd_to_dnf_no_negations(bdd) |> Enum.map(transform)
  end

  defp unwrap_domain_tuple(descr, _transform) when descr == %{}, do: []

  ## Optional

  # `not_set()` is a special base type that represents a not_set field in a map.
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
      %{dynamic: :term} -> %{dynamic: term_or_optional()}
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

  defp remove_optional_static(%{} = descr), do: Map.delete(descr, :optional)
  defp remove_optional_static(descr), do: descr

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

  @compile {:inline, pop_dynamic: 1}
  defp pop_dynamic(:term), do: {:term, :term}
  defp pop_dynamic(descr), do: Map.pop(descr, :dynamic, descr)

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
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(unfold(right), :dynamic, right)
        union_static(left, right_with_dynamic)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(unfold(left), :dynamic, left)
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
    is_gradual_left = gradual?(left)
    is_gradual_right = gradual?(right)

    cond do
      is_gradual_left and not is_gradual_right ->
        right_with_dynamic = Map.put(unfold(right), :dynamic, right)
        intersection_static(left, right_with_dynamic)

      is_gradual_right and not is_gradual_left ->
        left_with_dynamic = Map.put(unfold(left), :dynamic, left)
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
  defp intersection(:fun, v1, v2), do: fun_intersection(v1, v2)

  defp intersection(:dynamic, v1, v2) do
    descr = dynamic_intersection(v1, v2)
    if descr == @none, do: 0, else: descr
  end

  @doc """
  Computes the difference between two types.
  """
  def difference(left, :term), do: keep_optional(left)
  def difference(left, none) when none == @none, do: left

  def difference(left, right) do
    if gradual?(left) or gradual?(right) do
      {left_dynamic, left_static} = pop_dynamic(left)
      {right_dynamic, right_static} = pop_dynamic(right)
      dynamic_part = difference_static(left_dynamic, right_static)

      Map.put(difference_static(left_static, right_dynamic), :dynamic, dynamic_part)
    else
      difference_static(left, right)
    end
  end

  # For static types, the difference is component-wise
  defp difference_static(left, descr) when descr == %{}, do: left
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
  defp difference(:fun, v1, v2), do: fun_difference(v1, v2)

  @doc """
  Compute the negation of a type.
  """
  def negation(:term), do: none()
  def negation(%{} = descr), do: difference(term(), descr)

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
  Converts all floats or integers into numbers.
  """
  def numberize(:term), do: :term
  def numberize(descr), do: numberize_each(descr, [:bitmap, :tuple, :map, :list, :dynamic])

  defp numberize_each(descr, [key | keys]) do
    case descr do
      %{^key => val} -> %{descr | key => numberize(key, val)}
      %{} -> descr
    end
    |> numberize_each(keys)
  end

  defp numberize_each(descr, []) do
    descr
  end

  defp numberize(:dynamic, descr), do: numberize(descr)
  defp numberize(:bitmap, bitmap) when (bitmap &&& @bit_number) != 0, do: bitmap ||| @bit_number
  defp numberize(:bitmap, bitmap), do: bitmap

  defp numberize(:map, bdd) do
    bdd_map(bdd, fn {tag, fields} ->
      {tag, fields_map(fn _key, value -> numberize(value) end, fields)}
    end)
  end

  defp numberize(:tuple, bdd) do
    bdd_map(bdd, fn {tag, fields} -> {tag, Enum.map(fields, &numberize/1)} end)
  end

  defp numberize(:list, bdd) do
    bdd_map(bdd, fn {head, tail} -> {numberize(head), numberize(tail)} end)
  end

  @doc """
  Returns if the type is a singleton.
  """
  def singleton?(:term), do: false
  def singleton?(descr), do: static_singleton?(Map.get(descr, :dynamic, descr))

  defp static_singleton?(:term), do: false
  defp static_singleton?(%{optional: _}), do: false
  defp static_singleton?(%{list: _}), do: false
  defp static_singleton?(%{fun: _}), do: false
  defp static_singleton?(descr), do: each_singleton?(descr, [:atom, :bitmap, :map, :tuple], false)

  defp each_singleton?(descr, [key | keys], acc) do
    case descr do
      %{^key => value} ->
        case each_singleton?(key, value) do
          true when acc == true -> false
          true -> each_singleton?(descr, keys, true)
          false -> false
          :empty -> each_singleton?(descr, keys, acc)
        end

      %{} ->
        each_singleton?(descr, keys, acc)
    end
  end

  defp each_singleton?(_descr, [], acc), do: acc

  # Implement for each type
  defp each_singleton?(:bitmap, bitmap), do: bitmap == @bit_empty_list

  defp each_singleton?(:atom, atoms), do: match?({:union, set} when map_size(set) == 1, atoms)

  defp each_singleton?(:tuple, bdd) do
    case tuple_bdd_to_dnf_no_negations(bdd) do
      [] -> :empty
      [{:closed, entries}] -> Enum.all?(entries, &static_singleton?/1)
      _ -> false
    end
  end

  defp each_singleton?(:map, bdd) do
    case map_bdd_to_dnf_remove_empty(bdd) do
      [] ->
        :empty

      [{:closed, fields, _negs}] ->
        Enum.all?(fields_to_list(fields), fn {_, v} -> static_singleton?(v) end)

      _ ->
        false
    end
  end

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
          cond do
            # Computing term_type?(difference(dynamic, static)) can be
            # expensive, so we check for term type before hand and check
            # for :term exclusively in dynamic_to_quoted/2.
            term_type?(dynamic) ->
              {:term, static, []}

            # We need to check for quoted here before we compute the difference below
            quoted = maybe_negated_term_type_to_quoted(static, opts) ->
              {dynamic, %{}, [quoted]}

            # Denormalize functions (only unions) before we do the difference
            true ->
              {static, dynamic, extra} = fun_denormalize(static, dynamic, opts)
              {difference(dynamic, static), static, extra}
          end
      end

    {static, extra} =
      if quoted = maybe_negated_term_type_to_quoted(static, opts) do
        {%{}, [quoted | extra]}
      else
        {static, extra}
      end

    # Dynamic always come first for visibility
    unions =
      to_quoted(:dynamic, dynamic, opts) ++ static_non_term_type_to_quoted(static, extra, opts)

    case unions do
      [] -> {:none, [], []}
      unions -> Enum.reduce(unions, &{:or, [], [&2, &1]})
    end
  end

  defp static_non_term_type_to_quoted(static, extra, opts) do
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

    Enum.sort(extra ++ Enum.flat_map(static, fn {key, value} -> to_quoted(key, value, opts) end))
  end

  defp to_quoted(:atom, val, _opts), do: atom_to_quoted(val)
  defp to_quoted(:bitmap, val, _opts), do: bitmap_to_quoted(val)
  defp to_quoted(:dynamic, descr, opts), do: dynamic_to_quoted(descr, opts)
  defp to_quoted(:map, bdd, opts), do: map_to_quoted(bdd, opts)
  defp to_quoted(:list, bdd, opts), do: list_to_quoted(bdd, false, opts)
  defp to_quoted(:tuple, bdd, opts), do: tuple_to_quoted(bdd, opts)
  defp to_quoted(:fun, bdd, opts), do: fun_to_quoted(bdd, opts)

  defp maybe_negated_term_type_to_quoted(static, opts) do
    if print_as_negated_type?(static) do
      static
      |> negation()
      |> unfold()
      |> static_non_term_type_to_quoted([], opts)
      |> case do
        [] ->
          nil

        [head | tail] ->
          Enum.reduce(tail, {:not, [], [head]}, &{:and, [], [&2, {:not, [], [&1]}]})
      end
    end
  end

  # Unions would be printed as negations within negations
  defp print_as_negated_type?(%{atom: {:union, _}}), do: false
  defp print_as_negated_type?(%{fun: {:union, _}}), do: false

  defp print_as_negated_type?(descr) do
    # In here we compute the score of negations.
    # If we have more than 6, we print it as a negated type.
    result =
      Enum.sum_by(Map.to_list(descr), fn
        {:tuple, bdd} -> print_as_negated_bdd(bdd, @tuple_top)
        {:map, bdd} -> print_as_negated_bdd(bdd, @map_top)
        {:list, bdd} -> print_as_negated_bdd(bdd, @non_empty_list_top)
        {:bitmap, bitmap} -> Integer.popcount(bitmap)
        {:atom, {:union, _}} -> -100
        {:fun, {:union, _}} -> -100
        {_, _} -> 1
      end)

    result > 8
  end

  # A bdd leaf can be trivially printed in negated format
  # but we don't count it towards the amount of negatives.
  defp print_as_negated_bdd(top, top), do: 1
  defp print_as_negated_bdd(bdd_leaf(_, _), _top), do: 0
  defp print_as_negated_bdd(bdd, top), do: if(negated_bdd?(bdd, top), do: 1, else: -100)

  defp negated_bdd?({top, bdd, :bdd_bot, :bdd_bot}, top),
    do: negated_bdd?(bdd, top)

  defp negated_bdd?({_, :bdd_bot, :bdd_bot, bdd}, top),
    do: bdd in [:bdd_top, top] or negated_bdd?(bdd, top)

  defp negated_bdd?(_, _), do: false

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

  # Two gradual types are disjoint if their upper bounds are disjoint.
  def disjoint?(left, right) do
    left_upper = unfold(left) |> Map.get(:dynamic, left)
    right_upper = unfold(right) |> Map.get(:dynamic, right)

    not non_disjoint_intersection?(left_upper, right_upper)
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
    {left_dynamic, left_static} = pop_dynamic(left)

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
    {left_dynamic, left_static} = pop_dynamic(left)

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
  Optimized version of `not empty?(intersection(bitstring(), type))`.
  """
  def bitstring_type?(:term), do: true
  def bitstring_type?(%{dynamic: :term}), do: true

  def bitstring_type?(%{dynamic: %{bitmap: bitmap}}) when (bitmap &&& @bit_bitstring) != 0,
    do: true

  def bitstring_type?(%{bitmap: bitmap}) when (bitmap &&& @bit_bitstring) != 0, do: true
  def bitstring_type?(_), do: false

  @doc """
  Optimized version of `not empty?(intersection(difference(bitstring(), binary()), type))`.

  Notice that this does not mean it is not a binary.
  It only means the bitstring bit is up, regardless of the binary bit.
  """
  def bitstring_no_binary_type?(:term), do: true
  def bitstring_no_binary_type?(%{dynamic: :term}), do: true

  def bitstring_no_binary_type?(%{dynamic: %{bitmap: bitmap}})
      when (bitmap &&& @bit_bitstring_no_binary) != 0, do: true

  def bitstring_no_binary_type?(%{bitmap: bitmap})
      when (bitmap &&& @bit_bitstring_no_binary) != 0, do: true

  def bitstring_no_binary_type?(_), do: false

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
        empty_list: @bit_empty_list,
        integer: @bit_integer,
        float: @bit_float,
        pid: @bit_pid,
        port: @bit_port,
        reference: @bit_reference
      ]

    quoted =
      for {type, mask} <- pairs,
          (mask &&& val) !== 0,
          do: {type, [], []}

    case val &&& @bit_bitstring do
      0 -> quoted
      1 -> [{:binary, [], []} | quoted]
      2 -> [{:and, [], [{:bitstring, [], []}, {:not, [], [{:binary, [], []}]}]} | quoted]
      3 -> [{:bitstring, [], []} | quoted]
    end
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

  @doc """
  Compute the booleaness of an element.

  It is either `:none`, `:maybe_true` (therefore never false),
  `:maybe_false` (therefore never true), or `:maybe_both`.

  This is an optimization against checking if a term is
  not disjoint on either true or false.
  """
  def booleaness(:term), do: :maybe_both

  def booleaness(%{} = descr) do
    descr = Map.get(descr, :dynamic, descr)

    case descr do
      :term ->
        :maybe_both

      %{atom: {:union, %{true: _, false: _}}} ->
        :maybe_both

      %{atom: {:union, %{true: _} = union}} when map_size(descr) == 1 and map_size(union) == 1 ->
        {true, :always}

      %{atom: {:union, %{false: _} = union}} when map_size(descr) == 1 and map_size(union) == 1 ->
        {false, :always}

      %{atom: {:union, %{true: _}}} ->
        {true, :maybe}

      %{atom: {:union, %{false: _}}} ->
        {false, :maybe}

      %{atom: {:union, _}} ->
        :none

      %{atom: {:negation, %{true: _, false: _}}} ->
        :none

      %{atom: {:negation, %{true: _}}} ->
        {false, :maybe}

      %{atom: {:negation, %{false: _}}} ->
        {true, :maybe}

      %{atom: {:negation, _}} ->
        :maybe_both

      _ ->
        :none
    end
  end

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
    {static_or_dynamic, static} = pop_dynamic(descr)

    if atom_only?(static) do
      case static_or_dynamic do
        :term -> {:infinite, []}
        %{atom: {:union, set}} -> {:finite, :sets.to_list(set)}
        %{atom: {:negation, set}} -> {:infinite, :sets.to_list(set)}
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
  #   - Normalized form for function applications: {domain, arrows} is produced by `fun_normalize/2`

  # * Examples:
  #   - fun([integer()], atom()): A function from integer to atom
  #   - intersection(fun([integer()], atom()), fun([float()], boolean())): A function handling both signatures

  # Note: Function domains are expressed as tuple types. We use separate representations
  # rather than unary functions with tuple domains to handle special cases like representing
  # functions of a specific arity (e.g., (none,none->term) for arity 2).
  defp fun_new(arity, inputs, output), do: {:union, %{arity => bdd_leaf(inputs, output)}}

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
    arity = length(args)
    dynamic_arguments? = Enum.any?(args, &gradual?/1)
    dynamic_output? = gradual?(output)

    if dynamic_arguments? or dynamic_output? do
      input_static = if dynamic_arguments?, do: Enum.map(args, &upper_bound/1), else: args
      input_dynamic = if dynamic_arguments?, do: Enum.map(args, &lower_bound/1), else: args

      output_static = if dynamic_output?, do: lower_bound(output), else: output
      output_dynamic = if dynamic_output?, do: upper_bound(output), else: output

      %{
        fun: fun_new(arity, input_static, output_static),
        dynamic: %{fun: fun_new(arity, input_dynamic, output_dynamic)}
      }
    else
      # No dynamic components, use standard function type
      %{fun: fun_new(arity, args, output)}
    end
  end

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

      τ◦τ′ = (lower_bound(τ) ◦ upper_bound(τ′)) or (dynamic(upper_bound(τ) ◦ upper_bound(τ′)))

  Where:

  - τ is a dynamic function type
  - τ′ are the arguments
  - ◦ is function application

  For more details, see Section 13.2 of
  https://gldubc.github.io/assets/duboc-phd-thesis-typing-elixir.pdf

  ## Examples

      iex> fun_apply(fun([integer()], atom()), [integer()])
      {:ok, atom()}

      iex> fun_apply(fun([integer()], atom()), [float()])
      :badarg

      iex> fun_apply(fun([dynamic()], atom()), [dynamic()])
      {:ok, atom()}
  """
  def fun_apply(:term, _arguments), do: :badfun

  def fun_apply(fun, arguments) do
    case :maps.take(:dynamic, fun) do
      :error ->
        if fun_only?(fun) do
          fun_apply_with_strategy(fun, nil, arguments)
        else
          :badfun
        end

      {fun_dynamic, fun_static} ->
        cond do
          fun_static == %{} and dynamic_fun_top?(fun_dynamic) ->
            {:ok, dynamic()}

          fun_only?(fun_static) ->
            fun_apply_with_strategy(fun_static, fun_dynamic, arguments)

          true ->
            :badfun
        end
    end
  end

  defp fun_only?(descr), do: empty?(Map.delete(descr, :fun))
  defp dynamic_fun_top?(:term), do: true
  defp dynamic_fun_top?(%{fun: {:negation, map}}), do: map == %{}
  defp dynamic_fun_top?(_), do: false

  # Gradual function application algorithm.
  #
  # 1. Domain check against the extended gradual domain (see fun_normalize_both/3):
  #    - If the argument is a subtype of the domain, proceed to application.
  #    - Otherwise, in gradual mode, check compatibility (see below). If
  #      compatible, the application may succeed at runtime but we have no
  #      static information about the result, so we return dynamic().
  #    - Otherwise, error.
  # 2. Compute the application result in three cases:
  #    - Fully static: apply static arrows to the arguments directly.
  #    - Purely dynamic function (no static arrows): wrap the result of
  #      applying dynamic arrows to upper-bounded arguments in dynamic().
  #    - Mixed: union the static result with the dynamic-wrapped dynamic result.
  defp fun_apply_with_strategy(fun_static, fun_dynamic, arguments) do
    args_domain = args_to_domain(arguments)
    static? = fun_dynamic == nil and Enum.all?(arguments, fn arg -> not gradual?(arg) end)
    arity = length(arguments)

    with false <- Enum.any?(arguments, &empty?/1),
         {:ok, domain, static_arrows, dynamic_arrows} <-
           fun_normalize_both(fun_static, fun_dynamic, arity) do
      cond do
        # The domain here is the extended gradual domain computed by
        # fun_normalize_both/3. If the argument does not satisfy it, we
        # check compatibility before rejecting.
        #
        # Compatibility has two cases to avoid a degenerate situation.
        # If the argument is purely dynamic (e.g. dynamic() and bool()),
        # its static part (lower bound) is none(). We do not want
        # none() <= domain to trivially succeed, because that would mean
        # "a diverging argument is accepted by any function", which is true but
        # useless. So when the static part is empty, we instead check
        # that the upper bound overlaps with the domain. When the static
        # part is non-empty, we check it is a subtype of the domain.
        not subtype?(args_domain, domain) ->
          if static? or not compatible?(args_domain, domain),
            do: {:badarg, domain_to_flat_args(domain, arity)},
            else: {:ok, dynamic()}

        static? ->
          {:ok, fun_apply_static(arguments, static_arrows)}

        static_arrows == [] ->
          # Purely dynamic function (e.g. dynamic() and (integer() -> integer())).
          # There are no static arrows, so the general mixed formula simplifies:
          # applying none() to anything yields none(), so the static branch
          # vanishes and only the dynamic branch remains.
          # The result is wrapped in dynamic(), so it is safe regardless of argument precision.
          # If the upper-bounded arguments escape the domain, fun_apply_static returns term(),
          # and dynamic(term()) = dynamic(), which brings back to the compatible case.
          arguments = Enum.map(arguments, &upper_bound/1)
          {:ok, dynamic(fun_apply_static(arguments, dynamic_arrows))}

        true ->
          # Mixed case: union of the static and dynamic results.
          # static_arrows (lower materialization) contain only arrows that are
          # guaranteed to exist at runtime. Static guarantees about the result
          # come from these alone.
          # dynamic_arrows (upper materialization) include dynamically uncertain
          # arrows, so their result is wrapped in dynamic().
          # We use upper_bound on the arguments for both branches. This is sound
          # because the dynamic branch wraps its result in dynamic().
          # It is more strict and informative than using lower_bound in the static part,
          # as it amounts to assuming the worst case of using the statically present arrows.
          arguments = Enum.map(arguments, &upper_bound/1)

          {:ok,
           union(
             fun_apply_static(arguments, static_arrows),
             dynamic(fun_apply_static(arguments, dynamic_arrows))
           )}
      end
    else
      true -> {:badarg, arguments}
      error -> error
    end
  end

  # Normalizes a gradual function type into static and dynamic arrow
  # components, and computes the extended gradual domain.
  #
  # The extended gradual domain is:
  #   dom(upper_bound and fun_top) or dynamic(dom(lower_bound))
  #
  # fun_normalize/3 implicitly performs the "and fun_top" projection
  # because it only looks at the :fun component, so any non-function
  # parts of the type are automatically discarded.
  #
  # Fallback cases:
  #
  # - Static normalization succeeds but dynamic fails (e.g. the dynamic
  #   part has no arrows at the given arity): we discard the dynamic
  #   arrows and use the static arrows for both branches, degenerating
  #   to the fully static case. This is sound because ignoring unusable
  #   dynamic information cannot produce incorrect static results.
  #
  # - Static normalization fails (:badfun): only the dynamic arrows
  #   contribute. The domain becomes dom(upper_bound) or dynamic(),
  #   reflecting that the lower bound has no function type at this arity.
  #   The application proceeds as purely dynamic (static_arrows = []).
  defp fun_normalize_both(fun_static, fun_dynamic, arity) do
    case fun_normalize(fun_static, arity) do
      {:ok, static_domain, static_arrows} ->
        # A static function with arrows at other arities is a mixed-arity union:
        # we cannot safely apply it because at runtime the value may have a
        # different arity than the one being called with.
        case fun_other_non_empty_arities(fun_static, arity) do
          [] when fun_dynamic == nil ->
            {:ok, static_domain, static_arrows, static_arrows}

          [] ->
            case fun_normalize(fun_dynamic, arity) do
              {:ok, dynamic_domain, dynamic_arrows} ->
                domain = union(dynamic_domain, dynamic(static_domain))
                {:ok, domain, static_arrows, dynamic_arrows}

              _ ->
                # Dynamic normalization failed: fall back to static-only.
                {:ok, static_domain, static_arrows, static_arrows}
            end

          other ->
            {:badarity, [arity | other]}
        end

      :badfun ->
        # No static arrows: dynamic-only path. Mixed-arity in the dynamic
        # component is fine — we pick the matching-arity arrows and the
        # result is wrapped in dynamic(), reflecting the uncertainty.
        case fun_normalize(fun_dynamic, arity) do
          {:ok, dynamic_domain, dynamic_arrows} ->
            {:ok, union(dynamic_domain, dynamic()), [], dynamic_arrows}

          error ->
            error
        end

      error ->
        error
    end
  end

  defp fun_other_non_empty_arities(%{fun: {:union, bdds}}, arity) do
    case :maps.take(arity, bdds) do
      {_bdd, rest} ->
        for {a, b} <- rest,
            not Enum.all?(bdd_to_dnf(b), fn {pos, neg} -> fun_line_empty?(pos, neg) end),
            do: a

      :error ->
        []
    end
  end

  defp fun_other_non_empty_arities(_, _), do: []

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
  defp fun_normalize(:term, arity), do: fun_normalize(fun(arity), arity)

  # In that case, we transform the {:negation, _} into a single union
  # where we add the previous negatives into the specified arity.
  defp fun_normalize(%{fun: {:negation, _}} = neg_fun, arity) do
    fun_normalize(intersection(fun(arity), neg_fun), arity)
  end

  defp fun_normalize(%{fun: {:union, bdds}}, arity) do
    case :maps.take(arity, bdds) do
      {bdd, _rest} ->
        {domain, arrows} =
          Enum.reduce(fun_bdd_to_pos_dnf(arity, bdd), {term(), []}, fn pos_funs,
                                                                       {domain, arrows} ->
            {intersection(domain, fetch_domain(pos_funs)), [pos_funs | arrows]}
          end)

        if arrows == [] do
          {:badarity, :maps.keys(bdds)}
        else
          {:ok, domain, arrows}
        end

      :error ->
        {:badarity, :maps.keys(bdds)}
    end
  end

  defp fun_normalize(%{}, _arity), do: :badfun

  # Applies a static function type to arguments by reducing over the
  # function's DNF clauses. Each clause is an intersection of arrows,
  # processed by aux_apply/4 with rets_reached initialized to term().
  #
  # When the arguments are within the domain, this is the standard
  # application operator. When the arguments escape the domain, the
  # result is term() (see aux_apply/4).
  defp fun_apply_static(arguments, arrows) do
    type_args = args_to_domain(arguments)

    Enum.reduce(arrows, none(), fn intersection_of_arrows, acc ->
      aux_apply(acc, type_args, term(), intersection_of_arrows)
    end)
  end

  # A fast way to do function application when the arguments of the function are disjoint.
  # Just build the union of all the return types of arrows that match the input.
  defp apply_disjoint(input_arguments, arrows) do
    type_input = args_to_domain(input_arguments)

    Enum.reduce(arrows, none(), fn {args, ret}, acc_return ->
      if empty?(intersection(args_to_domain(args), type_input)),
        do: acc_return,
        else: union(acc_return, ret)
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
  #
  # Domain escape: if the input is not covered by the union of all the
  # arrow domains in the clause, the result is term(). This is because
  # rets_reached starts at term() and is only refined (intersected) when
  # an arrow's domain covers the input, which is check by dom_subtract.
  # Along a path where no arrow covers the input, rets_reached stays
  # term() and gets unioned into the result at the base case. Since
  # term() is maximal, the overall result for that clause is term().

  # For more details, see Definitions 2.20 or 6.11 in https://vlanvin.fr/papers/thesis.pdf
  # For the escape case, see Section 13.2 of
  # https://gldubc.github.io/assets/duboc-phd-thesis-typing-elixir.pdf
  defp aux_apply(result, _input, rets_reached, []) do
    if subtype?(rets_reached, result), do: result, else: union(result, rets_reached)
  end

  defp aux_apply(result, input, returns_reached, [{args, ret} | arrow_intersections]) do
    # Calculate the part of the input not covered by this arrow's domain
    dom_subtract = difference(input, args_to_domain(args))

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
      if empty?(dom_subtract),
        do: result,
        else: aux_apply(result, dom_subtract, returns_reached, arrow_intersections)

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
  defp fun_bdd_to_dnf(bdd),
    do: bdd_to_dnf(bdd) |> Enum.filter(fn {pos, neg} -> not fun_line_empty?(pos, neg) end)

  # Check if all functions types for all arities are empty.
  defp fun_empty?({:negation, _}), do: false

  defp fun_empty?({:union, repr}) do
    Enum.all?(repr, fn {_ar, bdd} ->
      Enum.all?(bdd_to_dnf(bdd), fn {pos, neg} -> fun_line_empty?(pos, neg) end)
    end)
  end

  # A function type {positives, negatives} is empty if there exists a negative
  # function that negates the whole positive intersection
  #
  ## Examples
  #
  # - `{[fun(1)], []}` is not empty
  # - `{[fun(integer() -> atom())], [fun(none() -> term())]}` is empty
  defp fun_line_empty?([], _), do: false

  defp fun_line_empty?(positives, negatives) do
    # Check if any negative function negates the whole positive intersection
    # e.g. (integer() -> atom()) is negated by:
    #
    # * (none() -> term())
    # * (none() -> atom())
    # * (integer() -> term())
    # * (integer() -> atom())
    Enum.any?(negatives, fn {neg_arguments, neg_return} ->
      # Check if the negative function's domain is a supertype of the positive
      # domain and if the phi function determines emptiness.
      subtype?(args_to_domain(neg_arguments), fetch_domain(positives)) and
        phi_starter(neg_arguments, neg_return, positives)
    end)
  end

  # Returns the union of all domains of the arrows in the intersection of positives.
  defp fetch_domain(positives) do
    Enum.reduce(positives, none(), fn {args, _}, acc -> union(acc, args_to_domain(args)) end)
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
        # Initialize memoization cache for the recursive phi computation
        arguments = Enum.map(arguments, &{false, &1})
        {result, _cache} = phi(arguments, {false, negation(return)}, positives, %{})
        result
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

  # For all integers keys in fun_repr1, fun_repr2, call fun_bdd_union on the values that are in
  # both, else return the single value.
  defp fun_union({tag1, repr1}, {tag2, repr2}) do
    case {tag1, tag2} do
      {:union, :union} -> {:union, fun_repr_union(repr1, repr2)}
      {:negation, :negation} -> {:negation, fun_repr_intersection(repr1, repr2)}
      {:union, :negation} -> {:negation, fun_repr_difference(repr2, repr1)}
      {:negation, :union} -> {:negation, fun_repr_difference(repr1, repr2)}
    end
  end

  # For all integer keys that are both in fun_repr1 and fun_repr2, call fun_bdd_intersection
  # on the values that are in both, else discard.
  defp fun_intersection({tag1, repr1}, {tag2, repr2}) do
    {tag, repr} =
      case {tag1, tag2} do
        {:union, :union} -> {:union, fun_repr_intersection(repr1, repr2)}
        {:negation, :negation} -> {:negation, fun_repr_union(repr1, repr2)}
        {:union, :negation} -> {:union, fun_repr_difference(repr1, repr2)}
        {:negation, :union} -> {:union, fun_repr_difference(repr2, repr1)}
      end

    if tag == :union and map_size(repr) == 0, do: 0, else: {tag, repr}
  end

  defp fun_difference({tag1, repr1}, {tag2, repr2}) do
    {tag, repr} =
      case {tag1, tag2} do
        {:union, :union} -> {:union, fun_repr_difference(repr1, repr2)}
        {:negation, :negation} -> {:union, fun_repr_difference(repr2, repr1)}
        {:union, :negation} -> {:union, fun_repr_intersection(repr1, repr2)}
        {:negation, :union} -> {:negation, fun_repr_union(repr1, repr2)}
      end

    if tag == :union and map_size(repr) == 0, do: 0, else: {tag, repr}
  end

  defp fun_repr_union(repr1, repr2) do
    symmetrical_merge(repr1, repr2, fn _arity, v1, v2 -> bdd_union(v1, v2) end)
  end

  defp fun_repr_intersection(repr1, repr2) do
    symmetrical_intersection(repr1, repr2, fn _arity, v1, v2 -> bdd_intersection(v1, v2) end)
  end

  defp fun_repr_difference(repr1, repr2) do
    :maps.fold(
      fn arity, bdd2, acc ->
        case acc do
          %{^arity => bdd1} ->
            diff = bdd_difference(bdd1, bdd2)
            if diff in @empty_difference, do: Map.delete(acc, arity), else: %{acc | arity => diff}

          %{} ->
            acc
        end
      end,
      repr1,
      repr2
    )
  end

  # Converts the static and dynamic parts of descr to its quoted
  # representation. The goal here is to do the opposite of fun_descr
  # and put static and dynamic parts back together to improve
  # pretty printing.
  defp fun_denormalize(%{fun: {:union, static_repr}}, %{fun: {:union, dynamic_repr}}, opts) do
    # Denormalize each arity
    for {arity, static_bdd} <- static_repr,
        {^arity, dynamic_bdd} <- dynamic_repr,
        reduce: {static_repr, dynamic_repr, []} do
      {statics, dynamics, acc} ->
        with {:ok, quoted} <- fun_denormalize_arity(arity, static_bdd, dynamic_bdd, opts) do
          {Map.delete(statics, arity), Map.delete(dynamics, arity), [quoted | acc]}
        else
          _ -> {statics, dynamics, acc}
        end
    end
  end

  # If not unions of functions, do not try to denormalize.
  defp fun_denormalize(static_repr, dynamic_repr, _opts) do
    {static_repr, dynamic_repr, []}
  end

  defp fun_denormalize_arity(arity, static_bdd, dynamic_bdd, opts) do
    static_pos = fun_bdd_to_pos_dnf(arity, static_bdd)
    dynamic_pos = fun_bdd_to_pos_dnf(arity, dynamic_bdd)

    cond do
      static_pos == [] or dynamic_pos == [] ->
        :error

      true ->
        {static_pos, dynamic_pos} = fun_denormalize_pos(static_pos, dynamic_pos)

        quoted =
          case dynamic_pos do
            [] ->
              fun_pos_to_quoted(static_pos, opts)

            _ ->
              {:or, [],
               [
                 {:dynamic, [], [fun_pos_to_quoted(dynamic_pos, opts)]},
                 fun_pos_to_quoted(static_pos, opts)
               ]}
          end

        {:ok, quoted}
    end
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

  defp fun_denormalize_intersections([{static_args, static_return} | statics], dynamics, acc) do
    dynamics
    |> Enum.split_while(fn {dynamic_args, dynamic_return} ->
      not arrow_subtype?(static_args, static_return, dynamic_args, dynamic_return)
    end)
    |> case do
      {_dynamics, []} ->
        :error

      {pre, [{dynamic_args, dynamic_return} | post]} ->
        args =
          Enum.zip_with(static_args, dynamic_args, fn static_arg, dynamic_arg ->
            union(dynamic(static_arg), dynamic_arg)
          end)

        return = union(dynamic(dynamic_return), static_return)
        fun_denormalize_intersections(statics, pre ++ post, [{args, return} | acc])
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
  defp fun_to_quoted({:negation, bdds}, opts) do
    case fun_to_quoted({:union, bdds}, opts) do
      [] ->
        [{:fun, [], []}]

      parts ->
        ors = Enum.reduce(parts, &{:or, [], [&2, &1]})
        [{:and, [], [{:fun, [], []}, {:not, [], [ors]}]}]
    end
  end

  defp fun_to_quoted({:union, bdds}, opts) do
    for {arity, bdd} <- bdds,
        pos = fun_bdd_to_pos_dnf(arity, bdd),
        pos != [],
        do: fun_pos_to_quoted(pos, opts)
  end

  defp fun_bdd_to_pos_dnf(arity, :bdd_top) do
    args = List.duplicate(none(), arity)
    ret = term()
    [[{args, ret}]]
  end

  defp fun_bdd_to_pos_dnf(_arity, bdd) do
    fun_bdd_to_dnf(bdd)
    |> Enum.map(fn {pos, _negs} -> pos end)
    |> fun_eliminate_unions([])
  end

  defp fun_eliminate_unions([], acc), do: acc

  defp fun_eliminate_unions([[{args, return}] | tail], acc) do
    # If another arrow is a superset of the current one, we skip it
    superset = fn
      [{other_args, other_return}] ->
        arrow_subtype?(args, return, other_args, other_return)

      _ ->
        false
    end

    if Enum.any?(tail, superset) or Enum.any?(acc, superset) do
      fun_eliminate_unions(tail, acc)
    else
      fun_eliminate_unions(tail, [[{args, return}] | acc])
    end
  end

  defp fun_eliminate_unions([head | tail], acc) do
    fun_eliminate_unions(tail, [head | acc])
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
            # variant from the positive one. This is done in list_bdd_to_pos_dnf/1.
            {list_type, last_type} =
              list_bdd_to_pos_dnf(bdd)
              |> Enum.reduce({list_type, last_type_no_list}, fn
                {list, last, _negs}, {acc_list, acc_last} ->
                  {union(list, acc_list), union(last, acc_last)}
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

  defp list_new(list_type, last_type), do: bdd_leaf(list_type, last_type)

  defp non_empty_list_literals_intersection(list_literals) do
    try do
      Enum.reduce(list_literals, {:term, :term}, fn {next_list, next_last}, {list, last} ->
        {non_empty_intersection!(list, next_list), non_empty_intersection!(last, next_last)}
      end)
    catch
      :empty -> :empty
    end
  end

  # Takes all the lines from the root to the leaves finishing with a 1,
  # and compile into tuples of positive and negative nodes. Keep only the non-empty positives,
  # and include the impact of negations on the last type.
  # To see if a negation changes the last type or the list type, we just need to check
  # if the negative list type is a supertype of the positive list type. In that case,
  # we can remove the negative last type from the positive one.

  # (If this subtracted type was empty, the whole type would be empty)
  defp list_bdd_to_pos_dnf(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.reduce([], fn {pos_list, negs}, acc ->
      case non_empty_list_literals_intersection(pos_list) do
        :empty ->
          acc

        {list, last} ->
          Enum.reduce_while(negs, {last, []}, fn {neg_type, neg_last}, {acc_last, acc_negs} ->
            if subtype?(list, neg_type) do
              difference = difference(acc_last, neg_last)
              if empty?(difference), do: {:halt, nil}, else: {:cont, {difference, acc_negs}}
            else
              {:cont, {acc_last, [{neg_type, neg_last} | acc_negs]}}
            end
          end)
          |> case do
            nil -> acc
            {last, negs} -> [{list, last, Enum.reverse(negs)} | acc]
          end
      end
    end)
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

  defp list_top?(bdd_leaf(:term, :term)), do: true
  defp list_top?(_), do: false

  @doc """
  Returns the element type of a list, assuming the list is proper.

  It returns a two-element tuple. The first element dictates the
  empty list type. The second element returns the value type.

      {boolean(), t() or nil}

  If the value is `nil`, it means that component is missing.
  Note `{false, nil}` is not a valid return type, instead it
  returns `:badproperlist`.
  """
  def list_of(:term), do: :badproperlist

  def list_of(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        with {empty_list?, value} <- list_of_static(descr) do
          if empty?(value) and empty_list? == false do
            :badproperlist
          else
            {empty_list?, value}
          end
        end

      {dynamic, static} ->
        with {empty_list?, static_value} <- list_of_static(static) do
          empty_list? =
            empty_list? or
              match?(
                %{bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0,
                dynamic
              )

          dynamic_value =
            case dynamic do
              :term ->
                dynamic()

              %{list: bdd} ->
                Enum.reduce(list_bdd_to_pos_dnf(bdd), none(), fn {list, last, _negs}, acc ->
                  if last == @empty_list or subtype?(last, @empty_list) do
                    union(acc, list)
                  else
                    acc
                  end
                end)

              %{} ->
                none()
            end

          if empty?(dynamic_value) do
            # list_bdd_to_pos_dnf guarantees the lists actually exists,
            # so we can match on none() rather than empty.
            case empty_list? do
              false -> :badproperlist
              true -> {empty_list?, nil}
            end
          else
            {empty_list?, union(static_value, dynamic(dynamic_value))}
          end
        end
    end
  end

  defp list_of_static(descr) do
    case descr do
      %{bitmap: @bit_empty_list} ->
        case empty?(Map.drop(descr, [:bitmap, :list])) do
          true -> list_of_static(descr, true)
          false -> :badproperlist
        end

      %{bitmap: _} ->
        :badproperlist

      %{} ->
        case empty?(Map.delete(descr, :list)) do
          true -> list_of_static(descr, false)
          false -> :badproperlist
        end
    end
  end

  # A list is proper if it's either the empty list alone, or all non-empty
  # list types have tails that are subtypes of empty list.
  defp list_of_static(%{list: bdd}, empty_list) do
    try do
      result =
        Enum.reduce(list_bdd_to_pos_dnf(bdd), none(), fn {list, last, _negs}, acc ->
          if last == @empty_list or subtype?(last, @empty_list) do
            union(acc, list)
          else
            throw(:badproperlist)
          end
        end)

      {empty_list, result}
    catch
      :badproperlist -> :badproperlist
    end
  end

  defp list_of_static(%{}, empty_list) do
    {empty_list, none()}
  end

  defp list_intersection(bdd1, bdd2) do
    cond do
      list_top?(bdd1) and is_tuple(bdd2) -> bdd2
      list_top?(bdd2) and is_tuple(bdd1) -> bdd1
      true -> bdd_intersection(bdd1, bdd2, &list_leaf_intersection/2)
    end
  end

  defp list_leaf_intersection(bdd_leaf(list1, last1), bdd_leaf(list2, last2)) do
    try do
      list = non_empty_intersection!(list1, list2)
      last = non_empty_intersection!(last1, last2)
      bdd_leaf(list, last)
    catch
      :empty -> :bdd_bot
    end
  end

  defp list_difference(bdd_leaf(:term, :term), bdd_leaf(:term, :term)),
    do: :bdd_bot

  defp list_difference(bdd_leaf(:term, :term), bdd2),
    do: bdd_negation(bdd2)

  # Computes the difference between two BDD (Binary Decision Diagram) list types.
  # It progressively subtracts each type in bdd2 from all types in bdd1.
  # The algorithm handles three cases:
  # 1. Disjoint types: keeps the original type from bdd1
  # 2. Subtype relationship:
  #    a) If bdd2 type is a supertype, keeps only the negations
  #    b) If only the last type differs, subtracts it
  # 3. Base case: adds bdd2 type to negations of bdd1 type
  # The result may be larger than the initial bdd1, which is maintained in the accumulator.
  defp list_difference(bdd_leaf(list1, last1) = bdd1, bdd_leaf(list2, last2) = bdd2) do
    if subtype?(list1, list2) do
      if subtype?(last1, last2),
        do: :bdd_bot,
        else: bdd_leaf(list1, difference(last1, last2))
    else
      bdd_difference(bdd1, bdd2, &list_leaf_difference/3)
    end
  end

  defp list_difference(bdd1, bdd2),
    do: bdd_difference(bdd1, bdd2, &list_leaf_difference/3)

  defp list_leaf_difference(bdd_leaf(list1, last1), bdd_leaf(list2, last2), _) do
    if disjoint?(list1, list2) or disjoint?(last1, last2) do
      :disjoint
    else
      :none
    end
  end

  defp list_empty?(@non_empty_list_top), do: false

  defp list_empty?(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.all?(fn {pos, negs} ->
      case non_empty_list_literals_intersection(pos) do
        :empty -> true
        {list, last} -> list_line_empty?(list, last, negs)
      end
    end)
  end

  defp list_line_empty?(list_type, last_type, negs) do
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
          {:ok, static_value}
        else
          :badnonemptylist
        end

      {dynamic, static} ->
        dynamic_value = list_hd_static(dynamic)

        if non_empty_list_only?(static) and not empty?(dynamic_value) do
          {:ok, union(dynamic(dynamic_value), list_hd_static(static))}
        else
          :badnonemptylist
        end
    end
  end

  defp list_hd_static(:term), do: :term

  defp list_hd_static(%{list: bdd}) do
    list_bdd_to_pos_dnf(bdd)
    |> Enum.reduce(none(), fn {list, _last, _negs}, acc -> union(acc, list) end)
  end

  defp list_hd_static(%{}), do: none()

  @doc """
  Returns the tail of a list.

  For a `non_empty_list(t)`, the tail type is `list(t)`.
  For an improper list `non_empty_list(t, s)`, the tail type is
  `list(t, s) or s` (either the rest of the list or the terminator)
  """
  def list_tl(:term), do: :badnonemptylist

  def list_tl(descr) do
    case :maps.take(:dynamic, descr) do
      :error ->
        static_value = list_tl_static(descr)

        if non_empty_list_only?(descr) and not empty?(static_value) do
          {:ok, static_value}
        else
          :badnonemptylist
        end

      {dynamic, static} ->
        dynamic_value = list_tl_static(dynamic)

        if non_empty_list_only?(static) and not empty?(dynamic_value) do
          {:ok, union(dynamic(dynamic_value), list_tl_static(static))}
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

    list_bdd_to_pos_dnf(bdd)
    |> Enum.reduce(initial, fn {_list, last, _negs}, acc -> union(acc, last) end)
  end

  defp list_tl_static(%{}), do: none()

  defp list_to_quoted(bdd, empty?, opts) do
    dnf = list_normalize(bdd)

    {unions, list_rendered?} =
      dnf
      |> Enum.reduce({[], false}, fn {list_type, last_type, negs}, {acc, list_rendered?} ->
        {name, arguments, list_rendered?} =
          cond do
            subtype?(last_type, @empty_list) ->
              name = if empty?, do: :list, else: :non_empty_list
              {name, [to_quoted(list_type, opts)], empty?}

            # Sugar: print the last type as term() if it only misses non empty lists.
            subtype?(@not_non_empty_list, last_type) ->
              args = [to_quoted(list_type, opts), {:term, [], []}]
              {:non_empty_list, args, list_rendered?}

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
  # or that can be merged with others). List_bdd_to_pos_dnf already takes into account the
  # impact of negations on the last type.
  defp list_normalize(bdd) do
    list_bdd_to_pos_dnf(bdd)
    |> Enum.reduce([], fn {list, last, negs}, acc ->
      # Prune negations from those with empty intersections.
      negs =
        Enum.uniq(negs)
        |> Enum.filter(fn {nlist, nlast} ->
          not empty?(intersection(list, nlist)) and not empty?(intersection(last, nlast))
        end)

      add_to_list_normalize(acc, list, last, negs)
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
    case pop_elem(n, {list, last}, []) do
      {true, n1} -> [{t, l, n1} | rest]
      {false, _} -> [cur | add_to_list_normalize(rest, list, last, n)]
    end
  end

  defp add_to_list_normalize(rest, list, last, negs), do: [{list, last, negs} | rest]

  defp pop_elem([key | t], key, acc), do: {true, :lists.reverse(acc, t)}
  defp pop_elem([h | t], key, acc), do: pop_elem(t, key, [h | acc])
  defp pop_elem([], _key, acc), do: {false, :lists.reverse(acc)}

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
         %{bitmap: bitmap} when map_size(descr) == 1 and bitmap != @bit_bitstring <- descr,
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
  # For instance, the type `%{atom() => if_set(integer())}` is the type of maps where atom keys
  # map to integers, without any non-atom keys. It is represented using the map literal
  # `{%{atom: if_set(integer())}, %{}}`, with no defined keys.
  #
  # The type `%{..., atom() => integer()}` represents maps with atom keys bound to integers,
  # and other keys bound to any type. It will be represented using a map domain that maps
  # atom to `if_set(integer())`, and every other domain key to `term_or_optional()`.

  @doc """
  Converts a type into domain keys.
  """
  def to_domain_keys(:term), do: @domain_key_types

  def to_domain_keys(%{dynamic: dynamic}), do: to_domain_keys(dynamic)

  def to_domain_keys(key_descr) do
    for {type_kind, type} <- key_descr, reduce: [] do
      acc ->
        cond do
          type_kind == :list and
              match?(%{bitmap: bitmap} when (bitmap &&& @bit_empty_list) != 0, key_descr) ->
            acc

          type_kind == :atom and match?({:union, _}, type) ->
            acc

          type_kind == :bitmap ->
            bitmap_to_domain_keys(type, acc)

          not empty?(%{type_kind => type}) ->
            [type_kind | acc]

          true ->
            acc
        end
    end
  end

  defp bitmap_to_domain_keys(bitmap, acc) do
    acc = if (bitmap &&& @bit_binary) != 0, do: [:binary | acc], else: acc
    acc = if (bitmap &&& @bit_bitstring_no_binary) != 0, do: [:bitstring | acc], else: acc
    acc = if (bitmap &&& @bit_empty_list) != 0, do: [:list | acc], else: acc
    acc = if (bitmap &&& @bit_integer) != 0, do: [:integer | acc], else: acc
    acc = if (bitmap &&& @bit_float) != 0, do: [:float | acc], else: acc
    acc = if (bitmap &&& @bit_pid) != 0, do: [:pid | acc], else: acc
    acc = if (bitmap &&& @bit_port) != 0, do: [:port | acc], else: acc
    acc = if (bitmap &&& @bit_reference) != 0, do: [:reference | acc], else: acc
    acc
  end

  defp domain_key_to_descr(:atom), do: atom()
  defp domain_key_to_descr(:bitstring), do: bitstring_no_binary()
  defp domain_key_to_descr(:binary), do: binary()
  defp domain_key_to_descr(:integer), do: integer()
  defp domain_key_to_descr(:float), do: float()
  defp domain_key_to_descr(:pid), do: pid()
  defp domain_key_to_descr(:port), do: port()
  defp domain_key_to_descr(:reference), do: reference()
  defp domain_key_to_descr(:fun), do: fun()
  defp domain_key_to_descr(:tuple), do: tuple()
  defp domain_key_to_descr(:map), do: open_map()
  defp domain_key_to_descr(:list), do: @list_top

  defp map_descr(tag, pairs) do
    {fields, domains, dynamic?} = map_descr_pairs(pairs, [], @fields_new, false)

    map_new =
      if not is_fields_empty(domains) do
        domains =
          if tag == :open do
            value = term_or_optional()
            fields_put_all_new(domains, @domain_key_types, value)
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

  defp map_put_domain(domain, domain_keys, value) when is_list(domain_keys) do
    map_put_domain(domain, :lists.usort(domain_keys), if_set(value), value)
  end

  defp map_put_domain([{k1, v1} | t1], [k2 | _] = keys, initial, value) when k1 < k2 do
    [{k1, v1} | map_put_domain(t1, keys, initial, value)]
  end

  defp map_put_domain([{k1, v1} | t1], [k1 | keys], _initial, value) do
    [{k1, union(v1, value)} | map_put_domain(t1, keys, if_set(value), value)]
  end

  defp map_put_domain(domain, [k2 | keys], initial, value) do
    [{k2, initial} | map_put_domain(domain, keys, initial, value)]
  end

  defp map_put_domain(domain, [], _initial, _value), do: domain

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
    {fields_from_reverse_list(fields), domain, dynamic?}
  end

  # Gets the default type associated to atom keys in a map.
  defp map_key_tag_to_type(:open), do: term_or_optional()
  defp map_key_tag_to_type(:closed), do: not_set()
  defp map_key_tag_to_type(domains), do: fields_get(domains, :atom, not_set())

  # Gets the domain type association to a map.
  # In this case, we already remove the optional to simplify upstream.
  @compile {:inline, map_domain_tag_to_type: 1}
  defp map_domain_tag_to_type(:open), do: term()
  defp map_domain_tag_to_type(:closed), do: none()

  defp map_domain_tag_to_type(domain, key) when is_list(domain) do
    remove_optional(fields_get(domain, key, none()))
  end

  defp map_domain_tag_to_type(domain, _key) do
    map_domain_tag_to_type(domain)
  end

  defguardp is_optional_static(map)
            when is_map(map) and is_map_key(map, :optional)

  defp map_new(tag, fields), do: bdd_leaf(tag, fields)

  defp map_only?(descr), do: empty?(Map.delete(descr, :map))

  defp non_empty_map_only?(descr) do
    case :maps.take(:map, descr) do
      :error -> false
      {map_bdd, rest} -> empty?(rest) and not map_empty?(map_bdd)
    end
  end

  defp map_union(bdd_leaf(tag1, fields1), bdd_leaf(tag2, fields2)) do
    case maybe_optimize_map_union(tag1, fields1, tag2, fields2) do
      {tag, fields} -> bdd_leaf(tag, fields)
      nil -> bdd_union(bdd_leaf(tag1, fields1), bdd_leaf(tag2, fields2))
    end
  end

  defp map_union(bdd1, bdd2), do: bdd_union(bdd1, bdd2)

  defp maybe_optimize_map_union(:open, empty, _, _) when is_fields_empty(empty),
    do: {:open, @fields_new}

  defp maybe_optimize_map_union(_, _, :open, empty) when is_fields_empty(empty),
    do: {:open, @fields_new}

  defp maybe_optimize_map_union(tag1, pos1, tag2, pos2)
       when is_atom(tag1) and is_atom(tag2) do
    case map_union_strategy(pos1, pos2, tag1, tag2, :all_equal) do
      :all_equal when tag1 == :open -> {tag1, pos1}
      :all_equal -> {tag2, pos2}
      {:one_key_difference, key, v1, v2} -> {tag1, fields_store(key, union(v1, v2), pos1)}
      :left_subtype_of_right -> {tag2, pos2}
      :right_subtype_of_left -> {tag1, pos1}
      :none -> nil
    end
  end

  defp maybe_optimize_map_union(_, _, _, _), do: nil

  defp map_union_strategy([{k1, _} | t1], [{k2, _} | _] = l2, tag1, tag2, status)
       when k1 < k2 do
    # Left side has a key the right side does not have,
    # left can only be a subtype if the right side is open.
    case status do
      _ when tag2 != :open ->
        :none

      :all_equal ->
        map_union_strategy(t1, l2, tag1, tag2, :left_subtype_of_right)

      {:one_key_difference, _, p1, p2} ->
        if subtype?(p1, p2),
          do: map_union_strategy(t1, l2, tag1, tag2, :left_subtype_of_right),
          else: :none

      :left_subtype_of_right ->
        map_union_strategy(t1, l2, tag1, tag2, :left_subtype_of_right)

      _ ->
        :none
    end
  end

  defp map_union_strategy([{k1, _} | _] = l1, [{k2, _} | t2], tag1, tag2, status)
       when k1 > k2 do
    # Right side has a key the left side does not have,
    # right can only be a subtype if the left side is open.
    case status do
      _ when tag1 != :open ->
        :none

      :all_equal ->
        map_union_strategy(l1, t2, tag1, tag2, :right_subtype_of_left)

      {:one_key_difference, _, p1, p2} ->
        if subtype?(p2, p1),
          do: map_union_strategy(l1, t2, tag1, tag2, :right_subtype_of_left),
          else: :none

      :right_subtype_of_left ->
        map_union_strategy(l1, t2, tag1, tag2, :right_subtype_of_left)

      _ ->
        :none
    end
  end

  defp map_union_strategy([{_, v} | t1], [{_, v} | t2], tag1, tag2, status) do
    # Same key and same value, nothing changes
    map_union_strategy(t1, t2, tag1, tag2, status)
  end

  defp map_union_strategy([{k1, v1} | t1], [{_, v2} | t2], tag1, tag2, status) do
    # They have the same key but different values
    case status do
      :all_equal ->
        cond do
          # Don't do difference on struct keys
          k1 != :__struct__ and tag1 == tag2 ->
            map_union_strategy(t1, t2, tag1, tag2, {:one_key_difference, k1, v1, v2})

          subtype?(v1, v2) ->
            map_union_strategy(t1, t2, tag1, tag2, :left_subtype_of_right)

          subtype?(v2, v1) ->
            map_union_strategy(t1, t2, tag1, tag2, :right_subtype_of_left)

          true ->
            :none
        end

      :left_subtype_of_right ->
        if subtype?(v1, v2), do: map_union_strategy(t1, t2, tag1, tag2, status), else: :none

      :right_subtype_of_left ->
        if subtype?(v2, v1), do: map_union_strategy(t1, t2, tag1, tag2, status), else: :none

      {:one_key_difference, _key, p1, p2} ->
        cond do
          subtype?(p1, p2) and subtype?(v1, v2) ->
            map_union_strategy(t1, t2, tag1, tag2, :left_subtype_of_right)

          subtype?(p2, p1) and subtype?(v2, v1) ->
            map_union_strategy(t1, t2, tag1, tag2, :right_subtype_of_left)

          true ->
            :none
        end

      _ ->
        :none
    end
  end

  defp map_union_strategy([], [], _tag1, _tag2, status) do
    status
  end

  defp map_union_strategy(l1, l2, tag1, tag2, status) do
    lhs? = tag2 == :open and l2 == []
    rhs? = tag1 == :open and l1 == []

    case status do
      :all_equal when lhs? ->
        :left_subtype_of_right

      :all_equal when rhs? ->
        :right_subtype_of_left

      {:one_key_difference, _, p1, p2} ->
        cond do
          lhs? and subtype?(p1, p2) -> :left_subtype_of_right
          rhs? and subtype?(p2, p1) -> :right_subtype_of_left
          true -> :none
        end

      :left_subtype_of_right when lhs? ->
        :left_subtype_of_right

      :right_subtype_of_left when rhs? ->
        :right_subtype_of_left

      _ ->
        :none
    end
  end

  defp map_intersection(bdd_leaf(:open, []), bdd), do: bdd
  defp map_intersection(bdd, bdd_leaf(:open, [])), do: bdd
  defp map_intersection(bdd1, bdd2), do: map_bdd_intersection(bdd1, bdd2)

  # A variant of bdd_intersection/3 that only continues if the maps are closed
  # or both sides are leafs.
  #
  # This is necessary because the intersection of open maps end-up accumulating
  # fields and it is unlikely to eliminate, which would lead to explosions.
  # However, note this optimization only works because closed nodes come first
  # in the BDD representation. If that was not the case, open nodes would come
  # first and this optimization would not happen if they were mixed.
  defp map_bdd_intersection(bdd_leaf(_, _) = leaf1, bdd_leaf(_, _) = leaf2) do
    map_leaf_intersection(leaf1, leaf2)
  end

  defp map_bdd_intersection(bdd_leaf(:closed, _) = leaf, bdd) do
    bdd_leaf_intersection(leaf, bdd, &map_leaf_intersection/2)
  end

  defp map_bdd_intersection(bdd, bdd_leaf(:closed, _) = leaf) do
    bdd_leaf_intersection(leaf, bdd, &map_leaf_intersection/2)
  end

  defp map_bdd_intersection({bdd_leaf(:closed, _) = leaf, :bdd_top, u, d}, bdd) do
    bdd_leaf_intersection(leaf, bdd, &map_leaf_intersection/2)
    |> bdd_union(map_bdd_intersection(u, bdd))
    |> case do
      result when d == :bdd_bot -> result
      result -> bdd_union(result, bdd_intersection(bdd, {leaf, :bdd_bot, :bdd_bot, d}))
    end
  end

  defp map_bdd_intersection(bdd, {bdd_leaf(:closed, _) = leaf, :bdd_top, u, d}) do
    bdd_leaf_intersection(leaf, bdd, &map_leaf_intersection/2)
    |> bdd_union(map_bdd_intersection(u, bdd))
    |> case do
      result when d == :bdd_bot -> result
      result -> bdd_union(result, bdd_intersection(bdd, {leaf, :bdd_bot, :bdd_bot, d}))
    end
  end

  defp map_bdd_intersection(bdd1, bdd2) do
    bdd_intersection(bdd1, bdd2)
  end

  defp map_leaf_intersection(bdd_leaf(tag1, fields1), bdd_leaf(tag2, fields2)) do
    try do
      {tag, fields} = map_literal_intersection(tag1, fields1, tag2, fields2)
      bdd_leaf(tag, fields)
    catch
      :empty -> :bdd_bot
    end
  end

  defp map_difference(_, bdd_leaf(:open, [])),
    do: :bdd_bot

  defp map_difference(bdd1, bdd2),
    do: bdd_difference(bdd1, bdd2, &map_leaf_difference/3)

  # We only apply this particular optimization when comparing leafs (type == :none).
  # Applying it in other scenarios lead to slow compilation, likely because the
  # introduction of `a_int` and `a_union` lead to additional nodes and large rehashes
  # of the tree.
  #
  # Outside of this particular scenario, the `a_int` optimization has been useful,
  # but we haven't measured benefits for `a_union`.
  defp map_leaf_difference(bdd_leaf(tag, fields), bdd_leaf(:open, [{key, v2}]), type) do
    {found?, v1} =
      case fields_find(key, fields) do
        {:ok, value} -> {true, value}
        :error -> {false, map_key_tag_to_type(tag)}
      end

    cond do
      tag == :closed and not found? and not is_optional_static(v2) ->
        :disjoint

      tag == :open and not found? and fields != [] ->
        # In case the left-side is open, we will only be adding new keys
        # to the open map, which makes future eliminations harder.
        :none

      true ->
        map_leaf_one_key_difference(tag, fields, key, v1, v2, type)
    end
  end

  defp map_leaf_difference(bdd_leaf(tag, fields), bdd_leaf(neg_tag, neg_fields), type) do
    case map_difference_strategy(fields, neg_fields, tag, neg_tag) do
      :disjoint ->
        :disjoint

      :left_subtype_of_right ->
        :subtype

      {:one_key_difference, key, v1, v2} ->
        map_leaf_one_key_difference(tag, fields, key, v1, v2, type)

      :none ->
        :none
    end
  end

  defp map_leaf_one_key_difference(tag, fields, key, v1, v2, type) do
    v_diff = difference(v1, v2)

    if empty?(v_diff) do
      :subtype
    else
      a_diff = bdd_leaf(tag, fields_store(key, v_diff, fields))

      a_type =
        case type do
          :none ->
            :bdd_bot

          :union ->
            bdd_leaf(tag, fields_store(key, union(v1, v2), fields))

          :intersection ->
            v_int = intersection(v1, v2)

            if empty?(v_int),
              do: :bdd_bot,
              else: bdd_leaf(tag, fields_store(key, v_int, fields))
        end

      {:one_key_difference, a_diff, a_type}
    end
  end

  # Intersects two map literals; throws if their intersection is empty.
  # Both open: the result is open.
  defp map_literal_intersection(:open, map1, :open, map2) do
    new_fields =
      fields_merge(
        fn _, type1, type2 -> non_empty_intersection!(type1, type2) end,
        map1,
        map2
      )

    {:open, new_fields}
  end

  # Both closed: the result is closed.
  defp map_literal_intersection(:closed, map1, :closed, map2) do
    {:closed, map_literal_intersection_closed(map1, map2)}
  end

  # Open and closed: result is closed, all fields from open should be in closed, except not_set ones.
  defp map_literal_intersection(:open, open, :closed, closed) do
    {:closed, map_literal_intersection_open_closed(open, closed)}
  end

  defp map_literal_intersection(:closed, closed, :open, open) do
    {:closed, map_literal_intersection_open_closed(open, closed)}
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
     fields_merge_with_defaults(map1, default1, map2, default2, fn _key, v1, v2 ->
       non_empty_intersection!(v1, v2)
     end)}
  end

  # Compute the intersection of two tags or tag-domain pairs.
  defp map_domain_intersection(:closed, _), do: :closed
  defp map_domain_intersection(_, :closed), do: :closed
  defp map_domain_intersection(:open, tag_or_domains), do: tag_or_domains
  defp map_domain_intersection(tag_or_domains, :open), do: tag_or_domains

  defp map_domain_intersection(domains1, domains2) do
    # If the explicit domains are empty, use simple atom tags
    case map_domain_intersection_fields(domains1, domains2) do
      [] -> :closed
      new_domains -> new_domains
    end
  end

  defp map_domain_intersection_fields([{k1, _} | t1], [{k2, _} | _] = l2) when k1 < k2 do
    map_domain_intersection_fields(t1, l2)
  end

  defp map_domain_intersection_fields([{k1, _} | _] = l1, [{k2, _} | t2]) when k1 > k2 do
    map_domain_intersection_fields(l1, t2)
  end

  defp map_domain_intersection_fields([{k, type1} | t1], [{_, type2} | t2]) do
    inter = intersection(type1, type2)

    if empty_or_optional?(inter) do
      map_domain_intersection_fields(t1, t2)
    else
      [{k, inter} | map_domain_intersection_fields(t1, t2)]
    end
  end

  defp map_domain_intersection_fields(_, _), do: []

  defp map_literal_intersection_open_closed([{k1, v1} | t1], [{k2, _} | _] = l2) when k1 < k2 do
    # If the type in the open map is optional, we continue
    case v1 do
      %{optional: 1} -> map_literal_intersection_open_closed(t1, l2)
      _ -> throw(:empty)
    end
  end

  defp map_literal_intersection_open_closed([{k1, _} | _] = l1, [{k2, v2} | t2]) when k1 > k2 do
    # Anything in the closed map not in open is preserved
    [{k2, v2} | map_literal_intersection_open_closed(l1, t2)]
  end

  defp map_literal_intersection_open_closed([{key, v1} | t1], [{_, v2} | t2]) do
    [{key, non_empty_intersection!(v1, v2)} | map_literal_intersection_open_closed(t1, t2)]
  end

  defp map_literal_intersection_open_closed(t1, t2) do
    if Enum.all?(t1, fn {_, v} -> match?(%{optional: 1}, v) end) do
      t2
    else
      throw(:empty)
    end
  end

  defp map_literal_intersection_closed([{k1, v1} | t1], [{k2, _} | _] = l2) when k1 < k2 do
    if v1 == @not_set do
      map_literal_intersection_closed(t1, l2)
    else
      throw(:empty)
    end
  end

  defp map_literal_intersection_closed([{k1, _} | _] = l1, [{k2, v2} | t2]) when k1 > k2 do
    if v2 == @not_set do
      map_literal_intersection_closed(l1, t2)
    else
      throw(:empty)
    end
  end

  defp map_literal_intersection_closed([{key, v1} | t1], [{_, v2} | t2]) do
    [{key, non_empty_intersection!(v1, v2)} | map_literal_intersection_closed(t1, t2)]
  end

  defp map_literal_intersection_closed(t1, t2) do
    if Enum.any?(t1, fn {_, v} -> v != @not_set end) or
         Enum.any?(t2, fn {_, v} -> v != @not_set end) do
      throw(:empty)
    end

    []
  end

  defp non_empty_intersection!(type1, type2) do
    type = intersection(type1, type2)
    if empty?(type), do: throw(:empty), else: type
  end

  defp map_bdd_to_dnf_remove_empty(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.reduce([], fn {pos, negs}, acc ->
      case non_empty_map_literals_intersection(pos) do
        :empty ->
          acc

        {tag, fields} ->
          if init_map_line_empty?(tag, fields, negs) do
            acc
          else
            [{tag, fields, negs} | acc]
          end
      end
    end)
  end

  defp map_bdd_to_dnf_with_empty(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.reduce([], fn {pos, negs}, acc ->
      case non_empty_map_literals_intersection(pos) do
        :empty -> acc
        {tag, fields} -> [{tag, fields, negs} | acc]
      end
    end)
  end

  ## Map key functions

  @doc """
  Fetches the type of the value returned by accessing `key` on `map`
  with the assumption that the descr is exclusively a map (or dynamic).

  It returns a two element tuple or `:error`. The first element says
  if the type is dynamically optional or not, the second element is
  the type. In static mode, optional keys are not allowed.

  Being dynamically optional means that the field may be present
  (while statically optional means we need to consider the field as
  both present and absent).
  """
  def map_fetch_key(:term, _key), do: :badmap

  def map_fetch_key(%{} = descr, key) when is_atom(key) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and non_empty_map_only?(descr) do
          {static_optional?, static_type} = map_fetch_key_static(descr, key)

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
          {dynamic_optional?, dynamic_type} = map_fetch_key_static(dynamic, key)
          {static_optional?, static_type} = map_fetch_key_static(static, key)

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

  # Optimization for bdd leafs
  defp map_fetch_key_static(%{map: bdd_leaf(tag, fields)}, key) do
    case fields_find(key, fields) do
      {:ok, value} -> pop_optional_static(value)
      :error when tag == :open -> {true, term()}
      :error when tag == :closed -> {true, none()}
      :error -> tag |> map_key_tag_to_type() |> pop_optional_static()
    end
  end

  defp map_fetch_key_static(%{map: bdd}, key) do
    bdd |> map_bdd_to_dnf_with_empty() |> map_dnf_fetch_static(key)
  end

  defp map_fetch_key_static(%{}, _key), do: {false, none()}
  defp map_fetch_key_static(:term, _key), do: {true, term()}

  # Takes a map DNF and returns the union of types it can take for a given key.
  # If the key may be undefined, it will contain the `not_set()` type.
  defp map_dnf_fetch_static(dnf, key) do
    Enum.reduce(dnf, none(), fn
      # Optimization: if there are no negatives
      {tag, fields, []}, acc ->
        case fields_find(key, fields) do
          {:ok, value} -> union(value, acc)
          :error when tag == :open -> throw(:open)
          :error -> map_key_tag_to_type(tag) |> union(acc)
        end

      {tag, fields, negs}, acc ->
        {value, bdd} = map_pop_key_bdd(tag, fields, key)

        negs
        |> map_split_negative_key(key, value, bdd)
        |> Enum.reduce(acc, fn {value, _}, acc -> union(value, acc) end)
    end)
  catch
    :open -> {true, term()}
  else
    value ->
      pop_optional_static(value)
  end

  defp map_split_negative_key(negs, key, value, bdd) do
    map_split_negative(negs, value, bdd, fn neg_tag, neg_fields ->
      case fields_take(key, neg_fields) do
        {neg_value, neg_fields} -> {true, neg_value, map_new(neg_tag, neg_fields)}
        :error -> {false, map_key_tag_to_type(neg_tag), map_new(neg_tag, neg_fields)}
      end
    end)
  end

  # Remove negatives:
  # {t, s} \ {t₁, s₁} = {t \ t₁, s} ∪ {t ∩ t₁, s \ s₁}
  defp map_split_negative(negs, value, bdd, take_fun) do
    Enum.reduce(negs, [{value, bdd}], fn
      {:open, empty}, _acc when is_fields_empty(empty) ->
        throw(:empty)

      {neg_tag, neg_fields}, acc ->
        {found?, neg_value, neg_bdd} = take_fun.(neg_tag, neg_fields)

        if not found? and neg_tag == :open do
          # In case the map is open, t \ t₁ is always empty,
          # t ∩ t₁ is always t, so we just need to deal with the bdd.
          Enum.reduce(acc, [], fn {value, bdd}, acc ->
            diff_bdd = map_difference(bdd, neg_bdd)

            if map_empty?(diff_bdd) do
              acc
            else
              [{value, diff_bdd} | acc]
            end
          end)
        else
          Enum.reduce(acc, [], fn {value, bdd}, acc ->
            # If the negative tag is closed, then they are likely disjoint,
            # so we can drastically cut down the amount of operations.
            if neg_tag == :closed and map_empty?(map_intersection(bdd, neg_bdd)) do
              [{value, bdd} | acc]
            else
              intersection_value = intersection(value, neg_value)

              if empty?(intersection_value) do
                [{value, bdd} | acc]
              else
                diff_bdd = map_difference(bdd, neg_bdd)

                if map_empty?(diff_bdd) do
                  prepend_pair_unless_empty_diff(value, neg_value, bdd, acc)
                else
                  acc = [{intersection_value, diff_bdd} | acc]
                  prepend_pair_unless_empty_diff(value, neg_value, bdd, acc)
                end
              end
            end
          end)
        end
    end)
  catch
    :empty -> []
  end

  defp map_pop_key_bdd(tag, fields, key) do
    case fields_take(key, fields) do
      {value, fields} -> {value, map_new(tag, fields)}
      :error -> {map_key_tag_to_type(tag), map_new(tag, fields)}
    end
  end

  defp prepend_pair_unless_empty_diff(value, neg_value, bdd, acc) do
    diff_value = difference(value, neg_value)
    if empty?(diff_value), do: acc, else: [{diff_value, bdd} | acc]
  end

  @doc """
  Returns the map converted into a list.
  """
  def map_to_list(descr, fun \\ &tuple([&1, &2]))

  def map_to_list(:term, _fun), do: :badmap

  def map_to_list(descr, fun) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if map_only?(descr) do
          map_to_list_static(descr, fun)
        else
          :badmap
        end

      {dynamic, static} ->
        if map_only?(static) do
          with {:ok, dynamic_type} <- map_to_list_static(dynamic, fun) do
            if descr_key?(static, :map) do
              with {:ok, static_type} <- map_to_list_static(static, fun) do
                {:ok, union(static_type, dynamic(dynamic_type))}
              end
            else
              {:ok, dynamic(dynamic_type)}
            end
          end
        else
          :badmap
        end
    end
  end

  defp map_to_list_static(%{map: bdd}, fun) do
    case map_bdd_to_dnf_remove_empty(bdd) do
      [] ->
        :badmap

      dnf ->
        case map_to_list_static(bdd, dnf, fun) do
          value when value == @none ->
            {:ok, empty_list()}

          inner ->
            if has_empty_map?(dnf) do
              {:ok, list(inner)}
            else
              {:ok, non_empty_list(inner)}
            end
        end
    end
  end

  defp map_to_list_static(%{}, _fun) do
    :badmap
  end

  defp map_to_list_static(:term, fun) do
    {:ok, list(fun.(term(), term()))}
  end

  defp has_empty_map?(dnf) do
    Enum.any?(dnf, fn {_, fields, negs} ->
      Enum.all?(fields_to_list(fields), fn {_key, value} -> is_optional_static(value) end) and
        Enum.all?(negs, fn {_, fields} ->
          not Enum.all?(fields_to_list(fields), fn {_key, value} -> is_optional_static(value) end)
        end)
    end)
  end

  defp map_to_list_static(bdd, dnf, fun) do
    try do
      # Check if any line in the DNF represents an open map or compute the union of domain keys types
      Enum.reduce(dnf, none(), fn
        {tag_or_domains, _fields, _negs}, acc ->
          case tag_or_domains do
            :open ->
              # A negation cannot make an open map closed without cancelling it completely,
              # which is filtered by `map_bdd_to_dnf_*/1`.
              throw(:open)

            domains when is_list(domains) ->
              fields_fold(domains, acc, fn domain_key, value, acc ->
                value = remove_optional(value)

                if empty?(value) do
                  acc
                else
                  union(acc, fun.(domain_key_to_descr(domain_key), value))
                end
              end)

            _ ->
              acc
          end
      end)
    catch
      :open -> fun.(term(), term())
    else
      domain_keys_type ->
        {_seen, acc} =
          bdd_reduce(bdd, {%{}, domain_keys_type}, fn {_tag, fields}, seen_acc ->
            fields_fold(fields, seen_acc, fn key, _type, {seen, acc} ->
              if Map.has_key?(seen, key) do
                {seen, acc}
              else
                {_, value} = map_dnf_fetch_static(dnf, key)
                seen = Map.put(seen, key, [])

                if empty?(value) do
                  {seen, acc}
                else
                  {seen, union(acc, fun.(atom([key]), value))}
                end
              end
            end)
          end)

        acc
    end
  end

  @doc """
  Updates `key_descr` in `descr` with `type`.

  `key_descr` is split into optional and required keys and tracked accordingly.
  The gradual aspect of `key_descr` does not impact the return type.

  It returns `{type, descr, errors}`, `:badmap`, `{:error, errors}`.
  The list of `errors` may be empty, which implies a bad domain.
  The `return_type?` flag is used for optimizations purposes. If set to false,
  the returned `type` should not be used, as it will be imprecise.
  """
  def map_update(descr, key_descr, type, return_type? \\ true, force? \\ false)

  def map_update(descr, key_descr, :term, return_type?, force?),
    do: map_update_unchecked(descr, key_descr, fn _ -> :term end, return_type?, force?)

  def map_update(descr, key_descr, type, return_type?, force?) do
    case type do
      %{dynamic: dynamic} ->
        fun = fn _, _ -> dynamic end
        map_update_unchecked(dynamic(descr), key_descr, fun, return_type?, force?)

      %{} ->
        fun = fn _, _ -> type end
        map_update_unchecked(descr, key_descr, fun, return_type?, force?)
    end
  end

  @doc """
  Updates `key_descr` in `descr` with `type`.

  `key_descr` is split into optional and required keys and tracked accordingly.
  The gradual aspect of `key_descr` does not impact the return type.

  This is a more general version of `map_update/5` and has the same return values.
  However, the third argument is an anonymous function that receives the current
  value and returns `type_fun`. Note the value returned by `type_fun` cannot hold
  dynamic. Any dynamic conversion must happen before invoking this function.
  """
  def map_update_fun(descr, key_descr, type_fun, return_type? \\ true, force? \\ false) do
    gradual? = gradual?(descr)

    type_fun = fn optional?, value ->
      if is_function(type_fun, 1) do
        case type_fun.(if gradual?, do: dynamic(value), else: value) do
          %{dynamic: dynamic} -> dynamic
          descr -> descr
        end
      else
        value = if gradual?, do: dynamic(value), else: value

        case type_fun.(optional?, value) do
          %{dynamic: dynamic} -> dynamic
          descr -> descr
        end
      end
    end

    map_update_unchecked(descr, key_descr, type_fun, return_type?, force?)
  end

  def map_update_unchecked(:term, _key_descr, _type_fun, _return_type?, _force?), do: :badmap

  def map_update_unchecked(descr, key_descr, type_fun, return_type?, force?) do
    split_keys = map_split_keys_and_domains(key_descr)

    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and map_only?(descr) do
          {type, descr, errors, found?} =
            map_update_static(descr, split_keys, type_fun, return_type?, force?, true)

          if found? do
            {type, descr, errors}
          else
            {:error, errors}
          end
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          {static_value, static_descr, static_errors, _static_found?} =
            map_update_static(static, split_keys, type_fun, return_type?, force?, true)

          {dynamic_value, dynamic_descr, dynamic_errors, dynamic_found?} =
            map_update_static(dynamic, split_keys, type_fun, return_type?, force?, false)

          # We can exceptionally check for none() here because
          # we already check for empty downstream
          if dynamic_found? do
            {union(static_value, dynamic(dynamic_value)),
             union(static_descr, dynamic(dynamic_descr)), static_errors ++ dynamic_errors}
          else
            {:error, static_errors ++ dynamic_errors}
          end
        else
          :badmap
        end
    end
  end

  defp map_update_static(%{map: bdd}, split_keys, type_fun, return_type?, force?, static?) do
    {required_keys, optional_keys, maybe_negated_set, required_domains, optional_domains} =
      split_keys

    optional_keys =
      ((map_keys_from_negated_set(maybe_negated_set, bdd) -- optional_keys) -- required_keys) ++
        optional_keys

    dnf = map_bdd_to_dnf_with_empty(bdd)

    {found?, value, domains, errors} =
      if force? and not return_type? do
        {false, none(), required_domains ++ optional_domains, []}
      else
        callback =
          if return_type? do
            fn -> map_update_merge_atom_key(bdd, dnf) end
          else
            fn ->
              # If we have required keys, we can assume domain_atom always work
              if required_keys != [] or map_update_any_atom_key?(bdd, dnf) do
                term()
              else
                none()
              end
            end
          end

        # Required domains must be found
        {found_required?, matched_required_domains, missing_domains, value} =
          map_update_get_domains(dnf, required_domains, none(), return_type?, callback)

        # Optional domains can be missing
        {found_optional?, matched_optional_domains, _, value} =
          map_update_get_domains(dnf, optional_domains, value, return_type?, callback)

        errors = Enum.map(missing_domains, &{:baddomain, domain_key_to_descr(&1)})

        domains =
          if force?,
            do: required_domains ++ optional_domains,
            else: matched_required_domains ++ matched_optional_domains

        {found_required? or found_optional?, value, domains, errors}
      end

    acc =
      if found? or (force? and domains != []) do
        # If any of required or optional domains are satisfied, then we compute the
        # initial return type. `map_update_keys_static` will then union into the
        # computed type below, using the original bdd/dnf, not the one with updated domains.
        descr = map_update_put_domains(bdd, domains, type_fun)
        {remove_optional(value), descr, errors, true}
      else
        {remove_optional(value), none(), errors, false}
      end

    map_update_keys_static(dnf, required_keys, optional_keys, type_fun, force?, static?, acc)
  end

  defp map_update_static(%{}, _split_keys, _type_fun, _return_type?, _force?, _static?) do
    {none(), none(), [], false}
  end

  defp map_update_static(:term, split_keys, type_fun, _return_type?, force?, static?) do
    # Since it is an open map, we don't need to check the domains.
    # The negated set will also be empty, because there are no fields.
    # Finally, merged required_keys into optional_keys.
    {required_keys, optional_keys, _maybe_negated_set, required_domains, optional_domains} =
      split_keys

    if required_domains != [] or optional_domains != [] do
      {term(), open_map(), [], true}
    else
      acc = {none(), none(), [], false}
      dnf = map_bdd_to_dnf_with_empty(@map_top)
      map_update_keys_static(dnf, required_keys, optional_keys, type_fun, force?, static?, acc)
    end
  end

  defp map_update_keys_static(dnf, required, optional, type_fun, force?, static?, acc) do
    acc = map_update_keys(dnf, required, type_fun, true, force?, static?, acc)
    acc = map_update_keys(dnf, optional, type_fun, false, force?, static?, acc)
    acc
  end

  defp map_update_keys(dnf, keys, type_fun, required_key?, force?, static?, acc) do
    Enum.reduce(keys, acc, fn key, {acc_value, acc_descr, acc_errors, acc_found?} ->
      {{optional?, value}, descr} =
        case dnf do
          # Optimization: avoid creating term types when updating open maps
          [{:open, fields, []}] ->
            if fields_is_key(key, fields) do
              {value, descr} = map_dnf_pop_key_static(dnf, key, none())
              {pop_optional_static(value), descr}
            else
              {{true, term()}, %{map: map_new(:open, fields)}}
            end

          _ ->
            {value, descr} = map_dnf_pop_key_static(dnf, key, none())
            {pop_optional_static(value), descr}
        end

      if not force? and empty?(value) do
        acc_errors = if required_key?, do: [{:badkey, key} | acc_errors], else: acc_errors
        {acc_value, acc_descr, acc_errors, acc_found?}
      else
        acc_value = union(value, acc_value)
        acc_descr = union(map_put_key_static(descr, key, type_fun.(optional?, value)), acc_descr)

        # The field will be missing if we are not forcing,
        # we are in static mode and the value is optional.
        missing? = not force? and static? and optional?

        if required_key? and missing? do
          {acc_value, acc_descr, [{:badkey, key} | acc_errors], acc_found?}
        else
          {acc_value, acc_descr, acc_errors, acc_found? or not missing?}
        end
      end
    end)
  end

  # Directly inserts a key of a given type into every positive and negative map.
  defp map_put_key_static(%{map: bdd} = descr, key, type) do
    bdd =
      bdd_map(bdd, fn
        {:closed, fields} when type == @not_set -> {:closed, fields}
        {tag, fields} -> {tag, fields_store(key, type, fields)}
      end)

    %{descr | map: bdd}
  end

  defp map_put_key_static(descr, _key, _type), do: descr

  # Removes a key from a map type and return its type.
  #
  # ## Algorithm
  #
  # 1. Split the map type based on the presence of the key.
  # 2. Take the second part of the split, which represents the union of all
  #    record types where the key has been explicitly removed.
  # 3. Intersect this with an open record type where the key is explicitly absent.
  #    This step eliminates the key from open record types where it was implicitly present.
  #
  # Note: if initial is nil, it means the value is not required.
  # So we don't compute it for performance.
  defp map_dnf_pop_key_static(dnf, key, initial) do
    {value, bdd} =
      Enum.reduce(dnf, {initial, :bdd_bot}, fn
        # Optimization: if there are no negatives, we can directly remove the key.
        {tag, fields, []}, {value, bdd} ->
          {fst, snd} = map_pop_key_bdd(tag, fields, key)
          {maybe_union(value, fn -> fst end), map_union(bdd, snd)}

        {tag, fields, negs}, {value, bdd} ->
          {fst, snd} = map_pop_key_bdd(tag, fields, key)
          pairs = map_split_negative_key(negs, key, fst, snd)

          {maybe_union(value, fn -> Enum.reduce(pairs, none(), &union(elem(&1, 0), &2)) end),
           Enum.reduce(pairs, bdd, &map_union(elem(&1, 1), &2))}
      end)

    if bdd == :bdd_bot do
      {value, %{}}
    else
      {value, %{map: bdd}}
    end
  end

  defp map_update_merge_atom_key(bdd, dnf) do
    {_seen, acc} =
      bdd_reduce(bdd, {%{}, none()}, fn {_tag, fields}, seen_acc ->
        fields_fold(fields, seen_acc, fn key, _type, {seen, acc} ->
          if Map.has_key?(seen, key) do
            {seen, acc}
          else
            {_, value} = map_dnf_fetch_static(dnf, key)
            {Map.put(seen, key, []), union(acc, value)}
          end
        end)
      end)

    acc
  end

  defp map_update_any_atom_key?(bdd, dnf) do
    bdd_reduce(bdd, %{}, fn {_tag, fields}, acc ->
      fields_fold(fields, acc, fn key, _type, acc ->
        if Map.has_key?(acc, key) do
          acc
        else
          {_, value} = map_dnf_fetch_static(dnf, key)
          not empty?(value) and throw(:found_key)
          Map.put(acc, key, [])
        end
      end)
    end)
  catch
    :found_key -> true
  end

  defp map_update_get_domains(dnf, domain_keys, acc, require_type?, any_atom_key) do
    Enum.reduce(domain_keys, {false, [], [], acc}, fn domain_key, {found?, valid, invalid, acc} ->
      value = map_get_domain_no_optional(dnf, domain_key, none())

      cond do
        domain_key == :atom ->
          atom_acc = any_atom_key.()

          cond do
            not empty?(value) ->
              acc = if require_type?, do: union(union(atom_acc, acc), value), else: acc
              {true, [:atom | valid], invalid, acc}

            not empty?(atom_acc) ->
              acc = if require_type?, do: union(atom_acc, acc), else: acc
              {true, valid, [:atom | invalid], acc}

            true ->
              {found?, valid, [:atom | invalid], acc}
          end

        not empty?(value) ->
          acc = if require_type?, do: union(acc, value), else: acc
          {true, [domain_key | valid], invalid, acc}

        true ->
          {found?, valid, [domain_key | invalid], acc}
      end
    end)
  end

  # For negations, we count on the idea that a negation will not remove any
  # type from a domain unless it completely cancels out the type.
  #
  # So for any non-empty map bdd, we just update the domain with the new type,
  # as well as its negations to keep them accurate.
  #
  # Note we store all domain_keys at once. Therefore, this operation:
  #
  #    map = %{integer() => if_set(:foo), float() => if_set(:bar)}
  #    Map.put(map, integer() or float(), pid())
  #
  # will return:
  #
  #    %{integer() => if_set(:foo or pid()), float() => if_set(:bar or pid())}
  #
  # We could instead have returned:
  #
  #    %{integer() => if_set(:foo or pid()), float() => if_set(:bar)} or
  #      %{integer() => if_set(:foo), float() => if_set(:bar or pid())}
  #
  # But that would not be helpful, as we can't distinguish between these two
  # in Elixir code. It only makes sense to build the union for domain keys
  # that do not exist.
  defp map_update_put_domains(bdd, [], _type_fun), do: %{map: bdd}

  defp map_update_put_domains(bdd, domain_keys, type_fun) do
    bdd =
      bdd_map(bdd, fn {tag, fields} ->
        {map_update_put_domain(tag, domain_keys, type_fun), fields}
      end)

    %{map: bdd}
  end

  defp map_update_put_domain(tag_or_domains, domain_keys, type_fun) do
    case tag_or_domains do
      :open ->
        :open

      :closed ->
        fields_from_keys(domain_keys, if_set(type_fun.(true, none())))

      # Note: domain_keys may contain duplicates, so we cannot
      # do a side-by-side traversal here.
      domains when is_list(domains) ->
        Enum.reduce(domain_keys, domains, fn domain_key, acc ->
          case fields_find(domain_key, acc) do
            {:ok, value} ->
              fields_store(domain_key, union(value, type_fun.(true, remove_optional(value))), acc)

            :error ->
              fields_store(domain_key, if_set(type_fun.(true, none())), acc)
          end
        end)
    end
  end

  @doc """
  Puts a static key into `descr`.

  Shortcut around `map_put/3`.
  """
  def map_put_key(:term, key, _) when is_atom(key),
    do: :badmap

  def map_put_key(descr, key, type) when is_atom(key),
    do: map_put_shared(descr, {[key], [], nil, [], []}, type)

  @doc """
  Puts the `key_descr` with `type`.

  `key_descr` is split into optional and required keys and tracked accordingly.

  Returns `{:ok, descr}` or `:badmap`.
  """
  def map_put(:term, _, _), do: :badmap

  def map_put(descr, key_descr, type) do
    if key_descr in [:term, %{dynamic: :term}] and type in [:term, %{dynamic: :term}] do
      {:ok, if(gradual?(type) or gradual?(descr), do: dynamic(open_map()), else: open_map())}
    else
      map_put_shared(descr, map_split_keys_and_domains(key_descr), type)
    end
  end

  defp map_put_shared(%{} = descr, split_keys, :term),
    do: map_put_static_value(descr, split_keys, :term)

  defp map_put_shared(%{} = descr, split_keys, type) do
    case :maps.take(:dynamic, type) do
      :error -> map_put_static_value(descr, split_keys, type)
      {dynamic, _static} -> map_put_static_value(dynamic(descr), split_keys, dynamic)
    end
  end

  defp map_put_static_value(descr, split_keys, type) do
    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and map_only?(descr) do
          {:ok, map_put_static(descr, split_keys, type)}
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          static_descr = map_put_static(static, split_keys, type)
          dynamic_descr = map_put_static(dynamic, split_keys, type)
          {:ok, union(static_descr, dynamic(dynamic_descr))}
        else
          :badmap
        end
    end
  end

  defp map_put_static(%{map: bdd}, split_keys, type) do
    {required_keys, optional_keys, maybe_negated_set, required_domains, optional_domains} =
      split_keys

    optional_keys =
      ((map_keys_from_negated_set(maybe_negated_set, bdd) -- optional_keys) -- required_keys) ++
        optional_keys

    type_fun = fn _, _ -> type end

    descr =
      case required_domains ++ optional_domains do
        [] -> none()
        domains -> map_update_put_domains(bdd, domains, type_fun)
      end

    dnf = map_bdd_to_dnf_with_empty(bdd)
    map_put_keys_static(dnf, required_keys ++ optional_keys, type, descr)
  end

  defp map_put_static(%{}, _split_keys, _type) do
    none()
  end

  defp map_put_static(:term, split_keys, type) do
    # Since it is an open map, we don't need to check the domains.
    # The negated set will also be empty, because there are no fields.
    # Finally, merged required_keys into optional_keys.
    {required_keys, optional_keys, _maybe_negated_set, required_domains, optional_domains} =
      split_keys

    if required_domains != [] or optional_domains != [] do
      open_map()
    else
      dnf = map_bdd_to_dnf_with_empty(@map_top)
      map_put_keys_static(dnf, required_keys ++ optional_keys, type, none())
    end
  end

  defp map_put_keys_static(dnf, keys, type, acc) do
    Enum.reduce(keys, acc, fn key, acc ->
      {nil, descr} = map_dnf_pop_key_static(dnf, key, nil)
      union(map_put_key_static(descr, key, type), acc)
    end)
  end

  @doc """
  Computes the union of types for keys matching `key_type` within the `map_type`.

  Returns `{:ok, descr}`, `:error` (if no value across the whole domain is found),
  or `:badmap`.

  This is called `map_get/2` but it can be used to power `Map.fetch`, `Map.fetch!`,
  `Map.get`, etc. except `map.key`.
  """
  def map_get(:term, _key_descr), do: :badmap

  def map_get(%{} = descr, key_descr) do
    split_keys = map_split_keys_and_domains(key_descr)

    case :maps.take(:dynamic, descr) do
      :error ->
        if descr_key?(descr, :map) and map_only?(descr) do
          type_selected = map_get_static(descr, split_keys)

          if empty?(type_selected) do
            :error
          else
            {:ok, type_selected}
          end
        else
          :badmap
        end

      {dynamic, static} ->
        if descr_key?(dynamic, :map) and map_only?(static) do
          static_type = map_get_static(static, split_keys)
          dynamic_type = map_get_static(dynamic, split_keys)

          if empty?(dynamic_type) do
            :error
          else
            {:ok, union(dynamic(dynamic_type), static_type)}
          end
        else
          :badmap
        end
    end
  end

  defp map_get_static(%{map: bdd}, split_keys) do
    {required_keys, optional_keys, maybe_negated_set, required_domains, optional_domains} =
      split_keys

    dnf = map_bdd_to_dnf_with_empty(bdd)

    acc = none()
    acc = map_get_keys(dnf, required_keys, acc)
    acc = map_get_keys(dnf, optional_keys, acc)
    acc = map_get_keys(dnf, map_keys_from_negated_set(maybe_negated_set, bdd), acc)
    acc = Enum.reduce(required_domains, acc, &map_get_domain_no_optional(dnf, &1, &2))
    acc = Enum.reduce(optional_domains, acc, &map_get_domain_no_optional(dnf, &1, &2))
    remove_optional(acc)
  end

  defp map_get_static(%{}, _split_keys), do: none()
  defp map_get_static(:term, _split_keys), do: term()

  defp map_get_keys(dnf, keys, acc) do
    Enum.reduce(keys, acc, fn atom, acc ->
      {_, value} = map_dnf_fetch_static(dnf, atom)
      union(value, acc)
    end)
  end

  # Take a map bdd and return the union of types for the given key domain.
  # Notice this already removes the optional field from the domain.
  defp map_get_domain_no_optional(dnf, domain_key, acc) when is_atom(domain_key) do
    Enum.reduce(dnf, acc, fn
      # Optimization: if there are no negatives, get the domain tag directly
      {tag, _fields, []}, acc ->
        map_domain_tag_to_type(tag, domain_key) |> union(acc)

      {tag_or_domains, fields, negs}, acc ->
        {_found, value, bdd} = map_pop_domain_bdd(tag_or_domains, fields, domain_key)

        negs
        |> map_split_negative(value, bdd, fn neg_tag, neg_fields ->
          map_pop_domain_bdd(neg_tag, neg_fields, domain_key)
        end)
        |> Enum.reduce(acc, fn {value, _}, acc -> union(value, acc) end)
    end)
  end

  defp map_keys_from_negated_set(nil, _bdd), do: []

  defp map_keys_from_negated_set(set, bdd) do
    bdd
    |> bdd_reduce(%{}, fn {_, fields}, acc ->
      fields_fold(fields, acc, fn atom, _, acc ->
        if :sets.is_element(atom, set), do: acc, else: Map.put(acc, atom, true)
      end)
    end)
    |> Map.keys()
  end

  # Compute which keys are optional, which ones are required, as well as domain keys
  defp map_split_keys_and_domains(%{dynamic: dynamic} = static) do
    {required_keys, optional_keys, maybe_negated_set} =
      case {static, unfold(dynamic)} do
        {%{atom: {:union, static_union}}, %{atom: {:union, dynamic_union}}} ->
          # The static union is required, extract them from optional
          {:sets.to_list(static_union),
           :sets.to_list(:sets.subtract(dynamic_union, static_union)), nil}

        {%{atom: {:union, static_union}}, %{atom: {:negation, dynamic_negation}}} ->
          # The static union will already be checked, merge them into the negation
          {:sets.to_list(static_union), [], :sets.union(dynamic_negation, static_union)}

        {%{atom: {:union, static_union}}, _} ->
          {:sets.to_list(static_union), [], nil}

        {%{atom: {:negation, static_negation}}, %{atom: {:union, dynamic_union}}} ->
          # The dynamic union will already be checked, merge them into the negation
          {[], :sets.to_list(dynamic_union), :sets.union(static_negation, dynamic_union)}

        {%{atom: {:negation, static_negation}}, %{atom: {:negation, dynamic_negation}}} ->
          {[], [], :sets.union(dynamic_negation, static_negation)}

        {%{}, %{atom: {:union, dynamic_union}}} ->
          {[], :sets.to_list(dynamic_union), nil}

        {%{}, %{atom: {:negation, dynamic_negation}}} ->
          {[], [], dynamic_negation}

        {%{}, %{}} ->
          {[], [], nil}
      end

    required_domains = to_domain_keys(Map.delete(static, :dynamic))
    optional_domains = to_domain_keys(dynamic) -- required_domains
    {required_keys, optional_keys, maybe_negated_set, required_domains, optional_domains}
  end

  defp map_split_keys_and_domains(%{atom: {:union, atoms}} = key_descr) do
    {:sets.to_list(atoms), [], nil, to_domain_keys(key_descr), []}
  end

  defp map_split_keys_and_domains(%{atom: {:negation, atoms}} = key_descr) do
    {[], [], atoms, to_domain_keys(key_descr), []}
  end

  defp map_split_keys_and_domains(key_descr) do
    {[], [], nil, to_domain_keys(key_descr), []}
  end

  defp non_empty_map_literals_intersection(maps) do
    try do
      Enum.reduce(maps, {:open, @fields_new}, fn {next_tag, next_fields}, {tag, fields} ->
        map_literal_intersection(tag, fields, next_tag, next_fields)
      end)
    catch
      :empty -> :empty
    end
  end

  # Short-circuits if it finds a non-empty map literal in the union.
  # Since the algorithm is recursive, we implement the short-circuiting
  # as throw/catch.
  defp map_empty?(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.all?(fn {pos, negs} ->
      case non_empty_map_literals_intersection(pos) do
        :empty ->
          true

        {tag, fields} ->
          # We check the emptiness of the fields because non_empty_map_literal_intersection
          # will not return :empty on fields that are set to none() and that exist
          # just in one map, but not the other.
          init_map_line_empty?(tag, fields, negs)
      end
    end)
  end

  defp init_map_line_empty?(tag, fields, negs) do
    Enum.any?(fields_to_list(fields), fn {_key, type} -> empty?(type) end) or
      map_line_empty?(tag, fields, negs)
  end

  # These positives get checked once when calling init_map_line_empty?, and then every time
  # an intersection or difference is computed, its emptiness is checked again.
  # So they are all necessarily non-empty.
  defp map_line_empty?(_, _pos, []), do: false
  defp map_line_empty?(_, _, [{:open, neg_fields} | _]) when is_fields_empty(neg_fields), do: true
  defp map_line_empty?(:open, fs, [{:closed, _} | negs]), do: map_line_empty?(:open, fs, negs)

  defp map_line_empty?(tag, fields, [{neg_tag, neg_fields} | negs]) do
    if map_check_domain_keys?(tag, neg_tag) do
      if tag == :closed or neg_tag == :open do
        # This implements the same map line check as tuples
        map_line_meet_empty?(fields, neg_fields, tag, neg_tag, [], negs)
      else
        map_line_fields_empty?(fields, neg_fields, tag, neg_tag, fields, negs)
      end
    else
      map_line_empty?(tag, fields, negs)
    end
  catch
    :closed -> map_line_empty?(tag, fields, negs)
  end

  defp map_line_meet_empty?([{k1, v1} | t1], [{k2, _} | _] = l2, tag, neg_tag, acc_meet, negs)
       when k1 < k2 do
    cond do
      # The key is only in the positive map, which means the difference
      # with a negative open tag (all possible types) tag will surely be empty.
      neg_tag == :open ->
        map_line_meet_empty?(t1, l2, tag, neg_tag, [{k1, v1} | acc_meet], negs)

      # In this case the difference will never be empty, so we can skip ahead.
      neg_tag == :closed and not is_optional_static(v1) ->
        throw(:closed)

      true ->
        v2 = map_key_tag_to_type(neg_tag)
        map_line_meet_empty?(k1, v1, v2, t1, l2, tag, neg_tag, acc_meet, negs)
    end
  end

  defp map_line_meet_empty?([{k1, _} | _] = l1, [{k2, v2} | t2], tag, neg_tag, acc_meet, negs)
       when k1 > k2 do
    # The keys is only in the negative map and the positive map is closed,
    # in that case, this field is not_set(), and its difference with the
    # negative map type is empty iff the negative type is optional.
    if tag == :closed and not is_optional_static(v2) do
      throw(:closed)
    else
      v1 = map_key_tag_to_type(tag)
      map_line_meet_empty?(k2, v1, v2, l1, t2, tag, neg_tag, acc_meet, negs)
    end
  end

  defp map_line_meet_empty?([{k, v1} | t1], [{_, v2} | t2], tag, neg_tag, acc_meet, negs) do
    map_line_meet_empty?(k, v1, v2, t1, t2, tag, neg_tag, acc_meet, negs)
  end

  defp map_line_meet_empty?([{k1, v1} | t1], [], tag, neg_tag, acc_meet, negs) do
    v2 = map_key_tag_to_type(neg_tag)
    map_line_meet_empty?(k1, v1, v2, t1, [], tag, neg_tag, acc_meet, negs)
  end

  defp map_line_meet_empty?([], [{k2, v2} | t2], tag, neg_tag, acc_meet, negs) do
    v1 = map_key_tag_to_type(tag)
    map_line_meet_empty?(k2, v1, v2, [], t2, tag, neg_tag, acc_meet, negs)
  end

  defp map_line_meet_empty?([], [], _tag, _neg_tag, _acc_meet, _negs) do
    true
  end

  defp map_line_meet_empty?(key, type, neg_type, t1, t2, tag, neg_tag, acc_meet, negs) do
    diff = difference(type, neg_type)
    meet = intersection(type, neg_type)

    (empty?(diff) or map_line_empty?(tag, Enum.reverse(acc_meet, [{key, diff} | t1]), negs)) and
      (empty?(meet) or map_line_meet_empty?(t1, t2, tag, neg_tag, [{key, meet} | acc_meet], negs))
  end

  defp map_line_fields_empty?([{k1, v1} | t1], [{k2, _} | _] = l2, tag, neg_tag, fields, negs)
       when k1 < k2 do
    cond do
      # The key is only in the positive map, which means the difference
      # with a negative open tag (all possible types) tag will surely be empty.
      neg_tag == :open ->
        map_line_fields_empty?(t1, l2, tag, neg_tag, fields, negs)

      # In this case the difference will never be empty, so we can skip ahead.
      neg_tag == :closed and not is_optional_static(v1) ->
        throw(:closed)

      true ->
        map_line_fields_empty_recur?(k1, v1, map_key_tag_to_type(neg_tag), tag, fields, negs) and
          map_line_fields_empty?(t1, l2, tag, neg_tag, fields, negs)
    end
  end

  defp map_line_fields_empty?([{k1, _} | _] = l1, [{k2, v2} | t2], tag, neg_tag, fields, negs)
       when k1 > k2 do
    # The keys is only in the negative map and the positive map is closed,
    # in that case, this field is not_set(), and its difference with the
    # negative map type is empty iff the negative type is optional.
    if tag == :closed do
      if is_optional_static(v2) do
        map_line_fields_empty?(l1, t2, tag, neg_tag, fields, negs)
      else
        throw(:closed)
      end
    else
      map_line_fields_empty_recur?(k2, map_key_tag_to_type(tag), v2, tag, fields, negs) and
        map_line_fields_empty?(l1, t2, tag, neg_tag, fields, negs)
    end
  end

  defp map_line_fields_empty?([{key, v1} | t1], [{_, v2} | t2], tag, neg_tag, fields, negs) do
    map_line_fields_empty_recur?(key, v1, v2, tag, fields, negs) and
      map_line_fields_empty?(t1, t2, tag, neg_tag, fields, negs)
  end

  defp map_line_fields_empty?(t1, t2, tag, neg_tag, fields, negs) do
    Enum.all?(t1, fn {key, v1} ->
      map_line_fields_empty_recur?(key, v1, map_key_tag_to_type(neg_tag), tag, fields, negs)
    end) and
      Enum.all?(t2, fn {key, v2} ->
        map_line_fields_empty_recur?(key, map_key_tag_to_type(tag), v2, tag, fields, negs)
      end)
  end

  defp map_line_fields_empty_recur?(key, v1, v2, tag, fields, negs) do
    diff = difference(v1, v2)
    empty?(diff) or map_line_empty?(tag, fields_store(key, diff, fields), negs)
  end

  # Verify the domain condition from equation (22) in paper ICFP'23 https://www.irif.fr/~gc/papers/icfp23.pdf
  # which is that every domain key type in the positive map is a subtype
  # of the corresponding domain key type in the negative map.
  defp map_check_domain_keys?(:closed, _), do: true
  defp map_check_domain_keys?(_, :open), do: true

  # An open map is a subtype iff the negative domains are all present as term_or_optional()
  defp map_check_domain_keys?(:open, neg_domains) do
    fields_size(neg_domains) == length(@domain_key_types) and
      Enum.all?(fields_to_list(neg_domains), fn {_domain_key, type} ->
        subtype?(term_or_optional(), type)
      end)
  end

  # A positive domains is smaller than a closed map iff all its keys are empty or optional
  defp map_check_domain_keys?(pos_domains, :closed) do
    Enum.all?(fields_to_list(pos_domains), fn {_domain_key, type} -> empty_or_optional?(type) end)
  end

  # Component-wise comparison of domains
  defp map_check_domain_keys?(pos_domains, neg_domains) do
    Enum.all?(fields_to_list(pos_domains), fn {domain_key, type} ->
      subtype?(type, fields_get(neg_domains, domain_key, not_set()))
    end)
  end

  # Pop a domain type, already removing non optional.
  defp map_pop_domain_bdd(domains, fields, domain_key) when is_list(domains) do
    case fields_take(domain_key, domains) do
      {value, domains} -> {true, value, map_new(domains, fields)}
      :error -> {false, none(), map_new(domains, fields)}
    end
  end

  # Open/close key
  defp map_pop_domain_bdd(tag, fields, _domain_key),
    do: {false, map_domain_tag_to_type(tag), map_new(tag, fields)}

  # Continue to eliminate negations while length of list of negs decreases
  defp map_eliminate_while_negs_decrease(tag, fields, []), do: [{tag, fields, []}]

  defp map_eliminate_while_negs_decrease(tag, fields, negs) do
    try do
      maybe_eliminate_map_negations(tag, fields, negs)
    catch
      :empty -> []
    else
      {fields, new_negs} ->
        if length(new_negs) < length(negs) do
          map_eliminate_while_negs_decrease(tag, fields, new_negs)
        else
          [{tag, fields, new_negs}]
        end
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
        case map_difference_strategy(acc_fields, neg_fields, tag, neg_tag) do
          {:one_key_difference, key, v1, v2} ->
            {fields_store(key, difference(v1, v2), acc_fields), acc_negs}

          :disjoint ->
            {acc_fields, acc_negs}

          :left_subtype_of_right ->
            throw(:empty)

          :none ->
            {acc_fields, [neg | acc_negs]}
        end
      end
    end)
  end

  defp map_difference_strategy(fields1, fields2, tag1, tag2) do
    if is_atom(tag1) and is_atom(tag2) do
      status = if tag1 == tag2 or tag2 == :open, do: :all_equal, else: :none
      map_difference_strategy(fields1, fields2, tag1, tag2, status)
    else
      :none
    end
  end

  defp map_difference_strategy([{k1, value} | t1], [{k2, _} | _] = l2, tag1, tag2, status)
       when k1 < k2 do
    # Left side has a key the right side does not have,
    # left can only be a subtype if the right side is open.
    # If the right side is closed and the key is not optional, they are disjoint.
    case status do
      _ when tag2 == :closed ->
        if not is_optional_static(value) do
          :disjoint
        else
          map_difference_strategy(t1, l2, tag1, tag2, :none)
        end

      :all_equal ->
        map_difference_strategy(t1, l2, tag1, tag2, :left_subtype_of_right)

      {:one_key_difference, _, p1, p2} ->
        if subtype?(p1, p2),
          do: map_difference_strategy(t1, l2, tag1, tag2, :left_subtype_of_right),
          else: :none

      :left_subtype_of_right ->
        map_difference_strategy(t1, l2, tag1, tag2, :left_subtype_of_right)

      _ ->
        :none
    end
  end

  defp map_difference_strategy([{k1, _} | _] = l1, [{k2, value} | t2], tag1, tag2, _status)
       when k1 > k2 do
    # Right side has a key the left side does not have,
    # if left-side is closed, they are disjoint.
    if tag1 == :closed and not is_optional_static(value) do
      :disjoint
    else
      map_difference_strategy(l1, t2, tag1, tag2, :none)
    end
  end

  defp map_difference_strategy([{_, v} | t1], [{_, v} | t2], tag1, tag2, status) do
    # Same key and same value, nothing changes
    map_difference_strategy(t1, t2, tag1, tag2, status)
  end

  defp map_difference_strategy([{k1, v1} | t1], [{_, v2} | t2], tag1, tag2, status) do
    # They have the same key but different values
    if disjoint?(v1, v2) do
      :disjoint
    else
      case status do
        :all_equal when tag1 == tag2 ->
          map_difference_strategy(t1, t2, tag1, tag2, {:one_key_difference, k1, v1, v2})

        {:one_key_difference, _key, p1, p2} ->
          if subtype?(p1, p2) and subtype?(v1, v2) do
            map_difference_strategy(t1, t2, tag1, tag2, :left_subtype_of_right)
          else
            :none
          end

        _ ->
          if status in [:all_equal, :left_subtype_of_right] and subtype?(v1, v2),
            do: map_difference_strategy(t1, t2, tag1, tag2, :left_subtype_of_right),
            else: map_difference_strategy(t1, t2, tag1, tag2, :none)
      end
    end
  end

  defp map_difference_strategy([], [], _tag1, _tag2, status) do
    if status == :all_equal, do: :left_subtype_of_right, else: status
  end

  defp map_difference_strategy(l1, l2, tag1, tag2, status) do
    cond do
      tag2 == :open and l2 == [] ->
        case status do
          :all_equal ->
            :left_subtype_of_right

          {:one_key_difference, _, p1, p2} ->
            if subtype?(p1, p2), do: :left_subtype_of_right, else: :none

          :left_subtype_of_right ->
            :left_subtype_of_right

          :none ->
            :none
        end

      tag1 == :closed and l2 != [] and Enum.all?(l2, fn {_, v} -> not is_optional_static(v) end) ->
        :disjoint

      tag2 == :closed and l1 != [] and Enum.all?(l1, fn {_, v} -> not is_optional_static(v) end) ->
        :disjoint

      true ->
        :none
    end
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
      |> Enum.group_by(fn {tag, fields, _} -> {tag, fields_keys(fields)} end)
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
    {tag1, fields1, []} = map
    {tag2, fields2, []} = candidate

    case maybe_optimize_map_union(tag1, fields1, tag2, fields2) do
      nil -> [candidate | map_fuse_with_first_fusible(map, rest)]
      # we found a fusible candidate, we're done
      {tag, fields} -> [{tag, fields, []} | rest]
    end
  end

  defp map_to_quoted(bdd, opts) do
    bdd
    |> map_bdd_to_dnf_with_empty()
    |> Enum.flat_map(fn {tag, fields, negs} ->
      map_eliminate_while_negs_decrease(tag, fields, negs)
    end)
    |> map_fusion()
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

  defp map_literal_to_quoted({:closed, empty}, _opts) when is_fields_empty(empty) do
    {:empty_map, [], []}
  end

  defp map_literal_to_quoted({:open, empty}, _opts) when is_fields_empty(empty) do
    {:map, [], []}
  end

  defp map_literal_to_quoted({domains, empty}, _opts)
       when is_fields_empty(domains) and is_fields_empty(empty) do
    {:empty_map, [], []}
  end

  defp map_literal_to_quoted({:open, [{:__struct__, @not_atom_or_optional}]}, _opts) do
    {:non_struct_map, [], []}
  end

  defp map_literal_to_quoted({domains, fields}, opts) when is_list(domains) do
    domain_fields =
      for {domain_key, value_type} <- fields_to_list(domains) do
        non_optional = remove_optional_static(value_type)

        value_quoted =
          if empty?(non_optional) do
            {:not_set, [], []}
          else
            map_value_to_quoted(non_optional, opts)
          end

        {{domain_key, [], []}, value_quoted}
      end

    regular_fields_quoted = map_fields_to_quoted(:closed, fields, opts)
    {:%{}, [], domain_fields ++ regular_fields_quoted}
  end

  defp map_literal_to_quoted({tag, fields}, opts) do
    case tag do
      :closed ->
        with {:ok, struct_descr} <- fields_find(:__struct__, fields),
             {:finite, [struct]} <- atom_fetch(struct_descr),
             info when is_list(info) <- maybe_struct(struct),
             true <- fields_size(fields) == length(info) + 1,
             true <- Enum.all?(info, &fields_is_key(&1.field, fields)) do
          collapse? = Keyword.get(opts, :collapse_structs, true)

          fields =
            for %{field: field} <- info,
                type = fields_fetch!(field, fields),
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
            {:%{}, [], map_fields_to_quoted(tag, fields, opts)}
        end

      :open ->
        {:%{}, [], [{:..., [], nil} | map_fields_to_quoted(tag, fields, opts)]}
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

  ## Map fields helpers
  #
  # Map fields and domains are stored as orddicts (sorted key-value lists).
  # These helpers wrap :orddict operations so the representation
  # can be changed without modifying every call site.

  @compile {:inline,
            fields_from_reverse_list: 1,
            fields_from_keys: 2,
            fields_to_list: 1,
            fields_fold: 3,
            fields_keys: 1,
            fields_store: 3,
            fields_find: 2,
            fields_take: 2,
            fields_get: 3,
            fields_fetch!: 2,
            fields_is_key: 2,
            fields_merge: 3,
            fields_map: 2}

  defp fields_from_reverse_list(list), do: :lists.ukeysort(1, list)
  defp fields_from_keys(keys, value), do: Enum.map(:lists.usort(keys), &{&1, value})
  defp fields_to_list(fields), do: fields
  defp fields_fold(fields, acc, fun), do: :orddict.fold(fun, acc, fields)
  defp fields_keys(fields), do: :orddict.fetch_keys(fields)
  defp fields_store(key, value, fields), do: :orddict.store(key, value, fields)
  defp fields_find(key, fields), do: :orddict.find(key, fields)
  defp fields_take(key, fields), do: :orddict.take(key, fields)
  defp fields_fetch!(key, fields), do: :orddict.fetch(key, fields)
  defp fields_is_key(key, fields), do: :orddict.is_key(key, fields)

  defp fields_merge(fun, fields1, fields2), do: :orddict.merge(fun, fields1, fields2)
  defp fields_map(fun, fields), do: :orddict.map(fun, fields)

  defp fields_get(fields, key, default) do
    case :orddict.find(key, fields) do
      {:ok, value} -> value
      :error -> default
    end
  end

  defp fields_put_all_new(fields, [], _value), do: fields
  defp fields_put_all_new([], keys, value), do: Enum.map(keys, &{&1, value})

  defp fields_put_all_new([{k1, _} = h | t1], [k2 | _] = keys, value) when k1 < k2 do
    [h | fields_put_all_new(t1, keys, value)]
  end

  defp fields_put_all_new([{k1, _} | _] = fields, [k2 | keys], value) when k1 > k2 do
    [{k2, value} | fields_put_all_new(fields, keys, value)]
  end

  defp fields_put_all_new([h | t1], [_ | keys], value) do
    [h | fields_put_all_new(t1, keys, value)]
  end

  defp fields_merge_with_defaults([{k1, v1} | rest1] = f1, d1, [{k2, v2} | rest2] = f2, d2, fun) do
    cond do
      k1 < k2 ->
        [{k1, fun.(k1, v1, d2)} | fields_merge_with_defaults(rest1, d1, f2, d2, fun)]

      k1 > k2 ->
        [{k2, fun.(k2, d1, v2)} | fields_merge_with_defaults(f1, d1, rest2, d2, fun)]

      true ->
        [{k1, fun.(k1, v1, v2)} | fields_merge_with_defaults(rest1, d1, rest2, d2, fun)]
    end
  end

  defp fields_merge_with_defaults([], d1, f2, _d2, fun),
    do: Enum.map(f2, fn {k, v2} -> {k, fun.(k, d1, v2)} end)

  defp fields_merge_with_defaults(f1, _d1, [], d2, fun),
    do: Enum.map(f1, fn {k, v1} -> {k, fun.(k, v1, d2)} end)

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

  defp tuple_new(tag, elements), do: bdd_leaf(tag, elements)

  defp tuple_intersection(bdd_leaf(:open, []), bdd), do: bdd
  defp tuple_intersection(bdd, bdd_leaf(:open, [])), do: bdd

  defp tuple_intersection(bdd1, bdd2) do
    bdd_intersection(bdd1, bdd2, &tuple_leaf_intersection/2)
  end

  defp tuple_leaf_intersection(bdd_leaf(tag1, elements1), bdd_leaf(tag2, elements2)) do
    case tuple_literal_intersection(tag1, elements1, tag2, elements2) do
      {tag, elements} -> bdd_leaf(tag, elements)
      :empty -> :bdd_bot
    end
  end

  defp tuple_literal_intersection(:open, [], tag, elements), do: {tag, elements}

  defp tuple_literal_intersection(tag1, elements1, tag2, elements2) do
    case tuple_sizes_strategy(tag1, length(elements1), tag2, length(elements2)) do
      :disjoint ->
        :empty

      _ ->
        try do
          zip_non_empty_intersection!(elements1, elements2, [])
        catch
          :empty -> :empty
        else
          elements when tag1 == :open and tag2 == :open -> {:open, elements}
          elements -> {:closed, elements}
        end
    end
  end

  defp tuple_sizes_strategy(:closed, n1, :closed, n2) when n1 != n2, do: :disjoint
  defp tuple_sizes_strategy(:closed, n1, :closed, n2) when n1 == n2, do: :left_subtype_of_right
  defp tuple_sizes_strategy(:closed, n1, :open, n2) when n1 < n2, do: :disjoint
  defp tuple_sizes_strategy(_, n1, :open, n2) when n1 >= n2, do: :left_subtype_of_right
  defp tuple_sizes_strategy(:open, n1, :closed, n2) when n1 > n2, do: :disjoint
  defp tuple_sizes_strategy(_, _, _, _), do: :none

  # Intersects two lists of types, and _appends_ the extra elements to the result.
  defp zip_non_empty_intersection!([], types2, acc), do: Enum.reverse(acc, types2)
  defp zip_non_empty_intersection!(types1, [], acc), do: Enum.reverse(acc, types1)

  defp zip_non_empty_intersection!([type1 | rest1], [type2 | rest2], acc) do
    zip_non_empty_intersection!(rest1, rest2, [non_empty_intersection!(type1, type2) | acc])
  end

  defp zip_empty_intersection?([], _types2), do: false
  defp zip_empty_intersection?(_types1, []), do: false

  defp zip_empty_intersection?([type1 | rest1], [type2 | rest2]) do
    case empty?(intersection(type1, type2)) do
      true -> true
      false -> zip_empty_intersection?(rest1, rest2)
    end
  end

  defp tuple_difference(bdd_leaf(:open, []), bdd_leaf(:open, [])),
    do: :bdd_bot

  defp tuple_difference(bdd_leaf(:open, []), bdd2),
    do: bdd_negation(bdd2)

  defp tuple_difference(bdd1, bdd2),
    do: bdd_difference(bdd1, bdd2, &tuple_leaf_difference/3)

  defp tuple_leaf_difference(bdd_leaf(tag1, elements1), bdd_leaf(tag2, elements2), _) do
    case tuple_sizes_strategy(tag1, length(elements1), tag2, length(elements2)) do
      :disjoint -> :disjoint
      other -> tuple_leaf_difference(elements1, elements2, other == :left_subtype_of_right)
    end
  end

  defp tuple_leaf_difference([head1 | tail1], [head2 | tail2], subtype?) do
    cond do
      disjoint?(head1, head2) -> :disjoint
      subtype? and subtype?(head1, head2) -> tuple_leaf_difference(tail1, tail2, subtype?)
      true -> :none
    end
  end

  defp tuple_leaf_difference(_tail1, _tail2, subtype?) do
    if subtype?, do: :subtype, else: :none
  end

  defp non_empty_tuple_literals_intersection(tuples) do
    try do
      Enum.reduce(tuples, {:open, []}, fn {next_tag, next_elements}, {tag, elements} ->
        case tuple_literal_intersection(tag, elements, next_tag, next_elements) do
          :empty -> throw(:empty)
          next -> next
        end
      end)
    catch
      :empty -> :empty
    end
  end

  defp tuple_empty?(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.all?(fn {pos, negs} ->
      case non_empty_tuple_literals_intersection(pos) do
        :empty -> true
        {tag, fields} -> tuple_line_empty?(tag, fields, negs)
      end
    end)
  end

  # No negations, so not empty unless there's an empty type
  # Note: since the extraction from the BDD is done in a way that guarantees that
  # the elements are non-empty, we can avoid checking for empty types there.
  # Otherwise, tuple_empty?(_, elements, []) would be Enum.any?(elements, &empty?/1)
  defp tuple_line_empty?(_, _, []), do: false
  # Open empty negation makes it empty
  defp tuple_line_empty?(_, _, [{:open, []} | _]), do: true
  # Open positive can't be emptied by a single closed negative
  defp tuple_line_empty?(:open, _pos, [{:closed, _}]), do: false

  defp tuple_line_empty?(tag, elements, [{neg_tag, neg_elements} | negs]) do
    n = length(elements)
    m = length(neg_elements)

    # Scenarios where the difference is guaranteed to be empty:
    # 1. When removing larger tuples from a fixed-size positive tuple
    # 2. When removing smaller tuples from larger tuples
    if (tag == :closed and n < m) or (neg_tag == :closed and n > m) do
      tuple_line_empty?(tag, elements, negs)
    else
      tuple_elements_empty?([], tag, elements, neg_elements, negs) and
        tuple_empty_arity?(n, m, tag, elements, neg_tag, negs)
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
    (empty?(diff) or tuple_line_empty?(tag, Enum.reverse(acc_meet, [diff | elements]), negs)) and
      (empty?(meet) or tuple_elements_empty?([meet | acc_meet], tag, elements, neg_elements, negs))
  end

  # Determines if the set difference is empty when:
  # - Positive tuple: {tag, elements} of size n
  # - Negative tuple: open or closed tuples of size m
  defp tuple_empty_arity?(n, m, tag, elements, neg_tag, negs) do
    # The tuples to consider are all those of size n to m - 1, and if the negative tuple is
    # closed, we also need to consider tuples of size greater than m + 1.
    tag == :closed or
      (Enum.all?(n..(m - 1)//1, &tuple_line_empty?(:closed, tuple_fill(elements, &1), negs)) and
         (neg_tag == :open or tuple_line_empty?(:open, tuple_fill(elements, m + 1), negs)))
  end

  defp tuple_eliminate_negations(tag, elements, negs) do
    Enum.reduce(negs, [{tag, elements}], fn {neg_tag, neg_elements}, acc ->
      Enum.flat_map(acc, fn {tag, elements} ->
        tuple_eliminate_single_negation(tag, elements, {neg_tag, neg_elements})
      end)
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
         zip_empty_intersection?(elements, neg_elements) do
      [{tag, elements}]
    else
      tuple_dnf_union(
        tuple_elim_size(n, m, tag, elements, neg_tag),
        tuple_elim_content([], tag, elements, neg_elements)
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
    [ty | rest] =
      case elements do
        [] -> [term()]
        [_ | _] -> elements
      end

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

  # No more negative elements to process: there is no "all-equal" branch to add,
  # because we're constructing {t} ant not {u}, which must differ somewhere.
  defp tuple_elim_content(_acc, _tag, _elements, []) do
    []
  end

  # Eliminates negations according to size
  # Example: {integer(), ...} and not {term(), term(), ...} contains {integer()}
  # The tuples to consider are all those of size n to m - 1, and if the negative tuple is
  # closed, we also need to consider tuples of size greater than m + 1.
  defp tuple_elim_size(_, _, :closed, _, _), do: []

  defp tuple_elim_size(n, m, :open, elements, neg_tag) do
    acc =
      if neg_tag == :open do
        []
      else
        [{:open, tuple_fill(elements, m + 1)}]
      end

    Enum.reduce(n..(m - 1)//1, acc, fn i, acc ->
      [{:closed, tuple_fill(elements, i)} | acc]
    end)
  end

  # Prefer the smaller on the left
  defp tuple_dnf_union(dnf1, dnf2) do
    # Union of tuple DNFs is just concatenation,
    # but we do our best to remove duplicates.
    with [tuple1] <- dnf1,
         [tuple2] <- dnf2,
         optimized when optimized != nil <- maybe_optimize_tuple_union(tuple1, tuple2) do
      [optimized]
    else
      _ -> dnf1 ++ (dnf2 -- dnf1)
    end
  end

  defp tuple_union(
         bdd_leaf(tag1, elements1) = tuple1,
         bdd_leaf(tag2, elements2) = tuple2
       ) do
    case maybe_optimize_tuple_union({tag1, elements1}, {tag2, elements2}) do
      {tag, elements} -> bdd_leaf(tag, elements)
      nil -> bdd_union(tuple1, tuple2)
    end
  end

  @compile {:inline, tuple_union: 2}
  defp tuple_union(bdd1, bdd2), do: bdd_union(bdd1, bdd2)

  defp maybe_optimize_tuple_union({tag1, pos1} = tuple1, {tag2, pos2} = tuple2) do
    case tuple_union_strategy(tag1, pos1, tag2, pos2) do
      :all_equal ->
        tuple1

      {:one_index_difference, index, v1, v2} ->
        new_pos = List.replace_at(pos1, index, union(v1, v2))
        {tag1, new_pos}

      :left_subtype_of_right ->
        tuple2

      :right_subtype_of_left ->
        tuple1

      :none ->
        nil
    end
  end

  defp tuple_union_strategy(tag1, pos1, tag2, pos2) do
    case {tag1, tag2} do
      {tag, tag} when length(pos1) == length(pos2) ->
        tuple_union_strategy_index(pos1, pos2, 0, :all_equal)

      {:open, _} when length(pos1) <= length(pos2) ->
        tuple_union_strategy_index(pos1, pos2, 0, :right_subtype_of_left)

      {_, :open} when length(pos1) >= length(pos2) ->
        tuple_union_strategy_index(pos1, pos2, 0, :left_subtype_of_right)

      {_, _} ->
        :none
    end
  end

  defp tuple_union_strategy_index([v | pos1], [v | pos2], i, status) do
    tuple_union_strategy_index(pos1, pos2, i + 1, status)
  end

  defp tuple_union_strategy_index([v1 | pos1], [v2 | pos2], i, status) do
    case status do
      :all_equal ->
        tuple_union_strategy_index(pos1, pos2, i + 1, {:one_index_difference, i, v1, v2})

      {:one_index_difference, _, d1, d2} ->
        cond do
          subtype?(d1, d2) and subtype?(v1, v2) ->
            tuple_union_strategy_index(pos1, pos2, i + 1, :left_subtype_of_right)

          subtype?(d2, d1) and subtype?(v2, v1) ->
            tuple_union_strategy_index(pos1, pos2, i + 1, :right_subtype_of_left)

          true ->
            :none
        end

      :left_subtype_of_right ->
        if subtype?(v1, v2),
          do: tuple_union_strategy_index(pos1, pos2, i + 1, :left_subtype_of_right),
          else: :none

      :right_subtype_of_left ->
        if subtype?(v2, v1),
          do: tuple_union_strategy_index(pos1, pos2, i + 1, :right_subtype_of_left),
          else: :none
    end
  end

  defp tuple_union_strategy_index(_pos1, _pos2, _i, status) do
    status
  end

  defp tuple_to_quoted(bdd, opts) do
    tuple_bdd_to_dnf_with_negations(bdd)
    |> tuple_fusion()
    |> Enum.map(&tuple_literal_to_quoted(&1, opts))
  end

  # Transforms a bdd into a union of tuples with no negations.
  # Note: it is important to compose the results with
  # tuple_dnf_union/2 to avoid duplicates
  defp tuple_bdd_to_dnf_no_negations(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.reduce([], fn {pos, negs}, acc ->
      case non_empty_tuple_literals_intersection(pos) do
        :empty ->
          acc

        {tag, elements} ->
          if tuple_line_empty?(tag, elements, negs) do
            acc
          else
            tuple_eliminate_negations(tag, elements, negs) |> tuple_dnf_union(acc)
          end
      end
    end)
  end

  defp tuple_bdd_to_dnf_with_negations(bdd) do
    bdd_to_dnf(bdd)
    |> Enum.reduce([], fn {pos, negs}, acc ->
      case non_empty_tuple_literals_intersection(pos) do
        :empty ->
          acc

        {tag, elements} ->
          if tuple_line_empty?(tag, elements, negs) do
            acc
          else
            [{tag, elements, negs} | acc]
          end
      end
    end)
  end

  # Given a union of tuples, fuses the tuple unions when possible,
  # e.g. {integer(), atom()} or {float(), atom()} into {number(), atom()}
  # The negations of two fused tuples are just concatenated.
  #
  # Steps:
  # 1. Consider tuples without negations apart from those with
  # 2. Group tuples by size and tag
  # 3. Try fusions for each group until no fusion is found
  # 4. Merge the groups back into a dnf
  defp tuple_fusion(dnf) do
    {with_negs, without_negs} =
      Enum.reduce(dnf, {[], %{}}, fn
        {tag, elements, []}, {with, without} ->
          key = {tag, length(elements)}
          value = {tag, elements}
          {with, Map.update(without, key, [value], &[value | &1])}

        triplet, {with, without} ->
          {[triplet | with], without}
      end)

    Enum.reduce(without_negs, with_negs, fn {_, tuples}, with_negs ->
      tuples
      |> Enum.reduce([], fn tuple, acc ->
        tuple_fuse_with_first_fusible(tuple, acc)
      end)
      |> Enum.reduce(with_negs, fn {tag, elements}, with_negs ->
        [{tag, elements, []} | with_negs]
      end)
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

  defp tuple_literal_to_quoted({:closed, [], []}, _opts), do: {:{}, [], []}

  defp tuple_literal_to_quoted({tag, elements, negs}, opts) do
    pos = tuple_fields_to_quoted(tag, elements, opts)

    Enum.reduce(negs, pos, fn {tag, elements}, acc ->
      {:and, [], [acc, {:not, [], [tuple_fields_to_quoted(tag, elements, opts)]}]}
    end)
  end

  defp tuple_fields_to_quoted(tag, elements, opts) do
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
      %{tuple: bdd_leaf(tag, elements)} -> tuple_fetch_element(elements, index, tag)
      %{tuple: bdd} -> tuple_bdd_fetch_static(bdd, index)
      %{} -> {false, none()}
    end
  end

  defp tuple_bdd_fetch_static(bdd, index) do
    bdd
    |> tuple_bdd_to_dnf_with_negations()
    |> Enum.reduce({false, none()}, fn
      # Optimization: if there are no negatives
      {tag, elements, []}, {acc_optional?, acc_descr} ->
        {optional?, descr} = tuple_fetch_element(elements, index, tag)
        {optional? or acc_optional?, union(descr, acc_descr)}

      {tag, elements, negs}, acc ->
        {_, value, bdd} = tuple_take_element(elements, index, tag)

        negs
        |> tuple_split_negative(index, value, bdd)
        |> Enum.reduce(acc, fn {value, _}, {acc_optional?, acc_descr} ->
          {optional?, descr} = pop_optional_static(value)
          {optional? or acc_optional?, union(descr, acc_descr)}
        end)
    end)
  catch
    :open -> {true, term()}
  end

  # Remove negatives:
  # {t, s} \ {t₁, s₁} = {t \ t₁, s} ∪ {t ∩ t₁, s \ s₁}
  defp tuple_split_negative(negs, index, value, bdd) do
    Enum.reduce(negs, [{value, bdd}], fn
      {:open, []}, _acc ->
        throw(:empty)

      {neg_tag, neg_elements}, acc ->
        {found?, neg_value, neg_bdd} = tuple_take_element(neg_elements, index, neg_tag)

        if not found? and neg_tag == :open do
          # In case the tuple is open, t \ t₁ is always empty,
          # t ∩ t₁ is always t, so we just need to deal with the bdd.
          Enum.reduce(acc, [], fn {value, bdd}, acc ->
            diff_bdd = tuple_difference(bdd, neg_bdd)

            if tuple_empty?(diff_bdd) do
              acc
            else
              [{value, diff_bdd} | acc]
            end
          end)
        else
          Enum.reduce(acc, [], fn {value, bdd}, acc ->
            # If the negative tag is closed, then they are likely disjoint,
            # so we can drastically cut down the amount of operations.
            if neg_tag == :closed and tuple_empty?(tuple_intersection(bdd, neg_bdd)) do
              [{value, bdd} | acc]
            else
              intersection_value = intersection(value, neg_value)

              if empty?(intersection_value) do
                [{value, bdd} | acc]
              else
                diff_bdd = tuple_difference(bdd, neg_bdd)

                if tuple_empty?(diff_bdd) do
                  prepend_pair_unless_empty_diff(value, neg_value, bdd, acc)
                else
                  acc = [{intersection_value, diff_bdd} | acc]
                  prepend_pair_unless_empty_diff(value, neg_value, bdd, acc)
                end
              end
            end
          end)
        end
    end)
  catch
    :empty -> []
  end

  defp tuple_fetch_element([], _, :open), do: {true, term()}
  defp tuple_fetch_element([], _, :closed), do: {true, none()}
  defp tuple_fetch_element([h | _], 0, _tag), do: {false, h}
  defp tuple_fetch_element([_ | t], i, tag), do: tuple_fetch_element(t, i - 1, tag)

  defp tuple_take_element(elements, index, tag) do
    case do_tuple_take_element(elements, index, []) do
      :error -> {false, tuple_tag_to_type(tag), tuple_new(tag, elements)}
      {value, elements} -> {true, value, tuple_new(tag, elements)}
    end
  end

  defp do_tuple_take_element([], _, _), do: :error
  defp do_tuple_take_element([h | t], 0, acc), do: {h, Enum.reverse(acc, t)}
  defp do_tuple_take_element([h | t], i, acc), do: do_tuple_take_element(t, i - 1, [h | acc])

  defp tuple_tag_to_type(:open), do: term_or_optional()
  defp tuple_tag_to_type(:closed), do: none()

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

  def bdd_union(bdd1, bdd2) do
    case {bdd1, bdd2} do
      {:bdd_top, _bdd} ->
        :bdd_top

      {_bdd, :bdd_top} ->
        :bdd_top

      {:bdd_bot, bdd} ->
        bdd

      {bdd, :bdd_bot} ->
        bdd

      _ ->
        case bdd_compare(bdd1, bdd2) do
          {:lt, {lit1, c1, u1, d1}, bdd2} ->
            {lit1, c1, bdd_union(u1, bdd2), d1}

          {:gt, bdd1, {lit2, c2, u2, d2}} ->
            {lit2, c2, bdd_union(bdd1, u2), d2}

          {:eq, {lit, c1, u1, d1}, {_, c2, u2, d2}} ->
            {lit, bdd_union(c1, c2), bdd_union(u1, u2), bdd_union(d1, d2)}

          {:eq, {lit, _, u1, d1}, _} ->
            {lit, :bdd_top, u1, d1}

          {:eq, _, {lit, _, u2, d2}} ->
            {lit, :bdd_top, u2, d2}

          {:eq, _, _} ->
            bdd1
        end
        |> case do
          {_, :bdd_top, _, :bdd_top} -> :bdd_top
          other -> other
        end
    end
  end

  def bdd_difference(bdd1, bdd2) do
    case {bdd1, bdd2} do
      {_bdd, :bdd_top} ->
        :bdd_bot

      {:bdd_bot, _bdd} ->
        :bdd_bot

      {bdd, :bdd_bot} ->
        bdd

      {:bdd_top, bdd} ->
        bdd_negation(bdd)

      _ ->
        case bdd_compare(bdd1, bdd2) do
          {:lt, {lit1, c1, u1, d1}, bdd2} ->
            {lit1, bdd_difference(c1, bdd2), bdd_difference(u1, bdd2), bdd_difference(d1, bdd2)}

          {:gt, bdd1, {lit2, c2, u2, d2}} ->
            # The proper formula is:
            #
            #     b1 and not (c2 or u2) : bdd_bot : b1 and not (d2 or u2)
            #
            # Both extremes have (b1 and not u2), so we compute it once.
            bdd1_minus_u2 = bdd_difference(bdd1, u2)
            {lit2, bdd_difference(bdd1_minus_u2, c2), :bdd_bot, bdd_difference(bdd1_minus_u2, d2)}

          {:eq, {lit, c1, u1, d1}, {_, c2, u2, d2}} ->
            # The formula is:
            # {a1, (C1 or U1) and not (C2 or U2), :bdd_bot, (D1 or U1) and not (D2 or U2)} when a1 == a2
            #
            # Constrained: (C1 and not C2 and not U2) or (U1 and not C2 and not U2)
            # Dual: (D1 and not D2 and not U2) or (U1 and not D2 and not U2)
            #
            # We can optimize the cases below.
            if u1 == :bdd_bot or u1 == u2 do
              # Constrained = (C1 and not C2 and not U2)
              # Dual = (D1 and not D2 and not U2)
              # Hence:
              {lit, bdd_difference_union(c1, c2, u2), :bdd_bot, bdd_difference_union(d1, d2, u2)}
            else
              c =
                if c2 == :bdd_top,
                  do: :bdd_bot,
                  else: bdd_difference(bdd_union(c1, u1), bdd_union(c2, u2))

              d =
                if d2 == :bdd_top,
                  do: :bdd_bot,
                  else: bdd_difference(bdd_union(d1, u1), bdd_union(d2, u2))

              {lit, c, :bdd_bot, d}
            end

          {:eq, _, {lit, c2, u2, _d2}} ->
            {lit, bdd_negation(bdd_union(c2, u2)), :bdd_bot, :bdd_bot}

          {:eq, {lit, _c1, u1, d1}, _} ->
            {lit, :bdd_bot, :bdd_bot, bdd_union(d1, u1)}

          {:eq, _, _} ->
            :bdd_bot
        end
        |> case do
          {_, :bdd_bot, u, :bdd_bot} -> u
          other -> other
        end
    end
  end

  # Version of i \ (u1 v u2) that only computes the union if i is not bottom
  defp bdd_difference_union(:bdd_bot, _u1, _u2),
    do: :bdd_bot

  defp bdd_difference_union(i, u1, u2),
    do: bdd_difference(i, bdd_union(u1, u2))

  ## Optimize differences

  defp bdd_difference(bdd_leaf(_, _) = a1, bdd_leaf(_, _) = a2, leaf_compare) do
    case leaf_compare.(a1, a2, :none) do
      :disjoint -> a1
      :subtype -> :bdd_bot
      {:one_key_difference, a_diff, _} -> a_diff
      :none -> bdd_difference(a1, a2)
    end
  end

  # We could use bdd_expand but there was a bug in earlier versions
  # of the Erlang compiler which would emit bad ,code, so we match one by one.
  defp bdd_difference({_, _, _, _} = bdd1, bdd_leaf(_, _) = a2, leaf_compare),
    do: bdd_difference(bdd1, {a2, :bdd_top, :bdd_bot, :bdd_bot}, bdd1, a2, leaf_compare)

  defp bdd_difference(bdd_leaf(_, _) = a1, {_, _, _, _} = bdd2, leaf_compare),
    do: bdd_difference({a1, :bdd_top, :bdd_bot, :bdd_bot}, bdd2, a1, bdd2, leaf_compare)

  defp bdd_difference({_, _, _, _} = bdd1, {_, _, _, _} = bdd2, leaf_compare),
    do: bdd_difference(bdd1, bdd2, bdd1, bdd2, leaf_compare)

  defp bdd_difference(bdd1, bdd2, _leaf_compare),
    do: bdd_difference(bdd1, bdd2)

  # We have two formulas for differences.
  #
  # ## When D1 = bottom
  #
  #     (((a_diff and not D2) or (a_int and not C2)) and C1 and not U2) or (U1 and not B2)
  #
  # If disjoint, a_diff is a1 and a_int is none:
  #
  #     (a1 and C1 and not D2 and not U2) or (U1 and not B2)
  #
  # If subtype, a_diff is none and a_int is a1:
  #
  #     (a1 and C1 and not C2 and not U2) or (U1 and not B2)
  #
  # In the snippet below we also enforce that C1 is top for simplicity.
  #
  # ## When C2 = top
  #
  #     ((a_diff and C1) or (U1 and not a2) or (not a_union and D1)) and not U2 and not D2
  #
  # If disjoint, a_diff is a1 and a_union is a1 or a2:
  #
  #     ((a1 and C1) or (U1 and not a2) or (not a1 and D1 and not a2)) and not U2 and not D2
  #
  # If subtype, a_diff is none and a_union is a2:
  #
  #     ((U1 and not a2) or (D1 and not D2)) and not U2 and not D2
  #
  defp bdd_difference({a1, :bdd_top, u1, :bdd_bot}, {a2, c2, u2, d2}, bdd1, bdd2, leaf_compare) do
    type = if c2 == :bdd_top, do: :none, else: :intersection

    case leaf_compare.(a1, a2, type) do
      :disjoint ->
        a1
        |> bdd_difference(d2, leaf_compare)
        |> bdd_difference(u2, leaf_compare)
        |> bdd_union(bdd_difference(u1, bdd2, leaf_compare))

      :subtype ->
        a1
        |> bdd_difference(c2, leaf_compare)
        |> bdd_difference(u2, leaf_compare)
        |> bdd_union(bdd_difference(u1, bdd2, leaf_compare))

      {:one_key_difference, a_diff, a_int} ->
        bdd_union(
          a_diff |> bdd_difference(d2, leaf_compare) |> bdd_difference(u2, leaf_compare),
          a_int |> bdd_difference(c2, leaf_compare) |> bdd_difference(u2, leaf_compare)
        )
        |> bdd_union(bdd_difference(u1, bdd2, leaf_compare))

      :none ->
        bdd_difference(bdd1, bdd2)
    end
  end

  defp bdd_difference({a1, c1, u1, d1}, {a2, :bdd_top, u2, d2}, bdd1, bdd2, leaf_compare) do
    type = if d1 == :bdd_bot, do: :none, else: :union

    case leaf_compare.(a1, a2, type) do
      :disjoint ->
        bdd_difference(u1, a2, leaf_compare)
        |> bdd_union(bdd_difference({a1, c1, :bdd_bot, d1}, a2))
        |> bdd_difference(d2, leaf_compare)
        |> bdd_difference(u2, leaf_compare)

      :subtype ->
        bdd_union(
          bdd_difference(u1, a2, leaf_compare),
          bdd_difference(d1, a2, leaf_compare)
        )
        |> bdd_difference(d2, leaf_compare)
        |> bdd_difference(u2, leaf_compare)

      {:one_key_difference, a_diff, a_union} ->
        bdd_intersection(a_diff, c1)
        |> bdd_union(bdd_difference(u1, a2, leaf_compare))
        |> bdd_union(bdd_difference(d1, a_union, leaf_compare))
        |> bdd_difference(d2, leaf_compare)
        |> bdd_difference(u2, leaf_compare)

      :none ->
        bdd_difference(bdd1, bdd2)
    end
  end

  defp bdd_difference(_, _, bdd1, bdd2, _leaf_compare) do
    bdd_difference(bdd1, bdd2)
  end

  def bdd_intersection(bdd1, bdd2) do
    case {bdd1, bdd2} do
      {:bdd_top, bdd} ->
        bdd

      {bdd, :bdd_top} ->
        bdd

      {:bdd_bot, _bdd} ->
        :bdd_bot

      {_, :bdd_bot} ->
        :bdd_bot

      _ ->
        case bdd_compare(bdd1, bdd2) do
          {:lt, {lit1, c1, u1, d1}, bdd2} ->
            {lit1, bdd_intersection(c1, bdd2), bdd_intersection(u1, bdd2),
             bdd_intersection(d1, bdd2)}

          {:gt, bdd1, {lit2, c2, u2, d2}} ->
            {lit2, bdd_intersection(bdd1, c2), bdd_intersection(bdd1, u2),
             bdd_intersection(bdd1, d2)}

          # Notice that (a, c1, u1, d1) and (a, c2, u2, d2) is described as:
          #
          #     {a, (C1 or U1) and (C2 or U2), :bdd_bot, (D1 or U1) and (D2 or U2)}
          #
          # However, if we distribute the intersection over the unions, we find a
          # common term, U1 and U2, leading to:
          #
          #     {a1,
          #      (C1 and (C2 or U2)) or (U1 and C2),
          #      (U1 and U2),
          #      (D1 and (D2 or U2)) or (U1 and D2)}
          #
          # This formula is longer, meaning more operations, but it does preserve
          # unions in place whenever possible. This change has reduced the algorithmic
          # complexity in the past, but perhaps it is rendered less useful now due to
          # the eager literal intersections.
          {:eq, {lit, c1, u1, d1}, {_, c2, u2, d2}} ->
            {lit, bdd_intersection_eq(c1, c2, u1, u2), bdd_intersection(u1, u2),
             bdd_intersection_eq(d1, d2, u1, u2)}

          {:eq, {lit, c1, u1, _}, _} ->
            {lit, bdd_union(c1, u1), :bdd_bot, :bdd_bot}

          {:eq, _, {lit, c2, u2, _}} ->
            {lit, bdd_union(c2, u2), :bdd_bot, :bdd_bot}

          {:eq, bdd, _} ->
            bdd
        end
        |> case do
          {_, :bdd_bot, u, :bdd_bot} -> u
          other -> other
        end
    end
  end

  # The arms of bdd_intersect equal have shape:
  #
  # (C1 and C2) or (C1 and U2) or (U1 and C2)
  # (D1 and D2) or (D1 and U2) or (U1 and D2)
  #
  # They are symmetrical, so we optimize it using the formula below,
  # which also deals with cases that lead to large eliminations.
  #
  # The final clause reduces the amount of operations by rewriting it to:
  # (C1 and (C2 or U2)) or (U1 and C2)
  defp bdd_intersection_eq(:bdd_top, :bdd_top, _u1, _u2), do: :bdd_top

  defp bdd_intersection_eq(:bdd_bot, cd2, u1, _u2), do: bdd_intersection(u1, cd2)
  defp bdd_intersection_eq(cd1, :bdd_bot, _u1, u2), do: bdd_intersection(u2, cd1)
  defp bdd_intersection_eq(cd1, cd2, :bdd_bot, u2), do: bdd_intersection(cd1, bdd_union(cd2, u2))
  defp bdd_intersection_eq(cd1, cd2, u1, :bdd_bot), do: bdd_intersection(cd2, bdd_union(cd1, u1))

  defp bdd_intersection_eq(cd1, cd2, u1, u2) do
    bdd_union(bdd_intersection(cd1, bdd_union(cd2, u2)), bdd_intersection(u1, cd2))
  end

  # Intersections are great because they allow us to cut down
  # the number of nodes in the tree. So whenever we have a leaf,
  # we actually propagate it throughout the whole tree, cutting
  # down nodes.
  defp bdd_intersection(bdd_leaf(_, _) = leaf, bdd, leaf_intersection) do
    bdd_leaf_intersection(leaf, bdd, leaf_intersection)
  end

  defp bdd_intersection(bdd, bdd_leaf(_, _) = leaf, leaf_intersection) do
    bdd_leaf_intersection(leaf, bdd, leaf_intersection)
  end

  # Take two BDDs, B1 = {a1, C1, U2, D2} and B2.
  # We can treat a1 as a leaf if C1 = :bdd_top.
  # Then we have:
  #
  #     ((a1 and C1) or U2 or (not a1 and D2)) and B2
  #
  # Which is equivalent to:
  #
  #     (a1 and B2) or (B2 and U2) or (B2 and not a1 and D2)
  defp bdd_intersection({leaf, :bdd_top, u, d}, bdd, leaf_intersection) do
    bdd_leaf_intersection(leaf, bdd, leaf_intersection)
    |> bdd_union(bdd_intersection(u, bdd, leaf_intersection))
    |> case do
      result when d == :bdd_bot -> result
      result -> bdd_union(result, bdd_intersection(bdd, {leaf, :bdd_bot, :bdd_bot, d}))
    end
  end

  defp bdd_intersection(bdd, {leaf, :bdd_top, u, d}, leaf_intersection) do
    bdd_leaf_intersection(leaf, bdd, leaf_intersection)
    |> bdd_union(bdd_intersection(u, bdd, leaf_intersection))
    |> case do
      result when d == :bdd_bot -> result
      result -> bdd_union(result, bdd_intersection(bdd, {leaf, :bdd_bot, :bdd_bot, d}))
    end
  end

  defp bdd_intersection(bdd1, bdd2, _leaf_intersection) do
    bdd_intersection(bdd1, bdd2)
  end

  defp bdd_leaf_intersection(leaf, bdd, intersection) do
    case bdd do
      :bdd_top ->
        leaf

      :bdd_bot ->
        :bdd_bot

      bdd_leaf(_, _) ->
        intersection.(leaf, bdd)

      {lit, c, u, _} when lit == leaf ->
        case bdd_union(c, u) do
          :bdd_bot -> :bdd_bot
          cu -> {lit, cu, :bdd_bot, :bdd_bot}
        end

      {lit, c, u, d} ->
        rest =
          bdd_union(
            bdd_leaf_intersection(leaf, u, intersection),
            bdd_difference(bdd_leaf_intersection(leaf, d, intersection), lit)
          )

        with true <- c != :bdd_bot,
             new_leaf = intersection.(leaf, lit),
             true <- new_leaf != :bdd_bot do
          bdd_union(bdd_leaf_intersection(new_leaf, c, intersection), rest)
        else
          _ -> rest
        end
    end
  end

  # Lazy negation: eliminate the union, then perform normal negation (switching leaves)
  def bdd_negation(:bdd_top), do: :bdd_bot
  def bdd_negation(:bdd_bot), do: :bdd_top
  def bdd_negation({_, _} = pair), do: {pair, :bdd_bot, :bdd_bot, :bdd_top}

  def bdd_negation({lit, c, u, d}) do
    {lit, bdd_negation(bdd_union(c, u)), :bdd_bot, bdd_negation(bdd_union(d, u))}
  end

  def bdd_to_dnf(bdd), do: bdd_to_dnf([], [], [], bdd)

  defp bdd_to_dnf(acc, _pos, _neg, :bdd_bot), do: acc
  defp bdd_to_dnf(acc, pos, neg, :bdd_top), do: [{pos, neg} | acc]

  defp bdd_to_dnf(acc, pos, neg, {_, _} = lit) do
    [{[lit | pos], neg} | acc]
  end

  # Lazy node: {lit, C, U, D}  ≡  (lit ∧ C) ∪ U ∪ (¬lit ∧ D)
  defp bdd_to_dnf(acc, pos, neg, {lit, c, u, d}) do
    # U is a bdd in itself, we accumulate its lines first
    bdd_to_dnf(acc, pos, neg, u)
    # C-part
    |> bdd_to_dnf([lit | pos], neg, c)
    # D-part
    |> bdd_to_dnf(pos, [lit | neg], d)
  end

  defp bdd_compare(bdd1, bdd2) do
    case {bdd_head(bdd1), bdd_head(bdd2)} do
      {lit1, lit2} when lit1 < lit2 -> {:lt, bdd_expand(bdd1), bdd2}
      {lit1, lit2} when lit1 > lit2 -> {:gt, bdd1, bdd_expand(bdd2)}
      _ -> {:eq, bdd1, bdd2}
    end
  end

  defp bdd_map(bdd, fun) do
    case bdd do
      :bdd_bot ->
        :bdd_bot

      :bdd_top ->
        :bdd_top

      {_, _} ->
        fun.(bdd)

      {literal, left, union, right} ->
        {fun.(literal), bdd_map(left, fun), bdd_map(union, fun), bdd_map(right, fun)}
    end
  end

  defp bdd_reduce(bdd, acc, fun) do
    case bdd do
      :bdd_bot ->
        acc

      :bdd_top ->
        acc

      {_, _} ->
        fun.(bdd, acc)

      {literal, left, union, right} ->
        acc = fun.(literal, acc)
        acc = bdd_reduce(left, acc, fun)
        acc = bdd_reduce(union, acc, fun)
        acc = bdd_reduce(right, acc, fun)
        acc
    end
  end

  @compile {:inline, bdd_expand: 1, bdd_head: 1}
  defp bdd_expand({_, _} = pair), do: {pair, :bdd_top, :bdd_bot, :bdd_bot}
  defp bdd_expand(bdd), do: bdd

  defp bdd_head({lit, _, _, _}), do: lit
  defp bdd_head(pair), do: pair

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
