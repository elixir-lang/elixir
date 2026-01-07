# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Apply do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  alias Module.ParallelChecker
  import Module.Types.{Helpers, Descr}

  # We limit the size of the union for two reasons:
  # To avoid really large outputs in reports and to
  # reduce the computation cost of inferred code.
  @max_clauses 16

  @atom_true atom([true])
  @atom_false atom([false])

  ## Signatures

  # Define strong arrows found in the standard library.
  # A strong arrow means that, if a type outside of its
  # domain is given, an error is raised. We are also
  # ensuring that domains for the same function have
  # no overlaps.

  # Remote for callback info functions

  kw = fn kw ->
    kw
    |> Enum.map(fn {key, type} when is_atom(key) ->
      tuple([atom([key]), type])
    end)
    |> Enum.reduce(&union/2)
    |> list()
  end

  fas = list(tuple([atom(), integer()]))
  struct_info = list(closed_map(default: if_set(term()), field: atom()))

  shared_info = [
    attributes: list(tuple([atom(), list(term())])),
    compile: kw.(version: list(integer()), source: list(integer()), options: list(term())),
    exports: fas,
    md5: binary(),
    module: atom()
  ]

  module_info = [functions: fas, nifs: fas] ++ shared_info

  elixir_info =
    [
      deprecated: list(tuple([tuple([atom(), integer()]), binary()])),
      exports_md5: binary(),
      functions: fas,
      macros: fas,
      struct: struct_info |> union(atom([nil]))
    ] ++ shared_info

  infos =
    [
      # We have a special key that tracks if something is a struct or not
      {{:__info__, true}, Keyword.put(elixir_info, :struct, struct_info)},
      {{:__info__, false}, Keyword.put(elixir_info, :struct, atom([nil]))},
      {:__info__, elixir_info},
      {:behaviour_info, callbacks: fas, optional_callbacks: fas},
      {:module_info, module_info},
      # TODO: Move this to a type signature declared by `defprotocol` (or perhaps part of the behaviour)
      {:__protocol__,
       module: atom(),
       functions: fas,
       consolidated?: boolean(),
       impls: union(atom([:not_consolidated]), tuple([atom([:consolidated]), list(atom())]))}
    ]

  for {name, clauses} <- infos do
    domain = atom(Keyword.keys(clauses))
    clauses = Enum.map(clauses, fn {key, return} -> {[atom([key])], return} end)

    def signature(unquote(name), 1) do
      {:strong, [unquote(Macro.escape(domain))], unquote(Macro.escape(clauses))}
    end
  end

  def signature(:module_info, 0) do
    {:strong, nil, [{[], unquote(Macro.escape(kw.(module_info)))}]}
  end

  def signature(_, _), do: :none

  # Remote for compiler functions

  mfargs = [atom(), atom(), list(term())]

  send_destination =
    pid()
    |> union(reference())
    |> union(port())
    |> union(atom())
    |> union(tuple([atom(), atom()]))

  basic_arith_2_args_clauses = [
    {[integer(), integer()], integer()},
    {[integer(), float()], float()},
    {[float(), integer()], float()},
    {[float(), float()], float()}
  ]

  args_or_arity = union(list(term()), integer())
  args_or_none = union(list(term()), atom([:none]))
  extra_info = kw.(file: list(integer()), line: integer(), error_info: open_map())

  raise_stacktrace =
    list(
      tuple([atom(), atom(), args_or_arity, extra_info])
      |> union(tuple([atom(), atom(), args_or_arity]))
      |> union(tuple([fun(), args_or_arity, extra_info]))
      |> union(tuple([fun(), args_or_arity]))
    )

  not_signature =
    for bool <- [true, false] do
      {[atom([bool])], atom([not bool])}
    end

  and_signature =
    for left <- [true, false], right <- [true, false] do
      {[atom([left]), atom([right])], atom([left and right])}
    end

  or_signature =
    for left <- [true, false], right <- [true, false] do
      {[atom([left]), atom([right])], atom([left or right])}
    end

  for {mod, fun, clauses} <- [
        # :binary
        {:binary, :copy, [{[binary(), integer()], binary()}]},

        # :erlang
        {:erlang, :+, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :+, basic_arith_2_args_clauses},
        {:erlang, :-, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :-, basic_arith_2_args_clauses},
        {:erlang, :*, basic_arith_2_args_clauses},
        {:erlang, :/, [{[union(integer(), float()), union(integer(), float())], float()}]},
        {:erlang, :"/=", [{[term(), term()], boolean()}]},
        {:erlang, :"=/=", [{[term(), term()], boolean()}]},
        {:erlang, :<, [{[term(), term()], boolean()}]},
        {:erlang, :"=<", [{[term(), term()], boolean()}]},
        {:erlang, :==, [{[term(), term()], boolean()}]},
        {:erlang, :"=:=", [{[term(), term()], boolean()}]},
        {:erlang, :>, [{[term(), term()], boolean()}]},
        {:erlang, :>=, [{[term(), term()], boolean()}]},
        {:erlang, :abs, [{[integer()], integer()}, {[float()], float()}]},
        # TODO: Decide if it returns dynamic() or term()
        {:erlang, :apply, [{[fun(), list(term())], dynamic()}]},
        {:erlang, :apply, [{[atom(), atom(), list(term())], dynamic()}]},
        {:erlang, :and, and_signature},
        {:erlang, :atom_to_binary, [{[atom()], binary()}]},
        {:erlang, :atom_to_list, [{[atom()], list(integer())}]},
        {:erlang, :band, [{[integer(), integer()], integer()}]},
        {:erlang, :binary_part, [{[binary(), integer(), integer()], binary()}]},
        {:erlang, :binary_to_atom, [{[binary()], atom()}]},
        {:erlang, :binary_to_existing_atom, [{[binary()], atom()}]},
        {:erlang, :binary_to_integer, [{[binary()], integer()}]},
        {:erlang, :binary_to_integer, [{[binary(), integer()], integer()}]},
        {:erlang, :binary_to_float, [{[binary()], float()}]},
        {:erlang, :bit_size, [{[bitstring()], integer()}]},
        {:erlang, :bnot, [{[integer()], integer()}]},
        {:erlang, :bor, [{[integer(), integer()], integer()}]},
        {:erlang, :bsl, [{[integer(), integer()], integer()}]},
        {:erlang, :bsr, [{[integer(), integer()], integer()}]},
        {:erlang, :bxor, [{[integer(), integer()], integer()}]},
        {:erlang, :byte_size, [{[binary()], integer()}]},
        {:erlang, :ceil, [{[union(integer(), float())], integer()}]},
        {:erlang, :div, [{[integer(), integer()], integer()}]},
        {:erlang, :error, [{[term()], none()}]},
        {:erlang, :error, [{[term(), args_or_none], none()}]},
        {:erlang, :error, [{[term(), args_or_none, kw.(error_info: open_map())], none()}]},
        {:erlang, :floor, [{[union(integer(), float())], integer()}]},
        {:erlang, :function_exported, [{[atom(), atom(), integer()], boolean()}]},
        {:erlang, :integer_to_binary, [{[integer()], binary()}]},
        {:erlang, :integer_to_binary, [{[integer(), integer()], binary()}]},
        {:erlang, :integer_to_list, [{[integer()], non_empty_list(integer())}]},
        {:erlang, :integer_to_list, [{[integer(), integer()], non_empty_list(integer())}]},
        {:erlang, :is_function, [{[term(), integer()], boolean()}]},
        {:erlang, :is_map_key, [{[term(), open_map()], boolean()}]},
        {:erlang, :length, [{[list(term())], integer()}]},
        {:erlang, :list_to_atom, [{[list(integer())], atom()}]},
        {:erlang, :list_to_existing_atom, [{[list(integer())], atom()}]},
        {:erlang, :list_to_float, [{[non_empty_list(integer())], float()}]},
        {:erlang, :list_to_integer, [{[non_empty_list(integer())], integer()}]},
        {:erlang, :list_to_integer, [{[non_empty_list(integer()), integer()], integer()}]},
        {:erlang, :make_ref, [{[], reference()}]},
        {:erlang, :make_tuple, [{[integer(), term()], tuple()}]},
        {:erlang, :map_size, [{[open_map()], integer()}]},
        {:erlang, :node, [{[], atom()}]},
        {:erlang, :node, [{[pid() |> union(reference()) |> union(port())], atom()}]},
        {:erlang, :not, not_signature},
        {:erlang, :or, or_signature},
        {:erlang, :raise, [{[atom([:error, :exit, :throw]), term(), raise_stacktrace], none()}]},
        {:erlang, :rem, [{[integer(), integer()], integer()}]},
        {:erlang, :round, [{[union(integer(), float())], integer()}]},
        {:erlang, :self, [{[], pid()}]},
        {:erlang, :spawn, [{[fun(0)], pid()}]},
        {:erlang, :spawn, [{mfargs, pid()}]},
        {:erlang, :spawn_link, [{[fun(0)], pid()}]},
        {:erlang, :spawn_link, [{mfargs, pid()}]},
        {:erlang, :spawn_monitor, [{[fun(0)], tuple([pid(), reference()])}]},
        {:erlang, :spawn_monitor, [{mfargs, tuple([pid(), reference()])}]},
        {:erlang, :tuple_size, [{[open_tuple([])], integer()}]},
        {:erlang, :trunc, [{[union(integer(), float())], integer()}]},

        # TODO: Replace term()/dynamic() by parametric types
        {:erlang, :++,
         [
           {[empty_list(), term()], dynamic(term())},
           {[non_empty_list(term()), term()], dynamic(non_empty_list(term(), term()))}
         ]},
        {:erlang, :--, [{[list(term()), list(term())], dynamic(list(term()))}]},
        {:erlang, :delete_element, [{[integer(), open_tuple([])], dynamic(open_tuple([]))}]},
        {:erlang, :hd, [{[non_empty_list(term(), term())], dynamic()}]},
        {:erlang, :element, [{[integer(), open_tuple([])], dynamic()}]},
        {:erlang, :insert_element,
         [{[integer(), open_tuple([]), term()], dynamic(open_tuple([]))}]},
        {:erlang, :list_to_tuple, [{[list(term())], dynamic(open_tuple([]))}]},
        {:erlang, :max, [{[term(), term()], dynamic()}]},
        {:erlang, :min, [{[term(), term()], dynamic()}]},
        {:erlang, :send, [{[send_destination, term()], dynamic()}]},
        {:erlang, :setelement, [{[integer(), open_tuple([]), term()], dynamic(open_tuple([]))}]},
        {:erlang, :tl, [{[non_empty_list(term(), term())], dynamic()}]},
        {:erlang, :tuple_to_list, [{[open_tuple([])], dynamic(list(term()))}]},

        ## Map
        {Map, :from_struct, [{[open_map()], open_map(__struct__: not_set())}]},
        {Map, :get, [{[open_map(), term()], term()}]},
        {Map, :get, [{[open_map(), term(), term()], term()}]},
        {Map, :get_lazy, [{[open_map(), term(), fun(0)], term()}]},
        {Map, :pop, [{[open_map(), term()], tuple([term(), open_map()])}]},
        {Map, :pop, [{[open_map(), term(), term()], tuple([term(), open_map()])}]},
        {Map, :pop!, [{[open_map(), term()], tuple([term(), open_map()])}]},
        {Map, :pop_lazy, [{[open_map(), term(), fun(0)], tuple([term(), open_map()])}]},
        {Map, :put_new, [{[open_map(), term(), term()], open_map()}]},
        {Map, :put_new_lazy, [{[open_map(), term(), fun(0)], open_map()}]},
        {Map, :replace, [{[open_map(), term(), term()], open_map()}]},
        {Map, :replace_lazy, [{[open_map(), term(), fun(1)], open_map()}]},
        {Map, :update, [{[open_map(), term(), term(), fun(1)], open_map()}]},
        {Map, :update!, [{[open_map(), term(), fun(1)], open_map()}]},
        {:maps, :from_keys, [{[list(term()), term()], open_map()}]},
        {:maps, :find,
         [{[term(), open_map()], tuple([atom([:ok]), term()]) |> union(atom([:error]))}]},
        {:maps, :get, [{[term(), open_map()], term()}]},
        {:maps, :is_key, [{[term(), open_map()], boolean()}]},
        {:maps, :keys, [{[open_map()], list(term())}]},
        {:maps, :put, [{[term(), term(), open_map()], open_map()}]},
        {:maps, :remove, [{[term(), open_map()], open_map()}]},
        {:maps, :take,
         [{[term(), open_map()], tuple([term(), open_map()]) |> union(atom([:error]))}]},
        {:maps, :to_list, [{[open_map()], list(tuple([term(), term()]))}]},
        {:maps, :update, [{[term(), term(), open_map()], open_map()}]},
        {:maps, :values, [{[open_map()], list(term())}]}
      ] do
    [arity] = Enum.map(clauses, fn {args, _return} -> length(args) end) |> Enum.uniq()

    true = Code.ensure_loaded?(mod)

    domain_clauses =
      case clauses do
        [_] ->
          {:strong, nil, clauses}

        _ ->
          domain =
            clauses
            |> Enum.map(fn {args, _} -> args end)
            |> Enum.zip_with(fn types -> Enum.reduce(types, &union/2) end)

          {:strong, domain, clauses}
      end

    def signature(unquote(mod), unquote(fun), unquote(arity)),
      do: unquote(Macro.escape(domain_clauses))
  end

  is_guards = [
    is_atom: atom(),
    is_binary: binary(),
    is_bitstring: bitstring(),
    is_boolean: boolean(),
    is_float: float(),
    is_function: fun(),
    is_integer: integer(),
    is_list: union(empty_list(), non_empty_list(term(), term())),
    is_map: open_map(),
    is_number: union(float(), integer()),
    is_pid: pid(),
    is_port: port(),
    is_reference: reference(),
    is_tuple: tuple()
  ]

  for {guard, type} <- is_guards do
    domain_clauses =
      {:strong, [term()],
       [
         {[type], atom([true])},
         {[negation(type)], atom([false])}
       ]}

    def signature(:erlang, unquote(guard), 1),
      do: unquote(Macro.escape(domain_clauses))
  end

  def signature(_mod, _fun, _arity), do: :none

  @doc """
  Entry point for applying functions without a known module.

  Builds on top of remote_domain/4 + remote_apply/7.
  """
  def remote(fun, args, expected, expr, stack, context, of_fun) do
    {info, domain} = remote_domain(fun, args, expected, stack)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_fun.(&1, &2, expr, stack, &3))

    remote_apply(info, nil, fun, args_types, expr, stack, context)
  end

  @doc """
  Entry point for applying functions.

  Shared between expression and guards. Builds on top of remote_domain/7 + remote_apply/7
  """
  def remote(mod, fun, args, expected, expr, stack, context, of_fun) do
    arity = length(args)

    case :elixir_rewrite.inline(mod, fun, arity) do
      {new_mod, new_fun} ->
        do_remote(new_mod, new_fun, args, expected, expr, stack, context, of_fun)

      false ->
        do_remote(mod, fun, args, expected, expr, stack, context, of_fun)
    end
    |> case do
      {info, domain, context} ->
        {args_types, context} =
          zip_map_reduce(args, domain, context, &of_fun.(&1, &2, expr, stack, &3))

        remote_apply(info, mod, fun, args_types, expr, stack, context)

      {type, context} ->
        {type, context}
    end
  end

  # The functions implemented with custom do_remote functions work on values,
  # rather on types, hence the custom behaviour.
  defp do_remote(:erlang, name, [left, right], expected, expr, stack, context, of_fun)
       when name in [:==, :"/=", :"=:=", :"=/="] do
    left_literal? = Macro.quoted_literal?(left)
    right_literal? = Macro.quoted_literal?(right)

    case {left_literal?, right_literal?} do
      {true, false} -> custom_compare(name, right, left, expected, expr, stack, context, of_fun)
      {false, true} -> custom_compare(name, left, right, expected, expr, stack, context, of_fun)
      {literal?, _} -> compare(name, left, right, literal?, expr, stack, context, of_fun)
    end
  end

  defp do_remote(:erlang, name, [left, right], expected, expr, stack, context, of_fun)
       when name in [:>=, :"=<", :>, :<, :min, :max] do
    case sized_order(name, left, right, expected) do
      {arg, expected, return} ->
        {actual, context} = of_fun.(arg, expected, expr, stack, context)
        result = if compatible?(actual, expected), do: return, else: boolean()
        {result, context}

      :none ->
        {left_type, context} = of_fun.(left, term(), expr, stack, context)
        {right_type, context} = of_fun.(right, term(), expr, stack, context)

        result =
          if name in [:min, :max] do
            union(left_type, right_type)
          else
            return(boolean(), [left_type, right_type], stack)
          end

        if is_warning(stack) do
          common = intersection(left_type, right_type)

          cond do
            empty?(common) and not (number_type?(left_type) and number_type?(right_type)) ->
              error = {:mismatched_comparison, left_type, right_type}
              remote_error(error, :erlang, name, 2, expr, stack, context)

            match?({false, _}, map_fetch_key(dynamic(common), :__struct__)) ->
              error = {:struct_comparison, left_type, right_type}
              remote_error(error, :erlang, name, 2, expr, stack, context)

            true ->
              {result, context}
          end
        else
          {result, context}
        end
    end
  end

  defp do_remote(:erlang, :element, [index, tuple], expected, expr, stack, context, of_fun)
       when is_integer(index) do
    tuple_type = open_tuple(List.duplicate(term(), max(index - 1, 0)) ++ [expected])
    {tuple_type, context} = of_fun.(tuple, tuple_type, expr, stack, context)

    case tuple_fetch(tuple_type, index - 1) do
      {_optional?, value_type} ->
        {return(value_type, [tuple_type], stack), context}

      :badtuple ->
        remote_error(:erlang, :element, [integer(), tuple_type], expr, stack, context)

      :badindex ->
        remote_error({:badindex, index, tuple_type}, :erlang, :element, 2, expr, stack, context)
    end
  end

  defp do_remote(:erlang, :make_tuple, [size, elem], _, expr, stack, context, of_fun)
       when is_integer(size) and size >= 0 do
    {elem_type, context} = of_fun.(elem, term(), expr, stack, context)
    {return(tuple(List.duplicate(elem_type, size)), [elem_type], stack), context}
  end

  defp do_remote(:erlang, :insert_element, [index, tuple, elem], _, expr, stack, context, of_fun)
       when is_integer(index) do
    tuple_type = open_tuple(List.duplicate(term(), max(index - 1, 0)))

    {tuple_type, context} = of_fun.(tuple, tuple_type, expr, stack, context)
    {elem_type, context} = of_fun.(elem, term(), expr, stack, context)

    case tuple_insert_at(tuple_type, index - 1, elem_type) do
      value_type when is_descr(value_type) ->
        {return(value_type, [tuple_type, elem_type], stack), context}

      :badtuple ->
        args_types = [integer(), tuple_type, elem_type]
        remote_error(:erlang, :insert_element, args_types, expr, stack, context)

      :badindex ->
        error = {:badindex, index - 1, tuple_type}
        remote_error(error, :erlang, :insert_element, 3, expr, stack, context)
    end
  end

  defp do_remote(:erlang, :delete_element, [index, tuple], _, expr, stack, context, of_fun)
       when is_integer(index) do
    tuple_type = open_tuple(List.duplicate(term(), max(index, 1)))
    {tuple_type, context} = of_fun.(tuple, tuple_type, expr, stack, context)

    case tuple_delete_at(tuple_type, index - 1) do
      value_type when is_descr(value_type) ->
        {return(value_type, [tuple_type], stack), context}

      :badtuple ->
        remote_error(:erlang, :delete_element, [integer(), tuple_type], expr, stack, context)

      :badindex ->
        error = {:badindex, index, tuple_type}
        remote_error(error, :erlang, :delete_element, 2, expr, stack, context)
    end
  end

  defp do_remote(mod, fun, args, expected, expr, stack, context, _of_fun) do
    remote_domain(mod, fun, args, expected, elem(expr, 1), stack, context)
  end

  @empty_list empty_list()
  @non_empty_list non_empty_list(term())
  @empty_map empty_map()
  @non_empty_map difference(open_map(), empty_map())

  # Limit the size of tuples to 16 entries
  # as otherwise we may create large nodes
  defguardp is_data_size(fun, literal)
            when (fun in [:length, :map_size] and is_integer(literal) and literal >= 0) or
                   (fun in [:tuple_size] and literal in 0..15)

  defp custom_compare(
         name,
         {{:., _, [:erlang, fun]}, _, [arg]} = left,
         literal,
         expected,
         expr,
         stack,
         context,
         of_fun
       )
       when is_data_size(fun, literal) do
    case booleaness(expected) do
      booleaness when booleaness in [:maybe_both, :none] ->
        compare(name, left, literal, false, expr, stack, context, of_fun)

      booleaness ->
        {polarity, return} =
          case booleaness do
            :maybe_true -> {name in [:==, :"=:="], @atom_true}
            :maybe_false -> {name in [:"/=", :"=/="], @atom_false}
          end

        expected =
          case fun do
            :length when :erlang.xor(polarity, literal > 0) -> @empty_list
            :length -> @non_empty_list
            :map_size when :erlang.xor(polarity, literal > 0) -> @empty_map
            :map_size -> @non_empty_map
            :tuple_size when polarity -> tuple(List.duplicate(term(), literal))
            :tuple_size -> difference(open_tuple([]), tuple(List.duplicate(term(), literal)))
          end

        {actual, context} = of_fun.(arg, expected, expr, stack, context)
        result = if compatible?(actual, expected), do: return, else: boolean()
        {result, context}
    end
  end

  defp custom_compare(name, left, right, _expected, expr, stack, context, of_fun) do
    compare(name, left, right, false, expr, stack, context, of_fun)
  end

  defp compare(name, left, right, literal?, expr, stack, context, of_fun) do
    {left_type, context} = of_fun.(left, term(), expr, stack, context)
    {right_type, context} = of_fun.(right, term(), expr, stack, context)
    result = return(boolean(), [left_type, right_type], stack)

    cond do
      literal? or not is_warning(stack) ->
        {result, context}

      name in [:==, :"/="] and number_type?(left_type) and number_type?(right_type) ->
        {result, context}

      disjoint?(left_type, right_type) ->
        error = {:mismatched_comparison, left_type, right_type}
        remote_error(error, :erlang, name, 2, expr, stack, context)

      true ->
        {result, context}
    end
  end

  defp sized_order(name, left, right, expected) do
    if name in [:>=, :"=<", :>, :<] do
      case {left, right} do
        {{{:., _, [:erlang, fun]}, _, [arg]}, size} when is_data_size(fun, size) ->
          case booleaness(expected) do
            :maybe_true -> sized_order(name, fun, size, arg, @atom_true)
            :maybe_false -> sized_order(invert_order(name), fun, size, arg, @atom_false)
            _ -> :none
          end

        {size, {{:., _, [:erlang, fun]}, _, [arg]}} when is_data_size(fun, size) ->
          case booleaness(expected) do
            :maybe_true -> sized_order(invert_order(name), fun, size, arg, @atom_true)
            :maybe_false -> sized_order(name, fun, size, arg, @atom_false)
            _ -> :none
          end

        _ ->
          :none
      end
    else
      :none
    end
  end

  defp sized_order(name, fun, size, arg, return) do
    case expected_order(fun, name, size) do
      :none -> :none
      expected -> {arg, expected, return}
    end
  end

  defp expected_order(_, :<, 0), do: :none

  defp expected_order(:tuple_size, :<, size),
    do: difference(open_tuple([]), open_tuple(List.duplicate(term(), size)))

  defp expected_order(:tuple_size, :"=<", 0),
    do: tuple([])

  defp expected_order(:tuple_size, :"=<", size),
    do: difference(open_tuple([]), open_tuple(List.duplicate(term(), size + 1)))

  defp expected_order(:tuple_size, :>, size),
    do: open_tuple(List.duplicate(term(), size + 1))

  defp expected_order(:tuple_size, :>=, size),
    do: open_tuple(List.duplicate(term(), size))

  defp expected_order(:map_size, :<, 1), do: @empty_map
  defp expected_order(:map_size, :"=<", 0), do: @empty_map
  defp expected_order(:map_size, :>, _), do: @non_empty_map
  defp expected_order(:map_size, :>=, size) when size > 0, do: @non_empty_map

  defp expected_order(:length, :<, 1), do: @empty_list
  defp expected_order(:length, :"=<", 0), do: @empty_list
  defp expected_order(:length, :>, _), do: @non_empty_list
  defp expected_order(:length, :>=, size) when size > 0, do: @non_empty_list

  defp expected_order(_, _, _), do: :none

  defp invert_order(:>=), do: :<
  defp invert_order(:"=<"), do: :>
  defp invert_order(:>), do: :"=<"
  defp invert_order(:<), do: :>=

  @doc """
  Returns the domain of an unknown module.

  Used only by info functions.
  """
  def remote_domain(fun, args, expected, _stack) do
    arity = length(args)
    info = signature(fun, arity)
    {info, filter_domain(info, expected, arity)}
  end

  @doc """
  Returns the domain of a remote function with info to apply it.

  Note this does not consider rewrites done by the compiler, `remote/8` does.
  """
  def remote_domain(:erlang, :is_function, [_, arity], expected, _meta, _stack, context)
      when is_integer(arity) and arity >= 0 do
    type = fun(arity)

    info =
      {:strong, [term(), integer()],
       [
         {[type, integer()], atom([true])},
         {[negation(type), integer()], atom([false])}
       ]}

    {info, filter_domain(info, expected, 2), context}
  end

  def remote_domain(:erlang, :is_map_key, [key, _map], expected, _meta, _stack, context)
      when is_atom(key) do
    info =
      {:strong, [term(), open_map()],
       [
         {[term(), open_map([{key, term()}])], atom([true])},
         {[term(), open_map([{key, not_set()}])], atom([false])}
       ]}

    {info, filter_domain(info, expected, 2), context}
  end

  def remote_domain(:maps, :get, [key, _], expected, _meta, _stack, context) when is_atom(key) do
    domain = [term(), open_map([{key, expected}])]
    {{:strong, nil, [{domain, term()}]}, domain, context}
  end

  def remote_domain(:maps, :update, [key, _, _], _expected, _meta, _stack, context)
      when is_atom(key) do
    domain = [term(), term(), open_map([{key, term()}])]
    {{:strong, nil, [{domain, open_map()}]}, domain, context}
  end

  def remote_domain(Map, :pop!, [_, key], _expected, _meta, _stack, context) when is_atom(key) do
    domain = [open_map([{key, term()}]), term()]
    {{:strong, nil, [{domain, tuple([term(), open_map()])}]}, domain, context}
  end

  def remote_domain(Map, :update!, [_, key, _], _expected, _meta, _stack, context)
      when is_atom(key) do
    domain = [open_map([{key, term()}]), term(), fun(1)]
    {{:strong, nil, [{domain, open_map()}]}, domain, context}
  end

  def remote_domain(mod, fun, args, expected, meta, stack, context) do
    arity = length(args)
    {info, context} = signature(mod, fun, arity, meta, stack, context)
    {info, filter_domain(info, expected, arity), context}
  end

  defp remote_error(mod, fun, args, expr, stack, context) do
    remote_error(badremote(mod, fun, args), mod, fun, length(args), expr, stack, context)
  end

  defp remote_error(error, mod, fun, arity, expr, stack, context) do
    mfac = mfac(expr, mod, fun, arity)
    error = {error, mfac, expr, context}
    {error_type(), error(error, elem(expr, 1), stack, context)}
  end

  @doc """
  Applies a previously collected domain from `remote_domain/7`.
  """
  def remote_apply(info, mod, fun, args_types, expr, stack, context) do
    case remote_apply(mod, fun, info, args_types, stack) do
      {:ok, type} ->
        {type, context}

      {:error, error} ->
        remote_error(error, mod, fun, length(args_types), expr, stack, context)
    end
  end

  defp remote_apply(:erlang, :hd, _info, [list], stack) do
    case list_hd(list) do
      {:ok, value_type} -> {:ok, return(value_type, [list], stack)}
      :badnonemptylist -> {:error, badremote(:erlang, :hd, [list])}
    end
  end

  defp remote_apply(:erlang, :tl, _info, [list], stack) do
    case list_tl(list) do
      {:ok, value_type} -> {:ok, return(value_type, [list], stack)}
      :badnonemptylist -> {:error, badremote(:erlang, :tl, [list])}
    end
  end

  @struct_key atom([:__struct__])
  @nil_atom atom([nil])

  defp remote_apply(Map, :from_struct, _info, [map] = args_types, stack) do
    case map_update(map, @struct_key, not_set(), false, true) do
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(Map, :from_struct, args_types)}
      {:error, _errors} -> {:ok, map}
    end
  end

  defp remote_apply(Map, :get, _info, [map, key] = args_types, stack) do
    case map_get(map, key) do
      {:ok, value} -> {:ok, return(union(value, @nil_atom), args_types, stack)}
      :badmap -> {:error, badremote(Map, :get, args_types)}
      :error -> {:error, {:badkeydomain, map, key, @nil_atom}}
    end
  end

  defp remote_apply(Map, :get, _info, [map, key, default] = args_types, stack) do
    case map_get(map, key) do
      {:ok, value} -> {:ok, return(union(value, default), args_types, stack)}
      :badmap -> {:error, badremote(Map, :get, args_types)}
      :error -> {:error, {:badkeydomain, map, key, default}}
    end
  end

  defp remote_apply(Map, :get_lazy, _info, [map, key, fun] = args_types, stack) do
    case fun_apply(fun, []) do
      {:ok, default} ->
        case map_get(map, key) do
          {:ok, value} -> {:ok, return(union(value, default), args_types, stack)}
          :badmap -> {:error, badremote(Map, :get_lazy, args_types)}
          :error -> {:error, {:badkeydomain, map, key, default}}
        end

      reason ->
        {:error, {:badapply, fun, [], reason}}
    end
  end

  defp remote_apply(Map, :put_new, _info, [map, key, value] = args_types, stack) do
    map_put_new(map, key, value, :put_new, args_types, stack)
  end

  defp remote_apply(Map, :put_new_lazy, _info, [map, key, fun] = args_types, stack) do
    case fun_apply(fun, []) do
      {:ok, value} -> map_put_new(map, key, value, :put_new_lazy, args_types, stack)
      reason -> {:error, {:badapply, fun, [], reason}}
    end
  end

  defp remote_apply(Map, :pop, _info, args_types, stack) do
    [map, key, default] =
      case args_types do
        [map, key] -> [map, key, @nil_atom]
        _ -> args_types
      end

    case map_update(map, key, not_set(), true, false) do
      {value, descr, _errors} ->
        value = union(value, default)
        {:ok, return(tuple([value, descr]), args_types, stack)}

      :badmap ->
        {:error, badremote(Map, :pop, args_types)}

      {:error, _errors} ->
        {:error, {:badkeydomain, map, key, tuple([default, map])}}
    end
  end

  defp remote_apply(Map, :pop_lazy, _info, [map, key, fun] = args_types, stack) do
    case fun_apply(fun, []) do
      {:ok, default} ->
        case map_update(map, key, not_set(), true, false) do
          {value, descr, _errors} ->
            value = union(value, default)
            {:ok, return(tuple([value, descr]), args_types, stack)}

          :badmap ->
            {:error, badremote(Map, :pop_lazy, args_types)}

          {:error, _errors} ->
            {:error, {:badkeydomain, map, key, tuple([default, map])}}
        end

      reason ->
        {:error, {:badapply, fun, [], reason}}
    end
  end

  defp remote_apply(Map, :pop!, _info, [map, key] = args_types, stack) do
    case map_update(map, key, not_set(), true, false) do
      {value, descr, _errors} -> {:ok, return(tuple([value, descr]), args_types, stack)}
      :badmap -> {:error, badremote(Map, :pop!, args_types)}
      {:error, _errors} -> {:error, {:badkeydomain, map, key, "raise"}}
    end
  end

  defp remote_apply(Map, :replace, _info, [map, key, value] = args_types, stack) do
    fun = fn optional?, _type -> if optional?, do: if_set(value), else: value end

    case map_update_fun(map, key, fun, false, false) do
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(Map, :replace, args_types)}
      {:error, _errors} -> {:error, {:badkeydomain, map, key, "do nothing"}}
    end
  end

  defp remote_apply(Map, :replace_lazy, _info, args_types, stack) do
    map_update_or_replace_lazy(:replace_lazy, args_types, stack, "do nothing")
  end

  defp remote_apply(Map, :update, _info, [map, key, default, fun] = args_types, stack) do
    try do
      {map, default} =
        case default do
          %{dynamic: default} -> {dynamic(map), default}
          _ -> {map, default}
        end

      map =
        case fun do
          %{dynamic: _} -> dynamic(map)
          _ -> map
        end

      fun_apply = fn optional?, arg_type ->
        if empty?(arg_type) do
          default
        else
          case fun_apply(fun, [arg_type]) do
            {:ok, res} -> if optional?, do: union(res, default), else: res
            reason -> throw({:badapply, reason, [arg_type]})
          end
        end
      end

      map_update_fun(map, key, fun_apply, false, true)
    catch
      {:badapply, reason, args_types} ->
        {:error, {:badapply, fun, args_types, reason}}
    else
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(Map, :update, args_types)}
      {:error, _errors} -> {:ok, map}
    end
  end

  defp remote_apply(Map, :update!, _info, args_types, stack) do
    map_update_or_replace_lazy(:update!, args_types, stack, "raise")
  end

  defp remote_apply(:maps, :find, _info, [key, map] = args_types, stack) do
    case map_get(map, key) do
      {_, value} ->
        result = tuple([atom([:ok]), value]) |> union(atom([:error]))
        {:ok, return(result, args_types, stack)}

      :badmap ->
        {:error, badremote(:maps, :find, args_types)}

      :error ->
        {:error, {:badkeydomain, map, key, atom([:error])}}
    end
  end

  defp remote_apply(:maps, :from_keys, _info, [list, value_type] = args_types, stack) do
    case list_of(list) do
      {true, nil} ->
        {:ok, return(empty_map(), args_types, stack)}

      {empty_list?, key_type} ->
        if key_type == dynamic() or key_type == term() do
          {:ok, return(open_map(), args_types, stack)}
        else
          value_type = if_set(value_type)
          domain_keys = to_domain_keys(key_type)

          keys =
            case atom_fetch(key_type) do
              {:finite, atom_keys} -> [List.delete(domain_keys, :atom) | atom_keys]
              _ -> [domain_keys]
            end

          map = closed_map(Enum.map(keys, &{&1, value_type}))

          map_and_maybe_empty_map =
            case empty_list? do
              true -> map
              false -> difference(map, empty_map())
            end

          {:ok, return(map_and_maybe_empty_map, args_types, stack)}
        end

      :badproperlist ->
        {:error, badremote(:maps, :from_keys, args_types)}
    end
  end

  defp remote_apply(:maps, :get, _info, [key, map] = args_types, stack) do
    case map_get(map, key) do
      {:ok, value} -> {:ok, return(value, args_types, stack)}
      :badmap -> {:error, badremote(:maps, :get, args_types)}
      :error -> {:error, {:badkeydomain, map, key, "raise"}}
    end
  end

  defp remote_apply(:maps, :keys, _info, [map], stack) do
    case map_to_list(map, fn key, _value -> key end) do
      {:ok, list_type} -> {:ok, return(list_type, [map], stack)}
      :badmap -> {:error, badremote(:maps, :keys, [map])}
    end
  end

  defp remote_apply(:maps, :put, _info, [key, value, map] = args_types, stack) do
    case map_update(map, key, value, false, true) do
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(:maps, :put, args_types)}
      {:error, _errors} -> {:ok, map}
    end
  end

  defp remote_apply(:maps, :remove, _info, [key, map] = args_types, stack) do
    case map_update(map, key, not_set(), false, true) do
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(:maps, :remove, args_types)}
      {:error, _errors} -> {:ok, map}
    end
  end

  defp remote_apply(:maps, :take, _info, [key, map] = args_types, stack) do
    case map_update(map, key, not_set(), true, false) do
      {value, descr, _errors} ->
        result = union(tuple([value, descr]), atom([:error]))
        {:ok, return(result, args_types, stack)}

      :badmap ->
        {:error, badremote(:maps, :take, args_types)}

      {:error, _errors} ->
        {:error, {:badkeydomain, map, key, atom([:error])}}
    end
  end

  defp remote_apply(:maps, :to_list, _info, [map], stack) do
    case map_to_list(map) do
      {:ok, list_type} -> {:ok, return(list_type, [map], stack)}
      :badmap -> {:error, badremote(:maps, :to_list, [map])}
    end
  end

  defp remote_apply(:maps, :update, _info, [key, value, map] = args_types, stack) do
    fun = fn optional?, _type -> if optional?, do: if_set(value), else: value end

    case map_update_fun(map, key, fun, false, false) do
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(:maps, :update, args_types)}
      {:error, _errors} -> {:error, {:badkeydomain, map, key, "raise"}}
    end
  end

  defp remote_apply(:maps, :values, _info, [map], stack) do
    case map_to_list(map, fn _key, value -> value end) do
      {:ok, list_type} -> {:ok, return(list_type, [map], stack)}
      :badmap -> {:error, badremote(:maps, :keys, [map])}
    end
  end

  defp remote_apply(_mod, _fun, info, args_types, stack) do
    remote_apply(info, args_types, stack)
  end

  defp remote_apply(:none, _args_types, _stack) do
    {:ok, dynamic()}
  end

  defp remote_apply({:infer, _domain, clauses} = sig, args_types, _stack) do
    case apply_infer(clauses, args_types) do
      {_used, type} -> {:ok, type}
      :error -> {:error, {:badremote, sig, args_types}}
    end
  end

  defp remote_apply({:strong, domain, clauses} = sig, args_types, stack) do
    case apply_strong(domain, clauses, args_types, stack) do
      {_used, type} -> {:ok, type}
      :error -> {:error, {:badremote, sig, args_types}}
    end
  end

  defp badremote(mod, fun, args) do
    {:badremote, signature(mod, fun, length(args)), args}
  end

  @doc """
  Returns the type of a remote capture.
  """
  def remote_capture(modules, fun, arity, meta, stack, context) do
    case modules do
      [] ->
        {dynamic(fun(arity)), context}

      [_ | _] ->
        {type, fallback?, context} =
          Enum.reduce(modules, {none(), false, context}, fn module, {type, fallback?, context} ->
            case signature(module, fun, arity, meta, stack, context) do
              {{:strong, _, clauses}, context} ->
                {union(type, fun_from_non_overlapping_clauses(clauses)), fallback?, context}

              {{:infer, _, clauses}, context} when length(clauses) <= @max_clauses ->
                {union(type, fun_from_inferred_clauses(clauses)), fallback?, context}

              {_, context} ->
                {type, true, context}
            end
          end)

        if fallback? do
          {dynamic(fun(arity)), context}
        else
          {type, context}
        end
    end
  end

  @doc """
  Gets a mfa signature.

  It returns either a tuple with the remote information and the context.
  The remote information may be one of:

    * `:none` - no typing information found.

    * `{:infer, domain or nil, clauses}` - clauses from inferences.
      You must check all clauses and return the union between them.
      They are dynamic and they can only be converted into arrows by
      computing the union of all arguments.

    * `{:strong, domain or nil, clauses}` - clauses from signatures. So far
      these are strong arrows with non-overlapping domains

  """
  def signature(module, fun, arity, meta, stack, context) when is_atom(module) do
    if Keyword.get(meta, :runtime_module, false) do
      {:none, context}
    else
      case signature(module, fun, arity) do
        :none -> export(module, fun, arity, meta, stack, context)
        clauses -> {clauses, context}
      end
    end
  end

  defp export(_module, :module_info, arity, _meta, _stack, context) when arity in [0, 1] do
    {signature(:module_info, arity), context}
  end

  defp export(module, fun, arity, meta, %{cache: cache} = stack, context) do
    cond do
      cache == nil ->
        {:none, context}

      stack.mode == :infer ->
        case ParallelChecker.fetch_export(stack.cache, module, fun, arity, false) do
          {:ok, mode, _, info} when info == :none or mode == :protocol ->
            {signature(fun, arity), context}

          {:ok, _mode, _, info} ->
            {info, context}

          _ ->
            {:none, context}
        end

      true ->
        case ParallelChecker.fetch_export(stack.cache, module, fun, arity, true) do
          {:ok, mode, reason, info} ->
            info = if info == :none, do: signature(fun, arity), else: info
            {info, check_deprecated(mode, module, fun, arity, reason, meta, stack, context)}

          {:badfunction, mode} when mode != :erlang and fun == :__info__ and arity == 1 ->
            key =
              cond do
                not Code.ensure_loaded?(module) -> :__info__
                module.__info__(:struct) != nil -> {:__info__, true}
                true -> {:__info__, false}
              end

            {signature(key, arity), context}

          error ->
            context =
              if warn_undefined?(module, fun, arity, stack) do
                warn(__MODULE__, {:undefined, error, module, fun, arity}, meta, stack, context)
              else
                context
              end

            {:none, context}
        end
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, stack, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> :string.titlecase()
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> :string.titlecase()
        reason = "It will be removed in #{removal}. #{reason}"
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      _ ->
        context
    end
  end

  defp check_deprecated(_elixir_or_protocol, module, fun, arity, reason, meta, stack, context) do
    if reason do
      warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)
    else
      context
    end
  end

  defp warn_undefined?(_, _, _, %{no_warn_undefined: %Macro.Env{}}) do
    false
  end

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, stack) do
    not Enum.any?(stack.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  ## Funs

  def fun(fun_type, args_types, call, stack, context) do
    case fun_apply(fun_type, args_types) do
      {:ok, res} ->
        {res, context}

      reason ->
        error = {{:badapply, fun_type, args_types, reason}, nil, call, context}
        {error_type(), error(__MODULE__, error, elem(call, 1), stack, context)}
    end
  end

  ## Local

  def local(fun, args, expected, {_, meta, _} = expr, stack, context, of_fun) do
    {local_info, domain, context} = local_domain(fun, args, expected, meta, stack, context)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_fun.(&1, &2, expr, stack, &3))

    local_apply(local_info, fun, args_types, expr, stack, context)
  end

  defp local_domain(fun, args, expected, meta, stack, context) do
    arity = length(args)

    case stack.local_handler.(meta, {fun, arity}, stack, context) do
      false ->
        {{false, :none}, List.duplicate(term(), arity), context}

      {kind, info, context} ->
        update_used? = is_warning(stack) and kind == :defp

        if info == :none do
          {{update_used?, :none}, List.duplicate(term(), arity), context}
        else
          {{update_used?, info}, filter_domain(info, expected, arity), context}
        end
    end
  end

  defp local_apply({update_used?, :none}, fun, args_types, _expr, _stack, context) do
    if update_used? do
      {dynamic(), put_in(context.local_used[{fun, length(args_types)}], [])}
    else
      {dynamic(), context}
    end
  end

  defp local_apply({update_used?, info}, fun, args_types, expr, stack, context) do
    case local_apply(info, args_types, stack) do
      {indexes, type} ->
        context =
          if update_used? do
            update_in(context.local_used[{fun, length(args_types)}], fn current ->
              (current || used_from_clauses(info)) -- indexes
            end)
          else
            context
          end

        {type, context}

      :error ->
        error = {:badlocal, info, args_types, expr, context}
        {error_type(), error(error, with_span(elem(expr, 1), fun), stack, context)}
    end
  end

  defp local_apply({:infer, _domain, clauses}, args_types, _stack) do
    apply_infer(clauses, args_types)
  end

  defp local_apply({:strong, domain, clauses}, args_types, stack) do
    apply_strong(domain, clauses, args_types, stack)
  end

  defp used_from_clauses({_strong_or_infer, _domain, clauses}),
    do: Enum.with_index(clauses, fn _, i -> i end)

  @doc """
  Deal with local captures.
  """
  def local_capture(fun, arity, meta, stack, context) do
    fun_arity = {fun, arity}

    case stack.local_handler.(meta, fun_arity, stack, context) do
      false ->
        {dynamic(fun(arity)), context}

      {kind, info, context} ->
        result =
          case info do
            {:infer, _, clauses} when length(clauses) <= @max_clauses ->
              fun_from_inferred_clauses(clauses)

            _ ->
              dynamic(fun(arity))
          end

        context =
          if stack.mode != :infer and kind == :defp do
            # Mark all clauses as used, as the function is being exported.
            put_in(context.local_used[fun_arity], [])
          else
            context
          end

        {result, context}
    end
  end

  @doc """
  Computes the return type of an application.
  """
  def return(type, args_types, stack) do
    cond do
      stack.mode == :static -> type
      Enum.any?(args_types, &gradual?/1) -> dynamic(type)
      true -> type
    end
  end

  ## Map helpers

  defp map_put_new(map, key, value, name, args_types, stack) do
    fun = fn
      true, type -> union(type, value)
      false, type -> if empty?(type), do: value, else: type
    end

    case map_update_fun(map, key, fun, false, true) do
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(Map, name, args_types)}
      {:error, _errors} -> {:ok, map}
    end
  end

  def map_update_or_replace_lazy(name, [map, key, fun] = args_types, stack, error) do
    try do
      map =
        case fun do
          %{dynamic: _} -> dynamic(map)
          _ -> map
        end

      fun_apply = fn optional?, arg_type ->
        case fun_apply(fun, [arg_type]) do
          {:ok, res} -> if optional?, do: if_set(res), else: res
          reason -> throw({:badapply, reason, [arg_type]})
        end
      end

      map_update_fun(map, key, fun_apply, false, false)
    catch
      {:badapply, reason, args_types} ->
        {:error, {:badapply, fun, args_types, reason}}
    else
      {_value, descr, _errors} -> {:ok, return(descr, args_types, stack)}
      :badmap -> {:error, badremote(Map, name, args_types)}
      {:error, _errors} -> {:error, {:badkeydomain, map, key, error}}
    end
  end

  ## Application helpers

  defp domain(nil, [{domain, _}]), do: domain
  defp domain(domain, _clauses), do: domain

  @term_or_dynamic [term(), dynamic()]

  defp filter_domain(:none, _expected, arity) do
    List.duplicate(term(), arity)
  end

  defp filter_domain({_, domain, clauses}, expected, _arity) when expected in @term_or_dynamic do
    domain(domain, clauses)
  end

  defp filter_domain({_type, domain, clauses}, expected, arity) do
    case filter_domain(clauses, expected, [], true) do
      :none -> List.duplicate(term(), arity)
      :all -> domain(domain, clauses)
      args -> Enum.zip_with(args, fn types -> Enum.reduce(types, &union/2) end)
    end
  end

  defp filter_domain([{args, return} | clauses], expected, acc, all_compatible?) do
    case disjoint?(return, expected) do
      false -> filter_domain(clauses, expected, [args | acc], all_compatible?)
      true -> filter_domain(clauses, expected, acc, false)
    end
  end

  defp filter_domain([], _expected, [], _all_compatible?), do: :none
  defp filter_domain([], _expected, _acc, true), do: :all
  defp filter_domain([], _expected, acc, false), do: acc

  defp apply_infer(clauses, args_types) do
    case apply_clauses(clauses, args_types, 0, 0, [], []) do
      {0, [], []} ->
        :error

      {count, used, _returns} when count > @max_clauses ->
        {used, dynamic()}

      {_count, used, returns} ->
        {used, returns |> Enum.reduce(&union/2) |> dynamic()}
    end
  end

  defp apply_strong(_domain, [{expected, return}], args_types, stack) do
    # Optimize single clauses as the domain is the single clause args.
    case zip_compatible?(args_types, expected) do
      true -> {[0], return(return, args_types, stack)}
      false -> :error
    end
  end

  defp apply_strong(domain, clauses, args_types, stack) do
    # If the type is only gradual, the compatibility check is the same
    # as a non disjoint check. So we skip checking compatibility twice.
    with true <- zip_compatible_or_only_gradual?(args_types, domain),
         {count, used, returns} when count > 0 <- apply_clauses(clauses, args_types, 0, 0, [], []) do
      {used, returns |> Enum.reduce(&union/2) |> return(args_types, stack)}
    else
      _ -> :error
    end
  end

  defp apply_clauses([{expected, return} | clauses], args_types, index, count, used, returns) do
    if zip_not_disjoint?(args_types, expected) do
      apply_clauses(clauses, args_types, index + 1, count + 1, [index | used], [return | returns])
    else
      apply_clauses(clauses, args_types, index + 1, count, used, returns)
    end
  end

  defp apply_clauses([], _args_types, _index, count, used, returns) do
    {count, used, returns}
  end

  defp zip_compatible_or_only_gradual?([actual | actuals], [expected | expecteds]) do
    (only_gradual?(actual) or compatible?(actual, expected)) and
      zip_compatible_or_only_gradual?(actuals, expecteds)
  end

  defp zip_compatible_or_only_gradual?([], []), do: true

  defp zip_compatible?([actual | actuals], [expected | expecteds]) do
    compatible?(actual, expected) and zip_compatible?(actuals, expecteds)
  end

  defp zip_compatible?([], []), do: true

  defp zip_not_disjoint?([actual | actuals], [expected | expecteds]) do
    not disjoint?(actual, expected) and zip_not_disjoint?(actuals, expecteds)
  end

  defp zip_not_disjoint?([], []), do: true

  ## Error handling

  defp error(warning, meta, stack, context) do
    error(__MODULE__, warning, meta, stack, context)
  end

  ## Diagnostics

  def format_diagnostic({{:badapply, fun_type, args_types, reason}, mfac, expr, context}) do
    mfa_or_call =
      case mfac do
        {mod, fun, arity, _converter} ->
          "function call within #{Exception.format_mfa(mod, fun, arity)}"

        nil ->
          "function call"
      end

    {message, to_trace, hints} =
      case reason do
        {:badarg, domain} ->
          message = """
          incompatible types given on #{mfa_or_call}:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{args_to_quoted_string(args_types, domain, &Function.identity/1) |> indent(4)}

          but function has type:

              #{to_quoted_string(fun_type) |> indent(4)}
          """

          hints =
            cond do
              not empty?(args_to_domain(domain)) -> []
              match?({:or, _, _}, to_quoted(fun_type)) -> [:empty_union_domain]
              true -> [:empty_domain]
            end

          # When there is an argument error, we trace the arguments
          {message, elem(expr, 2), hints}

        {:badarity, arities} ->
          info =
            case arities do
              [arity] -> "function with arity #{arity}"
              _ -> "function with arities #{Enum.join(arities, ",")}"
            end

          message = """
          expected a #{length(args_types)}-arity function on #{mfa_or_call}:

              #{expr_to_string(expr) |> indent(4)}

          but got #{info}:

              #{to_quoted_string(fun_type) |> indent(4)}
          """

          {message, elem(expr, 0), []}

        :badfun ->
          message = """
          expected a #{length(args_types)}-arity function on #{mfa_or_call}:

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(fun_type) |> indent(4)}
          """

          {message, elem(expr, 0), []}
      end

    traces = collect_traces(to_trace, context)

    %{
      details: %{typing_traces: traces},
      message: IO.iodata_to_binary([message, format_traces(traces), format_hints(hints)])
    }
  end

  def format_diagnostic({:badlocal, {_, domain, clauses}, args_types, expr, context}) do
    domain = domain(domain, clauses)
    traces = collect_traces(expr, context)
    converter = &Function.identity/1
    {fun, meta, _} = expr

    explanation =
      empty_arg_reason(args_types) ||
        """
        but expected one of:
        #{clauses_args_to_quoted_string(clauses, converter, [])}
        """

    banner =
      case fun == :super && meta[:default] && meta[:super] do
        {_kind, fun} ->
          """
          incompatible types given as default arguments to #{fun}/#{length(args_types)}:
          """

        _ ->
          """
          incompatible types given to #{fun}/#{length(args_types)}:

              #{expr_to_string(expr) |> indent(4)}

          given types:
          """
      end

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          banner,
          """

              #{args_to_quoted_string(args_types, domain, converter) |> indent(4)}

          """,
          explanation,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({{:badindex, index, type}, mfac, expr, context}) do
    traces = collect_traces(expr, context)
    {mod, fun, arity, _converter} = mfac
    mfa = Exception.format_mfa(mod, fun, arity)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a tuple with at least #{pluralize(index, "element", "elements")} in #{mfa}:

              #{expr_to_string(expr) |> indent(4)}

          the given type does not have the given index:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({{:badkeydomain, map, key, error}, mfac, expr, context}) do
    {mod, fun, arity, _converter} = mfac
    mfa = Exception.format_mfa(mod, fun, arity)
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types given to #{mfa}:

              #{expr_to_string(expr) |> indent(4)}

          the map:

              #{to_quoted_string(map) |> indent(4)}

          does not have all required keys:

              #{to_quoted_string(key) |> indent(4)}

          therefore this function will always #{if is_binary(error) do
            error
          else
            "return #{to_quoted_string(error)}"
          end}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({{:badremote, {_, domain, clauses}, args_types}, mfac, expr, context}) do
    domain = domain(domain, clauses)
    {mod, fun, arity, converter} = mfac
    meta = elem(expr, 1)

    {banner, traces} =
      case Keyword.get(meta, :type_check) do
        :interpolation ->
          {_, _, [arg]} = expr

          {"""
           incompatible value given to string interpolation:

               #{expr_to_string(arg) |> indent(4)}

           it has type:
           """, collect_traces(expr, context)}

        :generator ->
          {:<-, _, [_, arg]} = expr

          {"""
           incompatible value given to for-comprehension:

               #{expr_to_string(expr) |> indent(4)}

           it has type:
           """, collect_traces(arg, context)}

        :into ->
          {"""
           incompatible value given to :into option in for-comprehension:

               into: #{expr_to_string(expr) |> indent(4)}

           it has type:
           """, collect_traces(expr, context)}

        _ ->
          mfa_or_fa = if mod, do: Exception.format_mfa(mod, fun, arity), else: "#{fun}/#{arity}"

          {"""
           incompatible types given to #{mfa_or_fa}:

               #{expr_to_string(expr) |> indent(4)}

           given types:
           """, collect_traces(expr, context)}
      end

    {explanation, impls} =
      cond do
        reason = empty_arg_reason(converter.(args_types)) ->
          {reason, ""}

        Code.ensure_loaded?(mod) and
            Keyword.has_key?(mod.module_info(:attributes), :__protocol__) ->
          if function_exported?(mod, :__protocol__, 1) and
               mod.__protocol__(:impls) == {:consolidated, []} do
            {"""
             but the #{inspect(mod)} protocol was not yet implemented \
             for any type and therefore will always fail.

             This warning will disappear once you define a implementation. \
             If the protocol is part of a library, you may define a dummy \
             implementation for development/test.
             """, ""}
          else
            fix =
              case mod do
                String.Chars ->
                  """
                  You either passed the wrong value or you must:

                  1. convert the given value to a string explicitly
                     (use inspect/1 if you want to convert any data structure to a string)
                  2. implement the String.Chars protocol
                  """

                Enumerable ->
                  """
                  You either passed the wrong value or you must:

                  1. convert the given value to an Enumerable explicitly
                  2. implement the Enumerable protocol
                  """

                _ ->
                  """
                  You either passed the wrong value or you forgot to implement the protocol.
                  """
              end

            # The protocol has, at the moment, simplified clauses, so we build the complete one
            clauses =
              case mod.__protocol__(:impls) do
                {:consolidated, mods} ->
                  domain = mods |> Enum.map(&Module.Types.Of.impl/1) |> Enum.reduce(&union/2)
                  [{[domain], dynamic()}]

                _ ->
                  clauses
              end

            {"""
             but expected a type that implements the #{inspect(mod)} protocol.
             #{fix}\
             """,
             """

             #{hint()} the #{inspect(mod)} protocol is implemented for the following types:
             #{clauses_args_to_quoted_string(clauses, converter, collapse_structs: true)}
             """}
          end

        true ->
          {"""
           but expected one of:
           #{clauses_args_to_quoted_string(clauses, converter, [])}
           """, ""}
      end

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          banner,
          """

              #{args_to_quoted_string(args_types, domain, converter) |> indent(4)}

          """,
          explanation,
          format_traces(traces),
          impls
        ])
    }
  end

  def format_diagnostic({{:mismatched_comparison, left, right}, mfac, expr, context}) do
    {_, name, _, _} = mfac
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison between distinct types found:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{type_comparison_to_string(name, left, right) |> indent(4)}
          """,
          format_traces(traces),
          """

          While Elixir can compare across all types, you are comparing \
          across types which are always disjoint, and the result is either \
          always true or always false
          """
        ])
    }
  end

  def format_diagnostic({{:struct_comparison, left, right}, mfac, expr, context}) do
    {_, name, _, _} = mfac
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison with structs found:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{type_comparison_to_string(name, left, right) |> indent(4)}
          """,
          format_traces(traces),
          """

          Comparison operators (>, <, >=, <=, min, and max) perform structural \
          and not semantic comparison. Comparing with a struct won't give meaningful \
          results. Structs that can be compared typically define a compare/2 function \
          within their modules that can be used for semantic comparison.
          """
        ])
    }
  end

  def format_diagnostic({:undefined, :badmodule, module, fun, arity}) do
    top =
      if fun == :__struct__ and arity == 0 do
        "struct #{inspect(module)}"
      else
        Exception.format_mfa(module, fun, arity)
      end

    %{
      message:
        IO.iodata_to_binary([
          top,
          " is undefined (module ",
          inspect(module),
          " is not available or is yet to be defined)",
          UndefinedFunctionError.hint_for_missing_module(module, fun, arity)
        ]),
      group: true
    }
  end

  def format_diagnostic({:undefined, {:badfunction, _}, module, :__struct__, 0}) do
    %{
      message:
        "struct #{inspect(module)} is undefined (there is such module but it does not define a struct)",
      group: true
    }
  end

  def format_diagnostic({:undefined, {:badfunction, _}, module, fun, arity}) do
    _ = Code.ensure_loaded(module)

    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is undefined or private",
          UndefinedFunctionError.hint_for_loaded_module(module, fun, arity)
        ]),
      group: true
    }
  end

  def format_diagnostic({:deprecated, module, fun, arity, reason}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is deprecated. ",
          reason
        ]),
      group: true
    }
  end

  defp empty_arg_reason(args_types) do
    if i = Enum.find_index(args_types, &empty?/1) do
      """
      the #{integer_to_ordinal(i + 1)} argument is empty (often represented as none()), \
      most likely because it is the result of an expression that always fails, such as \
      a `raise` or a previous invalid call. This causes any function called with this \
      value to fail
      """
    end
  end

  defp pluralize(1, singular, _), do: "1 #{singular}"
  defp pluralize(i, _, plural), do: "#{i} #{plural}"

  defp mfac({{:., _, [mod, fun]}, _, args}, _mod, _fun, _arity)
       when is_atom(mod) and is_atom(fun) do
    {mod, fun, args, converter} = :elixir_rewrite.erl_to_ex(mod, fun, args)
    {mod, fun, length(args), converter}
  end

  defp mfac(_, mod, fun, arity)
       when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    {mod, fun, arity, & &1}
  end

  ## Algebra helpers

  alias Inspect.Algebra, as: IA

  defp type_comparison_to_string(fun, left, right) do
    {_, fun, _, _} = :elixir_rewrite.erl_to_ex(:erlang, fun, [left, right])

    {fun, [], [to_quoted(left, collapse_structs: true), to_quoted(right, collapse_structs: true)]}
    |> Code.Formatter.to_algebra()
    |> Inspect.Algebra.format(98)
    |> IO.iodata_to_binary()
  end

  defp clauses_args_to_quoted_string([{args, _return}], converter, opts) do
    "\n    " <> (clause_args_to_quoted_string(args, converter, opts) |> indent(4))
  end

  defp clauses_args_to_quoted_string(clauses, converter, opts) do
    clauses
    |> Enum.with_index(fn {args, _return}, index ->
      """

      ##{index + 1}
      #{clause_args_to_quoted_string(args, converter, opts)}\
      """
      |> indent(4)
    end)
    |> Enum.join("\n")
  end

  defp clause_args_to_quoted_string(args, converter, opts) do
    docs = Enum.map(args, &(&1 |> to_quoted(opts) |> Code.Formatter.to_algebra()))
    args_docs_to_quoted_string(converter.(docs))
  end

  defp args_to_quoted_string(args_types, domain, converter) do
    docs =
      Enum.zip_with(args_types, domain, fn actual, expected ->
        if compatible?(actual, expected) or not has_simple_difference?(actual, expected) do
          actual |> to_quoted() |> Code.Formatter.to_algebra()
        else
          common = intersection(actual, expected)

          uncommon_doc =
            difference(actual, expected)
            |> to_quoted()
            |> Code.Formatter.to_algebra()
            |> ansi_red()

          if empty?(common) do
            uncommon_doc
          else
            common_doc = common |> to_quoted() |> Code.Formatter.to_algebra()
            IA.glue(IA.concat(uncommon_doc, " or"), IA.nest(common_doc, 2))
          end
        end
      end)

    args_docs_to_quoted_string(converter.(docs))
  end

  @composite_types non_empty_list(term(), term())
                   |> union(tuple())
                   |> union(open_map())
                   |> union(fun())

  # If actual/expected have a composite type, computing the
  # `intersection(actual, expected) or difference(actual, expected)`
  # can lead to an explosion of terms that actually make debugging
  # harder. So we check that at least one of the two operations
  # return none() (i.e. actual is a subtype or they are disjoint).
  defp has_simple_difference?(actual, expected) do
    composite_types = intersection(actual, @composite_types)
    subtype?(composite_types, expected) or disjoint?(composite_types, expected)
  end

  defp ansi_red(doc) do
    if IO.ANSI.enabled?() do
      IA.concat(IA.color(doc, IO.ANSI.red()), IA.color(IA.empty(), IO.ANSI.reset()))
    else
      IA.concat(["-", doc, "-"])
    end
  end

  defp args_docs_to_quoted_string(docs) do
    doc = IA.fold(docs, fn doc, acc -> IA.glue(IA.concat(doc, ","), acc) end)

    wrapped_docs =
      case docs do
        [_] -> IA.concat("(", IA.concat(doc, ")"))
        _ -> IA.group(IA.glue(IA.nest(IA.glue("(", "", doc), 2), "", ")"))
      end

    wrapped_docs
    |> IA.format(98)
    |> IO.iodata_to_binary()
    |> case do
      "(\n" <> _ = multiple_lines -> multiple_lines
      single_line -> binary_slice(single_line, 1..-2//1)
    end
  end
end
