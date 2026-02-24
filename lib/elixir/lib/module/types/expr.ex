# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Expr do
  @moduledoc false

  alias Module.Types.{Apply, Of, Pattern}
  import Module.Types.{Helpers, Descr}

  14 = length(Macro.Env.__info__(:struct))

  aliases = list(tuple([atom(), atom()]))
  functions_and_macros = list(tuple([atom(), list(tuple([atom(), integer()]))]))
  list_of_modules = list(atom())

  @try_catch atom([:error, :exit, :throw])
  @atom_true atom([true])

  @caller closed_map(
            __struct__: atom([Macro.Env]),
            aliases: aliases,
            context: atom([:match, :guard, nil]),
            context_modules: list_of_modules,
            file: binary(),
            function: union(tuple([atom(), integer()]), atom([nil])),
            functions: functions_and_macros,
            lexical_tracker: union(pid(), atom([nil])),
            line: integer(),
            macro_aliases: aliases,
            macros: functions_and_macros,
            module: atom(),
            requires: list_of_modules,
            tracers: list_of_modules,
            versioned_vars: open_map()
          )

  # An annotation for terms where the reverse arrow is not yet fully defined.
  # Also revisit all users of dynamic() in this module in a later date.
  @pending term()

  # We do not make exception dynamic on purpose. If you do a blank rescue,
  # then we will assume you need to statically handle all possible exceptions.
  @exception open_map(__struct__: atom(), __exception__: @atom_true)

  args_or_arity = union(list(term()), integer())

  extra_info =
    list(
      tuple([atom([:file]), list(integer())])
      |> union(tuple([atom([:line]), integer()]))
      |> union(tuple([atom([:error_info]), open_map()]))
    )

  @stacktrace list(
                union(
                  tuple([atom(), atom(), args_or_arity, extra_info]),
                  tuple([fun(), args_or_arity, extra_info])
                )
              )

  # :atom
  def of_expr(atom, _expected, _expr, _stack, context) when is_atom(atom),
    do: {atom([atom]), context}

  # 12
  def of_expr(literal, _expected, _expr, _stack, context) when is_integer(literal),
    do: {integer(), context}

  # 1.2
  def of_expr(literal, _expected, _expr, _stack, context) when is_float(literal),
    do: {float(), context}

  # "..."
  def of_expr(literal, _expected, _expr, _stack, context) when is_binary(literal),
    do: {binary(), context}

  # #PID<...>
  def of_expr(literal, _expected, _expr, _stack, context) when is_pid(literal),
    do: {pid(), context}

  # []
  def of_expr([], _expected, _expr, _stack, context),
    do: {empty_list(), context}

  # [expr, ...]
  def of_expr(list, expected, expr, stack, context) when is_list(list) do
    {prefix, suffix} = unpack_list(list, [])

    hd_type =
      case list_hd(expected) do
        {:ok, type} -> type
        _ -> term()
      end

    {prefix, context} = Enum.map_reduce(prefix, context, &of_expr(&1, hd_type, expr, stack, &2))

    {suffix, context} =
      if suffix == [] do
        {empty_list(), context}
      else
        tl_type =
          case list_tl(expected) do
            {:ok, type} -> type
            :badnonemptylist -> term()
          end

        of_expr(suffix, tl_type, expr, stack, context)
      end

    {non_empty_list(Enum.reduce(prefix, &union/2), suffix), context}
  end

  # {left, right}
  def of_expr({left, right}, expected, expr, stack, context) do
    of_tuple([left, right], expected, expr, stack, context)
  end

  # {...}
  def of_expr({:{}, _meta, exprs}, expected, expr, stack, context) do
    of_tuple(exprs, expected, expr, stack, context)
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args}, _expected, _expr, stack, context) do
    args
    |> Of.bitstring(:expr, stack, context)
    |> dynamic_unless_static(stack)
  end

  def of_expr({:__CALLER__, _meta, var_context}, _expected, _expr, _stack, context)
      when is_atom(var_context) do
    {@caller, context}
  end

  def of_expr({:__STACKTRACE__, _meta, var_context}, _expected, _expr, _stack, context)
      when is_atom(var_context) do
    {@stacktrace, context}
  end

  # left = right
  def of_expr({:=, _, [left_expr, right_expr]} = match, expected, expr, stack, context) do
    {left_expr, right_expr} = repack_match(left_expr, right_expr)

    case left_expr do
      # We do not raise on underscore in case someone writes _ = raise "omg"
      {:_, _, ctx} when is_atom(ctx) ->
        of_expr(right_expr, expected, expr, stack, context)

      _ ->
        type_fun = fn pattern_type, context ->
          # See if we can use the expected type to further refine the pattern type,
          # if we cannot, use the pattern type as that will fail later on.
          {_ok_or_error, type} = compatible_intersection(dynamic(pattern_type), expected)
          of_expr(right_expr, type, expr, stack, context)
        end

        Pattern.of_match(left_expr, type_fun, match, stack, context)
    end
  end

  # %{map | ...}
  # TODO: Once we support typed structs, we need to type check them here.
  def of_expr({:%{}, meta, [{:|, _, [map, args]}]} = update, expected, expr, stack, context) do
    # Theoretically we cannot process entries out of order but,
    # because all variables are versioned, and Elixir does not
    # allow variables defined on the left side of | to be available
    # on the right side, this is safe.
    {pairs_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context ->
        {key_type, context} = of_expr(key, term(), expr, stack, context)
        {value_type, context} = of_expr(value, term(), expr, stack, context)
        {{key_type, value_type}, context}
      end)

    # The only information we can attach to the expected types is that
    # certain keys are expected.
    expected_pairs =
      Enum.flat_map(pairs_types, fn {key_type, _value_type} ->
        case atom_fetch(key_type) do
          {:finite, [key]} -> [{key, term()}]
          _ -> []
        end
      end)

    expected = intersection(expected, open_map(expected_pairs))
    {map_type, context} = of_expr(map, expected, expr, stack, context)

    try do
      Enum.reduce(pairs_types, map_type, fn {key_type, value_type}, acc ->
        case literal_map_update(acc, key_type, value_type) do
          {:ok, descr} -> descr
          {:badkey, key} -> throw({:badkey, map_type, key, update, context})
          {:baddomain, domain} -> throw({:baddomain, map_type, domain, update, context})
          :badmap -> throw({:badmap, map_type, update, context})
        end
      end)
    catch
      error -> {error_type(), error(__MODULE__, error, meta, stack, context)}
    else
      map -> {map, context}
    end
  end

  # %Struct{map | ...}
  def of_expr(
        {:%, meta, [module, {:%{}, _, [{:|, _, [map, pairs]}]}]} = struct,
        _expected,
        expr,
        stack,
        context
      ) do
    {info, context} = Of.struct_info(module, :expr, meta, stack, context)

    if info do
      # We pass the expected type as `term()` because the struct update
      # operator already expects it to be a map at this point.
      {map_type, context} = of_expr(map, term(), struct, stack, context)

      context =
        with {false, struct_key_type} <- map_fetch_key(map_type, :__struct__),
             {:finite, [^module]} <- atom_fetch(struct_key_type) do
          context
        else
          _ ->
            error(__MODULE__, {:badupdate, map_type, struct, context}, meta, stack, context)
        end

      Enum.reduce(pairs, {map_type, context}, fn {key, value}, {acc, context} ->
        context =
          if Enum.any?(info, &(&1.field == key)) do
            context
          else
            Of.unknown_struct_field(module, key, :expr, meta, stack, context)
          end

        # TODO: Once we support typed structs, we need to type check them here
        {type, context} = of_expr(value, term(), expr, stack, context)

        case map_put_key(acc, key, type) do
          {:ok, acc} -> {acc, context}
          _ -> {acc, context}
        end
      end)
    else
      {error_type(), context}
    end
  end

  # %{...}
  def of_expr({:%{}, _meta, args}, expected, expr, stack, context) do
    Of.closed_map(args, expected, stack, context, &of_expr(&1, &2, expr, &3, &4))
  end

  # %Struct{}
  def of_expr({:%, meta, [module, {:%{}, _, args}]}, expected, expr, stack, context) do
    fun = &of_expr(&1, &2, expr, &3, &4)
    Of.struct_instance(module, args, expected, meta, stack, context, fun)
  end

  # ()
  def of_expr({:__block__, _meta, []}, _expected, _expr, _stack, context) do
    {atom([nil]), context}
  end

  # (expr; expr)
  def of_expr({:__block__, _meta, exprs}, expected, _expr, stack, context) do
    {pre, [post]} = Enum.split(exprs, -1)

    context =
      Enum.reduce(pre, context, fn expr, context ->
        {_, context} = of_expr(expr, term(), expr, stack, context)
        context
      end)

    of_expr(post, expected, post, stack, context)
  end

  def of_expr({:cond, _meta, [[{:do, clauses}]]}, expected, expr, stack, original) do
    clauses
    |> reduce_non_empty({none(), original}, fn
      {:->, meta, [[head], body]}, {acc, context}, last? ->
        {head_type, context} = of_expr(head, @pending, head, stack, context)

        context =
          if is_warning(stack) do
            case truthiness(head_type) do
              :always_true when not last? ->
                warning = {:badcond, "always match", head_type, head, context}
                warn(__MODULE__, warning, meta, stack, context)

              :always_false ->
                warning = {:badcond, "never match", head_type, head, context}
                warn(__MODULE__, warning, meta, stack, context)

              _ ->
                context
            end
          else
            context
          end

        {body_type, context} = of_expr(body, expected, expr, stack, context)
        {union(body_type, acc), Of.reset_vars(context, original)}
    end)
    |> dynamic_unless_static(stack)
  end

  def of_expr({:case, meta, [case_expr, [{:do, clauses}]]}, expected, _expr, stack, context) do
    {case_type, context} = of_expr(case_expr, @pending, case_expr, stack, context)
    info = {:case, meta, case_expr, case_type}

    added_meta =
      if Macro.quoted_literal?(case_expr) do
        [generated: true]
      else
        case_expr |> get_meta() |> Keyword.take([:generated])
      end

    # If the expression is generated or the construct is a literal,
    # it is most likely a macro code. However, if no clause is matched,
    # we should still check for that.
    if added_meta != [] do
      for {:->, meta, args} <- clauses, do: {:->, [generated: true] ++ meta, args}
    else
      clauses
    end
    |> of_redundant_clauses([case_type], expected, info, stack, context, none())
    |> dynamic_unless_static(stack)
  end

  # fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, _expected, _expr, stack, context) do
    [{:->, _, [head, _]} | _] = clauses
    {patterns, _guards} = extract_head(head)
    domain = Enum.map(patterns, fn _ -> dynamic() end)

    {acc, context} =
      of_clauses_fun(clauses, domain, @pending, nil, :fn, stack, context, [], fn
        trees, body, context, acc ->
          args = Pattern.of_domain(trees, stack, context)
          add_inferred(acc, args, body)
      end)

    {fun_from_inferred_clauses(acc), context}
  end

  def of_expr({:try, meta, [[do: body] ++ blocks]}, expected, expr, stack, original) do
    {after_block, blocks} = Keyword.pop(blocks, :after)
    {else_block, blocks} = Keyword.pop(blocks, :else)

    {type, context} =
      if else_block do
        {type, context} = of_expr(body, @pending, body, stack, original)
        info = {:try_else, meta, body, type}
        of_redundant_clauses(else_block, [type], expected, info, stack, context, none())
      else
        of_expr(body, expected, expr, stack, original)
      end

    {type, context} =
      blocks
      |> Enum.reduce({type, Of.reset_vars(context, original)}, fn
        {:rescue, clauses}, acc_context ->
          Enum.reduce(clauses, acc_context, fn
            {:->, _, [[{:in, meta, [var, exceptions]} = expr], body]}, {acc, context} ->
              {type, context} =
                of_rescue(var, exceptions, body, expr, :rescue, meta, stack, context)

              {union(type, acc), context}

            {:->, meta, [[var], body]}, {acc, context} ->
              {type, context} =
                of_rescue(var, [], body, var, :anonymous_rescue, meta, stack, context)

              {union(type, acc), context}
          end)

        {:catch, clauses}, {acc, context} ->
          args = [@try_catch, dynamic()]
          of_redundant_clauses(clauses, args, expected, :try_catch, stack, context, acc)
      end)
      |> dynamic_unless_static(stack)

    if after_block do
      {_type, context} = of_expr(after_block, term(), after_block, stack, context)
      {type, context}
    else
      {type, context}
    end
  end

  @timeout_type union(integer(), atom([:infinity]))

  def of_expr({:receive, _meta, [blocks]}, expected, expr, stack, original) do
    blocks
    |> Enum.reduce({none(), original}, fn
      {:do, {:__block__, _, []}}, acc_context ->
        acc_context

      {:do, clauses}, {acc, context} ->
        of_redundant_clauses(clauses, [dynamic()], expected, :receive, stack, context, acc)

      {:after, [{:->, meta, [[timeout], body]}] = after_expr}, {acc, context} ->
        {timeout_type, context} = of_expr(timeout, @timeout_type, after_expr, stack, context)
        {body_type, context} = of_expr(body, expected, expr, stack, context)

        if compatible?(timeout_type, @timeout_type) do
          {union(body_type, acc), Of.reset_vars(context, original)}
        else
          error = {:badtimeout, timeout_type, timeout, context}
          {union(body_type, acc), error(__MODULE__, error, meta, stack, context)}
        end
    end)
    |> dynamic_unless_static(stack)
  end

  def of_expr({:for, meta, [_ | _] = args}, expected, expr, stack, context) do
    {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)
    context = Enum.reduce(clauses, context, &for_clause(&1, stack, &2))

    # We don't need to type check uniq, as it is a compile-time boolean.
    # We handle reduce and into accordingly instead.
    if Keyword.has_key?(opts, :reduce) do
      reduce = Keyword.fetch!(opts, :reduce)
      {reduce_type, context} = of_expr(reduce, expected, expr, stack, context)
      # TODO: We need to type check against dynamic() instead of using reduce_type
      # because this is recursive. We need to infer the block type first.
      of_clauses(block, [dynamic()], expected, expr, :for_reduce, stack, context, reduce_type)
    else
      # TODO: Use the collectable protocol for the output
      into = Keyword.get(opts, :into, [])
      {into_type, into_kind, context} = for_into(into, meta, stack, context)
      {block_type, context} = of_expr(block, @pending, block, stack, context)

      case into_kind do
        :bitstring ->
          case compatible_intersection(block_type, bitstring()) do
            {:ok, intersection} ->
              {return_union(into_type, intersection, stack), context}

            {:error, _} ->
              error = {:badbitbody, block_type, block, context}
              {error_type(), error(__MODULE__, error, meta, stack, context)}
          end

        :non_empty_list ->
          {return_union(into_type, non_empty_list(block_type), stack), context}

        :none ->
          {into_type, context}
      end
    end
  end

  # TODO: with pat <- expr do expr end
  def of_expr({:with, _meta, [_ | _] = clauses}, _expected, _expr, stack, original) do
    {clauses, [options]} = Enum.split(clauses, -1)
    context = Enum.reduce(clauses, original, &with_clause(&1, stack, &2))
    context = Enum.reduce(options, context, &with_option(&1, stack, &2, original))
    {dynamic(), context}
  end

  def of_expr({{:., _, [fun]}, _, args} = call, _expected, _expr, stack, context) do
    {fun_type, context} = of_expr(fun, dynamic(fun(length(args))), call, stack, context)

    # TODO: Perform inference based on the strong domain of a function
    {args_types, context} =
      Enum.map_reduce(args, context, &of_expr(&1, @pending, &1, stack, &2))

    Apply.fun(fun_type, args_types, call, stack, context)
  end

  def of_expr({{:., _, [callee, key_or_fun]}, meta, []} = call, expected, expr, stack, context)
      when not is_atom(callee) and is_atom(key_or_fun) do
    if Keyword.get(meta, :no_parens, false) do
      {type, context} = of_expr(callee, open_map([{key_or_fun, expected}]), expr, stack, context)
      Of.map_fetch(call, type, key_or_fun, stack, context)
    else
      {type, context} = of_expr(callee, atom(), call, stack, context)
      {mods, context} = Of.modules(type, key_or_fun, 0, [:dot], call, meta, stack, context)
      apply_many(mods, key_or_fun, [], expected, call, stack, context)
    end
  end

  def of_expr({{:., _, [remote, name]}, meta, args} = call, expected, _expr, stack, context) do
    {remote_type, context} = of_expr(remote, atom(), call, stack, context)
    {mods, context} = Of.modules(remote_type, name, length(args), call, meta, stack, context)
    apply_many(mods, name, args, expected, call, stack, context)
  end

  # &Foo.bar/1
  def of_expr(
        {:&, _, [{:/, _, [{{:., _, [remote, name]}, meta, []}, arity]}]} = call,
        _expected,
        _expr,
        stack,
        context
      )
      when is_atom(name) and is_integer(arity) do
    {remote_type, context} = of_expr(remote, atom(), call, stack, context)
    {mods, context} = Of.modules(remote_type, name, arity, call, meta, stack, context)
    Apply.remote_capture(mods, name, arity, meta, stack, context)
  end

  # &foo/1
  def of_expr({:&, _meta, [{:/, _, [{fun, meta, _}, arity]}]}, _expected, _expr, stack, context) do
    Apply.local_capture(fun, arity, meta, stack, context)
  end

  # Super
  def of_expr({:super, meta, args} = call, expected, _expr, stack, context) when is_list(args) do
    {_kind, fun} = Keyword.fetch!(meta, :super)
    Apply.local(fun, args, expected, call, stack, context, &of_expr/5)
  end

  # Local calls
  def of_expr({fun, _meta, args} = call, expected, _expr, stack, context)
      when is_atom(fun) and is_list(args) do
    Apply.local(fun, args, expected, call, stack, context, &of_expr/5)
  end

  # var
  def of_expr({_, meta, _} = var, expected, expr, stack, context) when is_var(var) do
    version = Keyword.fetch!(meta, :version)
    {type, context} = Of.refine_body_var(version, expected, expr, stack, context)
    {type, Pattern.of_changed([version], stack, context)}
  end

  ## Tuples

  defp of_tuple(elems, expected, expr, stack, context) do
    of_tuple(elems, 0, [], expected, expr, stack, context)
  end

  defp of_tuple([elem | elems], index, acc, expected, expr, stack, context) do
    expr_expected =
      case tuple_fetch(expected, index) do
        {_, type} -> type
        _ -> term()
      end

    {type, context} = of_expr(elem, expr_expected, expr, stack, context)
    of_tuple(elems, index + 1, [type | acc], expected, expr, stack, context)
  end

  defp of_tuple([], _index, acc, _expected, _expr, _stack, context) do
    {tuple(Enum.reverse(acc)), context}
  end

  ## Try

  defp of_rescue(var, exceptions, body, expr, info, meta, stack, original) do
    args = [__exception__: @atom_true]

    {structs, context} =
      Enum.map_reduce(exceptions, original, fn exception, context ->
        # Exceptions are not validated in the compiler,
        # to avoid export dependencies. So we do it here.
        {info, context} = Of.struct_info(exception, :expr, meta, stack, context)

        if info do
          {Of.struct_type(exception, info, args), context}
        else
          {error_type(), context}
        end
      end)

    context =
      case var do
        {:_, _, _} ->
          context

        _ ->
          expected = if structs == [], do: @exception, else: Enum.reduce(structs, &union/2)
          expr = {:__block__, [type_check: info], [expr]}
          context = Of.declare_var(var, context)
          {_ok?, _type, context} = Of.refine_head_var(var, expected, expr, stack, context)
          context
      end

    {type, context} = of_expr(body, @pending, body, stack, context)
    {type, Of.reset_vars(context, original)}
  end

  ## Comprehensions

  defp for_clause({:<-, meta, [left, right]}, stack, context) do
    expr = {:<-, [type_check: :generator] ++ meta, [left, right]}
    {pattern, guards} = extract_head([left])

    {_type, context} =
      Apply.remote(Enumerable, :count, [right], dynamic(), expr, stack, context, &of_expr/5)

    Pattern.of_generator(pattern, guards, dynamic(), :for, expr, stack, context)
  end

  defp for_clause({:<<>>, _, [{:<-, meta, [left, right]}]} = expr, stack, context) do
    {right_type, context} = of_expr(right, bitstring(), expr, stack, context)
    context = Pattern.of_generator(left, [], bitstring(), :for, expr, stack, context)

    if compatible?(right_type, bitstring()) do
      context
    else
      error = {:badbitgenerator, right_type, right, context}
      error(__MODULE__, error, meta, stack, context)
    end
  end

  defp for_clause(expr, stack, context) do
    {_type, context} = of_expr(expr, term(), expr, stack, context)
    context
  end

  @into_compile union(bitstring(), empty_list())

  defp for_into([], _meta, _stack, context),
    do: {empty_list(), :non_empty_list, context}

  defp for_into(binary, _meta, _stack, context) when is_binary(binary),
    do: {binary(), :bitstring, context}

  defp for_into(into, meta, stack, context) do
    meta =
      case into do
        {_, meta, _} -> meta
        _ -> meta
      end

    expr = {:__block__, [type_check: :into] ++ meta, [into]}

    {info, [domain], context} =
      Apply.remote_domain(Collectable, :into, [into], term(), meta, stack, context)

    {type, context} = of_expr(into, domain, expr, stack, context)

    # We use subtype? instead of compatible because we want to handle
    # only bitstring/list, even if a dynamic with something else is given.
    if subtype?(type, @into_compile) do
      case {bitstring_type?(type), empty_list_type?(type)} do
        # If they can be both be true, then we don't know
        # what the contents of the block are for
        {true, true} ->
          type = union(bitstring(), list(term()))
          {if(gradual?(type), do: dynamic(type), else: type), :none, context}

        {false, true} ->
          {type, :non_empty_list, context}

        {true, false} ->
          {type, :bitstring, context}
      end
    else
      {_type, context} =
        Apply.remote_apply(info, Collectable, :into, [type], expr, stack, context)

      {dynamic(), :none, context}
    end
  end

  defp return_union(left, right, stack) do
    Apply.return(union(left, right), [left, right], stack)
  end

  ## With

  defp with_clause({:<-, _meta, [left, right]} = expr, stack, context) do
    {pattern, guards} = extract_head([left])
    {_type, context} = of_expr(right, @pending, right, stack, context)
    Pattern.of_generator(pattern, guards, dynamic(), :with, expr, stack, context)
  end

  defp with_clause(expr, stack, context) do
    {_type, context} = of_expr(expr, @pending, expr, stack, context)
    context
  end

  defp with_option({:do, body}, stack, context, original) do
    {_type, context} = of_expr(body, @pending, body, stack, context)
    Of.reset_vars(context, original)
  end

  defp with_option({:else, clauses}, stack, context, _original) do
    {_, context} =
      of_clauses(clauses, [dynamic()], @pending, nil, :with_else, stack, context, none())

    context
  end

  ## General helpers

  defp apply_many([], fun, args, expected, expr, stack, context) do
    Apply.remote(fun, args, expected, expr, stack, context, &of_expr/5)
  end

  defp apply_many([mod], fun, args, expected, expr, stack, context) do
    Apply.remote(mod, fun, args, expected, expr, stack, context, &of_expr/5)
  end

  defp apply_many(mods, fun, args, expected, call, stack, context) do
    {remote, meta, _} = call

    Of.with_conditional_vars(mods, none(), call, stack, context, fn mod, acc, context ->
      expr = {remote, [type_check: {:invoked_as, mod, fun, length(args)}] ++ meta, args}
      {type, context} = Apply.remote(mod, fun, args, expected, expr, stack, context, &of_expr/5)
      {union(acc, type), context}
    end)
  end

  defp reduce_non_empty([last], acc, fun),
    do: fun.(last, acc, true)

  defp reduce_non_empty([head | tail], acc, fun),
    do: reduce_non_empty(tail, fun.(head, acc, false), fun)

  defp dynamic_unless_static({_, _} = output, %{mode: :static}), do: output
  defp dynamic_unless_static({type, context}, %{mode: _}), do: {dynamic(type), context}

  defp of_clauses(clauses, domain, expected, expr, info, stack, context, acc) do
    fun = fn _trees, result, _context, acc -> union(result, acc) end
    of_clauses_fun(clauses, domain, expected, expr, info, stack, context, acc, fun)
  end

  defp of_clauses_fun(clauses, domain, expected, expr, info, stack, original, acc, fun) do
    %{failed: failed?} = original

    Enum.reduce(clauses, {acc, original}, fn {:->, meta, [head, body]}, {acc, context} ->
      {failed?, context} = reset_failed(context, failed?)
      {patterns, guards} = extract_head(head)

      {trees, _precise?, context} =
        Pattern.of_head(patterns, guards, domain, info, meta, stack, context)

      {result, context} = of_expr(body, expected, expr || body, stack, context)

      {fun.(trees, result, context, acc),
       context |> set_failed(failed?) |> Of.reset_vars(original)}
    end)
  end

  defp of_redundant_clauses(clauses, domain, expected, clause_info, stack, original, acc) do
    %{failed: failed?} = original

    {result, _previous, context} =
      Enum.reduce(clauses, {acc, [], original}, fn
        {:->, meta, [head, body]} = clause, {acc, previous, context} ->
          {failed?, context} = reset_failed(context, failed?)
          {patterns, guards} = extract_head(head)
          info = {clause_info, head, previous}

          {trees, precise?, context} =
            Pattern.of_head(patterns, guards, domain, previous, info, meta, stack, context)

          # It failed, let's try to detect if it was due a previous or current clause.
          # The current clause will be easier to understand, so we prefer that
          {trees, precise?, context} =
            if context.failed and previous != [] do
              info = {clause_info, head, []}

              case Pattern.of_head(patterns, guards, domain, info, meta, stack, context) do
                {_, _, %{failed: true}} = result -> result
                _ -> {trees, precise?, context}
              end
            else
              {trees, precise?, context}
            end

          {previous, context} =
            if context.failed do
              {previous, context}
            else
              clause_type =
                Enum.map(trees, fn {tree, _, _} ->
                  tree
                  |> Pattern.of_pattern_tree(stack, context)
                  |> upper_bound()
                end)

              cond do
                stack.mode != :infer and previous != [] and
                    Pattern.args_subtype?(clause_type, previous) ->
                  stack = %{stack | meta: meta}
                  {previous, Pattern.badpattern_error(clause, nil, info, stack, context)}

                precise? ->
                  {[clause_type | previous], context}

                true ->
                  {previous, context}
              end
            end

          {result, context} = of_expr(body, expected, body, stack, context)

          {union(result, acc), previous,
           context |> set_failed(failed?) |> Of.reset_vars(original)}
      end)

    {result, context}
  end

  defp reset_failed(%{failed: true} = context, false), do: {true, %{context | failed: false}}
  defp reset_failed(context, _), do: {false, context}

  defp set_failed(%{failed: false} = context, true), do: %{context | failed: true}
  defp set_failed(context, _bool), do: context

  defp extract_head([{:when, _meta, args}]) do
    case Enum.split(args, -1) do
      {patterns, [guards]} -> {patterns, flatten_when(guards)}
      {patterns, []} -> {patterns, []}
    end
  end

  defp extract_head(other) do
    {other, []}
  end

  defp flatten_when({:when, _meta, [left, right]}), do: [left | flatten_when(right)]
  defp flatten_when(other), do: [other]

  defp repack_match(left_expr, {:=, meta, [new_left, new_right]}),
    do: repack_match({:=, meta, [left_expr, new_left]}, new_right)

  defp repack_match(left_expr, right_expr),
    do: {left_expr, right_expr}

  defp add_inferred([{args, existing_return} | tail], args, return),
    do: [{args, union(existing_return, return)} | tail]

  defp add_inferred([head | tail], args, return),
    do: [head | add_inferred(tail, args, return)]

  defp add_inferred([], args, return),
    do: [{args, return}]

  defp literal_map_update(descr, key_descr, value_descr) do
    case map_update(descr, key_descr, value_descr, false, false) do
      {_type, descr, []} -> {:ok, descr}
      {_, _, [error | _]} -> error
      :badmap -> :badmap
      {:error, [error | _]} -> error
      {:error, []} -> {:baddomain, key_descr}
    end
  end

  ## Warning formatting

  def format_diagnostic({:badclause, {:case, meta, type, expr}, [head], context}) do
    {expr, message} =
      if meta[:type_check] == :expr do
        {expr,
         """
         the following conditional expression will always evaluate to #{to_quoted_string(type)}:

             #{expr_to_string(expr) |> indent(4)}
         """}
      else
        {head, "the following clause has no effect because a previous clause will always match\n"}
      end

    %{
      details: %{typing_traces: collect_traces(expr, context)},
      message: message
    }
  end

  def format_diagnostic({:badupdate, type, expr, context}) do
    {:%, _, [module, {:%{}, _, [{:|, _, [map, _]}]}]} = expr
    traces = collect_traces(map, context)

    fix =
      case map do
        {var, meta, context} when is_atom(var) and is_atom(context) ->
          if capture = meta[:capture] do
            "instead of using &#{capture}, you must define an anonymous function, define a variable and pattern match on \"%#{inspect(module)}{}\""
          else
            "when defining the variable \"#{Macro.to_string(map)}\", you must also pattern match on \"%#{inspect(module)}{}\""
          end

        _ ->
          "you must assign \"#{Macro.to_string(map)}\" to variable and pattern match on \"%#{inspect(module)}{}\""
      end

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          a struct for #{inspect(module)} is expected on struct update:

              #{expr_to_string(expr, collapse_structs: false) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces),
          """

          #{fix}
          """
        ])
    }
  end

  def format_diagnostic({:badmap, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a map within map update syntax:

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badkey, type, key, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a map with key #{inspect(key)} in map update syntax:

              #{expr_to_string(expr, collapse_structs: false) |> indent(4)}

          but got type:

              #{to_quoted_string(type, collapse_structs: false) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:baddomain, type, key_type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a map with key of type #{to_quoted_string(key_type)} in map update syntax:

              #{expr_to_string(expr, collapse_structs: false) |> indent(4)}

          but got type:

              #{to_quoted_string(type, collapse_structs: false) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badbitgenerator, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected the right side of <- in a binary generator to be a binary (or bitstring):

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badbitbody, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected the body of a for-comprehension with into: binary() (or bitstring()) to be a binary (or bitstring):

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badtimeout, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected "after" timeout given to receive to be an integer:

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badcond, explain, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          this clause in cond will #{explain}:

              #{expr_to_string(expr) |> indent(4)}

          since it has type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end
end
