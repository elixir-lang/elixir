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

  # An annotation for terms where the reverse arrow is not yet fully defined
  @pending term()
  @atom_true atom([true])
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

    if stack.mode == :traversal do
      {_, context} = Enum.map_reduce(prefix, context, &of_expr(&1, term(), expr, stack, &2))
      {_, context} = of_expr(suffix, term(), expr, stack, context)
      {dynamic(), context}
    else
      hd_type =
        case list_hd(expected) do
          {_, type} -> type
          _ -> term()
        end

      {prefix, context} = Enum.map_reduce(prefix, context, &of_expr(&1, hd_type, expr, stack, &2))

      {suffix, context} =
        if suffix == [] do
          {empty_list(), context}
        else
          tl_type =
            case list_tl(expected) do
              {_, type} -> type
              _ -> term()
            end

          of_expr(suffix, tl_type, expr, stack, context)
        end

      {non_empty_list(Enum.reduce(prefix, &union/2), suffix), context}
    end
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
    context = Of.binary(args, :expr, stack, context)
    {binary(), context}
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
      Of.pairs(args, expected, stack, context, &of_expr(&1, &2, expr, &3, &4))

    expected =
      if stack.mode == :traversal do
        expected
      else
        # TODO: Once we introduce domain keys, if we ever find a domain
        # that overlaps atoms, we can only assume optional(atom()) => term(),
        # which is what the `open_map()` below falls back into anyway.
        Enum.reduce_while(pairs_types, expected, fn
          {_, [key], _}, acc ->
            case map_fetch_and_put(acc, key, term()) do
              {_value, acc} -> {:cont, acc}
              _ -> {:halt, open_map()}
            end

          _, _ ->
            {:halt, open_map()}
        end)
      end

    {map_type, context} = of_expr(map, expected, expr, stack, context)

    try do
      Of.permutate_map(pairs_types, stack, fn fallback, keys_to_assert, pairs ->
        # Ensure all keys to assert and all type pairs exist in map
        keys_to_assert = Enum.map(pairs, &elem(&1, 0)) ++ keys_to_assert

        Enum.each(Enum.map(pairs, &elem(&1, 0)) ++ keys_to_assert, fn key ->
          case map_fetch(map_type, key) do
            {_, _} -> :ok
            :badkey -> throw({:badkey, map_type, key, update, context})
            :badmap -> throw({:badmap, map_type, update, context})
          end
        end)

        # If all keys are known is no fallback (i.e. we know all keys being updated),
        # we can update the existing map.
        if fallback == none() do
          Enum.reduce(pairs, map_type, fn {key, type}, acc ->
            case map_fetch_and_put(acc, key, type) do
              {_value, descr} -> descr
              :badkey -> throw({:badkey, map_type, key, update, context})
              :badmap -> throw({:badmap, map_type, update, context})
            end
          end)
        else
          # TODO: Use the fallback type to actually indicate if open or closed.
          # The fallback must be unioned with the result of map_values with all
          # `keys` deleted.
          dynamic(open_map(pairs))
        end
      end)
    catch
      error -> {error_type(), error(__MODULE__, error, meta, stack, context)}
    else
      map -> {map, context}
    end
  end

  # %Struct{map | ...}
  # This syntax is deprecated, so we simply traverse.
  def of_expr(
        {:%, meta, [module, {:%{}, _, [{:|, _, [map, pairs]}]}]} = struct,
        _expected,
        expr,
        stack,
        context
      ) do
    {map_type, context} = of_expr(map, term(), struct, stack, context)

    context =
      if stack.mode == :traversal do
        context
      else
        with {false, struct_key_type} <- map_fetch(map_type, :__struct__),
             {:finite, [^module]} <- atom_fetch(struct_key_type) do
          context
        else
          _ ->
            error(__MODULE__, {:badupdate, map_type, struct, context}, meta, stack, context)
        end
      end

    Enum.reduce(pairs, {map_type, context}, fn {key, value}, {acc, context} ->
      # TODO: Once we support typed structs, we need to type check them here
      {type, context} = of_expr(value, term(), expr, stack, context)

      case map_fetch_and_put(acc, key, type) do
        {_value, acc} -> {acc, context}
        _ -> {acc, context}
      end
    end)
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
        {union(body_type, acc), reset_vars(context, original)}
    end)
    |> dynamic_unless_static(stack)
  end

  def of_expr({:case, meta, [case_expr, [{:do, clauses}]]}, expected, expr, stack, context) do
    {case_type, context} = of_expr(case_expr, @pending, case_expr, stack, context)
    info = {:case, meta, case_type, case_expr}

    # If we are only type checking the expression and the expression is a literal,
    # let's mark it as generated, as it is most likely a macro code. However, if
    # no clause is matched, we should still check for that.
    if Macro.quoted_literal?(case_expr) do
      for {:->, meta, args} <- clauses, do: {:->, [generated: true] ++ meta, args}
    else
      clauses
    end
    |> of_clauses([case_type], expected, expr, info, stack, context, none())
    |> dynamic_unless_static(stack)
  end

  # TODO: fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, _expected, _expr, stack, context) do
    [{:->, _, [head, _]} | _] = clauses
    {patterns, _guards} = extract_head(head)
    domain = Enum.map(patterns, fn _ -> dynamic() end)

    if stack.mode == :traversal do
      {_acc, context} = of_clauses(clauses, domain, @pending, nil, :fn, stack, context, none())
      {dynamic(fun(length(patterns))), context}
    else
      {acc, context} =
        of_clauses_fun(clauses, domain, @pending, nil, :fn, stack, context, [], fn
          trees, body, context, acc ->
            args = Pattern.of_domain(trees, domain, context)
            add_inferred(acc, args, body)
        end)

      {fun_from_overlapping_clauses(acc), context}
    end
  end

  def of_expr({:try, _meta, [[do: body] ++ blocks]}, expected, expr, stack, original) do
    {after_block, blocks} = Keyword.pop(blocks, :after)
    {else_block, blocks} = Keyword.pop(blocks, :else)

    {type, context} =
      if else_block do
        {type, context} = of_expr(body, @pending, body, stack, original)
        info = {:try_else, type}
        of_clauses(else_block, [type], expected, expr, info, stack, context, none())
      else
        of_expr(body, expected, expr, stack, original)
      end

    {type, context} =
      blocks
      |> Enum.reduce({type, reset_vars(context, original)}, fn
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
          of_clauses(clauses, args, expected, expr, :try_catch, stack, context, acc)
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
        of_clauses(clauses, [dynamic()], expected, expr, :receive, stack, context, acc)

      {:after, [{:->, meta, [[timeout], body]}] = after_expr}, {acc, context} ->
        {timeout_type, context} = of_expr(timeout, @timeout_type, after_expr, stack, context)
        {body_type, context} = of_expr(body, expected, expr, stack, context)

        if compatible?(timeout_type, @timeout_type) do
          {union(body_type, acc), reset_vars(context, original)}
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
      {into_wrapper, gradual?, context} = for_into(into, meta, stack, context)
      {block_type, context} = of_expr(block, @pending, block, stack, context)

      for_type =
        for type <- into_wrapper do
          case type do
            :binary -> binary()
            :list -> list(block_type)
            :term -> term()
          end
        end
        |> Enum.reduce(&union/2)

      {if(gradual?, do: dynamic(for_type), else: for_type), context}
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

    Apply.fun_apply(fun_type, args_types, call, stack, context)
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

  # TODO: &Foo.bar/1
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

  # TODO: &foo/1
  def of_expr({:&, _meta, [{:/, _, [{fun, meta, _}, arity]}]}, _expected, _expr, stack, context) do
    Apply.local_capture(fun, arity, meta, stack, context)
  end

  # Super
  def of_expr({:super, meta, args} = call, expected, _expr, stack, context) when is_list(args) do
    {_kind, fun} = Keyword.fetch!(meta, :super)
    apply_local(fun, args, expected, call, stack, context)
  end

  # Local calls
  def of_expr({fun, _meta, args} = call, expected, _expr, stack, context)
      when is_atom(fun) and is_list(args) do
    apply_local(fun, args, expected, call, stack, context)
  end

  # var
  def of_expr(var, expected, expr, stack, context) when is_var(var) do
    case stack do
      %{mode: :traversal} -> {dynamic(), context}
      %{refine_vars: false} -> {Of.var(var, context), context}
      %{} -> Of.refine_body_var(var, expected, expr, stack, context)
    end
  end

  ## Tuples

  defp of_tuple(elems, _expected, expr, %{mode: :traversal} = stack, context) do
    {_types, context} = Enum.map_reduce(elems, context, &of_expr(&1, term(), expr, stack, &2))
    {dynamic(), context}
  end

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
        if Code.ensure_loaded?(exception) and function_exported?(exception, :__struct__, 0) do
          {info, context} = Of.struct_info(exception, meta, stack, context)
          {Of.struct_type(exception, info, args), context}
        else
          # If the exception cannot be found or is invalid, fetch the signature to emit warnings.
          {_, context} = Apply.signature(exception, :__struct__, 0, meta, stack, context)
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
          {_ok?, _type, context} = Of.refine_head_var(var, expected, expr, stack, context)
          context
      end

    {type, context} = of_expr(body, @pending, body, stack, context)
    {type, reset_vars(context, original)}
  end

  ## Comprehensions

  defp for_clause({:<-, meta, [left, right]}, stack, context) do
    expr = {:<-, [type_check: :generator] ++ meta, [left, right]}
    {pattern, guards} = extract_head([left])

    {_type, context} =
      apply_one(Enumerable, :count, [right], dynamic(), expr, stack, context)

    Pattern.of_generator(pattern, guards, dynamic(), :for, expr, stack, context)
  end

  defp for_clause({:<<>>, _, [{:<-, meta, [left, right]}]} = expr, stack, context) do
    {right_type, context} = of_expr(right, binary(), expr, stack, context)
    context = Pattern.of_generator(left, [], binary(), :for, expr, stack, context)

    if compatible?(right_type, binary()) do
      context
    else
      error = {:badbinary, right_type, right, context}
      error(__MODULE__, error, meta, stack, context)
    end
  end

  defp for_clause(expr, stack, context) do
    {_type, context} = of_expr(expr, term(), expr, stack, context)
    context
  end

  @into_compile union(binary(), empty_list())

  defp for_into([], _meta, _stack, context),
    do: {[:list], false, context}

  defp for_into(binary, _meta, _stack, context) when is_binary(binary),
    do: {[:binary], false, context}

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
    # only binary/list, even if a dynamic with something else is given.
    if subtype?(type, @into_compile) do
      case {binary_type?(type), empty_list_type?(type)} do
        {false, true} -> {[:list], gradual?(type), context}
        {true, false} -> {[:binary], gradual?(type), context}
        {_, _} -> {[:binary, :list], gradual?(type), context}
      end
    else
      {_type, context} =
        Apply.remote_apply(info, Collectable, :into, [type], expr, stack, context)

      {[:term], true, context}
    end
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
    reset_vars(context, original)
  end

  defp with_option({:else, clauses}, stack, context, _original) do
    {_, context} =
      of_clauses(clauses, [dynamic()], @pending, nil, :with_else, stack, context, none())

    context
  end

  ## General helpers

  defp apply_local(fun, args, expected, {_, meta, _} = expr, stack, context) do
    {local_info, domain, context} = Apply.local_domain(fun, args, expected, meta, stack, context)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_expr(&1, &2, expr, stack, &3))

    Apply.local_apply(local_info, fun, args_types, expr, stack, context)
  end

  defp apply_one(mod, fun, args, expected, expr, stack, context) do
    {info, domain, context} =
      Apply.remote_domain(mod, fun, args, expected, elem(expr, 1), stack, context)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_expr(&1, &2, expr, stack, &3))

    Apply.remote_apply(info, mod, fun, args_types, expr, stack, context)
  end

  defp apply_many([], fun, args, expected, expr, stack, context) do
    {info, domain} = Apply.remote_domain(fun, args, expected, stack)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_expr(&1, &2, expr, stack, &3))

    Apply.remote_apply(info, nil, fun, args_types, expr, stack, context)
  end

  defp apply_many([mod], fun, args, expected, expr, stack, context) do
    apply_one(mod, fun, args, expected, expr, stack, context)
  end

  defp apply_many(mods, fun, args, expected, {remote, meta, args}, stack, context) do
    {returns, context} =
      Enum.map_reduce(mods, context, fn mod, context ->
        expr = {remote, [type_check: {:invoked_as, mod, fun, length(args)}] ++ meta, args}
        apply_one(mod, fun, args, expected, expr, %{stack | refine_vars: false}, context)
      end)

    {Enum.reduce(returns, &union/2), context}
  end

  defp reduce_non_empty([last], acc, fun),
    do: fun.(last, acc, true)

  defp reduce_non_empty([head | tail], acc, fun),
    do: reduce_non_empty(tail, fun.(head, acc, false), fun)

  defp dynamic_unless_static({_, _} = output, %{mode: :static}), do: output
  defp dynamic_unless_static({type, context}, %{mode: _}), do: {dynamic(type), context}

  defp of_clauses(clauses, domain, expected, expr, info, %{mode: mode} = stack, context, acc) do
    fun =
      if mode == :traversal do
        fn _, _, _, _ -> dynamic() end
      else
        fn _trees, result, _context, acc -> union(result, acc) end
      end

    of_clauses_fun(clauses, domain, expected, expr, info, stack, context, acc, fun)
  end

  defp of_clauses_fun(clauses, domain, expected, expr, info, stack, original, acc, fun) do
    %{failed: failed?} = original

    Enum.reduce(clauses, {acc, original}, fn {:->, meta, [head, body]}, {acc, context} ->
      {failed?, context} = reset_failed(context, failed?)
      {patterns, guards} = extract_head(head)
      {trees, context} = Pattern.of_head(patterns, guards, domain, info, meta, stack, context)

      {result, context} = of_expr(body, expected, expr || body, stack, context)
      {fun.(trees, result, context, acc), context |> set_failed(failed?) |> reset_vars(original)}
    end)
  end

  defp reset_failed(%{failed: true} = context, false), do: {true, %{context | failed: false}}
  defp reset_failed(context, _), do: {false, context}

  defp set_failed(%{failed: false} = context, true), do: %{context | failed: true}
  defp set_failed(context, _bool), do: context

  defp reset_vars(context, %{vars: vars}), do: %{context | vars: vars}

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

  ## Warning formatting

  def format_diagnostic({:badupdate, type, expr, context}) do
    {:%, _, [module, {:%{}, _, [{:|, _, [map, _]}]}]} = expr
    traces = collect_traces(map, context)

    suggestion =
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

          #{hint()} #{suggestion}. Given pattern matching is enough to catch typing errors, \
          you may optionally convert the struct update into a map update. For example, \
          instead of:

              user = some_fun()
              %User{user | name: "John Doe"}

          it is enough to write:

              %User{} = user = some_fun()
              %{user | name: "John Doe"}
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

  def format_diagnostic({:badbinary, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected the right side of <- in a binary generator to be a binary:

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
