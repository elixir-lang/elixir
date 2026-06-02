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
            function: opt_union(tuple([atom(), integer()]), atom([nil])),
            functions: functions_and_macros,
            lexical_tracker: opt_union(pid(), atom([nil])),
            line: integer(),
            macro_aliases: aliases,
            macros: functions_and_macros,
            module: atom(),
            requires: list_of_modules,
            tracers: list_of_modules,
            versioned_vars: open_map()
          )

  # We do not make exception dynamic on purpose. If you do a blank rescue,
  # then we will assume you need to statically handle all possible exceptions.
  @exception open_map(__struct__: atom(), __exception__: term())

  args_or_arity = opt_union(list(term()), integer())

  extra_info =
    list(
      tuple([atom([:file]), list(integer())])
      |> opt_union(tuple([atom([:line]), integer()]))
      |> opt_union(tuple([atom([:error_info]), open_map()]))
    )

  @stacktrace list(
                opt_union(
                  tuple([atom(), atom(), args_or_arity, extra_info]),
                  tuple([fun(), args_or_arity, extra_info])
                )
              )

  @falsy atom([false, nil])
  @truthy Module.Types.Descr.opt_negation(atom([false, nil]))

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

    {non_empty_list(Enum.reduce(prefix, &opt_union/2), suffix), context}
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
  def of_expr({:=, meta, [left_expr, right_expr]} = match, expected, expr, stack, context) do
    {left_expr, right_expr} = repack_match(left_expr, right_expr)

    case left_expr do
      # We do not raise on underscore in case someone writes _ = raise "omg"
      {:_, _, ctx} when is_atom(ctx) ->
        of_expr(right_expr, expected, expr, stack, context)

      _ ->
        type_fun =
          fn pattern_type, context ->
            # See if we can use the expected type to further refine the pattern type,
            # if we cannot, use the pattern type as that will fail later on.
            type = opt_intersection(pattern_type, expected)
            type = if empty?(type), do: pattern_type, else: type
            {result, context} = of_expr(right_expr, type, expr, stack, context)

            # The function may still return a too broad type,
            # so we refine once again to assign the most appropriate to the pattern.
            {opt_intersection(result, expected), context}
          end

        Pattern.of_match(left_expr, type_fun, match, meta, stack, context)
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

    expected = opt_intersection(expected, open_map(expected_pairs))
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
      context =
        Enum.reduce(pairs, context, fn {_key, value}, context ->
          {_type, context} = of_expr(value, term(), expr, stack, context)
          context
        end)

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

  def of_expr(
        {:cond, meta,
         [
           [
             {:do,
              [
                {:->, pos_meta, [[pos_head], pos_body]},
                {:->, _neg_meta, [[neg_head], neg_body]}
              ]}
           ]
         ]},
        expected,
        expr,
        stack,
        acc_context
      )
      when is_atom(neg_head) and neg_head not in [false, nil] do
    cache_result(meta, stack, acc_context, fn ->
      {pos_head_type, context} =
        of_expr(pos_head, term(), pos_head, %{stack | reverse_arrow: :cache}, acc_context)

      context =
        maybe_always_or_never_match_cond(pos_head_type, pos_head, pos_meta, stack, context, false)

      {_, truthy_context} =
        of_expr(pos_head, @truthy, pos_head, %{stack | reverse_arrow: :except_none}, context)

      # Keep the context except the warnings, and compute the body
      truthy_context = reset_warnings(truthy_context, context)

      {pos_body_type, pos_body_context} =
        of_expr(pos_body, expected, expr, stack, truthy_context)

      # Reset the context vars once again to compute the falsy type
      context = Of.reset_vars(pos_body_context, context)

      {_, falsy_context} =
        of_expr(pos_head, @falsy, pos_head, %{stack | reverse_arrow: :except_none}, context)

      falsy_context = reset_warnings(falsy_context, context)

      {neg_body_type, neg_body_context} =
        of_expr(neg_body, expected, expr, stack, falsy_context)

      body_type = opt_union(pos_body_type, neg_body_type)

      context =
        cond do
          empty?(pos_body_type) -> Of.reset_vars(neg_body_context, falsy_context)
          empty?(neg_body_type) -> Of.reset_vars(neg_body_context, truthy_context)
          true -> Of.reset_vars(neg_body_context, acc_context)
        end

      dynamic_unless_static({body_type, context}, stack)
    end)
  end

  def of_expr({:cond, meta, [[{:do, clauses}]]}, expected, expr, stack, context) do
    cache_result(meta, stack, context, fn ->
      {body_type, acc_context} =
        reduce_non_empty(clauses, {none(), context}, fn
          {:->, meta, [[head], body]}, {acc, context}, last? ->
            {head_type, context} =
              of_expr(head, term(), head, %{stack | reverse_arrow: :cache}, context)

            context =
              maybe_always_or_never_match_cond(head_type, head, meta, stack, context, last?)

            {_, truthy_context} =
              of_expr(head, @truthy, head, %{stack | reverse_arrow: :except_none}, context)

            # Keep the context except the warnings, and compute the body
            truthy_context = reset_warnings(truthy_context, context)
            {body_type, body_context} = of_expr(body, expected, expr, stack, truthy_context)

            # Reset the context vars to the head definition to compute the falsy type
            context = Of.reset_vars(body_context, context)

            context =
              if last? do
                context
              else
                {_, falsy_context} =
                  of_expr(head, @falsy, head, %{stack | reverse_arrow: :except_none}, context)

                reset_warnings(falsy_context, context)
              end

            {opt_union(body_type, acc), context}
        end)

      dynamic_unless_static({body_type, Of.reset_vars(acc_context, context)}, stack)
    end)
  end

  def of_expr({:case, meta, [case_expr, [do: _clauses]]}, expected, _expr, stack, context)
      when stack.reverse_arrow in [:except_none, :include_none] do
    version = Keyword.fetch!(meta, :version)
    clauses = Map.fetch!(context.reverse_arrows, version)
    original = context

    context =
      clauses
      |> Enum.reduce({0, []}, fn {_arg_type, body_type, _clause} = triplet, {counter, acc} ->
        if disjoint?(body_type, expected) do
          {counter + 1, acc}
        else
          {counter, [triplet | acc]}
        end
      end)
      |> case do
        # Nothing skipped, just return the context as is
        {0, _} ->
          context

        # If there is a single clause, we assume it is always evaluated
        # by doing reverse arrows and incorporting all variables into the context.
        # We have to evaluate the head again but it might have been preferred
        # if we could somehow merge a previously computed context.
        {_, [{arg_type, _body_type, {:->, meta, [head, body]}}]} ->
          {case_type, context} = of_expr(case_expr, arg_type, case_expr, stack, context)

          {patterns, guards} = extract_head(head)
          previous = Pattern.init_previous()
          info = {{:case, meta, case_expr, case_type}, head}

          {_, _, _, _, context} =
            Pattern.of_head(patterns, guards, [case_type], previous, info, meta, stack, context)

          {_, context} = of_expr(body, expected, body, stack, context)
          reset_warnings(context, original)

        {_, filtered} ->
          case_expected = Enum.reduce(filtered, none(), &opt_union(elem(&1, 0), &2))
          {_, context} = of_expr(case_expr, case_expected, case_expr, stack, context)
          reset_warnings(context, original)
      end

    dynamic_unless_static({expected, context}, stack)
  end

  def of_expr({:case, meta, [case_expr, [do: clauses]]}, expected, _expr, stack, base_context) do
    {case_type, context} =
      of_expr(case_expr, term(), case_expr, %{stack | reverse_arrow: :cache}, base_context)

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
    clauses =
      if added_meta != [] do
        for {:->, meta, args} <- clauses, do: {:->, [generated: true] ++ meta, args}
      else
        clauses
      end

    cache_arrows(meta, stack, fn ->
      acc = {false, none(), []}

      {{none?, body_acc, clauses_acc}, context} =
        of_clauses_fun(clauses, [case_type], info, stack, context, acc, fn
          trees, precise?, {:->, _, [_, body]} = clause, context, acc ->
            # Compute the arg type based on the clause itself
            [arg_type] = Pattern.of_domain(trees, stack, context)

            # Now we refine the case_expr context and use it to compute the body
            {_, refined_context} =
              of_expr(
                case_expr,
                arg_type,
                case_expr,
                %{stack | reverse_arrow: :except_none},
                context
              )

            {body_type, context} =
              of_expr(body, expected, body, stack, reset_warnings(refined_context, context))

            # Now we compute the return type and the clauses for reverse arrow
            {none?, body_acc, clauses_acc} = acc

            if precise? and empty?(body_type) do
              {{true, body_acc, clauses_acc}, context}
            else
              [arg_type] = Pattern.of_domain(trees, stack, context)
              clauses_acc = [{arg_type, body_type, clause} | clauses_acc]
              {{none?, opt_union(body_type, body_acc), clauses_acc}, context}
            end
        end)

      context =
        if none? or stack.mode != :static do
          head_type = Enum.reduce(clauses_acc, none(), &opt_union(elem(&1, 0), &2))

          {_, refined_context} =
            of_expr(
              case_expr,
              head_type,
              case_expr,
              %{stack | reverse_arrow: :except_none},
              context
            )

          reset_warnings(refined_context, context)
        else
          context
        end

      {body_acc, clauses_acc, context}
    end)
    |> dynamic_unless_static(stack)
  end

  # fn pat -> expr end
  def of_expr({:fn, meta, clauses}, _expected, _expr, stack, context) do
    cache_result(meta, stack, context, fn ->
      [{:->, _, [head, _]} | _] = clauses
      {patterns, _guards} = extract_head(head)
      domain = Enum.map(patterns, fn _ -> dynamic() end)

      {acc, context} =
        of_clauses_fun(clauses, domain, :fn, stack, context, [], fn
          trees, _precise?, {:->, _, [_, body]}, context, acc ->
            {body_type, context} = of_expr(body, term(), body, stack, context)
            args_types = Pattern.of_domain(trees, stack, context)
            {add_inferred(acc, args_types, body_type), context}
        end)

      {fun_from_inferred_clauses(acc), context}
    end)
  end

  def of_expr({:try, meta, [[do: body] ++ blocks]}, expected, expr, stack, original) do
    cache_result(meta, stack, original, fn ->
      {after_block, blocks} = Keyword.pop(blocks, :after)
      {else_block, blocks} = Keyword.pop(blocks, :else)

      {type, context} =
        if else_block do
          {type, context} = of_expr(body, term(), body, stack, original)
          info = {:try_else, type}
          of_clauses(else_block, [type], expected, info, stack, context, none())
        else
          of_expr(body, expected, expr, stack, original)
        end

      {type, context} =
        blocks
        |> Enum.reduce({type, Of.reset_vars(context, original)}, fn
          {:rescue, clauses}, {_acc, %{failed: failed?}} = acc_context ->
            Enum.reduce(clauses, acc_context, fn
              {:->, meta, [[head], body]}, {acc, context} ->
                {failed?, context} = reset_failed(context, failed?)

                context =
                  case head do
                    {:in, meta, [var, mods]} ->
                      of_rescue(var, mods, expr, :rescue, meta, stack, context)

                    var ->
                      of_rescue(var, [], var, :anonymous_rescue, meta, stack, context)
                  end

                {type, context} = of_expr(body, expected, body, stack, context)
                {opt_union(type, acc), context |> set_failed(failed?) |> Of.reset_vars(original)}
            end)

          {:catch, clauses}, {acc, context} ->
            args = [@try_catch, dynamic()]
            of_clauses(clauses, args, expected, :try_catch, stack, context, acc)
        end)
        |> dynamic_unless_static(stack)

      if after_block do
        {_type, context} = of_expr(after_block, term(), after_block, stack, context)
        {type, context}
      else
        {type, context}
      end
    end)
  end

  @timeout_type opt_union(integer(), atom([:infinity]))

  def of_expr({:receive, meta, [blocks]}, expected, expr, stack, original) do
    cache_result(meta, stack, original, fn ->
      blocks
      |> Enum.reduce({none(), original}, fn
        {:do, {:__block__, _, []}}, acc_context ->
          acc_context

        {:do, clauses}, {acc, context} ->
          of_clauses(clauses, [dynamic()], expected, :receive, stack, context, acc)

        {:after, [{:->, meta, [[timeout], body]}] = after_expr}, {acc, context} ->
          {timeout_type, context} = of_expr(timeout, @timeout_type, after_expr, stack, context)
          {body_type, context} = of_expr(body, expected, expr, stack, context)

          if compatible?(timeout_type, @timeout_type) do
            {opt_union(body_type, acc), Of.reset_vars(context, original)}
          else
            error = {:badtimeout, timeout_type, timeout, context}
            {opt_union(body_type, acc), error(__MODULE__, error, meta, stack, context)}
          end
      end)
      |> dynamic_unless_static(stack)
    end)
  end

  def of_expr({:for, meta, [_ | _] = args}, expected, expr, stack, context) do
    cache_result(meta, stack, context, fn ->
      {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)
      context = Enum.reduce(clauses, context, &for_clause(&1, stack, &2))

      # We don't need to type check uniq, as it is a compile-time boolean.
      # We handle reduce and into accordingly instead.
      if Keyword.has_key?(opts, :reduce) do
        reduce = Keyword.fetch!(opts, :reduce)
        {reduce_type, context} = of_expr(reduce, expected, expr, stack, context)
        # TODO: We need to type check against dynamic() instead of using reduce_type
        # because this is recursive. We need to infer the block type first.
        args = [dynamic()]
        of_clauses(block, args, expected, :for_reduce, stack, context, reduce_type)
      else
        # TODO: Use the collectable protocol for the output
        into = Keyword.get(opts, :into, [])
        {into_type, into_kind, context} = for_into(into, meta, stack, context)

        case into_kind do
          :bitstring ->
            {block_type, context} = of_expr(block, bitstring(), block, stack, context)
            intersection = opt_intersection(block_type, bitstring())

            if empty?(intersection) do
              error = {:badbitbody, block_type, block, context}
              {error_type(), error(__MODULE__, error, meta, stack, context)}
            else
              {opt_union(into_type, intersection), context}
            end

          :non_empty_list ->
            expected =
              case list_hd(expected) do
                {:ok, head} -> head
                _ -> term()
              end

            {block_type, context} = of_expr(block, expected, block, stack, context)
            {opt_union(into_type, non_empty_list(block_type)), context}

          :none ->
            {_, context} = of_expr(block, term(), block, stack, context)
            {into_type, context}
        end
        |> dynamic_unless_static(stack)
      end
    end)
  end

  # TODO: with pat <- expr do expr end
  def of_expr({:with, meta, [_ | _] = clauses}, expected, _expr, stack, original) do
    cache_result(meta, stack, original, fn ->
      {clauses, [[do: do_block] ++ options]} = Enum.split(clauses, -1)

      {else_types, context} =
        Enum.reduce(clauses, {none(), original}, &with_clause(&1, stack, &2))

      {do_result, context} = of_expr(do_block, expected, do_block, stack, context)
      context = Of.reset_vars(context, original)

      {else_result, context} =
        case options do
          [else: clauses] ->
            info = {:with_else, else_types}
            of_clauses(clauses, [else_types], expected, info, stack, context, none())

          [] ->
            {else_types, context}
        end

      dynamic_unless_static({opt_union(do_result, else_result), context}, stack)
    end)
  end

  def of_expr({{:., _, [fun]}, _, args} = call, _expected, _expr, stack, context) do
    {fun_type, context} = of_expr(fun, dynamic(fun(length(args))), call, stack, context)

    # TODO: Perform inference based on the strong domain of a function
    {args_types, context} =
      Enum.map_reduce(args, context, &of_expr(&1, term(), &1, stack, &2))

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
    Of.refine_body_var(version, expected, expr, stack, context)
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

  defp of_rescue(var, exceptions, expr, info, meta, stack, context) do
    args = [__exception__: term()]

    {structs, context} =
      Enum.map_reduce(exceptions, context, fn exception, context ->
        # Exceptions are not validated in the compiler,
        # to avoid export dependencies. So we do it here.
        {info, context} = Of.struct_info(exception, :expr, meta, stack, context)

        if info do
          {Of.struct_type(exception, info, args), context}
        else
          {error_type(), context}
        end
      end)

    case var do
      {:_, _, _} ->
        context

      _ ->
        expected = if structs == [], do: @exception, else: Enum.reduce(structs, &opt_union/2)
        expr = {:__block__, [type_check: info], [expr]}
        context = Of.declare_var(var, context)
        {_ok?, _type, context} = Of.refine_head_var(var, expected, expr, stack, context)
        context
    end
  end

  ## Comprehensions

  defp for_clause({:<-, meta, [left, right]}, stack, context) do
    meta = [type_check: :generator] ++ meta
    expr = {:<-, meta, [left, right]}
    {[pattern], guards} = extract_head([left])

    # TODO: Extract the type from enumerable protocol
    {_type, context} =
      Apply.remote(Enumerable, :count, [right], term(), expr, stack, context, &of_expr/5)

    {_tree, _precise, context} =
      Pattern.of_generator(pattern, guards, dynamic(), :for, expr, meta, stack, context)

    context
  end

  defp for_clause({:<<>>, _, [{:<-, meta, [left, right]}]} = expr, stack, context) do
    {right_type, context} = of_expr(right, bitstring(), expr, stack, context)

    {_tree, _precise?, context} =
      Pattern.of_generator(left, [], bitstring(), :for, expr, meta, stack, context)

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

  @into_compile opt_union(bitstring(), empty_list())

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
          type = opt_union(bitstring(), list(term()))
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

  ## With

  defp with_clause({:<-, meta, [left, right]} = expr, stack, {else_types, context}) do
    {[pattern], guards} = extract_head([left])

    {type, context} =
      of_expr(right, term(), right, %{stack | reverse_arrow: :cache}, context)

    {trees, precise?, context} =
      Pattern.of_generator(pattern, guards, type, :with, expr, meta, stack, context)

    [pattern_type] = Pattern.of_domain(trees, stack, context)

    {_, refined_context} =
      of_expr(right, pattern_type, right, %{stack | reverse_arrow: :except_none}, context)

    else_type = if precise?, do: opt_difference(type, pattern_type), else: type
    {opt_union(else_type, else_types), reset_warnings(refined_context, context)}
  end

  defp with_clause(expr, stack, {else_types, context}) do
    {_type, context} = of_expr(expr, term(), expr, stack, context)
    {else_types, context}
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
      {opt_union(acc, type), context}
    end)
  end

  defp reduce_non_empty([last], acc, fun),
    do: fun.(last, acc, true)

  defp reduce_non_empty([head | tail], acc, fun),
    do: reduce_non_empty(tail, fun.(head, acc, false), fun)

  defp maybe_always_or_never_match_cond(head_type, head, meta, stack, context, last?) do
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
  end

  defp dynamic_unless_static({_, _} = output, %{mode: :static}), do: output
  defp dynamic_unless_static({type, context}, %{mode: _}), do: {dynamic(type), context}

  defp cache_result(meta, %{reverse_arrow: reverse_arrow}, context, fun) do
    case reverse_arrow do
      nil ->
        fun.()

      :cache ->
        {result, context} = fun.()
        version = Keyword.fetch!(meta, :version)
        context = put_in(context.reverse_arrows[version], result)
        {result, context}

      _ ->
        version = Keyword.fetch!(meta, :version)
        {Map.fetch!(context.reverse_arrows, version), context}
    end
  end

  defp cache_arrows(_meta, %{reverse_arrow: nil}, fun) do
    {result, _cache, context} = fun.()
    {result, context}
  end

  defp cache_arrows(meta, %{reverse_arrow: :cache}, fun) do
    {result, cache, context} = fun.()
    version = Keyword.fetch!(meta, :version)
    context = put_in(context.reverse_arrows[version], cache)
    {result, context}
  end

  defp of_clauses(clauses, domain, expected, base_info, stack, context, acc) do
    of_acc = fn _args_types, _precise?, {:->, _, [_, body]}, context, acc ->
      {body_type, context} = of_expr(body, expected, body, stack, context)
      {opt_union(acc, body_type), context}
    end

    of_clauses_fun(clauses, domain, base_info, stack, context, acc, of_acc)
  end

  defp of_clauses_fun(clauses, domain, base_info, stack, original, acc, of_acc) do
    %{failed: failed?} = original

    {result, _previous, context} =
      Enum.reduce(clauses, {acc, Pattern.init_previous(), original}, fn
        {:->, meta, [head, _]} = clause, {acc, previous, context} ->
          {failed?, context} = reset_failed(context, failed?)
          {patterns, guards} = extract_head(head)
          info = {base_info, head}

          {trees, precise?, _, previous, context} =
            Pattern.of_head(patterns, guards, domain, previous, info, meta, stack, context)

          {acc, context} = of_acc.(trees, precise?, clause, context, acc)
          {acc, previous, context |> set_failed(failed?) |> Of.reset_vars(original)}
      end)

    {result, context}
  end

  defp reset_warnings(context, %{warnings: warnings}), do: %{context | warnings: warnings}

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
    do: [{args, opt_union(existing_return, return)} | tail]

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
