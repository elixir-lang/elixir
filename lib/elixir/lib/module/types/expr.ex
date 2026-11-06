# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Expr do
  @moduledoc false

  alias Module.Types.{Apply, Of, Pattern}
  import Module.Types.{Helpers, Descr}

  14 = length(Macro.Env.__info__(:struct))

  aliases = list(tuple([atom(), atom()]))
  macro_aliases = list(tuple([atom(), tuple([term(), atom()])]))
  functions_and_macros = list(tuple([atom(), list(tuple([atom(), integer()]))]))
  list_of_modules = list(atom())

  @try_catch atom([:error, :exit, :throw])

  @caller closed_map(
            __struct__: {atom([Macro.Env]), false},
            aliases: {aliases, false},
            context: {atom([:match, :guard, nil]), false},
            context_modules: {list_of_modules, false},
            file: {binary(), false},
            function: {opt_union(tuple([atom(), integer()]), atom([nil])), false},
            functions: {functions_and_macros, false},
            lexical_tracker: {opt_union(pid(), atom([nil])), false},
            line: {integer(), false},
            macro_aliases: {macro_aliases, false},
            macros: {functions_and_macros, false},
            module: {atom(), false},
            requires: {list_of_modules, false},
            tracers: {list_of_modules, false},
            versioned_vars: {open_map(), false}
          )

  # We do not make exception dynamic on purpose. If you do a blank rescue,
  # then we will assume you need to statically handle all possible exceptions.
  @exception open_map(__struct__: {atom(), false}, __exception__: {term(), false})

  args_or_arity = opt_union(list(term()), integer())

  custom_info_key = opt_difference(atom(), atom([:file, :line, :error_info]))

  extra_info =
    list(
      tuple([atom([:file]), opt_union(list(integer()), binary())])
      |> opt_union(tuple([atom([:line]), integer()]))
      |> opt_union(tuple([atom([:error_info]), open_map()]))
      |> opt_union(tuple([custom_info_key, term()]))
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
        # Unless we are already computing reverse arrows, we cache the reverse
        # arrows of the right side and track the assignment, so refinements to
        # the variables in the pattern can be propagated back to the right side
        # (see refine_assignments/5)
        {stack, track?} =
          case stack.reverse_arrow do
            nil -> {%{stack | reverse_arrow: :cache}, true}
            :cache -> {stack, true}
            _ -> {stack, false}
          end

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

        if track? do
          {{type, versions}, used, context} =
            with_used(context, fn context ->
              {type, versions, context} =
                Pattern.of_match(left_expr, type_fun, match, meta, stack, context)

              {{type, versions}, context}
            end)

          {type, track_assignment(versions, meta, match, used, context)}
        else
          {type, _versions, context} =
            Pattern.of_match(left_expr, type_fun, match, meta, stack, context)

          {type, context}
        end
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

    expected = expected_map_update(pairs_types, expected)
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
      cache_stack = %{stack | reverse_arrow: :cache}

      {pos_head_type, head_used, context} =
        with_used(acc_context, &of_expr(pos_head, term(), pos_head, cache_stack, &1))

      refine = {acc_context.vars, context.vars, head_used}

      context =
        maybe_always_or_never_match_cond(pos_head_type, pos_head, pos_meta, stack, context, false)

      # Keep the context except the warnings, and compute the body
      pos_context =
        refine_expr(pos_head, @truthy, pos_body, refine, stack, context)
        |> reset_warnings(context)

      {pos_body_type, pos_body_context} = of_expr(pos_body, expected, expr, stack, pos_context)

      # Reset the context vars once again to compute the falsy type
      context = Of.reset_vars(pos_body_context, context)

      neg_context =
        refine_expr(pos_head, @falsy, neg_body, refine, stack, context)
        |> reset_warnings(context)

      {neg_body_type, neg_body_context} = of_expr(neg_body, expected, expr, stack, neg_context)

      body_type = opt_union(pos_body_type, neg_body_type)

      # If a branch is dead, the refinement of the other one applies
      # after the cond, so we don't know the targets
      context =
        cond do
          empty?(pos_body_type) ->
            falsy_context = refine_expr(pos_head, @falsy, :all, refine, stack, context)
            Of.reset_vars(neg_body_context, falsy_context)

          empty?(neg_body_type) ->
            truthy_context = refine_expr(pos_head, @truthy, :all, refine, stack, context)
            Of.reset_vars(neg_body_context, truthy_context)

          true ->
            Of.reset_vars(neg_body_context, acc_context)
        end

      dynamic_unless_static({body_type, context}, stack)
    end)
  end

  def of_expr({:cond, meta, [[{:do, clauses}]]}, expected, expr, stack, context) do
    cache_result(meta, stack, context, fn ->
      {body_type, acc_context} =
        reduce_non_empty(clauses, {none(), context}, fn
          {:->, meta, [[head], body]}, rest, {acc, head_context} ->
            last? = rest == []
            cache_stack = %{stack | reverse_arrow: :cache}

            {head_type, head_used, context} =
              with_used(head_context, &of_expr(head, term(), head, cache_stack, &1))

            refine = {head_context.vars, context.vars, head_used}

            context =
              maybe_always_or_never_match_cond(head_type, head, meta, stack, context, last?)

            # Keep the context except the warnings, and compute the body
            truthy_context =
              refine_expr(head, @truthy, body, refine, stack, context)
              |> reset_warnings(context)

            {body_type, body_context} = of_expr(body, expected, expr, stack, truthy_context)

            # Reset the context vars to the head definition to compute the falsy type
            context = Of.reset_vars(body_context, context)

            context =
              if last? do
                context
              else
                # The falsy refinement applies to the remaining clauses
                refine_expr(head, @falsy, rest, refine, stack, context)
                |> reset_warnings(context)
              end

            {opt_union(body_type, acc), context}
        end)

      dynamic_unless_static({body_type, Of.reset_vars(acc_context, context)}, stack)
    end)
  end

  # When refining a case, we filter the clauses which may return the expected
  # type. If only one clause may return it, we refine the case expression and
  # the clause in sequence. If multiple clauses may return it, each clause is a
  # branch, and we refine the variables as long as we only have only one refining
  # clause (see `of_case_branches/5`). Otherwise, we refine the case expression
  # to the union of the clauses arguments.
  def of_expr({:case, meta, [case_expr, [do: _clauses]]}, expected, _expr, stack, context)
      when stack.reverse_arrow in [:except_none, :include_none] do
    version = Keyword.fetch!(meta, :version)
    {clauses, branch} = Map.fetch!(context.reverse_arrows, version)
    original = context

    {skipped, filtered} =
      Enum.reduce(clauses, {0, []}, fn {_arg_type, body_type, _clause} = entry, {skipped, acc} ->
        if disjoint?(body_type, expected) do
          {skipped + 1, acc}
        else
          {skipped, [entry | acc]}
        end
      end)

    context =
      case filtered do
        # Nothing skipped and a single clause, there is nothing to refine
        [_] when skipped == 0 ->
          context

        [{arg_type, _body_type, clause}] ->
          leaf = {:case, arg_type, case_expr, clause, expected}
          {_, context} = of_case_leaf(leaf, stack, context)
          reset_warnings(context, original)

        _ ->
          case of_case_branches(branch, clauses, expected, stack, context) do
            {:ok, context} ->
              reset_warnings(context, original)

            # Nothing skipped, there is nothing to refine
            :error when skipped == 0 ->
              context

            :error ->
              case_expected = Enum.reduce(filtered, none(), &opt_union(elem(&1, 0), &2))
              {_, context} = of_case_leaf({case_expr, case_expected}, stack, context)
              reset_warnings(context, original)
          end
      end

    dynamic_unless_static({expected, context}, stack)
  end

  def of_expr({:case, meta, [case_expr, [do: clauses]]}, expected, _expr, stack, base_context) do
    cache_stack = %{stack | reverse_arrow: :cache}

    {case_type, case_used, context} =
      with_used(base_context, &of_expr(case_expr, term(), case_expr, cache_stack, &1))

    info = {:case, meta, case_expr, case_type}
    literal? = Macro.quoted_literal?(case_expr)

    # If the expression is generated or the construct is a literal,
    # it is most likely a macro code. However, if no clause is matched,
    # we should still check for that.
    clauses =
      if literal? or case_expr |> get_meta() |> Keyword.get(:generated, false) do
        for {:->, meta, args} <- clauses, do: {:->, [generated: true] ++ meta, args}
      else
        clauses
      end

    # Used to classify clauses when refining them in conditions,
    # which depends on the variables defined before the case and
    # the ones defined after the case expression
    %{vars: pre_vars} = base_context
    %{vars: case_vars} = context
    refine = {pre_vars, case_vars, case_used}

    cache_arrows(meta, stack, fn ->
      acc = {false, none(), [], 0, :none}

      {{none?, body_acc, clauses_acc, count, refining}, context} =
        of_clauses_fun(clauses, [case_type], info, stack, context, acc, fn
          trees, precise?, errored?, {:->, _, [_, body]} = clause, context, acc ->
            # Compute the arg type based on the clause itself
            [arg_type] = Pattern.of_domain(trees, stack, context)

            # Now we refine the case_expr context and use it to compute the body
            refined_context = refine_expr(case_expr, arg_type, body, refine, stack, context)

            # We track which variables the body uses to classify the clause
            body_context = reset_warnings(refined_context, context)

            {body_type, used, context} =
              with_used(body_context, &of_expr(body, expected, body, stack, &1))

            # Now we compute the return type and the clauses for reverse arrow
            {none?, body_acc, clauses_acc, count, refining} = acc

            if (precise? and empty?(body_type)) or (errored? and not literal?) do
              {{true, body_acc, clauses_acc, count, refining}, context}
            else
              [arg_type] = Pattern.of_domain(trees, stack, context)

              refining =
                refining_clause(clause, body_type, used, count, refining, pre_vars, case_vars)

              clauses_acc = [{arg_type, body_type, clause} | clauses_acc]
              {{none?, opt_union(body_type, body_acc), clauses_acc, count + 1, refining}, context}
            end
        end)

      context =
        if none? or stack.mode != :static do
          head_type = Enum.reduce(clauses_acc, none(), &opt_union(elem(&1, 0), &2))

          # The refinement applies after the case, so we don't know the targets
          refine_expr(case_expr, head_type, :all, refine, stack, context)
          |> reset_warnings(context)
        else
          context
        end

      # The case can only refine across branches if at most one clause
      # refines variables in its head or body (see of_case_branches/6)
      branch =
        case refining do
          :many ->
            nil

          :none ->
            {branch_ref(case_expr, context), :none}

          {index, body_type, vars, leaf} ->
            # Only the variables used by both the case expression and the
            # refining clause may be refined by all branches
            case Enum.filter(vars, &is_map_key(case_used, &1)) do
              [] ->
                nil

              vars ->
                # Clauses are stored in reverse, so we convert the index to its position
                leaf = with {:body, body} <- leaf, do: {:body, branch_ref(body, context)}
                {branch_ref(case_expr, context), {count - 1 - index, body_type, vars, leaf}}
            end
        end

      {body_acc, {clauses_acc, branch}, context}
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
          trees, _precise?, errored?, {:->, _, [_, body]}, context, acc ->
            {body_type, context} = of_expr(body, term(), body, stack, context)

            if errored? do
              {acc, context}
            else
              args_types = Pattern.of_domain(trees, stack, context)
              {add_inferred(acc, args_types, body_type), context}
            end
        end)

      if acc == [] do
        {fun(length(domain)), context}
      else
        {fun_from_inferred_clauses(acc), context}
      end
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
            context = error(__MODULE__, error, meta, stack, context)
            {opt_union(body_type, acc), Of.reset_vars(context, original)}
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

        {into_kinds, into_type, expected, context} =
          for_into(into, meta, expected, stack, context)

        {block_type, context} = of_expr(block, expected, block, stack, context)

        result_type =
          if :non_empty_list in into_kinds do
            opt_union(into_type, non_empty_list(block_type))
          else
            into_type
          end

        {result_type, context} =
          if :bitstring in into_kinds do
            intersection = opt_intersection(block_type, bitstring())

            # On the binary path the body must be a bitstring. If the collectable
            # is compatible with a list, that path may never happen, so we accept.
            if compatible?(block_type, bitstring()) or
                 (:non_empty_list in into_kinds and compatible?(into_type, empty_list())) do
              # If into_type is a binary but the block is a bitstring_no_binary(),
              # then the result may be a bitstring_no_binary().
              {opt_union(intersection, opt_union(result_type, binary())), context}
            else
              error = {:badbitbody, block_type, block, context}
              {error_type(), error(__MODULE__, error, meta, stack, context)}
            end
          else
            {result_type, context}
          end

        {if(gradual?(into_type), do: dynamic(result_type), else: result_type), context}
        |> dynamic_unless_static(stack)
      end
    end)
  end

  def of_expr({:with, meta, [_ | _] = clauses}, expected, _expr, stack, original) do
    cache_result(meta, stack, original, fn ->
      {clauses, [[do: do_block] ++ options]} = Enum.split(clauses, -1)

      {else_types, context} = with_clauses(clauses, do_block, stack, {none(), original})

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
      {type, context} =
        of_expr(callee, open_map([{key_or_fun, {expected, false}}]), expr, stack, context)

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
        {_optional?, type} -> type
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
    args = [__exception__: {term(), false}]

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

  defp maybe_list_hd_or_term(expected) do
    case list_hd(expected) do
      {:ok, head} -> head
      _ -> term()
    end
  end

  defp for_into([], _meta, expected, _stack, context),
    do: {[:non_empty_list], empty_list(), maybe_list_hd_or_term(expected), context}

  defp for_into(binary, _meta, _expected, _stack, context) when is_binary(binary),
    do: {[:bitstring], binary(), bitstring(), context}

  defp for_into(into, meta, expected, stack, context) do
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
      # A comprehension may concatenate the block an arbitrary number of times.
      # Even if both the initial value and each block are unaligned bitstrings,
      # repeated concatenation may eventually produce an aligned binary.
      case {bitstring_type?(type), empty_list_type?(type)} do
        # The collectable may be a list, which accepts any element, so the
        # body is not restricted. The caller checks the binary path.
        {true, true} ->
          {[:bitstring, :non_empty_list], type, term(), context}

        {true, false} ->
          {[:bitstring], type, bitstring(), context}

        {false, true} ->
          {[:non_empty_list], type, maybe_list_hd_or_term(expected), context}

        # The type is empty...
        {false, false} ->
          {[], type, term(), context}
      end
    else
      {_type, context} =
        Apply.remote_apply(info, Collectable, :into, [type], expr, stack, context)

      {[], dynamic(), term(), context}
    end
  end

  ## With

  defp with_clauses([clause | rest], do_block, stack, acc) do
    acc = with_clause(clause, [do_block | rest], stack, acc)
    with_clauses(rest, do_block, stack, acc)
  end

  defp with_clauses([], _do_block, _stack, acc), do: acc

  # The refinement of each clause applies to the remaining ones and the do block
  defp with_clause({:<-, meta, [left, right]} = expr, remaining, stack, {else_types, context}) do
    {[pattern], guards} = extract_head([left])
    cache_stack = %{stack | reverse_arrow: :cache}

    {type, used, right_context} =
      with_used(context, &of_expr(right, term(), right, cache_stack, &1))

    refine = {context.vars, right_context.vars, used}

    {trees, precise?, context} =
      Pattern.of_generator(pattern, guards, type, :with, expr, meta, stack, right_context)

    [pattern_type] = Pattern.of_domain(trees, stack, context)
    refined_context = refine_expr(right, pattern_type, remaining, refine, stack, context)
    else_type = if precise?, do: opt_difference(type, pattern_type), else: type
    {opt_union(else_type, else_types), reset_warnings(refined_context, context)}
  end

  defp with_clause(expr, _remaining, stack, {else_types, context}) do
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
    do: fun.(last, [], acc)

  defp reduce_non_empty([head | tail], acc, fun),
    do: reduce_non_empty(tail, fun.(head, tail, acc), fun)

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
    of_acc = fn _args_types, _precise?, errored?, {:->, _, [_, body]}, context, acc ->
      {body_type, context} = of_expr(body, expected, body, stack, context)
      acc = if errored?, do: acc, else: opt_union(acc, body_type)
      {acc, context}
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

          {trees, precise?, errored?, _, previous, context} =
            Pattern.of_head(patterns, guards, domain, previous, info, meta, stack, context)

          {acc, context} = of_acc.(trees, precise?, errored?, clause, context, acc)
          {acc, previous, context |> set_failed(failed?) |> Of.reset_vars(original)}
      end)

    {result, context}
  end

  # A case refined in a condition may have multiple clauses that can return the
  # expected type. Each of them is a branch: if the case returned the expected
  # type, one of the branches ran, so a variable is refined to the union of its
  # types across all branches. This allows `or`, `||`, and the falsy side of
  # `and` and `&&` to refine variables, similar to orelse in guards.
  #
  # We only do so if a single clause of the case uses the variables defined
  # before it, as only that clause may refine them (the refining clause). The
  # other clauses are simple and only refine the case expression with their
  # arguments (such as `true -> true` or `x -> x`). We decide which clause is
  # the refining one when caching the clauses, so cases with multiple refining
  # clauses are never refined across branches.
  #
  # For performance, we flatten nested cases, to avoid computing the union of the
  # conditional variables at each step along the way. We also refine only the
  # variables refined by all branches and we type them as dynamic within each
  # leaf. This way, each leaf only refines what it learns about the variable (for
  # example, `%{..., m: not :pp}`) instead of the whole variable type, which keeps
  # the join cheap, and we intersect the result with the current type at the end.
  defp of_case_branches(
         {case_ref, {_, body_type, candidates, _}} = branch,
         clauses,
         expected,
         stack,
         context
       ) do
    with false <- disjoint?(body_type, expected),
         false <- subtype?(upper_bound(body_type), upper_bound(expected)),
         {_count, [_, _ | _] = leaves} <- case_leaves(branch, clauses, expected, context, []) do
      # Only the variables used by the refining clause may be refined by all branches,
      # so we only track them and type them as dynamic, to keep the join cheap
      %{vars: vars} = context
      candidate_vars = Map.take(vars, candidates)

      vars =
        Enum.reduce(candidate_vars, vars, fn {version, data}, vars ->
          Map.put(vars, version, %{data | type: dynamic()})
        end)

      branch_context = %{context | vars: vars, conditional_vars: {candidate_vars, %{}}}

      vars_conds =
        Enum.map(leaves, fn leaf ->
          {_, %{vars: vars, conditional_vars: {_, cond_vars}}} =
            of_case_leaf(leaf, stack, branch_context)

          {vars, cond_vars}
        end)

      {:ok, Of.reduce_conditional_vars(vars_conds, ref_expr(case_ref), stack, context)}
    else
      _ -> :error
    end
  end

  defp of_case_branches(_branch, _clauses, _expected, _stack, _context), do: :error

  defp of_case_leaf(
         {:clause, head_type, case_expr, {:->, meta, [head, body]}, expected},
         stack,
         context
       ) do
    {patterns, guards} = extract_head(head)
    info = {{:case, meta, case_expr, head_type}, head}
    previous = Pattern.init_previous()

    {_, _, _, _, _, context} =
      Pattern.of_head(patterns, guards, [head_type], previous, info, meta, stack, context)

    of_expr(body, expected, body, stack, context)
  end

  defp of_case_leaf({:case, arg_type, case_expr, clause, expected}, stack, context) do
    {case_type, context} = of_expr(case_expr, arg_type, case_expr, stack, context)
    of_case_leaf({:clause, case_type, case_expr, clause, expected}, stack, context)
  end

  defp of_case_leaf({expr, expected}, stack, context) do
    of_expr(expr, expected, expr, stack, context)
  end

  # Computes the leaves of the given reference with the expected type.
  # If it references a case where multiple clauses may return the
  # expected type, we compute the leaves of its clauses.
  defp ref_leaves({:case, version, expr}, expected, context, acc) do
    case Map.fetch!(context.reverse_arrows, version) do
      {clauses, branch} when branch != nil ->
        case case_leaves(branch, clauses, expected, context, acc) do
          {count, leaves} when count > 1 -> leaves
          {_count, _leaves} -> [{expr, expected} | acc]
        end

      _ ->
        [{expr, expected} | acc]
    end
  end

  defp ref_leaves({:expr, expr}, expected, _context, acc) do
    [{expr, expected} | acc]
  end

  # Computes the leaves of the clauses which may return the expected type,
  # alongside how many there are. Simple clauses only refine the case
  # expression, so their leaves are the leaves of the case expression.
  # At most one clause may refine variables in its body, its leaves are
  # the leaves of its body or, if its body uses the variables bound by
  # its head, the clause itself.
  defp case_leaves({case_ref, refining}, clauses, expected, context, acc) do
    {_pos, count, acc} =
      Enum.reduce(clauses, {0, 0, acc}, fn {arg_type, body_type, clause}, {pos, count, acc} ->
        if disjoint?(body_type, expected) do
          {pos + 1, count, acc}
        else
          acc =
            case refining do
              {^pos, _, _, :case} ->
                [{:case, arg_type, ref_expr(case_ref), clause, expected} | acc]

              {^pos, _, _, :clause} ->
                [{:clause, arg_type, ref_expr(case_ref), clause, expected} | acc]

              {^pos, _, _, {:body, body_ref}} ->
                ref_leaves(body_ref, expected, context, acc)

              _ ->
                ref_leaves(case_ref, arg_type, context, acc)
            end

          {pos + 1, count + 1, acc}
        end
      end)

    {count, acc}
  end

  defp ref_expr({:case, _version, expr}), do: expr
  defp ref_expr({:expr, expr}), do: expr

  # References an expression, which may be a case with reverse arrows
  defp branch_ref({:case, meta, [_, [do: _]]} = expr, context) do
    with {:ok, version} <- Keyword.fetch(meta, :version),
         true <- is_map_key(context.reverse_arrows, version) do
      {:case, version, expr}
    else
      _ -> {:expr, expr}
    end
  end

  defp branch_ref(expr, _context), do: {:expr, expr}

  # Tracks the clause which refines variables defined by the parent
  # (see `of_case_branches/5`). At most one clause may refine, which we
  # store with how much of the case must be typed for its body:
  #
  #   * `:case` - the case expression narrowed to the clause, then the
  #     clause, as its body uses variables bound by the case expression
  #
  #   * `:clause` - its head and body, as its body uses variables bound
  #     by its head
  #
  #   * `{:body, ref}` - only its body
  #
  defp refining_clause(_clause, _body_type, _used, _index, :many, _pre_vars, _case_vars),
    do: :many

  defp refining_clause(
         {:->, _, [_head, body]},
         body_type,
         used,
         index,
         refining,
         pre_vars,
         case_vars
       ) do
    # Splits the variables used by the body into the ones defined before the case
    # and checks if any variable bound by the case expression or the head is used
    {vars, expr?, head?} =
      Enum.reduce(used, {[], false, false}, fn {version, _}, {vars, expr?, head?} ->
        cond do
          is_map_key(pre_vars, version) -> {[version | vars], expr?, head?}
          is_map_key(case_vars, version) -> {vars, true, head?}
          true -> {vars, expr?, true}
        end
      end)

    cond do
      vars == [] -> refining
      refining != :none -> :many
      expr? -> {index, body_type, vars, :case}
      head? -> {index, body_type, vars, :clause}
      true -> {index, body_type, vars, {:body, body}}
    end
  end

  # Tracks the variables bound by an assignment alongside the variables used by it.
  # The assignment version is given after its pattern and right side are expanded,
  # so it is greater than the versions of all variables (and assignments) it uses.
  defp track_assignment([], _meta, _match, _used, context), do: context

  defp track_assignment(versions, meta, match, used, %{vars: vars} = context) do
    assignment = {Keyword.fetch!(meta, :version), match, Map.keys(used)}

    vars =
      Enum.reduce(versions, vars, fn version, vars ->
        %{^version => data} = vars
        %{vars | version => Map.put(data, :assignment, assignment)}
      end)

    %{context | vars: vars}
  end

  # Refines the variables in the expression with the given type, so they can be
  # used by the body (which may be `:all` if we don't know where they are used).
  # It receives the variables before and after the expression, alongside the
  # variables used by it (the sources).
  #
  # The refinement is skipped when it cannot affect the variables used by the
  # body (the targets). That's when the expression neither uses nor binds any
  # variables, or when the body uses none of them and none of the sources were
  # bound by an assignment, through which their refinements could propagate.
  defp refine_expr(expr, type, body, {pre_vars, vars, used} = refine, stack, context) do
    with true <- used != %{} or map_size(vars) != map_size(pre_vars),
         {targets, direct?} <- body_targets(body, refine),
         true <- direct? or Enum.any?(used, &has_assignment?(&1, vars)) do
      {_, refined_context} =
        of_expr(expr, type, expr, %{stack | reverse_arrow: :except_none}, context)

      refine_assignments(used, targets, context, stack, refined_context)
    else
      _ -> context
    end
  end

  defp has_assignment?({version, _}, vars) do
    match?(%{^version => %{assignment: _}}, vars)
  end

  # Collects the variables defined before the body which are used by it, and
  # whether any of them is used or bound by the expression being refined
  defp body_targets(:all, _refine), do: {:all, true}

  defp body_targets(body, {pre_vars, vars, used}) do
    {_, {targets, direct?}} =
      Macro.prewalk(body, {%{}, false}, fn
        {_, meta, _} = var, {targets, direct?} = acc when is_var(var) ->
          case Keyword.fetch(meta, :version) do
            {:ok, version} when is_map_key(vars, version) ->
              direct? =
                direct? or is_map_key(used, version) or not is_map_key(pre_vars, version)

              {var, {Map.put(targets, version, true), direct?}}

            _ ->
              {var, acc}
          end

        node, acc ->
          {node, acc}
      end)

    case targets do
      targets when targets == %{} -> nil
      targets -> {{targets |> Map.keys() |> Enum.min(), targets}, direct?}
    end
  end

  # Once an expression refines the variables it uses (the sources), we propagate
  # their refinements to the assignments that bound them (see track_assignment/5),
  # by computing the reverse arrow of the assignment with the refined variables.
  # This may refine the variables used by the assignment, which are propagated
  # to their own assignments.
  #
  # Refining a variable is only useful if it is used later on (the targets), such
  # as by the body of a clause, so we only compute assignments which may lead to
  # a target. Targets may be `:all`, when we don't know which variables are used.
  # Otherwise they are stored alongside the lowest version among them, as there is
  # no point in following variables with lower versions (see var_reaches_targets/4).
  #
  # Assignments are sequential, so the variables used by an assignment were bound
  # by previous ones and we can process them in reverse order of their versions,
  # each at most once, without dealing with cycles.
  defp refine_assignments(used, targets, original, stack, context) do
    %{vars: vars} = context

    case changed_assignments(Map.keys(used), original.vars, vars) do
      [] ->
        context

      assignments ->
        acc = {:gb_trees.empty(), %{}}
        {queue, reaches} = enqueue_assignments(assignments, targets, vars, acc)
        reverse_stack = %{stack | reverse_arrow: :except_none}
        refine_assignments_queue(queue, reaches, targets, reverse_stack, context)
    end
  end

  defp refine_assignments_queue(queue, reaches, targets, stack, context) do
    if :gb_trees.is_empty(queue) do
      context
    else
      {_version, {match, used}, queue} = :gb_trees.take_largest(queue)
      {_, refined_context} = of_expr(match, term(), match, stack, context)

      # In theory, the refinement should never fail, as the assignment is
      # replayed with a subset of the types it has already accepted.
      # However, if it does, the assignment is not reachable within the
      # current refinement and the error will be reported elsewhere.
      # We also discard the context, so its failure does not leak.
      if refined_context.failed and not context.failed do
        refine_assignments_queue(queue, reaches, targets, stack, context)
      else
        %{vars: vars} = refined_context
        assignments = changed_assignments(used, context.vars, vars)
        {queue, reaches} = enqueue_assignments(assignments, targets, vars, {queue, reaches})
        refine_assignments_queue(queue, reaches, targets, stack, refined_context)
      end
    end
  end

  # Returns the assignments of the used variables which changed. The variables used
  # by an assignment existed before it, so their assignments always have lower versions
  defp changed_assignments(used, old_vars, vars) do
    Enum.reduce(used, [], fn version, acc ->
      case vars do
        %{^version => %{type: type, assignment: triplet} = data}
        when not is_map_key(data, :errored) ->
          case old_vars do
            %{^version => %{type: ^type}} -> acc
            %{} -> [triplet | acc]
          end

        %{} ->
          acc
      end
    end)
  end

  defp enqueue_assignments(assignments, targets, vars, acc) do
    Enum.reduce(assignments, acc, fn {version, match, used} = assignment, {queue, reaches} ->
      case reaches_targets(assignment, targets, vars, reaches) do
        {true, reaches} -> {:gb_trees.enter(version, {match, used}, queue), reaches}
        {false, reaches} -> {queue, reaches}
      end
    end)
  end

  # An assignment reaches the targets if any of the variables it uses is a target or
  # was bound by an assignment which reaches them. The results are cached by version,
  # as the same assignment may be reached from all of the variables it binds.
  defp reaches_targets(_assignment, :all, _vars, reaches), do: {true, reaches}

  defp reaches_targets({version, _match, used}, targets, vars, reaches) do
    case reaches do
      %{^version => reaches?} ->
        {reaches?, reaches}

      %{} ->
        {reaches?, reaches} =
          Enum.reduce_while(used, {false, reaches}, fn var_version, {false, reaches} ->
            case var_reaches_targets(var_version, targets, vars, reaches) do
              {true, reaches} -> {:halt, {true, reaches}}
              {false, reaches} -> {:cont, {false, reaches}}
            end
          end)

        {reaches?, Map.put(reaches, version, reaches?)}
    end
  end

  # The variables used by an assignment always have lower versions than the variables
  # it binds, so a variable with a lower version than all targets cannot lead to them
  defp var_reaches_targets(version, {min_target, targets} = all_targets, vars, reaches) do
    cond do
      is_map_key(targets, version) ->
        {true, reaches}

      version < min_target ->
        {false, reaches}

      true ->
        case vars do
          %{^version => %{assignment: assignment}} ->
            reaches_targets(assignment, all_targets, vars, reaches)

          %{} ->
            {false, reaches}
        end
    end
  end

  # Invokes the function while tracking which variables it uses,
  # which are then also given to the outer conditional, if any
  defp with_used(context, fun) do
    %{vars: vars, conditional_vars: conditional_vars} = context
    context = %{context | conditional_vars: {vars, %{}}}
    {result, context} = fun.(context)
    %{conditional_vars: {_, used}} = context
    {result, used, %{context | conditional_vars: merge_conditional_vars(conditional_vars, used)}}
  end

  # Merges the variables used in a nested conditional into the outer one
  defp merge_conditional_vars({pre_vars, cond_vars}, used) do
    {pre_vars,
     Enum.reduce(used, cond_vars, fn {version, _}, acc ->
       if is_map_key(pre_vars, version), do: Map.put(acc, version, true), else: acc
     end)}
  end

  defp merge_conditional_vars(nil, _used), do: nil

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

  # In map update syntax, fields that are replaced by the update need
  # to exist in the original map, but their old values are not constrained
  # by what the consumer expects of the new values. Fields that are not
  # replaced keep whatever constraint the consumer has on them.
  #
  # One way to implement this function is to compute the union of any
  # non-singleton key and then call map_update(type, union, term(), :reject, false).
  # The downside of this approach is that %{x | key => value} will generate
  # a union with all possible keys set to term, which is not terribly useful.
  # So in case there is a non-singleton key, we fallback to an open_map.
  # The alternative implementation would only fallback to open_map if the
  # update of either required or optional keys fail.
  defp expected_map_update(pairs_types, expected) do
    {all_singleton?, required_keys} =
      Enum.reduce(pairs_types, {true, []}, fn {key_type, _}, {all_singleton?, required_keys} ->
        # The only consider a key as required if it has no other components,
        # even dynamic ones (hence upper bound).
        case atom_fetch(upper_bound(key_type)) do
          {:finite, [key]} -> {all_singleton?, [key | required_keys]}
          _ -> {false, required_keys}
        end
      end)

    with true <- all_singleton?,
         {:ok, expected} <- put_required_keys(expected, required_keys) do
      expected
    else
      _ -> open_map(Enum.map(required_keys, &{&1, {term(), false}}))
    end
  end

  defp put_required_keys(type, [key | keys]) do
    case map_put_key(type, key, term()) do
      {:ok, type} -> put_required_keys(type, keys)
      _ -> :error
    end
  end

  defp put_required_keys(type, []), do: {:ok, type}

  defp literal_map_update(descr, key_descr, value_descr) do
    case map_update(descr, key_descr, value_descr, :reject, false) do
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
