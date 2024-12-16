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
  def of_expr(atom, _stack, context) when is_atom(atom),
    do: {atom([atom]), context}

  # 12
  def of_expr(literal, _stack, context) when is_integer(literal),
    do: {integer(), context}

  # 1.2
  def of_expr(literal, _stack, context) when is_float(literal),
    do: {float(), context}

  # "..."
  def of_expr(literal, _stack, context) when is_binary(literal),
    do: {binary(), context}

  # #PID<...>
  def of_expr(literal, _stack, context) when is_pid(literal),
    do: {pid(), context}

  # []
  def of_expr([], _stack, context),
    do: {empty_list(), context}

  # [expr, ...]
  def of_expr(list, stack, context) when is_list(list) do
    {prefix, suffix} = unpack_list(list, [])
    {prefix, context} = Enum.map_reduce(prefix, context, &of_expr(&1, stack, &2))
    {suffix, context} = of_expr(suffix, stack, context)

    if stack.mode == :traversal do
      {dynamic(), context}
    else
      {non_empty_list(Enum.reduce(prefix, &union/2), suffix), context}
    end
  end

  # {left, right}
  def of_expr({left, right}, stack, context) do
    {left, context} = of_expr(left, stack, context)
    {right, context} = of_expr(right, stack, context)

    if stack.mode == :traversal do
      {dynamic(), context}
    else
      {tuple([left, right]), context}
    end
  end

  # {...}
  def of_expr({:{}, _meta, exprs}, stack, context) do
    {types, context} = Enum.map_reduce(exprs, context, &of_expr(&1, stack, &2))

    if stack.mode == :traversal do
      {dynamic(), context}
    else
      {tuple(types), context}
    end
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args}, stack, context) do
    context = Of.binary(args, :expr, stack, context)
    {binary(), context}
  end

  def of_expr({:__CALLER__, _meta, var_context}, _stack, context) when is_atom(var_context) do
    {@caller, context}
  end

  def of_expr({:__STACKTRACE__, _meta, var_context}, _stack, context)
      when is_atom(var_context) do
    {@stacktrace, context}
  end

  # left = right
  def of_expr({:=, _, [left_expr, right_expr]} = expr, stack, context) do
    {left_expr, right_expr} = repack_match(left_expr, right_expr)
    {right_type, context} = of_expr(right_expr, stack, context)

    # We do not raise on underscore in case someone writes _ = raise "omg"
    case left_expr do
      {:_, _, ctx} when is_atom(ctx) -> {right_type, context}
      _ -> Pattern.of_match(left_expr, right_type, expr, {:match, right_type}, stack, context)
    end
  end

  # %{map | ...}
  # TODO: Once we support typed structs, we need to type check them here.
  def of_expr({:%{}, meta, [{:|, _, [map, args]}]} = expr, stack, context) do
    {map_type, context} = of_expr(map, stack, context)

    Of.permutate_map(args, stack, context, &of_expr/3, fn fallback, keys, pairs ->
      # If there is no fallback (i.e. it is closed), we can update the existing map,
      # otherwise we only assert the existing keys.
      keys = if fallback == none(), do: keys, else: Enum.map(pairs, &elem(&1, 0)) ++ keys

      # Assert the keys exist
      Enum.each(keys, fn key ->
        case map_fetch(map_type, key) do
          {_, _} -> :ok
          :badkey -> throw({:badkey, map_type, key, expr, context})
          :badmap -> throw({:badmap, map_type, expr, context})
        end
      end)

      if fallback == none() do
        Enum.reduce(pairs, map_type, fn {key, type}, acc ->
          case map_fetch_and_put(acc, key, type) do
            {_value, descr} -> descr
            :badkey -> throw({:badkey, map_type, key, expr, context})
            :badmap -> throw({:badmap, map_type, expr, context})
          end
        end)
      else
        # TODO: Use the fallback type to actually indicate if open or closed.
        # The fallback must be unioned with the result of map_values with all
        # `keys` deleted.
        open_map(pairs)
      end
    end)
  catch
    error -> {error_type(), error(__MODULE__, error, meta, stack, context)}
  end

  # %Struct{map | ...}
  # Note this code, by definition, adds missing struct fields to `map`
  # because at runtime we do not check for them (only for __struct__ itself).
  # TODO: Once we support typed structs, we need to type check them here.
  def of_expr(
        {:%, struct_meta, [module, {:%{}, _, [{:|, update_meta, [map, args]}]}]} = expr,
        stack,
        context
      ) do
    {info, context} = Of.struct_info(module, struct_meta, stack, context)
    struct_type = Of.struct_type(module, info)
    {map_type, context} = of_expr(map, stack, context)

    if disjoint?(struct_type, map_type) do
      warning = {:badstruct, expr, struct_type, map_type, context}
      {error_type(), error(__MODULE__, warning, update_meta, stack, context)}
    else
      map_type = map_put!(map_type, :__struct__, atom([module]))

      Enum.reduce(args, {map_type, context}, fn
        {key, value}, {map_type, context} when is_atom(key) ->
          {value_type, context} = of_expr(value, stack, context)
          {map_put!(map_type, key, value_type), context}
      end)
    end
  end

  # %{...}
  def of_expr({:%{}, _meta, args}, stack, context) do
    Of.closed_map(args, stack, context, &of_expr/3)
  end

  # %Struct{}
  def of_expr({:%, meta, [module, {:%{}, _, args}]}, stack, context) do
    Of.struct_instance(module, args, meta, stack, context, &of_expr/3)
  end

  # ()
  def of_expr({:__block__, _meta, []}, _stack, context) do
    {atom([nil]), context}
  end

  # (expr; expr)
  def of_expr({:__block__, _meta, exprs}, stack, context) do
    {pre, [post]} = Enum.split(exprs, -1)

    context =
      Enum.reduce(pre, context, fn expr, context ->
        {_, context} = of_expr(expr, stack, context)
        context
      end)

    of_expr(post, stack, context)
  end

  def of_expr({:cond, _meta, [[{:do, clauses}]]}, stack, context) do
    clauses
    |> reduce_non_empty({none(), context}, fn
      {:->, meta, [[head], body]}, {acc, context}, last? ->
        {head_type, context} = of_expr(head, stack, context)

        context =
          if stack.mode in [:infer, :traversal] do
            context
          else
            case truthness(head_type) do
              :always_true when not last? ->
                warning = {:badcond, "always match", head_type, head, context}
                warn(__MODULE__, warning, meta, stack, context)

              :always_false ->
                warning = {:badcond, "never match", head_type, head, context}
                warn(__MODULE__, warning, meta, stack, context)

              _ ->
                context
            end
          end

        {body_type, context} = of_expr(body, stack, context)
        {union(body_type, acc), context}
    end)
    |> dynamic_unless_static(stack)
  end

  def of_expr({:case, meta, [case_expr, [{:do, clauses}]]}, stack, context) do
    {case_type, context} = of_expr(case_expr, stack, context)

    # If we are only type checking the expression and the expression is a literal,
    # let's mark it as generated, as it is most likely a macro code.
    if is_atom(case_expr) and {:type_check, :expr} in meta do
      for {:->, meta, args} <- clauses, do: {:->, [generated: true] ++ meta, args}
    else
      clauses
    end
    |> of_clauses([case_type], {:case, meta, case_type, case_expr}, stack, {none(), context})
    |> dynamic_unless_static(stack)
  end

  # TODO: fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, stack, context) do
    [{:->, _, [head, _]} | _] = clauses
    {patterns, _guards} = extract_head(head)
    expected = Enum.map(patterns, fn _ -> dynamic() end)
    {_acc, context} = of_clauses(clauses, expected, :fn, stack, {none(), context})
    {fun(), context}
  end

  def of_expr({:try, _meta, [[do: body] ++ blocks]}, stack, context) do
    {body_type, context} = of_expr(body, stack, context)
    initial = if Keyword.has_key?(blocks, :else), do: none(), else: body_type

    blocks
    |> Enum.reduce({initial, context}, fn
      {:rescue, clauses}, acc_context ->
        Enum.reduce(clauses, acc_context, fn
          {:->, _, [[{:in, meta, [var, exceptions]} = expr], body]}, {acc, context} ->
            {type, context} = of_rescue(var, exceptions, body, expr, [], meta, stack, context)
            {union(type, acc), context}

          {:->, meta, [[var], body]}, {acc, context} ->
            hint = [:anonymous_rescue]
            {type, context} = of_rescue(var, [], body, var, hint, meta, stack, context)
            {union(type, acc), context}
        end)

      {:after, body}, {acc, context} ->
        {_type, context} = of_expr(body, stack, context)
        {acc, context}

      {:catch, clauses}, acc_context ->
        of_clauses(clauses, [@try_catch, dynamic()], :try_catch, stack, acc_context)

      {:else, clauses}, acc_context ->
        of_clauses(clauses, [body_type], {:try_else, body_type}, stack, acc_context)
    end)
    |> dynamic_unless_static(stack)
  end

  def of_expr({:receive, _meta, [blocks]}, stack, context) do
    blocks
    |> Enum.reduce({none(), context}, fn
      {:do, {:__block__, _, []}}, acc_context ->
        acc_context

      {:do, clauses}, acc_context ->
        of_clauses(clauses, [dynamic()], :receive, stack, acc_context)

      {:after, [{:->, meta, [[timeout], body]}]}, {acc, context} ->
        {timeout_type, context} = of_expr(timeout, stack, context)
        {body_type, context} = of_expr(body, stack, context)

        if integer_type?(timeout_type) do
          {union(body_type, acc), context}
        else
          error = {:badtimeout, timeout_type, timeout, context}
          {union(body_type, acc), error(__MODULE__, error, meta, stack, context)}
        end
    end)
    |> dynamic_unless_static(stack)
  end

  # TODO: for pat <- expr do expr end
  def of_expr({:for, _meta, [_ | _] = args}, stack, context) do
    {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)
    context = Enum.reduce(clauses, context, &for_clause(&1, stack, &2))
    context = Enum.reduce(opts, context, &for_option(&1, stack, &2))

    if Keyword.has_key?(opts, :reduce) do
      {_, context} = of_clauses(block, [dynamic()], :for_reduce, stack, {none(), context})
      {dynamic(), context}
    else
      {_type, context} = of_expr(block, stack, context)
      {dynamic(), context}
    end
  end

  # TODO: with pat <- expr do expr end
  def of_expr({:with, _meta, [_ | _] = clauses}, stack, context) do
    {clauses, [options]} = Enum.split(clauses, -1)
    context = Enum.reduce(clauses, context, &with_clause(&1, stack, &2))
    context = Enum.reduce(options, context, &with_option(&1, stack, &2))
    {dynamic(), context}
  end

  # TODO: fun.(args)
  def of_expr({{:., meta, [fun]}, _meta, args} = call, stack, context) do
    {fun_type, context} = of_expr(fun, stack, context)
    {_args_types, context} = Enum.map_reduce(args, context, &of_expr(&1, stack, &2))

    case fun_fetch(fun_type, length(args)) do
      :ok ->
        {dynamic(), context}

      :error ->
        error = {:badfun, length(args), fun_type, fun, call, context}
        {error_type(), error(__MODULE__, error, meta, stack, context)}
    end
  end

  def of_expr({{:., _, [callee, key_or_fun]}, meta, []} = expr, stack, context)
      when not is_atom(callee) and is_atom(key_or_fun) do
    {type, context} = of_expr(callee, stack, context)

    if Keyword.get(meta, :no_parens, false) do
      Of.map_fetch(expr, type, key_or_fun, stack, context)
    else
      {mods, context} = Of.modules(type, key_or_fun, 0, [:dot], expr, meta, stack, context)
      apply_many(mods, key_or_fun, [], expr, stack, context)
    end
  end

  def of_expr({{:., _, [remote, name]}, meta, args} = expr, stack, context) do
    {remote_type, context} = of_expr(remote, stack, context)
    {args_types, context} = Enum.map_reduce(args, context, &of_expr(&1, stack, &2))
    {mods, context} = Of.modules(remote_type, name, length(args), expr, meta, stack, context)
    apply_many(mods, name, args_types, expr, stack, context)
  end

  # TODO: &Foo.bar/1
  def of_expr(
        {:&, _, [{:/, _, [{{:., _, [remote, name]}, meta, []}, arity]}]} = expr,
        stack,
        context
      )
      when is_atom(name) and is_integer(arity) do
    {remote_type, context} = of_expr(remote, stack, context)
    {mods, context} = Of.modules(remote_type, name, arity, expr, meta, stack, context)
    Apply.remote_capture(mods, name, arity, meta, stack, context)
  end

  # TODO: &foo/1
  def of_expr({:&, _meta, [{:/, _, [{fun, meta, _}, arity]}]}, stack, context) do
    Apply.local_capture(fun, arity, meta, stack, context)
  end

  # Super
  def of_expr({:super, meta, args} = expr, stack, context) when is_list(args) do
    {_kind, fun} = Keyword.fetch!(meta, :super)
    {args_types, context} = Enum.map_reduce(args, context, &of_expr(&1, stack, &2))
    Apply.local(fun, args_types, expr, stack, context)
  end

  # Local calls
  def of_expr({fun, _meta, args} = expr, stack, context)
      when is_atom(fun) and is_list(args) do
    {args_types, context} = Enum.map_reduce(args, context, &of_expr(&1, stack, &2))
    Apply.local(fun, args_types, expr, stack, context)
  end

  # var
  def of_expr(var, stack, context) when is_var(var) do
    if stack.mode == :traversal do
      {dynamic(), context}
    else
      {Of.var(var, context), context}
    end
  end

  ## Try

  defp of_rescue(var, exceptions, body, expr, hints, meta, stack, context) do
    args = [__exception__: @atom_true]

    {structs, context} =
      Enum.map_reduce(exceptions, context, fn exception, context ->
        # Exceptions are not validated in the compiler,
        # to avoid export dependencies. So we do it here.
        if Code.ensure_loaded?(exception) and function_exported?(exception, :__struct__, 0) do
          {info, context} = Of.struct_info(exception, meta, stack, context)
          # TODO: For properly defined structs, this should not be dynamic
          {dynamic(Of.struct_type(exception, info, args)), context}
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
          formatter = fn expr -> {"rescue #{expr_to_string(expr)} ->", hints} end
          {_ok?, _type, context} = Of.refine_var(var, expected, expr, formatter, stack, context)
          context
      end

    of_expr(body, stack, context)
  end

  ## Comprehensions

  defp for_clause({:<-, _, [left, right]} = expr, stack, context) do
    {pattern, guards} = extract_head([left])
    {_, context} = of_expr(right, stack, context)

    {_type, context} =
      Pattern.of_match(pattern, guards, dynamic(), expr, :for, stack, context)

    context
  end

  defp for_clause({:<<>>, _, [{:<-, meta, [left, right]}]} = expr, stack, context) do
    {right_type, context} = of_expr(right, stack, context)

    {_pattern_type, context} =
      Pattern.of_match(left, binary(), expr, :for, stack, context)

    if binary_type?(right_type) do
      context
    else
      error = {:badbinary, right_type, right, context}
      error(__MODULE__, error, meta, stack, context)
    end
  end

  defp for_clause(expr, stack, context) do
    {_type, context} = of_expr(expr, stack, context)
    context
  end

  defp for_option({:into, expr}, stack, context) do
    {_type, context} = of_expr(expr, stack, context)
    context
  end

  defp for_option({:reduce, expr}, stack, context) do
    {_type, context} = of_expr(expr, stack, context)
    context
  end

  defp for_option({:uniq, _}, _stack, context) do
    context
  end

  ## With

  defp with_clause({:<-, _meta, [left, right]} = expr, stack, context) do
    {pattern, guards} = extract_head([left])
    {_type, context} = Pattern.of_match(pattern, guards, dynamic(), :with, expr, stack, context)
    {_, context} = of_expr(right, stack, context)
    context
  end

  defp with_clause(expr, stack, context) do
    {_type, context} = of_expr(expr, stack, context)
    context
  end

  defp with_option({:do, body}, stack, context) do
    {_type, context} = of_expr(body, stack, context)
    context
  end

  defp with_option({:else, clauses}, stack, context) do
    {_, context} = of_clauses(clauses, [dynamic()], :with_else, stack, {none(), context})
    context
  end

  ## General helpers

  defp apply_many([], function, args_types, expr, stack, context) do
    Apply.remote(function, args_types, expr, stack, context)
  end

  defp apply_many([mod], function, args_types, expr, stack, context) do
    Apply.remote(mod, function, args_types, expr, stack, context)
  end

  defp apply_many(mods, function, args_types, expr, stack, context) do
    {returns, context} =
      Enum.map_reduce(mods, context, fn mod, context ->
        Apply.remote(mod, function, args_types, expr, stack, context)
      end)

    {Enum.reduce(returns, &union/2), context}
  end

  defp reduce_non_empty([last], acc, fun),
    do: fun.(last, acc, true)

  defp reduce_non_empty([head | tail], acc, fun),
    do: reduce_non_empty(tail, fun.(head, acc, false), fun)

  defp dynamic_unless_static({_, _} = output, %{mode: :static}), do: output
  defp dynamic_unless_static({type, context}, %{mode: _}), do: {dynamic(type), context}

  defp of_clauses(clauses, expected, info, %{mode: mode} = stack, {acc, context}) do
    %{failed: failed?} = context

    Enum.reduce(clauses, {acc, context}, fn {:->, meta, [head, body]}, {acc, context} ->
      {failed?, context} = reset_failed(context, failed?)
      {patterns, guards} = extract_head(head)
      {_types, context} = Pattern.of_head(patterns, guards, expected, info, meta, stack, context)
      {body, context} = of_expr(body, stack, context)
      context = set_failed(context, failed?)

      if mode == :traversal do
        {dynamic(), context}
      else
        {union(acc, body), context}
      end
    end)
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

  defp map_put!(map_type, key, value_type) do
    case map_put(map_type, key, value_type) do
      {:ok, descr} -> descr
      error -> raise "unexpected #{inspect(error)}"
    end
  end

  defp repack_match(left_expr, {:=, meta, [new_left, new_right]}),
    do: repack_match({:=, meta, [left_expr, new_left]}, new_right)

  defp repack_match(left_expr, right_expr),
    do: {left_expr, right_expr}

  ## Warning formatting

  def format_diagnostic({:badstruct, expr, expected_type, actual_type, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types in struct update:

              #{expr_to_string(expr) |> indent(4)}

          expected type:

              #{to_quoted_string(expected_type) |> indent(4)}

          but got type:

              #{to_quoted_string(actual_type) |> indent(4)}
          """,
          format_traces(traces)
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

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
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

  def format_diagnostic({:badfun, arity, type, fun_expr, call_expr, context}) do
    traces = collect_traces(fun_expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a #{arity}-arity function on call:

              #{expr_to_string(call_expr) |> indent(4)}

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
