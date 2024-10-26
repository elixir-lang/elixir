defmodule Module.Types.Expr do
  @moduledoc false

  alias Module.Types.{Of, Pattern}
  import Module.Types.{Helpers, Descr}

  14 = length(Macro.Env.__info__(:struct))

  aliases = list(tuple([atom(), atom()]))
  functions_and_macros = list(tuple([atom(), list(tuple([atom(), integer()]))]))
  list_of_modules = list(atom())

  @caller closed_map(
            __struct__: atom([Macro.Env]),
            aliases: aliases,
            context: atom([:match, :guard, nil]),
            context_modules: list_of_modules,
            file: binary(),
            function: union(tuple(), atom([nil])),
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
    {non_empty_list(Enum.reduce(prefix, &union/2), suffix), context}
  end

  # {left, right}
  def of_expr({left, right}, stack, context) do
    {left, context} = of_expr(left, stack, context)
    {right, context} = of_expr(right, stack, context)
    {tuple([left, right]), context}
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args}, stack, context) do
    context = Of.binary(args, :expr, stack, context)
    {binary(), context}
  end

  def of_expr({:__CALLER__, _meta, var_context}, _stack, context) when is_atom(var_context) do
    {@caller, context}
  end

  # TODO: __STACKTRACE__
  def of_expr({:__STACKTRACE__, _meta, var_context}, _stack, context)
      when is_atom(var_context) do
    {list(term()), context}
  end

  # {...}
  def of_expr({:{}, _meta, exprs}, stack, context) do
    {types, context} = Enum.map_reduce(exprs, context, &of_expr(&1, stack, &2))
    {tuple(types), context}
  end

  # left = right
  def of_expr({:=, _meta, [left_expr, right_expr]} = expr, stack, context) do
    {right_type, context} = of_expr(right_expr, stack, context)
    Pattern.of_match(left_expr, right_type, expr, stack, context)
  end

  # %{map | ...}
  def of_expr({:%{}, _, [{:|, _, [map, args]}]}, stack, context) do
    {_args_type, context} = Of.closed_map(args, stack, context, &of_expr/3)
    {_map_type, context} = of_expr(map, stack, context)
    # TODO: intersect map with keys of terms for args
    # TODO: Merge args_type into map_type with dynamic/static key requirement
    {dynamic(open_map()), context}
  end

  # %Struct{map | ...}
  def of_expr(
        {:%, struct_meta, [module, {:%{}, _, [{:|, update_meta, [map, args]}]}]} = expr,
        stack,
        context
      ) do
    {args_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context when is_atom(key) ->
        {type, context} = of_expr(value, stack, context)
        {{key, type}, context}
      end)

    # TODO: args_types could be an empty list
    {struct_type, context} =
      Of.struct(module, args_types, :only_defaults, struct_meta, stack, context)

    {map_type, context} = of_expr(map, stack, context)

    if disjoint?(struct_type, map_type) do
      warning = {:badupdate, :struct, expr, struct_type, map_type, context}
      {error_type(), error(__MODULE__, warning, update_meta, stack, context)}
    else
      # TODO: Merge args_type into map_type with dynamic/static key requirement
      Of.struct(module, args_types, :merge_defaults, struct_meta, stack, context)
    end
  end

  # %{...}
  def of_expr({:%{}, _meta, args}, stack, context) do
    Of.closed_map(args, stack, context, &of_expr/3)
  end

  # %Struct{}
  def of_expr({:%, _, [module, {:%{}, _, args}]} = expr, stack, context) do
    # TODO: We should not skip defaults
    Of.struct(expr, module, args, :skip_defaults, stack, context, &of_expr/3)
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

  # TODO: cond do pat -> expr end
  def of_expr({:cond, _meta, [[{:do, clauses}]]}, stack, context) do
    context =
      Enum.reduce(clauses, context, fn {:->, _meta, [head, body]}, context ->
        {_, context} = of_expr(head, stack, context)
        {_, context} = of_expr(body, stack, context)
        context
      end)

    {dynamic(), context}
  end

  # TODO: case expr do pat -> expr end
  def of_expr({:case, _meta, [case_expr, [{:do, clauses}]]}, stack, context) do
    {_expr_type, context} = of_expr(case_expr, stack, context)
    context = of_clauses(clauses, stack, context)
    {dynamic(), context}
  end

  # TODO: fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, stack, context) do
    context = of_clauses(clauses, stack, context)
    {fun(), context}
  end

  @try_blocks [:do, :after]
  @try_clause_blocks [:catch, :else]

  # TODO: try do expr end
  def of_expr({:try, _meta, [blocks]}, stack, context) do
    context =
      Enum.reduce(blocks, context, fn
        {:rescue, clauses}, context ->
          Enum.reduce(clauses, context, fn
            {:->, _, [[{:in, meta, [var, exceptions]} = expr], body]}, context ->
              of_rescue(var, exceptions, body, expr, [], meta, stack, context)

            {:->, meta, [[var], body]}, context ->
              of_rescue(var, [], body, var, [:anonymous_rescue], meta, stack, context)
          end)

        {block, body}, context when block in @try_blocks ->
          of_expr_context(body, stack, context)

        {block, clauses}, context when block in @try_clause_blocks ->
          of_clauses(clauses, stack, context)
      end)

    {dynamic(), context}
  end

  # TODO: receive do pat -> expr end
  def of_expr({:receive, _meta, [blocks]}, stack, context) do
    context =
      Enum.reduce(blocks, context, fn
        {:do, {:__block__, _, []}}, context ->
          context

        {:do, clauses}, context ->
          of_clauses(clauses, stack, context)

        {:after, [{:->, _meta, [head, body]}]}, context ->
          {_type, context} = of_expr(head, stack, context)
          {_type, context} = of_expr(body, stack, context)
          context
      end)

    {dynamic(), context}
  end

  # TODO: for pat <- expr do expr end
  def of_expr({:for, _meta, [_ | _] = args}, stack, context) do
    {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)
    context = Enum.reduce(clauses, context, &for_clause(&1, stack, &2))
    context = Enum.reduce(opts, context, &for_option(&1, stack, &2))

    if Keyword.has_key?(opts, :reduce) do
      context = of_clauses(block, stack, context)
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
  def of_expr({{:., _meta1, [fun]}, _meta2, args}, stack, context) do
    {fun_type, context} = of_expr(fun, stack, context)
    {_args_types, context} = Enum.map_reduce(args, context, &of_expr(&1, stack, &2))

    case fun_fetch(fun_type, length(args)) do
      :ok -> {dynamic(), context}
      :error -> {dynamic(), Of.incompatible_error(fun, fun(), fun_type, stack, context)}
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

  # TODO: expr.fun(arg)
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
    # TODO: We cannot return the unions of functions. Do we forbid this?
    # Do we check it is always the same return type? Do we simply say it is a function?
    {mods, context} = Of.modules(remote_type, name, arity, expr, meta, stack, context)

    context =
      Enum.reduce(mods, context, &(Of.remote(&1, name, arity, meta, stack, &2) |> elem(1)))

    {fun(), context}
  end

  # &foo/1
  # TODO: & &1
  def of_expr({:&, _meta, _arg}, _stack, context) do
    {fun(), context}
  end

  # TODO: local_call(arg)
  def of_expr({fun, _meta, args}, stack, context)
      when is_atom(fun) and is_list(args) do
    {_arg_types, context} = Enum.map_reduce(args, context, &of_expr(&1, stack, &2))
    {dynamic(), context}
  end

  # var
  def of_expr(var, _stack, context) when is_var(var) do
    {Of.var(var, context), context}
  end

  ## Try

  defp of_rescue(var, exceptions, body, expr, hints, meta, stack, context) do
    args = [__exception__: @atom_true]

    {structs, context} =
      Enum.map_reduce(exceptions, context, fn exception, context ->
        # Exceptions are not validated in the compiler,
        # to avoid export dependencies. So we do it here.
        if Code.ensure_loaded?(exception) and function_exported?(exception, :__struct__, 0) do
          Of.struct(exception, args, :merge_defaults, meta, stack, context)
        else
          # If the exception cannot be found or is invalid,
          # we call Of.remote/5 to emit a warning.
          {_, context} = Of.remote(exception, :__struct__, 0, meta, stack, context)
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

    of_expr_context(body, stack, context)
  end

  ## Comprehensions

  defp for_clause({:<-, meta, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])
    {_expr_type, context} = of_expr(expr, stack, context)
    {[_type], context} = Pattern.of_head([pattern], guards, meta, stack, context)
    context
  end

  defp for_clause({:<<>>, _, [{:<-, meta, [left, right]}]}, stack, context) do
    {right_type, context} = of_expr(right, stack, context)
    {_pattern_type, context} = Pattern.of_match(left, binary(), left, stack, context)

    if binary_type?(right_type) do
      context
    else
      warning = {:badbinary, right_type, right, context}
      error(__MODULE__, warning, meta, stack, context)
    end
  end

  defp for_clause(expr, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp for_option({:into, expr}, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp for_option({:reduce, expr}, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp for_option({:uniq, _}, _stack, context) do
    context
  end

  ## With

  defp with_clause({:<-, meta, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    {[_type], context} = Pattern.of_head([pattern], guards, meta, stack, context)
    {_expr_type, context} = of_expr(expr, stack, context)
    context
  end

  defp with_clause(expr, stack, context) do
    of_expr_context(expr, stack, context)
  end

  defp with_option({:do, body}, stack, context) do
    of_expr_context(body, stack, context)
  end

  defp with_option({:else, clauses}, stack, context) do
    of_clauses(clauses, stack, context)
  end

  ## General helpers

  defp apply_many([], _function, _args_types, _expr, _stack, context) do
    {dynamic(), context}
  end

  defp apply_many([mod], function, args_types, expr, stack, context) do
    Of.apply(mod, function, args_types, expr, stack, context)
  end

  defp apply_many(mods, function, args_types, expr, stack, context) do
    {returns, context} =
      Enum.map_reduce(mods, context, fn mod, context ->
        Of.apply(mod, function, args_types, expr, stack, context)
      end)

    {Enum.reduce(returns, &union/2), context}
  end

  defp of_clauses(clauses, stack, context) do
    Enum.reduce(clauses, context, fn {:->, meta, [head, body]}, context ->
      {patterns, guards} = extract_head(head)

      {_types, context} = Pattern.of_head(patterns, guards, meta, stack, context)
      {_, context} = of_expr(body, stack, context)
      context
    end)
  end

  defp extract_head([{:when, _meta, args}]) do
    case Enum.split(args, -1) do
      {patterns, [guards]} -> {patterns, flatten_when(guards)}
      {patterns, []} -> {patterns, []}
    end
  end

  defp extract_head(other) do
    {other, []}
  end

  defp flatten_when({:when, _meta, [left, right]}) do
    [left | flatten_when(right)]
  end

  defp flatten_when(other) do
    [other]
  end

  defp of_expr_context(expr, stack, context) do
    {_type, context} = of_expr(expr, stack, context)
    context
  end

  ## Warning formatting

  def format_diagnostic({:badupdate, type, expr, expected_type, actual_type, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types in #{type} update:

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
end
