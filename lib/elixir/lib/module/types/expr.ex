defmodule Module.Types.Expr do
  @moduledoc false

  alias Module.Types.{Of, Pattern}
  import Module.Types.{Helpers, Descr}

  14 = length(Macro.Env.__info__(:struct))

  @caller closed_map(
            __struct__: atom([Macro.Env]),
            aliases: list(),
            context: atom([:match, :guard, nil]),
            context_modules: list(),
            file: binary(),
            function: union(tuple(), atom([nil])),
            functions: list(),
            lexical_tracker: union(pid(), atom([nil])),
            line: integer(),
            macro_aliases: list(),
            macros: list(),
            module: atom(),
            requires: list(),
            tracers: list(),
            versioned_vars: open_map()
          )

  @atom_true atom([true])
  @exception open_map(__struct__: atom(), __exception__: @atom_true)

  defp of_expr(expr, expected_expr, stack, context) do
    with {:ok, actual, context} <- of_expr(expr, stack, context) do
      Of.intersect(actual, expected_expr, stack, context)
    end
  end

  # :atom
  def of_expr(atom, _stack, context) when is_atom(atom) do
    {:ok, atom([atom]), context}
  end

  # 12
  def of_expr(literal, _stack, context) when is_integer(literal) do
    {:ok, integer(), context}
  end

  # 1.2
  def of_expr(literal, _stack, context) when is_float(literal) do
    {:ok, float(), context}
  end

  # "..."
  def of_expr(literal, _stack, context) when is_binary(literal) do
    {:ok, binary(), context}
  end

  # #PID<...>
  def of_expr(literal, _stack, context) when is_pid(literal) do
    {:ok, pid(), context}
  end

  # []
  def of_expr([], _stack, context) do
    {:ok, empty_list(), context}
  end

  # TODO: [expr, ...]
  def of_expr(exprs, stack, context) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, context} -> {:error, context}
    end
  end

  # {left, right}
  def of_expr({left, right}, stack, context) do
    with {:ok, left, context} <- of_expr(left, stack, context),
         {:ok, right, context} <- of_expr(right, stack, context) do
      {:ok, tuple([left, right]), context}
    end
  end

  # <<...>>>
  def of_expr({:<<>>, _meta, args}, stack, context) do
    case Of.binary(args, :expr, stack, context, &of_expr/4) do
      {:ok, context} -> {:ok, binary(), context}
      # It is safe to discard errors from binary inside expressions
      {:error, context} -> {:ok, binary(), context}
    end
  end

  # TODO: left | []
  def of_expr({:|, _meta, [left_expr, []]}, stack, context) do
    of_expr(left_expr, stack, context)
  end

  # TODO: left | right
  def of_expr({:|, _meta, [left_expr, right_expr]}, stack, context) do
    case of_expr(left_expr, stack, context) do
      {:ok, _left, context} ->
        of_expr(right_expr, stack, context)

      {:error, context} ->
        {:error, context}
    end
  end

  def of_expr({:__CALLER__, _meta, var_context}, _stack, context)
      when is_atom(var_context) do
    {:ok, @caller, context}
  end

  # TODO: __STACKTRACE__
  def of_expr({:__STACKTRACE__, _meta, var_context}, _stack, context)
      when is_atom(var_context) do
    {:ok, list(), context}
  end

  # {...}
  def of_expr({:{}, _meta, exprs}, stack, context) do
    with {:ok, types, context} <- map_reduce_ok(exprs, context, &of_expr(&1, stack, &2)) do
      {:ok, tuple(types), context}
    end
  end

  # TODO: left = right
  def of_expr({:=, _meta, [left_expr, right_expr]} = expr, stack, context) do
    with {:ok, right_type, context} <- of_expr(right_expr, stack, context) do
      Pattern.of_match(left_expr, {right_type, expr}, stack, context)
    end
  end

  # %{map | ...}
  def of_expr({:%{}, _, [{:|, _, [map, args]}]}, stack, context) do
    with {:ok, _args_type, context} <- Of.closed_map(args, stack, context, &of_expr/3),
         {:ok, _map_type, context} <- of_expr(map, stack, context) do
      # TODO: intersect map with keys of terms for args
      # TODO: Merge args_type into map_type with dynamic/static key requirement
      {:ok, dynamic(open_map()), context}
    end
  end

  # %Struct{map | ...}
  def of_expr(
        {:%, struct_meta, [module, {:%{}, _, [{:|, update_meta, [map, args]}]}]} = expr,
        stack,
        context
      ) do
    with {:ok, args_types, context} <-
           map_reduce_ok(args, context, fn {key, value}, context when is_atom(key) ->
             with {:ok, type, context} <- of_expr(value, stack, context) do
               {:ok, {key, type}, context}
             end
           end),
         {:ok, struct_type, context} <-
           Of.struct(module, args_types, :only_defaults, struct_meta, stack, context),
         {:ok, map_type, context} <- of_expr(map, stack, context) do
      if disjoint?(struct_type, map_type) do
        warning = {:badupdate, :struct, expr, struct_type, map_type, context}
        {:ok, error_type(), warn(__MODULE__, warning, update_meta, stack, context)}
      else
        # TODO: Merge args_type into map_type with dynamic/static key requirement
        Of.struct(module, args_types, :merge_defaults, struct_meta, stack, context)
      end
    end
  end

  # %{...}
  def of_expr({:%{}, _meta, args}, stack, context) do
    Of.closed_map(args, stack, context, &of_expr/3)
  end

  # %Struct{}
  def of_expr({:%, _, [module, {:%{}, _, args}]} = expr, stack, context) do
    Of.struct(expr, module, args, :skip_defaults, stack, context, &of_expr/3)
  end

  # ()
  def of_expr({:__block__, _meta, []}, _stack, context) do
    {:ok, atom([nil]), context}
  end

  # (expr; expr)
  def of_expr({:__block__, _meta, exprs}, stack, context) do
    {pre, [post]} = Enum.split(exprs, -1)

    result =
      map_reduce_ok(pre, context, fn expr, context ->
        of_expr(expr, stack, context)
      end)

    case result do
      {:ok, _, context} -> of_expr(post, stack, context)
      {:error, context} -> {:error, context}
    end
  end

  # TODO: cond do pat -> expr end
  def of_expr({:cond, _meta, [[{:do, clauses}]]}, stack, context) do
    {result, context} =
      reduce_ok(clauses, context, fn {:->, _meta, [head, body]}, context ->
        with {:ok, _, context} <- of_expr(head, stack, context),
             {:ok, _, context} <- of_expr(body, stack, context),
             do: {:ok, context}
      end)

    case result do
      :ok -> {:ok, dynamic(), context}
      :error -> {:error, context}
    end
  end

  # TODO: case expr do pat -> expr end
  def of_expr({:case, _meta, [case_expr, [{:do, clauses}]]}, stack, context) do
    with {:ok, _expr_type, context} <- of_expr(case_expr, stack, context),
         {:ok, context} <- of_clauses(clauses, stack, context),
         do: {:ok, dynamic(), context}
  end

  # TODO: fn pat -> expr end
  def of_expr({:fn, _meta, clauses}, stack, context) do
    case of_clauses(clauses, stack, context) do
      {:ok, context} -> {:ok, fun(), context}
      {:error, context} -> {:error, context}
    end
  end

  @try_blocks [:do, :after]
  @try_clause_blocks [:catch, :else]

  # TODO: try do expr end
  def of_expr({:try, _meta, [blocks]}, stack, context) do
    {result, context} =
      reduce_ok(blocks, context, fn
        {:rescue, clauses}, context ->
          reduce_ok(clauses, context, fn
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

    case result do
      :ok -> {:ok, dynamic(), context}
      :error -> {:error, context}
    end
  end

  # TODO: receive do pat -> expr end
  def of_expr({:receive, _meta, [blocks]}, stack, context) do
    {result, context} =
      reduce_ok(blocks, context, fn
        {:do, {:__block__, _, []}}, context ->
          {:ok, context}

        {:do, clauses}, context ->
          of_clauses(clauses, stack, context)

        {:after, [{:->, _meta, [head, body]}]}, context ->
          with {:ok, _type, context} <- of_expr(head, stack, context),
               {:ok, _type, context} <- of_expr(body, stack, context),
               do: {:ok, context}
      end)

    case result do
      :ok -> {:ok, dynamic(), context}
      :error -> {:error, context}
    end
  end

  # TODO: for pat <- expr do expr end
  def of_expr({:for, _meta, [_ | _] = args}, stack, context) do
    {clauses, [[{:do, block} | opts]]} = Enum.split(args, -1)

    with {:ok, context} <- reduce_ok(clauses, context, &for_clause(&1, stack, &2)),
         {:ok, context} <- reduce_ok(opts, context, &for_option(&1, stack, &2)) do
      if Keyword.has_key?(opts, :reduce) do
        with {:ok, context} <- of_clauses(block, stack, context) do
          {:ok, dynamic(), context}
        end
      else
        with {:ok, _type, context} <- of_expr(block, stack, context) do
          {:ok, dynamic(), context}
        end
      end
    end
  end

  # TODO: with pat <- expr do expr end
  def of_expr({:with, _meta, [_ | _] = clauses}, stack, context) do
    case reduce_ok(clauses, context, &with_clause(&1, stack, &2)) do
      {:ok, context} -> {:ok, dynamic(), context}
      {:error, context} -> {:error, context}
    end
  end

  # TODO: fun.(args)
  def of_expr({{:., _meta1, [fun]}, _meta2, args}, stack, context) do
    with {:ok, fun_type, context} <- of_expr(fun, stack, context),
         {:ok, _args_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      context =
        case fun_fetch(fun_type, length(args)) do
          :ok -> context
          :error -> Of.incompatible_warn(fun, fun(), fun_type, stack, context)
        end

      {:ok, dynamic(), context}
    end
  end

  def of_expr({{:., _, [callee, key_or_fun]}, meta, []} = expr, stack, context)
      when not is_atom(callee) and is_atom(key_or_fun) do
    with {:ok, type, context} <- of_expr(callee, stack, context) do
      if Keyword.get(meta, :no_parens, false) do
        Of.map_fetch(expr, type, key_or_fun, stack, context)
      else
        {mods, context} = Of.remote(type, key_or_fun, 0, [:dot], expr, meta, stack, context)
        apply_many(mods, key_or_fun, [], expr, stack, context)
      end
    end
  end

  # TODO: expr.fun(arg)
  def of_expr({{:., _, [remote, name]}, meta, args} = expr, stack, context) do
    with {:ok, remote_type, context} <- of_expr(remote, stack, context),
         {:ok, args_types, context} <- map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {mods, context} = Of.remote(remote_type, name, length(args), expr, meta, stack, context)
      apply_many(mods, name, args_types, expr, stack, context)
    end
  end

  # TODO: &Foo.bar/1
  def of_expr(
        {:&, _, [{:/, _, [{{:., _, [remote, name]}, meta, []}, arity]}]} = expr,
        stack,
        context
      )
      when is_atom(name) and is_integer(arity) do
    with {:ok, remote_type, context} <- of_expr(remote, stack, context) do
      # TODO: We cannot return the unions of functions. Do we forbid this?
      # Do we check it is always the same return type? Do we simply say it is a function?
      {_mods, context} = Of.remote(remote_type, name, arity, expr, meta, stack, context)
      {:ok, fun(), context}
    end
  end

  # &foo/1
  # TODO: & &1
  def of_expr({:&, _meta, _arg}, _stack, context) do
    {:ok, fun(), context}
  end

  # TODO: call(arg)
  def of_expr({fun, _meta, args}, stack, context)
      when is_atom(fun) and is_list(args) do
    with {:ok, _arg_types, context} <-
           map_reduce_ok(args, context, &of_expr(&1, stack, &2)) do
      {:ok, dynamic(), context}
    end
  end

  # var
  def of_expr(var, _stack, context) when is_var(var) do
    {:ok, Of.var(var, context), context}
  end

  ## Try

  defp of_rescue(var, exceptions, body, expr, hints, meta, stack, context) do
    args = [__exception__: @atom_true]

    with {:ok, structs, context} <-
           map_reduce_ok(exceptions, context, fn exception, context ->
             # Exceptions are not validated in the compiler,
             # to avoid export dependencies. So we do it here.
             if Code.ensure_loaded?(exception) and function_exported?(exception, :__struct__, 0) do
               Of.struct(exception, args, :merge_defaults, meta, stack, context)
             else
               # If the exception cannot be found or is invalid,
               # we call Of.remote/5 to emit a warning.
               context = Of.remote(exception, :__struct__, 0, meta, stack, context)
               {:ok, error_type(), context}
             end
           end) do
      context =
        case var do
          {:_, _, _} ->
            context

          _ ->
            expected = if structs == [], do: @exception, else: Enum.reduce(structs, &union/2)

            formatter = fn expr ->
              {"rescue #{expr_to_string(expr)} ->", hints}
            end

            {:ok, _type, context} = Of.refine_var(var, expected, expr, formatter, stack, context)
            context
        end

      of_expr_context(body, stack, context)
    end
  end

  ## Comprehensions

  defp for_clause({:<-, meta, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _expr_type, context} <- of_expr(expr, stack, context),
         {:ok, _pattern_type, context} <-
           Pattern.of_head([pattern], guards, meta, stack, context),
         do: {:ok, context}
  end

  defp for_clause({:<<>>, _, [{:<-, meta, [left, right]}]}, stack, context) do
    with {:ok, right_type, context} <- of_expr(right, stack, context),
         {:ok, _pattern_type, context} <- Pattern.of_match(left, {binary(), left}, stack, context) do
      if binary_type?(right_type) do
        {:ok, context}
      else
        warning = {:badbinary, right_type, right, context}
        {:ok, warn(__MODULE__, warning, meta, stack, context)}
      end
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
    {:ok, context}
  end

  ## With

  defp with_clause({:<-, meta, [left, expr]}, stack, context) do
    {pattern, guards} = extract_head([left])

    with {:ok, _pattern_type, context} <-
           Pattern.of_head([pattern], guards, meta, stack, context),
         {:ok, _expr_type, context} <- of_expr(expr, stack, context),
         do: {:ok, context}
  end

  defp with_clause(list, stack, context) when is_list(list) do
    reduce_ok(list, context, &with_option(&1, stack, &2))
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

  defp error_type(), do: dynamic()

  defp apply_many([], _function, _args_types, _expr, _stack, context) do
    {:ok, dynamic(), context}
  end

  defp apply_many([mod], function, args_types, expr, stack, context) do
    Of.apply(mod, function, args_types, expr, stack, context)
  end

  defp apply_many(mods, function, args_types, expr, stack, context) do
    with {:ok, returns, context} <-
           map_reduce_ok(mods, context, fn mod, context ->
             Of.apply(mod, function, args_types, expr, stack, context)
           end) do
      {:ok, Enum.reduce(returns, &union/2), context}
    end
  end

  defp of_clauses(clauses, stack, context) do
    reduce_ok(clauses, context, fn {:->, meta, [head, body]}, context ->
      {patterns, guards} = extract_head(head)

      with {:ok, _, context} <- Pattern.of_head(patterns, guards, meta, stack, context),
           {:ok, _, context} <- of_expr(body, stack, context),
           do: {:ok, context}
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
    case of_expr(expr, stack, context) do
      {:ok, _type, context} -> {:ok, context}
      {:error, context} -> {:error, context}
    end
  end

  ## Warning formatting

  def format_diagnostic({:badupdate, type, expr, expected_type, actual_type, context}) do
    traces = Of.collect_traces(expr, context)

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
          Of.format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badbinary, type, expr, context}) do
    traces = Of.collect_traces(expr, context)

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
          Of.format_traces(traces)
        ])
    }
  end
end
