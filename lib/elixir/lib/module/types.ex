defmodule Module.Types do
  @moduledoc false

  defmodule Error do
    defexception [:message]
  end

  import Module.Types.Helpers
  alias Module.Types.{Expr, Pattern, Unify}

  @doc false
  def warnings(module, file, defs, no_warn_undefined, cache) do
    stack = stack()

    Enum.flat_map(defs, fn {{fun, arity} = function, kind, meta, clauses} ->
      context = context(with_file_meta(meta, file), module, function, no_warn_undefined, cache)

      Enum.flat_map(clauses, fn {_meta, args, guards, body} ->
        def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args})]}

        try do
          warnings_from_clause(args, guards, body, def_expr, stack, context)
        rescue
          e ->
            def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args}), [do: body]]}

            error =
              Error.exception("""
              found error while checking types for #{Exception.format_mfa(module, fun, arity)}

              #{Macro.to_string(def_expr)}

              Please report this bug: https://github.com/elixir-lang/elixir/issues

              #{Exception.format_banner(:error, e, __STACKTRACE__)}\
              """)

            reraise error, __STACKTRACE__
        end
      end)
    end)
  end

  defp with_file_meta(meta, file) do
    case Keyword.fetch(meta, :file) do
      {:ok, {meta_file, _}} -> meta_file
      :error -> file
    end
  end

  defp guards_to_expr([], left) do
    left
  end

  defp guards_to_expr([guard | guards], left) do
    guards_to_expr(guards, {:when, [], [left, guard]})
  end

  defp warnings_from_clause(args, guards, body, def_expr, stack, context) do
    head_stack = Unify.push_expr_stack(def_expr, stack)

    with {:ok, _types, context} <- Pattern.of_head(args, guards, head_stack, context),
         {:ok, _type, context} <- Expr.of_expr(body, stack, context) do
      context.warnings
    else
      {:error, {type, error, context}} ->
        [error_to_warning(type, error, context) | context.warnings]
    end
  end

  @doc false
  def context(file, module, function, no_warn_undefined, cache) do
    %{
      # File of module
      file: file,
      # Module of definitions
      module: module,
      # Current function
      function: function,
      # List of calls to not warn on as undefined
      no_warn_undefined: no_warn_undefined,
      # A list of cached modules received from the parallel compiler
      cache: cache,
      # Expression variable to type variable
      vars: %{},
      # Type variable to expression variable
      types_to_vars: %{},
      # Type variable to type
      types: %{},
      # Trace of all variables that have been refined to a type,
      # including the type they were refined to, why, and where
      traces: %{},
      # Counter to give type variables unique names
      counter: 0,
      # Track if a variable was infered from a type guard function such is_tuple/1
      # or a guard function that fails such as elem/2, possible values are:
      # `:guarded` when `is_tuple(x)`
      # `:guarded` when `is_tuple and elem(x, 0)`
      # `:fail` when `elem(x, 0)`
      guard_sources: %{},
      # A list with all warnings from the running the code
      warnings: []
    }
  end

  @doc false
  def stack() do
    %{
      # Stack of variables we have refined during unification,
      # used for creating relevant traces
      unify_stack: [],
      # Last expression we have recursed through during inference,
      # used for tracing
      last_expr: nil,
      # When false do not add a trace when a type variable is refined,
      # useful when merging contexts where the variables already have traces
      trace: true,
      # Track if we are in a context where type guard functions should
      # affect inference
      type_guards_enabled?: true,
      # Context used to determine if unification is bi-directional, :expr
      # is directional, :pattern is bi-directional
      context: nil
    }
  end

  ## VARIABLE LIFTING

  @doc """
  Lifts type variables to their infered types from the context.
  """
  def lift_types(types, context) do
    context = %{
      types: context.types,
      lifted_types: %{},
      lifted_counter: 0
    }

    {types, _context} = Enum.map_reduce(types, context, &do_lift_type/2)
    types
  end

  @doc """
  Lifts a single type to its infered type from the context.
  """
  def lift_type(type, context) do
    context = %{
      types: context.types,
      lifted_types: %{},
      lifted_counter: 0
    }

    {type, _context} = do_lift_type(type, context)
    type
  end

  # Lift type variable to its infered (hopefully concrete) types from the context
  defp do_lift_type({:var, var}, context) do
    case Map.fetch(context.lifted_types, var) do
      {:ok, lifted_var} ->
        {{:var, lifted_var}, context}

      :error ->
        case Map.fetch(context.types, var) do
          {:ok, :unbound} ->
            new_lifted_var(var, context)

          {:ok, type} ->
            # Remove visited types to avoid infinite loops
            # then restore after we are done recursing on vars
            types = context.types
            context = %{context | types: Map.delete(context.types, var)}
            {type, context} = do_lift_type(type, context)
            {type, %{context | types: types}}

          :error ->
            new_lifted_var(var, context)
        end
    end
  end

  defp do_lift_type({:tuple, n, types}, context) do
    {types, context} = Enum.map_reduce(types, context, &do_lift_type/2)
    {{:tuple, n, types}, context}
  end

  defp do_lift_type({:map, pairs}, context) do
    {pairs, context} =
      Enum.map_reduce(pairs, context, fn {kind, key, value}, context ->
        {key, context} = do_lift_type(key, context)
        {value, context} = do_lift_type(value, context)
        {{kind, key, value}, context}
      end)

    {{:map, pairs}, context}
  end

  defp do_lift_type({:list, type}, context) do
    {type, context} = do_lift_type(type, context)
    {{:list, type}, context}
  end

  defp do_lift_type(other, context) do
    {other, context}
  end

  defp new_lifted_var(original_var, context) do
    types = Map.put(context.lifted_types, original_var, context.lifted_counter)
    counter = context.lifted_counter + 1

    type = {:var, context.lifted_counter}
    context = %{context | lifted_types: types, lifted_counter: counter}
    {type, context}
  end

  ## ERROR TO WARNING

  # Collect relevant information from context and traces to report error
  def error_to_warning(:unable_unify, {left, right, stack}, context) do
    {fun, arity} = context.function
    line = get_meta(stack.last_expr)[:line]
    location = {context.file, line, {context.module, fun, arity}}

    traces = type_traces(stack, context)
    traces = tag_traces(traces, context)

    error = {:unable_unify, left, right, {location, stack.last_expr, traces}}
    {Module.Types, error, location}
  end

  # Collect relevant traces from context.traces using stack.unify_stack
  defp type_traces(stack, context) do
    # TODO: Do we need the unify_stack or is enough to only get the last variable
    #       in the stack since we get related variables anyway?
    stack =
      stack.unify_stack
      |> Enum.uniq()
      |> Enum.flat_map(&[&1 | related_variables(&1, context.types)])
      |> Enum.uniq()

    Enum.flat_map(stack, fn var_index ->
      with %{^var_index => traces} <- context.traces,
           %{^var_index => expr_var} <- context.types_to_vars do
        Enum.map(traces, &{expr_var, &1})
      else
        _other -> []
      end
    end)
  end

  defp related_variables(var, types) do
    Enum.flat_map(types, fn
      {related_var, {:var, ^var}} ->
        [related_var | related_variables(related_var, types)]

      _ ->
        []
    end)
  end

  # Tag if trace is for a concrete type or type variable
  defp tag_traces(traces, context) do
    Enum.flat_map(traces, fn {var, {type, expr, location}} ->
      with {:var, var_index} <- type,
           %{^var_index => expr_var} <- context.types_to_vars do
        [{var, {:var, expr_var, expr, location}}]
      else
        _ -> [{var, {:type, type, expr, location}}]
      end
    end)
  end

  ## FORMAT WARNINGS

  def format_warning({:unable_unify, left, right, {location, expr, traces}}) do
    cond do
      map_type?(left) and map_type?(right) and match?({:ok, _, _}, missing_field(left, right)) ->
        {:ok, atom, known_atoms} = missing_field(left, right)

        # Drop the last trace which is the expression map.foo
        traces = Enum.drop(traces, 1)
        {traces, hints} = format_traces(traces, true)

        [
          "undefined field \"#{atom}\" ",
          format_expr(expr, location),
          "expected one of the following fields: ",
          Enum.map_join(Enum.sort(known_atoms), ", ", & &1),
          "\n\n",
          traces,
          format_message_hints(hints),
          "Conflict found at"
        ]

      true ->
        simplify_left? = simplify_type?(left, right)
        simplify_right? = simplify_type?(right, left)

        {traces, hints} = format_traces(traces, simplify_left? or simplify_right?)

        [
          "incompatible types:\n\n    ",
          Unify.format_type(left, simplify_left?),
          " !~ ",
          Unify.format_type(right, simplify_right?),
          "\n\n",
          format_expr(expr, location),
          traces,
          format_message_hints(hints),
          "Conflict found at"
        ]
    end
  end

  defp missing_field(
         {:map, [{:required, {:atom, atom} = type, _}, {:optional, :dynamic, :dynamic}]},
         {:map, fields}
       ) do
    matched_missing_field(fields, type, atom)
  end

  defp missing_field(
         {:map, fields},
         {:map, [{:required, {:atom, atom} = type, _}, {:optional, :dynamic, :dynamic}]}
       ) do
    matched_missing_field(fields, type, atom)
  end

  defp missing_field(_, _), do: :error

  defp matched_missing_field(fields, type, atom) do
    if List.keymember?(fields, type, 1) do
      :error
    else
      known_atoms = for {_, {:atom, atom}, _} <- fields, do: atom
      {:ok, atom, known_atoms}
    end
  end

  defp format_traces([], _simplify?) do
    {[], []}
  end

  defp format_traces(traces, simplify?) do
    traces
    |> Enum.reverse()
    |> Enum.map_reduce([], fn
      {var, {:type, type, expr, location}}, hints ->
        {hint, hints} = format_type_hint(type, expr, hints)

        trace = [
          "where \"",
          Macro.to_string(var),
          "\" was given the type ",
          Unify.format_type(type, simplify?),
          hint,
          " in:\n\n    # ",
          format_location(location),
          "    ",
          indent(expr_to_string(expr)),
          "\n\n"
        ]

        {trace, hints}

      {var1, {:var, var2, expr, location}}, hints ->
        trace = [
          "where \"",
          Macro.to_string(var1),
          "\" was given the same type as \"",
          Macro.to_string(var2),
          "\" in:\n\n    # ",
          format_location(location),
          "    ",
          indent(expr_to_string(expr)),
          "\n\n"
        ]

        {trace, hints}
    end)
  end

  defp format_location({file, line, _mfa}) do
    format_location({file, line})
  end

  defp format_location({file, line}) do
    file = Path.relative_to_cwd(file)
    line = if line, do: [Integer.to_string(line)], else: []
    [file, ?:, line, ?\n]
  end

  defp simplify_type?(type, other) do
    map_type?(type) and not map_type?(other)
  end

  ## EXPRESSION FORMATTING

  defp format_expr(nil, _location) do
    []
  end

  defp format_expr(expr, location) do
    [
      "in expression:\n\n    # ",
      format_location(location),
      "    ",
      indent(expr_to_string(expr)),
      "\n\n"
    ]
  end

  @doc false
  def expr_to_string(expr) do
    expr
    |> reverse_rewrite()
    |> Macro.to_string()
  end

  defp reverse_rewrite(guard) do
    Macro.prewalk(guard, fn
      {{:., _, [mod, fun]}, meta, args} -> erl_to_ex(mod, fun, args, meta)
      other -> other
    end)
  end

  defp erl_to_ex(mod, fun, args, meta) do
    case :elixir_rewrite.erl_to_ex(mod, fun, args) do
      {Kernel, fun, args} -> {fun, meta, args}
      {mod, fun, args} -> {{:., [], [mod, fun]}, meta, args}
    end
  end

  ## Hints

  defp format_message_hints(hints) do
    hints |> Enum.uniq() |> Enum.reverse() |> Enum.map(&format_message_hint/1)
  end

  defp format_message_hint(:inferred_dot) do
    """
    HINT: "var.field" (without parentheses) implies "var" is a map() while \
    "var.fun()" (with parentheses) implies "var" is an atom()

    """
  end

  defp format_message_hint(:inferred_bitstring_spec) do
    """
    HINT: all expressions given to binaries are assumed to be of type \
    integer() unless said otherwise. For example, <<expr>> assumes "expr" \
    is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
    to change the default behaviour.

    """
  end

  defp format_type_hint(type, expr, hints) do
    case format_type_hint(type, expr) do
      {message, hint} -> {message, [hint | hints]}
      :error -> {[], hints}
    end
  end

  defp format_type_hint(type, expr) do
    cond do
      dynamic_map_dot?(type, expr) ->
        {" (due to calling var.field)", :inferred_dot}

      dynamic_remote_call?(type, expr) ->
        {" (due to calling var.fun())", :inferred_dot}

      inferred_bitstring_spec?(type, expr) ->
        {[], :inferred_bitstring_spec}

      true ->
        :error
    end
  end

  defp dynamic_map_dot?(type, expr) do
    with true <- map_type?(type),
         {{:., _meta1, [_map, _field]}, meta2, []} <- expr,
         true <- Keyword.get(meta2, :no_parens, false) do
      true
    else
      _ -> false
    end
  end

  defp dynamic_remote_call?(type, expr) do
    with true <- atom_type?(type),
         {{:., _meta1, [_module, _field]}, meta2, []} <- expr,
         false <- Keyword.get(meta2, :no_parens, false) do
      true
    else
      _ -> false
    end
  end

  defp inferred_bitstring_spec?(type, expr) do
    with true <- integer_type?(type),
         {:<<>>, _, args} <- expr,
         true <- Enum.any?(args, &match?({:"::", [{:inferred_bitstring_spec, true} | _], _}, &1)) do
      true
    else
      _ -> false
    end
  end

  ## Formatting helpers

  defp indent(string) do
    String.replace(string, "\n", "    \n")
  end

  defp map_type?({:map, _}), do: true
  defp map_type?(_other), do: false

  defp atom_type?(:atom), do: true
  defp atom_type?({:atom, _}), do: false
  defp atom_type?({:union, union}), do: Enum.all?(union, &atom_type?/1)
  defp atom_type?(_other), do: false

  defp integer_type?(:integer), do: true
  defp integer_type?(_other), do: false
end
