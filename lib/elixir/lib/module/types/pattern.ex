defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.Of
  import Module.Types.{Helpers, Descr}

  @guard atom([true, false, :fail])

  @doc """
  Handles patterns and guards at once.

  The algorithm works as follows:

  1. First we traverse the patterns and build a pattern tree
     (which tells how to compute the type of a pattern) alongside
     the variable trees (which tells us how to compute the type
     of a variable).

  2. Then we traverse the pattern tree and compute the intersection
     between the pattern and the expected types (which is currently dynamic).

  3. Then we compute the values for each variable.

  4. Then we refine the variables inside guards. If any variable
     is refined, we restart at step 2.

  """
  # TODO: The expected types for patterns/guards must always given as arguments.
  # TODO: Perform full guard inference
  def of_head(patterns, guards, meta, stack, context) do
    stack = %{stack | meta: meta}
    dynamic = dynamic()
    expected_types = Enum.map(patterns, fn _ -> dynamic end)

    with {:ok, _trees, types, context} <-
           of_pattern_args(patterns, expected_types, stack, context),
         {:ok, _, context} <-
           map_reduce_ok(guards, context, &of_guard(&1, {@guard, &1}, stack, &2)) do
      {:ok, types, context}
    end
  end

  defp of_pattern_args(patterns, expected_types, stack, context) do
    context = %{context | pattern_vars: %{}}

    with {:ok, trees, context} <-
           of_pattern_args_index(patterns, expected_types, 0, [], stack, context),
         {:ok, types, context} <-
           of_pattern_args_tree(trees, expected_types, [], stack, context),
         {:ok, context} <-
           of_pattern_vars(types, stack, context) do
      {:ok, trees, types, context}
    end
  end

  defp of_pattern_args_index(
         [pattern | tail],
         [type | expected_types],
         index,
         acc,
         stack,
         context
       ) do
    with {:ok, tree, context} <-
           of_pattern(pattern, {[{:arg, index, type, pattern}], pattern}, stack, context) do
      acc = [{pattern, tree} | acc]
      of_pattern_args_index(tail, expected_types, index + 1, acc, stack, context)
    end
  end

  defp of_pattern_args_index([], [], _index, acc, _stack, context),
    do: {:ok, Enum.reverse(acc), context}

  defp of_pattern_args_tree(
         [{pattern, tree} | tail],
         [type | expected_types],
         acc,
         stack,
         context
       ) do
    # TODO: In case the pattern itself is empty, we should say the pattern cannot match any type
    with {:ok, type, context} <-
           Of.intersect(of_pattern_tree(tree, context), {type, pattern}, stack, context) do
      of_pattern_args_tree(tail, expected_types, [type | acc], stack, context)
    end
  end

  defp of_pattern_args_tree([], [], acc, _stack, context) do
    {:ok, Enum.reverse(acc), context}
  end

  @doc """
  Return the type and typing context of a pattern expression with
  the given {expected, expr} pair or an error in case of a typing conflict.
  """
  def of_match(pattern, {expected, expr}, stack, context) do
    context = %{context | pattern_vars: %{}}

    with {:ok, tree, context} <-
           of_pattern(pattern, {[{:arg, 0, expected, expr}], pattern}, stack, context),
         # TODO: In case the pattern itself is empty, we should say the pattern cannot match any type
         {:ok, type, context} <-
           Of.intersect(of_pattern_tree(tree, context), {expected, expr}, stack, context),
         {:ok, context} <-
           of_pattern_vars([type], stack, context) do
      {:ok, type, context}
    end
  end

  defp of_pattern_vars(types, stack, %{pattern_vars: pattern_vars} = context) do
    # TODO: we may need to recompute the pattern tree depending on what changes
    pattern_vars
    |> Map.to_list()
    |> reduce_ok(%{context | pattern_vars: nil}, fn {_version, paths}, context ->
      reduce_ok(paths, context, fn [var, {:arg, index, expected, expr} | paths], context ->
        actual = Enum.fetch!(types, index)

        case of_pattern_var(paths, actual) do
          {:ok, type} ->
            with {:ok, _var_type, context} <- Of.refine_var(var, {type, expr}, stack, context) do
              {:ok, context}
            end

          :error ->
            {:error, Of.incompatible_warn(expr, expected, actual, stack, context)}
        end
      end)
    end)
  end

  defp of_pattern_var([], type) do
    {:ok, type}
  end

  defp of_pattern_var([{:key, field} | rest], type) when is_atom(field) do
    case map_fetch(type, field) do
      {_optional?, type} -> of_pattern_var(rest, type)
      _reason -> :error
    end
  end

  defp of_pattern_var([{:key, _key} | rest], _type) do
    of_pattern_var(rest, dynamic())
  end

  defp of_pattern_tree(descr, _context) when is_descr(descr),
    do: descr

  defp of_pattern_tree({:map, static, dynamic}, context) do
    dynamic = Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, context)} end)
    open_map(static ++ dynamic)
  end

  defp of_pattern_tree({:match, entries}, context) do
    entries
    |> Enum.map(&of_pattern_tree(&1, context))
    |> Enum.reduce(&intersection/2)
  end

  defp of_pattern_tree({:var, version}, context) do
    case context do
      %{vars: %{^version => %{type: type}}} -> type
      _ -> term()
    end
  end

  @doc """
  Function used to assign a type to a variable. Used by %struct{}
  and binary patterns.
  """
  def of_match_var({:^, _, [var]}, expected_expr, stack, context) do
    Of.intersect(Of.var(var, context), expected_expr, stack, context)
  end

  def of_match_var({:_, _, _}, {expected, _expr}, _stack, context) do
    {:ok, expected, context}
  end

  def of_match_var(var, expected_expr, stack, context) when is_var(var) do
    Of.refine_var(var, expected_expr, stack, context)
  end

  def of_match_var(ast, expected_expr, stack, context) do
    of_match(ast, expected_expr, stack, context)
  end

  ## Patterns

  # The second argument of patterns is, opposite to guards,
  # either {descr, expr} or a {path, expr}. However, the descr
  # is only used for refining variables, outside of that, it is
  # not asserted on.

  # TODO: Remove the hardcoding of dynamic
  # TODO: Remove this function
  defp of_pattern(expr, stack, context) do
    of_pattern(expr, {dynamic(), expr}, stack, context)
  end

  # :atom
  defp of_pattern(atom, _expected_expr, _stack, context) when is_atom(atom),
    do: {:ok, atom([atom]), context}

  # 12
  defp of_pattern(literal, _expected_expr, _stack, context) when is_integer(literal),
    do: {:ok, integer(), context}

  # 1.2
  defp of_pattern(literal, _expected_expr, _stack, context) when is_float(literal),
    do: {:ok, float(), context}

  # "..."
  defp of_pattern(literal, _expected_expr, _stack, context) when is_binary(literal),
    do: {:ok, binary(), context}

  # []
  defp of_pattern([], _expected_expr, _stack, context),
    do: {:ok, empty_list(), context}

  # [expr, ...]
  defp of_pattern(exprs, _expected_expr, stack, context) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &of_pattern(&1, {dynamic(), &1}, stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # {left, right}
  defp of_pattern({left, right}, expected_expr, stack, context) do
    of_pattern({:{}, [], [left, right]}, expected_expr, stack, context)
  end

  # left = right
  # TODO: Track variables and handle nesting
  defp of_pattern({:=, _meta, [_, _]} = match, {path, expr}, stack, context) do
    match
    |> unpack_match([])
    |> reduce_ok({[], [], context}, fn pattern, {static, dynamic, context} ->
      with {:ok, type, context} <- of_pattern(pattern, {path, expr}, stack, context) do
        if is_descr(type) do
          {:ok, {[type | static], dynamic, context}}
        else
          {:ok, {static, [type | dynamic], context}}
        end
      end
    end)
    |> case do
      {:ok, {[], dynamic, context}} ->
        {:ok, {:match, dynamic}, context}

      {:ok, {static, [], context}} ->
        {:ok, Enum.reduce(static, &intersection/2), context}

      {:ok, {static, dynamic, context}} ->
        {:ok, {:match, [Enum.reduce(static, &intersection/2) | dynamic]}, context}

      {:error, context} ->
        {:error, context}
    end
  end

  # %var{...} and %^var{...}
  defp of_pattern(
         {:%, _meta, [struct_var, {:%{}, _meta2, args}]} = expr,
         expected_expr,
         stack,
         context
       )
       when not is_atom(struct_var) do
    with {:ok, _, context} <- of_match_var(struct_var, {atom(), expr}, stack, context) do
      of_open_map([__struct__: struct_var] ++ args, expected_expr, stack, context)
    end
  end

  # %Struct{...}
  defp of_pattern({:%, _meta, [module, {:%{}, _, args}]} = expr, _expected_expr, stack, context)
       when is_atom(module) do
    Of.struct(expr, module, args, :merge_defaults, stack, context, &of_pattern/3)
  end

  # %{...}
  defp of_pattern({:%{}, _meta, args}, expected_expr, stack, context) do
    of_open_map(args, expected_expr, stack, context)
  end

  # <<...>>>
  defp of_pattern({:<<>>, _meta, args}, _expected_expr, stack, context) do
    case Of.binary(args, :match, stack, context) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, context} -> {:error, context}
    end
  end

  # left | []
  defp of_pattern({:|, _meta, [left_expr, []]}, _expected_expr, stack, context) do
    of_pattern(left_expr, {dynamic(), left_expr}, stack, context)
  end

  # left | right
  defp of_pattern({:|, _meta, [left_expr, right_expr]}, _expected_expr, stack, context) do
    case of_pattern(left_expr, {dynamic(), left_expr}, stack, context) do
      {:ok, _, context} ->
        of_pattern(right_expr, {dynamic(), right_expr}, stack, context)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # left ++ right
  defp of_pattern(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]},
         _expected_expr,
         stack,
         context
       ) do
    # The left side is always a list
    with {:ok, _, context} <- of_pattern(left_expr, {dynamic(), left_expr}, stack, context),
         {:ok, _, context} <- of_pattern(right_expr, {dynamic(), right_expr}, stack, context) do
      # TODO: Both lists can be empty, so this may be an empty list,
      # so we return dynamic for now.
      {:ok, dynamic(), context}
    end
  end

  # {...}
  # TODO: Implement this
  defp of_pattern({:{}, _meta, exprs}, _expected_expr, stack, context) do
    case map_reduce_ok(exprs, context, &of_pattern(&1, {dynamic(), &1}, stack, &2)) do
      {:ok, types, context} -> {:ok, tuple(types), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # ^var
  defp of_pattern({:^, _meta, [var]}, _expected_expr, _stack, context) do
    {:ok, Of.var(var, context), context}
  end

  # _
  defp of_pattern({:_, _meta, _var_context}, {expected, _expr}, _stack, context) do
    # TODO: Remove descr check
    if is_descr(expected) do
      {:ok, expected, context}
    else
      {:ok, term(), context}
    end
  end

  # var
  defp of_pattern({name, meta, ctx} = var, {path, _expr} = path_expr, stack, context)
       when is_atom(name) and is_atom(ctx) do
    # TODO: Remove descr check
    if is_descr(path) do
      Of.refine_var(var, path_expr, stack, context)
    else
      version = Keyword.fetch!(meta, :version)
      path = [var | Enum.reverse(path)]
      paths = [path | Map.get(context.pattern_vars, version, [])]
      {:ok, {:var, version}, put_in(context.pattern_vars[version], paths)}
    end
  end

  # TODO: Properly traverse domain keys
  # TODO: Properly handle pin operator in keys
  defp of_open_map(args, {expected, expr}, stack, context) do
    result =
      reduce_ok(args, {[], [], context}, fn {key, value}, {static, dynamic, context} ->
        expected = prepend_path({:key, key}, expected)

        with {:ok, value_type, context} <- of_pattern(value, {expected, expr}, stack, context) do
          cond do
            # Only atom keys become part of the type because the other keys are divisible
            not is_atom(key) ->
              {:ok, {static, dynamic, context}}

            is_descr(value_type) ->
              {:ok, {[{key, value_type} | static], dynamic, context}}

            true ->
              {:ok, {static, [{key, value_type} | dynamic], context}}
          end
        end
      end)

    case result do
      {:ok, {static, [], context}} -> {:ok, open_map(static), context}
      {:ok, {static, dynamic, context}} -> {:ok, {:map, static, dynamic}, context}
      {:error, context} -> {:error, context}
    end
  end

  defp unpack_match({:=, _, [left, right]}, acc),
    do: unpack_match(left, unpack_match(right, acc))

  defp unpack_match(node, acc),
    do: [node | acc]

  # TODO: Remove me
  @compile {:inline, prepend_path: 2}
  defp prepend_path(_entry, descr) when is_descr(descr), do: dynamic()
  defp prepend_path(entry, acc), do: [entry | acc]

  ## Guards

  # The second argument of guards is, opposite to patterns,
  # only {descr, expr}, and the descr is always asserted on.
  # This function is public as it is invoked from Of.binary/4.

  # :atom
  def of_guard(atom, {expected, expr}, stack, context) when is_atom(atom) do
    if atom_type?(expected, atom) do
      {:ok, atom([atom]), context}
    else
      {:error, Of.incompatible_warn(expr, expected, atom([atom]), stack, context)}
    end
  end

  # 12
  def of_guard(literal, {expected, expr}, stack, context) when is_integer(literal) do
    if integer_type?(expected) do
      {:ok, integer(), context}
    else
      {:error, Of.incompatible_warn(expr, expected, integer(), stack, context)}
    end
  end

  # 1.2
  def of_guard(literal, {expected, expr}, stack, context) when is_float(literal) do
    if float_type?(expected) do
      {:ok, float(), context}
    else
      {:error, Of.incompatible_warn(expr, expected, float(), stack, context)}
    end
  end

  # "..."
  def of_guard(literal, {expected, expr}, stack, context) when is_binary(literal) do
    if binary_type?(expected) do
      {:ok, binary(), context}
    else
      {:error, Of.incompatible_warn(expr, expected, binary(), stack, context)}
    end
  end

  # []
  def of_guard([], {expected, expr}, stack, context) do
    if empty_list_type?(expected) do
      {:ok, empty_list(), context}
    else
      {:error, Of.incompatible_warn(expr, expected, empty_list(), stack, context)}
    end
  end

  # [expr, ...]
  def of_guard(exprs, _expected_expr, stack, context) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &of_guard(&1, {dynamic(), &1}, stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # {left, right}
  def of_guard({left, right}, expected_expr, stack, context) do
    of_guard({:{}, [], [left, right]}, expected_expr, stack, context)
  end

  # %Struct{...}
  def of_guard({:%, _, [module, {:%{}, _, args}]} = expr, _expected_expr, stack, context)
      when is_atom(module) do
    fun = &of_guard(&1, {dynamic(), &1}, &2, &3)
    Of.struct(expr, module, args, :skip_defaults, stack, context, fun)
  end

  # %{...}
  def of_guard({:%{}, _meta, args}, _expected_expr, stack, context) do
    Of.closed_map(args, stack, context, &of_guard(&1, {dynamic(), &1}, &2, &3))
  end

  # <<>>
  def of_guard({:<<>>, _meta, args}, _expected_expr, stack, context) do
    case Of.binary(args, :guard, stack, context) do
      {:ok, context} -> {:ok, binary(), context}
      # It is safe to discard errors from binary inside expressions
      {:error, context} -> {:ok, binary(), context}
    end
  end

  # ^var
  def of_guard({:^, _meta, [var]}, expected_expr, stack, context) do
    # This is by definition a variable defined outside of this pattern, so we don't track it.
    Of.intersect(Of.var(var, context), expected_expr, stack, context)
  end

  # left | []
  def of_guard({:|, _meta, [left_expr, []]}, _expected_expr, stack, context) do
    of_guard(left_expr, {dynamic(), left_expr}, stack, context)
  end

  # left | right
  def of_guard({:|, _meta, [left_expr, right_expr]}, _expected_expr, stack, context) do
    case of_guard(left_expr, {dynamic(), left_expr}, stack, context) do
      {:ok, _, context} ->
        of_guard(right_expr, {dynamic(), right_expr}, stack, context)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # {...}
  # TODO: Implement this
  def of_guard({:{}, _meta, exprs}, _expected_expr, stack, context) do
    case map_reduce_ok(exprs, context, &of_guard(&1, {dynamic(), &1}, stack, &2)) do
      {:ok, types, context} -> {:ok, tuple(types), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # var.field
  def of_guard({{:., _, [callee, key]}, _, []} = expr, _expected_expr, stack, context)
      when not is_atom(callee) do
    with {:ok, type, context} <- of_guard(callee, {dynamic(), expr}, stack, context) do
      Of.map_fetch(expr, type, key, stack, context)
    end
  end

  # Remote
  def of_guard({{:., _, [:erlang, function]}, _, args} = expr, _expected_expr, stack, context)
      when is_atom(function) do
    with {:ok, args_type, context} <-
           map_reduce_ok(args, context, &of_guard(&1, {dynamic(), expr}, stack, &2)) do
      Of.apply(:erlang, function, args_type, expr, stack, context)
    end
  end

  # var
  def of_guard(var, expected_expr, stack, context) when is_var(var) do
    # TODO: This should be ver refinement once we have inference in guards
    # Of.refine_var(var, expected_expr, stack, context)
    Of.intersect(Of.var(var, context), expected_expr, stack, context)
  end
end
