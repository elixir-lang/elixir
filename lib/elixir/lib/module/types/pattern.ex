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

  defp of_pattern_args([], [], _stack, context) do
    {:ok, [], context}
  end

  defp of_pattern_args(patterns, expected_types, stack, context) do
    context = %{context | pattern_info: {%{}, %{}}}
    changed = :lists.seq(0, length(patterns) - 1)

    with {:ok, trees, context} <-
           of_pattern_args_index(patterns, expected_types, 0, [], stack, context),
         {:ok, types, context} <-
           of_pattern_recur(expected_types, changed, stack, context, fn types, changed, context ->
             of_pattern_args_tree(trees, types, changed, 0, [], stack, context)
           end) do
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
           of_pattern(pattern, [{:arg, index, type, pattern}], stack, context) do
      acc = [{pattern, tree} | acc]
      of_pattern_args_index(tail, expected_types, index + 1, acc, stack, context)
    end
  end

  defp of_pattern_args_index([], [], _index, acc, _stack, context),
    do: {:ok, Enum.reverse(acc), context}

  defp of_pattern_args_tree(
         [{pattern, tree} | tail],
         [type | expected_types],
         [index | changed],
         index,
         acc,
         stack,
         context
       ) do
    with {:ok, type, context} <- of_pattern_intersect(tree, type, pattern, stack, context) do
      of_pattern_args_tree(tail, expected_types, changed, index + 1, [type | acc], stack, context)
    end
  end

  defp of_pattern_args_tree(
         [_ | tail],
         [type | expected_types],
         changed,
         index,
         acc,
         stack,
         context
       ) do
    of_pattern_args_tree(tail, expected_types, changed, index + 1, [type | acc], stack, context)
  end

  defp of_pattern_args_tree([], [], [], _index, acc, _stack, context) do
    {:ok, Enum.reverse(acc), context}
  end

  @doc """
  Return the type and typing context of a pattern expression with
  the given {expected, expr} pair or an error in case of a typing conflict.
  """
  def of_match(pattern, {expected, expr}, stack, context) do
    context = %{context | pattern_info: {%{}, %{}}}

    with {:ok, tree, context} <-
           of_pattern(pattern, [{:arg, 0, expected, expr}], stack, context),
         {:ok, [type], context} <-
           of_pattern_recur([expected], [0], stack, context, fn [type], [0], context ->
             with {:ok, type, context} <- of_pattern_intersect(tree, type, expr, stack, context) do
               {:ok, [type], context}
             end
           end) do
      {:ok, type, context}
    end
  end

  defp of_pattern_recur(types, changed, stack, context, callback) do
    %{pattern_info: {pattern_vars, pattern_args}} = context
    context = %{context | pattern_info: nil}
    pattern_vars = Map.to_list(pattern_vars)
    of_pattern_recur(types, changed, pattern_vars, pattern_args, stack, context, callback)
  end

  defp of_pattern_recur(types, [], _vars, _args, _stack, context, _callback) do
    {:ok, types, context}
  end

  defp of_pattern_recur(types, changed, vars, args, stack, context, callback) do
    with {:ok, types, %{vars: context_vars} = context} <- callback.(types, changed, context) do
      vars
      |> reduce_ok({[], context}, fn {version, paths}, {changed, context} ->
        current_type = context_vars[version][:type]

        paths
        |> reduce_ok({false, context}, fn
          [var, {:arg, index, expected, expr} | path], {var_changed?, context} ->
            actual = Enum.fetch!(types, index)

            case of_pattern_var(path, actual) do
              {:ok, type} ->
                with {:ok, type, context} <- Of.refine_var(var, {type, expr}, stack, context) do
                  {:ok, {var_changed? or current_type != type, context}}
                end

              :error ->
                {:error, Of.incompatible_warn(expr, expected, actual, stack, context)}
            end
        end)
        |> case do
          {:ok, {false, context}} ->
            {:ok, {changed, context}}

          {:ok, {true, context}} ->
            case paths do
              # A single change, check if there are other variables in this index.
              [[_var, {:arg, index, _, _} | _]] ->
                case args do
                  %{^index => true} -> {:ok, {[index | changed], context}}
                  %{^index => false} -> {:ok, {changed, context}}
                end

              # Several changes, we have to recompute all indexes.
              _ ->
                var_changed = Enum.map(paths, fn [_var, {:arg, index, _, _} | _] -> index end)
                {:ok, {var_changed ++ changed, context}}
            end

          {:error, context} ->
            {:error, context}
        end
      end)
      |> case do
        {:ok, {changed, context}} ->
          of_pattern_recur(types, :lists.usort(changed), vars, args, stack, context, callback)

        {:error, context} ->
          {:error, context}
      end
    end
  end

  defp of_pattern_intersect(tree, expected, expr, stack, context) do
    actual = of_pattern_tree(tree, context)

    case Of.intersect(actual, {expected, expr}, stack, context) do
      {:ok, type, context} ->
        {:ok, type, context}

      {:error, intersection_context} ->
        if empty?(actual) do
          meta = get_meta(expr) || stack.meta
          {:error, warn(__MODULE__, {:invalid_pattern, expr, context}, meta, stack, context)}
        else
          {:error, intersection_context}
        end
    end
  end

  defp of_pattern_var([], type) do
    {:ok, type}
  end

  defp of_pattern_var([{:elem, index} | rest], type) when is_integer(index) do
    case tuple_fetch(type, index) do
      {_optional?, type} -> of_pattern_var(rest, type)
      _reason -> :error
    end
  end

  defp of_pattern_var([{:key, field} | rest], type) when is_atom(field) do
    case map_fetch(type, field) do
      {_optional?, type} -> of_pattern_var(rest, type)
      _reason -> :error
    end
  end

  # TODO: Implement domain key types
  defp of_pattern_var([{:key, _key} | rest], _type) do
    of_pattern_var(rest, dynamic())
  end

  # TODO: This should intersect with the head of the list.
  defp of_pattern_var([:head | rest], _type) do
    of_pattern_var(rest, dynamic())
  end

  # TODO: This should intersect with the list itself and its tail.
  defp of_pattern_var([:tail | rest], _type) do
    of_pattern_var(rest, dynamic())
  end

  defp of_pattern_tree(descr, _context) when is_descr(descr),
    do: descr

  defp of_pattern_tree({:tuple, entries}, context) do
    tuple(Enum.map(entries, &of_pattern_tree(&1, context)))
  end

  defp of_pattern_tree({:open_map, static, dynamic}, context) do
    dynamic = Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, context)} end)
    open_map(static ++ dynamic)
  end

  defp of_pattern_tree({:closed_map, static, dynamic}, context) do
    dynamic = Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, context)} end)
    closed_map(static ++ dynamic)
  end

  defp of_pattern_tree({:non_empty_list, [head | tail], suffix}, context) do
    tail
    |> Enum.reduce(of_pattern_tree(head, context), &union(of_pattern_tree(&1, context), &2))
    |> non_empty_list(of_pattern_tree(suffix, context))
  end

  defp of_pattern_tree({:intersection, entries}, context) do
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

  # TODO: Simplify signature of Of.refine_var
  # TODO: Simplify signature of Of.intersect
  # TODO: Of.struct_keys
  # TODO: Test recursive vars

  # :atom
  defp of_pattern(atom, _path, _stack, context) when is_atom(atom),
    do: {:ok, atom([atom]), context}

  # 12
  defp of_pattern(literal, _path, _stack, context) when is_integer(literal),
    do: {:ok, integer(), context}

  # 1.2
  defp of_pattern(literal, _path, _stack, context) when is_float(literal),
    do: {:ok, float(), context}

  # "..."
  defp of_pattern(literal, _path, _stack, context) when is_binary(literal),
    do: {:ok, binary(), context}

  # []
  defp of_pattern([], _path, _stack, context),
    do: {:ok, empty_list(), context}

  # [expr, ...]
  defp of_pattern(exprs, path, stack, context) when is_list(exprs) do
    {prefix, suffix} = unpack_list(exprs, [])
    of_list(prefix, suffix, path, stack, context)
  end

  # {left, right}
  defp of_pattern({left, right}, path, stack, context) do
    of_tuple([left, right], path, stack, context)
  end

  # left = right
  defp of_pattern({:=, _meta, [_, _]} = match, path, stack, context) do
    match
    |> unpack_match([])
    |> reduce_ok({[], [], context}, fn pattern, {static, dynamic, context} ->
      with {:ok, type, context} <- of_pattern(pattern, path, stack, context) do
        if is_descr(type) do
          {:ok, {[type | static], dynamic, context}}
        else
          {:ok, {static, [type | dynamic], context}}
        end
      end
    end)
    |> case do
      {:ok, {[], dynamic, context}} ->
        {:ok, {:intersection, dynamic}, context}

      {:ok, {static, [], context}} ->
        {:ok, Enum.reduce(static, &intersection/2), context}

      {:ok, {static, dynamic, context}} ->
        {:ok, {:intersection, [Enum.reduce(static, &intersection/2) | dynamic]}, context}

      {:error, context} ->
        {:error, context}
    end
  end

  # %Struct{...}
  # TODO: Once we support typed structs, we need to type check them here.
  defp of_pattern({:%, meta, [struct, {:%{}, _, args}]}, path, stack, context)
       when is_atom(struct) do
    {info, context} = Of.struct_info(struct, meta, stack, context)

    result =
      map_reduce_ok(args, context, fn {key, value}, context ->
        with {:ok, value_type, context} <- of_pattern(value, [{:key, key} | path], stack, context) do
          {:ok, {key, value_type}, context}
        end
      end)

    with {:ok, pairs, context} <- result do
      pairs = Map.new(pairs)
      term = term()
      static = [__struct__: atom([struct])]
      dynamic = []

      {static, dynamic} =
        Enum.reduce(info, {static, dynamic}, fn %{field: field}, {static, dynamic} ->
          case pairs do
            %{^field => value_type} when is_descr(value_type) ->
              {[{field, value_type} | static], dynamic}

            %{^field => value_type} ->
              {static, [{field, value_type} | dynamic]}

            _ ->
              {[{field, term} | static], dynamic}
          end
        end)

      if dynamic == [] do
        {:ok, closed_map(static), context}
      else
        {:ok, {:closed_map, static, dynamic}, context}
      end
    end
  end

  # %var{...}
  defp of_pattern({:%, _, [{name, _, ctx} = var, {:%{}, _, args}]}, path, stack, context)
       when is_atom(name) and is_atom(ctx) and name != :_ do
    with {:ok, var, context} <- of_pattern(var, [{:key, :__struct__} | path], stack, context) do
      dynamic = [__struct__: {:intersection, [atom(), var]}]
      of_open_map(args, [], dynamic, path, stack, context)
    end
  end

  # %^var{...} and %_{...}
  defp of_pattern(
         {:%, _meta, [var, {:%{}, _meta2, args}]} = expr,
         path,
         stack,
         context
       ) do
    with {:ok, refined, context} <- of_match_var(var, {atom(), expr}, stack, context) do
      of_open_map(args, [__struct__: refined], [], path, stack, context)
    end
  end

  # %{...}
  defp of_pattern({:%{}, _meta, args}, path, stack, context) do
    of_open_map(args, [], [], path, stack, context)
  end

  # <<...>>>
  defp of_pattern({:<<>>, _meta, args}, _path, stack, context) do
    case Of.binary(args, :match, stack, context) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, context} -> {:error, context}
    end
  end

  # left ++ right
  defp of_pattern({{:., _meta1, [:erlang, :++]}, _meta2, [left, right]}, path, stack, context) do
    of_list(left, right, path, stack, context)
  end

  # {...}
  defp of_pattern({:{}, _meta, exprs}, path, stack, context) do
    of_tuple(exprs, path, stack, context)
  end

  # ^var
  defp of_pattern({:^, _meta, [var]}, _path, _stack, context) do
    {:ok, Of.var(var, context), context}
  end

  # _
  defp of_pattern({:_, _meta, _var_context}, _path, _stack, context) do
    {:ok, term(), context}
  end

  # var
  defp of_pattern({name, meta, ctx} = var, reverse_path, _stack, context)
       when is_atom(name) and is_atom(ctx) do
    version = Keyword.fetch!(meta, :version)
    [{:arg, arg, _type, _pattern} | _] = path = Enum.reverse(reverse_path)
    {vars, args} = context.pattern_info

    paths = [[var | path] | Map.get(vars, version, [])]
    vars = Map.put(vars, version, paths)

    # Our goal here is to compute if an argument has more than one variable.
    args =
      case args do
        %{^arg => false} -> %{args | arg => true}
        %{^arg => true} -> args
        %{} -> Map.put(args, arg, false)
      end

    {:ok, {:var, version}, %{context | pattern_info: {vars, args}}}
  end

  # TODO: Properly traverse domain keys
  # TODO: Properly handle pin operator in keys
  defp of_open_map(args, static, dynamic, path, stack, context) do
    result =
      reduce_ok(args, {static, dynamic, context}, fn {key, value}, {static, dynamic, context} ->
        with {:ok, value_type, context} <- of_pattern(value, [{:key, key} | path], stack, context) do
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
      {:ok, {static, dynamic, context}} -> {:ok, {:open_map, static, dynamic}, context}
      {:error, context} -> {:error, context}
    end
  end

  defp of_tuple(args, path, stack, context) do
    result =
      reduce_ok(args, {0, true, [], context}, fn arg, {index, static?, acc, context} ->
        with {:ok, type, context} <- of_pattern(arg, [{:elem, index} | path], stack, context) do
          {:ok, {index + 1, static? and is_descr(type), [type | acc], context}}
        end
      end)

    case result do
      {:ok, {_index, true, entries, context}} -> {:ok, tuple(Enum.reverse(entries)), context}
      {:ok, {_index, false, entries, context}} -> {:ok, {:tuple, Enum.reverse(entries)}, context}
      {:error, context} -> {:error, context}
    end
  end

  # [] ++ []
  defp of_list([], [], _path, _stack, context) do
    {:ok, empty_list(), context}
  end

  # [] ++ suffix
  defp of_list([], suffix, path, stack, context) do
    of_pattern(suffix, path, stack, context)
  end

  # [prefix1, prefix2, prefix3], [prefix1, prefix2 | suffix]
  defp of_list(prefix, suffix, path, stack, context) do
    with {:ok, suffix, context} <- of_pattern(suffix, [:tail | path], stack, context) do
      result =
        reduce_ok(prefix, {[], [], context}, fn arg, {static, dynamic, context} ->
          with {:ok, type, context} <- of_pattern(arg, [:head | path], stack, context) do
            if is_descr(type) do
              {:ok, {[type | static], dynamic, context}}
            else
              {:ok, {static, [type | dynamic], context}}
            end
          end
        end)

      case result do
        {:ok, {static, [], context}} when is_descr(suffix) ->
          {:ok, non_empty_list(Enum.reduce(static, &union/2), suffix), context}

        {:ok, {[], dynamic, context}} ->
          {:ok, {:non_empty_list, dynamic, suffix}, context}

        {:ok, {static, dynamic, context}} ->
          {:ok, {:non_empty_list, [Enum.reduce(static, &union/2) | dynamic], suffix}, context}

        {:error, context} ->
          {:error, context}
      end
    end
  end

  defp unpack_list([{:|, _, [head, tail]}], acc), do: {Enum.reverse([head | acc]), tail}
  defp unpack_list([head], acc), do: {Enum.reverse([head | acc]), []}
  defp unpack_list([head | tail], acc), do: unpack_list(tail, [head | acc])

  defp unpack_match({:=, _, [left, right]}, acc),
    do: unpack_match(left, unpack_match(right, acc))

  defp unpack_match(node, acc),
    do: [node | acc]

  ## Guards
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
      {:ok, types, context} -> {:ok, non_empty_list(Enum.reduce(types, &union/2)), context}
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
    Of.intersect(Of.var(var, context), expected_expr, stack, context)
  end

  ## Diagnostics

  def format_diagnostic({:invalid_pattern, expr, context}) do
    traces = Of.collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          the following pattern will never match:

              #{expr_to_string(expr) |> indent(4)}
          """,
          Of.format_traces(traces)
        ])
    }
  end
end
