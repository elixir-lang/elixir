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

    {_trees, types, context} = of_pattern_args(patterns, expected_types, stack, context)
    {_, context} = Enum.map_reduce(guards, context, &of_guard(&1, @guard, &1, stack, &2))
    {types, context}
  end

  defp of_pattern_args([], [], _stack, context) do
    {[], [], context}
  end

  defp of_pattern_args(patterns, expected_types, stack, context) do
    context = %{context | pattern_info: {%{}, %{}}}
    {trees, context} = of_pattern_args_index(patterns, expected_types, 0, [], stack, context)

    {types, context} =
      of_pattern_recur(expected_types, stack, context, fn types, changed, context ->
        of_pattern_args_tree(trees, types, changed, 0, [], stack, context)
      end)

    {trees, types, context}
  end

  defp of_pattern_args_index(
         [pattern | tail],
         [type | expected_types],
         index,
         acc,
         stack,
         context
       ) do
    {tree, context} = of_pattern(pattern, [{:arg, index, type, pattern}], stack, context)
    acc = [{pattern, tree} | acc]
    of_pattern_args_index(tail, expected_types, index + 1, acc, stack, context)
  end

  defp of_pattern_args_index([], [], _index, acc, _stack, context),
    do: {Enum.reverse(acc), context}

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
  the given expected and expr or an error in case of a typing conflict.
  """
  def of_match(pattern, expected, expr, stack, context) do
    context = %{context | pattern_info: {%{}, %{}}}
    {tree, context} = of_pattern(pattern, [{:arg, 0, expected, expr}], stack, context)

    {[type], context} =
      of_pattern_recur([expected], stack, context, fn [type], [0], context ->
        with {:ok, type, context} <- of_pattern_intersect(tree, type, expr, stack, context) do
          {:ok, [type], context}
        end
      end)

    {type, context}
  end

  defp of_pattern_recur(types, stack, context, callback) do
    %{pattern_info: {pattern_vars, pattern_args}} = context
    context = %{context | pattern_info: nil}
    pattern_vars = Map.to_list(pattern_vars)
    changed = :lists.seq(0, length(types) - 1)

    case callback.(types, changed, context) do
      {:ok, types, context} ->
        of_pattern_recur(types, pattern_vars, pattern_args, stack, context, callback)

      {:error, context} ->
        {types, context}
    end
  catch
    {types, context} -> {types, context}
  end

  defp of_pattern_recur(types, vars, args, stack, context, callback) do
    %{vars: context_vars} = context

    {changed, context} =
      Enum.reduce(vars, {[], context}, fn {version, paths}, {changed, context} ->
        current_type = context_vars[version][:type]

        {var_changed?, context} =
          Enum.reduce(paths, {false, context}, fn
            [var, {:arg, index, expected, expr} | path], {var_changed?, context} ->
              actual = Enum.fetch!(types, index)

              case of_pattern_var(path, actual) do
                {:ok, type} ->
                  case Of.refine_var(var, type, expr, stack, context) do
                    {:ok, type, context} ->
                      {var_changed? or current_type == nil or not equal?(current_type, type),
                       context}

                    {:error, _type, context} ->
                      throw({types, context})
                  end

                :error ->
                  context = Of.incompatible_error(expr, expected, actual, stack, context)
                  throw({types, context})
              end
          end)

        case var_changed? do
          false ->
            {changed, context}

          true ->
            case paths do
              # A single change, check if there are other variables in this index.
              [[_var, {:arg, index, _, _} | _]] ->
                case args do
                  %{^index => true} -> {[index | changed], context}
                  %{^index => false} -> {changed, context}
                end

              # Several changes, we have to recompute all indexes.
              _ ->
                var_changed = Enum.map(paths, fn [_var, {:arg, index, _, _} | _] -> index end)
                {var_changed ++ changed, context}
            end
        end
      end)

    case :lists.usort(changed) do
      [] ->
        {types, context}

      changed ->
        case callback.(types, changed, context) do
          # A simple structural comparison for optimization
          {:ok, ^types, context} -> {types, context}
          {:ok, types, context} -> of_pattern_recur(types, vars, args, stack, context, callback)
          {:error, context} -> {types, context}
        end
    end
  end

  defp of_pattern_intersect(tree, expected, expr, stack, context) do
    actual = of_pattern_tree(tree, context)
    type = intersection(actual, expected)

    cond do
      not empty?(type) ->
        {:ok, type, context}

      empty?(actual) ->
        # The pattern itself is invalid
        meta = get_meta(expr) || stack.meta
        {:error, error(__MODULE__, {:invalid_pattern, expr, context}, meta, stack, context)}

      true ->
        {:error, Of.incompatible_error(expr, expected, actual, stack, context)}
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

  defp of_pattern_var([:head | rest], type) do
    case list_hd(type) do
      {_, head} -> of_pattern_var(rest, head)
      _ -> :error
    end
  end

  # TODO: This should intersect with the list itself and its tail.
  defp of_pattern_var([:tail | rest], type) do
    case list_tl(type) do
      {_, tail} -> of_pattern_var(rest, tail)
      _ -> :error
    end
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
  def of_match_var({:^, _, [var]}, expected, expr, stack, context) do
    Of.intersect(Of.var(var, context), expected, expr, stack, context)
  end

  def of_match_var({:_, _, _}, expected, _expr, _stack, context) do
    {expected, context}
  end

  def of_match_var(var, expected, expr, stack, context) when is_var(var) do
    {_ok?, type, context} = Of.refine_var(var, expected, expr, stack, context)
    {type, context}
  end

  def of_match_var(ast, expected, expr, stack, context) do
    of_match(ast, expected, expr, stack, context)
  end

  ## Patterns

  # :atom
  defp of_pattern(atom, _path, _stack, context) when is_atom(atom),
    do: {atom([atom]), context}

  # 12
  defp of_pattern(literal, _path, _stack, context) when is_integer(literal),
    do: {integer(), context}

  # 1.2
  defp of_pattern(literal, _path, _stack, context) when is_float(literal),
    do: {float(), context}

  # "..."
  defp of_pattern(literal, _path, _stack, context) when is_binary(literal),
    do: {binary(), context}

  # []
  defp of_pattern([], _path, _stack, context),
    do: {empty_list(), context}

  # [expr, ...]
  defp of_pattern(list, path, stack, context) when is_list(list) do
    {prefix, suffix} = unpack_list(list, [])
    of_list(prefix, suffix, path, stack, context)
  end

  # {left, right}
  defp of_pattern({left, right}, path, stack, context) do
    of_tuple([left, right], path, stack, context)
  end

  # left = right
  defp of_pattern({:=, _meta, [_, _]} = match, path, stack, context) do
    result =
      match
      |> unpack_match([])
      |> Enum.reduce({[], [], context}, fn pattern, {static, dynamic, context} ->
        {type, context} = of_pattern(pattern, path, stack, context)

        if is_descr(type) do
          {[type | static], dynamic, context}
        else
          {static, [type | dynamic], context}
        end
      end)

    case result do
      {[], dynamic, context} ->
        {{:intersection, dynamic}, context}

      {static, [], context} ->
        {Enum.reduce(static, &intersection/2), context}

      {static, dynamic, context} ->
        {{:intersection, [Enum.reduce(static, &intersection/2) | dynamic]}, context}
    end
  end

  # %Struct{...}
  # TODO: Once we support typed structs, we need to type check them here.
  defp of_pattern({:%, meta, [struct, {:%{}, _, args}]}, path, stack, context)
       when is_atom(struct) do
    {info, context} = Of.struct_info(struct, meta, stack, context)

    {pairs, context} =
      Enum.map_reduce(args, context, fn {key, value}, context ->
        {value_type, context} = of_pattern(value, [{:key, key} | path], stack, context)
        {{key, value_type}, context}
      end)

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
      {closed_map(static), context}
    else
      {{:closed_map, static, dynamic}, context}
    end
  end

  # %var{...}
  defp of_pattern({:%, _, [{name, _, ctx} = var, {:%{}, _, args}]}, path, stack, context)
       when is_atom(name) and is_atom(ctx) and name != :_ do
    {var, context} = of_pattern(var, [{:key, :__struct__} | path], stack, context)
    dynamic = [__struct__: {:intersection, [atom(), var]}]
    of_open_map(args, [], dynamic, path, stack, context)
  end

  # %^var{...} and %_{...}
  defp of_pattern(
         {:%, _meta, [var, {:%{}, _meta2, args}]} = expr,
         path,
         stack,
         context
       ) do
    {refined, context} = of_match_var(var, atom(), expr, stack, context)
    of_open_map(args, [__struct__: refined], [], path, stack, context)
  end

  # %{...}
  defp of_pattern({:%{}, _meta, args}, path, stack, context) do
    of_open_map(args, [], [], path, stack, context)
  end

  # <<...>>>
  defp of_pattern({:<<>>, _meta, args}, _path, stack, context) do
    context = Of.binary(args, :match, stack, context)
    {binary(), context}
  end

  # left ++ right
  defp of_pattern({{:., _meta1, [:erlang, :++]}, _meta2, [left, right]}, path, stack, context) do
    of_list(left, right, path, stack, context)
  end

  # {...}
  defp of_pattern({:{}, _meta, args}, path, stack, context) do
    of_tuple(args, path, stack, context)
  end

  # ^var
  defp of_pattern({:^, _meta, [var]}, _path, _stack, context) do
    {Of.var(var, context), context}
  end

  # _
  defp of_pattern({:_, _meta, _var_context}, _path, _stack, context) do
    {term(), context}
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

    {{:var, version}, %{context | pattern_info: {vars, args}}}
  end

  # TODO: Properly traverse domain keys
  # TODO: Properly handle pin operator in keys
  defp of_open_map(args, static, dynamic, path, stack, context) do
    {static, dynamic, context} =
      Enum.reduce(args, {static, dynamic, context}, fn {key, value}, {static, dynamic, context} ->
        {value_type, context} = of_pattern(value, [{:key, key} | path], stack, context)

        cond do
          # Only atom keys become part of the type because the other keys are divisible
          not is_atom(key) ->
            {static, dynamic, context}

          is_descr(value_type) ->
            {[{key, value_type} | static], dynamic, context}

          true ->
            {static, [{key, value_type} | dynamic], context}
        end
      end)

    case dynamic do
      [] -> {open_map(static), context}
      _ -> {{:open_map, static, dynamic}, context}
    end
  end

  defp of_tuple(args, path, stack, context) do
    {_index, static?, entries, context} =
      Enum.reduce(args, {0, true, [], context}, fn arg, {index, static?, acc, context} ->
        {type, context} = of_pattern(arg, [{:elem, index} | path], stack, context)
        {index + 1, static? and is_descr(type), [type | acc], context}
      end)

    case static? do
      true -> {tuple(Enum.reverse(entries)), context}
      false -> {{:tuple, Enum.reverse(entries)}, context}
    end
  end

  # [] ++ []
  defp of_list([], [], _path, _stack, context) do
    {empty_list(), context}
  end

  # [] ++ suffix
  defp of_list([], suffix, path, stack, context) do
    of_pattern(suffix, path, stack, context)
  end

  # [prefix1, prefix2, prefix3], [prefix1, prefix2 | suffix]
  defp of_list(prefix, suffix, path, stack, context) do
    {suffix, context} = of_pattern(suffix, [:tail | path], stack, context)

    acc =
      Enum.reduce(prefix, {[], [], context}, fn arg, {static, dynamic, context} ->
        {type, context} = of_pattern(arg, [:head | path], stack, context)

        if is_descr(type) do
          {[type | static], dynamic, context}
        else
          {static, [type | dynamic], context}
        end
      end)

    case acc do
      {static, [], context} when is_descr(suffix) ->
        {non_empty_list(Enum.reduce(static, &union/2), suffix), context}

      {[], dynamic, context} ->
        {{:non_empty_list, dynamic, suffix}, context}

      {static, dynamic, context} ->
        {{:non_empty_list, [Enum.reduce(static, &union/2) | dynamic], suffix}, context}
    end
  end

  ## Guards
  # This function is public as it is invoked from Of.binary/4.

  # :atom
  def of_guard(atom, expected, expr, stack, context) when is_atom(atom) do
    if atom_type?(expected, atom) do
      {atom([atom]), context}
    else
      {error_type(), Of.incompatible_error(expr, expected, atom([atom]), stack, context)}
    end
  end

  # 12
  def of_guard(literal, expected, expr, stack, context) when is_integer(literal) do
    if integer_type?(expected) do
      {integer(), context}
    else
      {error_type(), Of.incompatible_error(expr, expected, integer(), stack, context)}
    end
  end

  # 1.2
  def of_guard(literal, expected, expr, stack, context) when is_float(literal) do
    if float_type?(expected) do
      {float(), context}
    else
      {error_type(), Of.incompatible_error(expr, expected, float(), stack, context)}
    end
  end

  # "..."
  def of_guard(literal, expected, expr, stack, context) when is_binary(literal) do
    if binary_type?(expected) do
      {binary(), context}
    else
      {error_type(), Of.incompatible_error(expr, expected, binary(), stack, context)}
    end
  end

  # []
  def of_guard([], expected, expr, stack, context) do
    if empty_list_type?(expected) do
      {empty_list(), context}
    else
      {error_type(), Of.incompatible_error(expr, expected, empty_list(), stack, context)}
    end
  end

  # [expr, ...]
  def of_guard(list, _expected, expr, stack, context) when is_list(list) do
    {prefix, suffix} = unpack_list(list, [])

    {prefix, context} =
      Enum.map_reduce(prefix, context, &of_guard(&1, dynamic(), expr, stack, &2))

    {suffix, context} = of_guard(suffix, dynamic(), expr, stack, context)
    {non_empty_list(Enum.reduce(prefix, &union/2), suffix), context}
  end

  # {left, right}
  def of_guard({left, right}, expected, expr, stack, context) do
    of_guard({:{}, [], [left, right]}, expected, expr, stack, context)
  end

  # %Struct{...}
  def of_guard({:%, _, [module, {:%{}, _, args}]} = struct, _expected, _expr, stack, context)
      when is_atom(module) do
    fun = &of_guard(&1, dynamic(), struct, &2, &3)
    Of.struct(struct, module, args, :skip_defaults, stack, context, fun)
  end

  # %{...}
  def of_guard({:%{}, _meta, args}, _expected, expr, stack, context) do
    Of.closed_map(args, stack, context, &of_guard(&1, dynamic(), expr, &2, &3))
  end

  # <<>>
  def of_guard({:<<>>, _meta, args}, expected, expr, stack, context) do
    if binary_type?(expected) do
      context = Of.binary(args, :guard, stack, context)
      {binary(), context}
    else
      {error_type(), Of.incompatible_error(expr, expected, binary(), stack, context)}
    end
  end

  # ^var
  def of_guard({:^, _meta, [var]}, expected, expr, stack, context) do
    # This is by definition a variable defined outside of this pattern, so we don't track it.
    Of.intersect(Of.var(var, context), expected, expr, stack, context)
  end

  # {...}
  def of_guard({:{}, _meta, args}, _expected, expr, stack, context) do
    {types, context} = Enum.map_reduce(args, context, &of_guard(&1, dynamic(), expr, stack, &2))
    {tuple(types), context}
  end

  # var.field
  def of_guard({{:., _, [callee, key]}, _, []} = map_fetch, _expected, expr, stack, context)
      when not is_atom(callee) do
    {type, context} = of_guard(callee, dynamic(), expr, stack, context)
    Of.map_fetch(map_fetch, type, key, stack, context)
  end

  # Remote
  def of_guard({{:., _, [:erlang, function]}, _, args}, _expected, expr, stack, context)
      when is_atom(function) do
    {args_type, context} =
      Enum.map_reduce(args, context, &of_guard(&1, dynamic(), expr, stack, &2))

    Of.apply(:erlang, function, args_type, expr, stack, context)
  end

  # var
  def of_guard(var, expected, expr, stack, context) when is_var(var) do
    Of.intersect(Of.var(var, context), expected, expr, stack, context)
  end

  ## Helpers

  def format_diagnostic({:invalid_pattern, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          the following pattern will never match:

              #{expr_to_string(expr) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end
end
