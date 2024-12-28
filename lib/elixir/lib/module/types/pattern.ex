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
  def of_head(_patterns, _guards, expected, _tag, _meta, %{mode: :traversal}, context) do
    {expected, context}
  end

  def of_head(patterns, guards, expected, tag, meta, stack, context) do
    stack = %{stack | meta: meta}

    {_trees, types, context} = of_pattern_args(patterns, expected, tag, stack, context)
    {_, context} = Enum.map_reduce(guards, context, &of_guard(&1, @guard, &1, stack, &2))
    {types, context}
  end

  defp of_pattern_args([], [], _tag, _stack, context) do
    {[], [], context}
  end

  defp of_pattern_args(patterns, expected, tag, stack, context) do
    context = init_pattern_info(context)
    {trees, context} = of_pattern_args_index(patterns, 0, [], stack, context)

    {types, context} =
      of_pattern_recur(expected, tag, stack, context, fn types, changed, context ->
        of_pattern_args_tree(trees, types, changed, 0, [], tag, stack, context)
      end)

    {trees, types, context}
  end

  defp of_pattern_args_index([pattern | tail], index, acc, stack, context) do
    {tree, context} = of_pattern(pattern, [{:arg, index, pattern}], stack, context)
    acc = [{pattern, tree} | acc]
    of_pattern_args_index(tail, index + 1, acc, stack, context)
  end

  defp of_pattern_args_index([], _index, acc, _stack, context),
    do: {Enum.reverse(acc), context}

  defp of_pattern_args_tree(
         [{pattern, tree} | tail],
         [type | expected_types],
         [index | changed],
         index,
         acc,
         tag,
         stack,
         context
       ) do
    with {:ok, type, context} <-
           of_pattern_intersect(tree, type, pattern, index, tag, stack, context) do
      acc = [type | acc]
      of_pattern_args_tree(tail, expected_types, changed, index + 1, acc, tag, stack, context)
    end
  end

  defp of_pattern_args_tree(
         [_ | tail],
         [type | expected_types],
         changed,
         index,
         acc,
         tag,
         stack,
         context
       ) do
    acc = [type | acc]
    of_pattern_args_tree(tail, expected_types, changed, index + 1, acc, tag, stack, context)
  end

  defp of_pattern_args_tree([], [], [], _index, acc, _tag, _stack, context) do
    {:ok, Enum.reverse(acc), context}
  end

  @doc """
  A simplified version of `of_head` used by `=` and `<-`.

  This version tracks the whole expression in tracing,
  instead of only the pattern.
  """
  def of_match(pattern, guards \\ [], expected, expr, tag, stack, context)

  def of_match(_pattern, _guards, expected, _expr, _tag, %{mode: :traversal}, context) do
    {expected, context}
  end

  def of_match(pattern, guards, expected, expr, tag, stack, context) do
    context = init_pattern_info(context)
    {tree, context} = of_pattern(pattern, [{:arg, 0, expr}], stack, context)

    {[type], context} =
      of_pattern_recur([expected], tag, stack, context, fn [type], [0], context ->
        with {:ok, type, context} <-
               of_pattern_intersect(tree, type, expr, 0, tag, stack, context) do
          {:ok, [type], context}
        end
      end)

    {_, context} = Enum.map_reduce(guards, context, &of_guard(&1, @guard, &1, stack, &2))
    {type, context}
  end

  defp all_single_path?(vars, info, index) do
    info
    |> Map.get(index, [])
    |> Enum.all?(fn version -> match?([_], Map.fetch!(vars, version)) end)
  end

  defp of_pattern_recur(types, tag, stack, context, callback) do
    %{pattern_info: {vars, info, _counter}} = context
    context = nilify_pattern_info(context)
    changed = :lists.seq(0, length(types) - 1)

    # If all variables in a given index have a single path,
    # then there are no changes to propagate
    unchangeable = for index <- changed, all_single_path?(vars, info, index), do: index

    vars = Map.to_list(vars)

    try do
      case callback.(types, changed, context) do
        {:ok, types, context} ->
          of_pattern_recur(types, unchangeable, vars, info, tag, stack, context, callback)

        {:error, context} ->
          {types, error_vars(vars, context)}
      end
    catch
      {types, context} -> {types, error_vars(vars, context)}
    end
  end

  defp of_pattern_recur(types, unchangeable, vars, info, tag, stack, context, callback) do
    {changed, context} =
      Enum.reduce(vars, {[], context}, fn {version, paths}, {changed, context} ->
        {var_changed?, context} =
          Enum.reduce(paths, {false, context}, fn
            [var, {:arg, index, expr} | path], {var_changed?, context} ->
              actual = Enum.fetch!(types, index)

              case of_pattern_var(path, actual, true, info, context) do
                {type, reachable_var?} ->
                  # If current type is already a subtype, there is nothing to refine.
                  with %{^version => %{type: current_type}} <- context.vars,
                       true <- subtype?(current_type, type) do
                    {var_changed?, context}
                  else
                    _ ->
                      case Of.refine_var(var, type, expr, stack, context) do
                        {:ok, _type, context} -> {var_changed? or reachable_var?, context}
                        {:error, _type, context} -> throw({types, context})
                      end
                  end

                :error ->
                  throw({types, badpattern_error(expr, index, tag, stack, context)})
              end
          end)

        case var_changed? do
          false ->
            {changed, context}

          true ->
            var_changed = Enum.map(paths, fn [_var, {:arg, index, _} | _] -> index end)
            {var_changed ++ changed, context}
        end
      end)

    case :lists.usort(changed) -- unchangeable do
      [] ->
        {types, context}

      changed ->
        case callback.(types, changed, context) do
          # A simple structural comparison for optimization
          {:ok, ^types, context} ->
            {types, context}

          {:ok, types, context} ->
            of_pattern_recur(types, unchangeable, vars, info, tag, stack, context, callback)

          {:error, context} ->
            {types, error_vars(vars, context)}
        end
    end
  end

  defp error_vars(vars, context) do
    Enum.reduce(vars, context, fn {_version, [[var | _path] | _paths]}, context ->
      Of.error_var(var, context)
    end)
  end

  defp badpattern_error(expr, index, tag, stack, context) do
    meta =
      if meta = get_meta(expr) do
        meta ++ Keyword.take(stack.meta, [:generated, :line])
      else
        stack.meta
      end

    error(__MODULE__, {:badpattern, expr, index, tag, context}, meta, stack, context)
  end

  defp of_pattern_intersect(tree, expected, expr, index, tag, stack, context) do
    actual = of_pattern_tree(tree, context)
    type = intersection(actual, expected)

    if empty?(type) do
      {:error, badpattern_error(expr, index, tag, stack, context)}
    else
      {:ok, type, context}
    end
  end

  defp of_pattern_var([], type, reachable_var?, _info, _context) do
    {type, reachable_var?}
  end

  defp of_pattern_var([{:elem, index} | rest], type, reachable_var?, info, context)
       when is_integer(index) do
    case tuple_fetch(type, index) do
      {_optional?, type} -> of_pattern_var(rest, type, reachable_var?, info, context)
      _reason -> :error
    end
  end

  defp of_pattern_var([{:key, field} | rest], type, reachable_var?, info, context)
       when is_atom(field) do
    case map_fetch(type, field) do
      {_optional?, type} -> of_pattern_var(rest, type, reachable_var?, info, context)
      _reason -> :error
    end
  end

  # TODO: Implement domain key types
  defp of_pattern_var([{:key, _key} | rest], _type, _reachable_var?, info, context) do
    of_pattern_var(rest, dynamic(), false, info, context)
  end

  defp of_pattern_var([{:head, counter} | rest], type, _reachable_var?, info, context) do
    case list_hd(type) do
      {_, head} ->
        tree = Map.fetch!(info, -counter)
        type = intersection(of_pattern_tree(tree, context), head)
        of_pattern_var(rest, type, false, info, context)

      _ ->
        :error
    end
  end

  defp of_pattern_var([:tail | rest], type, reachable_var?, info, context) do
    case list_tl(type) do
      {_, tail} -> of_pattern_var(rest, tail, reachable_var?, info, context)
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
    of_match(ast, expected, expr, :default, stack, context)
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
    [{:arg, arg, _pattern} | _] = path = Enum.reverse(reverse_path)
    {vars, info, counter} = context.pattern_info

    paths = [[var | path] | Map.get(vars, version, [])]
    vars = Map.put(vars, version, paths)

    # Stores all variables used at any given argument
    info = Map.update(info, arg, [version], &[version | &1])
    {{:var, version}, %{context | pattern_info: {vars, info, counter}}}
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
    {vars, info, counter} = context.pattern_info
    context = %{context | pattern_info: {vars, info, counter + length(prefix)}}

    {static, dynamic, info, context} =
      Enum.reduce(prefix, {[], [], %{}, context}, fn
        arg, {static, dynamic, info, context}
        when is_number(arg) or is_atom(arg) or is_binary(arg) or arg == [] ->
          {type, context} = of_pattern(arg, [], stack, context)
          {[type | static], dynamic, info, context}

        arg, {static, dynamic, info, context} ->
          counter = map_size(info) + counter
          {type, context} = of_pattern(arg, [{:head, counter} | path], stack, context)
          info = Map.put(info, -counter, type)

          if is_descr(type) do
            {[type | static], dynamic, info, context}
          else
            {static, [type | dynamic], info, context}
          end
      end)

    context =
      if info != %{} do
        update_in(context.pattern_info, fn {acc_vars, acc_info, acc_counter} ->
          {acc_vars, Map.merge(acc_info, info), acc_counter}
        end)
      else
        context
      end

    case {static, dynamic} do
      {static, []} when is_descr(suffix) ->
        {non_empty_list(Enum.reduce(static, &union/2), suffix), context}

      {[], dynamic} ->
        {{:non_empty_list, dynamic, suffix}, context}

      {static, dynamic} ->
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
  def of_guard({:%, meta, [module, {:%{}, _, args}]} = struct, _expected, _expr, stack, context)
      when is_atom(module) do
    fun = &of_guard(&1, dynamic(), struct, &2, &3)
    Of.struct_instance(module, args, meta, stack, context, fun)
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

  # Comparison operators
  def of_guard({{:., _, [:erlang, function]}, _, args}, _expected, expr, stack, context)
      when function in [:==, :"/=", :"=:=", :"=/="] do
    {_args_type, context} =
      Enum.map_reduce(args, context, &of_guard(&1, dynamic(), expr, stack, &2))

    {boolean(), context}
  end

  # Remote
  def of_guard({{:., _, [:erlang, function]}, _, args} = call, _expected, expr, stack, context)
      when is_atom(function) do
    {args_type, context} =
      Enum.map_reduce(args, context, &of_guard(&1, dynamic(), expr, stack, &2))

    Module.Types.Apply.remote(:erlang, function, args, args_type, call, stack, context)
  end

  # var
  def of_guard(var, expected, expr, stack, context) when is_var(var) do
    Of.intersect(Of.var(var, context), expected, expr, stack, context)
  end

  ## Helpers

  # pattern_info stores the variables defined in patterns,
  # additional information about the number of variables in
  # arguments and list heads, and a counter used to compute
  # the number of list heads.
  defp init_pattern_info(context) do
    %{context | pattern_info: {%{}, %{}, 1}}
  end

  defp nilify_pattern_info(context) do
    %{context | pattern_info: nil}
  end

  # $ type tag = head_pattern() or match_pattern()
  #
  # $ typep head_pattern =
  #     :for_reduce or :with_else or :receive or :try_catch or :fn or :default or
  #       {:try_else, type} or {:case, meta, type, expr}
  #
  # $ typep match_pattern =
  #     :with or :for or {:match, type}
  #
  # The match pattern ones have the whole expression instead
  # of a single pattern.
  def format_diagnostic({:badpattern, pattern_or_expr, index, tag, context}) do
    {to_trace, message} = badpattern(tag, pattern_or_expr, index)
    traces = collect_traces(to_trace, context)

    %{
      details: %{typing_traces: traces},
      message: IO.iodata_to_binary([message, format_traces(traces)])
    }
  end

  defp badpattern({:try_else, type}, pattern, _) do
    {pattern,
     """
     the following clause will never match:

         #{expr_to_string(pattern) |> indent(4)}

     it attempts to match on the result of the try do-block which has incompatible type:

         #{to_quoted_string(type) |> indent(4)}
     """}
  end

  defp badpattern({:case, meta, type, expr}, pattern, _) do
    if meta[:type_check] == :expr do
      {expr,
       """
       the following conditional expression will always evaluate to #{to_quoted_string(type)}:

           #{expr_to_string(expr) |> indent(4)}
       """}
    else
      {pattern,
       """
       the following clause will never match:

           #{expr_to_string(pattern) |> indent(4)}

       because it attempts to match on the result of:

           #{expr_to_string(expr) |> indent(4)}

       which has type:

           #{to_quoted_string(type) |> indent(4)}
       """}
    end
  end

  defp badpattern({:match, type}, expr, _) do
    {expr,
     """
     the following pattern will never match:

         #{expr_to_string(expr) |> indent(4)}

     because the right-hand side has type:

         #{to_quoted_string(type) |> indent(4)}
     """}
  end

  defp badpattern({:infer, types}, pattern_or_expr, index) do
    type = Enum.fetch!(types, index)

    if type == dynamic() do
      {pattern_or_expr,
       """
       the #{integer_to_ordinal(index + 1)} pattern in clause will never match:

           #{expr_to_string(pattern_or_expr) |> indent(4)}
       """}
    else
      # This can only happen in protocol implementations
      {pattern_or_expr,
       """
       the #{integer_to_ordinal(index + 1)} pattern in clause will never match:

           #{expr_to_string(pattern_or_expr) |> indent(4)}

       because it is expected to receive type:

           #{to_quoted_string(type) |> indent(4)}
       """}
    end
  end

  defp badpattern(:default, pattern_or_expr, _) do
    {pattern_or_expr,
     """
     the following pattern will never match:

         #{expr_to_string(pattern_or_expr) |> indent(4)}
     """}
  end
end
