# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.{Apply, Of}
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
  def of_head(patterns, _guards, _expected, _tag, _meta, %{mode: :traversal}, context) do
    term = term()
    {Enum.map(patterns, &{&1, term}), context}
  end

  def of_head(patterns, guards, expected, tag, meta, stack, context) do
    stack = %{stack | meta: meta}

    {trees, context} = of_pattern_args(patterns, expected, tag, stack, context)
    {_, context} = Enum.map_reduce(guards, context, &of_guard(&1, @guard, &1, stack, &2))
    {trees, context}
  end

  @doc """
  Computes the domain from the pattern tree and expected types.
  """
  def of_domain([{_pattern, tree} | trees], [type | expected], context) do
    [intersection(of_pattern_tree(tree, context), type) | of_domain(trees, expected, context)]
  end

  def of_domain([], [], _context) do
    []
  end

  defp of_pattern_args([], [], _tag, _stack, context) do
    {[], context}
  end

  defp of_pattern_args(patterns, expected, tag, stack, context) do
    context = init_pattern_info(context)
    {trees, context} = of_pattern_args_index(patterns, 0, [], stack, context)
    {pattern_info, context} = pop_pattern_info(context)

    {_, context} =
      case of_pattern_args_tree(trees, expected, 0, [], tag, stack, context) do
        {:ok, types, context} ->
          of_pattern_recur(types, tag, pattern_info, stack, context)

        {:error, context} ->
          {expected, error_vars(pattern_info, context)}
      end

    {trees, context}
  end

  defp of_pattern_args_index([pattern | tail], index, acc, stack, context) do
    {tree, context} = of_pattern(pattern, [%{root: {:arg, index}, expr: pattern}], stack, context)
    acc = [{pattern, tree} | acc]
    of_pattern_args_index(tail, index + 1, acc, stack, context)
  end

  defp of_pattern_args_index([], _index, acc, _stack, context),
    do: {Enum.reverse(acc), context}

  defp of_pattern_args_tree(
         [{pattern, tree} | tail],
         [type | expected_types],
         index,
         acc,
         tag,
         stack,
         context
       ) do
    with {:ok, type, context} <-
           of_pattern_intersect(tree, type, pattern, index, tag, stack, context) do
      acc = [type | acc]
      of_pattern_args_tree(tail, expected_types, index + 1, acc, tag, stack, context)
    end
  end

  defp of_pattern_args_tree([], [], _index, acc, _tag, _stack, context) do
    {:ok, Enum.reverse(acc), context}
  end

  @doc """
  Handles the match operator.
  """
  def of_match(pattern, expected_fun, expr, stack, context)

  def of_match(_pattern, expected_fun, _expr, %{mode: :traversal}, context) do
    expected_fun.(dynamic(), context)
  end

  def of_match(pattern, expected_fun, expr, stack, context) do
    context = init_pattern_info(context)
    {tree, context} = of_pattern(pattern, [%{root: {:arg, 0}, expr: expr}], stack, context)
    {pattern_info, context} = pop_pattern_info(context)
    {expected, context} = expected_fun.(of_pattern_tree(tree, context), context)
    tag = {:match, expected}

    {[type], context} =
      of_single_pattern_recur(expected, tag, tree, pattern_info, expr, stack, context)

    {type, context}
  end

  @doc """
  Handles matches in generators.
  """
  def of_generator(pattern, guards, expected, tag, expr, stack, context)

  def of_generator(_pattern, _guards, _expected, _tag, _expr, %{mode: :traversal}, context) do
    context
  end

  def of_generator(pattern, guards, expected, tag, expr, stack, context) do
    context = init_pattern_info(context)
    {tree, context} = of_pattern(pattern, [%{root: {:arg, 0}, expr: expr}], stack, context)
    {pattern_info, context} = pop_pattern_info(context)

    {_, context} =
      of_single_pattern_recur(expected, tag, tree, pattern_info, expr, stack, context)

    {_, context} = Enum.map_reduce(guards, context, &of_guard(&1, @guard, &1, stack, &2))
    context
  end

  defp of_single_pattern_recur(expected, tag, tree, pattern_info, expr, stack, context) do
    case of_pattern_intersect(tree, expected, expr, 0, tag, stack, context) do
      {:ok, type, context} ->
        of_pattern_recur([type], tag, pattern_info, stack, context)

      {:error, context} ->
        {[expected], error_vars(pattern_info, context)}
    end
  end

  defp of_pattern_recur(types, tag, pattern_info, stack, context) do
    {args_paths, vars_paths, vars_deps} = pattern_info

    try do
      {changed, context} =
        Enum.map_reduce(args_paths, context, fn {version, paths}, context ->
          context =
            Enum.reduce(paths, context, fn
              %{var: var, expr: expr, root: {:arg, index}, path: path}, context ->
                actual = Enum.fetch!(types, index)

                case of_pattern_var(path, actual, context) do
                  {:ok, type} ->
                    case Of.refine_head_var(var, type, expr, stack, context) do
                      {:ok, _type, context} ->
                        context

                      {:error, old_type, context} ->
                        throw({types, refine_error(var, old_type, type, stack, context)})
                    end

                  :error ->
                    throw({types, badpattern_error(expr, index, tag, stack, context)})
                end
            end)

          {version, context}
        end)

      try do
        {types, of_pattern_var_deps(changed, vars_paths, vars_deps, tag, stack, context)}
      catch
        {:error, context} -> {types, context}
      end
    catch
      {types, context} -> {types, error_vars(pattern_info, context)}
    end
  end

  defp of_pattern_var_deps([], _vars_paths, _vars_deps, _tag, _stack, context) do
    context
  end

  defp of_pattern_var_deps(previous_changed, vars_paths, vars_deps, tag, stack, context) do
    {changed, context} =
      Enum.reduce(previous_changed, {[], context}, fn version, {changed, context} ->
        paths = Map.get(vars_paths, version, [])

        {var_changed?, context} =
          Enum.reduce(paths, {false, context}, fn
            %{var: var, expr: expr, root: root, path: path}, {var_changed?, context} ->
              index = 0
              actual = of_pattern_tree(root, context)

              case of_pattern_var(path, actual, context) do
                {:ok, type} ->
                  # Optimization: if current type is already a subtype, there is nothing to refine.
                  with %{^version => %{type: current_type}} <- context.vars,
                       true <- subtype?(current_type, type) do
                    {var_changed?, context}
                  else
                    _ ->
                      case Of.refine_head_var(var, type, expr, stack, context) do
                        {:ok, _type, context} ->
                          {true, context}

                        {:error, old_type, context} ->
                          throw({:error, refine_error(var, old_type, type, stack, context)})
                      end
                  end

                :error ->
                  throw({:error, badpattern_error(expr, index, tag, stack, context)})
              end
          end)

        case var_changed? do
          false -> {changed, context}
          true -> {[version | changed], context}
        end
      end)

    changed
    |> Enum.reduce(%{}, fn version, acc ->
      case vars_deps do
        %{^version => deps} -> Map.merge(acc, deps)
        %{} -> acc
      end
    end)
    |> Map.keys()
    |> of_pattern_var_deps(vars_paths, vars_deps, tag, stack, context)
  end

  defp error_vars({args_paths, vars_paths, _vars_deps}, context) do
    callback = fn {_, [%{var: var} | _paths]}, context ->
      Of.error_var(var, context)
    end

    context = Enum.reduce(args_paths, context, callback)
    context = Enum.reduce(vars_paths, context, callback)
    context
  end

  defp refine_error({_, meta, _} = var, old_type, type, stack, context) do
    error(__MODULE__, {:badvar, old_type, type, var, context}, meta, stack, context)
  end

  defp badpattern_error(expr, index, tag, stack, context) do
    meta =
      if meta = get_meta(expr) do
        meta ++ Keyword.take(stack.meta, [:generated, :line, :type_check])
      else
        stack.meta
      end

    error(__MODULE__, {:badpattern, meta, expr, index, tag, context}, meta, stack, context)
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

  defp of_pattern_var([], type, _context) do
    {:ok, type}
  end

  defp of_pattern_var([{:elem, index} | rest], type, context)
       when is_integer(index) do
    case tuple_fetch(type, index) do
      {_optional?, type} -> of_pattern_var(rest, type, context)
      _reason -> :error
    end
  end

  defp of_pattern_var([{:key, field} | rest], type, context)
       when is_atom(field) do
    case map_fetch_key(type, field) do
      {_optional?, type} -> of_pattern_var(rest, type, context)
      _reason -> :error
    end
  end

  # TODO: Implement domain key types
  defp of_pattern_var([{:key, _key} | rest], _type, context) do
    of_pattern_var(rest, dynamic(), context)
  end

  defp of_pattern_var([:head | rest], type, context) do
    case list_hd(type) do
      {:ok, head} -> of_pattern_var(rest, head, context)
      _ -> :error
    end
  end

  defp of_pattern_var([:tail | rest], type, context) do
    case list_tl(type) do
      {:ok, tail} -> of_pattern_var(rest, tail, context)
      :badnonemptylist -> :error
    end
  end

  @doc """
  Receives the pattern tree and the context and returns a concrete type.
  """
  def of_pattern_tree(descr, _context) when is_descr(descr),
    do: descr

  def of_pattern_tree({:tuple, entries}, context) do
    tuple(Enum.map(entries, &of_pattern_tree(&1, context)))
  end

  def of_pattern_tree({:open_map, static, dynamic}, context) do
    dynamic = Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, context)} end)
    open_map(static ++ dynamic)
  end

  def of_pattern_tree({:closed_map, static, dynamic}, context) do
    dynamic = Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, context)} end)
    closed_map(static ++ dynamic)
  end

  def of_pattern_tree({:non_empty_list, [head | tail], suffix}, context) do
    tail
    |> Enum.reduce(of_pattern_tree(head, context), &union(of_pattern_tree(&1, context), &2))
    |> non_empty_list(of_pattern_tree(suffix, context))
  end

  def of_pattern_tree({:intersection, entries}, context) do
    entries
    |> Enum.map(&of_pattern_tree(&1, context))
    |> Enum.reduce(&intersection/2)
  end

  def of_pattern_tree({:var, version}, context) do
    case context do
      %{vars: %{^version => %{type: type}}} -> type
      _ -> term()
    end
  end

  @doc """
  Function used to assign a type to a variable. Used by %struct{}
  and binary patterns.

  Given those values are actually checked at compile-time,
  except for the variables, that's the only scenario we need to handle.
  """
  def of_match_var({:_, _, _}, expected, _expr, _stack, context) do
    {expected, context}
  end

  def of_match_var(var, expected, expr, stack, context) when is_var(var) do
    case Of.refine_head_var(var, expected, expr, stack, context) do
      {:ok, type, context} ->
        {type, context}

      {:error, old_type, context} ->
        {error_type(), refine_error(var, old_type, expected, stack, context)}
    end
  end

  def of_match_var({:<<>>, _meta, args}, _expected, _expr, stack, context) do
    {binary(), Of.binary(args, :match, stack, context)}
  end

  def of_match_var(ast, expected, expr, stack, context) do
    of_guard(ast, expected, expr, stack, context)
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
    {match, version, var} =
      match
      |> unpack_match([])
      |> Enum.split_while(&(not is_var(&1)))
      |> case do
        {match, []} ->
          version = make_ref()
          {match, version, {:match, [version: version], __MODULE__}}

        {pre, [{_, meta, _} = var | post]} ->
          version = Keyword.fetch!(meta, :version)
          {pre ++ post, version, var}
      end

    # Pass the current path to build the current var
    {expr, context} = of_var(var, version, path, context)
    root = %{root: {:var, version}, expr: expr}

    {static, dynamic, context} =
      Enum.reduce(match, {[], [], context}, fn pattern, {static, dynamic, context} ->
        {type, context} = of_pattern(pattern, [root], stack, context)

        if is_descr(type) do
          {[type | static], dynamic, context}
        else
          {static, [type | dynamic], context}
        end
      end)

    if dynamic == [] do
      {Enum.reduce(static, &intersection/2), context}
    else
      # The dynamic parts have to be recomputed whenever they change
      {var, context} =
        of_pattern(var, [%{root: {:intersection, dynamic}, expr: expr}], stack, context)

      # But the static parts we push down as part of the argument intersection
      if static == [] do
        {var, context}
      else
        {{:intersection, [var, Enum.reduce(static, &intersection/2)]}, context}
      end
    end
  end

  # %Struct{...}
  defp of_pattern({:%, meta, [struct, {:%{}, _, args}]}, path, stack, context)
       when is_atom(struct) do
    {info, context} = Of.struct_info(struct, meta, stack, context)

    {pairs, context} =
      Enum.map_reduce(args, context, fn {key, value}, context ->
        {value_type, context} = of_pattern(value, [{:key, key} | path], stack, context)
        # TODO: We need to assume that these are dynamic until we have typed structs.
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
         {:%, meta, [var, {:%{}, _meta2, args}]} = expr,
         path,
         stack,
         context
       ) do
    {refined, context} = of_match_var(var, atom(), expr, stack, context)

    if compatible?(refined, atom()) do
      of_open_map(args, [__struct__: refined], [], path, stack, context)
    else
      error = {:badstruct, refined, expr, context}
      {error_type(), error(__MODULE__, error, meta, stack, context)}
    end
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
  defp of_pattern({name, meta, ctx} = var, path, _stack, context)
       when is_atom(name) and is_atom(ctx) do
    version = Keyword.fetch!(meta, :version)
    {_, context} = of_var(var, version, path, context)
    {{:var, version}, context}
  end

  defp of_var(var, version, reverse_path, context) do
    {args_paths, vars_paths, vars_deps} = context.pattern_info
    [%{root: root, expr: expr} | path] = Enum.reverse(reverse_path)
    node = %{root: root, var: var, expr: expr, path: path}

    pattern_info =
      case root do
        {:arg, _} ->
          paths = [node | Map.get(args_paths, version, [])]
          args_paths = Map.put(args_paths, version, paths)
          {args_paths, vars_paths, vars_deps}

        {:var, other} ->
          paths = [node | Map.get(vars_paths, version, [])]
          vars_paths = Map.put(vars_paths, version, paths)
          vars_deps = Map.update(vars_deps, version, %{other => []}, &Map.put(&1, other, []))
          vars_deps = Map.update(vars_deps, other, %{version => []}, &Map.put(&1, version, []))
          {args_paths, vars_paths, vars_deps}

        _ ->
          paths = [node | Map.get(vars_paths, version, [])]
          vars_paths = Map.put(vars_paths, version, paths)
          {args_paths, vars_paths, vars_deps}
      end

    {expr, %{context | pattern_info: pattern_info}}
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

    result =
      Enum.reduce(prefix, {[], [], context}, fn arg, {static, dynamic, context} ->
        {type, context} = of_pattern(arg, [:head | path], stack, context)

        if is_descr(type) do
          {[type | static], dynamic, context}
        else
          {static, [type | dynamic], context}
        end
      end)

    case result do
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
  def of_guard(atom, _expected, _expr, _stack, context) when is_atom(atom) do
    {atom([atom]), context}
  end

  # 12
  def of_guard(literal, _expected, _expr, _stack, context) when is_integer(literal) do
    {integer(), context}
  end

  # 1.2
  def of_guard(literal, _expected, _expr, _stack, context) when is_float(literal) do
    {float(), context}
  end

  # "..."
  def of_guard(literal, _expected, _expr, _stack, context) when is_binary(literal) do
    {binary(), context}
  end

  # []
  def of_guard([], _expected, _expr, _stack, context) do
    {empty_list(), context}
  end

  # [expr, ...]
  def of_guard(list, _expected, expr, stack, context) when is_list(list) do
    {prefix, suffix} = unpack_list(list, [])

    {prefix, context} =
      Enum.map_reduce(prefix, context, &of_guard(&1, term(), expr, stack, &2))

    {suffix, context} = of_guard(suffix, term(), expr, stack, context)
    {non_empty_list(Enum.reduce(prefix, &union/2), suffix), context}
  end

  # {left, right}
  def of_guard({left, right}, expected, expr, stack, context) do
    of_guard({:{}, [], [left, right]}, expected, expr, stack, context)
  end

  # %Struct{...}
  def of_guard({:%, meta, [module, {:%{}, _, args}]} = struct, expected, _expr, stack, context)
      when is_atom(module) do
    fun = &of_guard(&1, &2, struct, &3, &4)
    Of.struct_instance(module, args, expected, meta, stack, context, fun)
  end

  # %{...}
  def of_guard({:%{}, _meta, args}, expected, expr, stack, context) do
    Of.closed_map(args, expected, stack, context, &of_guard(&1, &2, expr, &3, &4))
  end

  # <<>>
  def of_guard({:<<>>, _meta, args}, _expected, _expr, stack, context) do
    context = Of.binary(args, :guard, stack, context)
    {binary(), context}
  end

  # ^var
  def of_guard({:^, _meta, [var]}, expected, expr, stack, context) do
    # This is used by binary size, which behaves as a mixture of match and guard
    Of.refine_body_var(var, expected, expr, stack, context)
  end

  # {...}
  def of_guard({:{}, _meta, args}, _expected, expr, stack, context) do
    {types, context} = Enum.map_reduce(args, context, &of_guard(&1, term(), expr, stack, &2))
    {tuple(types), context}
  end

  # var.field
  def of_guard({{:., _, [callee, key]}, _, []} = map_fetch, _expected, expr, stack, context)
      when not is_atom(callee) do
    {type, context} = of_guard(callee, term(), expr, stack, context)
    Of.map_fetch(map_fetch, type, key, stack, context)
  end

  # Comparison operators
  def of_guard({{:., _, [:erlang, function]}, _, args}, _expected, expr, stack, context)
      when function in [:==, :"/=", :"=:=", :"=/="] do
    {_args_type, context} =
      Enum.map_reduce(args, context, &of_guard(&1, term(), expr, stack, &2))

    {boolean(), context}
  end

  # Remote
  def of_guard({{:., _, [:erlang, fun]}, meta, args} = call, expected, _expr, stack, context)
      when is_atom(fun) do
    {info, domain, context} =
      Apply.remote_domain(:erlang, fun, args, expected, meta, stack, context)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_guard(&1, &2, call, stack, &3))

    Apply.remote_apply(info, :erlang, fun, args_types, call, stack, context)
  end

  # var
  def of_guard(var, _expected, _expr, _stack, context) when is_var(var) do
    {Of.var(var, context), context}
  end

  ## Helpers

  # pattern_info stores the variables defined in patterns,
  # additional information about the number of variables in
  # arguments and list heads, and a counter used to compute
  # the number of list heads.
  defp init_pattern_info(context) do
    %{context | pattern_info: {%{}, %{}, %{}}}
  end

  defp pop_pattern_info(%{pattern_info: pattern_info} = context) do
    {pattern_info, %{context | pattern_info: nil}}
  end

  def format_diagnostic({:badvar, old_type, new_type, var, context}) do
    traces = collect_traces(var, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types assigned to #{format_var(var)}:

              #{to_quoted_string(old_type)} !~ #{to_quoted_string(new_type)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badstruct, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected an atom as struct name:

              #{expr_to_string(expr) |> indent(4)}

          got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
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
  def format_diagnostic({:badpattern, meta, pattern_or_expr, index, tag, context}) do
    {to_trace, message} = badpattern(tag, pattern_or_expr, index)
    traces = collect_traces(to_trace, context)

    hints =
      case Keyword.get(meta, :type_check) do
        {:impl, _} = impl -> [impl]
        _ -> []
      end

    %{
      details: %{typing_traces: traces},
      message: IO.iodata_to_binary([message, format_traces(traces), format_hints(hints)])
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

  defp badpattern(_, pattern_or_expr, _) do
    {pattern_or_expr,
     """
     the following pattern will never match:

         #{expr_to_string(pattern_or_expr) |> indent(4)}
     """}
  end
end
