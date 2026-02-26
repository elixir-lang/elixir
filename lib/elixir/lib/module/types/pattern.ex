# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.{Apply, Of}
  import Module.Types.{Helpers, Descr}

  @doc """
  Defines if two list of arguments are subtypes of each other.
  """
  def args_subtype?([], []),
    do: true

  def args_subtype?([type], previous),
    do: subtype?(type, Enum.reduce(previous, none(), &union(&2, hd(&1))))

  def args_subtype?(args, previous) do
    subtype?(
      args_to_domain(args),
      Enum.reduce(previous, none(), &union(&2, args_to_domain(&1)))
    )
  end

  @doc """
  Refine the dependencies of variables represented by version.
  """
  def of_changed([], _stack, context) do
    context
  end

  def of_changed([_ | _] = versions, stack, %{vars: vars} = context) do
    {changed, seen} =
      Enum.reduce(versions, {[], []}, fn version, changed_seen ->
        %{^version => %{deps: deps}} = vars

        # An optimized version for of_changed_seen for the first pass
        deps
        |> Map.keys()
        |> Enum.reduce(changed_seen, fn dep_version, {changed, seen} ->
          {[{dep_version, version} | changed], [[version | dep_version] | seen]}
        end)
      end)

    of_changed_deps(:maps.from_list(changed), Map.from_keys(seen, true), stack, context)
  end

  defp of_changed_seen(deps, version, source_version, changed_seen) do
    deps
    |> Map.keys()
    |> Enum.reduce(changed_seen, fn
      # We don't point back to the thing that caused us to change.
      # In case both changed at once, the other side will be pointed
      # to naturally. Otherwise it has to be pointed to through other nodes.
      ^source_version, {changed, seen} ->
        {changed, seen}

      dep_version, {changed, seen} ->
        edge = [version | dep_version]

        case seen do
          %{^edge => _} -> {changed, seen}
          %{} -> {Map.put(changed, dep_version, version), Map.put(seen, edge, true)}
        end
    end)
  end

  defp of_changed_deps(changed, _seen, _stack, context) when changed == %{} do
    context
  end

  defp of_changed_deps(previous_changed, seen, stack, context) do
    {{changed, seen}, context} =
      Enum.reduce(previous_changed, {{%{}, seen}, context}, fn
        {version, source_version}, {changed_seen, context} ->
          {deps, context} = of_changed_var(version, stack, context)
          {of_changed_seen(deps, version, source_version, changed_seen), context}
      end)

    of_changed_deps(changed, seen, stack, context)
  end

  defp of_changed_var(version, stack, context) do
    case context.vars do
      %{^version => %{type: old_type, deps: deps, paths: paths} = data}
      when not is_map_key(data, :errored) ->
        try do
          Enum.reduce(paths, {%{}, context}, fn
            %{var: var, expr: expr, root: root, path: path}, {new_deps, context} ->
              actual = of_pattern_tree(root, stack, context)

              case of_pattern_var(path, actual, context) do
                {:ok, new_type} ->
                  # Optimization: if current type is already a subtype,
                  # there is nothing to refine
                  if old_type != term() and subtype?(old_type, new_type) do
                    {new_deps, context}
                  else
                    case Of.refine_head_var(version, new_type, expr, stack, context) do
                      {:ok, _type, context} ->
                        # Return the actual deps to be recomputed
                        {deps, context}

                      {:error, _old_type, error_context} ->
                        if match_error?(var, new_type) do
                          throw(badpattern_error(var, expr, stack, context))
                        else
                          throw(badvar_error(var, old_type, new_type, stack, error_context))
                        end
                    end
                  end

                :error ->
                  throw(badpattern_error(var, expr, stack, context))
              end
          end)
        catch
          context -> {%{}, context}
        end

      _ ->
        {%{}, context}
    end
  end

  @doc """
  Handles patterns and guards at once.

  The algorithm works as follows:

  1. First we traverse the patterns and build a pattern tree
     (which tells how to compute the type of a pattern) alongside
     the variable dependencies (which tells us how to compute the type
     of a variable).

  2. Then we traverse the pattern tree and compute the intersection
     between the pattern and the expected types (which is currently dynamic).

  3. Then we compute the guards and propagate new dependencies

  4. Then we propagate all dependencies to refine variables

  """
  def of_head(patterns, guards, expected, previous \\ [], tag, meta, stack, context) do
    %{vars: vars} = context
    stack = %{stack | meta: meta}

    case of_pattern_args(patterns, expected, previous, tag, stack, context) do
      {trees, pattern_precise?, changed, context} ->
        {guard_precise?, context} = of_guards(guards, changed, vars, stack, context)
        {trees, pattern_precise? and guard_precise?, context}

      {trees, context} ->
        {trees, false, context}
    end
  end

  @doc """
  Computes the domain from the pattern tree and expected types.

  Note we use `upper_bound` because the user of dynamic in the signature
  won't make a difference.
  """
  def of_domain([{tree, expected, _pattern} | trees], stack, context) do
    [
      intersection(of_pattern_tree(tree, stack, context), expected) |> upper_bound()
      | of_domain(trees, stack, context)
    ]
  end

  def of_domain([], _stack, _context) do
    []
  end

  defp of_pattern_args([], [], _previous, _tag, _stack, context) do
    {[], true, [], context}
  end

  defp of_pattern_args(patterns, expected, previous, tag, stack, context) do
    context = init_pattern_info(context, [])

    {trees, precise?, context} =
      of_pattern_args_zip(patterns, expected, 0, [], true, stack, context)

    {pattern_info, context} = pop_pattern_info(context)

    with {:ok, types} <- of_pattern_intersect(trees, 0, [], pattern_info, tag, stack, context),
         {_types, changed, context} <-
           of_pattern_refine(types, previous, pattern_info, tag, stack, context) do
      {trees, precise?, changed, context}
    else
      {:error, context} -> {trees, context}
    end
  end

  defp of_pattern_args_zip(
         [pattern | tail],
         [expected | types],
         index,
         acc,
         precise?,
         stack,
         context
       ) do
    {tree, pattern_precise?, context} =
      of_pattern(pattern, [%{root: {:arg, index}, expr: pattern}], stack, context)

    precise? = pattern_precise? and precise?
    acc = [{tree, expected, pattern} | acc]
    of_pattern_args_zip(tail, types, index + 1, acc, precise?, stack, context)
  end

  defp of_pattern_args_zip([], _types, _index, acc, precise?, _stack, context),
    do: {Enum.reverse(acc), precise?, context}

  @doc """
  Handles the match operator.
  """
  def of_match(pattern, expected_fun, expr, stack, context) do
    context = init_pattern_info(context, [])

    {tree, _precise?, context} =
      of_pattern(pattern, [%{root: {:arg, 0}, expr: expr}], stack, context)

    {pattern_info, context} = pop_pattern_info(context)
    {expected, context} = expected_fun.(of_pattern_tree(tree, stack, context), context)

    args = [{tree, expected, expr}]
    tag = {:match, expected}

    with {:ok, types} <- of_pattern_intersect(args, 0, [], pattern_info, tag, stack, context),
         {[type], changed, context} <-
           of_pattern_refine(types, [], pattern_info, tag, stack, context) do
      {type, of_changed(changed, stack, context)}
    else
      {:error, context} -> {expected, context}
    end
  end

  @doc """
  Handles matches in generators.
  """
  def of_generator(pattern, guards, expected, tag, expr, stack, %{vars: vars} = context) do
    context = init_pattern_info(context, [])

    {tree, _precise?, context} =
      of_pattern(pattern, [%{root: {:arg, 0}, expr: expr}], stack, context)

    {pattern_info, context} = pop_pattern_info(context)
    args = [{tree, expected, pattern}]

    with {:ok, types} <- of_pattern_intersect(args, 0, [], pattern_info, tag, stack, context),
         {_types, changed, context} <-
           of_pattern_refine(types, [], pattern_info, tag, stack, context) do
      {_precise?, context} = of_guards(guards, changed, vars, stack, context)
      context
    else
      {:error, context} ->
        context
    end
  end

  defp of_pattern_intersect([head | tail], index, acc, pattern_info, tag, stack, context) do
    {tree, expected, pattern} = head
    actual = of_pattern_tree(tree, stack, context)
    type = intersection(actual, expected)

    if empty?(type) do
      context = badpattern_error(pattern, index, tag, stack, context)
      {:error, error_vars(pattern_info, context)}
    else
      of_pattern_intersect(tail, index + 1, [type | acc], pattern_info, tag, stack, context)
    end
  end

  defp of_pattern_intersect([], _index, acc, _pattern_info, _tag, _stack, _context) do
    {:ok, Enum.reverse(acc)}
  end

  defp of_pattern_refine(types, previous, pattern_info, tag, stack, context) do
    types =
      case types do
        _ when previous == [] ->
          types

        [type] ->
          [Enum.reduce(previous, type, &difference(&2, hd(&1)))]

        [_ | _] ->
          previous
          |> Enum.reduce(args_to_domain(types), &difference(&2, args_to_domain(&1)))
          |> domain_to_flat_args(types)
      end

    try do
      pattern_info
      |> Enum.reverse()
      |> Enum.reduce({[], context}, fn {version, _pinned, node}, {changed, context} ->
        %{var: var, expr: expr, root: root, path: path} = node

        {actual, index} =
          case root do
            {:arg, index} -> {Enum.fetch!(types, index), index}
            _ -> {of_pattern_tree(root, stack, context), nil}
          end

        context =
          case of_pattern_var(path, actual, context) do
            {:ok, new_type} ->
              case Of.refine_head_var(var, new_type, expr, stack, context) do
                {:ok, _type, context} ->
                  context

                {:error, old_type, error_context} ->
                  if match_error?(var, new_type) do
                    throw(badpattern_error(expr, index, tag, stack, context))
                  else
                    throw(badvar_error(var, old_type, new_type, stack, error_context))
                  end
              end

            :error ->
              throw(badpattern_error(expr, index, tag, stack, context))
          end

        {[version | changed], context}
      end)
    catch
      context -> {:error, error_vars(pattern_info, context)}
    else
      {changed, context} ->
        {types, changed, context}
    end
  end

  defp error_vars(pattern_info, context) do
    Enum.reduce(pattern_info, context, fn
      {_version, true, _}, context -> context
      {version, false, _}, context -> Of.error_var(version, context)
    end)
  end

  defp match_error?({:match, _, __MODULE__}, _type), do: true
  defp match_error?(_var, type), do: empty?(type)

  defp badvar_error(var, old_type, new_type, stack, context) do
    error = {:badvar, old_type, new_type, var, context}
    error(__MODULE__, error, error_meta(var, stack), stack, context)
  end

  defp badpattern_error(var, expr, stack, context) do
    meta = error_meta(expr, stack)
    context = Of.error_var(var, context)
    error = {:badpattern, meta, expr, nil, :default, context}
    error(__MODULE__, error, meta, stack, context)
  end

  @doc """
  Marks a badpattern error.
  """
  def badpattern_error(expr, index, tag, stack, context) do
    meta = error_meta(expr, stack)
    error = {:badpattern, meta, expr, index, tag, context}
    error(__MODULE__, error, meta, stack, context)
  end

  defp error_meta(expr, stack) do
    if meta = get_meta(expr) do
      meta ++ Keyword.take(stack.meta, [:generated, :line, :type_check])
    else
      stack.meta
    end
  end

  defp init_pattern_info(context, value) do
    %{context | pattern_info: value}
  end

  defp pop_pattern_info(%{pattern_info: pattern_info} = context) do
    {pattern_info, %{context | pattern_info: nil}}
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

  defp of_pattern_var([{:domain, domain} | rest], type, context) do
    case map_get(type, domain) do
      {:ok, type} -> of_pattern_var(rest, type, context)
      _ -> :error
    end
  end

  defp of_pattern_var([:head | rest], type, context) do
    case list_hd(type) do
      {:ok, head} -> of_pattern_var(rest, head, context)
      _ -> :error
    end
  end

  defp of_pattern_var([{:subpattern, key} | rest], type, context) do
    %{^key => subpattern} = context.subpatterns
    of_pattern_var(rest, intersection(type, subpattern), context)
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
  def of_pattern_tree(descr, _stack, _context) when is_descr(descr),
    do: descr

  def of_pattern_tree({:guard, name, polarity, guard, expr}, stack, context) do
    {type, _context} = of_guard(guard, term(), expr, stack, context)

    # This logic mirrors the code in `Apply.compare`
    cond do
      # If it is a singleton, we can always be precise
      singleton?(type) -> if polarity, do: type, else: negation(type)
      # We are checking for `not x == 1` or similar, we can't say anything about x
      polarity == false -> term()
      # We are checking for `x == 1`, make sure x is integer or float
      name in [:==, :"/="] -> numberize(type)
      # Otherwise we have the literal type as is
      true -> type
    end
  end

  def of_pattern_tree({:tuple, entries}, stack, context) do
    tuple(Enum.map(entries, &of_pattern_tree(&1, stack, context)))
  end

  def of_pattern_tree({:open_map, static, dynamic}, stack, context) do
    dynamic =
      Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, stack, context)} end)

    open_map(static ++ dynamic)
  end

  def of_pattern_tree({:closed_map, static, dynamic}, stack, context) do
    dynamic =
      Enum.map(dynamic, fn {key, value} -> {key, of_pattern_tree(value, stack, context)} end)

    closed_map(static ++ dynamic)
  end

  def of_pattern_tree({:non_empty_list, [head | tail], suffix}, stack, context) do
    tail
    |> Enum.reduce(
      of_pattern_tree(head, stack, context),
      &union(of_pattern_tree(&1, stack, context), &2)
    )
    |> non_empty_list(of_pattern_tree(suffix, stack, context))
  end

  def of_pattern_tree({:intersection, entries}, stack, context) do
    entries
    |> Enum.map(&of_pattern_tree(&1, stack, context))
    |> Enum.reduce(&intersection/2)
  end

  def of_pattern_tree({:var, version}, _stack, context) do
    case context do
      %{vars: %{^version => %{type: type}}} -> type
      _ -> term()
    end
  end

  def of_pattern_tree(:key, _stack, _context) do
    term()
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
    context = Of.declare_var(var, context)

    case Of.refine_head_var(var, expected, expr, stack, context) do
      {:ok, type, context} ->
        {type, context}

      {:error, old_type, error_context} ->
        {error_type(), badvar_error(var, old_type, expected, stack, error_context)}
    end
  end

  def of_match_var({:<<>>, _meta, args}, _expected, _expr, stack, context) do
    Of.bitstring(args, :match, stack, context)
  end

  def of_match_var({:^, _meta, [{_, meta, _}]}, expected, expr, stack, context) do
    version = Keyword.fetch!(meta, :version)
    Of.refine_body_var(version, expected, expr, stack, context)
  end

  def of_match_var(atom, _expected, _expr, _stack, context) when is_atom(atom) do
    {atom(), context}
  end

  def of_match_var(binary, _expected, _expr, _stack, context) when is_binary(binary) do
    {binary(), context}
  end

  def of_match_var(integer, _expected, _expr, _stack, context) when is_integer(integer) do
    {integer(), context}
  end

  def of_match_var(float, _expected, _expr, _stack, context) when is_float(float) do
    {float(), context}
  end

  ## Patterns

  # :atom
  defp of_pattern(atom, _path, _stack, context) when is_atom(atom),
    do: {atom([atom]), true, context}

  # 12
  defp of_pattern(literal, _path, _stack, context) when is_integer(literal),
    do: {integer(), false, context}

  # 1.2
  defp of_pattern(literal, _path, _stack, context) when is_float(literal),
    do: {float(), false, context}

  # "..."
  defp of_pattern(literal, _path, _stack, context) when is_binary(literal),
    do: {binary(), false, context}

  # []
  defp of_pattern([], _path, _stack, context),
    do: {empty_list(), true, context}

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
    {precise?, matches, version, var} =
      match
      |> unpack_match([])
      |> Enum.split_while(&(not is_versioned_var(&1)))
      |> case do
        {matches, []} ->
          version = make_ref()
          {true, matches, version, {:match, [version: version], __MODULE__}}

        {pre, [{_, meta, _} = var | post]} ->
          version = Keyword.fetch!(meta, :version)
          {not is_map_key(context.vars, version), pre ++ post, version, var}
      end

    # Pass the current path to build the current var
    context = of_var(var, version, path, context)
    root = %{root: {:var, version}, expr: match}

    {precise?, static, dynamic, context} =
      Enum.reduce(matches, {precise?, [], [{:var, version}], context}, fn
        pattern, {precise?, static, dynamic, context} ->
          {type, pattern_precise?, context} = of_pattern(pattern, [root], stack, context)
          precise? = precise? and pattern_precise?

          if is_descr(type) do
            {precise?, [type | static], dynamic, context}
          else
            {precise?, static, [type | dynamic], context}
          end
      end)

    return =
      if static == [] do
        {:intersection, dynamic}
      else
        {:intersection, [Enum.reduce(static, &intersection/2) | dynamic]}
      end

    context = of_var(var, version, [%{root: return, expr: match}], context)
    {return, precise?, context}
  end

  # %Struct{...}
  defp of_pattern({:%, meta, [struct, {:%{}, _, args}]}, path, stack, context)
       when is_atom(struct) do
    {info, context} = Of.struct_info(struct, :pattern, meta, stack, context)

    if info do
      {pairs, {precise?, context}} =
        Enum.map_reduce(args, {true, context}, fn {key, value}, {precise?, context} ->
          {value_type, value_precise?, context} =
            of_pattern(value, [{:key, key} | path], stack, context)

          context =
            if Enum.any?(info, &(&1.field == key)) do
              context
            else
              Of.unknown_struct_field(struct, key, :pattern, meta, stack, context)
            end

          # TODO: We need to assume that these are dynamic until we have typed structs.
          {{key, value_type}, {precise? and value_precise?, context}}
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
        {closed_map(static), precise?, context}
      else
        {{:closed_map, static, dynamic}, precise?, context}
      end
    else
      {error_type(), false, context}
    end
  end

  # %var{...}
  defp of_pattern({:%, _, [{name, _, ctx} = var, {:%{}, _, args}]}, path, stack, context)
       when is_atom(name) and is_atom(ctx) and name != :_ do
    {var, precise?, context} = of_pattern(var, [{:key, :__struct__} | path], stack, context)
    dynamic = [__struct__: {:intersection, [atom(), var]}]
    of_open_map(args, precise?, [], dynamic, path, stack, context)
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
      of_open_map(args, singleton?(refined), [__struct__: refined], [], path, stack, context)
    else
      error = {:badstruct, refined, expr, context}
      {error_type(), false, error(__MODULE__, error, meta, stack, context)}
    end
  end

  # %{...}
  defp of_pattern({:%{}, _meta, args}, path, stack, context) do
    of_open_map(args, true, [], [], path, stack, context)
  end

  # <<...>>>
  defp of_pattern({:<<>>, _meta, args} = node, _path, stack, context) do
    {type, context} = Of.bitstring(args, :match, stack, context)
    {type, of_precise_bitstring?(node), context}
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
  defp of_pattern({:^, _meta, [{_, meta, _} = var]}, reverse_path, _stack, context) do
    version = Keyword.fetch!(meta, :version)

    case Enum.reverse(reverse_path) do
      [%{root: :key} | _] -> {Of.var(var, context), false, context}
      path -> {{:var, version}, false, of_shared_var(var, version, true, path, context)}
    end
  end

  # _
  defp of_pattern({:_, _meta, _var_context}, _path, _stack, context) do
    {term(), true, context}
  end

  # var
  defp of_pattern({name, meta, ctx} = var, path, _stack, context)
       when is_atom(name) and is_atom(ctx) do
    version = Keyword.fetch!(meta, :version)
    {{:var, version}, not is_map_key(context.vars, version), of_var(var, version, path, context)}
  end

  defp is_versioned_var({name, _meta, ctx}) when is_atom(name) and is_atom(ctx) and name != :_,
    do: true

  defp is_versioned_var(_), do: false

  defp of_var(var, version, reverse_path, context) do
    of_shared_var(var, version, false, Enum.reverse(reverse_path), Of.declare_var(var, context))
  end

  defp of_shared_var(var, version, pinned, path, %{pattern_info: pattern_info} = context)
       when is_list(pattern_info) do
    [%{root: root, expr: expr} | path] = path
    node = path_node(root, var, expr, path)
    context = %{context | pattern_info: [{version, pinned, node} | pattern_info]}

    case root do
      {:arg, _} ->
        context

      {:var, other} ->
        context = Of.track_var(version, [other], [node], context)
        Of.track_var(other, [version], [], context)

      _ ->
        Of.track_var(version, [], [node], context)
    end
  end

  @compile {:inline, path_node: 4}
  defp path_node(root, var, expr, path) do
    %{root: root, var: var, expr: expr, path: path}
  end

  defp of_open_map(args, precise?, static, dynamic, path, stack, context) do
    {precise?, static, dynamic, context} =
      Enum.reduce(args, {precise?, static, dynamic, context}, fn
        {key, value}, {precise?, static, dynamic, context} when is_atom(key) ->
          {value_type, value_precise?, context} =
            of_pattern(value, [{:key, key} | path], stack, context)

          precise? = precise? and value_precise?

          if is_descr(value_type) do
            {precise?, [{key, value_type} | static], dynamic, context}
          else
            {precise?, static, [{key, value_type} | dynamic], context}
          end

        {key, value}, {_precise?, static, dynamic, context} ->
          {key_type, _, context} = of_pattern(key, [%{root: :key, expr: key}], stack, context)
          true = is_descr(key_type)

          {_value_type, _precise?, context} =
            of_subpattern(value, [{:domain, key_type} | path], stack, context)

          # A domain key cannot restrict the map in any way,
          # because we are matching only on one possible value
          # and we cannot assert anything about any of the others.
          {false, static, dynamic, context}
      end)

    case dynamic do
      [] -> {open_map(static), precise?, context}
      _ -> {{:open_map, static, dynamic}, precise?, context}
    end
  end

  defp of_tuple(args, path, stack, context) do
    {_index, precise?, static?, entries, context} =
      Enum.reduce(args, {0, true, true, [], context}, fn arg,
                                                         {index, precise?, static?, acc, context} ->
        {type, elem_precise?, context} = of_pattern(arg, [{:elem, index} | path], stack, context)
        precise? = precise? and elem_precise?
        static? = static? and is_descr(type)
        {index + 1, precise?, static?, [type | acc], context}
      end)

    case static? do
      true -> {tuple(Enum.reverse(entries)), precise?, context}
      false -> {{:tuple, Enum.reverse(entries)}, precise?, context}
    end
  end

  # [] ++ []
  defp of_list([], [], _path, _stack, context) do
    {empty_list(), true, context}
  end

  # [] ++ suffix
  defp of_list([], suffix, path, stack, context) do
    of_pattern(suffix, path, stack, context)
  end

  # [prefix | suffix]
  defp of_list([prefix], suffix, path, stack, context) when is_var(prefix) and is_var(suffix) do
    {suffix_type, suffix_precise?, context} =
      of_pattern(suffix, [:tail | path], stack, context)

    context = annotate_list_subpattern(suffix, context)

    {prefix_type, prefix_precise?, context} =
      of_subpattern(prefix, [:head | path], stack, context)

    context = annotate_list_subpattern(prefix, context)

    type =
      if is_descr(prefix_type) and is_descr(suffix_type) do
        non_empty_list(prefix_type, suffix_type)
      else
        {:non_empty_list, [prefix_type], suffix_type}
      end

    {type, prefix_precise? and suffix_precise?, context}
  end

  # [prefix1, prefix2, prefix3], [prefix1, prefix2 | suffix]
  defp of_list(prefix, suffix, path, stack, context) do
    {suffix_type, _precise?, context} = of_pattern(suffix, [:tail | path], stack, context)

    {static, dynamic, context} =
      Enum.reduce(prefix, {[], [], context}, fn arg, {static, dynamic, context} ->
        {type, _precise?, context} = of_subpattern(arg, [:head | path], stack, context)

        if is_descr(type) do
          {[type | static], dynamic, context}
        else
          {static, [type | dynamic], context}
        end
      end)

    type =
      case {static, dynamic} do
        {static, []} when is_descr(suffix_type) ->
          non_empty_list(Enum.reduce(static, &union/2), suffix_type)

        {[], dynamic} ->
          {:non_empty_list, dynamic, suffix_type}

        {static, dynamic} ->
          {:non_empty_list, [Enum.reduce(static, &union/2) | dynamic], suffix_type}
      end

    {type, false, context}
  end

  defp list_subpattern?(version, context) do
    is_map_key(context.subpatterns, {:list, version})
  end

  defp annotate_list_subpattern({name, meta, _}, context) do
    if name != :_ do
      put_in(context.subpatterns[{:list, Keyword.fetch!(meta, :version)}], true)
    else
      context
    end
  end

  # These cases don't need to store information because they have no intersection
  defp of_subpattern(arg, path, stack, context)
       when is_number(arg) or is_binary(arg) or is_atom(arg) or arg == [] or is_var(arg) do
    of_pattern(arg, path, stack, context)
  end

  defp of_subpattern(arg, path, stack, %{subpatterns: subpatterns} = context) do
    key = map_size(subpatterns)
    context = %{context | subpatterns: Map.put(subpatterns, key, nil)}
    {type, precise?, context} = of_pattern(arg, [{:subpattern, key} | path], stack, context)
    {type, precise?, put_in(context.subpatterns[key], of_pattern_tree(type, stack, context))}
  end

  defp of_precise_bitstring?({:<<>>, _meta, [{:"::", _, [expr, {type, _, _}]}]})
       when type in [:binary, :bitstring, :bytes, :bits] do
    is_var(expr) or of_precise_bitstring?(expr)
  end

  defp of_precise_bitstring?(_), do: false

  ## Guards
  #
  # Whenever we have a or/orelse, we need to build multiple environments
  # and we only preserve intersections of those environments. However,
  # when building those environments, domain checks are always passed
  # upstream, except when they are on the right-side of `orelse`.
  #
  # Therefore, in addition to `conditional_vars`, we have to track:
  #
  # 1. Should we process type checks? We always do so at the root of guards.
  #    Inside or/orelse, we also need to check the environments.
  #
  # 2. Should we process domain checks? We always process it, except that, if
  #    on the right-side of orelse, it is only kept if it is shared across
  #    the environment vars.

  @atom_true atom([true])
  @atom_false atom([false])

  defp of_guards([], changed, _vars, stack, context) do
    {true, of_changed(changed, stack, context)}
  end

  defp of_guards(guards, changed, vars, stack, context) do
    context =
      init_pattern_info(context, %{
        allow_empty?: false,
        parent_version: nil,
        vars: vars,
        changed: Map.from_keys(changed, [])
      })

    {precise?, context} = of_guards(guards, stack, context)
    {%{vars: vars, changed: changed}, context} = pop_pattern_info(context)
    {is_map(vars) and precise?, of_changed(Map.keys(changed), stack, context)}
  end

  defp of_guards([guard], stack, context) do
    {type, context} = of_guard(guard, stack, context)
    maybe_badguard(type, guard, stack, context)
  end

  defp of_guards(guards, stack, context) do
    expr = Enum.reduce(guards, {:_, [], []}, &{:when, [], [&2, &1]})

    Of.with_conditional_vars(guards, true, expr, stack, context, fn guard, precise?, context ->
      {type, context} = of_guard(guard, stack, context)
      {guard_precise?, context} = maybe_badguard(type, guard, stack, context)
      {guard_precise? and precise?, context}
    end)
  end

  defp update_parent_version(parent_version, %{pattern_info: pattern_info} = context) do
    {pattern_info.parent_version,
     %{context | pattern_info: %{pattern_info | parent_version: parent_version}}}
  end

  defp enable_conditional_mode(%{pattern_info: pattern_info} = context) do
    %{context | pattern_info: %{pattern_info | allow_empty?: true}, conditional_vars: %{}}
  end

  defp maybe_badguard(type, guard, stack, context) do
    case booleaness(type) do
      :maybe_both ->
        {false, context}

      {true, maybe_or_always} ->
        {maybe_or_always == :always, context}

      _false_tuple_or_none ->
        error = {:badguard, type, guard, context}
        {false, error(__MODULE__, error, error_meta(guard, stack), stack, context)}
    end
  end

  defp of_guard(guard, stack, context) do
    of_guard(guard, @atom_true, guard, stack, context)
  end

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
    Of.bitstring(args, :guard, stack, context)
  end

  # ^var
  def of_guard({:^, _meta, [var]}, expected, expr, stack, context) do
    # This is used by binary size, which behaves as a mixture of match and guard
    of_guard(var, expected, expr, stack, context)
  end

  # {...}
  def of_guard({:{}, _meta, args}, _expected, expr, stack, context) do
    {types, context} = Enum.map_reduce(args, context, &of_guard(&1, term(), expr, stack, &2))
    {tuple(types), context}
  end

  # var.field
  def of_guard({{:., _, [callee, key]}, _, []} = map_fetch, expected, expr, stack, context)
      when not is_atom(callee) do
    {type, context} = of_guard(callee, open_map([{key, expected}]), expr, stack, context)
    Of.map_fetch(map_fetch, type, key, stack, context)
  end

  # Remote
  def of_guard({{:., _, [:erlang, fun]}, _meta, args} = call, expected, _, stack, context)
      when is_atom(fun) do
    of_remote(fun, args, call, expected, stack, context)
  end

  # The only possible case right now is the rewritten :lists.member/2 checks
  def of_guard({{:., _, [mod, fun]}, _meta, args} = call, expected, _, stack, context)
      when is_atom(mod) and is_atom(fun) do
    Apply.remote(mod, fun, args, expected, call, stack, context, &of_guard/5)
  end

  # var
  def of_guard({_, meta, _} = var, expected, expr, stack, context) when is_var(var) do
    version = Keyword.fetch!(meta, :version)

    # of_guard is also invoked inside patterns in case of bitstrings,
    # and also when vars change, so we need to deal with all possibilities
    # for pattern_info.
    case context.pattern_info do
      %{allow_empty?: allow_empty?, vars: vars, parent_version: parent_version, changed: changed} =
          pattern_info ->
        vars =
          is_map(vars) and not is_map_key(vars, version) and
            not list_subpattern?(version, context) and vars

        changed = Map.put(changed, version, [])
        pattern_info = %{pattern_info | vars: vars, changed: changed}
        context = %{context | pattern_info: pattern_info}

        context =
          if parent_version != nil,
            do: Of.track_var(version, [parent_version], [], context),
            else: context

        Of.refine_body_var(version, expected, expr, stack, context, allow_empty?)

      list when is_list(list) ->
        node = path_node(expected, var, expr, [])
        {Of.var(var, context), %{context | pattern_info: [{version, false, node} | list]}}

      nil ->
        {Of.var(var, context), context}
    end
  end

  defp of_compare(fun, polarity, {_, meta, _} = var, other_side, call, stack, context)
       when is_var(var) do
    version = Keyword.fetch!(meta, :version)

    # Add a new path node to the current variable
    node = path_node({:guard, fun, polarity, other_side, call}, var, call, [])
    context = Of.track_var(version, [], [node], context)

    # Make any variable on the other side propagate the change to this one
    {parent_version, context} = update_parent_version(version, context)
    {guard_type, context} = of_guard(other_side, term(), call, stack, context)

    # Revert and return parent version
    {^version, context} = update_parent_version(parent_version, context)
    {guard_type, context}
  end

  defp of_compare(_fun, _polarity, _var, other_side, call, stack, context) do
    of_guard(other_side, term(), call, stack, context)
  end

  @comp_op [:==, :"/=", :"=:=", :"=/="]

  defp of_remote(fun, [left, right] = args, call, expected, stack, context)
       when fun in @comp_op do
    with false <- Macro.quoted_literal?(left) or Macro.quoted_literal?(right),
         true <- is_var(left) or is_var(right),
         {boolean, _maybe_or_always} <- booleaness(expected),
         %{pattern_info: %{}} <- context do
      polarity =
        case boolean do
          true -> fun in [:==, :"=:="]
          false -> fun in [:"/=", :"=/="]
        end

      {left_type, context} = of_compare(fun, polarity, right, left, call, stack, context)
      {right_type, context} = of_compare(fun, polarity, left, right, call, stack, context)
      Apply.return_compare(fun, left_type, right_type, boolean(), false, call, stack, context)
    else
      _ -> Apply.remote(:erlang, fun, args, expected, call, stack, context, &of_guard/5)
    end
  end

  defp of_remote(fun, _args, call, expected, stack, context)
       when fun in [:and, :or, :andalso, :orelse] do
    {both_domain, abort_domain, always_rhs?} =
      case fun do
        :andalso -> {@atom_true, @atom_false, false}
        :orelse -> {@atom_false, @atom_true, false}
        :and -> {@atom_true, @atom_false, true}
        :or -> {@atom_false, @atom_true, true}
      end

    # If we have multiple operations in a row,
    # we unpack them into a single pass, to avoid
    # building nested conditional environments.
    [left | right] =
      case unpack_op(call, fun, []) do
        entries when fun == :orelse -> reconstruct_lists_member(entries)
        entries -> entries
      end

    # For example, if the expected type is true for andalso, then it can
    # only be true if both clauses are executed, so we know the first
    # argument has to be true and the second has to be expected.
    if subtype?(expected, both_domain) do
      of_logical_all([left | right], true, both_domain, abort_domain, stack, context)
    else
      cond_context = enable_conditional_mode(context)

      # Compute the sure types, which are stored directly in the context
      {_type, context} = of_guard(left, boolean(), left, stack, context)

      # andalso/orelse may not execute the rhs, so we cannot get sure types from it
      context =
        case always_rhs? do
          true ->
            Enum.reduce(right, context, fn expr, context ->
              {_, context} = of_guard(expr, boolean(), expr, stack, context)
              context
            end)

          false ->
            context
        end

      {type, vars_conds} =
        of_logical_cond([left | right], true, expected, abort_domain, stack, cond_context, [])

      # We will be precise if all branches changed the same variable
      context =
        update_in(context.pattern_info.vars, fn
          false ->
            false

          vars ->
            [{_, cond} | tail] = vars_conds
            Enum.all?(tail, fn {_, tail_cond} -> cond == tail_cond end) and vars
        end)

      {type, Of.reduce_conditional_vars(vars_conds, call, stack, context)}
    end
  end

  defp of_remote(fun, args, call, expected, stack, context) do
    Apply.remote(:erlang, fun, args, expected, call, stack, context, &of_guard/5)
  end

  defp unpack_op({{:., _, [:erlang, fun]}, _, [left, right]}, fun, acc) do
    unpack_op(left, fun, unpack_op(right, fun, acc))
  end

  defp unpack_op(other, _fun, acc) do
    [other | acc]
  end

  # Reconstruct left in right operations but only when the right-side is a literal.
  # When the right-side is not a literal, we need to track dependencies between
  # left and right-side, which is currently not done for the `:lists.member/2` handling.
  defp reconstruct_lists_member([head | tail]) do
    with {{:., dot_meta, [:erlang, :"=:="]}, meta, [left, right]} <- head,
         true <- Macro.quoted_literal?(right),
         false <- data_size_op?(left),
         {[_ | _] = entries, tail} <- reconstruct_lists_member(tail, left, []) do
      in_args = [left, [right | entries]]
      [{{:., dot_meta, [:lists, :member]}, meta, in_args} | reconstruct_lists_member(tail)]
    else
      _ -> [head | reconstruct_lists_member(tail)]
    end
  end

  defp reconstruct_lists_member([]), do: []

  defp reconstruct_lists_member(list, left, acc) do
    with [{{:., _, [:erlang, :"=:="]}, _, [^left, right]} | tail] <- list,
         true <- Macro.quoted_literal?(right) do
      reconstruct_lists_member(tail, left, [right | acc])
    else
      _ -> {Enum.reverse(acc), list}
    end
  end

  defp data_size_op?({{:., _, [:erlang, op]}, _, [_]})
       when op in [:length, :tuple_size, :map_size],
       do: true

  defp data_size_op?(_),
    do: false

  defp of_logical_all([head], disjoint?, expected, to_abort, stack, context) do
    {type, context} = of_guard(head, expected, head, stack, context)

    case disjoint? do
      true -> {type, context}
      false -> {union(to_abort, type), context}
    end
  end

  defp of_logical_all([head | tail], disjoint?, expected, to_abort, stack, context) do
    {type, context} = of_guard(head, expected, head, stack, context)
    disjoint? = disjoint? and disjoint?(type, to_abort)
    of_logical_all(tail, disjoint?, expected, to_abort, stack, context)
  end

  defp of_logical_cond([head], disjoint?, expected, to_abort, stack, context, acc) do
    {type, %{vars: vars, conditional_vars: cond_vars}} =
      of_guard(head, expected, head, stack, context)

    acc = [{vars, cond_vars} | acc]

    case disjoint? do
      true -> {type, acc}
      false -> {union(to_abort, type), acc}
    end
  end

  defp of_logical_cond([head | tail], disjoint?, expected, to_abort, stack, context, acc) do
    {type, %{vars: vars, conditional_vars: cond_vars}} =
      of_guard(head, expected, head, stack, context)

    disjoint? = disjoint? and disjoint?(type, to_abort)
    acc = [{vars, cond_vars} | acc]
    of_logical_cond(tail, disjoint?, expected, to_abort, stack, context, acc)
  end

  ## Helpers

  def format_diagnostic({:badguard, type, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          this guard will never succeed:

              #{expr_to_string(expr) |> indent(4)}

          because it returns type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
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
  #     :for_reduce or :with_else or :fn or :default or
  #       {{:case | :try_else, meta, expr, type}, [arg], [previous]} or
  #       {:receive | :try_catch, [arg], [previous]}
  #
  # $ typep match_pattern =
  #     :with or :for or {:match, type}
  #
  # The match pattern ones have the whole expression instead
  # of a single pattern.
  def format_diagnostic({:badpattern, meta, pattern_or_expr, index, tag, context}) do
    # TODO: stop passing pattern_or_expr as argument
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

  defp badpattern({{op, meta, expr, type}, args, previous}, _, _) when op in [:case, :try_else] do
    type_check = meta[:type_check]

    cond do
      match?({:case, _}, type_check) ->
        message =
          case type_check do
            {:case, op} when op in [:and, :or] ->
              {first_message, second_message} =
                case booleaness(type) do
                  {true, _} -> {" will always succeed", "because it evaluates to"}
                  {false, _} -> {" will never succeed", "because it evaluates to"}
                  :none -> {" will always fail", "because it evaluates to"}
                  _ -> {"", "will always evaluate to"}
                end

              """
              the following conditional expression#{first_message}:

                  #{expr_to_string(expr) |> indent(4)}

              #{second_message}:

                  #{to_quoted_string(type) |> indent(4)}
              """

            {:case, :||} ->
              if subtype?(type, atom([false, nil])) do
                """
                the following conditional expression will never succeed:

                    #{expr_to_string(expr) |> indent(4)}

                because it evaluates to:

                    #{to_quoted_string(type) |> indent(4)}
                """
              else
                """
                the right-hand side of || will never be executed:

                    #{expr_to_string({:||, [], [expr, {:..., [], []}]}) |> indent(4)}

                because the left-hand side always evaluates to:

                    #{to_quoted_string(type) |> indent(4)}
                """
              end

            _ ->
              """
              the following conditional expression:

                  #{expr_to_string(expr) |> indent(4)}

              will always evaluate to:

                  #{to_quoted_string(type) |> indent(4)}
              """
          end

        {expr, message}

      previous == [] ->
        {args,
         """
         the following clause will never match:

             #{args_to_string(args) |> indent(4)} ->

         because it attempts to match on the result of:

             #{expr_to_string(expr) |> indent(4)}

         which has type:

             #{to_quoted_string(type) |> indent(4)}
         """}

      args_subtype?([type], previous) ->
        {args,
         """
         the following clause cannot match because the previous clauses already matched all possible values:

             #{args_to_string(args) |> indent(4)} ->

         it attempts to match on the result of:

             #{expr_to_string(expr) |> indent(4)}

         which has the already matched type:

             #{to_quoted_string(type) |> indent(4)}
         """}

      true ->
        {args,
         """
         the following clause is redundant:

             #{args_to_string(args) |> indent(4)} ->

         previous clauses have already matched on the following types:

             #{previous_to_string(previous)}
         """}
    end
  end

  defp badpattern({op, args, previous}, _, _) when op in [:receive, :try_catch] do
    {args,
     """
     the following clause is redundant:

         #{args_to_string(args) |> indent(4)} ->

     previous clauses have already matched on the following types:

         #{previous_to_string(previous)}
     """}
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

  defp badpattern({:infer, types}, pattern_or_expr, index) when is_integer(index) do
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

  defp args_to_string(args) do
    args
    |> Enum.map_join(", ", &expr_to_string/1)
    |> indent(4)
  end

  defp previous_to_string(previous) do
    Enum.map_join(previous, "\n    ", fn types ->
      types
      |> Enum.map_join(", ", &to_quoted_string/1)
      |> indent(4)
    end)
  end
end
