# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.{Apply, Of}
  import Module.Types.{Helpers, Descr}

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
  def of_head(patterns, guards, expected, tag, meta, stack, context) do
    stack = %{stack | meta: meta}

    case of_pattern_args(patterns, expected, tag, stack, context) do
      {trees, changed, context} ->
        {trees, of_changed(changed, stack, of_guards(guards, stack, context))}

      {trees, context} ->
        {trees, context}
    end
  end

  @doc """
  Computes the domain from the pattern tree and expected types.

  Note we use `upper_bound` because the user of dynamic in the signature
  won't make a difference.
  """
  def of_domain([{tree, expected, _pattern} | trees], context) do
    [
      intersection(of_pattern_tree(tree, context), expected) |> upper_bound()
      | of_domain(trees, context)
    ]
  end

  def of_domain([], _context) do
    []
  end

  defp of_pattern_args([], [], _tag, _stack, context) do
    {[], %{}, context}
  end

  defp of_pattern_args(patterns, expected, tag, stack, context) do
    context = init_pattern_info(context)
    {trees, context} = of_pattern_args_zip(patterns, expected, 0, [], stack, context)
    {pattern_info, context} = pop_pattern_info(context)

    case of_pattern_intersect(trees, 0, [], pattern_info, tag, stack, context) do
      {_types, changed, context} -> {trees, changed, context}
      {:error, context} -> {trees, context}
    end
  end

  defp of_pattern_args_zip([pattern | tail], [expected | types], index, acc, stack, context) do
    {tree, context} = of_pattern(pattern, [%{root: {:arg, index}, expr: pattern}], stack, context)
    acc = [{tree, expected, pattern} | acc]
    of_pattern_args_zip(tail, types, index + 1, acc, stack, context)
  end

  defp of_pattern_args_zip([], _types, _index, acc, _stack, context),
    do: {Enum.reverse(acc), context}

  @doc """
  Handles the match operator.
  """
  def of_match(pattern, expected_fun, expr, stack, context) do
    context = init_pattern_info(context)
    {tree, context} = of_pattern(pattern, [%{root: {:arg, 0}, expr: expr}], stack, context)
    {pattern_info, context} = pop_pattern_info(context)
    {expected, context} = expected_fun.(of_pattern_tree(tree, context), context)

    args = [{tree, expected, expr}]
    tag = {:match, expected}

    case of_pattern_intersect(args, 0, [], pattern_info, tag, stack, context) do
      {[type], changed, context} -> {type, of_changed(changed, stack, context)}
      {:error, context} -> {expected, context}
    end
  end

  @doc """
  Handles matches in generators.
  """
  def of_generator(pattern, guards, expected, tag, expr, stack, context) do
    context = init_pattern_info(context)
    {tree, context} = of_pattern(pattern, [%{root: {:arg, 0}, expr: expr}], stack, context)
    {pattern_info, context} = pop_pattern_info(context)
    args = [{tree, expected, pattern}]

    case of_pattern_intersect(args, 0, [], pattern_info, tag, stack, context) do
      {_types, changed, context} -> of_changed(changed, stack, of_guards(guards, stack, context))
      {:error, context} -> context
    end
  end

  defp of_pattern_intersect([head | tail], index, acc, pattern_info, tag, stack, context) do
    {tree, expected, pattern} = head
    actual = of_pattern_tree(tree, context)
    type = intersection(actual, expected)

    if empty?(type) do
      context = badpattern_error(pattern, index, tag, stack, context)
      {:error, error_vars(pattern_info, context)}
    else
      of_pattern_intersect(tail, index + 1, [type | acc], pattern_info, tag, stack, context)
    end
  end

  defp of_pattern_intersect([], _index, acc, pattern_info, tag, stack, context) do
    %{vars: context_vars} = context
    types = Enum.reverse(acc)

    try do
      pattern_info
      |> Enum.reverse()
      |> Enum.reduce({%{}, context}, fn {version, node}, {changed, context} ->
        %{var: var, expr: expr, root: root, path: path} = node

        {actual, index} =
          case root do
            {:arg, index} -> {Enum.fetch!(types, index), index}
            _ -> {of_pattern_tree(root, context), nil}
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

        %{^version => %{deps: deps}} = context_vars
        {Map.merge(changed, deps), context}
      end)
    catch
      context -> {:error, error_vars(pattern_info, context)}
    else
      {changed, context} ->
        {types, changed, context}
    end
  end

  defp of_changed(changed, _stack, context) when changed == %{} do
    context
  end

  defp of_changed(previous_changed, stack, context) do
    {changed, context} =
      previous_changed
      |> Map.keys()
      |> Enum.reduce({%{}, context}, fn version, {changed, context} ->
        {new_deps, context} = of_changed_var(version, stack, context)
        {Map.merge(changed, new_deps), context}
      end)

    of_changed(changed, stack, context)
  end

  defp of_changed_var(version, stack, context) do
    case context.vars do
      %{^version => %{type: old_type, deps: deps, paths: paths} = data}
      when not is_map_key(data, :errored) ->
        try do
          Enum.reduce(paths, {%{}, context}, fn
            %{var: var, expr: expr, root: root, path: path}, {new_deps, context} ->
              actual = of_pattern_tree(root, context)

              case of_pattern_var(path, actual, context) do
                {:ok, new_type} ->
                  # Optimization: if current type is already a subtype,
                  # there is nothing to refine
                  if old_type != term() and subtype?(old_type, new_type) do
                    {new_deps, context}
                  else
                    case Of.refine_head_var(var, new_type, expr, stack, context) do
                      {:ok, _type, context} ->
                        # Return the actual deps to be recomputed
                        {deps, context}

                      {:error, _old_type, error_context} ->
                        if match_error?(var, new_type) do
                          throw(badmatch_error(var, expr, stack, context))
                        else
                          throw(badvar_error(var, old_type, new_type, stack, error_context))
                        end
                    end
                  end

                :error ->
                  throw(badmatch_error(var, expr, stack, context))
              end
          end)
        catch
          context -> {%{}, context}
        end

      _ ->
        {%{}, context}
    end
  end

  defp error_vars(pattern_info, context) do
    Enum.reduce(pattern_info, context, fn {_version, %{var: var}}, context ->
      Of.error_var(var, context)
    end)
  end

  defp match_error?({:match, _, __MODULE__}, _type), do: true
  defp match_error?(_var, type), do: empty?(type)

  defp badvar_error(var, old_type, new_type, stack, context) do
    error = {:badvar, old_type, new_type, var, context}
    error(__MODULE__, error, error_meta(var, stack), stack, context)
  end

  defp badmatch_error(var, expr, stack, context) do
    context = Of.error_var(var, context)
    error(__MODULE__, {:badmatch, expr, context}, error_meta(expr, stack), stack, context)
  end

  defp badpattern_error(expr, index, tag, stack, context) do
    meta = error_meta(expr, stack)

    error =
      if index do
        {:badpattern, meta, expr, index, tag, context}
      else
        {:badmatch, expr, context}
      end

    error(__MODULE__, error, meta, stack, context)
  end

  defp error_meta(expr, stack) do
    if meta = get_meta(expr) do
      meta ++ Keyword.take(stack.meta, [:generated, :line, :type_check])
    else
      stack.meta
    end
  end

  # pattern_info stores the paths defined from patterns.
  defp init_pattern_info(context) do
    %{context | pattern_info: []}
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
    context = Of.declare_var(var, context)

    case Of.refine_head_var(var, expected, expr, stack, context) do
      {:ok, type, context} ->
        {type, context}

      {:error, old_type, error_context} ->
        {error_type(), badvar_error(var, old_type, expected, stack, error_context)}
    end
  end

  def of_match_var({:<<>>, _meta, args}, _expected, _expr, stack, context) do
    {binary(), Of.binary(args, :match, stack, context)}
  end

  def of_match_var({:^, _meta, [var]}, expected, expr, stack, context) do
    Of.refine_body_var(var, expected, expr, stack, context)
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

  @doc """
  Handle `size` in binary modifiers.

  They behave like guards, so we need to take into account their scope.
  """
  def of_size(:match, arg, expr, stack, context) do
    of_guard(arg, integer(), expr, stack, context)
  end

  def of_size(:guard, arg, expr, stack, context) do
    of_guard(arg, integer(), expr, stack, context)
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
    {matches, version, var} =
      match
      |> unpack_match([])
      |> Enum.split_while(&(not is_versioned_var(&1)))
      |> case do
        {matches, []} ->
          version = make_ref()
          {matches, version, {:match, [version: version], __MODULE__}}

        {pre, [{_, meta, _} = var | post]} ->
          version = Keyword.fetch!(meta, :version)
          {pre ++ post, version, var}
      end

    # Pass the current path to build the current var
    context = of_var(var, version, path, context)
    root = %{root: {:var, version}, expr: match}

    {static, dynamic, context} =
      Enum.reduce(matches, {[], [], context}, fn pattern, {static, dynamic, context} ->
        {type, context} = of_pattern(pattern, [root], stack, context)

        if is_descr(type) do
          {[type | static], dynamic, context}
        else
          {static, [type | dynamic], context}
        end
      end)

    return =
      cond do
        dynamic == [] -> Enum.reduce(static, &intersection/2)
        static == [] -> {:intersection, dynamic}
        true -> {:intersection, [Enum.reduce(static, &intersection/2) | dynamic]}
      end

    # If the path has domain keys or head, then it is imprecise,
    # and we need to keep filtering the match variable itself.
    imprecise? =
      Enum.any?(path, fn
        {:domain, _} -> true
        :head -> true
        _ -> false
      end)

    if dynamic == [] and not imprecise? do
      {return, context}
    else
      context = of_var(var, version, [%{root: return, expr: match}], context)
      {return, context}
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
    {{:var, version}, of_var(var, version, path, context)}
  end

  defp is_versioned_var({name, _meta, ctx}) when is_atom(name) and is_atom(ctx) and name != :_,
    do: true

  defp is_versioned_var(_), do: false

  defp of_var(var, version, reverse_path, context) do
    [%{root: root, expr: expr} | path] = Enum.reverse(reverse_path)
    node = %{root: root, var: var, expr: expr, path: path}
    context = Of.declare_var(var, context)
    context = %{context | pattern_info: [{version, node} | context.pattern_info]}

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

  defp of_open_map(args, static, dynamic, path, stack, context) do
    {static, dynamic, context} =
      Enum.reduce(args, {static, dynamic, context}, fn
        {key, value}, {static, dynamic, context} when is_atom(key) ->
          {value_type, context} = of_pattern(value, [{:key, key} | path], stack, context)

          if is_descr(value_type) do
            {[{key, value_type} | static], dynamic, context}
          else
            {static, [{key, value_type} | dynamic], context}
          end

        {key, value}, {static, dynamic, context} ->
          # Keys are always static and won't use the path
          {key_type, context} = of_pattern(key, path, stack, context)
          true = is_descr(key_type)

          {_value_type, context} = of_pattern(value, [{:domain, key_type} | path], stack, context)

          # A domain key cannot restrict the map in any way,
          # because we are matching only on one possible value
          # and we cannot assert anything about any of the others.
          {static, dynamic, context}
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

  defp of_guards([], _stack, context) do
    context
  end

  defp of_guards([guard], stack, context) do
    {type, context} = of_guard(guard, stack, context)
    maybe_badguard(type, guard, stack, context)
  end

  defp of_guards(guards, stack, context) do
    expr = Enum.reduce(guards, {:_, [], []}, &{:when, [], [&2, &1]})

    {:ok, context} =
      Of.with_conditional_vars(guards, :ok, expr, stack, context, fn guard, :ok, context ->
        {type, context} = of_guard(guard, stack, context)
        {:ok, maybe_badguard(type, guard, stack, context)}
      end)

    context
  end

  defp maybe_badguard(type, guard, stack, context) do
    if never_true?(type) do
      error = {:badguard, type, guard, context}
      error(__MODULE__, error, error_meta(guard, stack), stack, context)
    else
      context
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
  def of_guard({{:., _, [callee, key]}, _, []} = map_fetch, expected, expr, stack, context)
      when not is_atom(callee) do
    {type, context} = of_guard(callee, open_map([{key, expected}]), expr, stack, context)
    Of.map_fetch(map_fetch, type, key, stack, context)
  end

  # Remote
  def of_guard({{:., _, [:erlang, fun]}, meta, args} = call, expected, _, stack, context)
      when is_atom(fun) do
    of_remote(fun, meta, args, call, expected, stack, context)
  end

  # var
  def of_guard(var, expected, expr, stack, context) when is_var(var) do
    Of.refine_body_var(var, expected, expr, stack, context)
  end

  defp of_remote(fun, _meta, _args, call, expected, stack, context)
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
    [left | right] = unpack_op(call, fun, [])

    # For example, if the expected type is true for andalso, then it can
    # only be true if both clauses are executed, so we know the first
    # argument has to be true and the second has to be expected.
    if subtype?(expected, both_domain) do
      of_logical_all([left | right], true, both_domain, abort_domain, stack, context)
    else
      cond_context = %{context | conditional_vars: %{}}

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

      {type, Of.reduce_conditional_vars(vars_conds, call, stack, context)}
    end
  end

  defp of_remote(fun, meta, args, call, expected, stack, context) do
    {info, domain, context} =
      Apply.remote_domain(:erlang, fun, args, expected, meta, stack, context)

    {args_types, context} =
      zip_map_reduce(args, domain, context, &of_guard(&1, &2, call, stack, &3))

    Apply.remote_apply(info, :erlang, fun, args_types, call, stack, context)
  end

  defp unpack_op({{:., _, [:erlang, fun]}, _, [left, right]}, fun, acc) do
    unpack_op(left, fun, unpack_op(right, fun, acc))
  end

  defp unpack_op(other, _fun, acc) do
    [other | acc]
  end

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

  def format_diagnostic({:badmatch, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          this match will never succeed due to incompatible types:

              #{expr_to_string(expr) |> indent(4)}
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
