defmodule Module.Types.Infer do
  @moduledoc false

  import Module.Types.Helpers

  @doc """
  Unifies two types and returns the unified type and an updated typing context
  or an error in case of a typing conflict.
  """
  def unify(source, target, stack, context) do
    case do_unify(source, target, stack, context) do
      {:ok, type, context} ->
        {:ok, type, context}

      {:error, reason} ->
        if stack.context == :pattern do
          case do_unify(target, source, stack, context) do
            {:ok, type, context} ->
              {:ok, type, context}

            {:error, _} ->
              {:error, reason}
          end
        else
          {:error, reason}
        end
    end
  end

  defp do_unify(same, same, _stack, context) do
    {:ok, same, context}
  end

  defp do_unify(type, {:var, var}, stack, context) do
    case Map.fetch!(context.types, var) do
      {:var, var_type} ->
        do_unify(type, {:var, var_type}, stack, context)

      _other ->
        unify_var(var, type, stack, context, _var_source = false)
    end
  end

  defp do_unify({:var, var}, type, stack, context) do
    case Map.fetch!(context.types, var) do
      {:var, var_type} ->
        do_unify({:var, var_type}, type, stack, context)

      _other ->
        unify_var(var, type, stack, context, _var_source = true)
    end
  end

  defp do_unify({:tuple, sources}, {:tuple, targets}, stack, context)
       when length(sources) == length(targets) do
    result =
      map_reduce_ok(Enum.zip(sources, targets), context, fn {source, target}, context ->
        unify(source, target, stack, context)
      end)

    case result do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify({:list, source}, {:list, target}, stack, context) do
    case unify(source, target, stack, context) do
      {:ok, type, context} -> {:ok, {:list, type}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify({:map, source_pairs}, {:map, target_pairs}, stack, context) do
    case unify_structs(source_pairs, target_pairs, stack, context) do
      :ok -> unify_maps(source_pairs, target_pairs, stack, context)
      {:error, reason} -> {:error, reason}
    end
  end

  defp do_unify(source, :dynamic, _stack, context) do
    {:ok, source, context}
  end

  defp do_unify(:dynamic, target, _stack, context) do
    {:ok, target, context}
  end

  defp do_unify(source, target, stack, context) do
    if subtype?(source, target, context) do
      {:ok, source, context}
    else
      error({:unable_unify, source, target}, stack, context)
    end
  end

  defp unify_var(var, :dynamic, _stack, context, _var_source?) do
    {:ok, {:var, var}, context}
  end

  defp unify_var(var, type, stack, context, var_source?) do
    case Map.fetch!(context.types, var) do
      :unbound ->
        context = refine_var(var, type, stack, context)
        stack = push_unify_stack(var, stack)

        if recursive_type?(type, [], context) do
          if var_source? do
            error({:unable_unify, {:var, var}, type}, stack, context)
          else
            error({:unable_unify, type, {:var, var}}, stack, context)
          end
        else
          {:ok, {:var, var}, context}
        end

      var_type ->
        # Only add trace if the variable wasn't already "expanded"
        context =
          if variable_expanded?(var, stack, context) do
            context
          else
            trace_var(var, type, stack, context)
          end

        stack = push_unify_stack(var, stack)

        unify_result =
          if var_source? do
            unify(var_type, type, stack, context)
          else
            unify(type, var_type, stack, context)
          end

        case unify_result do
          {:ok, var_type, context} ->
            context = refine_var(var, var_type, stack, context)
            {:ok, {:var, var}, context}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp unify_structs(left_pairs, right_pairs, stack, context) do
    with {:ok, left_module} when is_atom(left_module) <- fetch_struct_pair(left_pairs),
         {:ok, right_module} when is_atom(right_module) <- fetch_struct_pair(right_pairs) do
      if left_module == right_module do
        :ok
      else
        left = {:map, [{:required, {:atom, :__struct__}, left_module}]}
        right = {:map, [{:required, {:atom, :__struct__}, right_module}]}
        error({:unable_unify, left, right}, stack, context)
      end
    else
      _ -> :ok
    end
  end

  # * All required keys on each side need to match to the other side.
  # * All optional keys on each side that do not match must be discarded.

  defp unify_maps(source_pairs, target_pairs, stack, context) do
    source_pairs = expand_struct(source_pairs)
    target_pairs = expand_struct(target_pairs)

    {source_required, source_optional} = split_pairs(source_pairs)
    {target_required, target_optional} = split_pairs(target_pairs)

    with {:ok, source_required_pairs, context} <-
           unify_source_required(source_required, target_pairs, stack, context),
         {:ok, target_required_pairs, context} <-
           unify_target_required(target_required, source_pairs, stack, context),
         {:ok, source_optional_pairs, context} <-
           unify_source_optional(source_optional, target_optional, stack, context),
         {:ok, target_optional_pairs, context} <-
           unify_target_optional(target_optional, source_optional, stack, context),
         pairs =
           [
             source_required_pairs,
             target_required_pairs,
             source_optional_pairs,
             target_optional_pairs
           ]
           |> Enum.concat()
           # Remove duplicate pairs from matching in both left and right directions
           |> Enum.uniq()
           |> simplify_struct() do
      {:ok, {:map, pairs}, context}
    else
      {:error, :unify} -> error({:unable_unify, {:map, source_pairs}, {:map, target_pairs}}, stack, context)
      {:error, context} -> {:error, context}
    end
  end

  defp unify_source_required(source_required, target_pairs, stack, context) do
    map_reduce_ok(source_required, context, fn {source_key, source_value}, context ->
      Enum.find_value(target_pairs, fn {target_kind, target_key, target_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, {:required, key, value}, context}

            {:error, _reason} ->
              source_map = {:map, [{:required, source_key, source_value}]}
              target_map = {:map, [{target_kind, target_key, target_value}]}
              error({:unable_unify, source_map, target_map}, stack, context)
          end
        else
          {:error, _reason} -> nil
        end
      end) || {:error, :unify}
    end)
  end

  defp unify_target_required(target_required, source_pairs, stack, context) do
    map_reduce_ok(target_required, context, fn {target_key, target_value}, context ->
      Enum.find_value(source_pairs, fn {source_kind, source_key, source_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, {:required, key, value}, context}

            {:error, _reason} ->
              source_map = {:map, [{source_kind, source_key, source_value}]}
              target_map = {:map, [{:required, target_key, target_value}]}
              error({:unable_unify, source_map, target_map}, stack, context)
          end
        else
          {:error, _reason} -> nil
        end
      end) || {:error, :unify}
    end)
  end

  defp unify_source_optional(source_optional, target_optional, stack, context) do
    flat_map_reduce_ok(source_optional, context, fn {source_key, source_value}, context ->
      Enum.find_value(target_optional, fn {target_key, target_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, [{:optional, key, value}], context}

            {:error, _reason} ->
              source_map = {:map, [{:optional, source_key, source_value}]}
              target_map = {:map, [{:optional, target_key, target_value}]}
              error({:unable_unify, source_map, target_map}, stack, context)
          end
        else
          _ -> nil
        end
      end) || {:ok, [], context}
    end)
  end

  defp unify_target_optional(target_optional, source_optional, stack, context) do
    flat_map_reduce_ok(target_optional, context, fn {target_key, target_value}, context ->
      Enum.find_value(source_optional, fn {source_key, source_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, [{:optional, key, value}], context}

            {:error, _reason} ->
              source_map = {:map, [{:optional, source_key, source_value}]}
              target_map = {:map, [{:optional, target_key, target_value}]}
              error({:unable_unify, source_map, target_map}, stack, context)
          end
        else
          _ -> nil
        end
      end) || {:ok, [], context}
    end)
  end

  defp split_pairs(pairs) do
    {required, optional} =
      Enum.split_with(pairs, fn {kind, _key, _value} -> kind == :required end)

    required = Enum.map(required, fn {_kind, key, value} -> {key, value} end)
    optional = Enum.map(optional, fn {_kind, key, value} -> {key, value} end)
    {required, optional}
  end

  @doc """
  Adds a variable to the typing context and returns its type variables.
  If the variable has already been added, return the existing type variable.
  """
  def new_var(var, context) do
    case Map.fetch(context.vars, var_name(var)) do
      {:ok, type} ->
        {type, context}

      :error ->
        type = {:var, context.counter}
        vars = Map.put(context.vars, var_name(var), type)
        types_to_vars = Map.put(context.types_to_vars, context.counter, var)
        types = Map.put(context.types, context.counter, :unbound)
        traces = Map.put(context.traces, context.counter, [])

        context = %{
          context
          | vars: vars,
            types_to_vars: types_to_vars,
            types: types,
            traces: traces,
            counter: context.counter + 1
        }

        {type, context}
    end
  end

  def add_var(context) do
    type = {:var, context.counter}
    types = Map.put(context.types, context.counter, :unbound)
    traces = Map.put(context.traces, context.counter, [])

    context = %{
      context
      | types: types,
        traces: traces,
        counter: context.counter + 1
    }

    {type, context}
  end

  # Check unify stack to see if variable was already expanded
  defp variable_expanded?(var, stack, context) do
    Enum.any?(stack.unify_stack, &variable_same?(var, &1, context))
  end

  defp variable_same?(left, right, context) do
    case Map.fetch(context.types, left) do
      {:ok, {:var, new_left}} ->
        variable_same?(new_left, right, context)

      _ ->
        case Map.fetch(context.types, right) do
          {:ok, {:var, new_right}} -> variable_same?(left, new_right, context)
          _ -> false
        end
    end
  end

  defp push_unify_stack(var, stack) do
    %{stack | unify_stack: [var | stack.unify_stack]}
  end

  @doc """
  Set the type for a variable and add trace.
  """
  def refine_var(var, type, stack, context) do
    types = Map.put(context.types, var, type)
    context = %{context | types: types}
    trace_var(var, type, stack, context)
  end

  @doc """
  Remove type variable and all its traces.
  """
  def remove_var(var, context) do
    types = Map.delete(context.types, var)
    traces = Map.delete(context.traces, var)
    %{context | types: types, traces: traces}
  end

  defp trace_var(var, type, %{trace: true, last_expr: last_expr} = _stack, context) do
    line = get_meta(last_expr)[:line]
    trace = {type, last_expr, {context.file, line}}
    traces = Map.update!(context.traces, var, &[trace | &1])
    %{context | traces: traces}
  end

  defp trace_var(_var, _type, %{trace: false} = _stack, context) do
    context
  end

  # Check if a variable is recursive and incompatible with itself
  # Bad: `{var} = var`
  # Good: `x = y; y = z; z = x`
  defp recursive_type?({:var, var} = parent, parents, context) do
    case Map.fetch!(context.types, var) do
      :unbound ->
        false

      type ->
        if type in parents do
          not Enum.all?(parents, &match?({:var, _}, &1))
        else
          recursive_type?(type, [parent | parents], context)
        end
    end
  end

  defp recursive_type?({:list, type} = parent, parents, context) do
    recursive_type?(type, [parent | parents], context)
  end

  defp recursive_type?({:tuple, types} = parent, parents, context) do
    Enum.any?(types, &recursive_type?(&1, [parent | parents], context))
  end

  defp recursive_type?({:map, pairs} = parent, parents, context) do
    Enum.any?(pairs, fn {_kind, key, value} ->
      recursive_type?(key, [parent | parents], context) or
        recursive_type?(value, [parent | parents], context)
    end)
  end

  defp recursive_type?(_other, _parents, _context) do
    false
  end

  @doc """
  Checks if the first argument is a subtype of the second argument.
  Only checks for simple and concrete types.
  """
  # TODO: boolean <: false | true
  # TODO: number <: float | integer
  def subtype?({:atom, boolean}, :boolean, _context) when is_boolean(boolean), do: true
  def subtype?({:atom, atom}, :atom, _context) when is_atom(atom), do: true
  def subtype?(:boolean, :atom, _context), do: true
  def subtype?(:float, :number, _context), do: true
  def subtype?(:integer, :number, _context), do: true
  def subtype?({:tuple, _}, :tuple, _context), do: true

  # TODO: Lift unions to unify/3?
  def subtype?({:union, left_types}, {:union, right_types} = right_union, context) do
    # Since we can't unify unions we give up when encountering variables
    Enum.any?(left_types ++ right_types, &match?({:var, _}, &1)) or
      Enum.all?(left_types, &subtype?(&1, right_union, context))
  end

  def subtype?(left, {:union, right_types}, context) do
    Enum.any?(right_types, &subtype?(left, &1, context))
  end

  def subtype?(left, right, _context), do: left == right

  @doc """
  Returns a "simplified" union using `subtype?/3` to remove redundant types.

  Due to limitations in `subtype?/3` some overlapping types may still be
  included. For example unions with overlapping non-concrete types such as
  `{boolean()} | {atom()}` will not be merged or types with variables that
  are distinct but equivalent such as `a | b when a ~ b`.
  """
  # TODO: Translate union of all top types to dynamic()
  def to_union(types, context) when types != [] do
    flat_types = flatten_union(types)

    if :dynamic in flat_types do
      :dynamic
    else
      case unique_super_types(flat_types, context) do
        [type] -> type
        types -> {:union, types}
      end
    end
  end

  defp flatten_union(types) do
    Enum.flat_map(types, fn
      {:union, types} -> flatten_union(types)
      type -> [type]
    end)
  end

  # Filter subtypes
  # `boolean() | atom()` => `atom()`
  # `:foo | atom()` => `atom()`
  # Does not unify `true | false` => `boolean()`
  defp unique_super_types([type | types], context) do
    types = Enum.reject(types, &subtype?(&1, type, context))

    if Enum.any?(types, &subtype?(type, &1, context)) do
      unique_super_types(types, context)
    else
      [type | unique_super_types(types, context)]
    end
  end

  defp unique_super_types([], _context) do
    []
  end

  def unify_kinds(:required, _), do: :required
  def unify_kinds(_, :required), do: :required
  def unify_kinds(:optional, :optional), do: :optional

  # Collect relevant information from context and traces to report error
  # TODO: We should do this lazily since in some cases unification will error
  #       but we continue attempting unifying other types
  defp error({:unable_unify, left, right}, stack, context) do
    {fun, arity} = context.function
    line = get_meta(stack.last_expr)[:line]
    location = {context.file, line, {context.module, fun, arity}}

    traces = type_traces(stack, context)
    traces = tag_traces(traces, context)

    error = {:unable_unify, left, right, {location, stack.last_expr, traces}}
    context = update_in(context.warnings, &[{Module.Types, error, location} | &1])
    {:error, context}
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
      case type do
        {:var, var_index} ->
          var2 = Map.fetch!(context.types_to_vars, var_index)
          [{var, {:var, var2, expr, location}}]

        _ ->
          [{var, {:type, type, expr, location}}]
      end
    end)
  end

  # TODO: We should check if structs have keys that do not belong to them.
  #       This might not be the best place to do it since it will only be
  #       called if the type is unified. A post-pass walking over all
  #       inferred types might be better.
  defp expand_struct(pairs) do
    case fetch_struct_pair(pairs) do
      {:ok, module} ->
        struct_pairs =
          Enum.flat_map(Map.from_struct(module.__struct__()), fn {key, _value} ->
            if List.keyfind(pairs, {:atom, key}, 1) do
              []
            else
              [{:required, {:atom, key}, :dynamic}]
            end
          end)

        pairs ++ struct_pairs

      :error ->
        pairs
    end
  end

  defp simplify_struct(pairs) do
    case fetch_struct_pair(pairs) do
      {:ok, module} ->
        Enum.reduce(Map.from_struct(module.__struct__()), pairs, fn {key, _value}, pairs ->
          case List.keyfind(pairs, {:atom, key}, 1) do
            {_, _key, :dynamic} -> List.keydelete(pairs, {:atom, key}, 1)
            _ -> pairs
          end
        end)

      :error ->
        pairs
    end
  end

  # TODO: Resolve type variables if %{__struct__: var, ...}
  defp fetch_struct_pair(pairs) do
    case Enum.find(pairs, &match?({:required, {:atom, :__struct__}, {:atom, _}}, &1)) do
      {:required, {:atom, :__struct__}, {:atom, module}} -> {:ok, module}
      nil -> :error
    end
  end
end
