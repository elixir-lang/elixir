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
    unify_var(var, type, stack, context, _var_source = false)
  end

  defp do_unify({:var, var}, type, stack, context) do
    unify_var(var, type, stack, context, _var_source = true)
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
    # Since maps in patterns only support literal keys (excluding maps)
    # we can do exact type match without subtype checking

    unique_right_pairs =
      Enum.reject(target_pairs, fn {key, _value} ->
        :lists.keyfind(key, 1, source_pairs)
      end)

    unique_pairs = source_pairs ++ unique_right_pairs

    # Build union of all unique key-value pairs between the maps
    result =
      map_reduce_ok(unique_pairs, context, fn {source_key, source_value}, context ->
        case :lists.keyfind(source_key, 1, target_pairs) do
          {^source_key, target_value} ->
            case unify(source_value, target_value, stack, context) do
              {:ok, value, context} -> {:ok, {source_key, value}, context}
              {:error, reason} -> {:error, reason}
            end

          false ->
            {:ok, {source_key, source_value}, context}
        end
      end)

    case result do
      {:ok, pairs, context} -> {:ok, {:map, pairs}, context}
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
    Enum.any?(pairs, fn {key, value} ->
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
    if :dynamic in types do
      :dynamic
    else
      case unique_super_types(flatten_union(types), context) do
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

  # Collect relevant information from context and traces to report error
  defp error({:unable_unify, left, right}, stack, context) do
    {fun, arity} = context.function
    line = get_meta(stack.last_expr)[:line]
    location = {context.file, line, {context.module, fun, arity}}

    traces = type_traces(stack, context)
    traces = tag_traces(traces, context)

    {:error, {Module.Types, {:unable_unify, left, right, stack.last_expr, traces}, [location]}}
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
      case Map.fetch(context.traces, var_index) do
        {:ok, traces} ->
          expr_var = Map.fetch!(context.types_to_vars, var_index)
          Enum.map(traces, &{expr_var, &1})

        _other ->
          []
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

  defp get_meta({_fun, meta, _args}) when is_list(meta), do: meta
  defp get_meta(_other), do: []
end
