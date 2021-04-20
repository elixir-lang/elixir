defmodule Module.Types.Unify do
  @moduledoc false

  import Module.Types.Helpers

  # Those are the simple types known to the system:
  #
  #   :dynamic
  #   {:var, var}
  #   {:atom, atom} < :atom
  #   :integer
  #   :float
  #   :binary
  #   :pid
  #   :port
  #   :reference
  #
  # Those are the composite types:
  #
  #   {:list, type}
  #   {:tuple, size, [type]} < :tuple
  #   {:union, [type]}
  #   {:map, [{:required | :optional, key_type, value_type}]}
  #   {:fun, [{params, return}]}
  #
  # Once new types are added, they should be considered in:
  #
  #   * unify (all)
  #   * format_type (all)
  #   * subtype? (subtypes only)
  #   * recursive_type? (composite only)
  #   * collect_var_indexes (composite only)
  #   * lift_types (composite only)
  #   * flatten_union (composite only)
  #   * walk (composite only)
  #

  @doc """
  Unifies two types and returns the unified type and an updated typing context
  or an error in case of a typing conflict.
  """
  def unify(same, same, _stack, context) do
    {:ok, same, context}
  end

  def unify(type, {:var, var}, stack, context) do
    unify_var(var, type, stack, context, _var_source = false)
  end

  def unify({:var, var}, type, stack, context) do
    unify_var(var, type, stack, context, _var_source = true)
  end

  def unify({:tuple, n, sources}, {:tuple, n, targets}, stack, context) do
    result =
      map_reduce_ok(Enum.zip(sources, targets), context, fn {source, target}, context ->
        unify(source, target, stack, context)
      end)

    case result do
      {:ok, types, context} -> {:ok, {:tuple, n, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def unify({:list, source}, {:list, target}, stack, context) do
    case unify(source, target, stack, context) do
      {:ok, type, context} -> {:ok, {:list, type}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def unify({:map, source_pairs}, {:map, target_pairs}, stack, context) do
    unify_maps(source_pairs, target_pairs, stack, context)
  end

  def unify(source, :dynamic, _stack, context) do
    {:ok, source, context}
  end

  def unify(:dynamic, target, _stack, context) do
    {:ok, target, context}
  end

  def unify({:union, types}, target, stack, context) do
    unify_result =
      map_reduce_ok(types, context, fn type, context ->
        unify(type, target, stack, context)
      end)

    case unify_result do
      {:ok, types, context} -> {:ok, to_union(types, context), context}
      {:error, context} -> {:error, context}
    end
  end

  def unify(source, target, stack, context) do
    cond do
      # TODO: This condition exists to handle unions with unbound vars.
      match?({:union, _}, target) and has_unbound_var?(target, context) ->
        {:ok, source, context}

      subtype?(source, target, context) ->
        {:ok, source, context}

      true ->
        error(:unable_unify, {source, target, stack}, context)
    end
  end

  def unify_var(var, :dynamic, _stack, context, _var_source?) do
    {:ok, {:var, var}, context}
  end

  def unify_var(var, type, stack, context, var_source?) do
    case context.types do
      %{^var => :unbound} ->
        context = refine_var!(var, type, stack, context)
        stack = push_unify_stack(var, stack)

        if recursive_type?(type, [], context) do
          if var_source? do
            error(:unable_unify, {{:var, var}, type, stack}, context)
          else
            error(:unable_unify, {type, {:var, var}, stack}, context)
          end
        else
          {:ok, {:var, var}, context}
        end

      %{^var => {:var, new_var} = var_type} ->
        unify_result =
          if var_source? do
            unify(var_type, type, stack, context)
          else
            unify(type, var_type, stack, context)
          end

        case unify_result do
          {:ok, type, context} ->
            {:ok, type, context}

          {:error, {type, reason, %{traces: error_traces} = error_context}} ->
            old_var_traces = Map.get(context.traces, new_var, [])
            new_var_traces = Map.get(error_traces, new_var, [])
            add_var_traces = Enum.drop(new_var_traces, -length(old_var_traces))

            error_traces =
              error_traces
              |> Map.update(var, add_var_traces, &(add_var_traces ++ &1))
              |> Map.put(new_var, old_var_traces)

            {:error, {type, reason, %{error_context | traces: error_traces}}}
        end

      %{^var => var_type} ->
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
          {:ok, {:var, ^var}, context} ->
            {:ok, {:var, var}, context}

          {:ok, res_type, context} ->
            context = refine_var!(var, res_type, stack, context)
            {:ok, {:var, var}, context}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  # * All required keys on each side need to match to the other side.
  # * All optional keys on each side that do not match must be discarded.

  def unify_maps(source_pairs, target_pairs, stack, context) do
    {source_required, source_optional} = split_pairs(source_pairs)
    {target_required, target_optional} = split_pairs(target_pairs)

    with {:ok, source_required_pairs, context} <-
           unify_source_required(source_required, target_pairs, stack, context),
         {:ok, target_required_pairs, context} <-
           unify_target_required(target_required, source_pairs, stack, context),
         {:ok, source_optional_pairs, context} <-
           unify_source_optional(source_optional, target_optional, stack, context),
         {:ok, target_optional_pairs, context} <-
           unify_target_optional(target_optional, source_optional, stack, context) do
      # Remove duplicate pairs from matching in both left and right directions
      pairs =
        Enum.uniq(
          source_required_pairs ++
            target_required_pairs ++
            source_optional_pairs ++
            target_optional_pairs
        )

      {:ok, {:map, pairs}, context}
    else
      {:error, :unify} ->
        error(:unable_unify, {{:map, source_pairs}, {:map, target_pairs}, stack}, context)

      {:error, context} ->
        {:error, context}
    end
  end

  def unify_source_required(source_required, target_pairs, stack, context) do
    map_reduce_ok(source_required, context, fn {source_key, source_value}, context ->
      Enum.find_value(target_pairs, fn {target_kind, target_key, target_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, {:required, key, value}, context}

            {:error, _reason} ->
              source_map = {:map, [{:required, source_key, source_value}]}
              target_map = {:map, [{target_kind, target_key, target_value}]}
              error(:unable_unify, {source_map, target_map, stack}, context)
          end
        else
          {:error, _reason} -> nil
        end
      end) || {:error, :unify}
    end)
  end

  def unify_target_required(target_required, source_pairs, stack, context) do
    map_reduce_ok(target_required, context, fn {target_key, target_value}, context ->
      Enum.find_value(source_pairs, fn {source_kind, source_key, source_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, {:required, key, value}, context}

            {:error, _reason} ->
              source_map = {:map, [{source_kind, source_key, source_value}]}
              target_map = {:map, [{:required, target_key, target_value}]}
              error(:unable_unify, {source_map, target_map, stack}, context)
          end
        else
          {:error, _reason} -> nil
        end
      end) || {:error, :unify}
    end)
  end

  def unify_source_optional(source_optional, target_optional, stack, context) do
    flat_map_reduce_ok(source_optional, context, fn {source_key, source_value}, context ->
      Enum.find_value(target_optional, fn {target_key, target_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, [{:optional, key, value}], context}

            {:error, _reason} ->
              source_map = {:map, [{:optional, source_key, source_value}]}
              target_map = {:map, [{:optional, target_key, target_value}]}
              error(:unable_unify, {source_map, target_map, stack}, context)
          end
        else
          _ -> nil
        end
      end) || {:ok, [], context}
    end)
  end

  def unify_target_optional(target_optional, source_optional, stack, context) do
    flat_map_reduce_ok(target_optional, context, fn {target_key, target_value}, context ->
      Enum.find_value(source_optional, fn {source_key, source_value} ->
        with {:ok, key, context} <- unify(source_key, target_key, stack, context) do
          case unify(source_value, target_value, stack, context) do
            {:ok, value, context} ->
              {:ok, [{:optional, key, value}], context}

            {:error, _reason} ->
              source_map = {:map, [{:optional, source_key, source_value}]}
              target_map = {:map, [{:optional, target_key, target_value}]}
              error(:unable_unify, {source_map, target_map, stack}, context)
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

  def error(type, reason, context), do: {:error, {type, reason, context}}

  @doc """
  Push expression to stack.

  The expression stack is used to give the context where a type variable
  was refined when show a type conflict error.
  """
  def push_expr_stack(expr, stack) do
    %{stack | last_expr: expr}
  end

  @doc """
  Gets a variable.
  """
  def get_var!(var, context) do
    Map.fetch!(context.vars, var_name(var))
  end

  @doc """
  Adds a variable to the typing context and returns its type variable.
  If the variable has already been added, return the existing type variable.
  """
  def new_var(var, context) do
    var_name = var_name(var)

    case context.vars do
      %{^var_name => type} ->
        {type, context}

      %{} ->
        type = {:var, context.counter}
        vars = Map.put(context.vars, var_name, type)
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

  @doc """
  Adds an internal variable to the typing context and returns its type variable.
  An internal variable is used to help unify complex expressions,
  it does not belong to a specific AST expression.
  """
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

  @doc """
  Resolves a variable raising if it is unbound.
  """
  def resolve_var({:var, var}, context) do
    case context.types do
      %{^var => :unbound} -> raise "cannot resolve unbound var"
      %{^var => type} -> resolve_var(type, context)
    end
  end

  def resolve_var(other, _context), do: other

  # Check unify stack to see if variable was already expanded
  defp variable_expanded?(var, stack, context) do
    Enum.any?(stack.unify_stack, &variable_same?(var, &1, context))
  end

  defp variable_same?(left, right, context) do
    case context.types do
      %{^left => {:var, new_left}} ->
        variable_same?(new_left, right, context)

      %{^right => {:var, new_right}} ->
        variable_same?(left, new_right, context)

      %{} ->
        false
    end
  end

  defp push_unify_stack(var, stack) do
    %{stack | unify_stack: [var | stack.unify_stack]}
  end

  @doc """
  Restores the variable information from the old context into new context.
  """
  def restore_var!(var, new_context, old_context) do
    %{^var => type} = old_context.types
    %{^var => trace} = old_context.traces
    types = Map.put(new_context.types, var, type)
    traces = Map.put(new_context.traces, var, trace)
    %{new_context | types: types, traces: traces}
  end

  @doc """
  Set the type for a variable and add trace.
  """
  def refine_var!(var, type, stack, context) do
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
    case context.types do
      %{^var => :unbound} ->
        false

      %{^var => type} ->
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

  defp recursive_type?({:union, types} = parent, parents, context) do
    Enum.any?(types, &recursive_type?(&1, [parent | parents], context))
  end

  defp recursive_type?({:tuple, _, types} = parent, parents, context) do
    Enum.any?(types, &recursive_type?(&1, [parent | parents], context))
  end

  defp recursive_type?({:map, pairs} = parent, parents, context) do
    Enum.any?(pairs, fn {_kind, key, value} ->
      recursive_type?(key, [parent | parents], context) or
        recursive_type?(value, [parent | parents], context)
    end)
  end

  defp recursive_type?({:fun, clauses}, parents, context) do
    Enum.any?(clauses, fn {args, return} ->
      Enum.any?([return | args], &recursive_type?(&1, [clauses | parents], context))
    end)
  end

  defp recursive_type?(_other, _parents, _context) do
    false
  end

  @doc """
  Collects all type vars recursively.
  """
  def collect_var_indexes(type, context, acc \\ %{}) do
    {_type, indexes} =
      walk(type, acc, fn
        {:var, var}, acc ->
          case acc do
            %{^var => _} ->
              {{:var, var}, acc}

            %{} ->
              case context.types do
                %{^var => :unbound} ->
                  {{:var, var}, Map.put(acc, var, true)}

                %{^var => type} ->
                  {{:var, var}, collect_var_indexes(type, context, Map.put(acc, var, true))}
              end
          end

        other, acc ->
          {other, acc}
      end)

    indexes
  end

  @doc """
  Checks if the type has a type var.
  """
  def has_unbound_var?(type, context) do
    walk(type, :ok, fn
      {:var, var}, acc ->
        case context.types do
          %{^var => :unbound} ->
            throw(:has_unbound_var?)

          %{^var => type} ->
            has_unbound_var?(type, context)
            {{:var, var}, acc}
        end

      other, acc ->
        {other, acc}
    end)

    false
  catch
    :throw, :has_unbound_var? -> true
  end

  @doc """
  Returns true if it is a singleton type.

  Only atoms are singleton types. Unbound vars are not
  considered singleton types.
  """
  def singleton?({:var, var}, context) do
    case context.types do
      %{^var => :unbound} -> false
      %{^var => type} -> singleton?(type, context)
    end
  end

  def singleton?({:atom, _}, _context), do: true
  def singleton?(_type, _context), do: false

  @doc """
  Checks if the first argument is a subtype of the second argument.

  This function assumes that:

    * unbound variables are not subtype of anything

    * dynamic is not considered a subtype of all other types but the top type.
      This allows this function can be used for ordering, in other cases, you
      may need to check for both sides

  """
  def subtype?(type, type, _context), do: true

  def subtype?({:var, var}, other, context) do
    case context.types do
      %{^var => :unbound} -> false
      %{^var => type} -> subtype?(type, other, context)
    end
  end

  def subtype?(other, {:var, var}, context) do
    case context.types do
      %{^var => :unbound} -> false
      %{^var => type} -> subtype?(other, type, context)
    end
  end

  def subtype?(_, :dynamic, _context), do: true
  def subtype?({:atom, atom}, :atom, _context) when is_atom(atom), do: true

  # Composite

  def subtype?({:tuple, _, _}, :tuple, _context), do: true

  def subtype?({:tuple, n, left_types}, {:tuple, n, right_types}, context) do
    left_types
    |> Enum.zip(right_types)
    |> Enum.all?(fn {left, right} -> subtype?(left, right, context) end)
  end

  def subtype?({:map, left_pairs}, {:map, right_pairs}, context) do
    Enum.all?(left_pairs, fn
      {:required, left_key, left_value} ->
        Enum.any?(right_pairs, fn {_, right_key, right_value} ->
          subtype?(left_key, right_key, context) and subtype?(left_value, right_value, context)
        end)

      {:optional, _, _} ->
        true
    end)
  end

  def subtype?({:list, left}, {:list, right}, context) do
    subtype?(left, right, context)
  end

  def subtype?({:union, left_types}, {:union, _} = right_union, context) do
    Enum.all?(left_types, &subtype?(&1, right_union, context))
  end

  def subtype?(left, {:union, right_types}, context) do
    Enum.any?(right_types, &subtype?(left, &1, context))
  end

  def subtype?({:union, left_types}, right, context) do
    Enum.all?(left_types, &subtype?(&1, right, context))
  end

  def subtype?(_left, _right, _context), do: false

  @doc """
  Returns a "simplified" union using `subtype?/3` to remove redundant types.

  Due to limitations in `subtype?/3` some overlapping types may still be
  included. For example unions with overlapping non-concrete types such as
  `{boolean()} | {atom()}` will not be merged or types with variables that
  are distinct but equivalent such as `a | b when a ~ b`.
  """
  def to_union([type], _context), do: type

  def to_union(types, context) when types != [] do
    case unique_super_types(unnest_unions(types), context) do
      [type] -> type
      types -> {:union, types}
    end
  end

  defp unnest_unions(types) do
    Enum.flat_map(types, fn
      {:union, types} -> unnest_unions(types)
      type -> [type]
    end)
  end

  # Filter subtypes
  #
  # `boolean() | atom()` => `atom()`
  # `:foo | atom()` => `atom()`
  #
  # Does not merge `true | false` => `boolean()`
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

  ## Type lifting

  @doc """
  Lifts type variables to their inferred types from the context.
  """
  def lift_types(types, %{lifted_types: _} = context) do
    Enum.map_reduce(types, context, &lift_type/2)
  end

  def lift_types(types, context) do
    context = %{
      types: context.types,
      lifted_types: %{},
      lifted_counter: 0
    }

    Enum.map_reduce(types, context, &lift_type/2)
  end

  # Lift type variable to its inferred (hopefully concrete) types from the context
  defp lift_type({:var, var}, context) do
    case context.lifted_types do
      %{^var => lifted_var} ->
        {{:var, lifted_var}, context}

      %{} ->
        case context.types do
          %{^var => :unbound} ->
            new_lifted_var(var, context)

          %{^var => type} ->
            if recursive_type?(type, [], context) do
              new_lifted_var(var, context)
            else
              # Remove visited types to avoid infinite loops
              # then restore after we are done recursing on vars
              types = context.types
              context = put_in(context.types[var], :unbound)
              {type, context} = lift_type(type, context)
              {type, %{context | types: types}}
            end

          %{} ->
            new_lifted_var(var, context)
        end
    end
  end

  defp lift_type({:union, types}, context) do
    {types, context} = Enum.map_reduce(types, context, &lift_type/2)
    {{:union, types}, context}
  end

  defp lift_type({:tuple, n, types}, context) do
    {types, context} = Enum.map_reduce(types, context, &lift_type/2)
    {{:tuple, n, types}, context}
  end

  defp lift_type({:map, pairs}, context) do
    {pairs, context} =
      Enum.map_reduce(pairs, context, fn {kind, key, value}, context ->
        {key, context} = lift_type(key, context)
        {value, context} = lift_type(value, context)
        {{kind, key, value}, context}
      end)

    {{:map, pairs}, context}
  end

  defp lift_type({:list, type}, context) do
    {type, context} = lift_type(type, context)
    {{:list, type}, context}
  end

  defp lift_type({:fun, clauses}, context) do
    clauses =
      Enum.map_reduce(clauses, context, fn {args, return}, context ->
        {[return | args], context} = Enum.map_reduce([return | args], context, &lift_type/2)
        {{args, return}, context}
      end)

    {{:fun, clauses}, context}
  end

  defp lift_type(other, context) do
    {other, context}
  end

  defp new_lifted_var(original_var, context) do
    types = Map.put(context.lifted_types, original_var, context.lifted_counter)
    counter = context.lifted_counter + 1

    type = {:var, context.lifted_counter}
    context = %{context | lifted_types: types, lifted_counter: counter}
    {type, context}
  end

  # TODO: Figure out function expansion
  # TODO: Flatten across type variables

  @doc """
  Expand unions so that all unions are at the top level.

      {integer() | float()} => {integer()} | {float()}
  """
  def flatten_union({:union, types}) do
    Enum.flat_map(types, &flatten_union/1)
  end

  def flatten_union(type) do
    List.wrap(do_flatten_union(type))
  end

  def do_flatten_union({:tuple, num, types}) do
    flatten_union_tuple(types, num, [])
  end

  def do_flatten_union({:list, type}) do
    case do_flatten_union(type) do
      {:union, union_types} -> Enum.map(union_types, &{:list, &1})
      _type -> [{:list, type}]
    end
  end

  def do_flatten_union({:map, pairs}) do
    flatten_union_map(pairs, [])
  end

  def do_flatten_union(type) do
    type
  end

  defp flatten_union_tuple([type | types], num, acc) do
    case do_flatten_union(type) do
      {:union, union_types} ->
        Enum.flat_map(union_types, &flatten_union_tuple(types, num, [&1 | acc]))

      type ->
        flatten_union_tuple(types, num, [type | acc])
    end
  end

  defp flatten_union_tuple([], num, acc) do
    [{:tuple, num, Enum.reverse(acc)}]
  end

  defp flatten_union_map([{kind, key, value} | pairs], acc) do
    case do_flatten_union(key) do
      {:union, union_types} ->
        Enum.flat_map(union_types, &flatten_union_map_value(kind, &1, value, pairs, acc))

      type ->
        flatten_union_map_value(kind, type, value, pairs, acc)
    end
  end

  defp flatten_union_map([], acc) do
    [{:map, Enum.reverse(acc)}]
  end

  defp flatten_union_map_value(kind, key, value, pairs, acc) do
    case do_flatten_union(value) do
      {:union, union_types} ->
        Enum.flat_map(union_types, &flatten_union_map(pairs, [{kind, key, &1} | acc]))

      value ->
        flatten_union_map(pairs, [{kind, key, value} | acc])
    end
  end

  @doc """
  Formats types.

  The second argument says when complex types such as maps and
  structs should be simplified and not shown.
  """
  def format_type({:map, pairs}, true) do
    case List.keyfind(pairs, {:atom, :__struct__}, 1) do
      {:required, {:atom, :__struct__}, {:atom, struct}} ->
        ["%", inspect(struct), "{}"]

      _ ->
        "map()"
    end
  end

  def format_type({:union, types}, simplify?) do
    types
    |> Enum.map(&format_type(&1, simplify?))
    |> Enum.intersperse(" | ")
  end

  def format_type({:tuple, _, types}, simplify?) do
    format =
      types
      |> Enum.map(&format_type(&1, simplify?))
      |> Enum.intersperse(", ")

    ["{", format, "}"]
  end

  def format_type({:list, type}, simplify?) do
    ["[", format_type(type, simplify?), "]"]
  end

  def format_type({:map, pairs}, false) do
    case List.keytake(pairs, {:atom, :__struct__}, 1) do
      {{:required, {:atom, :__struct__}, {:atom, struct}}, pairs} ->
        ["%", inspect(struct), "{", format_map_pairs(pairs), "}"]

      _ ->
        ["%{", format_map_pairs(pairs), "}"]
    end
  end

  def format_type({:atom, literal}, _simplify?) do
    inspect(literal)
  end

  def format_type({:var, index}, _simplify?) do
    ["var", Integer.to_string(index + 1)]
  end

  def format_type({:fun, clauses}, simplify?) do
    format =
      Enum.map(clauses, fn {params, return} ->
        params = Enum.intersperse(Enum.map(params, &format_type(&1, simplify?)), ", ")
        params = if params == [], do: params, else: [params, " "]
        return = format_type(return, simplify?)
        [params, "-> ", return]
      end)

    ["(", Enum.intersperse(format, "; "), ")"]
  end

  def format_type(atom, _simplify?) when is_atom(atom) do
    [Atom.to_string(atom), "()"]
  end

  defp format_map_pairs(pairs) do
    {atoms, others} = Enum.split_with(pairs, &match?({:required, {:atom, _}, _}, &1))
    {required, optional} = Enum.split_with(others, &match?({:required, _, _}, &1))

    (atoms ++ required ++ optional)
    |> Enum.map(fn
      {:required, {:atom, atom}, right} ->
        [Atom.to_string(atom), ": ", format_type(right, false)]

      {:required, left, right} ->
        [format_type(left, false), " => ", format_type(right, false)]

      {:optional, left, right} ->
        ["optional(", format_type(left, false), ") => ", format_type(right, false)]
    end)
    |> Enum.intersperse(", ")
  end

  @doc """
  Performs a depth-first, pre-order traversal of the type tree using an accumulator.
  """
  def walk({:map, pairs}, acc, fun) do
    {pairs, acc} =
      Enum.map_reduce(pairs, acc, fn {kind, key, value}, acc ->
        {key, acc} = walk(key, acc, fun)
        {value, acc} = walk(value, acc, fun)
        {{kind, key, value}, acc}
      end)

    fun.({:map, pairs}, acc)
  end

  def walk({:union, types}, acc, fun) do
    {types, acc} = Enum.map_reduce(types, acc, &walk(&1, &2, fun))
    fun.({:union, types}, acc)
  end

  def walk({:tuple, num, types}, acc, fun) do
    {types, acc} = Enum.map_reduce(types, acc, &walk(&1, &2, fun))
    fun.({:tuple, num, types}, acc)
  end

  def walk({:list, type}, acc, fun) do
    {type, acc} = walk(type, acc, fun)
    fun.({:list, type}, acc)
  end

  def walk({:fun, clauses}, acc, fun) do
    {clauses, acc} =
      Enum.map_reduce(clauses, acc, fn {params, return}, acc ->
        {params, acc} = Enum.map_reduce(params, acc, &walk(&1, &2, fun))
        {return, acc} = walk(return, acc, fun)
        {{params, return}, acc}
      end)

    fun.({:fun, clauses}, acc)
  end

  def walk(type, acc, fun) do
    fun.(type, acc)
  end
end
