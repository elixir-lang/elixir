defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.Of
  import Module.Types.{Helpers, Unify}

  @doc """
  Handles patterns and guards at once.
  """
  def of_head(patterns, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(patterns, context, &of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/4 returns boolean() | :fail
         {:ok, _, context} <- of_guard(guards_to_or(guards), :dynamic, stack, context),
         do: {:ok, types, context}
  end

  @doc """
  Return the type and typing context of a pattern expression or an error
  in case of a typing conflict.
  """
  def of_pattern(pattern, %{context: stack_context} = stack, context)
      when stack_context != :pattern do
    of_pattern(pattern, %{stack | context: :pattern}, context)
  end

  # _
  def of_pattern({:_, _meta, atom}, _stack, context) when is_atom(atom) do
    {:ok, :dynamic, context}
  end

  # ^var
  def of_pattern({:^, _meta, [var]}, _stack, context) do
    {:ok, get_var!(var, context), context}
  end

  # var
  def of_pattern(var, _stack, context) when is_var(var) do
    {type, context} = new_var(var, context)
    {:ok, type, context}
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]} = expr, stack, context) do
    stack = push_expr_stack(expr, stack)

    with {:ok, left_type, context} <- of_pattern(left_expr, stack, context),
         {:ok, right_type, context} <- of_pattern(right_expr, stack, context),
         do: unify(left_type, right_type, stack, context)
  end

  # %_{...}
  def of_pattern(
        {:%, _meta1, [{:_, _meta2, var_context}, {:%{}, _meta3, args}]} = expr,
        stack,
        context
      )
      when is_atom(var_context) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> of_pattern(arg, stack, context) end

    with {:ok, {:map, pairs}, context} <- Of.open_map(args, stack, context, expected_fun) do
      {:ok, {:map, [{:required, {:atom, :__struct__}, :atom} | pairs]}, context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, stack, context)
      when not is_atom(var) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> of_pattern(arg, stack, context) end

    with {:ok, var_type, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- unify(var_type, :atom, stack, context),
         {:ok, {:map, pairs}, context} <- Of.open_map(args, stack, context, expected_fun) do
      {:ok, {:map, [{:required, {:atom, :__struct__}, var_type} | pairs]}, context}
    end
  end

  def of_pattern(expr, stack, context) do
    of_shared(expr, stack, context, &of_pattern/3)
  end

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  def of_guard(_expr, _expected, _stack, context) do
    {:ok, :dynamic, context}
  end

  ## Shared

  # :atom
  defp of_shared(atom, _stack, context, _fun) when is_atom(atom) do
    {:ok, {:atom, atom}, context}
  end

  # 12
  defp of_shared(literal, _stack, context, _fun) when is_integer(literal) do
    {:ok, :integer, context}
  end

  # 1.2
  defp of_shared(literal, _stack, context, _fun) when is_float(literal) do
    {:ok, :float, context}
  end

  # "..."
  defp of_shared(literal, _stack, context, _fun) when is_binary(literal) do
    {:ok, :binary, context}
  end

  # <<...>>>
  defp of_shared({:<<>>, _meta, args}, stack, context, fun) do
    expected_fun = fn arg, _expected, stack, context -> fun.(arg, stack, context) end

    case Of.binary(args, stack, context, expected_fun) do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left | []
  defp of_shared({:|, _meta, [left_expr, []]} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)
    fun.(left_expr, stack, context)
  end

  # left | right
  defp of_shared({:|, _meta, [left_expr, right_expr]} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)

    case fun.(left_expr, stack, context) do
      {:ok, left, context} ->
        case fun.(right_expr, stack, context) do
          {:ok, {:list, right}, context} ->
            {:ok, to_union([left, right], context), context}

          {:ok, right, context} ->
            {:ok, to_union([left, right], context), context}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # []
  defp of_shared([], _stack, context, _fun) do
    {:ok, {:list, :dynamic}, context}
  end

  # [expr, ...]
  defp of_shared(exprs, stack, context, fun) when is_list(exprs) do
    stack = push_expr_stack(exprs, stack)

    case map_reduce_ok(exprs, context, &fun.(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:list, to_union(types, context)}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left ++ right
  defp of_shared(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]} = expr,
         stack,
         context,
         fun
       ) do
    stack = push_expr_stack(expr, stack)

    case fun.(left_expr, stack, context) do
      {:ok, {:list, left}, context} ->
        case fun.(right_expr, stack, context) do
          {:ok, {:list, right}, context} ->
            {:ok, {:list, to_union([left, right], context)}, context}

          {:ok, right, context} ->
            {:ok, {:list, to_union([left, right], context)}, context}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # {left, right}
  defp of_shared({left, right}, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, stack, context, fun)
  end

  # {...}
  defp of_shared({:{}, _meta, exprs} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)

    case map_reduce_ok(exprs, context, &fun.(&1, stack, &2)) do
      {:ok, types, context} -> {:ok, {:tuple, length(types), types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %{...}
  defp of_shared({:%{}, _meta, args} = expr, stack, context, fun) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> fun.(arg, stack, context) end
    Of.open_map(args, stack, context, expected_fun)
  end

  # %Struct{...}
  defp of_shared({:%, meta1, [module, {:%{}, _meta2, args}]} = expr, stack, context, fun)
       when is_atom(module) do
    stack = push_expr_stack(expr, stack)
    expected_fun = fn arg, _expected, stack, context -> fun.(arg, stack, context) end

    with {:ok, struct, context} <- Of.struct(module, meta1, context),
         {:ok, map, context} <- Of.open_map(args, stack, context, expected_fun) do
      unify(map, struct, stack, context)
    end
  end
end
