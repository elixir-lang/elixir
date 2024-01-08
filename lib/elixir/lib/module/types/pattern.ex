defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.Of
  import Module.Types.{Helpers, Descr}

  @doc """
  Handles patterns and guards at once.
  """
  def of_head(patterns, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(patterns, context, &of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/4 returns boolean() | :fail
         {:ok, _, context} <- of_guards(guards, term(), stack, context),
         do: {:ok, types, context}
  end

  @doc """
  Return the type and typing context of a pattern expression or an error
  in case of a typing conflict.
  """
  def of_pattern(expr, stack, context) do
    of_pattern(expr, dynamic(), stack, context)
  end

  # ^var
  defp of_pattern({:^, _meta, [var]}, _expected, _stack, context) do
    {:ok, fetch_var!(var, context), context}
  end

  # left = right
  defp of_pattern({:=, _meta, [left_expr, right_expr]}, expected, stack, context) do
    with {:ok, _, context} <- of_pattern(left_expr, expected, stack, context),
         {:ok, _, context} <- of_pattern(right_expr, expected, stack, context),
         do: {:ok, dynamic(), context}
  end

  # %_{...}
  defp of_pattern(
         {:%, _meta1, [{:_, _meta2, var_context}, {:%{}, _meta3, args}]},
         _expected,
         stack,
         context
       )
       when is_atom(var_context) do
    with {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %var{...} and %^var{...}
  defp of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]}, _expected, stack, context)
       when not is_atom(var) do
    # TODO: validate var is an atom
    with {:ok, _, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %Struct{...}
  defp of_pattern({:%, meta1, [module, {:%{}, _meta2, args}]}, _expected, stack, context)
       when is_atom(module) do
    with {:ok, _, context} <- Of.struct(module, meta1, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %{...}
  defp of_pattern({:%{}, _meta, args}, _expected, stack, context) do
    Of.open_map(args, stack, context, &of_pattern/3)
  end

  # <<...>>>
  defp of_pattern({:<<>>, _meta, args}, _expected, stack, context) do
    case Of.binary(args, :pattern, stack, context, &of_pattern/4) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # var or _
  defp of_pattern(var, _expected, _stack, context) when is_var(var) do
    {_version, type, context} = new_var(var, dynamic(), context)
    {:ok, type, context}
  end

  defp of_pattern(expr, expected, stack, context) do
    of_shared(expr, expected, stack, context, &of_pattern/4)
  end

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  # TODO: Function calls, maps, and binary handling are not in of_shared
  # and must be implemented here. There is a question of how much of
  # of_shared can also be shared with of_expr, but still unclear.
  def of_guards(_expr, _expected, _stack, context) do
    {:ok, dynamic(), context}
  end

  ## Shared

  # :atom
  defp of_shared(atom, _expected, _stack, context, _fun) when is_atom(atom) do
    {:ok, atom(atom), context}
  end

  # 12
  defp of_shared(literal, _expected, _stack, context, _fun) when is_integer(literal) do
    {:ok, integer(), context}
  end

  # 1.2
  defp of_shared(literal, _expected, _stack, context, _fun) when is_float(literal) do
    {:ok, float(), context}
  end

  # "..."
  defp of_shared(literal, _expected, _stack, context, _fun) when is_binary(literal) do
    {:ok, binary(), context}
  end

  # left | []
  defp of_shared({:|, _meta, [left_expr, []]}, _expected, stack, context, fun) do
    fun.(left_expr, dynamic(), stack, context)
  end

  # left | right
  defp of_shared({:|, _meta, [left_expr, right_expr]}, _expected, stack, context, fun) do
    case fun.(left_expr, dynamic(), stack, context) do
      {:ok, _, context} ->
        fun.(right_expr, dynamic(), stack, context)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # []
  defp of_shared([], _expected, _stack, context, _fun) do
    {:ok, empty_list(), context}
  end

  # [expr, ...]
  defp of_shared(exprs, _expected, stack, context, fun) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &fun.(&1, dynamic(), stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left ++ right
  defp of_shared(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]},
         _expected,
         stack,
         context,
         fun
       ) do
    # The left side is always a list
    with {:ok, _, context} <- fun.(left_expr, dynamic(), stack, context),
         {:ok, _, context} <- fun.(right_expr, dynamic(), stack, context) do
      # TODO: Both lists can be empty, so this may be an empty list,
      # so we return dynamic() for now.
      {:ok, dynamic(), context}
    end
  end

  # {left, right}
  defp of_shared({left, right}, expected, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, expected, stack, context, fun)
  end

  # {...}
  defp of_shared({:{}, _meta, exprs}, _expected, stack, context, fun) do
    case map_reduce_ok(exprs, context, &fun.(&1, dynamic(), stack, &2)) do
      {:ok, _, context} -> {:ok, tuple(), context}
      {:error, reason} -> {:error, reason}
    end
  end
end
