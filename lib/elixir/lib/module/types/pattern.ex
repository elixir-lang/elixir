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
  # ^var
  def of_pattern({:^, _meta, [var]}, _stack, context) do
    {:ok, fetch_var!(var, context), context}
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]}, stack, context) do
    with {:ok, _, context} <- of_pattern(left_expr, stack, context),
         {:ok, _, context} <- of_pattern(right_expr, stack, context),
         do: {:ok, dynamic(), context}
  end

  # %_{...}
  def of_pattern(
        {:%, _meta1, [{:_, _meta2, var_context}, {:%{}, _meta3, args}]},
        stack,
        context
      )
      when is_atom(var_context) do
    with {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]}, stack, context)
      when not is_atom(var) do
    # TODO: validate var is an atom
    with {:ok, _, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # <<...>>>
  def of_pattern({:<<>>, _meta, args}, stack, context) do
    case Of.binary(args, :pattern, stack, context, &of_pattern/3) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # var or _
  def of_pattern(var, _stack, context) when is_var(var) do
    {_version, type, context} = new_var(var, dynamic(), context)
    {:ok, type, context}
  end

  def of_pattern(expr, stack, context) do
    of_shared(expr, stack, context, &of_pattern/3)
  end

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  def of_guards(_expr, _expected, _stack, context) do
    {:ok, dynamic(), context}
  end

  ## Shared

  # :atom
  defp of_shared(atom, _stack, context, _fun) when is_atom(atom) do
    {:ok, atom(atom), context}
  end

  # 12
  defp of_shared(literal, _stack, context, _fun) when is_integer(literal) do
    {:ok, integer(), context}
  end

  # 1.2
  defp of_shared(literal, _stack, context, _fun) when is_float(literal) do
    {:ok, float(), context}
  end

  # "..."
  defp of_shared(literal, _stack, context, _fun) when is_binary(literal) do
    {:ok, binary(), context}
  end

  # left | []
  defp of_shared({:|, _meta, [left_expr, []]}, stack, context, fun) do
    fun.(left_expr, stack, context)
  end

  # left | right
  defp of_shared({:|, _meta, [left_expr, right_expr]}, stack, context, fun) do
    case fun.(left_expr, stack, context) do
      {:ok, _, context} ->
        fun.(right_expr, stack, context)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # []
  defp of_shared([], _stack, context, _fun) do
    {:ok, empty_list(), context}
  end

  # [expr, ...]
  defp of_shared(exprs, stack, context, fun) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &fun.(&1, stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # left ++ right
  defp of_shared(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]},
         stack,
         context,
         fun
       ) do
    # The left side is always a list
    with {:ok, _, context} <- fun.(left_expr, stack, context),
         {:ok, _, context} <- fun.(right_expr, stack, context) do
      # TODO: Both lists can be empty, so this may be an empty list,
      # so we return dynamic() for now.
      {:ok, dynamic(), context}
    end
  end

  # {left, right}
  defp of_shared({left, right}, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, stack, context, fun)
  end

  # {...}
  defp of_shared({:{}, _meta, exprs}, stack, context, fun) do
    case map_reduce_ok(exprs, context, &fun.(&1, stack, &2)) do
      {:ok, _, context} -> {:ok, tuple(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # %{...}
  defp of_shared({:%{}, _meta, args}, stack, context, fun) do
    Of.open_map(args, stack, context, fun)
  end

  # %Struct{...}
  defp of_shared({:%, meta1, [module, {:%{}, _meta2, args}]}, stack, context, fun)
       when is_atom(module) do
    with {:ok, _, context} <- Of.struct(module, meta1, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, fun) do
      {:ok, map(), context}
    end
  end
end
