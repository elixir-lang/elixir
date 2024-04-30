defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.Of
  import Module.Types.{Helpers, Descr}

  @expected_expr {dynamic(), nil}

  @doc """
  Handles patterns and guards at once.
  """
  def of_head(patterns, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(patterns, context, &of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/4 returns boolean() | :fail
         {:ok, _, context} <- of_guards(guards, {term(), nil}, stack, context),
         do: {:ok, types, context}
  end

  ## Patterns

  @doc """
  Return the type and typing context of a pattern expression
  with no {expected, expr} pair. of_pattern/4 must be preferred
  whenever possible as it adds more context to errors.
  """
  def of_pattern(expr, stack, context) do
    of_pattern(expr, @expected_expr, stack, context)
  end

  @doc """
  Return the type and typing context of a pattern expression with
  the given {expected, expr} pair or an error in case of a typing conflict.
  """

  # ^var
  def of_pattern({:^, _meta, [var]}, _expected_expr, _stack, context) do
    {:ok, Of.var(var, context), context}
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]}, expected_expr, stack, context) do
    # TODO: We need to properly track and annotate variables across (potentially nested) sides.
    case {is_var(left_expr), is_var(right_expr)} do
      {true, false} ->
        with {:ok, type, context} <- of_pattern(right_expr, expected_expr, stack, context) do
          of_pattern(left_expr, {type, right_expr}, stack, context)
        end

      {false, true} ->
        with {:ok, type, context} <- of_pattern(left_expr, expected_expr, stack, context) do
          of_pattern(right_expr, {type, left_expr}, stack, context)
        end

      {_, _} ->
        with {:ok, _, context} <- of_pattern(left_expr, expected_expr, stack, context),
             {:ok, _, context} <- of_pattern(right_expr, expected_expr, stack, context),
             do: {:ok, dynamic(), context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]} = expr, _expected_expr, stack, context)
      when not is_atom(var) do
    with {:ok, _, context} <- of_pattern(var, {atom(), expr}, stack, context) do
      Of.map(:open, args, stack, context, &of_pattern/3)
    end
  end

  # %Struct{...}
  def of_pattern({:%, _, [module, {:%{}, _, args}]} = expr, _expected_expr, stack, context)
      when is_atom(module) do
    Of.struct(expr, module, args, true, stack, context, &of_pattern/3)
  end

  # %{...}
  def of_pattern({:%{}, _meta, args}, _expected_expr, stack, context) do
    Of.map(:open, args, stack, context, &of_pattern/3)
  end

  # <<...>>>
  def of_pattern({:<<>>, _meta, args}, _expected_expr, stack, context) do
    case Of.binary(args, :pattern, stack, context, &of_pattern/4) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # _
  def of_pattern({:_, _meta, _var_context}, {type, _expr}, _stack, context) do
    {:ok, type, context}
  end

  # var
  def of_pattern(var, {type, expr}, stack, context) when is_var(var) do
    Of.refine_var(var, type, expr, stack, context)
  end

  def of_pattern(expr, expected_expr, stack, context) do
    of_shared(expr, expected_expr, stack, context, &of_pattern/4)
  end

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  # TODO: All expressions in of_pattern plus functions calls are not handled
  # by of_guards. There is a question of how much of of_shared can also be
  # shared with of_expr, but still unclear. In the worst case scenario,
  # Of.literal() could be added for pattern, guards, and expr.
  def of_guards(_expr, _expected_expr, _stack, context) do
    {:ok, dynamic(), context}
  end

  ## Shared

  # :atom
  defp of_shared(atom, _expected_expr, _stack, context, _fun) when is_atom(atom) do
    {:ok, atom([atom]), context}
  end

  # 12
  defp of_shared(literal, _expected_expr, _stack, context, _fun) when is_integer(literal) do
    {:ok, integer(), context}
  end

  # 1.2
  defp of_shared(literal, _expected_expr, _stack, context, _fun) when is_float(literal) do
    {:ok, float(), context}
  end

  # "..."
  defp of_shared(literal, _expected_expr, _stack, context, _fun) when is_binary(literal) do
    {:ok, binary(), context}
  end

  # []
  defp of_shared([], _expected_expr, _stack, context, _fun) do
    {:ok, empty_list(), context}
  end

  # [expr, ...]
  defp of_shared(exprs, _expected_expr, stack, context, fun) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &fun.(&1, @expected_expr, stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # {left, right}
  defp of_shared({left, right}, expected_expr, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, expected_expr, stack, context, fun)
  end

  # left | []
  defp of_shared({:|, _meta, [left_expr, []]}, _expected_expr, stack, context, fun) do
    fun.(left_expr, @expected_expr, stack, context)
  end

  # left | right
  defp of_shared({:|, _meta, [left_expr, right_expr]}, _expected_expr, stack, context, fun) do
    case fun.(left_expr, @expected_expr, stack, context) do
      {:ok, _, context} ->
        fun.(right_expr, @expected_expr, stack, context)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # left ++ right
  defp of_shared(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]},
         _expected_expr,
         stack,
         context,
         fun
       ) do
    # The left side is always a list
    with {:ok, _, context} <- fun.(left_expr, @expected_expr, stack, context),
         {:ok, _, context} <- fun.(right_expr, @expected_expr, stack, context) do
      # TODO: Both lists can be empty, so this may be an empty list,
      # so we return dynamic() for now.
      {:ok, dynamic(), context}
    end
  end

  # {...}
  defp of_shared({:{}, _meta, exprs}, _expected_expr, stack, context, fun) do
    case map_reduce_ok(exprs, context, &fun.(&1, @expected_expr, stack, &2)) do
      {:ok, _, context} -> {:ok, tuple(), context}
      {:error, reason} -> {:error, reason}
    end
  end
end
