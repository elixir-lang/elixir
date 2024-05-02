defmodule Module.Types.Pattern do
  @moduledoc false

  alias Module.Types.Of
  import Module.Types.{Helpers, Descr}

  # Compute the expected guard once at compile-time.
  @expected_guard {atom([true, false, :fail]), nil}
  defp expected_guard, do: @expected_guard

  # Compute the expected term once at compile-time.
  @expected_term {term(), nil}
  defp expected_term, do: @expected_term

  @doc """
  Handles patterns and guards at once.
  """
  def of_head(patterns, guards, stack, context) do
    with {:ok, types, context} <-
           map_reduce_ok(patterns, context, &of_pattern(&1, stack, &2)),
         # TODO: Check that of_guard/4 returns boolean() | :fail
         {:ok, _, context} <-
           map_reduce_ok(guards, context, &of_guard(&1, expected_guard(), stack, &2)),
         do: {:ok, types, context}
  end

  ## Patterns

  @doc """
  Return the type and typing context of a pattern expression
  with no {expected, expr} pair. of_pattern/4 must be preferred
  whenever possible as it adds more context to errors.
  """
  def of_pattern(expr, stack, context) do
    of_pattern(expr, expected_term(), stack, context)
  end

  @doc """
  Return the type and typing context of a pattern expression with
  the given {expected, expr} pair or an error in case of a typing conflict.
  """

  # ^var
  def of_pattern({:^, _meta, [var]}, _expected_term, _stack, context) do
    {:ok, Of.var(var, context), context}
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]} = expr, expected_term, stack, context) do
    # TODO: We need to properly track and annotate variables across (potentially nested) sides.
    case {is_var(left_expr), is_var(right_expr)} do
      {true, false} ->
        with {:ok, type, context} <- of_pattern(right_expr, expected_term, stack, context) do
          of_pattern(left_expr, {type, expr}, stack, context)
        end

      {false, true} ->
        with {:ok, type, context} <- of_pattern(left_expr, expected_term, stack, context) do
          of_pattern(right_expr, {type, expr}, stack, context)
        end

      {_, _} ->
        with {:ok, _, context} <- of_pattern(left_expr, expected_term, stack, context),
             {:ok, _, context} <- of_pattern(right_expr, expected_term, stack, context),
             do: {:ok, dynamic(), context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, meta, [inner, {:%{}, _meta2, args}]} = expr, _expected_term, stack, context)
      when not is_atom(inner) do
    with {:ok, type, context} <- of_pattern(inner, {atom(), expr}, stack, context),
         {:ok, map, context} <-
           Of.map(:open, args, [__struct__: type], stack, context, &of_pattern/3) do
      # Skip the check if a variable, as it has already been checked
      if is_var(inner) or atom_type?(type) do
        {:ok, map, context}
      else
        warning = {:badstruct, expr, context}
        {:error, warn(__MODULE__, warning, meta, stack, context)}
      end
    end
  end

  # %Struct{...}
  def of_pattern({:%, _, [module, {:%{}, _, args}]} = expr, _expected_term, stack, context)
      when is_atom(module) do
    Of.struct(expr, module, args, true, stack, context, &of_pattern/3)
  end

  # %{...}
  def of_pattern({:%{}, _meta, args}, _expected_term, stack, context) do
    Of.map(:open, args, stack, context, &of_pattern/3)
  end

  # <<...>>>
  def of_pattern({:<<>>, _meta, args}, _expected_term, stack, context) do
    case Of.binary(args, :pattern, stack, context, &of_pattern/4) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, context} -> {:error, context}
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

  def of_pattern(expr, expected_term, stack, context) do
    of_shared(expr, expected_term, stack, context, &of_pattern/4)
  end

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """

  # TODO: There is a certain amount of code to be shared between patterns
  # and guards and then guards and expressions, but it is unclear how much.
  # In the worst case scenario, Of.literal() could be added for pattern,
  # guards, and expr.

  def of_guard(expr, stack, context) do
    of_guard(expr, expected_term(), stack, context)
  end

  # %Struct{...}
  def of_guard({:%, _, [module, {:%{}, _, args}]} = expr, _expected_term, stack, context)
      when is_atom(module) do
    Of.struct(expr, module, args, false, stack, context, &of_guard/3)
  end

  # %{...}
  def of_guard({:%{}, _meta, args}, _expected_term, stack, context) do
    Of.map(:closed, args, stack, context, &of_guard/3)
  end

  # <<>>
  def of_guard({:<<>>, _meta, args}, _expected_term, stack, context) do
    case Of.binary(args, :expr, stack, context, &of_guard/4) do
      {:ok, context} -> {:ok, binary(), context}
      # It is safe to discard errors from binary inside expressions
      {:error, context} -> {:ok, binary(), context}
    end
  end

  # var.field
  def of_guard({{:., _, [callee, key]}, _, []} = expr, _expected_term, stack, context)
      when not is_atom(callee) do
    with {:ok, type, context} <- of_guard(callee, expected_term(), stack, context) do
      Of.map_fetch(expr, type, key, stack, context)
    end
  end

  # Remote
  def of_guard({{:., _, [:erlang, function]}, _, args} = expr, _expected_term, stack, context)
      when is_atom(function) do
    with {:ok, args_type, context} <-
           map_reduce_ok(args, context, &of_guard(&1, expected_term(), stack, &2)) do
      Of.apply(:erlang, function, args_type, expr, stack, context)
    end
  end

  # var
  def of_guard(var, _expected_term, _stack, context) when is_var(var) do
    {:ok, Of.var(var, context), context}
  end

  def of_guard(expr, expected_term, stack, context) do
    of_shared(expr, expected_term, stack, context, &of_guard/4)
  end

  ## Shared

  # :atom
  defp of_shared(atom, _expected_term, _stack, context, _fun) when is_atom(atom) do
    {:ok, atom([atom]), context}
  end

  # 12
  defp of_shared(literal, _expected_term, _stack, context, _fun) when is_integer(literal) do
    {:ok, integer(), context}
  end

  # 1.2
  defp of_shared(literal, _expected_term, _stack, context, _fun) when is_float(literal) do
    {:ok, float(), context}
  end

  # "..."
  defp of_shared(literal, _expected_term, _stack, context, _fun) when is_binary(literal) do
    {:ok, binary(), context}
  end

  # []
  defp of_shared([], _expected_term, _stack, context, _fun) do
    {:ok, empty_list(), context}
  end

  # [expr, ...]
  defp of_shared(exprs, _expected_term, stack, context, fun) when is_list(exprs) do
    case map_reduce_ok(exprs, context, &fun.(&1, expected_term(), stack, &2)) do
      {:ok, _types, context} -> {:ok, non_empty_list(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # {left, right}
  defp of_shared({left, right}, expected_term, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, expected_term, stack, context, fun)
  end

  # left | []
  defp of_shared({:|, _meta, [left_expr, []]}, _expected_term, stack, context, fun) do
    fun.(left_expr, expected_term(), stack, context)
  end

  # left | right
  defp of_shared({:|, _meta, [left_expr, right_expr]}, _expected_term, stack, context, fun) do
    case fun.(left_expr, expected_term(), stack, context) do
      {:ok, _, context} ->
        fun.(right_expr, expected_term(), stack, context)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # left ++ right
  defp of_shared(
         {{:., _meta1, [:erlang, :++]}, _meta2, [left_expr, right_expr]},
         _expected_term,
         stack,
         context,
         fun
       ) do
    # The left side is always a list
    with {:ok, _, context} <- fun.(left_expr, expected_term(), stack, context),
         {:ok, _, context} <- fun.(right_expr, expected_term(), stack, context) do
      # TODO: Both lists can be empty, so this may be an empty list,
      # so we return dynamic() for now.
      {:ok, dynamic(), context}
    end
  end

  # {...}
  defp of_shared({:{}, _meta, exprs}, _expected_term, stack, context, fun) do
    case map_reduce_ok(exprs, context, &fun.(&1, expected_term(), stack, &2)) do
      {:ok, _, context} -> {:ok, tuple(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  ## Warning formatting

  def format_warning({:badstruct, expr, context}) do
    {traces, trace_hints} = Of.format_traces(expr, context)

    [
      """
      struct must be an atom() in expression:

          #{expr_to_string(expr) |> indent(4)}
      """,
      traces,
      format_hints(trace_hints),
      "\ntyping violation found at:"
    ]
  end
end
