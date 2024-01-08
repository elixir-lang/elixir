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

  ## Variable handling

  @doc """
  Fetches the type of a defined variable.
  """
  def of_var({_name, meta, _context}, context) do
    version = Keyword.fetch!(meta, :version)
    %{vars: %{^version => %{type: type}}} = context
    type
  end

  defp refine_var({var_name, meta, var_context}, type, _stack, context) do
    version = Keyword.fetch!(meta, :version)

    # TODO: Properly handle dynamic
    case context.vars do
      %{^version => %{type: previous_type} = data} ->
        # TODO: Properly compute intersection and union of dynamic
        dynamic = dynamic()

        if previous_type == dynamic or type == dynamic do
          context = put_in(context.vars[version], %{data | type: dynamic})
          {:ok, dynamic, context}
        else
          refined_type = intersection(type, previous_type)

          if is_none(refined_type) do
            # TODO: Add warning here
            {:error, context}
          else
            context = put_in(context.vars[version], %{data | type: refined_type})
            {:ok, refined_type, context}
          end
        end

      %{} ->
        data = %{type: type, name: var_name, context: var_context}
        context = put_in(context.vars[version], data)
        {:ok, type, context}
    end
  end

  ## Patterns

  @doc """
  Return the type and typing context of a pattern expression or an error
  in case of a typing conflict.
  """

  # ^var
  def of_pattern({:^, _meta, [var]}, _expected, _stack, context) do
    {:ok, of_var(var, context), context}
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]}, expected, stack, context) do
    with {:ok, _, context} <- of_pattern(left_expr, expected, stack, context),
         {:ok, _, context} <- of_pattern(right_expr, expected, stack, context),
         do: {:ok, dynamic(), context}
  end

  # %_{...}
  def of_pattern(
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
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]}, _expected, stack, context)
      when not is_atom(var) do
    # TODO: validate var is an atom
    with {:ok, _, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %Struct{...}
  def of_pattern({:%, meta1, [module, {:%{}, _meta2, args}]}, _expected, stack, context)
      when is_atom(module) do
    with {:ok, _, context} <- Of.struct(module, meta1, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %{...}
  def of_pattern({:%{}, _meta, args}, _expected, stack, context) do
    Of.open_map(args, stack, context, &of_pattern/3)
  end

  # <<...>>>
  def of_pattern({:<<>>, _meta, args}, _expected, stack, context) do
    case Of.binary(args, :pattern, stack, context, &of_pattern/4) do
      {:ok, context} -> {:ok, binary(), context}
      {:error, reason} -> {:error, reason}
    end
  end

  # _
  def of_pattern({:_, _meta, _var_context}, expected, _stack, context) do
    {:ok, expected, context}
  end

  # var
  def of_pattern(var, expected, stack, context) when is_var(var) do
    refine_var(var, expected, stack, context)
  end

  def of_pattern(expr, expected, stack, context) do
    of_shared(expr, expected, stack, context, &of_pattern/4)
  end

  defp of_pattern(expr, stack, context) do
    of_pattern(expr, dynamic(), stack, context)
  end

  @doc """
  Refines the type variables in the typing context using type check guards
  such as `is_integer/1`.
  """
  # TODO: All expressions in of_pattern plus functions calls are not handled
  # by of_guards. There is a question of how much of of_shared can also be
  # shared with of_expr, but still unclear.
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

  # {left, right}
  defp of_shared({left, right}, expected, stack, context, fun) do
    of_shared({:{}, [], [left, right]}, expected, stack, context, fun)
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

  # {...}
  defp of_shared({:{}, _meta, exprs}, _expected, stack, context, fun) do
    case map_reduce_ok(exprs, context, &fun.(&1, dynamic(), stack, &2)) do
      {:ok, _, context} -> {:ok, tuple(), context}
      {:error, reason} -> {:error, reason}
    end
  end
end
