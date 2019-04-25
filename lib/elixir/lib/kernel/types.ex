defmodule Kernel.Types do
  @moduledoc false

  defguardp is_var(expr)
            when is_tuple(expr) and
                   tuple_size(expr) == 3 and
                   is_atom(elem(expr, 0)) and
                   is_atom(elem(expr, 2))

  def infer_defs(_module, defs) do
    Enum.map(defs, fn {name, kind, _meta, clauses} ->
      types =
        Enum.map(clauses, fn {_meta, params, _guards, _body} ->
          of_clause(params)
        end)

      {name, kind, types}
    end)
  rescue
    e ->
      case :code.ensure_loaded(Access) do
        {:module, _} -> reraise(e, __STACKTRACE__)
        {:error, _} -> :error
      end
  end

  def of_clause(params) do
    of_pattern_multi(params, context())
  end

  def context(enum \\ []) do
    context = %{vars: %{}, types: %{}, counter: 0}
    Map.merge(context, Map.new(enum))
  end

  def of_pattern_multi(patterns, pattern_types \\ [], context)

  def of_pattern_multi([pattern | patterns], pattern_types, context) do
    case of_pattern(pattern, context) do
      {:ok, type, context} -> of_pattern_multi(patterns, [type | pattern_types], context)
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern_multi([], pattern_types, context) do
    {:ok, Enum.reverse(pattern_types), context}
  end

  def of_pattern(atom, context) when is_atom(atom) do
    {:ok, {:literal, atom}, context}
  end

  def of_pattern(literal, context) when is_integer(literal) do
    {:ok, :integer, context}
  end

  def of_pattern(literal, context) when is_float(literal) do
    {:ok, :float, context}
  end

  def of_pattern(literal, context) when is_binary(literal) do
    {:ok, :binary, context}
  end

  def of_pattern({:<<>>, _meta, args}, context) do
    # TODO: Add bitstring type
    case of_binary(args, context) do
      {:ok, context} -> {:ok, :binary, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern([{:|, _meta, [left_expr, right_expr]}], context) do
    with {:ok, left, context} <- of_pattern(left_expr, context),
         {:ok, right, context} <- of_pattern(right_expr, context),
         do: {:ok, {:cons, left, right}, context}
  end

  def of_pattern([left_expr | right_expr], context) do
    with {:ok, left, context} <- of_pattern(left_expr, context),
         {:ok, right, context} <- of_pattern(right_expr, context),
         do: {:ok, {:cons, left, right}, context}
  end

  def of_pattern([], context) do
    {:ok, :null, context}
  end

  def of_pattern(var, context) when is_var(var) do
    {type, context} = new_var(var_name(var), context)
    {:ok, type, context}
  end

  def of_pattern({left, right}, context) do
    of_pattern({:{}, [], [left, right]}, context)
  end

  def of_pattern({:{}, _meta, exprs}, context) do
    case of_pattern_multi(exprs, context) do
      {:ok, types, context} -> {:ok, {:tuple, types}, context}
      {:error, reason} -> {:error, reason}
    end
  end

  def of_pattern({:=, _meta, [left_expr, right_expr]}, context) do
    with {:ok, left_type, context} <- of_pattern(left_expr, context),
         {:ok, right_type, context} <- of_pattern(right_expr, context),
         do: unify(left_type, right_type, context)
  end

  def of_pattern(expr, _context) do
    {:error, {:unsupported_pattern, expr}}
  end

  defp of_binary([], context) do
    {:ok, context}
  end

  defp of_binary([{:"::", _meta, [expr, specifiers]} | args], context) do
    specifiers = List.flatten(collect_specifiers(specifiers))

    expected_type =
      cond do
        :integer in specifiers -> :integer
        :float in specifiers -> :float
        # TODO: Add bitstring type
        :bits in specifiers -> :binary
        :bitstring in specifiers -> :binary
        :bytes in specifiers -> :binary
        :binary in specifiers -> :binary
        # TODO: UTF needs union support
        # :utf8 in specifiers -> {:union, [:integer, :binary]}
        # :utf16 in specifiers -> {:union, [:integer, :binary]}
        # :utf32 in specifiers -> {:union, [:integer, :binary]}
        true -> :integer
      end

    with {:ok, type, context} <- of_pattern(expr, context),
         {:ok, _type, context} <- unify(type, expected_type, context),
         do: of_binary(args, context)
  end

  defp of_binary([expr | args], context) do
    case of_pattern(expr, context) do
      {:ok, type, context} when type in [:integer, :float, :binary] ->
        of_binary(args, context)

      {:ok, type, _context} ->
        {:error, {:invalid_binary_type, type}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp unify(same, same, context) do
    {:ok, same, context}
  end

  defp unify({:var, left}, {:var, right}, context) do
    case {Map.fetch(context.types, left), Map.fetch(context.types, right)} do
      {{:ok, type}, :error} -> {:ok, type, put_in(context.types[right], type)}
      {:error, {:ok, type}} -> {:ok, type, put_in(context.types[left], type)}
      {_, {:ok, :unbound}} -> {:ok, {:var, left}, put_in(context.types[right], {:var, left})}
      {{:ok, :unbound}, _} -> {:ok, {:var, right}, put_in(context.types[left], {:var, right})}
      {{:ok, same}, {:ok, same}} -> {:ok, same}
      {{:ok, left_type}, {:ok, right_type}} -> unify(left_type, right_type, context)
    end
  end

  defp unify(type, {:var, var}, context) do
    case Map.fetch(context.types, var) do
      :error -> add_var(var, type, context)
      {:ok, :unbound} -> add_var(var, type, context)
      {:ok, var_type} -> unify(type, var_type, context)
    end
  end

  defp unify({:var, var}, type, context) do
    unify(type, {:var, var}, context)
  end

  defp unify(left, right, _context) do
    {:error, {:unable_unify, left, right}}
  end

  defp add_var(var, type, context) do
    context = put_in(context.types[var], type)

    if recursive_type?(type, [], context) do
      {:error, {:recursive_type, type}}
    else
      {:ok, type, context}
    end
  end

  defp recursive_type?({:var, var} = parent, parents, context) do
    case Map.fetch(context.types, var) do
      :error -> false
      {:ok, :unbound} -> false
      {:ok, type} -> type in parents or recursive_type?(type, [parent | parents], context)
    end
  end

  defp recursive_type?({:cons, left, right} = parent, parents, context) do
    recursive_type?(left, [parent | parents], context) or
      recursive_type?(right, [parent | parents], context)
  end

  defp recursive_type?({:tuple, types} = parent, parents, context) do
    Enum.any?(types, &recursive_type?(&1, [parent | parents], context))
  end

  defp recursive_type?(_other, _parents, _context) do
    false
  end

  defp collect_specifiers({:-, _meta, [left, right]}) do
    [collect_specifiers(left), collect_specifiers(right)]
  end

  defp collect_specifiers({specifier, _meta, []}) do
    [specifier]
  end

  defp collect_specifiers(var) when is_var(var) do
    [var_name(var)]
  end

  defp collect_specifiers(other) do
    []
  end

  defp new_var(var_name, context) do
    case Map.fetch(context.vars, var_name) do
      {:ok, type} ->
        {type, context}

      :error ->
        type = {:var, context.counter}
        types = Map.put(context.types, context.counter, :unbound)
        vars = Map.put(context.vars, var_name, type)
        context = %{context | vars: vars, types: types, counter: context.counter + 1}
        {type, context}
    end
  end

  # NOTE: Ignores the variable's context
  defp var_name({name, _meta, _context}), do: name
end
