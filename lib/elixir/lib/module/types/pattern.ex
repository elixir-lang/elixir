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

  ## Variable handling

  @doc """
  Fetches the type of a defined variable.
  """
  def of_var({_name, meta, _context}, context) do
    version = Keyword.fetch!(meta, :version)
    %{vars: %{^version => %{type: type}}} = context
    type
  end

  defp refine_var({var_name, meta, var_context} = var, type, expr, stack, context) do
    version = Keyword.fetch!(meta, :version)

    case context.vars do
      %{^version => %{type: old_type, off_traces: off_traces} = data} ->
        new_type = intersection(type, old_type)
        data = %{data | type: new_type, off_traces: new_trace(expr, type, stack, off_traces)}
        context = put_in(context.vars[version], data)

        if empty?(new_type) do
          {:error,
           warn(__MODULE__, {:refine_var, old_type, type, var, context}, meta, stack, context)}
        else
          {:ok, new_type, context}
        end

      %{} ->
        data = %{
          type: type,
          name: var_name,
          context: var_context,
          off_traces: new_trace(expr, type, stack, [])
        }

        context = put_in(context.vars[version], data)
        {:ok, type, context}
    end
  end

  defp new_trace(nil, _type, _stack, traces), do: traces
  defp new_trace(expr, type, stack, traces), do: [{expr, stack.file, type} | traces]

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
  the given {expected, expr} pair  or an error in case of a typing conflict.
  """

  # ^var
  def of_pattern({:^, _meta, [var]}, _expected_expr, _stack, context) do
    {:ok, of_var(var, context), context}
  end

  # left = right
  def of_pattern({:=, _meta, [left_expr, right_expr]}, expected_expr, stack, context) do
    with {:ok, _, context} <- of_pattern(left_expr, expected_expr, stack, context),
         {:ok, _, context} <- of_pattern(right_expr, expected_expr, stack, context),
         do: {:ok, dynamic(), context}
  end

  # %_{...}
  def of_pattern(
        {:%, _meta1, [{:_, _meta2, var_context}, {:%{}, _meta3, args}]},
        _expected_expr,
        stack,
        context
      )
      when is_atom(var_context) do
    with {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %var{...} and %^var{...}
  def of_pattern({:%, _meta1, [var, {:%{}, _meta2, args}]}, _expected_expr, stack, context)
      when not is_atom(var) do
    # TODO: validate var is an atom
    with {:ok, _, context} = of_pattern(var, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %Struct{...}
  def of_pattern({:%, meta1, [module, {:%{}, _meta2, args}]}, _expected_expr, stack, context)
      when is_atom(module) do
    with {:ok, _, context} <- Of.struct(module, meta1, stack, context),
         {:ok, _, context} <- Of.open_map(args, stack, context, &of_pattern/3) do
      {:ok, map(), context}
    end
  end

  # %{...}
  def of_pattern({:%{}, _meta, args}, _expected_expr, stack, context) do
    Of.open_map(args, stack, context, &of_pattern/3)
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
    refine_var(var, type, expr, stack, context)
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

  ## Format warnings

  def format_warning({:refine_var, old_type, new_type, var, context}) do
    {traces, hints} = format_traces(var, context)

    [
      """
      incompatible types assigned to #{format_var(var)}:

          #{to_quoted_string(old_type)} !~ #{to_quoted_string(new_type)}
      """,
      traces,
      format_hints(hints),
      "\ntyping violation found at:"
    ]
  end

  def format_traces(expr, %{vars: vars}) do
    {_, versions} =
      Macro.prewalk(expr, %{}, fn
        {var_name, meta, var_context}, versions when is_atom(var_name) and is_atom(var_context) ->
          version = meta[:version]

          case vars do
            %{^version => data} -> {:ok, Map.put(versions, version, data)}
            %{} -> {:ok, versions}
          end

        node, versions ->
          {node, versions}
      end)

    vars = Map.values(versions)

    formatted_traces =
      vars
      |> Enum.sort_by(& &1.name)
      |> Enum.map(&format_trace/1)

    {formatted_traces, trace_hints(vars)}
  end

  defp format_trace(%{off_traces: []}) do
    []
  end

  defp format_trace(%{name: name, context: context, off_traces: traces}) do
    traces =
      traces
      |> Enum.reverse()
      |> Enum.map(fn {expr, file, type} ->
        meta = get_meta(expr)

        location =
          file
          |> Path.relative_to_cwd()
          |> Exception.format_file_line(meta[:line])
          |> String.replace_suffix(":", "")

        """

            # type: #{to_quoted_string(type)}
            # from: #{location}
            #{Macro.to_string(expr)}
        """
      end)

    type_or_types = pluralize(traces, "type", "types")
    ["\nwhere #{format_var(name, context)} was given the #{type_or_types}:\n" | traces]
  end

  defp format_var({var, _, context}), do: format_var(var, context)
  defp format_var(var, nil), do: "\"#{var}\""
  defp format_var(var, context), do: "\"#{var}\" (context #{inspect(context)})"

  defp pluralize([_], singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp inferred_bitstring_spec?(trace) do
    match?({{:<<>>, [inferred_bitstring_spec: true] ++ _meta, _}, _file, _type}, trace)
  end

  defp trace_hints(vars) do
    if Enum.any?(vars, fn data -> Enum.any?(data.off_traces, &inferred_bitstring_spec?/1) end) do
      [:inferred_bitstring_spec]
    else
      []
    end
  end
end
