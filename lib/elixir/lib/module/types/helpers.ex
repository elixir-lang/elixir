defmodule Module.Types.Helpers do
  # AST and enumeration helpers.
  @moduledoc false

  ## AST helpers

  @doc """
  Guard function to check if an AST node is a variable.
  """
  defmacro is_var(expr) do
    quote do
      is_tuple(unquote(expr)) and
        tuple_size(unquote(expr)) == 3 and
        is_atom(elem(unquote(expr), 0)) and
        is_atom(elem(unquote(expr), 2))
    end
  end

  @doc """
  Unpacks a list into head elements and tail.
  """
  def unpack_list([{:|, _, [head, tail]}], acc), do: {Enum.reverse([head | acc]), tail}
  def unpack_list([head], acc), do: {Enum.reverse([head | acc]), []}
  def unpack_list([head | tail], acc), do: unpack_list(tail, [head | acc])

  @doc """
  Unpacks a match into several matches.
  """
  def unpack_match({:=, _, [left, right]}, acc),
    do: unpack_match(left, unpack_match(right, acc))

  def unpack_match(node, acc),
    do: [node | acc]

  @doc """
  Returns the AST metadata.
  """
  def get_meta({_, meta, _}), do: meta
  def get_meta(_other), do: []

  ## Warnings

  @doc """
  Formatted hints in typing errors.
  """
  def format_hints(hints) do
    hints
    |> Enum.uniq()
    |> Enum.map(fn
      :inferred_bitstring_spec ->
        """

        #{hint()} all expressions given to binaries are assumed to be of type \
        integer() unless said otherwise. For example, <<expr>> assumes "expr" \
        is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
        to change the default behavior.
        """

      :dot ->
        """

        #{hint()} "var.field" (without parentheses) means "var" is a map() while \
        "var.fun()" (with parentheses) means "var" is an atom()
        """

      :anonymous_rescue ->
        """

        #{hint()} when you rescue without specifying exception names, \
        the variable is assigned a type of a struct but all of its fields are unknown. \
        If you are trying to access an exception's :message key, either specify the \
        exception names or use `Exception.message/1`.
        """
    end)
  end

  defp hint, do: :elixir_errors.prefix(:hint)

  @doc """
  Collect traces from variables in expression.
  """
  def collect_traces(expr, %{vars: vars}) do
    {_, versions} =
      Macro.prewalk(expr, %{}, fn
        {var_name, meta, var_context}, versions when is_atom(var_name) and is_atom(var_context) ->
          version = meta[:version]

          case vars do
            %{^version => %{off_traces: [_ | _] = off_traces, name: name, context: context}} ->
              {:ok,
               Map.put(versions, version, %{
                 type: :variable,
                 name: name,
                 context: context,
                 traces: collect_var_traces(off_traces)
               })}

            _ ->
              {:ok, versions}
          end

        node, versions ->
          {node, versions}
      end)

    versions
    |> Map.values()
    |> Enum.sort_by(& &1.name)
  end

  defp collect_var_traces(traces) do
    traces
    |> Enum.reject(fn {_expr, _file, type, _formatter} ->
      Module.Types.Descr.dynamic_term_type?(type)
    end)
    |> case do
      [] -> traces
      filtered -> filtered
    end
    |> Enum.reverse()
    |> Enum.map(fn {expr, file, type, formatter} ->
      meta = get_meta(expr)

      {formatted_expr, formatter_hints} =
        case formatter do
          :default -> {expr_to_string(expr), []}
          formatter -> formatter.(expr)
        end

      %{
        file: file,
        meta: meta,
        formatted_expr: formatted_expr,
        formatted_hints: format_hints(formatter_hints ++ expr_hints(expr)),
        formatted_type: Module.Types.Descr.to_quoted_string(type)
      }
    end)
    |> Enum.sort_by(&{&1.meta[:line], &1.meta[:column]})
  end

  @doc """
  Format previously collected traces.
  """
  def format_traces(traces) do
    Enum.map(traces, &format_trace/1)
  end

  defp format_trace(%{type: :variable, name: name, context: context, traces: traces}) do
    traces =
      traces
      |> Enum.map(fn trace ->
        location =
          trace.file
          |> Path.relative_to_cwd()
          |> Exception.format_file_line(trace.meta[:line])
          |> String.replace_suffix(":", "")

        {trace.formatted_type, location, trace.formatted_expr, trace.formatted_hints}
      end)
      |> Enum.dedup()
      |> Enum.map(fn {formatted_type, location, formatted_expr, formatted_hints} ->
        [
          """

              # type: #{indent(formatted_type, 4)}
              # from: #{location}
              \
          """,
          indent(formatted_expr, 4),
          ?\n,
          formatted_hints
        ]
      end)

    type_or_types = pluralize(traces, "type", "types")
    ["\nwhere #{format_var(name, context)} was given the #{type_or_types}:\n" | traces]
  end

  @doc """
  Formats a var for pretty printing.
  """
  def format_var({var, _, context}), do: format_var(var, context)
  def format_var(var, nil), do: "\"#{var}\""
  def format_var(var, context), do: "\"#{var}\" (context #{inspect(context)})"

  defp pluralize([_], singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp expr_hints({:<<>>, [inferred_bitstring_spec: true] ++ _meta, _}),
    do: [:inferred_bitstring_spec]

  defp expr_hints(_), do: []

  @doc """
  Converts the given expression to a string,
  translating inlined Erlang calls back to Elixir.
  """
  def expr_to_string(expr) do
    expr
    |> reverse_rewrite()
    |> Macro.to_string()
  end

  defp reverse_rewrite(guard) do
    Macro.prewalk(guard, fn
      {{:., _, [mod, fun]}, meta, args} -> erl_to_ex(mod, fun, args, meta)
      other -> other
    end)
  end

  defp erl_to_ex(mod, fun, args, meta) do
    case :elixir_rewrite.erl_to_ex(mod, fun, args) do
      {Kernel, fun, args} -> {fun, meta, args}
      {mod, fun, args} -> {{:., [], [mod, fun]}, meta, args}
    end
  end

  @doc """
  Indents new lines.
  """
  def indent(content, count) do
    String.replace(content, "\n", "\n" <> String.duplicate(" ", count))
  end

  @doc """
  Emits a warning.
  """
  def warn(module, warning, meta, stack, context) do
    if Keyword.get(meta, :generated, false) do
      context
    else
      {fun, arity} = stack.function
      location = {stack.file, meta, {stack.module, fun, arity}}
      %{context | warnings: [{module, warning, location} | context.warnings]}
    end
  end

  @doc """
  Emits an error.

  In practice an error is a warning that halts other errors from being collected.
  """
  def error(module, warning, meta, stack, context) do
    case context do
      %{failed: true} -> context
      %{failed: false} -> warn(module, warning, meta, stack, %{context | failed: true})
    end
  end

  @doc """
  The type to return when there is an error.
  """
  def error_type, do: Module.Types.Descr.dynamic()
end
