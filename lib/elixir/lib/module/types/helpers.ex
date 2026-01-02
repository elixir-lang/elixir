# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Helpers do
  # AST and enumeration helpers.
  @moduledoc false

  ## AST helpers

  @doc """
  Returns true if the mode cares about warnings.
  """
  defguard is_warning(stack) when stack.mode != :infer

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

  @doc """
  Attaches span information.
  """
  def with_span(meta, name) do
    :elixir_env.calculate_span(meta, name)
  end

  ## Warnings

  @doc """
  Converts an itneger into ordinal.
  """
  def integer_to_ordinal(i) do
    case rem(i, 10) do
      1 when rem(i, 100) != 11 -> "#{i}st"
      2 when rem(i, 100) != 12 -> "#{i}nd"
      3 when rem(i, 100) != 13 -> "#{i}rd"
      _ -> "#{i}th"
    end
  end

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

      :empty_union_domain ->
        """

        #{hint()} the function has an empty domain and therefore cannot be applied to \
        any argument. This may happen when you have a union of functions, which means \
        the only valid argument to said function are types that satisfy all sides of \
        the union (which may be none)
        """

      {:impl, for} ->
        # Get the type without dynamic for better pretty printing
        type =
          for
          |> Module.Types.Of.impl()
          |> Module.Types.Descr.dynamic()
          |> Map.fetch!(:dynamic)
          |> Module.Types.Descr.to_quoted_string(collapse_structs: true)

        """

        #{hint()} defimpl for #{inspect(for)} requires its callbacks to match exclusively on #{type}
        """

      :empty_domain ->
        """

        #{hint()} the function has an empty domain and therefore cannot be applied to \
        any argument
        """
    end)
  end

  @doc "The hint prefix"
  def hint, do: :elixir_errors.prefix(:hint)

  @doc """
  Collect traces from variables in expression.

  This information is exposed to language servers and
  therefore must remain backwards compatible.
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
                 traces: collect_var_traces(expr, off_traces)
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

  defp collect_var_traces(parent_expr, traces) do
    traces
    |> Enum.reject(fn {expr, _file, type} ->
      # As an optimization do not care about dynamic terms
      type == %{dynamic: :term} or expr == parent_expr
    end)
    |> case do
      [] -> traces
      filtered -> filtered
    end
    |> Enum.reverse()
    |> Enum.map(fn {expr, file, type} ->
      meta = get_meta(expr)

      # This information is exposed to language servers and
      # therefore must remain backwards compatible.
      %{
        file: file,
        meta: meta,
        formatted_expr: expr_to_string(expr),
        formatted_hints: format_hints(expr_hints(expr)),
        formatted_type: Module.Types.Descr.to_quoted_string(type, collapse_structs: true)
      }
    end)
    |> Enum.sort_by(&{&1.meta[:line], &1.meta[:column]})
    |> Enum.dedup()
  end

  defp expr_hints(expr) do
    case expr do
      {:<<>>, [inferred_bitstring_spec: true] ++ _meta, _} ->
        [:inferred_bitstring_spec]

      {_, meta, _} ->
        case meta[:type_check] do
          :anonymous_rescue -> [:anonymous_rescue]
          _ -> []
        end

      _ ->
        []
    end
  end

  @doc """
  Format previously collected traces.
  """
  def format_traces(traces) do
    Enum.map(traces, &format_trace/1)
  end

  defp format_trace(%{type: :variable, name: name, context: context, traces: traces}) do
    traces =
      for trace <- traces do
        location =
          trace.file
          |> Path.relative_to_cwd()
          |> Exception.format_file_line(trace.meta[:line], trace.meta[:column])
          |> String.replace_suffix(":", "")

        [
          """

              # type: #{indent(trace.formatted_type, 4)}
              # from: #{location}
              \
          """,
          indent(trace.formatted_expr, 4),
          ?\n,
          trace.formatted_hints
        ]
      end

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

  @doc """
  Converts the given expression to a string,
  translating inlined Erlang calls back to Elixir.

  We also undo some macro expressions done by the Kernel module.

  ## Options

    * `:collapse_structs` - when false, show structs full representation
  """
  def expr_to_string(expr, opts \\ []) do
    string = prewalk_expr_to_string(expr, opts)

    case expr do
      {_, meta, _} ->
        case meta[:type_check] do
          :anonymous_rescue ->
            "rescue " <> string

          :rescue ->
            "rescue " <> string

          {:invoked_as, mod, fun, arity} ->
            string <> "\n#=> invoked as " <> Exception.format_mfa(mod, fun, arity)

          _ ->
            string
        end

      _ ->
        string
    end
  end

  defp prewalk_expr_to_string(expr, opts) do
    collapse_structs? = Keyword.get(opts, :collapse_structs, true)

    expr
    |> Macro.prewalk(fn
      {:%, _, [Range, {:%{}, _, fields}]} = node ->
        case :lists.usort(fields) do
          [first: first, last: last, step: step] ->
            quote do
              unquote(first)..unquote(last)//unquote(step)
            end

          _ ->
            node
        end

      {:%, struct_meta, [struct, {:%{}, map_meta, fields}]} = node
      when collapse_structs? ->
        try do
          struct.__info__(:struct)
        rescue
          _ -> node
        else
          infos ->
            filtered =
              for {field, value} <- fields, not matches_default?(infos, field, value) do
                {field, value}
              end

            if length(fields) != length(filtered) do
              {:%, struct_meta, [struct, {:%{}, map_meta, [{:..., [], []} | filtered]}]}
            else
              node
            end
        end

      {{:., _, [Elixir.String.Chars, :to_string]}, meta, [arg]} ->
        {:to_string, meta, [arg]}

      {{:., _, [Elixir.List.Chars, :to_charlist]}, meta, [arg]} ->
        {:to_charlist, meta, [arg]}

      {{:., _, [mod, fun]}, meta, args} ->
        erl_to_ex(mod, fun, args, meta)

      {:fn, meta, [{:->, _, [_args, return]}]} = expr ->
        if meta[:capture] do
          {:&, meta, [return]}
        else
          expr
        end

      {:&, amp_meta, [{:/, slash_meta, [{{:., dot_meta, [mod, fun]}, call_meta, []}, arity]}]} ->
        {mod, fun} =
          case :elixir_rewrite.erl_to_ex(mod, fun, arity) do
            {mod, fun} -> {mod, fun}
            false -> {mod, fun}
          end

        {:&, amp_meta, [{:/, slash_meta, [{{:., dot_meta, [mod, fun]}, call_meta, []}, arity]}]}

      {:case, meta, [expr, [do: clauses]]} = case ->
        if meta[:type_check] == :expr do
          case clauses do
            [
              {:->, _,
               [
                 [
                   {:when, _,
                    [
                      {var, _, Kernel},
                      {{:., _, [:erlang, :orelse]}, _,
                       [
                         {{:., _, [:erlang, :"=:="]}, _, [{var, _, Kernel}, false]},
                         {{:., _, [:erlang, :"=:="]}, _, [{var, _, Kernel}, nil]}
                       ]}
                    ]}
                 ],
                 else_block
               ]},
              {:->, _, [[{:_, _, Kernel}], do_block]}
            ] ->
              {:if, meta, [expr, [do: do_block, else: else_block]]}

            [
              {:->, _, [[false], else_block]},
              {:->, _, [[true], do_block]}
            ] ->
              {:if, meta, [expr, [do: do_block, else: else_block]]}

            _ ->
              case
          end
        else
          case
        end

      {var, meta, context} = expr when is_atom(var) and is_atom(context) ->
        if is_integer(meta[:capture]) do
          {:&, meta, [meta[:capture]]}
        else
          expr
        end

      other ->
        other
    end)
    |> Macro.to_string()
  end

  defp matches_default?(infos, field, value) do
    case Enum.find(infos, &(&1.field == field)) do
      %{default: default} -> Macro.escape(default) == value
      _ -> false
    end
  end

  defp erl_to_ex(
         :erlang,
         :error,
         [expr, :none, [error_info: {:%{}, _, [module: Exception]}]],
         meta
       ) do
    {:raise, meta, [expr]}
  end

  defp erl_to_ex(mod, fun, args, meta) do
    case :elixir_rewrite.erl_to_ex(mod, fun, args) do
      {Kernel, fun, args, _} -> {fun, meta, args}
      {mod, fun, args, _} -> {{:., [], [mod, fun]}, meta, args}
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
      effectively_warn(module, warning, meta, stack, context)
    end
  end

  defp effectively_warn(module, warning, meta, stack, context) do
    {fun, arity} = stack.function
    location = {stack.file, meta, {stack.module, fun, arity}}
    %{context | warnings: [{module, warning, location} | context.warnings]}
  end

  @doc """
  Emits an error.

  In practice an error is a warning that halts other errors from being collected.
  """
  def error(module, warning, meta, stack, context) do
    case context do
      %{failed: true} ->
        context

      %{failed: false} ->
        if Keyword.get(meta, :generated, false) do
          context
        else
          effectively_warn(module, warning, meta, stack, %{context | failed: true})
        end
    end
  end

  @doc """
  The type to return when there is an error.
  """
  def error_type, do: Module.Types.Descr.dynamic()

  ## Enum helpers

  def zip_map_reduce(args1, args2, acc, fun) do
    zip_map_reduce(args1, args2, [], acc, fun)
  end

  defp zip_map_reduce([arg1 | args1], [arg2 | args2], list, acc, fun) do
    {item, acc} = fun.(arg1, arg2, acc)
    zip_map_reduce(args1, args2, [item | list], acc, fun)
  end

  defp zip_map_reduce([], [], list, acc, _fun) do
    {Enum.reverse(list), acc}
  end
end
