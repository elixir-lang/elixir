defmodule Module.Types.Helpers do
  # AST and enumeration helpers.
  @moduledoc false

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
  Returns the AST metadata.
  """
  def get_meta({_, meta, _}), do: meta
  def get_meta(_other), do: []

  @doc """
  Indents new lines.
  """
  def indent(content, count) do
    String.replace(content, "\n", "\n" <> String.duplicate(" ", count))
  end

  @doc """
  Emits a warnings.
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
  Like `Enum.reduce/3` but only continues while `fun` returns `{:ok, acc}`
  and stops on `{:error, reason}`.
  """
  def reduce_ok(list, acc, fun) do
    do_reduce_ok(list, acc, fun)
  end

  defp do_reduce_ok([head | tail], acc, fun) do
    case fun.(head, acc) do
      {:ok, acc} ->
        do_reduce_ok(tail, acc, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_reduce_ok([], acc, _fun), do: {:ok, acc}

  @doc """
  Like `Enum.map_reduce/3` but only continues while `fun` returns `{:ok, elem, acc}`
  and stops on `{:error, reason}`.
  """
  def map_reduce_ok(list, acc, fun) do
    do_map_reduce_ok(list, [], acc, fun)
  end

  defp do_map_reduce_ok([head | tail], list, acc, fun) do
    case fun.(head, acc) do
      {:ok, elem, acc} ->
        do_map_reduce_ok(tail, [elem | list], acc, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_map_reduce_ok([], list, acc, _fun), do: {:ok, Enum.reverse(list), acc}
end
