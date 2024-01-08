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
      :inferred_integer_bitstring ->
        """

        #{hint()} all expressions given to binaries are assumed to be of type \
        integer() unless said otherwise. For example, <<expr>> assumes "expr" \
        is an integer. Pass a modifier, such as <<expr::float>> or <<expr::binary>>, \
        to change the default behaviour.
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
  Emits a warnings.
  """
  def warn(module, warning, meta, stack, context) do
    {fun, arity} = stack.function
    location = {stack.file, meta, {stack.module, fun, arity}}
    %{context | warnings: [{module, warning, location} | context.warnings]}
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
  Like `Enum.unzip/1` but only continues while `fun` returns `{:ok, elem1, elem2}`
  and stops on `{:error, reason}`.
  """
  def unzip_ok(list) do
    do_unzip_ok(list, [], [])
  end

  defp do_unzip_ok([{:ok, head1, head2} | tail], acc1, acc2) do
    do_unzip_ok(tail, [head1 | acc1], [head2 | acc2])
  end

  defp do_unzip_ok([{:error, reason} | _tail], _acc1, _acc2), do: {:error, reason}

  defp do_unzip_ok([], acc1, acc2), do: {:ok, Enum.reverse(acc1), Enum.reverse(acc2)}

  @doc """
  Like `Enum.map/2` but only continues while `fun` returns `{:ok, elem}`
  and stops on `{:error, reason}`.
  """
  def map_ok(list, fun) do
    do_map_ok(list, [], fun)
  end

  defp do_map_ok([head | tail], acc, fun) do
    case fun.(head) do
      {:ok, elem} ->
        do_map_ok(tail, [elem | acc], fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_map_ok([], acc, _fun), do: {:ok, Enum.reverse(acc)}

  @doc """
  Like `Enum.map_reduce/3` but only continues while `fun` returns `{:ok, elem, acc}`
  and stops on `{:error, reason}`.
  """
  def map_reduce_ok(list, acc, fun) do
    do_map_reduce_ok(list, {[], acc}, fun)
  end

  defp do_map_reduce_ok([head | tail], {list, acc}, fun) do
    case fun.(head, acc) do
      {:ok, elem, acc} ->
        do_map_reduce_ok(tail, {[elem | list], acc}, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_map_reduce_ok([], {list, acc}, _fun), do: {:ok, Enum.reverse(list), acc}

  @doc """
  Like `Enum.flat_map/2` but only continues while `fun` returns `{:ok, list}`
  and stops on `{:error, reason}`.
  """
  def flat_map_ok(list, fun) do
    do_flat_map_ok(list, [], fun)
  end

  defp do_flat_map_ok([head | tail], acc, fun) do
    case fun.(head) do
      {:ok, elem} ->
        do_flat_map_ok(tail, [elem | acc], fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_flat_map_ok([], acc, _fun), do: {:ok, Enum.reverse(Enum.concat(acc))}

  @doc """
  Like `Enum.flat_map_reduce/3` but only continues while `fun` returns `{:ok, list, acc}`
  and stops on `{:error, reason}`.
  """
  def flat_map_reduce_ok(list, acc, fun) do
    do_flat_map_reduce_ok(list, {[], acc}, fun)
  end

  defp do_flat_map_reduce_ok([head | tail], {list, acc}, fun) do
    case fun.(head, acc) do
      {:ok, elems, acc} ->
        do_flat_map_reduce_ok(tail, {[elems | list], acc}, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_flat_map_reduce_ok([], {list, acc}, _fun),
    do: {:ok, Enum.reverse(Enum.concat(list)), acc}

  @doc """
  Like `Enum.zip/1` but will zip multiple lists together instead of only two.
  """
  def zip_many(lists) do
    zip_many(lists, [], [[]])
  end

  defp zip_many([], [], [[] | acc]) do
    map_reverse(acc, [], &Enum.reverse/1)
  end

  defp zip_many([], remain, [last | acc]) do
    zip_many(Enum.reverse(remain), [], [[] | [last | acc]])
  end

  defp zip_many([[] | _], remain, [last | acc]) do
    zip_many(Enum.reverse(remain), [], [last | acc])
  end

  defp zip_many([[elem | list1] | list2], remain, [last | acc]) do
    zip_many(list2, [list1 | remain], [[elem | last] | acc])
  end

  defp map_reverse([], acc, _fun), do: acc
  defp map_reverse([head | tail], acc, fun), do: map_reverse(tail, [fun.(head) | acc], fun)
end
