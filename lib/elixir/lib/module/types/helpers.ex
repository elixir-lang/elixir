defmodule Module.Types.Helpers do
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
  Returns unique identifier for the current assignment of the variable.
  """
  def var_name({_name, meta, _context}), do: Keyword.fetch!(meta, :version)

  @doc """
  Push expression to stack.

  The expression stack is used to give the context where a type variable
  was refined when show a type conflict error.
  """
  def push_expr_stack(expr, stack) do
    %{stack | expr_stack: [expr | stack.expr_stack]}
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

      result when elem(result, 0) == :ok ->
        result = Tuple.delete_at(result, 0)
        do_reduce_ok(tail, result, fun)

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

      result when elem(result, 0) == :ok ->
        result = Tuple.delete_at(result, 0)
        do_map_ok(tail, [result | acc], fun)

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
  Given a list of `[{:ok, term()} | {:error, term()}]` it returns a list of
  errors `{:error, [term()]}` in case of at least one error or `{:ok, [term()]}`
  if there are no errors.
  """
  def oks_or_errors(list) do
    case Enum.split_with(list, &match?({:ok, _}, &1)) do
      {oks, []} -> {:ok, Enum.map(oks, fn {:ok, ok} -> ok end)}
      {_oks, errors} -> {:error, Enum.map(errors, fn {:error, error} -> error end)}
    end
  end
end
