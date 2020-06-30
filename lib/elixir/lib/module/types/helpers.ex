defmodule Module.Types.Helpers do
  @moduledoc false

  alias Module.Types.Infer

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
    %{stack | last_expr: expr}
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
  Like `Enum.each/2` but only continues while `fun` returns `:ok`
  and stops on `{:error, reason}`.
  """
  def each_ok([head | tail], fun) do
    case fun.(head) do
      :ok -> each_ok(tail, fun)
      {:error, reason} -> {:error, reason}
    end
  end

  def each_ok([], _fun), do: :ok

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

  def of_binary({:"::", _meta, [expr, specifiers]}, stack, context, fun) do
    expected_type =
      collect_binary_specifier(specifiers, &binary_type(stack.context, &1)) || :integer

    utf? = collect_binary_specifier(specifiers, &utf_type?/1)
    float? = collect_binary_specifier(specifiers, &float_type?/1)

    # Special case utf and float specifiers because they can be two types as literals
    # but only a specific type as a variable in a pattern
    cond do
      stack.context == :pattern and utf? and is_binary(expr) ->
        {:ok, context}

      stack.context == :pattern and float? and is_integer(expr) ->
        {:ok, context}

      true ->
        with {:ok, type, context} <- fun.(expr, stack, context),
             {:ok, _type, context} <- Infer.unify(type, expected_type, stack, context),
             do: {:ok, context}
    end
  end

  def of_binary(expr, stack, context, fun) do
    case fun.(expr, stack, context) do
      {:ok, type, context} when type in [:integer, :float, :number, :binary, :dynamic] ->
        {:ok, context}

      {:ok, type, _context} ->
        {:error, {:invalid_binary_type, type}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Collect binary type specifiers,
  # from `<<pattern::integer-size(10)>>` collect `integer`
  defp collect_binary_specifier({:-, _meta, [left, right]}, fun) do
    collect_binary_specifier(left, fun) || collect_binary_specifier(right, fun)
  end

  defp collect_binary_specifier(other, fun) do
    fun.(other)
  end

  defp binary_type(:expr, {:float, _, _}), do: :number
  defp binary_type(:expr, {:utf8, _, _}), do: {:union, [:integer, :binary]}
  defp binary_type(:expr, {:utf16, _, _}), do: {:union, [:integer, :binary]}
  defp binary_type(:expr, {:utf32, _, _}), do: {:union, [:integer, :binary]}
  defp binary_type(:pattern, {:utf8, _, _}), do: :integer
  defp binary_type(:pattern, {:utf16, _, _}), do: :integer
  defp binary_type(:pattern, {:utf32, _, _}), do: :integer
  defp binary_type(:pattern, {:float, _, _}), do: :float
  defp binary_type(_context, {:integer, _, _}), do: :integer
  defp binary_type(_context, {:bits, _, _}), do: :binary
  defp binary_type(_context, {:bitstring, _, _}), do: :binary
  defp binary_type(_context, {:bytes, _, _}), do: :binary
  defp binary_type(_context, {:binary, _, _}), do: :binary
  defp binary_type(_context, _specifier), do: nil

  defp utf_type?({specifier, _, _}), do: specifier in [:utf8, :utf16, :utf32]
  defp utf_type?(_), do: false

  defp float_type?({:float, _, _}), do: true
  defp float_type?(_), do: false

  # TODO: Remove this and let multiple when be treated as multiple clauses,
  #       meaning they will be intersection types
  # TODO
  def guards_to_or([]) do
    []
  end

  def guards_to_or(guards) do
    Enum.reduce(guards, fn guard, acc -> {{:., [], [:erlang, :orelse]}, [], [guard, acc]} end)
  end
end
