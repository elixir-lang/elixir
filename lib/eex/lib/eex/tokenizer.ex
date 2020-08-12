defmodule EEx.Tokenizer do
  @moduledoc false

  @type content :: IO.chardata()
  @type line :: non_neg_integer
  @type column :: non_neg_integer
  @type marker :: '=' | '/' | '|' | ''
  @type token ::
          {:text, content}
          | {:expr | :start_expr | :middle_expr | :end_expr, line, column, marker, content}
          | {:eof, line, column}

  @spaces [?\s, ?\t]

  @doc """
  Tokenizes the given charlist or binary.

  It returns {:ok, list} with the following tokens:

    * `{:text, content}`
    * `{:expr, line, column, marker, content}`
    * `{:start_expr, line, column, marker, content}`
    * `{:middle_expr, line, column, marker, content}`
    * `{:end_expr, line, column, marker, content}`
    * `{:eof, line, column}`

  Or `{:error, line, column, message}` in case of errors.
  """
  @spec tokenize(binary | charlist, line, column, map) ::
          {:ok, [token]} | {:error, line, column, String.t()}

  def tokenize(bin, line, column, opts) when is_binary(bin) do
    tokenize(String.to_charlist(bin), line, column, opts)
  end

  def tokenize(list, line, column, opts)
      when is_list(list) and is_integer(line) and line >= 0 and is_integer(column) and column >= 0 do
    column = opts.indentation + column
    {list, line, column} = (opts.trim && trim_init(list, line, column)) || {list, line, column}
    tokenize(list, line, column, opts, [], [])
  end

  defp tokenize('<%%' ++ t, line, column, opts, buffer, acc) do
    tokenize(t, line, column + 3, opts, [?%, ?< | buffer], acc)
  end

  defp tokenize('<%#' ++ t, line, column, opts, buffer, acc) do
    case expr(t, line, column + 3, opts, []) do
      {:error, _, _, _} = error ->
        error

      {:ok, _, new_line, new_column, rest} ->
        {rest, new_line, new_column, buffer} =
          trim_if_needed(rest, new_line, new_column, opts, buffer)

        tokenize(rest, new_line, new_column, opts, buffer, acc)
    end
  end

  defp tokenize('<%' ++ t, line, column, opts, buffer, acc) do
    {marker, t} = retrieve_marker(t)

    case expr(t, line, column + 2 + length(marker), opts, []) do
      {:error, _, _, _} = error ->
        error

      {:ok, expr, new_line, new_column, rest} ->
        key =
          case :elixir_tokenizer.tokenize(expr, 1, file: "eex", check_terminators: false) do
            {:ok, tokens} -> token_key(tokens)
            {:error, _, _, _} -> :expr
          end

        {rest, new_line, new_column, buffer} =
          trim_if_needed(rest, new_line, new_column, opts, buffer)

        acc = tokenize_text(buffer, acc)
        final = {key, line, column, marker, expr}
        tokenize(rest, new_line, new_column, opts, [], [final | acc])
    end
  end

  defp tokenize('\n' ++ t, line, _column, opts, buffer, acc) do
    tokenize(t, line + 1, opts.indentation + 1, opts, [?\n | buffer], acc)
  end

  defp tokenize([h | t], line, column, opts, buffer, acc) do
    tokenize(t, line, column + 1, opts, [h | buffer], acc)
  end

  defp tokenize([], line, column, _opts, buffer, acc) do
    eof = {:eof, line, column}
    {:ok, Enum.reverse([eof | tokenize_text(buffer, acc)])}
  end

  # Retrieve marker for <%

  defp retrieve_marker([marker | t]) when marker in [?=, ?/, ?|] do
    {[marker], t}
  end

  defp retrieve_marker(t) do
    {'', t}
  end

  # Tokenize an expression until we find %>

  defp expr([?%, ?> | t], line, column, _opts, buffer) do
    {:ok, Enum.reverse(buffer), line, column + 2, t}
  end

  defp expr('\n' ++ t, line, _column, opts, buffer) do
    expr(t, line + 1, opts.indentation + 1, opts, [?\n | buffer])
  end

  defp expr([h | t], line, column, opts, buffer) do
    expr(t, line, column + 1, opts, [h | buffer])
  end

  defp expr([], line, column, _opts, _buffer) do
    {:error, line, column, "missing token '%>'"}
  end

  # Receives tokens and check if it is a start, middle or an end token.
  defp token_key(tokens) do
    case {tokens, Enum.reverse(tokens)} do
      {[{:end, _} | _], [{:do, _} | _]} ->
        :middle_expr

      {_, [{:do, _} | _]} ->
        :start_expr

      {_, [{:block_identifier, _, _} | _]} ->
        :middle_expr

      {[{:end, _} | _], [{:stab_op, _, _} | _]} ->
        :middle_expr

      {_, [{:stab_op, _, _} | reverse_tokens]} ->
        fn_index = Enum.find_index(reverse_tokens, &match?({:fn, _}, &1)) || :infinity
        end_index = Enum.find_index(reverse_tokens, &match?({:end, _}, &1)) || :infinity

        if end_index > fn_index do
          :start_expr
        else
          :middle_expr
        end

      {tokens, _} ->
        case Enum.drop_while(tokens, &closing_bracket?/1) do
          [{:end, _} | _] -> :end_expr
          _ -> :expr
        end
    end
  end

  defp closing_bracket?({closing, _}) when closing in ~w"( [ {"a, do: true
  defp closing_bracket?(_), do: false

  # Tokenize the buffered text by appending
  # it to the given accumulator.

  defp tokenize_text([], acc) do
    acc
  end

  defp tokenize_text(buffer, acc) do
    [{:text, Enum.reverse(buffer)} | acc]
  end

  defp trim_if_needed(rest, line, column, opts, buffer) do
    if opts.trim do
      buffer = trim_left(buffer, 0)
      {rest, line, column} = trim_right(rest, line, column, 0)
      {rest, line, column, buffer}
    else
      {rest, line, column, buffer}
    end
  end

  defp trim_init([h | t], line, column) when h in @spaces, do: trim_init(t, line, column + 1)
  defp trim_init([?\r, ?\n | t], line, _column), do: trim_init(t, line + 1, 1)
  defp trim_init([?\n | t], line, _column), do: trim_init(t, line + 1, 1)
  defp trim_init([?<, ?% | _] = rest, line, column), do: {rest, line, column}
  defp trim_init(_, _, _), do: false

  defp trim_left(buffer, count) do
    case trim_whitespace(buffer) do
      [?\n, ?\r | rest] -> trim_left(rest, count + 1)
      [?\n | rest] -> trim_left(rest, count + 1)
      _ when count > 0 -> [?\n | buffer]
      _ -> buffer
    end
  end

  defp trim_right(rest, line, column, count) do
    case trim_whitespace(rest) do
      [?\r, ?\n | rest] -> trim_right(rest, line + 1, 1, count + 1)
      [?\n | rest] -> trim_right(rest, line + 1, 1, count + 1)
      [] -> {[], line, column + length(rest)}
      _ when count > 0 -> {[?\n | rest], line - 1, column}
      _ -> {rest, line, column}
    end
  end

  defp trim_whitespace([h | t]) when h in @spaces, do: trim_whitespace(t)
  defp trim_whitespace(list), do: list
end
