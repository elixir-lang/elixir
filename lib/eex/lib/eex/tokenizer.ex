defmodule EEx.Tokenizer do
  @moduledoc false

  @type content :: IO.chardata()
  @type line :: non_neg_integer
  @type column :: non_neg_integer
  @type marker :: '=' | '/' | '|' | ''
  @type token ::
          {:text, content}
          | {:expr | :start_expr | :middle_expr | :end_expr, line, marker, content}

  @spaces [?\s, ?\t]
  @closing_brackets ')]}'

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
  @spec tokenize(binary | charlist, line, column, keyword) ::
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
        token = token_name(expr)

        {rest, new_line, new_column, buffer} =
          trim_if_needed(rest, new_line, new_column, opts, buffer)

        acc = tokenize_text(buffer, acc)
        final = {token, line, column, marker, Enum.reverse(expr)}
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
    {:ok, buffer, line, column + 2, t}
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

  # Receive an expression content and check
  # if it is a start, middle or an end token.
  #
  # Start tokens finish with "do" and "fn ->"
  # Middle tokens are marked with "->" or keywords
  # End tokens contain only the end word and optionally
  # combinations of ")", "]" and "}".

  defp token_name([h | t]) when h in @spaces do
    token_name(t)
  end

  defp token_name('od' ++ [h | rest]) when h in @spaces or h in @closing_brackets do
    case tokenize_rest(rest) do
      {:ok, [{:end, _} | _]} -> :middle_expr
      _ -> :start_expr
    end
  end

  defp token_name('>-' ++ rest) do
    case tokenize_rest(rest) do
      {:ok, [{:end, _} | _]} ->
        :middle_expr

      # Check if there is a "fn" token and, if so, it is not
      # followed by an "end" token. If this is the case, we
      # are on a start expr.
      {:ok, tokens} ->
        tokens = Enum.reverse(tokens)
        fn_index = fn_index(tokens)

        if fn_index && end_index(tokens) > fn_index do
          :start_expr
        else
          :middle_expr
        end

      _error ->
        :middle_expr
    end
  end

  defp token_name('esle' ++ t), do: check_spaces(t, :middle_expr)
  defp token_name('retfa' ++ t), do: check_spaces(t, :middle_expr)
  defp token_name('hctac' ++ t), do: check_spaces(t, :middle_expr)
  defp token_name('eucser' ++ t), do: check_spaces(t, :middle_expr)

  defp token_name(rest) do
    case Enum.drop_while(rest, &(&1 in @spaces or &1 in @closing_brackets)) do
      'dne' ++ t -> check_spaces(t, :end_expr)
      _ -> :expr
    end
  end

  # Tokenize the remaining passing check_terminators as false,
  # which relax the tokenizer to not error on unmatched pairs.
  # If the tokens start with an "end" we have a middle expr.
  defp tokenize_rest(rest) do
    :elixir_tokenizer.tokenize(Enum.reverse(rest), 1, file: "eex", check_terminators: false)
  end

  defp fn_index(tokens) do
    Enum.find_index(tokens, fn
      {:fn_paren, _} -> true
      {:fn, _} -> true
      _ -> false
    end)
  end

  defp end_index(tokens) do
    Enum.find_index(tokens, &match?({:end, _}, &1)) || :infinity
  end

  defp check_spaces(string, token) do
    if Enum.all?(string, &(&1 in @spaces)) do
      token
    else
      :expr
    end
  end

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
