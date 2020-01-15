defmodule EEx.Tokenizer do
  @moduledoc false

  @type content :: IO.chardata()
  @type line :: non_neg_integer
  @type column :: non_neg_integer
  @type marker :: '=' | '/' | '|' | ''
  @type trimmed? :: boolean
  @type token ::
          {:text, content}
          | {:expr | :start_expr | :middle_expr | :end_expr, line, marker, content, trimmed?}

  @spaces [?\s, ?\t]
  @closing_brackets ')]}'

  @doc """
  Tokenizes the given charlist or binary.

  It returns {:ok, list} with the following tokens:

    * `{:text, content}`
    * `{:expr, line, column, marker, content, trimmed?}`
    * `{:start_expr, line, column, marker, content, trimmed?}`
    * `{:middle_expr, line, column, marker, content, trimmed?}`
    * `{:end_expr, line, column, marker, content, trimmed?}`
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
    tokenize(list, line, opts.indentation + column, opts, [], [])
  end

  defp tokenize('<%%' ++ t, line, column, opts, buffer, acc) do
    tokenize(t, line, column + 3, opts, [?%, ?< | buffer], acc)
  end

  defp tokenize('<%#' ++ t, line, column, opts, buffer, acc) do
    case expr(t, line, column + 3, opts, []) do
      {:error, _, _, _} = error ->
        error

      {:ok, _, new_line, new_column, rest} ->
        {_, rest, new_line, new_column, buffer} =
          trim_if_needed(rest, new_line, new_column, opts, buffer, acc)

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

        {trimmed?, rest, new_line, new_column, buffer} =
          trim_if_needed(rest, new_line, new_column, opts, buffer, acc)

        expr = pad_if_needed(token, expr, trimmed?)
        acc = tokenize_text(buffer, acc)
        final = {token, line, column, marker, Enum.reverse(expr), trimmed?}
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

  # If trim mode is enabled and the token is on a line with
  # only itself and whitespace, trim the whitespace around it,
  # including the line break following it if there is one.
  defp trim_if_needed(rest, line, column, opts, buffer, acc) do
    with true <- opts.trim,
         {true, new_buffer} <- trim_left(buffer, acc),
         {true, new_rest, new_line, new_column} <- trim_right(rest, line, column) do
      {true, new_rest, new_line, new_column, new_buffer}
    else
      _ -> {false, rest, line, column, buffer}
    end
  end

  defp trim_left(buffer, acc) do
    case {trim_whitespace(buffer), acc} do
      {[?\n | _] = trimmed_buffer, _} -> {true, trimmed_buffer}
      {[], [{_, _, _, _, _, true} | _]} -> {true, []}
      {[], []} -> {true, []}
      _ -> {false, buffer}
    end
  end

  defp trim_right(rest, line, column) do
    case trim_whitespace(rest) do
      [?\r, ?\n | trimmed_rest] -> {true, trimmed_rest, line + 1, 1}
      [?\n | trimmed_rest] -> {true, trimmed_rest, line + 1, 1}
      [] -> {true, [], line, column}
      _ -> {false, rest, line, column}
    end
  end

  defp trim_whitespace([h | t]) when h in @spaces do
    trim_whitespace(t)
  end

  defp trim_whitespace(list) do
    list
  end

  defp pad_if_needed(:start_expr, [h | _] = expr, true) when h not in @spaces, do: [?\s | expr]
  defp pad_if_needed(_, expr, _), do: expr
end
