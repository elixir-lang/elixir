defmodule EEx.Tokenizer do
  @moduledoc false

  @doc """
  Tokenizes the given char list or binary.
  It returns 4 different types of tokens as result:

  * { :text, line, contents }
  * { :expr, line, marker, contents }
  * { :start_expr, line, marker, contents }
  * { :end_expr, line, marker, contents }

  """
  def tokenize(bin, line) when is_binary(bin) do
    tokenize(:unicode.characters_to_list(bin), line)
  end

  def tokenize(list, line) do
    Enum.reverse(tokenize(list, line, line, [], []))
  end

  defp tokenize('<%%' ++ t, current_line, line, buffer, acc) do
    { buffer, new_line, rest } = tokenize_expr t, line, [?%, ?<|buffer]
    tokenize rest, current_line, new_line, [?>, ?%|buffer], acc
  end

  defp tokenize('<%#' ++ t, current_line, line, buffer, acc) do
    { _, new_line, rest } = tokenize_expr t, line, []
    tokenize rest, current_line, new_line, buffer, acc
  end

  defp tokenize('<%' ++ t, current_line, line, buffer, acc) do
    { marker, t } = retrieve_marker(t)
    { expr, new_line, rest } = tokenize_expr t, line, []

    token = token_name(expr)
    acc   = tokenize_text(current_line, buffer, acc)
    final = { token, line, marker, Enum.reverse(expr) }
    tokenize rest, new_line, new_line, [], [final | acc]
  end

  defp tokenize('\n' ++ t, current_line, line, buffer, acc) do
    tokenize t, current_line, line + 1, [?\n|buffer], acc
  end

  defp tokenize([h|t], current_line, line, buffer, acc) do
    tokenize t, current_line, line, [h|buffer], acc
  end

  defp tokenize([], current_line, _line, buffer, acc) do
    tokenize_text(current_line, buffer, acc)
  end

  # Retrieve marker for <%

  defp retrieve_marker('=' ++ t) do
    { "=", t }
  end

  defp retrieve_marker(t) do
    { "", t }
  end

  # Tokenize an expression until we find %>

  defp tokenize_expr([?%, ?>|t], line, buffer) do
    { buffer, line, t }
  end

  defp tokenize_expr('\n' ++ t, line, buffer) do
    tokenize_expr t, line + 1, [?\n|buffer]
  end

  defp tokenize_expr([h|t], line, buffer) do
    tokenize_expr t, line, [h|buffer]
  end

  defp tokenize_expr([], _line, _buffer) do
    raise EEx.SyntaxError, message: "missing token: %>"
  end

  # Receive an expression content and check
  # if it is a start, middle or an end token.
  #
  # Start tokens finish with `do` and `fn ->`
  # Middle tokens are marked with `->` or keywords
  # End tokens contain only the end word

  defp token_name([h|t]) when h in [?\s, ?\t] do
    token_name(t)
  end

  defp token_name('od' ++ [h|_]) when h in [?\s, ?\t, ?)] do
    :start_expr
  end

  defp token_name('>-' ++ rest) do
    rest = Enum.reverse(rest)

    # Tokenize the remaining passing check_terminators as
    # false, which relax the tokenizer to not error on
    # unmatched pairs. Then, we check if there is a "fn"
    # token and, if so, it is not followed by an "end"
    # token. If this is the case, we are on a start expr.
    case :elixir_tokenizer.tokenize(rest, 1, file: "eex", check_terminators: false) do
      { :ok, tokens } ->
        tokens   = Enum.reverse(tokens)
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

  defp token_name('esle' ++ t),   do: check_spaces(t, :middle_expr)
  defp token_name('retfa' ++ t),  do: check_spaces(t, :middle_expr)
  defp token_name('hctac' ++ t),  do: check_spaces(t, :middle_expr)
  defp token_name('eucser' ++ t), do: check_spaces(t, :middle_expr)
  defp token_name('dne' ++ t),    do: check_spaces(t, :end_expr)

  defp token_name(_) do
    :expr
  end

  defp fn_index(tokens) do
    Enum.find_index tokens, fn
      { :fn_paren, _ } -> true
      { :fn, _ }       -> true
      _                -> false
    end
  end

  defp end_index(tokens) do
    Enum.find_index(tokens, match?({ :end, _ }, &1)) || :infinity
  end

  defp check_spaces(string, token) do
    if only_spaces?(string), do: token, else: :expr
  end

  defp only_spaces?([h|t]) when h in [?\s, ?\t], do: only_spaces?(t)
  defp only_spaces?(other), do: other == []

  # Tokenize the buffered text by appending
  # it to the given accumulator.

  defp tokenize_text(_line, [], acc) do
    acc
  end

  defp tokenize_text(line, buffer, acc) do
    [{ :text, line, :unicode.characters_to_binary(Enum.reverse(buffer)) } | acc]
  end
end
