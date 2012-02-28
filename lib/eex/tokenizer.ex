defmodule EEx::Tokenizer do
  # TODO: Add errors scenarios

  @doc """
  Tokenizes the given char list. It returns 4 tokens as result:

  * { :text, contents }
  * { :expr, marker, contents}
  * { :start_expr, marker, contents}
  * { :end_expr, marker, contents}

  """
  def tokenize(bin, line) when is_binary(bin) do
    tokenize(binary_to_list(bin), line)
  end

  def tokenize(list, line) do
    List.reverse(tokenize(list, line, line, [], []))
  end

  defp tokenize('<%' ++ t, current_line, line, buffer, acc) do
    { marker, t }  = retrieve_marker(t)
    { expr, new_line, rest } = tokenize_expr t, line, []

    token = tip_expr_token_name(expr)
    expr  = List.reverse(expr)

    # If it isn't a start or end token, it may be a middle token.
    if token == :expr, do:
      token = middle_expr_token_name(expr)

    acc = tokenize_text(current_line, buffer, acc)
    tokenize rest, new_line, new_line, [], [ { token, line, marker, expr } | acc]
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
    { '=', t }
  end

  defp retrieve_marker(t) do
    { '', t }
  end

  # Tokenize an expression until we find %>

  defp tokenize_expr('%>' ++ t, line, buffer) do
    { buffer, line, t }
  end

  defp tokenize_expr('\n' ++ t, line, buffer) do
    tokenize_expr t, line + 1, [?\n|buffer]
  end

  defp tokenize_expr([h|t], line, buffer) do
    tokenize_expr t, line, [h|buffer]
  end

  # Raise an error if the %> is not found

  defp tokenize_expr([], _line, buffer) do
    raise EEx::SyntaxError, message: "invalid token: #{inspect List.reverse(buffer)}"
  end

  # Receive an expression content and check
  # if it is a start or an end token.
  # Start tokens finish with `do` or `->`
  # while end tokens contain only the end word.

  defp tip_expr_token_name([h|t]) when h == ?\s or h == ?\t do
    tip_expr_token_name(t)
  end

  defp tip_expr_token_name('od' ++ [h|_]) when h == ?\s or h == ?\t or h == ?) do
    :start_expr
  end

  defp tip_expr_token_name('>-' ++ [h|_]) when h == ?\s or h == ?\t or h == ?) do
    :start_expr
  end

  defp tip_expr_token_name('dne' ++ t) do
    if only_spaces?(t), do: :end_expr, else: :expr
  end

  defp tip_expr_token_name(_) do
    :expr
  end

  # Receive an expression contents and see if it matches
  # a key-value arg syntax, like elsif: foo.

  defp middle_expr_token_name([h|t]) when h == ?\s or h == ?\t do
    middle_expr_token_name(t)
  end

  defp middle_expr_token_name([h|t]) when h >= ?a and h <= ?z do
    if valid_key_identifier?(t), do: :middle_expr, else: :expr
  end

  defp middle_expr_token_name(_) do
    :expr
  end

  defp valid_key_identifier?([h|t]) \
      when h >= ?a and h <= ?z      \
      when h >= ?A and h <= ?Z      \
      when h >= ?0 and h <= ?9 do
    valid_key_identifier?(t)
  end

  defp valid_key_identifier?([?:|_]) do
    true
  end

  defp valid_key_identifier?(_) do
    false
  end

  defp only_spaces?([h|t]) when h == ?\s or h == ?\t, do: only_spaces?(t)
  defp only_spaces?(other), do: other == []

  # Tokenize the buffered text by appending
  # it to the given accumulator.

  defp tokenize_text(_line, [], acc) do
    acc
  end

  defp tokenize_text(line, buffer, acc) do
    [{ :text, line, list_to_binary(List.reverse(buffer)) } | acc]
  end
end
