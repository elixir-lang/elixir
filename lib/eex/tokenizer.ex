defmodule EEx::Tokenizer do
  # TODO: Add errors scenarios

  @doc """
  Tokenizes the given char list. It returns 4 tokens as result:

  * { :text, contents }
  * { :expr, marker, contents}
  * { :start_expr, marker, contents}
  * { :end_expr, marker, contents}

  """
  def tokenize(bin) when is_binary(bin) do
    tokenize(binary_to_list(bin))
  end

  def tokenize(list) do
    List.reverse(tokenize(list, [], []))
  end

  defp tokenize('<%' ++ t, buffer, acc) do
    { marker, t }  = retrieve_marker(t)
    { expr, rest } = tokenize_expr t, []

    token = tip_expr_token_name(expr)
    expr  = List.reverse(expr)

    # If it isn't a start or end token, it may be a middle token.
    if token == :expr, do:
      token = middle_expr_token_name(expr)

    acc = tokenize_text(buffer, acc)
    tokenize rest, [], [ { token, marker, list_to_binary(expr) } | acc]
  end

  defp tokenize([h|t], buffer, acc) do
    tokenize t, [h|buffer], acc
  end

  defp tokenize([], buffer, acc) do
    tokenize_text(buffer, acc)
  end

  # Retrieve marker for <%

  defp retrieve_marker('=' ++ t) do
    { '=', t }
  end

  defp retrieve_marker(t) do
    { '', t }
  end

  # Tokenize an expression until we find %>

  defp tokenize_expr('%>' ++ t, buffer) do
    { buffer, t }
  end

  defp tokenize_expr([h|t], buffer) do
    tokenize_expr t, [h|buffer]
  end

  # Receive an expression content and check
  # if it is a start or an end token.
  # Start tokens finish with `do` or `->`
  # while end tokens contain only the end word.

  defp tip_expr_token_name([h|t]) when h == ?\s orelse h == ?\t do
    tip_expr_token_name(t)
  end

  defp tip_expr_token_name('od' ++ [h|_]) when h == ?\s orelse h == ?\t orelse h == ?) do
    :start_expr
  end

  defp tip_expr_token_name('>-' ++ [h|_]) when h == ?\s orelse h == ?\t orelse h == ?) do
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

  defp middle_expr_token_name([h|t]) when h == ?\s orelse h == ?\t do
    middle_expr_token_name(t)
  end

  defp middle_expr_token_name([h|t]) when h >= ?a andalso h <= ?z do
    if valid_key_identifier?(t), do: :middle_expr, else: :expr
  end

  defp middle_expr_token_name(_) do
    :expr
  end

  defp valid_key_identifier?([h|t]) \
      when h >= ?a andalso h <= ?z   \
      when h >= ?A andalso h <= ?Z   \
      when h >= ?0 andalso h <= ?9 do
    valid_key_identifier?(t)
  end

  defp valid_key_identifier?([?:|_]) do
    true
  end

  defp valid_key_identifier?(_) do
    false
  end

  defp only_spaces?([h|t]) when h == ?\s orelse h == ?\t, do: only_spaces?(t)
  defp only_spaces?(other), do: other == []

  # Tokenize the buffered text by appending
  # it to the given accumulator.

  defp tokenize_text([], acc) do
    acc
  end

  defp tokenize_text(buffer, acc) do
    [{ :text, list_to_binary(List.reverse(buffer)) } | acc]
  end
end
