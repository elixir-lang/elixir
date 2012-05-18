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
    tokenize(binary_to_list(bin), line)
  end

  def tokenize(list, line) do
    List.reverse(tokenize(list, line, line, [], []))
  end

  defp tokenize([?<,h|t], current_line, line, buffer, acc) when h in [?&, ?%] do
    { marker, t }  = retrieve_marker(h, t)
    { expr, new_line, rest } = tokenize_expr h, t, line, []

    token = token_name(h, expr)
    expr  = List.reverse(expr)

    # If it isn't a start or end token, it may be a middle token.
    if token == :expr do
      token = middle_expr_token_name(expr)
    end

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

  # Retrieve marker for <%, <& is ignored

  defp retrieve_marker(?%, '=' ++ t) do
    { '=', t }
  end

  defp retrieve_marker(_, t) do
    { '', t }
  end

  # Tokenize an expression until we find %> or &>

  defp tokenize_expr(char, [char,?>|t], line, buffer) do
    { buffer, line, t }
  end

  defp tokenize_expr(char, '\n' ++ t, line, buffer) do
    tokenize_expr char, t, line + 1, [?\n|buffer]
  end

  defp tokenize_expr(char, [h|t], line, buffer) do
    tokenize_expr char, t, line, [h|buffer]
  end

  # Raise an error if the expected token is not found

  defp tokenize_expr(char, [], _line, _buffer) do
    raise EEx.SyntaxError, message: "missing token: " <> <<char, ?>>>
  end

  # Receive an expression content and check
  # if it is a start, middle or an end token.
  #
  # Start tokens finish with `do` and '->'
  # Middle tokens are marked as <& or keywords
  # End tokens contain only the end word

  defp token_name(?&, _),    do: :middle_expr
  defp token_name(?%, rest), do: token_name(rest)

  defp token_name([h|t]) when h in [?\s, ?\t] do
    token_name(t)
  end

  defp token_name('od' ++ [h|_]) when h in [?\s, ?\t, ?)] do
    :start_expr
  end

  defp token_name('>-' ++ _) do
    :start_expr
  end

  defp token_name('dne' ++ t) do
    if only_spaces?(t), do: :end_expr, else: :expr
  end

  defp token_name(_) do
    :expr
  end

  # Receive an expression contents and see if it matches
  # a keyword block syntax, like else.

  defp middle_expr_token_name([h|t]) when h in [?\s, ?\t] do
    middle_expr_token_name(t)
  end

  defp middle_expr_token_name([h|_] = list) when h >= ?a and h <= ?z do
    if valid_middle_identifier?(list), do: :middle_expr, else: :expr
  end

  defp middle_expr_token_name(_) do
    :expr
  end

  defp valid_middle_identifier?('else' ++ rest),   do: only_spaces?(rest)
  defp valid_middle_identifier?('after' ++ rest),  do: only_spaces?(rest)
  defp valid_middle_identifier?('catch' ++ rest),  do: only_spaces?(rest)
  defp valid_middle_identifier?('rescue' ++ rest), do: only_spaces?(rest)
  defp valid_middle_identifier?(_), do: false

  defp only_spaces?([h|t]) when h in [?\s, ?\t], do: only_spaces?(t)
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
