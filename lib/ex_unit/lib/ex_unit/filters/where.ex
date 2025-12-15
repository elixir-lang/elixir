# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule ExUnit.Filters.Where do
  @moduledoc false

  @doc """
  Parses a where expression string into an AST.

  ## Grammar

      expr     ::= or_expr
      or_expr  ::= and_expr ('or' and_expr)*
      and_expr ::= not_expr ('and' not_expr)*
      not_expr ::= 'not' not_expr | primary
      primary  ::= '(' expr ')' | tag
      tag      ::= IDENTIFIER (':' VALUE)?

  ## Examples

      iex> ExUnit.Filters.Where.parse("slow")
      {:ok, {:tag, :slow}}

      iex> ExUnit.Filters.Where.parse("slow and fast")
      {:ok, {:and, {:tag, :slow}, {:tag, :fast}}}

      iex> ExUnit.Filters.Where.parse("slow or fast")
      {:ok, {:or, {:tag, :slow}, {:tag, :fast}}}

      iex> ExUnit.Filters.Where.parse("not slow")
      {:ok, {:not, {:tag, :slow}}}

      iex> ExUnit.Filters.Where.parse("interface:ui")
      {:ok, {:tag, {:interface, "ui"}}}

  """
  @spec parse(String.t()) :: {:ok, term} | {:error, String.t()}
  def parse(string) do
    case tokenize(string) do
      {:ok, tokens} ->
        case parse_expr(tokens) do
          {expr, []} -> {:ok, expr}
          {_expr, rest} -> {:error, "unexpected tokens: #{inspect(rest)}"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Evaluates a where expression against a map of tags.

  Returns `true` if the tags match the expression, `false` otherwise.
  """
  @spec eval(term, map) :: boolean
  def eval({:tag, key}, tags) when is_atom(key) do
    case Map.fetch(tags, key) do
      {:ok, value} -> !!value
      :error -> false
    end
  end

  def eval({:tag, {key, value}}, tags) when is_atom(key) do
    case Map.fetch(tags, key) do
      {:ok, ^value} -> true
      {:ok, tag_value} -> to_string(tag_value) == to_string(value)
      :error -> false
    end
  end

  def eval({:not, expr}, tags) do
    not eval(expr, tags)
  end

  def eval({:and, left, right}, tags) do
    eval(left, tags) and eval(right, tags)
  end

  def eval({:or, left, right}, tags) do
    eval(left, tags) or eval(right, tags)
  end

  # Tokenizer

  defp tokenize(string) do
    string
    |> String.trim()
    |> do_tokenize([])
    |> case do
      {:ok, tokens} -> {:ok, Enum.reverse(tokens)}
      error -> error
    end
  end

  defp do_tokenize("", acc), do: {:ok, acc}

  defp do_tokenize(<<char, rest::binary>>, acc) when char in ~c[ \t\n\r] do
    do_tokenize(rest, acc)
  end

  defp do_tokenize("(" <> rest, acc), do: do_tokenize(rest, [:lparen | acc])
  defp do_tokenize(")" <> rest, acc), do: do_tokenize(rest, [:rparen | acc])

  defp do_tokenize("and" <> rest, acc) when rest == "" or binary_part(rest, 0, 1) in [" ", "\t", "\n", "\r", "(", ")"] do
    do_tokenize(rest, [:and | acc])
  end

  defp do_tokenize("or" <> rest, acc) when rest == "" or binary_part(rest, 0, 1) in [" ", "\t", "\n", "\r", "(", ")"] do
    do_tokenize(rest, [:or | acc])
  end

  defp do_tokenize("not" <> rest, acc) when rest == "" or binary_part(rest, 0, 1) in [" ", "\t", "\n", "\r", "(", ")"] do
    do_tokenize(rest, [:not | acc])
  end

  defp do_tokenize(string, acc) do
    case read_identifier(string, "") do
      {:ok, identifier, rest} -> do_tokenize(rest, [{:identifier, identifier} | acc])
      {:error, reason} -> {:error, reason}
    end
  end

  defp read_identifier("", acc) when acc != "" do
    {:ok, parse_identifier(acc), ""}
  end

  defp read_identifier("", "") do
    {:error, "unexpected end of input"}
  end

  defp read_identifier(<<char, rest::binary>>, acc) when char in ~c[ \t\n\r()] do
    if acc == "" do
      {:error, "unexpected character: #{<<char>>}"}
    else
      {:ok, parse_identifier(acc), <<char, rest::binary>>}
    end
  end

  defp read_identifier(<<char, rest::binary>>, acc) do
    read_identifier(rest, acc <> <<char>>)
  end

  defp parse_identifier(string) do
    case String.split(string, ":", parts: 2) do
      [key] -> String.to_atom(key)
      [key, value] -> {String.to_atom(key), value}
    end
  end

  # Parser - recursive descent

  defp parse_expr(tokens), do: parse_or(tokens)

  defp parse_or(tokens) do
    {left, rest} = parse_and(tokens)
    parse_or_rest(left, rest)
  end

  defp parse_or_rest(left, [:or | rest]) do
    {right, rest} = parse_and(rest)
    parse_or_rest({:or, left, right}, rest)
  end

  defp parse_or_rest(expr, tokens), do: {expr, tokens}

  defp parse_and(tokens) do
    {left, rest} = parse_not(tokens)
    parse_and_rest(left, rest)
  end

  defp parse_and_rest(left, [:and | rest]) do
    {right, rest} = parse_not(rest)
    parse_and_rest({:and, left, right}, rest)
  end

  defp parse_and_rest(expr, tokens), do: {expr, tokens}

  defp parse_not([:not | rest]) do
    {expr, rest} = parse_not(rest)
    {{:not, expr}, rest}
  end

  defp parse_not(tokens), do: parse_primary(tokens)

  defp parse_primary([:lparen | rest]) do
    {expr, rest} = parse_expr(rest)

    case rest do
      [:rparen | rest] -> {expr, rest}
      _ -> raise "expected closing parenthesis"
    end
  end

  defp parse_primary([{:identifier, value} | rest]) do
    {{:tag, value}, rest}
  end

  defp parse_primary([token | _rest]) do
    raise "unexpected token: #{inspect(token)}"
  end

  defp parse_primary([]) do
    raise "unexpected end of expression"
  end
end
