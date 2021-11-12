defmodule EEx.Tokenizer do
  @moduledoc false

  @type content :: IO.chardata()
  @type line :: non_neg_integer
  @type column :: non_neg_integer
  @type marker :: '=' | '/' | '|' | ''
  @type token ::
          {:text, line, column, content}
          | {:expr | :start_expr | :middle_expr | :end_expr, line, column, marker, content}
          | {:eof, line, column}

  @default_delimiter ?%
  @spaces [?\s, ?\t]

  @doc """
  Tokenizes the given charlist or binary.

  It returns {:ok, list} with the following tokens:

    * `{:text, line, column, content}`
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

    {list, line, column} =
      (opts.trim && trim_init(list, line, column, opts)) || {list, line, column}

    tokenize(list, line, column, opts, [{line, column}], [])
  end

  defp tokenize(list, line, column, opts, buffer, acc) do
    case list do
      [?<, @default_delimiter, @default_delimiter] ++ t ->
        tokenize(t, line, column + 3, opts, [@default_delimiter, ?< | buffer], acc)

      [?<, @default_delimiter, ?\#] ++ t ->
        case expr(t, line, column + 3, opts, []) do
          {:error, _, _, _} = error ->
            error

          {:ok, _, new_line, new_column, rest} ->
            {rest, new_line, new_column, buffer} =
              trim_if_needed(rest, new_line, new_column, opts, buffer)

            acc = tokenize_text(buffer, acc)
            tokenize(rest, new_line, new_column, opts, [{new_line, new_column}], acc)
        end

      [?<, @default_delimiter] ++ t ->
        {marker, t} = retrieve_marker(t)

        case expr(t, line, column + 2 + length(marker), opts, []) do
          {:error, _, _, _} = error ->
            error

          {:ok, expr, new_line, new_column, rest} ->
            {key, expr} =
              case :elixir_tokenizer.tokenize(expr, 1, file: "eex", check_terminators: false) do
                {:ok, _line, _column, warnings, tokens} ->
                  Enum.each(Enum.reverse(warnings), fn {location, file, msg} ->
                    :elixir_errors.erl_warn(location, file, msg)
                  end)

                  token_key(tokens, expr)

                {:error, _, _, _, _} ->
                  {:expr, expr}
              end

            {rest, new_line, new_column, buffer} =
              trim_if_needed(rest, new_line, new_column, opts, buffer)

            acc = tokenize_text(buffer, acc)
            final = {key, line, column, marker, expr}
            tokenize(rest, new_line, new_column, opts, [{new_line, new_column}], [final | acc])
        end

      '\n' ++ t ->
        tokenize(t, line + 1, opts.indentation + 1, opts, [?\n | buffer], acc)

      [h | t] ->
        tokenize(t, line, column + 1, opts, [h | buffer], acc)

      [] ->
        eof = {:eof, line, column}
        {:ok, Enum.reverse([eof | tokenize_text(buffer, acc)])}
    end
  end

  # Retrieve marker for opening delimiter

  defp retrieve_marker([marker | t]) when marker in [?=, ?/, ?|] do
    {[marker], t}
  end

  defp retrieve_marker(t) do
    {'', t}
  end

  # Tokenize an expression until we find the closing delimiter
  defp expr(list, line, column, opts, buffer) do
    case list do
      [@default_delimiter, ?> | t] -> {:ok, Enum.reverse(buffer), line, column + 2, t}
      '\n' ++ t -> expr(t, line + 1, opts.indentation + 1, opts, [?\n | buffer])
      [h | t] -> expr(t, line, column + 1, opts, [h | buffer])
      [] -> {:error, line, column, "missing token '#{[@default_delimiter]}>'"}
    end
  end

  # Receives tokens and check if it is a start, middle or an end token.
  defp token_key(tokens, expr) do
    case {tokens, tokens |> Enum.reverse() |> drop_eol()} do
      {[{:end, _} | _], [{:do, _} | _]} ->
        {:middle_expr, expr}

      {_, [{:do, _} | _]} ->
        {:start_expr, maybe_append_space(expr)}

      {_, [{:block_identifier, _, _} | _]} ->
        {:middle_expr, maybe_append_space(expr)}

      {[{:end, _} | _], [{:stab_op, _, _} | _]} ->
        {:middle_expr, expr}

      {_, [{:stab_op, _, _} | reverse_tokens]} ->
        fn_index = Enum.find_index(reverse_tokens, &match?({:fn, _}, &1)) || :infinity
        end_index = Enum.find_index(reverse_tokens, &match?({:end, _}, &1)) || :infinity

        if end_index > fn_index do
          {:start_expr, expr}
        else
          {:middle_expr, expr}
        end

      {tokens, _} ->
        case Enum.drop_while(tokens, &closing_bracket?/1) do
          [{:end, _} | _] -> {:end_expr, expr}
          _ -> {:expr, expr}
        end
    end
  end

  defp drop_eol([{:eol, _} | rest]), do: drop_eol(rest)
  defp drop_eol(rest), do: rest

  defp maybe_append_space([?\s]), do: [?\s]
  defp maybe_append_space([h]), do: [h, ?\s]
  defp maybe_append_space([h | t]), do: [h | maybe_append_space(t)]

  defp closing_bracket?({closing, _}) when closing in ~w"( [ {"a, do: true
  defp closing_bracket?(_), do: false

  # Tokenize the buffered text by appending
  # it to the given accumulator.

  defp tokenize_text([{_line, _column}], acc) do
    acc
  end

  defp tokenize_text(buffer, acc) do
    [{line, column} | buffer] = Enum.reverse(buffer)
    [{:text, line, column, buffer} | acc]
  end

  defp trim_if_needed(rest, line, column, opts, buffer) do
    if opts.trim do
      buffer = trim_left(buffer, 0)
      {rest, line, column} = trim_right(rest, line, column, 0, opts)
      {rest, line, column, buffer}
    else
      {rest, line, column, buffer}
    end
  end

  defp trim_init(list, line, column, opts) do
    case list do
      [h | t] when h in @spaces -> trim_init(t, line, column + 1, opts)
      [?\r, ?\n | t] -> trim_init(t, line + 1, opts.indentation + 1, opts)
      [?\n | t] -> trim_init(t, line + 1, opts.indentation + 1, opts)
      [?<, @default_delimiter | _] = rest -> {rest, line, column}
      _ -> false
    end
  end

  defp trim_left(buffer, count) do
    case trim_whitespace(buffer, 0) do
      {[?\n, ?\r | rest], _} -> trim_left(rest, count + 1)
      {[?\n | rest], _} -> trim_left(rest, count + 1)
      _ when count > 0 -> [?\n | buffer]
      _ -> buffer
    end
  end

  defp trim_right(rest, line, column, last_column, opts) do
    case trim_whitespace(rest, column) do
      {[?\r, ?\n | rest], column} ->
        trim_right(rest, line + 1, opts.indentation + 1, column + 1, opts)

      {[?\n | rest], column} ->
        trim_right(rest, line + 1, opts.indentation + 1, column, opts)

      {[], column} ->
        {[], line, column}

      _ when last_column > 0 ->
        {[?\n | rest], line - 1, last_column}

      _ ->
        {rest, line, column}
    end
  end

  defp trim_whitespace([h | t], column) when h in @spaces, do: trim_whitespace(t, column + 1)
  defp trim_whitespace(list, column), do: {list, column}
end
