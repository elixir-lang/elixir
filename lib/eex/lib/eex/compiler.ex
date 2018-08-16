defmodule EEx.Compiler do
  @moduledoc false

  # When changing this setting, don't forget to update the docs for EEx
  @default_engine EEx.SmartEngine

  @doc """
  This is the compilation entry point. It glues the tokenizer
  and the engine together by handling the tokens and invoking
  the engine every time a full expression or text is received.
  """
  @spec compile(String.t(), keyword) :: Macro.t()
  def compile(source, opts) when is_binary(source) and is_list(opts) do
    file = opts[:file] || "nofile"
    line = opts[:line] || 1
    trim = opts[:trim] || false

    case EEx.Tokenizer.tokenize(source, line, trim: trim) do
      {:ok, tokens} ->
        state = %{
          engine: opts[:engine] || @default_engine,
          file: file,
          line: line,
          quoted: [],
          start_line: nil
        }

        init = state.engine.init(opts)
        generate_buffer(tokens, init, [], state)

      {:error, line, message} ->
        raise EEx.SyntaxError, line: line, file: file, message: message
    end
  end

  # Generates the buffers by handling each expression from the tokenizer.
  # It returns Macro.t/0 or it raises.

  defp generate_buffer([{:text, chars} | rest], buffer, scope, state) do
    buffer = state.engine.handle_text(buffer, IO.chardata_to_string(chars))
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer([{:expr, line, mark, chars} | rest], buffer, scope, state) do
    expr = Code.string_to_quoted!(chars, line: line, file: state.file)
    buffer = state.engine.handle_expr(buffer, IO.chardata_to_string(mark), expr)
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer([{:start_expr, start_line, mark, chars} | rest], buffer, scope, state) do
    {contents, line, rest} = look_ahead_text(rest, start_line, chars)

    {contents, rest} =
      generate_buffer(rest, state.engine.handle_begin(buffer), [contents | scope], %{
        state
        | quoted: [],
          line: line,
          start_line: start_line
      })

    buffer = state.engine.handle_expr(buffer, IO.chardata_to_string(mark), contents)
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer([{:middle_expr, line, '', chars} | rest], buffer, [current | scope], state) do
    {wrapped, state} = wrap_expr(current, line, buffer, chars, state)
    state = %{state | line: line}
    generate_buffer(rest, state.engine.handle_begin(buffer), [wrapped | scope], state)
  end

  defp generate_buffer(
         [{:middle_expr, line, modifier, chars} | t],
         buffer,
         [_ | _] = scope,
         state
       ) do
    message =
      "unexpected beginning of EEx tag \"<%#{modifier}\" on \"<%#{modifier}#{chars}%>\", " <>
        "please remove \"#{modifier}\" accordingly"

    :elixir_errors.warn(line, state.file, message)
    generate_buffer([{:middle_expr, line, '', chars} | t], buffer, scope, state)
    # TODO: Make this an error on Elixir v2.0 since it accidentally worked previously.
    # raise EEx.SyntaxError, message: message, file: state.file, line: line
  end

  defp generate_buffer([{:middle_expr, line, _, chars} | _], _buffer, [], state) do
    raise EEx.SyntaxError,
      message: "unexpected middle of expression <%#{chars}%>",
      file: state.file,
      line: line
  end

  defp generate_buffer([{:end_expr, line, '', chars} | rest], buffer, [current | _], state) do
    {wrapped, state} = wrap_expr(current, line, buffer, chars, state)
    tuples = Code.string_to_quoted!(wrapped, line: state.start_line, file: state.file)
    buffer = insert_quoted(tuples, state.quoted)
    {buffer, rest}
  end

  defp generate_buffer([{:end_expr, line, modifier, chars} | t], buffer, [_ | _] = scope, state) do
    message =
      "unexpected beginning of EEx tag \"<%#{modifier}\" on end of " <>
        "expression \"<%#{modifier}#{chars}%>\", please remove \"#{modifier}\" accordingly"

    :elixir_errors.warn(line, state.file, message)
    generate_buffer([{:end_expr, line, '', chars} | t], buffer, scope, state)
    # TODO: Make this an error on Elixir v2.0 since it accidentally worked previously.
    # raise EEx.SyntaxError, message: message, file: state.file, line: line
  end

  defp generate_buffer([{:end_expr, line, _, chars} | _], _buffer, [], state) do
    raise EEx.SyntaxError,
      message: "unexpected end of expression <%#{chars}%>",
      file: state.file,
      line: line
  end

  defp generate_buffer([], buffer, [], state) do
    state.engine.handle_body(buffer)
  end

  defp generate_buffer([], _buffer, _scope, state) do
    raise EEx.SyntaxError,
      message: "unexpected end of string, expected a closing '<% end %>'",
      file: state.file,
      line: state.line
  end

  # Creates a placeholder and wrap it inside the expression block

  defp wrap_expr(current, line, buffer, chars, state) do
    new_lines = List.duplicate(?\n, line - state.line)
    key = length(state.quoted)
    placeholder = '__EEX__(' ++ Integer.to_charlist(key) ++ ');'
    count = current ++ placeholder ++ new_lines ++ chars
    new_state = %{state | quoted: [{key, state.engine.handle_end(buffer)} | state.quoted]}

    {count, new_state}
  end

  # Look text ahead on expressions

  defp look_ahead_text(
         [{:text, text}, {:middle_expr, line, _, chars} | rest] = tokens,
         start,
         contents
       ) do
    if only_spaces?(text) do
      {contents ++ text ++ chars, line, rest}
    else
      {contents, start, tokens}
    end
  end

  defp look_ahead_text([{:middle_expr, line, _, chars} | rest], _start, contents) do
    {contents ++ chars, line, rest}
  end

  defp look_ahead_text(tokens, start, contents) do
    {contents, start, tokens}
  end

  defp only_spaces?(chars) do
    Enum.all?(chars, &(&1 in [?\s, ?\t, ?\r, ?\n]))
  end

  # Changes placeholder to real expression

  defp insert_quoted({:__EEX__, _, [key]}, quoted) do
    {^key, value} = List.keyfind(quoted, key, 0)
    value
  end

  defp insert_quoted({left, line, right}, quoted) do
    {insert_quoted(left, quoted), line, insert_quoted(right, quoted)}
  end

  defp insert_quoted({left, right}, quoted) do
    {insert_quoted(left, quoted), insert_quoted(right, quoted)}
  end

  defp insert_quoted(list, quoted) when is_list(list) do
    Enum.map(list, &insert_quoted(&1, quoted))
  end

  defp insert_quoted(other, _quoted) do
    other
  end
end
