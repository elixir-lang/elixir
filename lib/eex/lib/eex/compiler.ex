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
    column = 1
    indentation = opts[:indentation] || 0
    trim = opts[:trim] || false
    tokenizer_options = %{trim: trim, indentation: indentation}

    case EEx.Tokenizer.tokenize(source, line, column, tokenizer_options) do
      {:ok, tokens} ->
        state = %{
          engine: opts[:engine] || @default_engine,
          file: file,
          line: line,
          quoted: [],
          start_line: nil,
          start_column: nil,
          parser_options: Code.get_compiler_option(:parser_options)
        }

        init = state.engine.init(opts)
        generate_buffer(tokens, init, [], state)

      {:error, line, column, message} ->
        raise EEx.SyntaxError, file: file, line: line, column: column, message: message
    end
  end

  # Generates the buffers by handling each expression from the tokenizer.
  # It returns Macro.t/0 or it raises.

  defp generate_buffer([{:text, line, column, chars} | rest], buffer, scope, state) do
    buffer =
      if function_exported?(state.engine, :handle_text, 3) do
        meta = [line: line, column: column]
        state.engine.handle_text(buffer, meta, IO.chardata_to_string(chars))
      else
        # TODO: Remove this branch on Elixir v2.0
        state.engine.handle_text(buffer, IO.chardata_to_string(chars))
      end

    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer([{:expr, line, column, mark, chars} | rest], buffer, scope, state) do
    options = [file: state.file, line: line, column: column(column, mark)] ++ state.parser_options
    expr = Code.string_to_quoted!(chars, options)
    buffer = state.engine.handle_expr(buffer, IO.chardata_to_string(mark), expr)
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer(
         [{:start_expr, start_line, start_column, mark, chars} | rest],
         buffer,
         scope,
         state
       ) do
    if mark != '=' do
      message =
        "the contents of this expression won't be output unless the EEx block starts with \"<%=\""

      :elixir_errors.erl_warn(start_line, state.file, message)
    end

    {contents, line, rest} = look_ahead_middle(rest, start_line, chars)

    {contents, rest} =
      generate_buffer(
        rest,
        state.engine.handle_begin(buffer),
        [contents | scope],
        %{
          state
          | quoted: [],
            line: line,
            start_line: start_line,
            start_column: column(start_column, mark)
        }
      )

    buffer = state.engine.handle_expr(buffer, IO.chardata_to_string(mark), contents)
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer(
         [{:middle_expr, line, _column, '', chars} | rest],
         buffer,
         [current | scope],
         state
       ) do
    {wrapped, state} = wrap_expr(current, line, buffer, chars, state)
    state = %{state | line: line}
    generate_buffer(rest, state.engine.handle_begin(buffer), [wrapped | scope], state)
  end

  defp generate_buffer(
         [{:middle_expr, line, column, modifier, chars} | t],
         buffer,
         [_ | _] = scope,
         state
       ) do
    message =
      "unexpected beginning of EEx tag \"<%#{modifier}\" on \"<%#{modifier}#{chars}%>\", " <>
        "please remove \"#{modifier}\" accordingly"

    :elixir_errors.erl_warn(line, state.file, message)
    generate_buffer([{:middle_expr, line, column, '', chars} | t], buffer, scope, state)
    # TODO: Make this an error on Elixir v2.0 since it accidentally worked previously.
    # raise EEx.SyntaxError, message: message, file: state.file, line: line
  end

  defp generate_buffer([{:middle_expr, line, column, _, chars} | _], _buffer, [], state) do
    raise EEx.SyntaxError,
      message: "unexpected middle of expression <%#{chars}%>",
      file: state.file,
      line: line,
      column: column
  end

  defp generate_buffer(
         [{:end_expr, line, _column, '', chars} | rest],
         buffer,
         [current | _],
         state
       ) do
    {wrapped, state} = wrap_expr(current, line, buffer, chars, state)
    column = state.start_column
    options = [file: state.file, line: state.start_line, column: column] ++ state.parser_options
    tuples = Code.string_to_quoted!(wrapped, options)
    buffer = insert_quoted(tuples, state.quoted)
    {buffer, rest}
  end

  defp generate_buffer(
         [{:end_expr, line, column, modifier, chars} | t],
         buffer,
         [_ | _] = scope,
         state
       ) do
    message =
      "unexpected beginning of EEx tag \"<%#{modifier}\" on end of " <>
        "expression \"<%#{modifier}#{chars}%>\", please remove \"#{modifier}\" accordingly"

    :elixir_errors.erl_warn(line, state.file, message)
    generate_buffer([{:end_expr, line, column, '', chars} | t], buffer, scope, state)
    # TODO: Make this an error on Elixir v2.0 since it accidentally worked previously.
    # raise EEx.SyntaxError, message: message, file: state.file, line: line, column: column
  end

  defp generate_buffer([{:end_expr, line, column, _, chars} | _], _buffer, [], state) do
    raise EEx.SyntaxError,
      message: "unexpected end of expression <%#{chars}%>",
      file: state.file,
      line: line,
      column: column
  end

  defp generate_buffer([{:eof, _, _}], buffer, [], state) do
    state.engine.handle_body(buffer)
  end

  defp generate_buffer([{:eof, line, column}], _buffer, _scope, state) do
    raise EEx.SyntaxError,
      message: "unexpected end of string, expected a closing '<% end %>'",
      file: state.file,
      line: line,
      column: column
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

  # Look middle expressions that immediately follow a start_expr

  defp look_ahead_middle(
         [{:text, _, _, text}, {:middle_expr, line, _, _, chars} | rest] = tokens,
         start,
         contents
       ) do
    if only_spaces?(text) do
      {contents ++ text ++ chars, line, rest}
    else
      {contents, start, tokens}
    end
  end

  defp look_ahead_middle([{:middle_expr, line, _column, _, chars} | rest], _start, contents) do
    {contents ++ chars, line, rest}
  end

  defp look_ahead_middle(tokens, start, contents) do
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

  defp column(column, mark) do
    # length('<%') == 2
    column + 2 + length(mark)
  end
end
