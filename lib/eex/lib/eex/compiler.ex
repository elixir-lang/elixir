defrecord EEx.State, engine: EEx.SmartEngine, dict: [], file: "nofile", line: 1, start_line: 1

defmodule EEx.Compiler do
  @moduledoc false

  @doc """
  This is the compilation entry point. It glues the tokenizer
  and the engine together by handling the tokens and invoking
  the engine every time a full expression or text is received.
  """
  def compile(source, options) do
    line   = Keyword.get(options, :line, 1)
    tokens = EEx.Tokenizer.tokenize(source, line)
    state  = EEx.State.new(options)
    generate_buffer(tokens, "", [], state)
  end

  # Generates the buffers by handling each expression from the tokenizer

  defp generate_buffer([{ :text, chars }|t], buffer, scope, state) do
    buffer = state.engine.handle_text(buffer, String.from_char_list!(chars))
    generate_buffer(t, buffer, scope, state)
  end

  defp generate_buffer([{ :expr, line, mark, chars }|t], buffer, scope, state) do
    expr = Code.string_to_quoted!(chars, [line: line, file: state.file])
    buffer = state.engine.handle_expr(buffer, mark, expr)
    generate_buffer(t, buffer, scope, state)
  end

  defp generate_buffer([{ :start_expr, start_line, mark, chars }|t], buffer, scope, state) do
    { contents, line, t } = look_ahead_text(t, start_line, chars)
    { contents, t } = generate_buffer(t, "", [contents|scope], state.dict([]).line(line).start_line(start_line))
    buffer = state.engine.handle_expr(buffer, mark, contents)
    generate_buffer(t, buffer, scope, state)
  end

  defp generate_buffer([{ :middle_expr, line, _, chars }|t], buffer, [current|scope], state) do
    { wrapped, state } = wrap_expr(current, line, buffer, chars, state)
    generate_buffer(t, "", [wrapped|scope], state.line(line))
  end

  defp generate_buffer([{ :end_expr, line, _, chars }|t], buffer, [current|_], state) do
    { wrapped, state } = wrap_expr(current, line, buffer, chars, state)
    tuples = Code.string_to_quoted!(wrapped, [line: state.start_line, file: state.file])
    buffer = insert_quotes(tuples, state.dict)
    { buffer, t }
  end

  defp generate_buffer([{ :end_expr, line, _, chars }|_], _buffer, [], _state) do
    raise EEx.SyntaxError, message: "unexpected token: #{inspect chars} at line #{inspect line}"
  end

  defp generate_buffer([], buffer, [], state) do
    state.engine.handle_body(buffer)
  end

  defp generate_buffer([], _buffer, _scope, _state) do
    raise EEx.SyntaxError, message: "unexpected end of string. expecting a closing <% end %>."
  end

  # Creates a placeholder and wrap it inside the expression block

  defp wrap_expr(current, line, buffer, chars, state) do
    new_lines = List.duplicate(?\n, line - state.line)
    key = length(state.dict)
    placeholder = '__EEX__(' ++ integer_to_list(key) ++ ');'
    { current ++ placeholder ++ new_lines ++ chars, state.update_dict(&[{key, buffer}|&1]) }
  end

  # Look text ahead on expressions

  defp look_ahead_text([{ :text, text }, { :middle_expr, line, _, chars }|t]=list, start, contents) do
    if only_spaces?(text) do
      { contents ++ text ++ chars, line, t }
    else
      { contents, start, list }
    end
  end

  defp look_ahead_text(t, start, contents) do
    { contents, start, t }
  end

  defp only_spaces?(chars) do
    Enum.all?(chars, &(&1 in [?\s, ?\t, ?\r, ?\n]))
  end

  # Changes placeholder to real expression

  defp insert_quotes({ :__EEX__, _, [key] }, dict) do
    { ^key, value } = List.keyfind dict, key, 0
    value
  end

  defp insert_quotes({ left, line, right }, dict) do
    { insert_quotes(left, dict), line, insert_quotes(right, dict) }
  end

  defp insert_quotes({ left, right }, dict) do
    { insert_quotes(left, dict), insert_quotes(right, dict) }
  end

  defp insert_quotes(list, dict) when is_list(list) do
    Enum.map list, &insert_quotes(&1, dict)
  end

  defp insert_quotes(other, _dict) do
    other
  end
end
