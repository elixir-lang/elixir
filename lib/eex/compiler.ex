defrecord EEx::State, engine: nil, dict: [], filename: nil, line: 0

defmodule EEx::Compiler do
  @moduledoc false

  @doc """
  This is the compilation entry point. It glues the tokenizer
  and the engine together by handling the tokens and invoking
  the engine every time a full expression or text is received.
  """
  # TODO: receive filename and line as arguments
  def compile(source, engine) do
    tokens = EEx::Tokenizer.tokenize(source, 1)
    state = EEx::State.new(engine: engine)
    generate_buffer(tokens, "", [], state)
  end

  # Generates the buffers by handling each expression from the tokenizer

  defp generate_buffer([{ :text, _line, chars }|t], buffer, scope, state) do
    buffer = state.engine.handle_text(buffer, chars)
    generate_buffer(t, buffer, scope, state)
  end

  defp generate_buffer([{ :expr, line, mark, chars }|t], buffer, scope, state) do
    expr = { :__block__, 0, Erlang.elixir_translator.forms(chars, line, 'nofile') }
    buffer = state.engine.handle_expr(buffer, mark, expr)
    generate_buffer(t, buffer, scope, state)
  end

  defp generate_buffer([{ :start_expr, line, _, chars }|t], buffer, scope, state) do
    { contents, t } = generate_buffer(t, "", [chars|scope], state.dict([]).line(line))
    buffer = state.engine.handle_expr(buffer, '=', contents)
    generate_buffer(t, buffer, scope, state.dict([]))
  end

  defp generate_buffer([{ :middle_expr, line, _, chars }|t], buffer, [current|scope], state) do
    { wrapped, state } = wrap_expr(current, line, buffer, chars, state)
    generate_buffer(t, "", [wrapped|scope], state)
  end

  defp generate_buffer([{ :end_expr, line, _, chars }|t], buffer, [current|_], state) do
    { wrapped, state } = wrap_expr(current, line, buffer, chars, state)
    tuples = { :__block__, 0, Erlang.elixir_translator.forms(wrapped, state.line, 'nofile') }
    buffer = insert_quotes(tuples, state.dict)
    { buffer, t }
  end

  defp generate_buffer([{ :end_expr, line, _, chars }|_], _buffer, [], _state) do
    raise EEx::SyntaxError, message: "unexpected token: #{inspect chars} at line #{inspect line}"
  end

  defp generate_buffer([], buffer, [], _state) do
    buffer
  end

  defp generate_buffer([], _buffer, _scope, _state) do
    raise EEx::SyntaxError, message: "unexpected end of string. expecting a closing <% end %>."
  end

  # Creates a placeholder and wrap it inside the expression block

  defp wrap_expr(current, line, buffer, chars, state) do
    key = length(state.dict)
    new_lines = List.duplicate(?\n, line - state.line)
    placeholder = '__EEX__(' ++ integer_to_list(key) ++ ');'

    { current ++ new_lines ++ placeholder ++ chars, state.merge_dict([{key, buffer}]) }
  end

  # Changes placeholder to real expression

  defp insert_quotes({ :__EEX__, _, [key] }, dict) do
    Orddict.get(dict, key)
  end

  defp insert_quotes({ left, line, right }, dict) do
    { insert_quotes(left, dict), line, insert_quotes(right, dict) }
  end

  defp insert_quotes({ left, right }, dict) do
    { insert_quotes(left, dict), insert_quotes(right, dict) }
  end

  defp insert_quotes(list, dict) when is_list(list) do
    Enum.map list, insert_quotes(&1, dict)
  end

  defp insert_quotes(other, _dict) do
    other
  end
end
