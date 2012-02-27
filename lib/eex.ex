defmodule EEx do
  def compile(source, engine // EEx::Engine) do
    EEx::Compiler.compile(source, engine)
  end
end

defmodule EEx::Compiler do
  def compile(source, engine) do
    tokens = EEx::Tokenizer.tokenize(source)
    generate_buffer(tokens, engine, "", [], [])
  end

  defp generate_buffer([{ :text, chars }|t], engine, buffer, scope, dict) do
    buffer = engine.handle_text(buffer, chars)
    generate_buffer(t, engine, buffer, scope, dict)
  end

  # TODO: use line and filename
  defp generate_buffer([{ :expr, mark, chars }|t], engine, buffer, scope, dict) do
    expr = { :__BLOCK__, 0, Erlang.elixir_translator.forms(chars, 1, 'nofile') }
    buffer = engine.handle_expr(buffer, mark, expr)
    generate_buffer(t, engine, buffer, scope, dict)
  end

  defp generate_buffer([{ :start_expr, _, chars }|t], engine, buffer, scope, _dict) do
    { contents, t } = generate_buffer(t, engine, "", [chars|scope], [])
    buffer = engine.handle_expr(buffer, '=', contents)
    generate_buffer(t, engine, buffer, scope, [])
  end

  defp generate_buffer([{ :middle_expr, _, chars }|t], engine, buffer, [current|scope], dict) do
    { wrapped, dict } = wrap_expr(current, buffer, chars, dict)
    generate_buffer(t, engine, "", [wrapped|scope], dict)
  end

  defp generate_buffer([{ :end_expr, _, chars }|t], _engine, buffer, [current|_], dict) do
    { wrapped, dict } = wrap_expr(current, buffer, chars, dict)
    tuples = { :__BLOCK__, 0, Erlang.elixir_translator.forms(wrapped, 1, 'nofile') }
    buffer = insert_quotes(tuples, dict)
    { buffer, t }
  end

  defp generate_buffer([], _engine, buffer, _scope, _dict) do
    buffer
  end

  ####

  def wrap_expr(current, buffer, chars, dict) do
    key = length(dict)
    placeholder = '__EEX__(' ++ integer_to_list(key) ++ ');'
    dict = Orddict.put(dict, key, buffer)

    { current ++ placeholder ++ chars, dict }
  end

  ###

  def insert_quotes( { :__EEX__, _, [key] }, dict) do
    Orddict.get(dict, key)
  end

  def insert_quotes({ left, line, right }, dict) do
    { insert_quotes(left, dict), line, insert_quotes(right, dict) }
  end

  def insert_quotes({ left, right }, dict) do
    { insert_quotes(left, dict), insert_quotes(right, dict) }
  end

  def insert_quotes(list, dict) when is_list(list) do
    Enum.map list, insert_quotes(&1, dict)
  end

  def insert_quotes(other, _dict) do
    other
  end
end
