defmodule EEx do
  def compile(source, engine // EEx::Engine) do
    EEx::Compiler.compile(source, engine)
  end
end

defmodule EEx::Compiler do
  def compile(source, engine) do
    tokens = EEx::Tokenizer.tokenize(source)
    generate_buffer(tokens, engine, "", [])
  end

  defp generate_buffer([{ :text, chars }|t], engine, buffer, scope) do
    buffer = engine.handle_text(buffer, chars)
    generate_buffer(t, engine, buffer, scope)
  end

  # TODO: use line and filename
  defp generate_buffer([{ :expr, mark, chars }|t], engine, buffer, scope) do
    expr = { :__BLOCK__, 0, Erlang.elixir_translator.forms(chars, 1, 'nofile') }
    buffer = engine.handle_expr(buffer, mark, expr)
    generate_buffer(t, engine, buffer, scope)
  end

  defp generate_buffer([{ :start_expr, mark, chars }|t], engine, buffer, scope) do
    { contents, t } = generate_buffer(t, engine, "", [chars|scope])
    buffer = engine.handle_expr(buffer, mark, contents)
    generate_buffer(t, engine, buffer, scope)
  end

  defp generate_buffer([{ :end_expr, _mark, chars }|t], _engine, buffer, [current|_]) do
    tuples = { :__BLOCK__, 0, Erlang.elixir_translator.forms(current ++ '__EEX__(1)' ++ chars, 1, 'nofile') }
    dict = Orddict.put([], 1, buffer)
    buffer = insert_quotes(tuples, dict)
    { buffer, t }
  end

  defp generate_buffer([], _engine, buffer, _scope) do
    buffer
  end

  ####

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
