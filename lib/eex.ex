defmodule EEx do
  def compile(source, engine // EEx::Engine) do
    EEx::Compiler.compile(source, engine)
  end
end

defmodule EEx::Compiler do
  def compile(source, engine) do
    tokens = EEx::Tokenizer.tokenize(source)
    generate_buffer(tokens, engine, "")
  end

  defp generate_buffer([{ :text, chars }|t], engine, buffer) do
    buffer = engine.handle_text(buffer, chars)
    generate_buffer(t, engine, buffer)
  end

  defp generate_buffer([], _engine, buffer) do
    buffer
  end
end
