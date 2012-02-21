defmodule EEx::Engine do
  def handle_text(buffer, text) do
    quote do
      unquote(buffer) <> unquote(text)
    end
  end

  def handle_expr(buffer, '=', expr) do
    quote do
      unquote(buffer) <> to_binary(unquote(expr))
    end
  end
end
