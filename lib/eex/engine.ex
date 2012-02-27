defmodule EEx::Engine do
  def handle_text(buffer, text) do
    quote do: unquote(buffer) <> unquote(text)
  end

  def handle_expr(buffer, '=', expr) do
    quote do
      tmp_1 = unquote(buffer)
      tmp_2 = to_binary(unquote(expr))
      tmp_1 <> tmp_2
    end
  end

  def handle_expr(buffer, '', expr) do
    quote do
      tmp = unquote(buffer)
      unquote(expr)
      tmp
    end
  end
end
