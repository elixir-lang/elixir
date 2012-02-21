defmodule EEx::Engine do
  def handle_text(buffer, text) do
    quote do
      unquote(buffer) <> unquote(text)
    end
  end
end
