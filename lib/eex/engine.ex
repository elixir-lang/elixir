defmodule EEx::Engine do
  def handle_text(buffer, text) do
    quote do: unquote(buffer) <> unquote(text)
  end

  def handle_expr(buffer, '=', expr) do
    quote do: unquote(buffer) <> to_binary(unquote(expr))
  end

  def handle_expr(buffer, '', expr) do
    quote do
      unquote(expr)
      unquote(buffer)
    end
  end

  def wrap_expr(current, buffer, chars, dict) do
    key = length(dict)
    placeholder = '__EEX__(' ++ integer_to_list(key) ++ ');'
    dict = Orddict.put(dict, key, buffer)

    { current ++ placeholder ++ chars, dict }
  end
end
