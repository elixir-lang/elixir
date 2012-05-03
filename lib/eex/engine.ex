defmodule EEx.Engine do
  @moduledoc """
  This is the basic EEx engine that ships with Elixir.
  An engine needs to implement two functions:

  * `handle_text(buffer, text)` - it receives the buffer,
    the text and must return a new quoted expression;

  * `handle_expr(buffer, marker, expr)` - it receives the buffer,
    the marker, the expr and must return a new quoted expression;

    The marker is what follows exactly after `<%`. For example,
    `<% foo %>` has an empty marker, but `<%= foo %>` has `'='`
    as marker. The allowed markers so far are:

    * `''`
    * `'='`

    The semantic of the markers are dictated by the engine. This
    engine implements both markers above.
  """

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

  def behaviour_info(:callbacks) do
    [handle_text: 2, handle_expr: 3]
  end
end