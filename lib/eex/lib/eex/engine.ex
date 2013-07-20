defmodule EEx.Engine do
  @moduledoc %B"""
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

    Read `handle_expr/3` below for more information about the markers
    implemented by default by this engine.
  """

  use Behaviour

  defcallback handle_text(Macro.t, binary) :: Macro.t
  defcallback handle_expr(Macro.t, binary, Macro.t) :: Macro.t

  @doc """
  The default implementation simply concatenates text to the buffer.
  """
  def handle_text(buffer, text) do
    quote do: unquote(buffer) <> unquote(text)
  end

  @doc """
  Implements expressions according to the markers.

      <% Elixir expression - inline with output %>
      <%= Elixir expression - replace with result %>

  All other markers are not implemented by this engine.
  """
  def handle_expr(buffer, "=", expr) do
    quote do
      tmp = unquote(buffer)
      tmp <> to_binary(unquote(expr))
    end
  end

  def handle_expr(buffer, "", expr) do
    quote do
      tmp = unquote(buffer)
      unquote(expr)
      tmp
    end
  end
end