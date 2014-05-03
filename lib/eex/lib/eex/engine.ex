defmodule EEx.Engine do
  @moduledoc ~S"""
  This is the basic EEx engine that ships with Elixir.
  An engine needs to implement three functions:

  * `handle_body(quoted)` - receives the final built quoted
    expression, should do final post-processing and return a
    quoted expression;

  * `handle_text(buffer, text)` - it receives the buffer,
    the text and must return a new quoted expression;

  * `handle_expr(buffer, marker, expr)` - it receives the buffer,
    the marker, the expr and must return a new quoted expression;

    The marker is what follows exactly after `<%`. For example,
    `<% foo %>` has an empty marker, but `<%= foo %>` has `"="`
    as marker. The allowed markers so far are:

    * `""`
    * `"="`

    Read `handle_expr/3` below for more information about the markers
    implemented by default by this engine.
  """

  use Behaviour

  defcallback handle_body(Macro.t) :: Macro.t
  defcallback handle_text(Macro.t, binary) :: Macro.t
  defcallback handle_expr(Macro.t, binary, Macro.t) :: Macro.t

  @doc """
  The default implementation implementation simply returns the
  given expression.
  """
  def handle_body(quoted) do
    quoted
  end

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
      tmp <> to_string(unquote(expr))
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
