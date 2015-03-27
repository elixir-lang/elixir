defmodule EEx.Engine do
  @moduledoc ~S"""
  Basic EEx engine that ships with Elixir.

  An engine needs to implement three functions:

    * `handle_body(quoted)` - receives the final built quoted
      expression, should do final post-processing and return a
      quoted expression.

    * `handle_text(buffer, text)` - it receives the buffer,
      the text and must return a new quoted expression.

    * `handle_expr(buffer, marker, expr)` - it receives the buffer,
      the marker, the expr and must return a new quoted expression.

      The marker is what follows exactly after `<%`. For example,
      `<% foo %>` has an empty marker, but `<%= foo %>` has `"="`
      as marker. The allowed markers so far are: `""` and `"="`.

      Read `handle_expr/3` below for more information about the markers
      implemented by default by this engine.

  `EEx.Engine` can be used directly if one desires to use the
  default implementations for the functions above.
  """

  use Behaviour

  defcallback handle_body(Macro.t) :: Macro.t
  defcallback handle_text(Macro.t, String.t) :: Macro.t
  defcallback handle_expr(Macro.t, String.t, Macro.t) :: Macro.t

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour EEx.Engine

      def handle_body(body) do
        EEx.Engine.handle_body(body)
      end

      def handle_text(buffer, text) do
        EEx.Engine.handle_text(buffer, text)
      end

      def handle_expr(buffer, marker, expr) do
        EEx.Engine.handle_expr(buffer, marker, expr)
      end

      defoverridable [handle_body: 1, handle_expr: 3, handle_text: 2]
    end
  end

  @doc """
  Handles assigns in quoted expressions.

  This can be added to any custom engine by invoking
  `handle_assign/3` with `Macro.prewalk/1`:

      def handle_expr(buffer, token, expr) do
        expr = Macro.prewalk(expr, &EEx.Engine.handle_assign/1)
        EEx.Engine.handle_expr(buffer, token, expr)
      end

  """
  def handle_assign({:@, meta, [{name, _, atom}]}) when is_atom(name) and is_atom(atom) do
    line = meta[:line] || 0
    quote line: line, do: Dict.get(var!(assigns), unquote(name))
  end

  def handle_assign(arg) do
    arg
  end

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
  @spec handle_expr(Macro.t, String.t, Macro.t) :: Macro.t
  def handle_expr(buffer, "=", expr) do
    quote do
      tmp = unquote(buffer)
      tmp <> String.Chars.to_string(unquote(expr))
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
