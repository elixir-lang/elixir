defmodule EEx.Engine do
  @moduledoc ~S"""
  Basic EEx engine that ships with Elixir.

  An engine needs to implement four functions:

    * `init(opts)` - returns the initial buffer

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

  @callback init(opts :: Keyword.t) :: Macro.t
  @callback handle_body(quoted :: Macro.t) :: Macro.t
  @callback handle_text(buffer :: Macro.t, text :: String.t) :: Macro.t
  @callback handle_expr(buffer :: Macro.t, marker :: String.t, expr :: Macro.t) :: Macro.t

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour EEx.Engine

      def init(opts) do
        EEx.Engine.init(opts)
      end

      def handle_body(quoted) do
        EEx.Engine.handle_body(quoted)
      end

      def handle_text(buffer, text) do
        EEx.Engine.handle_text(buffer, text)
      end

      def handle_expr(buffer, marker, expr) do
        EEx.Engine.handle_expr(buffer, marker, expr)
      end

      defoverridable [handle_body: 1, handle_expr: 3, handle_text: 2, init: 1]
    end
  end

  @doc """
  Handles assigns in quoted expressions.

  A warning will be printed on missing assigns.
  Future versions will raise.

  This can be added to any custom engine by invoking
  `handle_assign/1` with `Macro.prewalk/2`:

      def handle_expr(buffer, token, expr) do
        expr = Macro.prewalk(expr, &EEx.Engine.handle_assign/1)
        EEx.Engine.handle_expr(buffer, token, expr)
      end

  """
  @spec handle_assign(Macro.t) :: Macro.t
  def handle_assign({:@, meta, [{name, _, atom}]}) when is_atom(name) and is_atom(atom) do
    line = meta[:line] || 0
    quote line: line, do: EEx.Engine.fetch_assign!(var!(assigns), unquote(name))
  end
  def handle_assign(arg) do
    arg
  end

  @doc false
  # TODO: Raise on 2.0
  @spec fetch_assign!(Access.t, Access.key) :: term | nil
  def fetch_assign!(assigns, key) do
    case Access.fetch(assigns, key) do
      {:ok, val} ->
        val
      :error ->
        keys = Enum.map(assigns, &elem(&1, 0))
        IO.warn "assign @#{key} not available in EEx template. " <>
                "Please ensure all assigns are given as options. " <>
                "Available assigns: #{inspect keys}"
        nil
    end
  end

  @doc """
  Returns an empty string as initial buffer.
  """
  def init(_opts) do
    ""
  end

  @doc """
  The default implementation simply returns the given expression.
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
      tmp1 = unquote(buffer)
      tmp1 <> String.Chars.to_string(unquote(expr))
    end
  end

  def handle_expr(buffer, "", expr) do
    quote do
      tmp2 = unquote(buffer)
      unquote(expr)
      tmp2
    end
  end
end
