defmodule EEx.Engine do
  @moduledoc ~S"""
  Basic EEx engine that ships with Elixir.

  An engine needs to implement six functions:

    * `init(opts)` - called at the beginning of every text
      and it must return the initial state.

    * `handle_body(state)` - receives the state of the document
      and it must return a quoted expression.

    * `handle_text(state, text)` - it receives the state,
      the text and must return a new quoted expression.

    * `handle_expr(state, marker, expr)` - it receives the state,
      the marker, the expr and must return a new state.

    * `handle_begin(state)` - called every time a new state
      is needed with an empty buffer. Typically called for do/end
      blocks, case expressions, anonymous functions, etc.

    * `handle_end(state)` - opposite of `handle_begin(state)` and
      it must return quoted expression.

      The marker is what follows exactly after `<%`. For example,
      `<% foo %>` has an empty marker, but `<%= foo %>` has `"="`
      as marker. The allowed markers so far are: 

        * `""`
        * `"="`
        * `"/"`
        * `"|"`

      Markers `"/"` and `"|"` are only for use in custom EEx engines
      and are not implemented by default. Using them without the
      implementation raises `EEx.SyntaxError`.

      If your engine does not implement all markers, please ensure that
      `handle_expr/3` falls back to `EEx.Engine.handle_expr/3` 
      to raise the proper error message.

      Read `handle_expr/3` below for more information about the markers
      implemented by default by this engine.

  `EEx.Engine` can be used directly if one desires to use the
  default implementations for the functions above.
  """

  @type state :: term

  @callback init(opts :: keyword) :: state
  @callback handle_body(state) :: Macro.t()
  @callback handle_text(state, text :: String.t()) :: state
  @callback handle_expr(state, marker :: String.t(), expr :: Macro.t()) :: state
  @callback handle_begin(state) :: state
  @callback handle_end(state) :: Macro.t()

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour EEx.Engine

      def init(opts) do
        EEx.Engine.init(opts)
      end

      def handle_body(state) do
        EEx.Engine.handle_body(state)
      end

      def handle_begin(state) do
        EEx.Engine.handle_begin(state)
      end

      def handle_end(state) do
        EEx.Engine.handle_end(state)
      end

      def handle_text(state, text) do
        EEx.Engine.handle_text(state, text)
      end

      def handle_expr(state, marker, expr) do
        EEx.Engine.handle_expr(state, marker, expr)
      end

      defoverridable EEx.Engine
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
  @spec handle_assign(Macro.t()) :: Macro.t()
  def handle_assign({:@, meta, [{name, _, atom}]}) when is_atom(name) and is_atom(atom) do
    line = meta[:line] || 0
    quote(line: line, do: EEx.Engine.fetch_assign!(var!(assigns), unquote(name)))
  end

  def handle_assign(arg) do
    arg
  end

  @doc false
  # TODO: Raise on 2.0
  @spec fetch_assign!(Access.t(), Access.key()) :: term | nil
  def fetch_assign!(assigns, key) do
    case Access.fetch(assigns, key) do
      {:ok, val} ->
        val

      :error ->
        keys = Enum.map(assigns, &elem(&1, 0))

        IO.warn(
          "assign @#{key} not available in EEx template. " <>
            "Please ensure all assigns are given as options. " <>
            "Available assigns: #{inspect(keys)}"
        )

        nil
    end
  end

  @doc """
  Returns an empty string as initial buffer.
  """
  def init(_opts) do
    %{
      binary: [],
      dynamic: [],
      vars_count: 0
    }
  end

  @doc """
  Returns an empty string as the new buffer.
  """
  def handle_begin(state) do
    %{state | binary: [], dynamic: []}
  end

  @doc """
  End of the new buffer.
  """
  def handle_end(quoted) do
    handle_body(quoted)
  end

  @doc """
  The default implementation simply returns the given expression.
  """
  def handle_body(state) do
    %{binary: binary, dynamic: dynamic} = state
    binary = {:<<>>, [], Enum.reverse(binary)}
    dynamic = [binary | dynamic]
    {:__block__, [], Enum.reverse(dynamic)}
  end

  @doc """
  The default implementation simply concatenates text to the buffer.
  """
  def handle_text(state, text) do
    %{binary: binary} = state
    %{state | binary: [text | binary]}
  end

  @doc """
  Implements expressions according to the markers.

      <% Elixir expression - inline with output %>
      <%= Elixir expression - replace with result %>
      <%/ Elixir expression - raise EEx.SyntaxError, to be implemented by custom engines %>
      <%| Elixir expression - raise EEx.SyntaxError, to be implemented by custom engines %>

  All other markers are not implemented by this engine.
  """
  def handle_expr(state, "=", ast) do
    %{binary: binary, dynamic: dynamic, vars_count: vars_count} = state
    var = Macro.var(:"arg#{vars_count}", __MODULE__)

    ast =
      quote do
        unquote(var) = String.Chars.to_string(unquote(ast))
      end

    segment =
      quote do
        unquote(var) :: binary
      end

    %{state | dynamic: [ast | dynamic], binary: [segment | binary], vars_count: vars_count + 1}
  end

  def handle_expr(state, "", ast) do
    %{dynamic: dynamic} = state
    %{state | dynamic: [ast | dynamic]}
  end

  def handle_expr(_state, marker, _ast) when marker in ["/", "|"] do
    raise EEx.SyntaxError,
          "unsupported EEx syntax <%#{marker} %> (the syntax is valid but not supported by the current EEx engine)"
  end
end
