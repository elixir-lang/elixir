defmodule EEx.Engine do
  @moduledoc ~S"""
  Basic EEx engine that ships with Elixir.

  An engine needs to implement all callbacks below.

  An engine may also `use EEx.Engine` to get the default behaviour
  but this is not advised. In such cases, if any of the callbacks
  are overridden, they must call `super()` to delegate to the
  underlying `EEx.Engine`.
  """

  @type state :: term

  @doc """
  Called at the beginning of every template.

  It must return the initial state.
  """
  @callback init(opts :: keyword) :: state

  @doc """
  Called at the end of every template.

  It must return Elixir's quoted expressions for the template.
  """
  @callback handle_body(state) :: Macro.t()

  @doc """
  Called for the text/static parts of a template.

  It must return the updated state.
  """
  @callback handle_text(state, text :: String.t()) :: state

  @doc """
  Called for the dynamic/code parts of a template.

  The marker is what follows exactly after `<%`. For example,
  `<% foo %>` has an empty marker, but `<%= foo %>` has `"="`
  as marker. The allowed markers so far are:

    * `""`
    * `"="`
    * `"/"`
    * `"|"`

  Markers `"/"` and `"|"` are only for use in custom EEx engines
  and are not implemented by default. Using them without an
  appropriate implementation raises `EEx.SyntaxError`.

  It must return the updated state.
  """
  @callback handle_expr(state, marker :: String.t(), expr :: Macro.t()) :: state

  @doc """
  Invoked at the beginning of every nesting.

  It must return a new state that is used only inside the nesting.
  Once the nesting terminates, the current `state` is resumed.
  """
  @callback handle_begin(state) :: state

  @doc """
  Invokes at the end of a nesting.

  It must return Elixir's quoted expressions for the nesting.
  """
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

      def handle_expr(state, token, expr) do
        expr = Macro.prewalk(expr, &EEx.Engine.handle_assign/1)
        super(state, token, expr)
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
  # TODO: Raise on v2.0
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

  @doc false
  def init(_opts) do
    %{
      binary: [],
      dynamic: [],
      vars_count: 0
    }
  end

  @doc false
  def handle_begin(state) do
    %{state | binary: [], dynamic: []}
  end

  @doc false
  def handle_end(quoted) do
    handle_body(quoted)
  end

  @doc false
  def handle_body(state) do
    %{binary: binary, dynamic: dynamic} = state
    binary = {:<<>>, [], Enum.reverse(binary)}
    dynamic = [binary | dynamic]
    {:__block__, [], Enum.reverse(dynamic)}
  end

  @doc false
  def handle_text(state, text) do
    %{binary: binary} = state
    %{state | binary: [text | binary]}
  end

  @doc false
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
