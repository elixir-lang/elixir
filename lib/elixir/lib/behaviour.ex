defmodule Behaviour do
  @moduledoc """
  A convenience module for defining behaviours.
  Behaviours can be referenced by other modules
  in order to ensure they implement the proper
  callbacks.

  For example, you can specify the `URI.Parser`
  behaviour as follow:

      defmodule URI.Parser do
        use Behaviour

        @doc "Parses the given URL"
        defcallback parse(uri_info :: URI.Info.t), do: URI.Info.t

        @doc "Defines a default port"
        defcallback default_port(), do: integer
      end

  And then a specific module may use it as:

      defmodule URI.HTTP do
        @behaviour URI.Parser
        def default_port(), do: 80
        def parse(info), do: info
      end

  In case the behaviour changes or URI.HTTP does
  not implement one of the callbacks, a warning
  will be raised.

  ## Implementation

  Behaviours since Erlang R15 must be defined via
  `@callback` attributes. `defcallback` is a simple
  mechanism that defines the `@callback` attribute
  according to the type specification and also allows
  docs and defines a custom function signature.
  """

  @doc """
  Annotates the given function is a callback. `defcallback` is
  slightly different than simple using `def` because, even if
  `defcallback` contains default values, a default function
  won't be generated, which would happen with `def`.
  """
  defmacro defcallback(fun) do
    { name, args } = :elixir_clauses.extract_args(fun)
    length = length(args)

    { args, defaults } = Enum.map_reduce(args, 0, function do
      { ://, _, [expr, _] }, acc ->
        { expr, acc + 1 }
      expr, acc ->
        { expr, acc }
    end)

    attributes = Enum.map (length - defaults)..length, fn(i) ->
      quote do
        @__behaviour_callbacks { unquote(name), unquote(i) }
      end
    end

    quote do
      __block__ unquote(attributes)
      def unquote(name)(unquote_splicing(args))
    end
  end

  @doc """
  Defines a callback according to the given type specification.
  """
  defmacro defcallback(fun, do: return) do
    { name, args } = :elixir_clauses.extract_args(fun)

    docs = Enum.map args, (function do
      { :::, _, [left, right] } ->
        ensure_not_default(left)
        ensure_not_default(right)
        left
      other ->
        ensure_not_default(other)
        other
    end)

    quote do
      @callback unquote(name)(unquote_splicing(args)), do: unquote(return)
      def unquote(name)(unquote_splicing(docs))
    end
  end

  defp ensure_not_default({ ://, _, [_, _] }) do
    raise ArgumentError, message: "default arguments // not supported in defcallback"
  end

  defp ensure_not_default(_), do: :ok

  @doc false
  defmacro __using__(_) do
    quote do
      Module.register_attribute(__MODULE__, :__behaviour_callbacks, accumulate: true)
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(_) do
    quote do
      @doc false
      if @__behaviour_callbacks != [] do
        def behaviour_info(:callbacks) do
          @__behaviour_callbacks
        end
      end
    end
  end
end