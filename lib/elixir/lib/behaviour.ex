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
        defcallback parse(arg)

        @doc "Defines a default port"
        defcallback default_port()
      end

  And then a specific protocol may use it as:

      defmodule URI.HTTP do
        @behaviour URI.Parser
        def default_port(), do: 80
        def parse(info), do: info
      end

  In case the behaviour changes or URI.HTTP does
  not implement one of the callbacks, a warning
  will be raised.

  ## Implementation

  Internally, Erlang call `behaviour_info(:callbacks)`
  to obtain all functions that a behaviour should
  implemented. Therefore, all this module does is
  to define `behaviour_info(:callbacks)` with the
  `defcallback` definitions.
  """

  @doc """
  Annotates the given function is a callback. `defcallback` is
  slightly different than simple using `def` because, even if
  `defcallback` contains default values, a default function
  won't be generated, which would happen with `def`.
  """
  defmacro defcallback(fun) do
    { name, args } = Erlang.elixir_clauses.extract_args(fun)
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
      def behaviour_info(:callbacks) do
        @__behaviour_callbacks
      end
    end
  end
end