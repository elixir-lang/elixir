defmodule Behaviour do
  @moduledoc """
  A convenient module for defining behaviors.
  It provides a `defcallback` macro for defining
  the callbacks and automatically generates a
  `behaviour_info` function before compilation.

      defmodule URI.Parser do
        use Behaviour

        @doc "Parses the given URL"
        defcallback parse(arg)

        @doc "Defines a default port"
        defcallback default_port(arg)
      end

  """

  @doc """
  Annotates the given function is a callback. `defcallback` is
  slightly different than simple using `def` because, even if
  `defcallback` contains default values, a default function
  won't be generated, which would happen with `def`.
  """
  defmacro defcallback(fun) do
    { name, args } = Erlang.elixir_clauses.extract_args(fun)
    length         = length(args)
    defaults       = Enum.count args, match?({ ://, _, _ }, &1)

    attributes = Enum.map (length - defaults)..length, fn(i) ->
      quote do
        @__behaviour_callbacks { unquote(name), unquote(i) }
      end
    end

    quote do
      __block__ unquote(attributes)
      def unquote(fun), :skip_definition
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      Module.register_attribute(__MODULE__, :__behaviour_callbacks, accumulate: true)
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__), only: [defcallback: 1]
    end
  end

  @doc false
  defmacro before_compile(_) do
    quote do
      @doc false
      def behaviour_info(:callbacks) do
        @__behaviour_callbacks
      end
    end
  end
end