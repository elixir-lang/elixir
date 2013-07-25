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
        defcallback parse(uri_info :: URI.Info.t) :: URI.Info.t

        @doc "Defines a default port"
        defcallback default_port() :: integer
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

  The callbacks and their documentation can be retrieved
  via the `__behaviour__` callback function.
  """

  @doc """
  Defines a callback according to the given type specification.
  """
  defmacro defcallback({ :::, _, [fun, return] }) do
    do_defcallback(fun, return, __CALLER__)
  end

  defmacro defcallback(fun) do
    do_defcallback(fun, quote(do: term), __CALLER__)
  end

  @doc """
  Defines a macro callback according to the given type specification.
  """
  defmacro defmacrocallback({ :::, _, [fun, return] }) do
    do_defmacrocallback(fun, return, __CALLER__)
  end

  defmacro defmacrocallback(fun) do
    do_defmacrocallback(fun, quote(do: Macro.t), __CALLER__)
  end

  defp do_defcallback(fun, spec, caller) do
    case Macro.extract_args(fun) do
      { name, args } ->
        do_callback(:def, name, args, name, length(args), spec, caller)
      :error ->
        raise ArgumentError, message: "invalid syntax in defcallback #{Macro.to_string(fun)}"
    end
  end

  defp do_defmacrocallback(fun, spec, caller) do
    case Macro.extract_args(fun) do
      { name, args } ->
        do_callback(:defmacro, :"MACRO-#{name}", [quote(do: env :: Macro.Env.t)|args], name, length(args), spec, caller)
      :error ->
        raise ArgumentError, message: "invalid syntax in defmacrocallback #{Macro.to_string(fun)}"
    end
  end

  defp do_callback(kind, name, args, docs_name, docs_arity, return, caller) do
    Enum.each args, fn
      { :::, _, [left, right] } ->
        ensure_not_default(left)
        ensure_not_default(right)
        left
      other ->
        ensure_not_default(other)
        other
    end

    quote do
      @callback unquote(name)(unquote_splicing(args)) :: unquote(return)
      Behaviour.store_docs __MODULE__, unquote(caller.line), unquote(kind), unquote(docs_name), unquote(docs_arity)
    end
  end

  defp ensure_not_default({ ://, _, [_, _] }) do
    raise ArgumentError, message: "default arguments // not supported in defcallback"
  end

  defp ensure_not_default(_), do: :ok

  @doc false
  def store_docs(module, line, kind, name, arity) do
    doc = Module.get_attribute module, :doc
    Module.delete_attribute module, :doc
    Module.put_attribute module, :behaviour_docs, { { name, arity }, line, kind, doc }
  end

  @doc false
  defmacro __using__(_) do
    quote do
      Module.register_attribute(__MODULE__, :behaviour_docs, accumulate: true)
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    docs = Enum.reverse Module.get_attribute(env.module, :behaviour_docs)

    quote do
      @doc false
      def __behaviour__(:callbacks) do
        __MODULE__.behaviour_info(:callbacks)
      end

      def __behaviour__(:docs) do
        unquote(Macro.escape(docs))
      end
    end
  end
end
