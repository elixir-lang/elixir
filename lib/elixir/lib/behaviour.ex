defmodule Behaviour do
  @moduledoc """
  Utilities for defining behaviour interfaces.

  Behaviours can be referenced by other modules
  to ensure they implement required callbacks.

  For example, you can specify the `URI.Parser`
  behaviour as follows:

      defmodule URI.Parser do
        use Behaviour

        @doc "Parses the given URL"
        defcallback parse(uri_info :: URI.t) :: URI.t

        @doc "Defines a default port"
        defcallback default_port() :: integer
      end

  And then a module may use it as:

      defmodule URI.HTTP do
        @behaviour URI.Parser
        def default_port(), do: 80
        def parse(info), do: info
      end

  If the behaviour changes or `URI.HTTP` does
  not implement one of the callbacks, a warning
  will be raised.

  ## Implementation

  Since Erlang R15, behaviours must be defined via
  `@callback` attributes. `defcallback` is a simple
  mechanism that defines the `@callback` attribute
  according to the given type specification. `defcallback` allows
  documentation to be created for the callback and defines
  a custom function signature.

  The callbacks and their documentation can be retrieved
  via the `__behaviour__` callback function.
  """

  @doc """
  Defines a function callback according to the given type specification.
  """
  defmacro defcallback(spec) do
    do_defcallback(split_spec(spec, quote(do: term)), __CALLER__)
  end

  @doc """
  Defines a macro callback according to the given type specification.
  """
  defmacro defmacrocallback(spec) do
    do_defmacrocallback(split_spec(spec, quote(do: Macro.t)), __CALLER__)
  end

  defp split_spec({:when, _, [{:::, _, [spec, return]}, guard]}, _default) do
    {spec, return, guard}
  end

  defp split_spec({:when, _, [spec, guard]}, default) do
    {spec, default, guard}
  end

  defp split_spec({:::, _, [spec, return]}, _default) do
    {spec, return, []}
  end

  defp split_spec(spec, default) do
    {spec, default, []}
  end

  defp do_defcallback({spec, return, guards}, caller) do
    case Macro.decompose_call(spec) do
      {name, args} ->
        do_callback(:def, name, args, name, length(args), args, return, guards, caller)
      _ ->
        raise ArgumentError, "invalid syntax in defcallback #{Macro.to_string(spec)}"
    end
  end

  defp do_defmacrocallback({spec, return, guards}, caller) do
    case Macro.decompose_call(spec) do
      {name, args} ->
        do_callback(:defmacro, :"MACRO-#{name}", [quote(do: env :: Macro.Env.t)|args],
                    name, length(args), args, return, guards, caller)
      _ ->
        raise ArgumentError, "invalid syntax in defmacrocallback #{Macro.to_string(spec)}"
    end
  end

  defp do_callback(kind, name, args, docs_name, docs_arity, _docs_args, return, guards, caller) do
    :lists.foreach fn
      {:::, _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left
      other ->
        ensure_not_default(other)
        other
    end, args

    quote do
      @callback unquote(name)(unquote_splicing(args)) :: unquote(return) when unquote(guards)
      Behaviour.store_docs(__MODULE__, unquote(caller.line), unquote(kind),
                           unquote(docs_name), unquote(docs_arity))
    end
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in defcallback/defmacrocallback"
  end

  defp ensure_not_default(_), do: :ok

  @doc false
  def store_docs(module, line, kind, name, arity) do
    doc = Module.get_attribute module, :doc
    Module.delete_attribute module, :doc
    Module.put_attribute module, :behaviour_docs, {{name, arity}, line, kind, doc}
  end

  @doc false
  defmacro __using__(_) do
    quote do
      Module.register_attribute(__MODULE__, :behaviour_docs, accumulate: true)

      # TODO: Deprecate by 1.2
      # TODO: Remove by 2.0
      @doc false
      def __behaviour__(:callbacks) do
        __MODULE__.behaviour_info(:callbacks)
      end

      def __behaviour__(:docs) do
        Code.get_docs(__MODULE__, :behaviour_docs)
      end

      import unquote(__MODULE__)
    end
  end
end
