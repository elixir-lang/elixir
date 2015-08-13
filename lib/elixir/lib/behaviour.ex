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
    do_defcallback(:def, split_spec(spec, quote(do: term)))
  end

  @doc """
  Defines a macro callback according to the given type specification.
  """
  defmacro defmacrocallback(spec) do
    do_defcallback(:defmacro, split_spec(spec, quote(do: Macro.t)))
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

  defp do_defcallback(kind, {spec, return, guards}) do
    case Macro.decompose_call(spec) do
      {name, args} ->
        do_callback(kind, name, args, return, guards)
      _ ->
        raise ArgumentError, "invalid syntax in #{kind}callback #{Macro.to_string(spec)}"
    end
  end

  defp do_callback(kind, name, args, return, guards) do
    :lists.foreach fn
      {:::, _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left
      other ->
        ensure_not_default(other)
        other
    end, args

    spec =
      quote do
        unquote(name)(unquote_splicing(args)) :: unquote(return) when unquote(guards)
      end

    case kind do
      :def -> quote(do: @callback unquote(spec))
      :defmacro -> quote(do: @macrocallback unquote(spec))
    end
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in defcallback/defmacrocallback"
  end

  defp ensure_not_default(_), do: :ok

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
