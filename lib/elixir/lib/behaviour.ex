defmodule Behaviour do
  @moduledoc """
  Utilities for defining behaviour intefaces.

  Behaviours can be referenced by other modules
  to ensure they implement required callbacks.

  For example, you can specify the `URI.Parser`
  behaviour as follows:

      defmodule URI.Parser do
        use Behaviour

        @doc "Parses the given URL"
        defcallback parse(uri_info :: URI.Info.t) :: URI.Info.t

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
  Define a function callback according to the given type specification.
  """
  defmacro defcallback({ :::, _, [fun, return_and_guard] }) do
    { return, guard } = split_return_and_guard(return_and_guard)
    do_defcallback(fun, return, guard, __CALLER__)
  end

  defmacro defcallback(fun) do
    do_defcallback(fun, quote(do: term), [], __CALLER__)
  end

  @doc """
  Define a macro callback according to the given type specification.
  """
  defmacro defmacrocallback({ :::, _, [fun, return_and_guard] }) do
    { return, guard } = split_return_and_guard(return_and_guard)
    do_defmacrocallback(fun, return, guard, __CALLER__)
  end

  defmacro defmacrocallback(fun) do
    do_defmacrocallback(fun, quote(do: Macro.t), [], __CALLER__)
  end

  defp split_return_and_guard({ :when, _, [return, guard] }) do
    { return, guard }
  end

  defp split_return_and_guard({ :|, meta, [left, right] }) do
    { return, guard } = split_return_and_guard(right)
    { { :|, meta, [left, return] }, guard }
  end

  defp split_return_and_guard(other) do
    { other, [] }
  end

  defp do_defcallback(fun, return, guards, caller) do
    case Macro.decompose_call(fun) do
      { name, args } ->
        do_callback(:def, name, args, name, length(args), args, return, guards, caller)
      _ ->
        raise ArgumentError, message: "invalid syntax in defcallback #{Macro.to_string(fun)}"
    end
  end

  defp do_defmacrocallback(fun, return, guards, caller) do
    case Macro.decompose_call(fun) do
      { name, args } ->
        do_callback(:defmacro, :"MACRO-#{name}", [quote(do: env :: Macro.Env.t)|args],
                    name, length(args), args, return, guards, caller)
      _ ->
        raise ArgumentError, message: "invalid syntax in defmacrocallback #{Macro.to_string(fun)}"
    end
  end

  defp do_callback(kind, name, args, docs_name, docs_arity, docs_args, return, guards, caller) do
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
      docs_args = unquote(Macro.escape(docs_args))
      @callback unquote(name)(unquote_splicing(args)) :: unquote(return) when unquote(guards)
      Behaviour.store_docs(__MODULE__, unquote(caller.line), unquote(kind),
                           unquote(docs_name), docs_args, unquote(docs_arity))
    end
  end

  defp ensure_not_default({ ://, _, [_, _] }) do
    raise ArgumentError, message: "default arguments // not supported in defcallback/defmacrocallback"
  end

  defp ensure_not_default(_), do: :ok

  @doc false
  def store_docs(module, line, kind, name, args, arity) do
    doc = Module.get_attribute module, :doc
    Module.delete_attribute module, :doc

    signature = Enum.map(Stream.with_index(args), fn { arg, ix } ->
      { simplify_signature(arg, ix + 1), [line: line], nil }
    end)

    Module.put_attribute module, :behaviour_docs, { { name, arity }, line, kind, signature, doc }
  end

  defp simplify_signature({ :::, _, [var, _] }, i) do
    simplify_signature(var, i)
  end

  defp simplify_signature({ :|, _, [first, _] }, i) do
    simplify_signature(first, i)
  end

  defp simplify_signature({ :., _, [_, name] }, _i) do
    name
  end

  defp simplify_signature({ :->, _, list }, i) when is_list(list) do
    :"fun#{i}"
  end

  defp simplify_signature({ :.., _, list }, i) when is_list(list) do
    :"range#{i}"
  end

  defp simplify_signature({ :<<>>, _, list }, i) when is_list(list) do
    :"bitstring#{i}"
  end

  defp simplify_signature({ :{}, _, list }, i) when is_list(list) do
    :"tuple#{i}"
  end

  defp simplify_signature({ name, _, args }, _i)
      when is_atom(name) and (is_atom(args) or is_list(args)) do
    name
  end

  defp simplify_signature({ ast, _, list }, i) when is_list(list) do
    simplify_signature(ast, i)
  end

  defp simplify_signature(other, i) when is_integer(other), do: :"int#{i}"
  defp simplify_signature(other, i) when is_boolean(other), do: :"bool#{i}"
  defp simplify_signature(other, i) when is_atom(other),    do: :"atom#{i}"
  defp simplify_signature(other, i) when is_list(other),    do: :"list#{i}"
  defp simplify_signature(other, i) when is_tuple(other),   do: :"tuple#{i}"
  defp simplify_signature(_, i), do: :"arg#{i}"

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
    docs = if Code.compiler_options[:docs] do
      Enum.reverse Module.get_attribute(env.module, :behaviour_docs)
    end

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
