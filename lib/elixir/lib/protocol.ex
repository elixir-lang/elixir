defmodule Protocol do
  @moduledoc false

  # We need to use :lists because Enum is not available yet
  require :lists, as: L

  # Callback for defprotocol.
  @doc false
  def defprotocol(name, [do: block]) do
    quote do
      defmodule unquote(name) do
        # We don't allow function definition inside protocols
        import Kernel, except: [
          defmacrop: 1, defmacrop: 2, defmacro: 1, defmacro: 2,
          defp: 1, defp: 2, def: 1, def: 2
        ]

        # Import the new dsl that holds the new def
        import Protocol.DSL, only: :macros

        # Set up a clear slate to store defined functions
        @functions []
        @prioritize []

        # Deprecated
        @only nil
        @except nil

        # Invoke the user given block
        unquote(block)

        # Finalize expansion
        unquote(after_defprotocol)
      end
    end
  end

  defp after_defprotocol do
    quote unquote: false do
      { arg, bodies } = Protocol.impl_for(__MODULE__, @prioritize)

      @spec impl_for(term) :: module | nil
      Kernel.def impl_for(data)

      lc { guard, body } inlist bodies do
        Kernel.def impl_for(unquote(arg)) when unquote(guard), do: unquote(body)
      end

      @spec impl_for!(term) :: module | no_return
      Kernel.def impl_for!(data) do
        impl_for(data) || raise(Protocol.UndefinedError, protocol: __MODULE__, value: data)
      end

      unless Kernel.Typespec.defines_type?(__MODULE__, :t, 0) do
        @type t :: term
      end

      if @only do
        IO.write "warning: @only in protocols is deprecated, use @prioritize instead\n#{Exception.format_stacktrace}"
      end

      if @except do
        IO.write "warning: @except in protocols is deprecated, use @prioritize instead\n#{Exception.format_stacktrace}"
      end

      @doc false
      Kernel.def __protocol__(:name),       do: __MODULE__
      Kernel.def __protocol__(:functions),  do: unquote(:lists.sort(@functions))
      Kernel.def __protocol__(:prioritize), do: unquote(@prioritize)
    end
  end

  # Callback for defimpl.
  @doc false
  def defimpl(protocol, opts) do
    do_defimpl(protocol, :lists.keysort(1, opts))
  end

  defp do_defimpl(protocol, [do: block, for: for]) when is_list(for) do
    lc f inlist for, do: do_defimpl(protocol, [do: block, for: f])
  end

  defp do_defimpl(protocol, [do: block, for: for]) do
    quote do
      protocol = unquote(protocol)
      for      = unquote(for)
      name     = Module.concat(protocol, for)

      Protocol.assert_protocol(protocol)

      defmodule name do
        @behaviour unquote(protocol)
        @protocol  unquote(protocol)
        @for       unquote(for)

        unquote(block)

        def __impl__(:name),     do: __MODULE__
        def __impl__(:protocol), do: @protocol
        def __impl__(:for),      do: @for
      end
    end
  end

  # Check if the given module is a protocol. Raises an error
  # if not loaded or not a protocol.
  @doc false
  def assert_protocol(module) do
    case Code.ensure_compiled(module) do
      { :module, ^module } -> nil
      _ -> raise ArgumentError, message: "#{inspect module} is not loaded"
    end

    try do
      module.__protocol__(:name)
    rescue
      UndefinedFunctionError ->
        raise ArgumentError, message: "#{inspect module} is not a protocol"
    end
  end

  # Implements the function that detects the protocol and
  # returns the module to dispatch to.
  @doc false
  def impl_for(current, prioritize) do
    ordered = prioritize ++
              :lists.foldl(&:lists.delete/2, builtin, prioritize)

    arg = quote(do: arg)
    { arg, lc(mod inlist ordered, do: impl_for(current, mod, arg)) }
  end

  defp impl_for(current, Record, arg) do
    fallback = impl_for(current, Tuple, arg) |> elem(1)

    dispatch = quote do
      atom = :erlang.element(1, unquote(arg))

      case not(atom in unquote(builtin)) and match?('Elixir.' ++ _, atom_to_list(atom)) do
        true ->
          target = Module.concat(unquote(current), :erlang.element(1, unquote(arg)))
          try do
            target.__impl__(:name)
          catch
            :error, :undef, [[{ ^target, :__impl__, [:name], _ }|_]|_] ->
              unquote(fallback)
          end
        false ->
          unquote(fallback)
      end
    end

    quote do
      { is_record(unquote(arg)), unquote(dispatch) }
    end
  end

  defp impl_for(current, Tuple, arg),     do: impl_with_fallback(Tuple, :is_tuple, current, Any, arg)
  defp impl_for(current, Atom, arg),      do: impl_with_fallback(Atom, :is_atom, current, Any, arg)
  defp impl_for(current, List, arg),      do: impl_with_fallback(List, :is_list, current, Any, arg)
  defp impl_for(current, BitString, arg), do: impl_with_fallback(BitString, :is_bitstring, current, Any, arg)
  defp impl_for(current, Number, arg),    do: impl_with_fallback(Number, :is_number, current, Any, arg)
  defp impl_for(current, Function, arg),  do: impl_with_fallback(Function, :is_function, current, Any, arg)
  defp impl_for(current, PID, arg),       do: impl_with_fallback(PID, :is_pid, current, Any, arg)
  defp impl_for(current, Port, arg),      do: impl_with_fallback(Port, :is_port, current, Any, arg)
  defp impl_for(current, Reference, arg), do: impl_with_fallback(Reference, :is_reference, current, Any, arg)

  defp impl_for(current, Any, _arg) do
    target = Module.concat(current, Any)

    { true, quote do
      try do
        unquote(target).__impl__(:name)
      catch
        :error, :undef, [[{ unquote(target), :__impl__, [:name], _ }|_]|_] ->
          nil
      end
    end }
  end

  # Prioritized mdules are dispatched directly, falling back to tuples.
  defp impl_for(current, mod, arg) do
    quote do
      { is_record(unquote(arg), unquote(mod)),
        unquote(with_fallback(Module.concat(current, mod), current, Tuple, arg)) }
    end
  end

  # Defines an implementation with fallback to the given module.
  defp impl_with_fallback(mod, guard, current, fallback, arg) do
    quote do
      { unquote(guard)(unquote(arg)),
        unquote(with_fallback(Module.concat(current, mod), current, fallback, arg)) }
    end
  end

  # Tries to dispatch to a given target, fallbacks to the
  # given `fallback` implementation if the target does not exist.
  defp with_fallback(target, current, fallback, arg) when is_atom(target) do
    quote do
      try do
        unquote(target).__impl__(:name)
      catch
        :error, :undef, [[{ unquote(target), :__impl__, [:name], _ }|_]|_] ->
          unquote(impl_for(current, fallback, arg) |> elem(1))
      end
    end
  end

  defp builtin do
    [ Record, Tuple, Atom, List, BitString, Number,
      Function, PID, Port, Reference, Any ]
  end
end

defmodule Protocol.DSL do
  @moduledoc false

  @doc false
  defmacro def({ _, _, args }) when args == [] or is_atom(args) do
    raise ArgumentError, message: "protocol functions expect at least one argument"
  end

  defmacro def({ name, _, args }) when is_atom(name) and is_list(args) do
    arity = length(args)

    type_args = lc _ inlist :lists.seq(2, arity), do: quote(do: term)
    type_args = [quote(do: t) | type_args]

    call_args = lc i inlist :lists.seq(2, arity),
                  do: { binary_to_atom(<<?x, i + 64>>), [], __MODULE__ }
    call_args = [quote(do: t) | call_args]

    quote do
      name  = unquote(name)
      arity = unquote(arity)

      @functions [{name, arity}|@functions]

      # Generate a fake definition with the user
      # signature that will be used by docs
      Kernel.def unquote(name)(unquote_splicing(args))

      # Generate the actual implementation
      Kernel.def unquote(name)(unquote_splicing(call_args)) do
        impl_for!(t).unquote(name)(unquote_splicing(call_args))
      end

      # Convert the spec to callback if possible,
      # otherwise generate a dummy callback
      Protocol.DSL.callback_from_spec(__MODULE__, name, arity) ||
        @callback unquote(name)(unquote_splicing(type_args)) :: term
    end
  end

  defmacro def(_) do
    raise ArgumentError, message: "invalid args for def inside defprotocol"
  end

  @doc false
  def callback_from_spec(module, name, arity) do
    tuple = { name, arity }
    specs = Module.get_attribute(module, :spec)

    found = lc { k, v } inlist specs, k == tuple do
      Kernel.Typespec.define_callback(module, tuple, v)
      true
    end

    found != []
  end
end
