defmodule Protocol do
  @moduledoc false

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

        # Compile with debug info for consolidation
        @compile :debug_info

        # Set up a clear slate to store defined functions
        @functions []
        @fallback_to_any false

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
      msg = "is deprecated and should be removed. " <>
            "Note if you want to fallback to Any, you have to set @fallback_to_any true"

      if @only do
        IO.puts "warning: @only in protocol #{inspect __MODULE__} " <> msg
        @fallback_to_any @fallback_to_any || Any in @only
      end

      if @except do
        IO.puts "warning: @except in protocol #{inspect __MODULE__} " <> msg
        @fallback_to_any @fallback_to_any || not(Any in @except)
      end

      { arg, bodies, rec } = Protocol.impl_for(__MODULE__)

      @spec impl_for(term) :: module | nil
      Kernel.def impl_for(data)

      lc { guard, body } inlist bodies do
        Kernel.def impl_for(unquote(arg)) when unquote(guard), do: unquote(body)
      end

      @spec impl_for!(term) :: module | no_return
      Kernel.def impl_for!(data) do
        impl_for(data) || raise(Protocol.UndefinedError, protocol: __MODULE__, value: data)
      end

      # Handle special Record type
      Kernel.defp rec_impl_for(unquote(arg)), do: unquote(rec)

      # Handle special Any type
      if @fallback_to_any do
        Kernel.defp any_impl_for do
          try do
            __MODULE__.Any.__impl__(:name)
          catch
            :error, :undef, [[{ __MODULE__.Any, :__impl__, [:name], _ }|_]|_] ->
              nil
          end
        end
      else
        Kernel.defp any_impl_for, do: nil
      end

      # Inline both helpers
      @compile { :inline, any_impl_for: 0, rec_impl_for: 1 }

      unless Kernel.Typespec.defines_type?(__MODULE__, :t, 0) do
        @type t :: term
      end

      # Store information as an attribute so it
      # can be read without loading the module.
      Module.register_attribute(__MODULE__, :protocol, persist: true)
      @protocol [fallback_to_any: !!@fallback_to_any, consolidated: false]

      @doc false
      Kernel.def __protocol__(:name),      do: __MODULE__
      Kernel.def __protocol__(:functions), do: unquote(:lists.sort(@functions))
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

        Module.register_attribute(__MODULE__, :impl, persist: true)
        @impl [protocol: @protocol, for: @for]

        @doc false
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

  # Builtin types.
  @doc false
  def builtin do
    [ Tuple, Atom, List, BitString, Integer, Float,
      Function, PID, Port, Reference, Any ]
  end

  # Implements the function that detects the protocol and
  # returns the module to dispatch to.
  @doc false
  def impl_for(current) do
    arg = quote(do: arg)
    all = [Record|builtin]

    { arg,
      lc(mod inlist all, do: impl_for(current, mod, arg)),
      rec_impl_for(current, arg) }
  end

  defp rec_impl_for(current, arg) do
    fallback = impl_for(current, Tuple, arg) |> elem(1)

    quote do
      target = Module.concat(unquote(current), unquote(arg))
      try do
        target.__impl__(:name)
      catch
        :error, :undef, [[{ ^target, :__impl__, [:name], _ }|_]|_] ->
          unquote(fallback)
      end
    end
  end

  defp impl_for(current, Record, arg) do
    fallback = impl_for(current, Tuple, arg) |> elem(1)

    dispatch = quote do
      atom = :erlang.element(1, unquote(arg))

      case not(atom in unquote(builtin)) and match?('Elixir.' ++ _, atom_to_list(atom)) do
        true  -> rec_impl_for(atom)
        false -> unquote(fallback)
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
  defp impl_for(current, Integer, arg),   do: impl_with_fallback(Integer, :is_integer, current, Any, arg)
  defp impl_for(current, Float, arg),     do: impl_with_fallback(Float, :is_float, current, Any, arg)
  defp impl_for(current, Function, arg),  do: impl_with_fallback(Function, :is_function, current, Any, arg)
  defp impl_for(current, PID, arg),       do: impl_with_fallback(PID, :is_pid, current, Any, arg)
  defp impl_for(current, Port, arg),      do: impl_with_fallback(Port, :is_port, current, Any, arg)
  defp impl_for(current, Reference, arg), do: impl_with_fallback(Reference, :is_reference, current, Any, arg)

  defp impl_for(_current, Any, _arg) do
    { true, quote(do: any_impl_for) }
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
