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

        # Invoke the user given block
        unquote(block)

        # Finalize expansion
        unquote(after_defprotocol)
      end
    end
  end

  defp after_defprotocol do
    quote unquote: false do
      # == Deprecated records handling ==
      { arg, impl } = Protocol.rec_impl_for(__MODULE__)
      Kernel.def impl_for(unquote(arg)) when Kernel.is_record(unquote(arg)), do: unquote(impl)
      # == Deprecated records handling ==

      @spec impl_for(term) :: module | nil
      Kernel.def impl_for(data)

      # Define the implementation for structs.
      #
      # It simply delegates to struct_impl_for which is then
      # optimized during protocol consolidation.
      Kernel.def impl_for(%{ __struct__: struct }) when :erlang.is_atom(struct) do
        struct_impl_for(struct)
      end

      # Define the implementation for builtins.
      for { guard, mod } <- Protocol.builtin do
        target = Module.concat(__MODULE__, mod)

        Kernel.def impl_for(data) when :erlang.unquote(guard)(data) do
          case impl_for?(unquote(target)) do
            true  -> unquote(target).__impl__(:name)
            false -> any_impl_for
          end
        end
      end

      @spec impl_for!(term) :: module | no_return
      Kernel.def impl_for!(data) do
        impl_for(data) || raise(Protocol.UndefinedError, protocol: __MODULE__, value: data)
      end

      # Internal handler for Any
      if @fallback_to_any do
        Kernel.defp any_impl_for do
          case impl_for?(__MODULE__.Any) do
            true  -> __MODULE__.Any.__impl__(:name)
            false -> nil
          end
        end
      else
        Kernel.defp any_impl_for, do: nil
      end

      # Internal handler for Structs
      Kernel.defp struct_impl_for(struct) do
        target = Module.concat(__MODULE__, struct)
        case impl_for?(target) do
          true  -> target.__impl__(:name)
          false -> any_impl_for
        end
      end

      # Check if compilation is available internally
      Kernel.defp impl_for?(target) do
        Code.ensure_compiled?(target) and
          function_exported?(target, :__impl__, 1)
      end

      # Inline any and struct implementations
      @compile { :inline, any_impl_for: 0, struct_impl_for: 1, impl_for?: 1 }

      if :code.ensure_loaded(Kernel.Typespec) == { :module, Kernel.Typespec } and
         not Kernel.Typespec.defines_type?(__MODULE__, :t, 0) do
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
    for f <- for, do: do_defimpl(protocol, [do: block, for: f])
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
    [is_tuple: Tuple,
     is_atom: Atom,
     is_list: List,
     is_map: Map,
     is_bitstring: BitString,
     is_integer: Integer,
     is_float: Float,
     is_function: Function,
     is_pid: PID,
     is_port: Port,
     is_reference: Reference]
  end

  # Implements the function that detects the protocol and
  # returns the module to dispatch to.
  @doc false
  def rec_impl_for(current) do
    all = [Any] ++ for { _guard, mod } <- builtin, do: mod
    arg = quote do: arg
    target = Module.concat(current, Tuple)

    fallback = quote do
      case impl_for?(unquote(target)) do
        true  -> unquote(target).__impl__(:name)
        false -> any_impl_for
      end
    end

    impl_for = quote do
      atom = :erlang.element(1, unquote(arg))

      case not(atom in unquote(all)) and match?('Elixir.' ++ _, atom_to_list(atom)) do
        true ->
          target = Module.concat(unquote(current), atom)
          case impl_for?(target) do
            true  -> target.__impl__(:name)
            false -> unquote(fallback)
          end
        false ->
          unquote(fallback)
      end
    end

    { arg, impl_for }
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

    type_args = for _ <- :lists.seq(2, arity), do: quote(do: term)
    type_args = [quote(do: t) | type_args]

    call_args = for i <- :lists.seq(2, arity),
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
      Protocol.DSL.__spec__?(__MODULE__, name, arity) ||
        @callback unquote(name)(unquote_splicing(type_args)) :: term
    end
  end

  defmacro def(_) do
    raise ArgumentError, message: "invalid args for def inside defprotocol"
  end

  @doc false
  def __spec__?(module, name, arity) do
    case :code.ensure_loaded(Kernel.Typespec) do
      { :module, Kernel.Typespec } ->
        tuple = { name, arity }
        specs = Module.get_attribute(module, :spec)

        found = for { k, v } <- specs, k == tuple do
          Kernel.Typespec.define_callback(module, tuple, v)
          true
        end

        found != []
      { :error, _ } ->
        true
    end
  end
end
