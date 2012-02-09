defmodule Protocol do
  # We need to use Erlang.lists because Enum is not available yet
  require Erlang.lists, as: L

  # Handle `defprotocol`. It will define a function for each
  # protocol plus two extra functions:
  #
  # * `__protocol__/0` - returns a key-value pair with the protocol functions
  #
  # * `__protocol_for__/1` - receives one argument and returns the protocol
  #                          module that the function should be dispatched to
  #                          according to the only/except rules
  #
  def defprotocol(name, args, opts) do
    as = Orddict.get(opts, :as, true)
    kv = to_kv(args)

    quote do
      defmodule unquote(name) do
        def __protocol__(:name),      do: unquote(name)
        def __protocol__(:functions), do: unquote(kv)
        Protocol.functions(__MODULE__, unquote(kv))
        Protocol.protocol_for(__MODULE__, unquote(opts))
      end

      require unquote(name), as: unquote(as)
    end
  end

  # Implement the given protocol for the given module.
  # It also defines a `__impl__` function which
  # returns the protocol being implemented.
  def defimpl(protocol, [for: for], [do: block]) do
    quote do
      protocol = unquote(protocol)
      for      = unquote(for)
      name     = protocol::for

      Protocol.assert_protocol(protocol)

      defmodule name do
        def __impl__, do: unquote(protocol)
        unquote(block)
      end

      Protocol.assert_impl(name, protocol)
    end
  end

  # Check if the given module is a protocol. Raises an error
  # if not loaded or not a protocol.
  # :api: private
  def assert_protocol(module) do
    try do
      module.__info__(:data)
    rescue: UndefinedFunctionError
      raise ArgumentError, message: "#{module} is not loaded"
    end

    try do
      module.__protocol__(:name)
    rescue: UndefinedFunctionError
      raise ArgumentError, message: "#{module} is not a protocol"
    end
  end

  # Check if the given `impl` is a valid impl for `protocol`.
  # Raises an error if not.
  # :api: private
  def assert_impl(impl, protocol) do
    remaining = protocol.__protocol__(:functions) -- impl.__info__(:exports)

    if remaining != [], do:
      raise ArgumentError, message: "#{impl} did not implement #{protocol}, missing: #{remaining}"
  end

  # Callback entrypoint that defines the protocol functions.
  # It simply detects the protocol using __protocol_for__ and
  # then dispatches to it.
  # :api: private
  def functions(module, funs) do
    lc fun in L.reverse(funs), do: each_function(module, fun)
  end

  # Implements the method that detects the protocol and returns
  # the module to dispatch to. Returns module::Record for records
  # which should be properly handled by the dispatching function.
  # :api: private
  def protocol_for(module, opts) do
    lc kind in conversions_for(opts), do: each_protocol_for(module, kind)
  end

  ## Helpers

  # Specially handle tuples as they can also be record.
  # If this is the case, module::Record will be returned.
  defp each_protocol_for(module, { Tuple, :is_tuple }) do
    contents = quote do
      def __protocol_for__({}) do
        unquote(module)::Tuple
      end

      def __protocol_for__(arg) when is_tuple(arg) do
        case is_atom(element(1, arg)) do
        match: true
          unquote(module)::Record
        else:
          unquote(module)::Tuple
        end
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
  end

  # Special case any as we don't need to generate a guard.
  defp each_protocol_for(module, { _, :is_any }) do
    contents = quote do
      def __protocol_for__(_) do
        unquote(module)::Any
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
  end

  # Generate all others protocols.
  defp each_protocol_for(module, { kind, fun }) do
    contents = quote do
      def __protocol_for__(arg) when unquote(fun).(arg) do
        unquote(module)::unquote(kind)
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
  end

  # Implement the protocol invocation callbacks for each function.
  defp each_function(module, { name, arity }) do
    # Generate arguments according the arity. The arguments
    # are named xa, xb and so forth. We cannot use string
    # interpolation to generate the arguments because of compile
    # dependencies, so we use the <<>> instead.
    args = lc i in :lists.seq(1, arity) do
      { binary_to_atom(<<?x, i + 64>>, :utf8), 0, :quoted }
    end

    contents = quote do
      def unquote(name).(unquote_splicing(args)) do
        args = [unquote_splicing(args)]
        case __protocol_for__(xA) do
        match: unquote(module)::Record
          result =
            try do
              { apply(unquote(module)::element(1, xA), unquote(name), args), true }
            rescue: UndefinedFunctionError
              :error
            end

          case result do
          match: :error
            apply unquote(module)::Tuple, unquote(name), args
          match: { value, true }
            value
          end
        match: other
          apply other, unquote(name), args
        end
      end
    end

    Module.eval_quoted module, contents, [], __FILE__, __LINE__
  end

  # Converts the protocol expressions as [each(collection), length(collection)]
  # to an ordered dictionary [each: 1, length: 1] also checking for invalid args
  defp to_kv(args) do
    :orddict.from_list lc(x in args) ->
      case x do
      match: { _, _, args } when args == [] or args == false
        raise ArgumentError, message: "protocol functions expect at least one argument"
      match: { name, _, args } when is_atom(name) and is_list(args)
        { name, length(args) }
      else:
        raise ArgumentError, message: "invalid args for defprotocol"
      end
    end
  end

  # Returns the default conversions according to the given only/except options.
  defp conversions_for(opts) do
    kinds = [
      { Tuple,     :is_tuple },
      { Atom,      :is_atom },
      { List,      :is_list },
      { BitString, :is_bitstring },
      { Number,    :is_number },
      { Function,  :is_function },
      { PID,       :is_pid },
      { Port,      :is_port },
      { Reference, :is_reference }
    ]

    if only = Orddict.get(opts, :only, false) do
      selected = L.map fn(i, do: L.keyfind(i, 1, kinds)), only
      selected ++ [{ Any, :is_any }]
    elsif: except = Orddict.get(opts, :except, false)
      selected = L.foldl fn(i, list, do: L.keydelete(i, 1, list)), kinds, except
      selected ++ [{ Any, :is_any }]
    else:
      kinds
    end
  end
end