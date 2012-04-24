defmodule Protocol do
  # We need to use Erlang.lists because Enum is not available yet
  require Erlang.lists, as: L

  # Handle `defprotocol`. It will define a function for each
  # protocol plus two extra functions:
  #
  # * `__protocol__/1` - returns the protocol name when :name is given,
  #                      and key-value pair with the protocol functions
  #                      when :functions is given;
  #
  # * `__protocol_for__/1` - receives one argument and returns the protocol
  #                          module that the function should be dispatched to
  #                          according to the only/except rules. If no protocol
  #                          matches, returns nil;
  #
  # * `__protocol_for__!/1` - same as above but raises an error if protocol is not found
  #
  def defprotocol(name, args, opts) do
    funs = to_funs(args)

    quote do
      defmodule unquote(name) do
        def __protocol__(:name),      do: unquote(name)
        def __protocol__(:functions), do: unquote(funs)
        { conversions, fallback } = Protocol.conversions_for(unquote(opts))
        Protocol.functions(__MODULE__, unquote(funs), fallback)
        Protocol.protocol_for(__MODULE__, conversions, fallback)
      end
    end
  end

  # Implement the given protocol for the given module.
  # It also defines a `__impl__` function which
  # returns the protocol being implemented.
  def defimpl(protocol, [for: for], [do: block]) do
    quote do
      protocol = unquote(protocol)
      for      = unquote(for)
      name     = Module.concat(protocol, for)

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
  @doc false
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
  @doc false
  def assert_impl(impl, protocol) do
    remaining = protocol.__protocol__(:functions) -- impl.__info__(:functions)

    if remaining != [], do:
      raise ArgumentError, message: "#{impl} did not implement #{protocol}, missing: #{remaining}"
  end

  # Callback entrypoint that defines the protocol functions.
  # It simply detects the protocol using __protocol_for__ and
  # then dispatches to it.
  @doc false
  def functions(module, funs, fallback) do
    contents = lc fun in L.reverse(funs), do: each_function(fun, fallback)
    Module.eval_quoted module, contents, [], file: __FILE__, line: __LINE__
  end

  # Implements the function that detects the protocol and returns
  # the module to dispatch to. Returns module.Record for records
  # which should be properly handled by the dispatching function.
  @doc false
  def protocol_for(module, conversions, fallback) do
    contents = lc kind in conversions, do: each_protocol_for(kind, conversions)

    # If we don't implement all protocols and any is not in the
    # list, we need to add a final clause that returns nil.
    if !L.member({ Any, :is_any }, conversions) && length(conversions) < 10 do
      contents = contents ++ [quote do
        defp __raw_protocol__(_) do
          nil
        end
      end]
    end

    # Finally add __protocol_for__ and __protocol_for__!
    contents = contents ++ [quote do
      def __protocol_for__(arg) do
        case __raw_protocol__(arg) do
        match: __MODULE__.Record
          target = Module.concat(__MODULE__, :erlang.element(1, arg))
          if :erlang.function_exported(target, :__protocol__, 1) do
            target
          else:
            Module.concat(__MODULE__, unquote(fallback))
          end
        match: other
          other
        end
      end

      def __protocol_for__!(arg) do
        if module = __protocol_for__(arg) do
          module
        else:
          raise Protocol.UndefinedError, protocol: __MODULE__, structure: arg
        end
      end
    end]

    Module.eval_quoted module, contents, [], file: __FILE__, line: __LINE__
  end

  # Returns the default conversions according to the given
  # only/except options.
  @doc false
  def conversions_for(opts) do
    kinds = all_types

    conversions =
      if only = Keyword.get(opts, :only, false) do
        L.map(fn(i) -> L.keyfind(i, 1, kinds) end, only)
      else:
        except = Keyword.get(opts, :except, [Any])
        L.foldl(fn(i, list) -> L.keydelete(i, 1, list) end, kinds, except)
      end

    fallback = if L.keyfind(Tuple, 1, conversions), do: Tuple, else: Any
    { conversions, fallback }
  end

  ## Helpers

  defp all_types do
    [
      { Record,    :is_record },
      { Tuple,     :is_tuple },
      { Atom,      :is_atom },
      { List,      :is_list },
      { BitString, :is_bitstring },
      { Number,    :is_number },
      { Function,  :is_function },
      { PID,       :is_pid },
      { Port,      :is_port },
      { Reference, :is_reference },
      { Any,       :is_any }
    ]
  end

  # Returns a quoted expression that allow to checks
  # if a variable named first is built in or not.
  defp is_builtin?([{h,_}]) do
    quote do
      first == unquote(h)
    end
  end

  defp is_builtin?([{h,_}|t]) do
    quote do
      first == unquote(h) or unquote(is_builtin?(t))
    end
  end

  # Specially handle tuples as they can also be record.
  # If this is the case, module.Record will be returned.
  defp each_protocol_for({ _, :is_record }, conversions) do
    quote do
      defp __raw_protocol__(arg) when is_tuple(arg) and is_atom(:erlang.element(1, arg)) do
        first = :erlang.element(1, arg)
        case unquote(is_builtin?(conversions)) do
        match: true
          __MODULE__.Tuple
        match: false
          case atom_to_list(first) do
          match: '__MAIN__' ++ _
            __MODULE__.Record
          else:
            __MODULE__.Tuple
          end
        end
      end
    end
  end

  # Special case any as we don't need to generate a guard.
  defp each_protocol_for({ _, :is_any }, _) do
    quote do
      defp __raw_protocol__(_) do
        __MODULE__.Any
      end
    end
  end

  # Generate all others protocols.
  defp each_protocol_for({ kind, fun }, _) do
    quote do
      defp __raw_protocol__(arg) when unquote(fun).(arg) do
        Module.concat __MODULE__, unquote(kind)
      end
    end
  end

  # Implement the protocol invocation callbacks for each function.
  defp each_function({ name, arity }, fallback) do
    # Generate arguments according the arity. The arguments
    # are named xa, xb and so forth. We cannot use string
    # interpolation to generate the arguments because of compile
    # dependencies, so we use the <<>> instead.
    args = lc i in :lists.seq(1, arity) do
      { binary_to_atom(<<?x, i + 64>>), 0, :quoted }
    end

    quote do
      def unquote(name).(unquote_splicing(args)) do
        args = [unquote_splicing(args)]
        case __raw_protocol__(xA) do
        match: __MODULE__.Record
          try do
            target = Module.concat(__MODULE__, :erlang.element(1, xA))
            apply target, unquote(name), args
          rescue: UndefinedFunctionError
            apply Module.concat(__MODULE__, unquote(fallback)), unquote(name), args
          end
        match: nil
          raise Protocol.UndefinedError, protocol: __MODULE__, structure: xA
        match: other
          apply other, unquote(name), args
        end
      end
    end
  end

  # Converts the protocol expressions as [each(collection), length(collection)]
  # to an ordered dictionary [each: 1, length: 1] also checking for invalid args
  defp to_funs(args) do
    lc(x in args) ->
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
end