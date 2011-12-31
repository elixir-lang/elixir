module Protocol do
  require Erlang.lists, as: L
  import Orddict, only: [fetch: 3]

  # Handle `defprotocol` when it is declared in the current module.
  def defprotocol(nil, args, opts) do
    quote do
      require ::Protocol
      Protocol.reflection(unquote(args))
      Protocol.functions(__MODULE__, unquote(args), unquote(opts))
    end
  end

  # Handle `defprotocol` when it is declared as a module.
  def defprotocol(name, args, opts) do
    quote do
      module __MODULE__ :: name do
        require ::Protocol
        Protocol.reflection(unquote(args))
        Protocol.functions(unquote(name), unquote(args), unquote(opts))
      end
    end
  end

  def defimpl(protocol, opts) do
    block = fetch(opts, :do, nil)
    for   = fetch(opts, :for, nil)

    quote do
      # Build up the name, protocol and block
      protocol = unquote(protocol)
      name     = protocol::(unquote(for) || __MODULE__)

      # Check if protocol is loaded
      try do
        protocol.module_info
      catch: { :error, :undef, _ }
        error { :badarg, "#{protocol} is not loaded" }
      end

      # Check if protocol is really a protocol
      funs = try do
        protocol.__protocol__
      catch: { :error, :undef, _ }
        error { :badarg, "#{protocol} is not a protocol" }
      end

      # Create a module with the given contents
      module name, do: unquote(block)

      # Check if the implemented protocol was valid
      exports   = name.module_info(:exports)
      remaining = funs -- exports

      if remaining != [], do:
        error { :badarg, "#{name} did not implement #{protocol}, missing: #{remaining}" }
    end
  end

  ## Entry callback functions

  # Returns the reflection for this protocol, which is
  # is a function called __protocol__ that returns a key-value
  # with functions to be implemented and their arity.
  defmacro reflection(args) do
    quote do
      def __protocol__, do: unquote(to_kv(args))
    end
  end

  defmacro functions(module, funs, opts) do
    List.map funs, fn(fun) { each_function(module, fun, conversions(opts)) }
  end

  ## Helpers

  # Implement the protocol invocation callbacks for each function.

  defp each_function(module, {name,_,args}, original_kinds) do
    # Replace the first argument by a known name called __arg.
    # We don't care about the other args as they are simply passed along.
    args = [quote { __arg }|tl(args)]

    # Remove tuples from the list, it will be special cased later.
    tuple = { Tuple, :is_tuple }
    kinds = L.delete(tuple, original_kinds)

    acc = List.map kinds, fn({kind, fun}) {
      quote do
        def unquote(name).(unquote_splice(args)) when unquote(fun).(__arg) do
          apply unquote(module)::unquote(kind), unquote(name), [unquote_splice(args)]
        end
      end
    }

    # Protocols for tuples and records require a more complex lookup
    if L.member(tuple, original_kinds) do
      new = quote do
        def unquote(name).(unquote_splice(args)) when __arg == {} do
          apply unquote(module)::Tuple, unquote(name), [unquote_splice(args)]
        end

        def unquote(name).(unquote_splice(args)) when is_tuple(__arg) do
          first = element(1, __arg)

          if is_atom(first) do
            try do
              apply unquote(module)::element(1, __arg), unquote(name), [unquote_splice(args)]
            catch: { :error, :undef, _ }
              apply unquote(module)::Tuple, unquote(name), [unquote_splice(args)]
            end
          else:
            apply unquote(module)::Tuple, unquote(name), [unquote_splice(args)]
          end
        end
      end
      [new|acc]
    else:
      acc
    end
  end

  # Converts the protocol expressions as [each(collection), length(collection)]
  # to an ordered dictionary [each: 1, length: 1] also checking for invalid args

  defp to_kv(args) do
    Orddict.from_list List.map(args, fn(x) {
      case x do
      match: { _, _, args } when args == [] or args == false
        error({ :badarg, "protocol functions expect at least one argument" })
      match: { name, _, args }
        { name, length(args) }
      else:
        error({ :badarg, "invalid args for defprotocol" })
      end
    })
  end

  # Returns the default conversions according to the given only/except options.

  defp conversions(opts) do
    kinds = [
      { Tuple,     :is_tuple },
      { Atom,      :is_atom },
      { List,      :is_list },
      { BitString, :is_bitstring },
      { Number,    :is_number },
      { Functions, :is_function },
      { PID,       :is_pid },
      { Port,      :is_port },
      { Reference, :is_reference },
      { Any,       :is_any }
    ]

    if only = fetch(opts, :only, false) do
      List.map only, fn(i) { { i, L.keyfind(i, 1, kinds) } }
    elsif: except = fetch(opts, :except, false)
      List.foldl except, kinds, fn(i, list) { L.keydelete(i, 1, list) }
    else:
      L.delete { Any, :is_any }, kinds
    end
  end
end