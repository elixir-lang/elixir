module Protocol do
  # Handle `defprotocol` when it is declared in the current module.
  def defprotocol(nil, args) do
    quote do
      require ::Protocol
      Protocol.reflection(unquote(args))
      Protocol.functions(__MODULE__, unquote(args), [])
    end
  end

  # Handle `defprotocol` when it is declared as a module.
  def defprotocol(name, args) do
    quote do
      module __MODULE__ :: name do
        require ::Protocol
        Protocol.reflection(unquote(args))
        Protocol.functions(unquote(name), unquote(args), [])
      end
    end
  end

  # Returns the reflection for this protocol, which is
  # is a function called __protocol__ that returns a key-value
  # with functions to be implemented and their arity.
  defmacro reflection(args) do
    kv = to_kv(args)

    quote do
      def __protocol__, do: unquote(kv)
    end
  end

  # Implement the protocol invocation callbacks for each function.
  defmacro functions(module, [{name,_,args}|t], acc) do
    # Replace the first argument by a known name called __arg.
    # We don't care about the other arguments as they are simply
    # passed along.
    args = [quote { __arg }|tl(args)]

    new = quote do
      def unquote(name).(unquote_splice(args)) when is_atom(__arg) do
        apply unquote(module)::Atom, unquote(name), [unquote_splice(args)]
      end
    end

    functions(module, t, [new|acc])
  end

  defmacro functions(_name, [], acc), do: acc

  # Converts the protocol expressions as [each(collection), length(collection)]
  # to an ordered dictionary [each: 1, length: 1] also checking for invalid args.
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
end