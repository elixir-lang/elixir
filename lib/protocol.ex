defmodule Protocol do
  # We need to use Erlang.lists because Enum is not available yet
  refer Erlang.lists, as: L
  import Orddict, only: [fetch: 3]

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
    as = Orddict.fetch(opts, :as, true)
    kv = to_kv(args)

    quote do
      defmodule unquote(name) do
        def __protocol__, do: unquote(kv)
        Protocol.functions(__MODULE__, unquote(kv))
        Protocol.protocol_for(__MODULE__, unquote(opts))
      end

      require unquote(name), as: unquote(as)
    end
  end

  # Implement the given protocol for the given module.
  # It also defines a `__impl__` function which
  # returns the protocol being implemented.
  def defimpl(protocol, do: block, for: for) do
    quote do
      # Build up the name, protocol and block
      protocol = unquote(protocol)
      for      = unquote(for)
      name     = protocol::for

      # Check if protocol is loaded
      try do
        protocol.module_info
      catch: :error, :undef
        error { :badarg, "#{protocol} is not loaded" }
      end

      # Check if protocol is really a protocol
      funs = try do
        protocol.__protocol__
      catch: :error, :undef
        error { :badarg, "#{protocol} is not a protocol" }
      end

      # Create a module with the given contents
      defmodule name do
        def __impl__, do: unquote(protocol)
        unquote(block)
      end

      # Check if the implemented protocol was valid
      remaining = funs -- name.module_info(:exports)

      if remaining != [], do:
        error { :badarg, "#{name} did not implement #{protocol}, missing: #{remaining}" }
    end
  end

  # Callback entrypoint that defines the protocol functions.
  # It simply detects the protocol using __protocol_for__ and
  # then dispatches to it.
  def functions(module, funs) do
    lc fun in L.reverse(funs), do: each_function(module, fun)
  end

  # Implements the method that detects the protocol and returns
  # the module to dispatch to. Returns module::Record for records
  # which should be properly handled by the dispatching function.
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
    args = generate_args(arity, [])

    contents = quote do
      def unquote(name).(unquote_splicing(args)) do
        args = [unquote_splicing(args)]
        case __protocol_for__(xA) do
        match: unquote(module)::Record
          result =
            try do
              { apply(unquote(module)::element(1, xA), unquote(name), args), true }
            catch: :error, :undef
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
    Orddict.from_list lc(x in args) {
      case x do
      match: { _, _, args } when args == [] or args == false
        error({ :badarg, "protocol functions expect at least one argument" })
      match: { name, _, args } when is_atom(name) and is_list(args)
        { name, length(args) }
      else:
        error({ :badarg, "invalid args for defprotocol", x })
      end
    }
  end

  # Geenerate arguments according the arity. The arguments
  # are named xa, xb and so forth. We cannot use string
  # interpolation to generate the arguments because of compile
  # dependencies, so we use the <<>> instead.
  defp generate_args(0, acc) do
    acc
  end

  defp generate_args(counter, acc) do
    name = binary_to_atom(<<?x, counter + 64>>, :utf8)
    generate_args(counter - 1, [{ name, 0, :quoted }|acc])
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

    if only = fetch(opts, :only, false) do
      selected = L.map fn(i) { L.keyfind(i, 1, kinds) }, only
      selected ++ [{ Any, :is_any }]
    elsif: except = fetch(opts, :except, false)
      selected = L.foldl fn(i, list) { L.keydelete(i, 1, list) }, kinds, except
      selected ++ [{ Any, :is_any }]
    else:
      kinds
    end
  end
end