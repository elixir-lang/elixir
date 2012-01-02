defmodule Protocol do
  require Erlang.lists, as: L
  import Orddict, only: [fetch: 3]

  # Handle `defprotocol` when it is declared in the current module.
  def defprotocol(nil, args, opts) do
    kv = to_kv(args)
    quote do
      def __protocol__, do: unquote(kv)
      Protocol.functions(__MODULE__, __MODULE__, unquote(kv), unquote(opts))
    end
  end

  # Handle `defprotocol` when it is declared as a module.
  def defprotocol(name, args, opts) do
    kv = to_kv(args)
    quote do
      defmodule __MODULE__ :: unquote(name) do
        def __protocol__, do: unquote(kv)
        Protocol.functions(__MODULE__, unquote(name), unquote(kv), unquote(opts))
      end
    end
  end

  # Implement the given protocol according to the given option.
  def defimpl(protocol, opts) do
    block = fetch(opts, :do, nil)
    for   = fetch(opts, :for, nil)

    quote do
      # Build up the name, protocol and block
      protocol = unquote(protocol)
      for      = unquote(for) || __MODULE__
      name     = protocol::for

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
      defmodule name, do: unquote(block)

      # Check if the implemented protocol was valid
      exports   = name.module_info(:exports)
      remaining = funs -- exports

      if remaining != [], do:
        error { :badarg, "#{name} did not implement #{protocol}, missing: #{remaining}" }
    end
  end

  # Callback entrypoint that defines the protocol functions.
  # It receives the target module, functions tuples and the options
  # that should be used to calculate which kinds we shuold generate
  # functions for.
  def functions(target, module, funs, opts) do
    kinds = conversions_for(opts)
    List.each funs, fn(fun) { each_function(target, module, fun, kinds) }
  end

  ## Helpers

  # Implement the protocol invocation callbacks for
  # each function considering all kinds selected.
  defp each_function(target, module, {name,arity}, kinds) do
    args = generate_args(arity, [])
    List.each kinds, fn(kind) { each_function_kind(target, module, name, args, kind) }
  end

  # For each function and kind, add a new function to the module.
  # We need to special case tuples so it properly handle records.
  # All the other cases simply do a function dispatch.
  defp each_function_kind(target, module, name, args, { Tuple, :is_tuple }) do
    Module.eval target, __FILE__, __LINE__, quote {
      def unquote(name).(unquote_splice(args)) when xA == {} do
        apply unquote(module)::Tuple, unquote(name), [unquote_splice(args)]
      end

      def unquote(name).(unquote_splice(args)) when is_tuple(xA) do
        first = element(1, xA)

        if is_atom(first) do
          try do
            apply unquote(module)::element(1, xA), unquote(name), [unquote_splice(args)]
          catch: { :error, :undef, _ }
            apply unquote(module)::Tuple, unquote(name), [unquote_splice(args)]
          end
        else:
          apply unquote(module)::Tuple, unquote(name), [unquote_splice(args)]
        end
      end
    }
  end

  defp each_function_kind(target, module, name, args, { kind, fun }) do
    Module.eval target, __FILE__, __LINE__, quote {
      def unquote(name).(unquote_splice(args)) when unquote(fun).(xA) do
        apply unquote(module)::unquote(kind), unquote(name), [unquote_splice(args)]
      end
    }
  end

  # Converts the protocol expressions as [each(collection), length(collection)]
  # to an ordered dictionary [each: 1, length: 1] also checking for invalid args
  defmacro to_kv(args) do
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

  # Geenerate arguments according the arity. The arguments
  # are named xa, xb and so forth. We cannot use string
  # interpolation to generate the arguments because of compile
  # dependencies, so we use the bitstr macro instead.
  defp generate_args(0, acc) do
    acc
  end

  defp generate_args(counter, acc) do
    name = binary_to_atom(bitstr(?x, counter + 64), :utf8)
    generate_args(counter - 1, [{ name, 0, false }|acc])
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
      selected = List.map only, fn(i) { L.keyfind(i, 1, kinds) }
      selected ++ [{ Any, :is_any }]
    elsif: except = fetch(opts, :except, false)
      selected = List.foldl except, kinds, fn(i, list) { L.keydelete(i, 1, list) }
      selected ++ [{ Any, :is_any }]
    else:
      kinds
    end
  end
end