defmodule Protocol do
  @moduledoc false

  # We need to use :lists because Enum is not available yet
  require :lists, as: L

  @doc """
  Handle `defprotocol`. It will define a function for each
  protocol plus two extra functions:

  * `__protocol__/1` - returns the protocol name when :name is given,
                       and a keyword list with the protocol functions
                       when :functions is given;

  * `__impl_for__/1` - receives one argument and returns a module
                       that implements the protocol for the given
                       data type. If no implementation matches, returns nil;

  * `__impl_for__!/1` - same as above but raises an error if an implementation is not found
  """
  def defprotocol(name, [do: block]) do
    quote do
      defmodule unquote(name) do
        # We don't want to allow function definition inside protocols
        import Kernel, except: [
          defmacro: 1, defmacro: 2, defmacro: 4,
          defp: 1, defp: 2, defp: 4,
          def: 1, def: 2, def: 4
        ]

        # Import the new dsl that holds the new def
        import Protocol.DSL

        # Set up a clear slate to store defined functions
        @functions []

        # Invoke the user given block
        unquote(block)

        # Define callbacks and meta information
        { conversions, fallback } = Protocol.conversions_for(__MODULE__, @only, @except)
        Protocol.impl_for(__ENV__, conversions, fallback)
        Protocol.meta(__ENV__, @functions, fallback)
      end
    end
  end

  @doc """
  Implement the given protocol for the given module.
  It also defines a `__impl__` function which
  returns the protocol being implemented.
  """
  def defimpl(protocol, [do: block, for: for]) when is_list(for) do
    lc f inlist for, do: Protocol.defimpl(protocol, [do: block, for: f])
  end
  def defimpl(protocol, [do: block, for: for]) do
    quote do
      protocol = unquote(protocol)
      for      = unquote(for)
      name     = Module.concat(protocol, for)

      Protocol.assert_protocol(protocol)

      defmodule name do
        @behaviour unquote(protocol)
        unquote(block)
        def __impl__, do: unquote(protocol)
      end
    end
  end

  @doc """
  Check if the given module is a protocol. Raises an error
  if not loaded or not a protocol.
  """
  def assert_protocol(module) do
    case Code.ensure_compiled(module) do
      { :module, ^module } -> nil
      _ -> raise ArgumentError, message: "#{module} is not loaded"
    end

    try do
      module.__protocol__(:name)
    rescue
      UndefinedFunctionError ->
        raise ArgumentError, message: "#{module} is not a protocol"
    end
  end

  @doc """
  Defines meta information about the protocol and internal callbacks.
  """
  def meta(env, functions, fallback) do
    contents = quote do
      def __protocol__(:name),        do: __MODULE__
      def __protocol__(:functions),   do: unquote(:lists.sort(functions))
      @doc false
      def behaviour_info(:callbacks), do: [{ :__impl__, 0 }|__protocol__(:functions)]

      def __impl_for__(arg) do
        case __raw_impl__(arg) do
          __MODULE__.Record ->
            target = Module.concat(__MODULE__, :erlang.element(1, arg))
            try do
              target.__impl__
              target
            catch
              :error, :undef, [[{ ^target, :__impl__, [], _ }|_]|_] ->
                __fallback__
            end
          other ->
            other
        end
      end

      def __impl_for__!(arg) do
        if module = __impl_for__(arg) do
          module
        else
          raise Protocol.UndefinedError, protocol: __MODULE__, structure: arg
        end
      end

      defp __fallback__, do: unquote(fallback)
    end

    Module.eval_quoted env, contents
  end

  @doc """
  Implements the function that detects the protocol and returns
  the module to dispatch to. Returns module.Record for records
  which should be properly handled by the dispatching function.
  """
  def impl_for(env, conversions, fallback) do
    contents = lc kind inlist conversions do
      each_impl_for(kind, if fallback, do: conversions)
    end

    # If we don't implement all protocols and any is not in the
    # list, we need to add a final clause that returns nil.
    if !L.keyfind(Any, 1, conversions) && length(conversions) < 10 do
      contents = contents ++ [quote do
        defp __raw_impl__(_) do
          nil
        end
      end]
    end

    Module.eval_quoted env, contents
  end

  @doc """
  Returns the default conversions according to the given
  only/except options.
  """
  def conversions_for(module, only, except) do
    kinds = all_types

    conversions =
      if only do
        L.map(fn i -> L.keyfind(i, 1, kinds) end, only)
      else
        except = except || [Any]
        L.foldl(fn i, list -> L.keydelete(i, 1, list) end, kinds, except)
      end

    fallback = cond do
      L.keyfind(Tuple, 1, conversions) ->
        Module.concat module, Tuple
      L.keyfind(Any, 1, conversions) ->
        Module.concat module, Any
      true ->
        nil
    end

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

  # Returns a quoted expression that allows to check
  # if the first item in the tuple is a built-in or not.
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

  # Handle records when we don't have fallbacks.
  # It simply gets the first element of the tuple.
  # This case assumes that, whenever a tuple is given
  # it is meant to be a record, so we don't need extra
  # checks.
  defp each_impl_for({ _, :is_record }, nil) do
    quote do
      defp __raw_impl__(arg) when is_tuple(arg) and is_atom(:erlang.element(1, arg)) do
        __MODULE__.Record
      end
    end
  end

  # Specially handle records in the case we have fallbacks.
  defp each_impl_for({ _, :is_record }, conversions) do
    quote do
      defp __raw_impl__(arg) when is_tuple(arg) and is_atom(:erlang.element(1, arg)) do
        first = :erlang.element(1, arg)
        case unquote(is_builtin?(conversions)) do
          true  -> __fallback__
          false ->
            case atom_to_list(first) do
              'Elixir-' ++ _ -> __MODULE__.Record
              _              -> __fallback__
            end
        end
      end
    end
  end

  # Special case any as we don't need to generate a guard.
  defp each_impl_for({ _, :is_any }, _) do
    quote do
      defp __raw_impl__(_) do
        __MODULE__.Any
      end
    end
  end

  # Generate all others protocols.
  defp each_impl_for({ kind, fun }, _) do
    quote do
      defp __raw_impl__(arg) when unquote(fun).(arg) do
        Module.concat __MODULE__, unquote(kind)
      end
    end
  end
end

defmodule Protocol.DSL do
  @moduledoc false

  defmacro def(expression) do
    case expression do
      { _, _, args } when args == [] or is_atom(args) ->
        raise ArgumentError, message: "protocol functions expect at least one argument"
      { name, _, args } when is_atom(name) and is_list(args) ->
        :ok
      _ ->
        raise ArgumentError, message: "invalid args for defprotocol"
    end

    arity = length(args)

    # Generate arguments according the arity. The arguments
    # are named xa, xb and so forth. We cannot use string
    # interpolation to generate the arguments because of compile
    # dependencies, so we use the <<>> instead.
    generated = lc i inlist :lists.seq(1, arity) do
      { binary_to_atom(<<?x, i + 64>>), 0, :quoted }
    end

    quote do
      # Append new function to the list
      @functions [unquote({name, arity})|@functions]

      # Generate a fake definition with the user
      # signature that will be used by docs
      Kernel.def unquote(name).(unquote_splicing(args))

      Kernel.def unquote(name).(unquote_splicing(generated)) do
        case __raw_impl__(xA) do
          __MODULE__.Record ->
            target = Module.concat(__MODULE__, :erlang.element(1, xA))
            try do
              target.unquote(name)(unquote_splicing(generated))
            catch
              :error, :undef, [[{ ^target, unquote(name), args, _ }|_]|_] when length(args) == unquote(arity) ->
                case __fallback__ do
                  nil ->
                    raise Protocol.UndefinedError, protocol: __MODULE__, structure: xA
                  other ->
                    apply other, unquote(name), [unquote_splicing(generated)]
                end
            end
          nil ->
            raise Protocol.UndefinedError, protocol: __MODULE__, structure: xA
          other ->
            apply other, unquote(name), [unquote_splicing(generated)]
        end
      end
    end
  end
end
