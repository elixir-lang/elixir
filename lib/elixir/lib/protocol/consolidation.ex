defmodule Protocol.Consolidation do
  @moduledoc false

  @doc """
  Receives a protocol and a list of implementations.
  """
  @spec apply_to(module, [module]) ::
    { :ok, binary } |
    { :error, :not_loaded } |
    { :error, :not_a_protocol } |
    { :error, :no_beam_info }

  def apply_to(protocol, types) when is_atom(protocol) do
    { :ok, protocol }
    |> ensure_protocol
    |> read_debug_info
    |> change_debug_info(types)
    |> recompile
  end

  # Ensure the given module is loaded and is a protocol.
  defp ensure_protocol({ :ok, protocol }) do
    if Code.ensure_loaded?(protocol) do
      if function_exported?(protocol, :__protocol__, 1) do
        { :ok, protocol }
      else
        { :error, :not_a_protocol }
      end
    else
      { :error, :not_loaded }
    end
  end

  defp ensure_protocol(other), do: other

  # Read the debug information from the protocol, fails if not available.
  defp read_debug_info({ :ok, protocol }) do
    case :beam_lib.chunks(beam_file(protocol), [:abstract_code]) do
      {:ok, { _, [{ :abstract_code, { _raw_abstract_v1, abstract_code } }] } } ->
        { :ok, protocol, abstract_code }
      _ ->
        { :error, :no_beam_info }
    end
  end

  defp read_debug_info(other), do: other

  defp beam_file(module) when is_atom(module) do
    case :code.which(module) do
      :non_existing -> module
      file -> file
    end
  end

  # Change the debug information to the optimized
  # impl_for/1 dispatch version.
  defp change_debug_info({ :ok, protocol, code }, types) do
    change_impl_for(code, protocol, types, [])
  end

  defp change_debug_info(other, _types), do: other

  defp change_impl_for([{ :function, line, :impl_for, 1, _ }|t], protocol, types, acc) do
    # Get all protocols available in prioritized fashion
    prioritized = protocol.__protocol__(:prioritize)
    prioritized = prioritized ++ Enum.reduce(prioritized, Protocol.builtin, &:lists.delete/2)

    # Now prioritize the given types given the prioritized compiled
    all     = prioritize(prioritized, types)
    clauses = lc type inlist all, do: clause_for(type, protocol, line)

    unless Any in all do
      clauses = clauses ++ [fallback_clause_for(protocol, line)]
    end

    { :ok, protocol, Enum.reverse(acc) ++ [{ :function, line, :impl_for, 1, clauses }|t] }
  end

  defp change_impl_for([h|t], protocol, types, acc) do
    change_impl_for(t, protocol, types, [h|acc])
  end

  defp change_impl_for([], _protocol, _types, _acc) do
    { :error, :not_a_protocol }
  end

  # Records should expand to all non-builtin types
  defp prioritize([Record|t], types) do
    records = types -- Protocol.builtin
    records ++ prioritize(t, types)
  end

  # For each builtin type, get it from the list if available
  defp prioritize([h|t], types) do
    if h in types do
      [h|prioritize(t, types)]
    else
      prioritize(t, types)
    end
  end

  defp prioritize([], _types) do
    []
  end

  defp clause_for(Tuple, protocol, line),     do: builtin_clause_for(Tuple, :is_tuple, protocol, line)
  defp clause_for(Atom, protocol, line),      do: builtin_clause_for(Atom, :is_atom, protocol, line)
  defp clause_for(List, protocol, line),      do: builtin_clause_for(List, :is_list, protocol, line)
  defp clause_for(BitString, protocol, line), do: builtin_clause_for(BitString, :is_bitstring, protocol, line)
  defp clause_for(Number, protocol, line),    do: builtin_clause_for(Number, :is_number, protocol, line)
  defp clause_for(Function, protocol, line),  do: builtin_clause_for(Function, :is_function, protocol, line)
  defp clause_for(PID, protocol, line),       do: builtin_clause_for(PID, :is_pid, protocol, line)
  defp clause_for(Port, protocol, line),      do: builtin_clause_for(Port, :is_port, protocol, line)
  defp clause_for(Reference, protocol, line), do: builtin_clause_for(Reference, :is_reference, protocol, line)

  defp clause_for(Any, protocol, line) do
    {:clause, line, [{:var, line, :_}], [],
      [{ :atom, line, Module.concat(protocol, Any) }]}
  end

  defp clause_for(other, protocol, line) do
    {:clause, line, [{:var, line, :x}],
      [[{:op, line, :andalso,
          {:call, line,
            {:remote, line, {:atom, line, :erlang}, {:atom, line, :is_tuple}},
            [{:var, line, :x}]},
          {:op, line, :==,
            {:call, line,
              {:remote, line, {:atom, line, :erlang}, {:atom, line, :element}},
              [{:integer, line, 1}, {:var, line, :x}]},
            {:atom, line, other}}
      }]],
      [{:atom, line, Module.concat(protocol, other)}]}
  end

  defp builtin_clause_for(mod, guard, protocol, line) do
    {:clause, line,
      [{:var, line, :x}],
      [[{:call, line,
          {:remote, line, {:atom, line, :erlang}, {:atom, line, guard}},
          [{:var, line, :x}],
      }]],
      [{:atom, line, Module.concat(protocol, mod)}]}
  end

  defp fallback_clause_for(_protocol, line) do
    {:clause, line, [{:var, line, :_}], [],
      [{ :atom, line, nil }]}
  end

  # Finally recompiled the module and emit its bytecode.
  defp recompile({ :ok, protocol, code }) do
    opts = if Code.compiler_options[:debug_info], do: [:debug_info], else: []
    { :ok, ^protocol, binary, _warnings } = :compile.forms(code, [:return|opts])
    :code.load_binary(protocol, protocol.__info__(:compile)[:source], binary)
    { :ok, binary }
  end

  defp recompile(other) do
    other
  end
end
