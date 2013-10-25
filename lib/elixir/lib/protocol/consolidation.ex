defmodule Protocol.Consolidation do
  @moduledoc """
  Module responsible for consolidating protocols and helpers for
  extracting protocols and implementations from code paths for
  consolidation.
  """

  @doc """
  Extract all protocols from the given paths.

  The paths can be either a char list or a string. Internally
  they are worked on as char lists, so passing them as lists
  avoid extra conversion.

  ## Examples

      # Get Elixir's ebin and retrieve all protocols
      iex> path = :code.lib_dir(:elixir, :ebin)
      iex> mods = Protocol.Consolidation.extract_protocols([path])
      iex> Enumerable in mods
      true

  """
  @spec extract_protocols([char_list | String.t]) :: [atom]
  def extract_protocols(paths) do
    extract_matching_by_attribute paths, 'Elixir.',
      fn module, attributes ->
        case attributes[:protocol] do
          [fallback_to_any: _, consolidated: _] -> module
          _ -> nil
        end
      end
  end

  @doc """
  Extract all types implemented for the given protocol from
  the given paths.

  The paths can be either a char list or a string. Internally
  they are worked on as char lists, so passing them as lists
  avoid extra conversion.

  ## Examples

      # Get Elixir's ebin and retrieve all protocols
      iex> path = :code.lib_dir(:elixir, :ebin)
      iex> mods = Protocol.Consolidation.extract_impls(Enumerable, [path])
      iex> List in mods
      true

  """
  @spec extract_impls(module, [char_list | String.t]) :: [atom]
  def extract_impls(protocol, paths) when is_atom(protocol) do
    prefix = atom_to_list(protocol) ++ '.'
    extract_matching_by_attribute paths, prefix, fn
      _mod, attributes ->
        case attributes[:impl] do
          [protocol: ^protocol, for: for] -> for
          _ -> nil
        end
    end
  end

  defp extract_matching_by_attribute(paths, prefix, callback) do
    lc path inlist paths,
       file inlist list_dir(path),
       mod = extract_from_file(path, file, prefix, callback),
       do: mod
  end

  defp list_dir(path) when is_list(path) do
    case :file.list_dir(path) do
      { :ok, files } -> files
      _ -> []
    end
  end

  defp list_dir(path), do: list_dir(to_char_list(path))

  defp extract_from_file(path, file, prefix, callback) do
    if :lists.prefix(prefix, file) and Path.extname(file) == '.beam' do
      extract_from_beam(Path.join(path, file), callback)
    end
  end

  defp extract_from_beam(file, callback) do
    case :beam_lib.chunks(file, [:attributes]) do
      {:ok, { module, [attributes: attributes] } } ->
        callback.(module, attributes)
       _ ->
         nil
    end
  end

  defmacrop if_ok(expr, call) do
    quote do
      case unquote(expr) do
        { :ok, var } -> unquote(Macro.pipe(quote(do: var), call))
        other -> other
      end
    end
  end

  @doc """
  Receives a protocol and a list of implementations and
  consolidates the given protocol. Consolidation happens
  by changing the protocol `impl_for` in the abstract
  format to have fast lookup rules.

  It returns the updated version of the protocol bytecode.
  A given bytecode or protocol implementation can be checked
  to be consolidated or not by analyzing the protocol
  attribute:

      Enumerable.__info__(:attributes)[:protocol]

  If the first element of the tuple is true, it means
  the protocol was consolidated.

  This function does not load the protocol at any point
  nor loads the new bytecode for the compiled module.
  """
  @spec apply_to(module, [module]) ::
    { :ok, binary } |
    { :error, :not_a_protocol } |
    { :error, :no_beam_info }
  def apply_to(protocol, types) when is_atom(protocol) do
    ensure_protocol(protocol)
    |> if_ok(change_debug_info types)
    |> if_ok(compile)
  end

  # Ensure the given module is loaded and is a protocol.
  defp ensure_protocol(protocol) do
    case :beam_lib.chunks(beam_file(protocol), [:abstract_code, :attributes]) do
      { :ok, { ^protocol, [abstract_code: { _raw, abstract_code },
                           attributes: attributes] } } ->
        case attributes[:protocol] do
          [fallback_to_any: any, consolidated: _] ->
            { :ok, { protocol, any, abstract_code } }
          _ ->
            { :error, :not_a_protocol }
        end
      _ ->
        { :error, :no_beam_info }
    end
  end

  defp beam_file(module) when is_atom(module) do
    case :code.which(module) do
      :non_existing -> module
      file -> file
    end
  end

  # Change the debug information to the optimized
  # impl_for/1 dispatch version.
  defp change_debug_info({ protocol, any, code }, types) do
    types   = if any, do: types, else: List.delete(types, Any)
    records = types -- Protocol.builtin
    builtin = Protocol.builtin -- (Protocol.builtin -- types)
    builtin = if records != [], do: [Record|builtin], else: builtin
    change_impl_for(code, protocol, builtin, records, false, [])
  end

  defp change_impl_for([{ :attribute, line, :protocol, _ }|t], protocol, builtin, records, _, acc) do
    attr = [fallback_to_any: Any in builtin, consolidated: true]
    change_impl_for(t, protocol, builtin, records, true,
                    [{ :attribute, line, :protocol, attr }|acc])
  end

  defp change_impl_for([{ :function, line, :impl_for, 1, _ }|t], protocol, builtin, records, is_protocol, acc) do
    clauses = lc type inlist builtin, do: clause_for(type, protocol, line)

    unless Any in builtin do
      clauses = clauses ++ [fallback_clause_for(nil, protocol, line)]
    end

    change_impl_for(t, protocol, builtin, records, is_protocol,
                    [{ :function, line, :impl_for, 1, clauses }|acc])
  end

  defp change_impl_for([{ :function, line, :rec_impl_for, 1, _ }|t], protocol, builtin, records, is_protocol, acc) do
    fallback = if Tuple in builtin, do: Module.concat(protocol, Tuple)
    clauses  = lc type inlist records, do: record_clause_for(type, protocol, line)
    clauses  = clauses ++ [fallback_clause_for(fallback, protocol, line)]

    change_impl_for(t, protocol, builtin, records, is_protocol,
                    [{ :function, line, :rec_impl_for, 1, clauses }|acc])
  end

  defp change_impl_for([h|t], protocol, info, types, is_protocol, acc) do
    change_impl_for(t, protocol, info, types, is_protocol, [h|acc])
  end

  defp change_impl_for([], protocol, _info, _types, is_protocol, acc) do
    if is_protocol do
      { :ok, { protocol, Enum.reverse(acc) } }
    else
      { :error, :not_a_protocol }
    end
  end

  defp clause_for(Tuple, protocol, line),     do: builtin_clause_for(Tuple, :is_tuple, protocol, line)
  defp clause_for(Atom, protocol, line),      do: builtin_clause_for(Atom, :is_atom, protocol, line)
  defp clause_for(List, protocol, line),      do: builtin_clause_for(List, :is_list, protocol, line)
  defp clause_for(BitString, protocol, line), do: builtin_clause_for(BitString, :is_bitstring, protocol, line)
  defp clause_for(Integer, protocol, line),   do: builtin_clause_for(Integer, :is_integer, protocol, line)
  defp clause_for(Float, protocol, line),     do: builtin_clause_for(Float, :is_float, protocol, line)
  defp clause_for(Function, protocol, line),  do: builtin_clause_for(Function, :is_function, protocol, line)
  defp clause_for(PID, protocol, line),       do: builtin_clause_for(PID, :is_pid, protocol, line)
  defp clause_for(Port, protocol, line),      do: builtin_clause_for(Port, :is_port, protocol, line)
  defp clause_for(Reference, protocol, line), do: builtin_clause_for(Reference, :is_reference, protocol, line)

  defp clause_for(Any, protocol, line) do
    {:clause, line, [{:var, line, :_}], [],
      [{ :atom, line, Module.concat(protocol, Any) }]}
  end

  defp clause_for(Record, _protocol, line) do
    {:clause, line, [{:var, line, :x}],
      [[{:op, line, :andalso,
          {:call, line,
            {:remote, line, {:atom, line, :erlang}, {:atom, line, :is_tuple}},
            [{:var, line, :x}]},
          {:call, line,
            {:remote, line, {:atom, line, :erlang}, {:atom, line, :is_atom}},
            [{:call, line,
              {:remote, line, {:atom, line, :erlang}, {:atom, line, :element}},
              [{:integer, line, 1}, {:var, line, :x}]
            }]},
      }]],
      [{:call, line,
          {:atom, line, :rec_impl_for},
          [{:call, line,
            {:remote, line, {:atom, line, :erlang}, {:atom, line, :element}},
            [{:integer, line, 1}, {:var, line, :x}]}]}]}
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

  defp record_clause_for(other, protocol, line) do
    {:clause, line, [{:atom, line, other}], [],
      [{:atom, line, Module.concat(protocol, other)}]}
  end

  defp fallback_clause_for(value, _protocol, line) do
    {:clause, line, [{:var, line, :_}], [],
      [{ :atom, line, value }]}
  end

  # Finally compile the module and emit its bytecode.
  defp compile({ protocol, code }) do
    opts = if Code.compiler_options[:debug_info], do: [:debug_info], else: []
    { :ok, ^protocol, binary, _warnings } = :compile.forms(code, [:return|opts])
    { :ok, binary }
  end
end
