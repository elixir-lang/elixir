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
    for path <- paths,
       file <- list_dir(path),
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
    raise ArgumentError, "consolidation is disabled as we can't consolidate records " <>
                         "and structs at once. Consolidation will be added back once " <>
                         "polymorphic records are removed"
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
    all     = [Any] ++ for { _guard, mod } <- Protocol.builtin, do: mod
    structs = types -- all
    change_impl_for(code, protocol, types, structs, false, [])
  end

  defp change_impl_for([{ :attribute, line, :protocol, opts }|t], protocol, types, structs, _, acc) do
    opts = [fallback_to_any: opts[:fallback_to_any], consolidated: true]
    change_impl_for(t, protocol, types, structs, true,
                    [{ :attribute, line, :protocol, opts }|acc])
  end

  defp change_impl_for([{ :function, line, :impl_for, 1, _ }|t], protocol, types, structs, is_protocol, acc) do
    fallback = if Any in types, do: Module.concat(protocol, Any), else: nil

    clauses = for { guard, mod } <- Protocol.builtin,
                  mod in types,
                  do: builtin_clause_for(mod, guard, protocol, line)

    clauses = [struct_clause_for(line)|clauses] ++
              [fallback_clause_for(fallback, protocol, line)]

    change_impl_for(t, protocol, types, structs, is_protocol,
                    [{ :function, line, :impl_for, 1, clauses }|acc])
  end

  defp change_impl_for([{ :function, line, :struct_impl_for, 1, _ }|t], protocol, types, structs, is_protocol, acc) do
    fallback = if Any in types, do: Module.concat(protocol, Any), else: nil
    clauses = for struct <- structs, do: each_struct_clause_for(struct, protocol, line)
    clauses = clauses ++ [fallback_clause_for(fallback, protocol, line)]

    change_impl_for(t, protocol, types, structs, is_protocol,
                    [{ :function, line, :struct_impl_for, 1, clauses }|acc])
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

  defp builtin_clause_for(mod, guard, protocol, line) do
    {:clause, line,
      [{:var, line, :x}],
      [[{:call, line,
          {:remote, line, {:atom, line, :erlang}, {:atom, line, guard}},
          [{:var, line, :x}],
      }]],
      [{:atom, line, Module.concat(protocol, mod)}]}
  end

  defp struct_clause_for(line) do
    {:clause, line,
      [{:map, line, [
        {:map_field_exact, line, {:atom, line, :__struct__}, {:var, line, :x}}
      ]}],
      [[{:call, line,
          {:remote, line, {:atom, line, :erlang}, {:atom, line, :is_atom}},
          [{:var, line, :x}],
      }]],
      [{:call, line,
          {:atom, line, :struct_impl_for},
          [{:var, line, :x}]}]}
  end

  defp each_struct_clause_for(other, protocol, line) do
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
