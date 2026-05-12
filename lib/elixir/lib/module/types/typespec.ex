# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defmodule Module.Types.Typespec do
  @moduledoc false

  # Converts a typespec AST to a `Module.Types.Descr` value.
  #
  # The conversion assumes the AST has already been validated by
  # `Kernel.Typespec`. Unsupported subterms degrade to `dynamic()`
  # so a single unrecognized form does not poison the whole alias.
  #
  # Two errors surface up the call stack instead of degrading:
  #
  #   * `{:cycle, name, arity}` — re-entering an alias still marked
  #     `:pending` in `defined`. Recursive aliases are deferred to
  #     a future release.
  #
  #   * `{:parametric_unsupported, name, arity}` — a reference to an
  #     alias with arity > 0. Parametric types are deferred.

  import Module.Types.Descr

  @elixir_checker_version :elixir_erl.checker_version()

  @doc """
  Convert `ast` to a `Descr`, using `state.defined` to resolve local references.

  `state` must contain:

    * `:module` — the module being compiled, used to expand `__MODULE__`.
    * `:defined` — map of `{name, arity} => :pending | {kind, descr}`.

  Returns `{:ok, descr}` or `{:error, reason}`.
  """
  def to_descr(ast, state) do
    try do
      {:ok, do_to_descr(ast, state)}
    catch
      {:to_descr, reason} -> {:error, reason}
    end
  end

  # Unions
  defp do_to_descr({:|, _, [left, right]}, state) do
    left_descr = do_to_descr(left, state)
    right_descr = do_to_descr(right, state)
    union(left_descr, right_descr)
  end

  # Parenthesized / annotated forms — strip and recurse.
  defp do_to_descr({:"::", _, [_var, ast]}, state), do: do_to_descr(ast, state)

  # Atom literals
  defp do_to_descr(atom, _state) when is_atom(atom) do
    case atom do
      :any -> term()
      :term -> term()
      _ -> atom([atom])
    end
  end

  # Integer literals — represent as the full integer() set for now.
  # Singleton integer types are not yet represented in Descr.
  defp do_to_descr(int, _state) when is_integer(int), do: integer()

  # Empty list literal
  defp do_to_descr([], _state), do: empty_list()

  # Function spec: `(args -> result)` is parsed as `[{:->, _, [args, result]}]`.
  defp do_to_descr([{:->, _, [args, return]}], state) when is_list(args) do
    cond do
      # `(... -> result)` — variable arity. Not statically representable;
      # degrade to the top function type.
      Enum.any?(args, &match?({:..., _, _}, &1)) ->
        fun()

      true ->
        arg_types = Enum.map(args, &do_to_descr(&1, state))
        return_type = do_to_descr(return, state)
        fun(arg_types, return_type)
    end
  end

  # Non-empty proper list literal: [type]
  defp do_to_descr([elem], state), do: list(do_to_descr(elem, state))

  # Tuple literals: {a, b} is parsed as {:{}, _, [...]} only for arity != 2.
  # 2-tuples come through as plain {a, b}.
  defp do_to_descr({left, right}, state) do
    tuple([do_to_descr(left, state), do_to_descr(right, state)])
  end

  defp do_to_descr({:{}, _, elements}, state) do
    tuple(Enum.map(elements, &do_to_descr(&1, state)))
  end

  # Empty map literal
  defp do_to_descr({:%{}, _, []}, _state), do: empty_map()

  # Map literal with keyword-style atom keys: %{a: integer(), b: atom()}
  defp do_to_descr({:%{}, _, pairs}, state) do
    converted =
      for {k, v} <- pairs, is_atom(k) do
        {k, do_to_descr(v, state)}
      end

    if length(converted) == length(pairs) do
      closed_map(converted)
    else
      # Non-atom keys: not handled yet, fall back to open_map().
      open_map()
    end
  end

  # Struct literal: %Mod{field: type, ...}
  defp do_to_descr({:%, _, [module_ast, {:%{}, _, pairs}]}, state) do
    module =
      case module_ast do
        {:__MODULE__, _, _} -> state.module
        {:__aliases__, _, parts} -> Module.concat(parts)
        mod when is_atom(mod) -> mod
        _ -> nil
      end

    if is_atom(module) and module != nil do
      field_pairs =
        for {k, v} <- pairs, is_atom(k) do
          {k, do_to_descr(v, state)}
        end

      closed_map([{:__struct__, atom([module])} | field_pairs])
    else
      dynamic()
    end
  end

  # Remote type reference: Mod.name(args).
  defp do_to_descr({{:., _, [module_ast, name]}, _, args}, state)
       when is_atom(name) and is_list(args) do
    arity = length(args)

    case expand_module(module_ast) do
      nil ->
        dynamic()

      module when module == state.module ->
        # Self-qualified reference — treat as a local lookup.
        local_or_pending(name, arity, state)

      module when is_atom(module) ->
        resolve_remote(module, name, arity)
    end
  end

  # Built-in type calls and local references: name(arg1, arg2, ...).

  defp do_to_descr({name, _meta, args}, state) when is_atom(name) and (is_list(args) or is_atom(args)) do
    arg_list = if is_list(args), do: args, else: []
    arity = length(arg_list)
    builtin(name, arity, arg_list, state)
  end

  # Anything else — degrade.
  defp do_to_descr(_ast, _state), do: dynamic()

  ## Built-ins and local references

  defp builtin(:integer, 0, [], _state), do: integer()
  defp builtin(:float, 0, [], _state), do: float()
  defp builtin(:atom, 0, [], _state), do: atom()
  defp builtin(:boolean, 0, [], _state), do: boolean()
  defp builtin(:pid, 0, [], _state), do: pid()
  defp builtin(:port, 0, [], _state), do: port()
  defp builtin(:reference, 0, [], _state), do: reference()
  defp builtin(:binary, 0, [], _state), do: binary()
  defp builtin(:bitstring, 0, [], _state), do: bitstring()
  defp builtin(:any, 0, [], _state), do: term()
  defp builtin(:term, 0, [], _state), do: term()
  defp builtin(:none, 0, [], _state), do: none()
  defp builtin(:no_return, 0, [], _state), do: none()
  defp builtin(:map, 0, [], _state), do: open_map()
  defp builtin(:tuple, 0, [], _state), do: tuple()

  defp builtin(:list, 0, [], _state), do: list(term())
  defp builtin(:list, 1, [elem], state), do: list(do_to_descr(elem, state))

  defp builtin(:non_empty_list, 0, [], _state), do: non_empty_list(term())
  defp builtin(:non_empty_list, 1, [elem], state), do: non_empty_list(do_to_descr(elem, state))

  # Local reference into the typespec table.
  defp builtin(name, arity, _args, state) do
    local_or_pending(name, arity, state)
  end

  ## Local + remote reference resolution

  defp local_or_pending(name, arity, state) do
    case Map.get(state.defined, {name, arity}) do
      nil ->
        # Unknown name — could be a remote built-in we haven't covered,
        # an as-yet-undefined helper, or a typevar. Degrade.
        dynamic()

      :pending ->
        throw({:to_descr, {:cycle, name, arity}})

      _ when arity > 0 ->
        throw({:to_descr, {:parametric_unsupported, name, arity}})

      {_kind, descr} ->
        descr
    end
  end

  defp expand_module({:__aliases__, _, parts}), do: Module.concat(parts)
  defp expand_module(mod) when is_atom(mod), do: mod
  defp expand_module(_), do: nil

  # Parametric types are unsupported. We degrade rather than throw
  # because the offending typespec lives in another module — we can't
  # surface a useful error at this site.
  defp resolve_remote(_module, _name, arity) when arity > 0, do: dynamic()

  defp resolve_remote(module, name, 0) do
    case fetch_remote_types(module) do
      %{{^name, 0} => {:type, descr}} -> descr
      # @opaque is dynamic from outside its defining module.
      %{{^name, 0} => {:opaque, _descr}} -> dynamic()
      _ -> dynamic()
    end
  end

  defp fetch_remote_types(module) do
    # Try the parallel checker ETS first (same-compilation-unit modules that
    # were compiled earlier in the same run).  Fall back to the in-memory beam
    # binary for cross-app references where the module is already loaded in the VM.
    case fetch_remote_types_from_checker(module) do
      {:ok, types} -> types
      :not_found -> fetch_remote_types_from_beam(module)
    end
  end

  # Look up types from the parallel checker ETS table.  Returns `{:ok, map}`
  # when type entries for the module are present in the checker table (written
  # by `cache_from_module_map` during a sibling module's `spawn_parallel_checker`
  # call), or `:not_found` if no type entries exist yet or the checker is unavailable.
  #
  # Note: the `{module, mode}` mode entry is written later (when :start is called),
  # but `{module, :type, name, arity}` entries are written eagerly by
  # `cache_types_descr_from_data_tables` inside `cache_from_module_map`.  We
  # therefore check for the presence of any type entry rather than the mode entry.
  defp fetch_remote_types_from_checker(module) do
    case :erlang.get(:elixir_checker_info) do
      {_parent, {_checker, table}} ->
        pairs = :ets.match(table, {{module, :type, :"$1", :"$2"}, :"$3"})

        case pairs do
          [] ->
            :not_found

          _ ->
            {:ok, Map.new(pairs, fn [name, arity, entry] -> {{name, arity}, entry} end)}
        end

      _ ->
        :not_found
    end
  end

  defp fetch_remote_types_from_beam(module) do
    with {:module, ^module} <- :code.ensure_loaded(module),
         {^module, binary, _path} <- :code.get_object_code(module),
         {:ok, {^module, [{~c"ExCk", chunk}]}} <- :beam_lib.chunks(binary, [~c"ExCk"]),
         {@elixir_checker_version, contents} <- :erlang.binary_to_term(chunk) do
      Map.get(contents, :types, %{})
    else
      _ -> %{}
    end
  end
end
