defmodule IEx.Autocomplete.Code do
  @moduledoc """
  This module is used by IEx.Autocomplete to provide
  automatic code completion.
  """

  alias IEx.Autocomplete, as: Auto

  @behaviour IEx.Autocomplete.Behaviour

  @bitstring_modifiers [
    %{kind: :variable, name: "big"},
    %{kind: :variable, name: "binary"},
    %{kind: :variable, name: "bitstring"},
    %{kind: :variable, name: "integer"},
    %{kind: :variable, name: "float"},
    %{kind: :variable, name: "little"},
    %{kind: :variable, name: "native"},
    %{kind: :variable, name: "signed"},
    %{kind: :function, name: "size", arity: 1},
    %{kind: :function, name: "unit", arity: 1},
    %{kind: :variable, name: "unsigned"},
    %{kind: :variable, name: "utf8"},
    %{kind: :variable, name: "utf16"},
    %{kind: :variable, name: "utf32"}
  ]

  # Autocomplete.Code is the fallback autocompletion
  # mechanism to be used, hence any code fragment
  # is considered expandable by this module
  @impl true
  def expandable_fragment(_code), do: true

  @impl true
  def expand(code, shell), do: expand_code(code, shell)

  defp expand_code(code, shell) do
    code = Enum.reverse(code)
    helper = get_helper(code)

    case Code.Fragment.cursor_context(code) do
      {:alias, alias} ->
        expand_aliases(List.to_string(alias), shell)

      {:unquoted_atom, unquoted_atom} ->
        expand_erlang_modules(List.to_string(unquoted_atom))

      expansion when helper == ?b ->
        expand_typespecs(expansion, shell, &get_module_callbacks/1)

      expansion when helper == ?t ->
        expand_typespecs(expansion, shell, &get_module_types/1)

      {:dot, path, hint} ->
        expand_dot(path, List.to_string(hint), false, shell)

      {:dot_arity, path, hint} ->
        expand_dot(path, List.to_string(hint), true, shell)

      {:dot_call, path, hint} ->
        expand_dot_call(path, List.to_atom(hint), shell)

      :expr ->
        expand_container_context(code, :expr, "", shell) || expand_local_or_var("", shell)

      {:local_or_var, local_or_var} ->
        hint = List.to_string(local_or_var)
        expand_container_context(code, :expr, hint, shell) || expand_local_or_var(hint, shell)

      {:local_arity, local} ->
        expand_local(List.to_string(local), true, shell)

      {:local_call, local} ->
        expand_local_call(List.to_atom(local), shell)

      {:operator, operator} when operator in ~w(:: -)c ->
        expand_container_context(code, :operator, "", shell) ||
          expand_local(List.to_string(operator), false, shell)

      {:operator, operator} ->
        expand_local(List.to_string(operator), false, shell)

      {:operator_arity, operator} ->
        expand_local(List.to_string(operator), true, shell)

      {:operator_call, _operator} ->
        expand_local_or_var("", shell)

      {:sigil, []} ->
        expand_sigil(shell)

      {:sigil, [_]} ->
        {:yes, '', ~w|" """ ' ''' ( / < [ { \||c}

      {:struct, struct} when is_list(struct) ->
        expand_structs(List.to_string(struct), shell)

      {:struct, {:dot, {:alias, struct}, ''}} when is_list(struct) ->
        expand_structs(List.to_string(struct ++ '.'), shell)

      _ ->
        Auto.no()
    end
  end

  defp get_helper(expr) do
    with [helper | rest] when helper in 'bt' <- expr,
         [space_or_paren, char | _] <- squeeze_spaces(rest),
         true <-
           space_or_paren in ' (' and
             (char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char in '_:') do
      helper
    else
      _ -> nil
    end
  end

  defp squeeze_spaces('  ' ++ rest), do: squeeze_spaces([?\s | rest])
  defp squeeze_spaces(rest), do: rest

  ## Typespecs

  defp expand_typespecs({:dot, path, hint}, shell, fun) do
    hint = List.to_string(hint)

    case expand_dot_path(path, shell) do
      {:ok, mod} when is_atom(mod) ->
        mod
        |> fun.()
        |> match_module_funs(hint, false)
        |> Auto.format_expansion(hint)

      _ ->
        Auto.no()
    end
  end

  defp expand_typespecs(_, _, _), do: Auto.no()

  ## Expand call

  defp expand_local_call(fun, shell) do
    imports_from_env(shell)
    |> Enum.filter(fn {_, funs} -> List.keymember?(funs, fun, 0) end)
    |> Enum.flat_map(fn {module, _} -> get_signatures(fun, module) end)
    |> expand_signatures(shell)
  end

  defp expand_dot_call(path, fun, shell) do
    case expand_dot_path(path, shell) do
      {:ok, mod} when is_atom(mod) -> get_signatures(fun, mod) |> expand_signatures(shell)
      _ -> Auto.no()
    end
  end

  defp get_signatures(name, module) when is_atom(module) do
    with docs when is_list(docs) <- get_docs(module, [:function, :macro], name) do
      Enum.map(docs, fn {_, _, signatures, _, _} -> Enum.join(signatures, " ") end)
    else
      _ -> []
    end
  end

  defp expand_signatures([_ | _] = signatures, _shell) do
    [head | tail] = Enum.sort(signatures, &(String.length(&1) <= String.length(&2)))
    if tail != [], do: IO.write("\n" <> (tail |> Enum.reverse() |> Enum.join("\n")))
    Auto.yes("", [head])
  end

  defp expand_signatures([], shell), do: expand_local_or_var("", shell)

  ## Expand dot

  defp expand_dot(path, hint, exact?, shell) do
    case expand_dot_path(path, shell) do
      {:ok, mod} when is_atom(mod) and hint == "" -> expand_dot_aliases(mod)
      {:ok, mod} when is_atom(mod) -> expand_require(mod, hint, exact?)
      {:ok, map} when is_map(map) -> expand_map_field_access(map, hint)
      _ -> Auto.no()
    end
  end

  defp expand_dot_path({:unquoted_atom, var}, _shell) do
    {:ok, List.to_atom(var)}
  end

  defp expand_dot_path(path, shell) do
    case recur_expand_dot_path(path, shell) do
      {:ok, [_ | _] = path} -> value_from_binding(Enum.reverse(path), shell)
      other -> other
    end
  end

  defp recur_expand_dot_path({:var, var}, _shell) do
    {:ok, [List.to_atom(var)]}
  end

  defp recur_expand_dot_path({:alias, var}, shell) do
    {:ok, var |> List.to_string() |> String.split(".") |> value_from_alias(shell)}
  end

  defp recur_expand_dot_path({:dot, parent, call}, shell) do
    case recur_expand_dot_path(parent, shell) do
      {:ok, [_ | _] = path} -> {:ok, [List.to_atom(call) | path]}
      _ -> :error
    end
  end

  defp recur_expand_dot_path(_, _shell) do
    :error
  end

  defp expand_map_field_access(map, hint) do
    case match_map_fields(map, hint) do
      [%{kind: :map_key, name: ^hint, value_is_map: false}] -> Auto.no()
      map_fields when is_list(map_fields) -> Auto.format_expansion(map_fields, hint)
    end
  end

  defp expand_dot_aliases(mod) do
    all = match_elixir_modules(mod, "") ++ match_module_funs(get_module_funs(mod), "", false)
    Auto.format_expansion(all, "")
  end

  defp expand_require(mod, hint, exact?) do
    mod
    |> get_module_funs()
    |> match_module_funs(hint, exact?)
    |> Auto.format_expansion(hint)
  end

  ## Expand local or var

  defp expand_local_or_var(hint, shell) do
    Auto.format_expansion(match_var(hint, shell) ++ match_local(hint, false, shell), hint)
  end

  defp expand_local(hint, exact?, shell) do
    Auto.format_expansion(match_local(hint, exact?, shell), hint)
  end

  defp expand_sigil(shell) do
    sigils =
      match_local("sigil_", false, shell)
      |> Enum.map(fn %{name: "sigil_" <> rest} -> %{kind: :sigil, name: rest} end)

    Auto.format_expansion(match_local("~", false, shell) ++ sigils, "~")
  end

  defp match_local(hint, exact?, shell) do
    imports = imports_from_env(shell) |> Enum.flat_map(&elem(&1, 1))
    module_funs = get_module_funs(Kernel.SpecialForms)
    match_module_funs(imports ++ module_funs, hint, exact?)
  end

  defp match_var(hint, shell) do
    variables_from_binding(hint, shell)
    |> Enum.sort()
    |> Enum.map(&%{kind: :variable, name: &1})
  end

  ## Erlang modules

  defp expand_erlang_modules(hint) do
    Auto.format_expansion(match_erlang_modules(hint), hint)
  end

  defp match_erlang_modules(hint) do
    for mod <- match_modules(hint, false), usable_as_unquoted_module?(mod) do
      %{kind: :module, name: mod}
    end
  end

  ## Structs

  defp expand_structs(hint, shell) do
    aliases =
      for {alias, mod} <- aliases_from_env(shell),
          [name] = Module.split(alias),
          String.starts_with?(name, hint),
          struct?(mod) and not function_exported?(mod, :exception, 1),
          do: %{kind: :struct, name: name}

    modules =
      for "Elixir." <> name = full_name <- match_modules("Elixir." <> hint, true),
          String.starts_with?(name, hint),
          mod = String.to_atom(full_name),
          struct?(mod) and not function_exported?(mod, :exception, 1),
          do: %{kind: :struct, name: name}

    Auto.format_expansion(aliases ++ modules, hint)
  end

  defp struct?(mod) do
    Code.ensure_loaded?(mod) and function_exported?(mod, :__struct__, 1)
  end

  defp expand_container_context(code, context, hint, shell) do
    case container_context(code, shell) do
      {:struct, alias, pairs} when context == :expr ->
        pairs =
          Enum.reduce(pairs, Map.from_struct(alias.__struct__), fn {key, _}, map ->
            Map.delete(map, key)
          end)

        entries =
          for {key, _value} <- pairs,
              name = Atom.to_string(key),
              if(hint == "",
                do: not String.starts_with?(name, "_"),
                else: String.starts_with?(name, hint)
              ),
              do: %{kind: :keyword, name: name}

        Auto.format_expansion(entries, hint)

      :bitstring_modifier ->
        existing =
          code
          |> List.to_string()
          |> String.split("::")
          |> List.last()
          |> String.split("-")

        @bitstring_modifiers
        |> Enum.filter(&(String.starts_with?(&1.name, hint) and &1.name not in existing))
        |> Auto.format_expansion(hint)

      _ ->
        nil
    end
  end

  defp container_context(code, shell) do
    case Code.Fragment.container_cursor_to_quoted(code) do
      {:ok, quoted} ->
        case Macro.path(quoted, &match?({:__cursor__, _, []}, &1)) do
          [cursor, {:%{}, _, pairs}, {:%, _, [{:__aliases__, _, aliases}, _map]} | _] ->
            with {pairs, [^cursor]} <- Enum.split(pairs, -1),
                 alias = value_from_alias(aliases, shell),
                 true <- Keyword.keyword?(pairs) and struct?(alias) do
              {:struct, alias, pairs}
            else
              _ -> nil
            end

          [cursor, {:"::", _, [_, cursor]}, {:<<>>, _, [_ | _]} | _] ->
            :bitstring_modifier

          _ ->
            nil
        end

      {:error, _} ->
        nil
    end
  end

  ## Aliases and modules

  defp expand_aliases(all, shell) do
    case String.split(all, ".") do
      [hint] ->
        all = match_aliases(hint, shell) ++ match_elixir_modules(Elixir, hint)
        Auto.format_expansion(all, hint)

      parts ->
        hint = List.last(parts)
        list = Enum.take(parts, length(parts) - 1)

        value_from_alias(list, shell)
        |> match_elixir_modules(hint)
        |> Auto.format_expansion(hint)
    end
  end

  defp value_from_alias([name | rest], shell) do
    case Keyword.fetch(aliases_from_env(shell), Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> name
      {:ok, name} -> Module.concat([name | rest])
      :error -> Module.concat([name | rest])
    end
  end

  defp match_aliases(hint, shell) do
    for {alias, module} <- aliases_from_env(shell),
        [name] = Module.split(alias),
        String.starts_with?(name, hint) do
      %{kind: :module, name: name, module: module}
    end
  end

  defp match_elixir_modules(module, hint) do
    name = Atom.to_string(module)
    depth = length(String.split(name, ".")) + 1
    base = name <> "." <> hint

    for mod <- match_modules(base, module == Elixir),
        parts = String.split(mod, "."),
        depth <= length(parts),
        name = Enum.at(parts, depth - 1),
        valid_alias_piece?("." <> name),
        uniq: true,
        do: %{kind: :module, name: name}
  end

  defp valid_alias_piece?(<<?., char, rest::binary>>) when char in ?A..?Z,
    do: valid_alias_rest?(rest)

  defp valid_alias_piece?(_), do: false

  defp valid_alias_rest?(<<char, rest::binary>>)
       when char in ?A..?Z
       when char in ?a..?z
       when char in ?0..?9
       when char == ?_,
       do: valid_alias_rest?(rest)

  defp valid_alias_rest?(<<>>), do: true
  defp valid_alias_rest?(rest), do: valid_alias_piece?(rest)

  ## Helpers

  defp usable_as_unquoted_module?(name) do
    # Conversion to atom is not a problem because
    # it is only called with existing modules names.
    Macro.classify_atom(String.to_atom(name)) in [:identifier, :unquoted]
  end

  defp match_modules(hint, elixir_root?) do
    get_modules(elixir_root?)
    |> Enum.sort()
    |> Enum.dedup()
    |> Enum.drop_while(&(not String.starts_with?(&1, hint)))
    |> Enum.take_while(&String.starts_with?(&1, hint))
  end

  defp get_modules(true) do
    ["Elixir.Elixir"] ++ get_modules(false)
  end

  defp get_modules(false) do
    modules = Enum.map(:code.all_loaded(), &Atom.to_string(elem(&1, 0)))

    case :code.get_mode() do
      :interactive -> modules ++ get_modules_from_applications()
      _otherwise -> modules
    end
  end

  defp get_modules_from_applications do
    for [app] <- loaded_applications(),
        {:ok, modules} = :application.get_key(app, :modules),
        module <- modules do
      Atom.to_string(module)
    end
  end

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end

  defp match_module_funs(funs, hint, exact?) do
    for {fun, arity} <- funs,
        name = Atom.to_string(fun),
        if(exact?, do: name == hint, else: String.starts_with?(name, hint)) do
      %{
        kind: :function,
        name: name,
        arity: arity
      }
    end
    |> Enum.sort_by(&{&1.name, &1.arity})
  end

  defp match_map_fields(map, hint) do
    for {key, value} when is_atom(key) <- Map.to_list(map),
        key = Atom.to_string(key),
        String.starts_with?(key, hint) do
      %{kind: :map_key, name: key, value_is_map: is_map(value)}
    end
    |> Enum.sort_by(& &1.name)
  end

  defp get_module_funs(mod) do
    cond do
      not ensure_loaded?(mod) ->
        []

      docs = get_docs(mod, [:function, :macro]) ->
        Auto.exports(mod)
        |> Kernel.--(default_arg_functions_with_doc_false(docs))
        |> Enum.reject(&hidden_fun?(&1, docs))

      true ->
        Auto.exports(mod)
    end
  end

  defp get_module_types(mod) do
    if ensure_loaded?(mod) do
      case Code.Typespec.fetch_types(mod) do
        {:ok, types} ->
          for {kind, {name, _, args}} <- types,
              kind in [:type, :opaque] do
            {name, length(args)}
          end

        :error ->
          []
      end
    else
      []
    end
  end

  defp get_module_callbacks(mod) do
    if ensure_loaded?(mod) do
      case Code.Typespec.fetch_callbacks(mod) do
        {:ok, callbacks} ->
          for {name_arity, _} <- callbacks do
            {_kind, name, arity} = IEx.Introspection.translate_callback_name_arity(name_arity)

            {name, arity}
          end

        :error ->
          []
      end
    else
      []
    end
  end

  defp get_docs(mod, kinds, fun \\ nil) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, _, _, _, docs} ->
        if is_nil(fun) do
          for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc
        else
          for {{kind, ^fun, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc
        end

      {:error, _} ->
        nil
    end
  end

  defp default_arg_functions_with_doc_false(docs) do
    for {{_, fun_name, arity}, _, _, :hidden, %{defaults: count}} <- docs,
        new_arity <- (arity - count)..arity,
        do: {fun_name, new_arity}
  end

  defp hidden_fun?({name, arity}, docs) do
    case Enum.find(docs, &match?({{_, ^name, ^arity}, _, _, _, _}, &1)) do
      nil -> hd(Atom.to_charlist(name)) == ?_
      {_, _, _, :hidden, _} -> true
      {_, _, _, _, _} -> false
    end
  end

  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(mod), do: Code.ensure_loaded?(mod)

  ## Evaluator interface

  defp imports_from_env(shell) do
    with {evaluator, server} <- IEx.Broker.evaluator(shell),
         env_fields = IEx.Evaluator.fields_from_env(evaluator, server, [:functions, :macros]),
         %{functions: funs, macros: macros} <- env_fields do
      funs ++ macros
    else
      _ -> []
    end
  end

  defp aliases_from_env(shell) do
    with {evaluator, server} <- IEx.Broker.evaluator(shell),
         %{aliases: aliases} <- IEx.Evaluator.fields_from_env(evaluator, server, [:aliases]) do
      aliases
    else
      _ -> []
    end
  end

  defp variables_from_binding(hint, shell) do
    with {evaluator, server} <- IEx.Broker.evaluator(shell) do
      IEx.Evaluator.variables_from_binding(evaluator, server, hint)
    else
      _ -> []
    end
  end

  defp value_from_binding([var | path], shell) do
    with {evaluator, server} <- IEx.Broker.evaluator(shell) do
      IEx.Evaluator.value_from_binding(evaluator, server, var, path)
    else
      _ -> :error
    end
  end
end
