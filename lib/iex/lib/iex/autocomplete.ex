defmodule IEx.Autocomplete do
  @moduledoc false

  @doc """
  Provides one helper function that is injected into connecting
  remote nodes to properly handle autocompletion.
  """
  def remsh(node) do
    fn e ->
      case :rpc.call(node, IEx.Autocomplete, :expand, [e, IEx.Broker.shell()]) do
        {:badrpc, _} -> {:no, '', []}
        r -> r
      end
    end
  end

  @doc """
  The expansion logic.

  Some of the expansion has to be use the current shell
  environment, which is found via the broker.
  """
  def expand(code, shell \\ IEx.Broker.shell()) do
    code = Enum.reverse(code)
    helper = get_helper(code)

    case Code.cursor_context(code) do
      {:alias, alias} ->
        expand_aliases(List.to_string(alias), shell)

      {:unquoted_atom, unquoted_atom} ->
        expand_erlang_modules(List.to_string(unquoted_atom))

      expansion when helper == ?b ->
        expand_typespecs(expansion, shell, &get_module_callbacks/1)

      expansion when helper == ?t ->
        expand_typespecs(expansion, shell, &get_module_types/1)

      {:dot, path, hint} ->
        expand_dot(path, List.to_string(hint), shell)

      {:dot_arity, path, hint} ->
        expand_dot(path, List.to_string(hint), shell)

      {:dot_call, path, hint} ->
        expand_dot_call(path, List.to_atom(hint), shell)

      :expr ->
        expand_local_or_var("", shell)

      {:local_or_var, local_or_var} ->
        expand_local_or_var(List.to_string(local_or_var), shell)

      {:local_arity, local} ->
        expand_local(List.to_string(local), shell)

      {:local_call, local} ->
        expand_local_call(List.to_atom(local), shell)

      # {:module_attribute, charlist}
      # :none
      _ ->
        no()
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

  @doc false
  def exports(mod) do
    if Code.ensure_loaded?(mod) and function_exported?(mod, :__info__, 1) do
      mod.__info__(:macros) ++ (mod.__info__(:functions) -- [__info__: 1])
    else
      mod.module_info(:exports) -- [module_info: 0, module_info: 1]
    end
  end

  ## Typespecs

  defp expand_typespecs({:dot, path, hint}, shell, fun) do
    hint = List.to_string(hint)

    case expand_dot_path(path, shell) do
      {:ok, mod} when is_atom(mod) ->
        mod
        |> fun.()
        |> match_module_funs(hint)
        |> format_expansion(hint)

      _ ->
        no()
    end
  end

  defp expand_typespecs(_, _, _), do: no()

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
      _ -> no()
    end
  end

  defp get_signatures(name, module) when is_atom(module) do
    with docs when is_list(docs) <- get_docs(module, [:function, :macro], name) do
      Enum.map(docs, fn {_, _, signatures, _, _} -> Enum.join(signatures, " ") end)
    else
      _ -> :error
    end
  end

  defp expand_signatures([_ | _] = signatures, _shell) do
    [head | tail] = Enum.sort(signatures, &(String.length(&1) <= String.length(&2)))
    if tail != [], do: IO.write("\n" <> (tail |> Enum.reverse() |> Enum.join("\n")))
    yes("", [head])
  end

  defp expand_signatures([], shell), do: expand_local_or_var("", shell)

  ## Expand dot

  defp expand_dot(path, hint, shell) do
    case expand_dot_path(path, shell) do
      {:ok, mod} when is_atom(mod) and hint == "" -> expand_aliases(mod, "", [], true)
      {:ok, mod} when is_atom(mod) -> expand_require(mod, hint)
      {:ok, map} when is_map(map) -> expand_map_field_access(map, hint)
      _ -> no()
    end
  end

  defp expand_dot_path({:var, var}, shell) do
    value_from_binding({List.to_atom(var), [], nil}, shell)
  end

  defp expand_dot_path({:alias, var}, shell) do
    var |> List.to_string() |> String.split(".") |> value_from_alias(shell)
  end

  defp expand_dot_path({:unquoted_atom, var}, _shell) do
    {:ok, List.to_atom(var)}
  end

  defp expand_dot_path({:dot, parent, call}, shell) do
    case expand_dot_path(parent, shell) do
      {:ok, %{} = map} -> Map.fetch(map, List.to_atom(call))
      _ -> :error
    end
  end

  defp expand_map_field_access(map, hint) do
    case match_map_fields(map, hint) do
      [%{kind: :map_key, name: ^hint, value_is_map: false}] -> no()
      map_fields when is_list(map_fields) -> format_expansion(map_fields, hint)
    end
  end

  defp expand_require(mod, hint) do
    format_expansion(match_module_funs(get_module_funs(mod), hint), hint)
  end

  ## Expand local or var

  defp expand_local_or_var(hint, shell) do
    format_expansion(match_var(hint, shell) ++ match_local(hint, shell), hint)
  end

  defp expand_local(hint, shell) do
    format_expansion(match_local(hint, shell), hint)
  end

  defp match_local(hint, shell) do
    imports = imports_from_env(shell) |> Enum.flat_map(&elem(&1, 1))
    module_funs = get_module_funs(Kernel.SpecialForms)
    match_module_funs(imports ++ module_funs, hint)
  end

  defp match_var(hint, shell) do
    variables_from_binding(hint, shell)
    |> Enum.sort()
    |> Enum.map(&%{kind: :variable, name: &1})
  end

  ## Erlang modules

  defp expand_erlang_modules(hint) do
    format_expansion(match_erlang_modules(hint), hint)
  end

  defp match_erlang_modules(hint) do
    for mod <- match_modules(hint, true), usable_as_unquoted_module?(mod) do
      %{kind: :module, name: mod, type: :erlang}
    end
  end

  ## Elixir modules

  defp expand_aliases(all, shell) do
    case String.split(all, ".") do
      [hint] ->
        aliases = match_aliases(hint, shell)
        expand_aliases(Elixir, hint, aliases, false)

      parts ->
        hint = List.last(parts)
        list = Enum.take(parts, length(parts) - 1)

        case value_from_alias(list, shell) do
          {:ok, alias} -> expand_aliases(alias, hint, [], false)
          :error -> no()
        end
    end
  end

  defp expand_aliases(mod, hint, aliases, include_funs?) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint))
    |> Kernel.++(if include_funs?, do: match_module_funs(get_module_funs(mod), hint), else: [])
    |> format_expansion(hint)
  end

  defp value_from_alias([name | rest], shell) when is_binary(name) do
    name = String.to_atom(name)

    case Keyword.fetch(aliases_from_env(shell), Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> {:ok, name}
      {:ok, name} -> {:ok, Module.concat([name | rest])}
      :error -> {:ok, Module.concat([name | rest])}
    end
  end

  defp value_from_alias([_ | _], _) do
    :error
  end

  defp match_aliases(hint, shell) do
    for {alias, _mod} <- aliases_from_env(shell),
        [name] = Module.split(alias),
        String.starts_with?(name, hint) do
      %{kind: :module, type: :alias, name: name}
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
        do: %{kind: :module, type: :elixir, name: name}
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

  ## Formatting

  defp format_expansion([], _) do
    no()
  end

  defp format_expansion([uniq], hint) do
    case to_hint(uniq, hint) do
      "" -> yes("", to_uniq_entries(uniq))
      hint -> yes(hint, [])
    end
  end

  defp format_expansion([first | _] = entries, hint) do
    binary = Enum.map(entries, & &1.name)
    length = byte_size(hint)
    prefix = :binary.longest_common_prefix(binary)

    if prefix in [0, length] do
      yes("", Enum.flat_map(entries, &to_entries/1))
    else
      yes(binary_part(first.name, prefix, length - prefix), [])
    end
  end

  defp yes(hint, entries) do
    {:yes, String.to_charlist(hint), Enum.map(entries, &String.to_charlist/1)}
  end

  defp no do
    {:no, '', []}
  end

  ## Helpers

  defp usable_as_unquoted_module?(name) do
    # Conversion to atom is not a problem because
    # it is only called with existing modules names.
    Code.Identifier.classify(String.to_atom(name)) != :other
  end

  defp match_modules(hint, root) do
    get_modules(root)
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

  defp match_module_funs(funs, hint) do
    for {fun, arity} <- funs, name = Atom.to_string(fun), String.starts_with?(name, hint) do
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
        exports(mod)
        |> Kernel.--(default_arg_functions_with_doc_false(docs))
        |> Enum.reject(&hidden_fun?(&1, docs))

      true ->
        exports(mod)
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
      {_, _, _, %{}, _} -> false
      {_, _, _, _, _} -> true
    end
  end

  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(mod), do: Code.ensure_loaded?(mod)

  ## Ad-hoc conversions

  defp to_entries(%{kind: kind, name: name})
       when kind in [:map_key, :module, :variable] do
    [name]
  end

  defp to_entries(%{kind: :function, name: name, arity: arity}) do
    ["#{name}/#{arity}"]
  end

  defp to_uniq_entries(%{kind: kind})
       when kind in [:map_key, :module, :variable] do
    []
  end

  defp to_uniq_entries(%{kind: :function} = fun) do
    to_entries(fun)
  end

  defp to_hint(%{kind: :module, name: name}, hint) when name == hint do
    format_hint(name, name) <> "."
  end

  defp to_hint(%{kind: :map_key, name: name, value_is_map: true}, hint) when name == hint do
    format_hint(name, hint) <> "."
  end

  defp to_hint(%{kind: kind, name: name}, hint)
       when kind in [:function, :map_key, :module, :variable] do
    format_hint(name, hint)
  end

  defp format_hint(name, hint) do
    hint_size = byte_size(hint)
    binary_part(name, hint_size, byte_size(name) - hint_size)
  end

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

  defp value_from_binding(ast_node, shell) do
    with {evaluator, server} <- IEx.Broker.evaluator(shell),
         {var, map_key_path} <- extract_from_ast(ast_node, []) do
      IEx.Evaluator.value_from_binding(evaluator, server, var, map_key_path)
    else
      _ -> :error
    end
  end

  defp extract_from_ast(var_name, acc) when is_atom(var_name) do
    {var_name, acc}
  end

  defp extract_from_ast({var_name, _, nil}, acc) when is_atom(var_name) do
    {var_name, acc}
  end

  defp extract_from_ast({{:., _, [ast_node, fun]}, _, []}, acc) when is_atom(fun) do
    extract_from_ast(ast_node, [fun | acc])
  end

  defp extract_from_ast(_ast_node, _acc) do
    :error
  end
end
