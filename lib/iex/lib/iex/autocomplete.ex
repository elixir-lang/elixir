defmodule IEx.Autocomplete do
  @moduledoc false

  @doc """
  The expansion logic.

  Some of the expansion has to be use the current shell
  environemnt, which is found via the broker.
  """
  def expand(expr, server \\ IEx.Broker)

  def expand('', server) do
    expand_variable_or_import("", server)
  end

  def expand([h | t] = expr, server) do
    helper = get_helper(expr)

    cond do
      helper == ?t ->
        expand_custom(expr, server, &get_module_types/1)

      helper == ?b ->
        expand_custom(expr, server, &get_module_callbacks/1)

      h == ?. and t != [] ->
        expand_dot(reduce(t), server)

      h == ?: and t == [] ->
        expand_erlang_modules()

      identifier?(h) ->
        expand_expr(reduce(expr), server)

      h == ?/ and t != [] and identifier?(hd(t)) ->
        expand_expr(reduce(t), server)

      h in '([{' ->
        expand('')

      true ->
        no()
    end
  end

  defp get_helper(expr) do
    with [helper | rest] when helper in 'bt' <- Enum.reverse(expr),
         [space_or_paren, char | _] <- squeeze_spaces(rest),
         true <-
           space_or_paren in ' (' and
             (char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char in '_:') do
      helper
    else
      _ ->
        nil
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

  defp identifier?(h) do
    h in ?a..?z or h in ?A..?Z or h in ?0..?9 or h in [?_, ??, ?!]
  end

  defp expand_dot(expr, server) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        expand_call(atom, "", server)

      {:ok, {:__aliases__, _, list}} ->
        expand_elixir_modules(list, "", server)

      {:ok, {_, _, _} = ast_node} ->
        expand_call(ast_node, "", server)

      _ ->
        no()
    end
  end

  defp expand_expr(expr, server) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom))

      {:ok, {atom, _, nil}} when is_atom(atom) ->
        expand_variable_or_import(Atom.to_string(atom), server)

      {:ok, {:__aliases__, _, [root]}} ->
        expand_elixir_modules([], Atom.to_string(root), server)

      {:ok, {:__aliases__, _, [h | _] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules(list, hint, server)

      {:ok, {{:., _, [ast_node, fun]}, _, []}} when is_atom(fun) ->
        expand_call(ast_node, Atom.to_string(fun), server)

      _ ->
        no()
    end
  end

  defp expand_custom([?. | expr], server, fun) do
    case Code.string_to_quoted(reduce(expr)) do
      {:ok, atom} when is_atom(atom) ->
        no()

      {:ok, {:__aliases__, _, [h | _] = list}} when is_atom(h) ->
        case expand_alias(list, server) do
          {:ok, alias} ->
            expand_elixir_module_custom(alias, "", fun)

          :error ->
            no()
        end

      _ ->
        no()
    end
  end

  defp expand_custom(expr, server, fun) do
    case Code.string_to_quoted(reduce(expr)) do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom))

      {:ok, {:__aliases__, _, [root]}} ->
        expand_elixir_modules([], Atom.to_string(root), server)

      {:ok, {:__aliases__, _, [h | _] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules(list, hint, server)

      {:ok, {{:., _, [{:__aliases__, _, list}, type]}, _, []}} when is_atom(type) ->
        case expand_alias(list, server) do
          {:ok, alias} ->
            expand_elixir_module_custom(alias, Atom.to_string(type), fun)

          :error ->
            no()
        end

      _ ->
        no()
    end
  end

  defp reduce(expr) do
    Enum.reduce(' ([{', expr, fn token, acc ->
      hd(:string.tokens(acc, [token]))
    end)
    |> Enum.reverse()
    |> trim_leading(?&)
    |> trim_leading(?%)
  end

  defp trim_leading([char | rest], char), do: rest
  defp trim_leading(expr, _char), do: expr

  defp yes(hint, entries) do
    {:yes, String.to_charlist(hint), Enum.map(entries, &String.to_charlist/1)}
  end

  defp no do
    {:no, '', []}
  end

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

  ## Expand calls

  # :atom.fun
  defp expand_call(mod, hint, _server) when is_atom(mod) do
    expand_require(mod, hint)
  end

  # Elixir.fun
  defp expand_call({:__aliases__, _, list}, hint, server) do
    case expand_alias(list, server) do
      {:ok, alias} -> expand_require(alias, hint)
      :error -> no()
    end
  end

  # variable.fun_or_key
  defp expand_call({_, _, _} = ast_node, hint, server) do
    case value_from_binding(ast_node, server) do
      {:ok, mod} when is_atom(mod) -> expand_call(mod, hint, server)
      {:ok, map} when is_map(map) -> expand_map_field_access(map, hint)
      _otherwise -> no()
    end
  end

  defp expand_call(_, _, _) do
    no()
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

  defp expand_variable_or_import(hint, server) do
    variables = expand_variable(hint, server)
    imports = imports_from_env(server)
    module_funs = get_module_funs(Kernel.SpecialForms)
    funs = match_module_funs(imports ++ module_funs, hint)
    format_expansion(variables ++ funs, hint)
  end

  defp expand_variable(hint, server) do
    variables_from_binding(hint, server)
    |> Enum.sort()
    |> Enum.map(&%{kind: :variable, name: &1})
  end

  ## Erlang modules

  defp expand_erlang_modules(hint \\ "") do
    format_expansion(match_erlang_modules(hint), hint)
  end

  defp match_erlang_modules(hint) do
    for mod <- match_modules(hint, true), usable_as_unquoted_module?(mod) do
      %{kind: :module, name: mod, type: :erlang}
    end
  end

  ## Elixir modules

  defp expand_elixir_modules([], hint, server) do
    aliases = match_aliases(hint, server)
    expand_elixir_modules_from_aliases(Elixir, hint, aliases)
  end

  defp expand_elixir_modules(list, hint, server) do
    case expand_alias(list, server) do
      {:ok, alias} -> expand_elixir_modules_from_aliases(alias, hint, [])
      :error -> no()
    end
  end

  defp expand_elixir_modules_from_aliases(mod, hint, aliases) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint))
    |> Kernel.++(match_module_funs(get_module_funs(mod), hint))
    |> format_expansion(hint)
  end

  defp expand_alias([name | rest], server) when is_atom(name) do
    case Keyword.fetch(aliases_from_env(server), Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> {:ok, name}
      {:ok, name} -> {:ok, Module.concat([name | rest])}
      :error -> {:ok, Module.concat([name | rest])}
    end
  end

  defp expand_alias([_ | _], _) do
    :error
  end

  defp match_aliases(hint, server) do
    for {alias, _mod} <- aliases_from_env(server),
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

  ## Elixir Types

  defp expand_elixir_module_custom(mod, hint, fun) do
    types = match_module_funs(fun.(mod), hint)
    format_expansion(types, hint)
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
    cond do
      not ensure_loaded?(mod) ->
        []

      docs = get_docs(mod, [:type]) ->
        Enum.map(docs, &extract_name_and_arity/1)

      true ->
        exports(mod)
    end
  end

  defp get_module_callbacks(mod) do
    cond do
      not ensure_loaded?(mod) ->
        []

      docs = get_docs(mod, [:callback, :macrocallback]) ->
        Enum.map(docs, &extract_name_and_arity/1)

      true ->
        exports(mod)
    end
  end

  defp get_docs(mod, kinds) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, _, _, _, docs} ->
        for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc

      {:error, _} ->
        nil
    end
  end

  defp extract_name_and_arity({{_, name, arity}, _, _, _, _}), do: {name, arity}

  defp default_arg_functions_with_doc_false(docs) do
    for {{_, fun_name, arity}, _, _, :hidden, %{defaults: count}} <- docs,
        new_arity <- (arity - count)..arity,
        do: {fun_name, new_arity}
  end

  defp hidden_fun?({name, arity}, docs) do
    case Enum.find(docs, &match?({{_, ^name, ^arity}, _, _, _, _}, &1)) do
      nil -> underscored_fun?(name)
      {_, _, _, :hidden, _} -> true
      {_, _, _, :none, _} -> underscored_fun?(name)
      {_, _, _, _, _} -> false
    end
  end

  defp underscored_fun?(name), do: hd(Atom.to_charlist(name)) == ?_

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

  defp imports_from_env(server) do
    with {evaluator, server} <- server.evaluator(),
         env_fields = IEx.Evaluator.fields_from_env(evaluator, server, [:functions, :macros]),
         %{functions: funs, macros: macros} <- env_fields do
      Enum.flat_map(funs ++ macros, &elem(&1, 1))
    else
      _ -> []
    end
  end

  defp aliases_from_env(server) do
    with {evaluator, server} <- server.evaluator(),
         %{aliases: aliases} <- IEx.Evaluator.fields_from_env(evaluator, server, [:aliases]) do
      aliases
    else
      _ -> []
    end
  end

  defp variables_from_binding(hint, server) do
    with {evaluator, server} <- server.evaluator() do
      IEx.Evaluator.variables_from_binding(evaluator, server, hint)
    else
      _ -> []
    end
  end

  defp value_from_binding(ast_node, server) do
    with {evaluator, server} <- server.evaluator(),
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
