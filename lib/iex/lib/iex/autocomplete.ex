defmodule IEx.Autocomplete do
  @moduledoc false

  def expand(expr, server \\ IEx.Server)

  def expand('', _server) do
    expand_import("")
  end

  def expand([h | t]=expr, server) do
    cond do
      h === ?. and t != [] ->
        expand_dot(reduce(t), server)
      h === ?: and t == [] ->
        expand_erlang_modules()
      identifier?(h) ->
        expand_expr(reduce(expr), server)
      (h == ?/) and t != [] and identifier?(hd(t)) ->
        expand_expr(reduce(t), server)
      h in '([{' ->
        expand('')
      true ->
        no()
    end
  end

  defp identifier?(h) do
    (h in ?a..?z) or (h in ?A..?Z) or (h in ?0..?9) or h in [?_, ??, ?!]
  end

  defp expand_dot(expr, server) do
    case Code.string_to_quoted expr do
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
    case Code.string_to_quoted expr do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom))
      {:ok, {atom, _, nil}} when is_atom(atom) ->
        expand_import(Atom.to_string(atom))
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

  defp reduce(expr) do
    Enum.reduce(' ([{', expr, fn token, acc ->
      hd(:string.tokens(acc, [token]))
    end)
    |> Enum.reverse()
    |> trim_leading(?&)
    |> trim_leading(?%)
  end

  defp trim_leading([char | rest], char),
    do: rest
  defp trim_leading(expr, _char),
    do: expr

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
      ""   -> yes("", to_uniq_entries(uniq))
      hint -> yes(hint, [])
    end
  end

  defp format_expansion([first | _]=entries, hint) do
    binary = Enum.map(entries, &(&1.name))
    length = byte_size(hint)
    prefix = :binary.longest_common_prefix(binary)
    if prefix in [0, length] do
      yes("", Enum.flat_map(entries, &to_entries/1))
    else
      yes(:binary.part(first.name, prefix, length-prefix), [])
    end
  end

  ## Expand calls

  # :atom.fun
  defp expand_call(mod, hint, _server) when is_atom(mod) do
    expand_require(mod, hint)
  end

  # Elixir.fun
  defp expand_call({:__aliases__, _, list}, hint, server) do
    expand_alias(list, server)
    |> normalize_module
    |> expand_require(hint)
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
      [%{kind: :map_key, name: name, value_is_map: false}] when name == hint -> no()
      map_fields when is_list(map_fields) -> format_expansion(map_fields, hint)
    end
  end

  defp expand_require(mod, hint) do
    format_expansion match_module_funs(mod, hint), hint
  end

  defp expand_import(hint) do
    funs = match_module_funs(IEx.Helpers, hint) ++
           match_module_funs(Kernel, hint) ++
           match_module_funs(Kernel.SpecialForms, hint)
    format_expansion funs, hint
  end

  ## Erlang modules

  defp expand_erlang_modules(hint \\ "") do
    format_expansion match_erlang_modules(hint), hint
  end

  defp match_erlang_modules(hint) do
    for mod <- match_modules(hint, true) do
      %{kind: :module, name: mod, type: :erlang}
    end
  end

  ## Elixir modules

  defp expand_elixir_modules([], hint, server) do
    aliases = match_aliases(hint, server)
    expand_elixir_modules_from_aliases(Elixir, hint, aliases)
  end

  defp expand_elixir_modules(list, hint, server) do
    expand_alias(list, server)
    |> normalize_module
    |> expand_elixir_modules_from_aliases(hint, [])
  end

  defp expand_elixir_modules_from_aliases(mod, hint, aliases) do
    aliases
    |> Kernel.++(match_elixir_modules(mod, hint))
    |> Kernel.++(match_module_funs(mod, hint))
    |> format_expansion(hint)
  end

  defp expand_alias([name | rest] = list, server) do
    module = Module.concat(Elixir, name)
    Enum.find_value aliases_from_env(server), list, fn {alias, mod} ->
      if alias === module do
        case Atom.to_string(mod) do
          "Elixir." <> mod ->
            Module.concat [mod | rest]
          _ ->
            mod
        end
      end
    end
  end

  defp match_aliases(hint, server) do
    for {alias, _mod} <- aliases_from_env(server),
        [name] = Module.split(alias),
        starts_with?(name, hint) do
      %{kind: :module, type: :alias, name: name}
    end
  end

  defp match_elixir_modules(module, hint) do
    name  = Atom.to_string(module)
    depth = length(String.split(name, ".")) + 1
    base  = name <> "." <> hint

    for mod <- match_modules(base, module === Elixir),
        parts = String.split(mod, "."),
        depth <= length(parts) do
      %{kind: :module, type: :elixir, name: Enum.at(parts, depth-1)}
    end
    |> Enum.uniq
  end

  ## Helpers

  defp normalize_module(mod) do
    if is_list(mod) do
      Module.concat(mod)
    else
      mod
    end
  end

  defp match_modules(hint, root) do
    get_modules(root)
    |> :lists.usort()
    |> Enum.drop_while(& not starts_with?(&1, hint))
    |> Enum.take_while(& starts_with?(&1, hint))
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

  defp match_module_funs(mod, hint) do
    case ensure_loaded(mod) do
      {:module, _} ->
        falist = get_module_funs(mod)

        list = Enum.reduce falist, [], fn {f, a}, acc ->
          case :lists.keyfind(f, 1, acc) do
            {f, aa} -> :lists.keyreplace(f, 1, acc, {f, [a | aa]})
            false -> [{f, [a]} | acc]
          end
        end

        for {fun, arities} <- list,
            name = Atom.to_string(fun),
            starts_with?(name, hint) do
          %{kind: :function, name: name, arities: arities}
        end |> :lists.sort()

      _otherwise -> []
    end
  end

  defp match_map_fields(map, hint) do
    for {key, value} <- Map.to_list(map),
        is_atom(key),
        key = Atom.to_string(key),
        String.starts_with?(key, hint),
        do: %{kind: :map_key, name: key, value_is_map: is_map(value)}
  end

  defp get_module_funs(mod) do
    docs = Code.get_docs(mod, :docs) || []
    module_info_funs(mod) |> Enum.reject(&hidden_fun?(&1, docs))
  end

  defp module_info_funs(mod) do
    if function_exported?(mod, :__info__, 1) do
      mod.__info__(:macros) ++ (mod.__info__(:functions) -- [__info__: 1])
    else
      mod.module_info(:exports)
    end
  end

  defp hidden_fun?(fun, docs) do
    case Enum.find(docs, &match?({^fun, _, _, _, _}, &1)) do
      nil -> underscored_fun?(fun)
      doc -> not has_content?(doc)
    end
  end

  defp has_content?({_, _, _, _, false}),
    do: false
  defp has_content?({fun, _, _, _, nil}),
    do: not underscored_fun?(fun)
  defp has_content?({_, _, _, _, _}),
    do: true

  defp underscored_fun?({name, _}),
    do: hd(Atom.to_charlist(name)) == ?_

  defp ensure_loaded(Elixir), do: {:error, :nofile}
  defp ensure_loaded(mod),
    do: Code.ensure_compiled(mod)

  defp starts_with?(_string, ""),  do: true
  defp starts_with?(string, hint), do: String.starts_with?(string, hint)

  ## Ad-hoc conversions

  defp to_entries(%{kind: :module, name: name}) do
    [name]
  end

  defp to_entries(%{kind: :function, name: name, arities: arities}) do
    for a <- :lists.sort(arities), do: "#{name}/#{a}"
  end

  defp to_entries(%{kind: :map_key, name: name}) do
    [name]
  end

  defp to_uniq_entries(%{kind: :module}) do
    []
  end

  defp to_uniq_entries(%{kind: :function} = fun) do
    to_entries(fun)
  end

  defp to_uniq_entries(%{kind: :map_key}) do
    []
  end

  defp to_hint(%{kind: :module, name: name}, hint) when name == hint do
    format_hint(name, name) <> "."
  end

  defp to_hint(%{kind: :module, name: name}, hint) do
    format_hint(name, hint)
  end

  defp to_hint(%{kind: :function, name: name}, hint) do
    format_hint(name, hint)
  end

  defp to_hint(%{kind: :map_key, name: name, value_is_map: true}, hint) when name == hint do
    format_hint(name, hint) <> "."
  end

  defp to_hint(%{kind: :map_key, name: name}, hint) do
    format_hint(name, hint)
  end

  defp format_hint(name, hint) do
    hint_size = byte_size(hint)
    :binary.part(name, hint_size, byte_size(name) - hint_size)
  end

  defp aliases_from_env(server) do
    with evaluator when is_pid(evaluator) <- server.evaluator,
         {:ok, aliases} <- IEx.Evaluator.value_from_env(evaluator, :aliases) do
      aliases
    else
      _ -> []
    end
  end

  defp value_from_binding(ast_node, server) do
    with evaluator when is_pid(evaluator) <- server.evaluator(),
         {var, map_key_path} <- extract_from_ast(ast_node, []) do
      IEx.Evaluator.value_from_binding(evaluator, var, map_key_path)
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
