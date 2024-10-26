defmodule IEx.Autocomplete do
  @moduledoc false

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

  @alias_only_atoms ~w(alias import require)a
  @alias_only_charlists ~w(alias import require)c

  @doc """
  Provides one helper function that is injected into connecting
  remote nodes to properly handle autocompletion.
  """
  def remsh(node) do
    fn e ->
      try do
        :erpc.call(node, IEx.Autocomplete, :expand, [e, IEx.Broker.shell()])
      catch
        _, _ -> {:no, ~c"", []}
      end
    end
  end

  @doc """
  The expansion logic.

  Some of the expansion has to use the current shell
  environment, which is found via the broker.
  """
  def expand(code, shell \\ IEx.Broker.shell()) do
    case path_fragment(code) do
      [] -> expand_code(code, shell)
      path -> expand_path(path)
    end
  end

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
        if alias = alias_only(path, hint, code, shell) do
          expand_aliases(List.to_string(alias), shell)
        else
          expand_dot(path, List.to_string(hint), false, shell)
        end

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

      {:local_call, local} when local in @alias_only_charlists ->
        expand_aliases("", shell)

      {:local_call, local} ->
        expand_local_call(List.to_atom(local), shell)

      {:operator, operator} when operator in ~w(:: -)c ->
        expand_container_context(code, :operator, "", shell) ||
          expand_local(List.to_string(operator), false, shell)

      {:operator, operator} ->
        expand_local(List.to_string(operator), false, shell)

      {:operator_arity, operator} ->
        expand_local(List.to_string(operator), true, shell)

      {:operator_call, operator} when operator in ~w(|)c ->
        expand_container_context(code, :expr, "", shell) || expand_local_or_var("", shell)

      {:operator_call, _operator} ->
        expand_local_or_var("", shell)

      {:sigil, []} ->
        expand_sigil(shell)

      {:sigil, [_]} ->
        {:yes, [], ~w|" """ ' ''' \( / < [ { \||c}

      {:struct, struct} when is_list(struct) ->
        expand_structs(List.to_string(struct), shell)

      {:struct, {:dot, {:alias, struct}, ~c""}} when is_list(struct) ->
        expand_structs(List.to_string(struct ++ ~c"."), shell)

      # {:module_attribute, charlist}
      # :none
      _ ->
        no()
    end
  end

  defp get_helper(expr) do
    with [helper | rest] when helper in ~c"bt" <- expr,
         [space_or_paren, char | _] <- squeeze_spaces(rest),
         true <-
           space_or_paren in ~c" (" and
             (char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char in ~c"_:") do
      helper
    else
      _ -> nil
    end
  end

  defp squeeze_spaces(~c"  " ++ rest), do: squeeze_spaces([?\s | rest])
  defp squeeze_spaces(rest), do: rest

  @doc false
  def exports(mod) do
    if ensure_loaded?(mod) and function_exported?(mod, :__info__, 1) do
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
        |> match_module_funs(hint, false)
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
      _ -> []
    end
  end

  defp expand_signatures([_ | _] = signatures, _shell) do
    yes("", Enum.sort_by(signatures, &String.length/1))
  end

  defp expand_signatures([], shell), do: expand_local_or_var("", shell)

  ## Expand dot

  defp expand_dot(path, hint, exact?, shell) do
    case expand_dot_path(path, shell) do
      {:ok, mod} when is_atom(mod) and hint == "" -> expand_dot_aliases(mod)
      {:ok, mod} when is_atom(mod) -> expand_require(mod, hint, exact?)
      {:ok, map} when is_map(map) -> expand_map_field_access(map, hint)
      _ -> no()
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
      [%{kind: :map_key, name: ^hint, value_is_map: false}] -> no()
      map_fields when is_list(map_fields) -> format_expansion(map_fields, hint)
    end
  end

  defp expand_dot_aliases(mod) do
    all = match_elixir_modules(mod, "") ++ match_module_funs(get_module_funs(mod), "", false)
    format_expansion(all, "")
  end

  defp expand_require(mod, hint, exact?) do
    mod
    |> get_module_funs()
    |> match_module_funs(hint, exact?)
    |> format_expansion(hint)
  end

  ## Expand local or var

  defp expand_local_or_var(hint, shell) do
    format_expansion(match_var(hint, shell) ++ match_local(hint, false, shell), hint)
  end

  defp expand_local(hint, exact?, shell) do
    format_expansion(match_local(hint, exact?, shell), hint)
  end

  defp expand_sigil(shell) do
    sigils =
      match_local("sigil_", false, shell)
      |> Enum.map(fn %{name: "sigil_" <> rest} -> %{kind: :sigil, name: rest} end)

    format_expansion(match_local("~", false, shell) ++ sigils, "~")
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
    format_expansion(match_erlang_modules(hint), hint)
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
          do: {mod, name}

    modules =
      for "Elixir." <> name = full_name <- match_modules("Elixir." <> hint, true),
          String.starts_with?(name, hint),
          mod = String.to_atom(full_name),
          do: {mod, name}

    all = aliases ++ modules
    Code.ensure_all_loaded(Enum.map(all, &elem(&1, 0)))

    refs =
      for {mod, name} <- all,
          function_exported?(mod, :__struct__, 1) and not function_exported?(mod, :exception, 1),
          do: %{kind: :struct, name: name}

    format_expansion(refs, hint)
  end

  defp expand_container_context(code, context, hint, shell) do
    case container_context(code, shell) do
      {:map, map, pairs} when context == :expr ->
        container_context_map_fields(pairs, map, hint)

      {:struct, alias, pairs} when context == :expr ->
        map = Map.from_struct(alias.__struct__())
        container_context_map_fields(pairs, map, hint)

      :bitstring_modifier ->
        existing =
          code
          |> List.to_string()
          |> String.split("::")
          |> List.last()
          |> String.split("-")

        @bitstring_modifiers
        |> Enum.filter(&(String.starts_with?(&1.name, hint) and &1.name not in existing))
        |> format_expansion(hint)

      _ ->
        nil
    end
  end

  defp container_context_map_fields(pairs, map, hint) do
    pairs =
      Enum.reduce(pairs, map, fn {key, _}, map ->
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

    format_expansion(entries, hint)
  end

  defp container_context(code, shell) do
    case Code.Fragment.container_cursor_to_quoted(code) do
      {:ok, quoted} ->
        case Macro.path(quoted, &match?({:__cursor__, _, []}, &1)) do
          [cursor, {:%{}, _, pairs}, {:%, _, [{:__aliases__, _, aliases}, _map]} | _] ->
            container_context_struct(cursor, pairs, aliases, shell)

          [
            cursor,
            pairs,
            {:|, _, _},
            {:%{}, _, _},
            {:%, _, [{:__aliases__, _, aliases}, _map]} | _
          ] ->
            container_context_struct(cursor, pairs, aliases, shell)

          [cursor, pairs, {:|, _, [{variable, _, nil} | _]}, {:%{}, _, _} | _] ->
            container_context_map(cursor, pairs, variable, shell)

          [cursor, {special_form, _, [cursor]} | _] when special_form in @alias_only_atoms ->
            :alias_only

          [cursor | tail] ->
            case remove_operators(tail, cursor) do
              [{:"::", _, [_, _]}, {:<<>>, _, [_ | _]} | _] -> :bitstring_modifier
              _ -> nil
            end

          _ ->
            nil
        end

      {:error, _} ->
        nil
    end
  end

  defp remove_operators([{op, _, [_, previous]} = head | tail], previous) when op in [:-],
    do: remove_operators(tail, head)

  defp remove_operators(tail, _previous),
    do: tail

  defp container_context_struct(cursor, pairs, aliases, shell) do
    with {pairs, [^cursor]} <- Enum.split(pairs, -1),
         alias = value_from_alias(aliases, shell),
         true <-
           Keyword.keyword?(pairs) and ensure_loaded?(alias) and
             function_exported?(alias, :__struct__, 1) do
      {:struct, alias, pairs}
    else
      _ -> nil
    end
  end

  defp container_context_map(cursor, pairs, variable, shell) do
    with {pairs, [^cursor]} <- Enum.split(pairs, -1),
         {:ok, map} when is_map(map) <- value_from_binding([variable], shell),
         true <- Keyword.keyword?(pairs) do
      {:map, map, pairs}
    else
      _ -> nil
    end
  end

  ## Aliases and modules

  defp alias_only(path, hint, code, shell) do
    with {:alias, alias} <- path,
         [] <- hint,
         :alias_only <- container_context(code, shell) do
      alias ++ [?.]
    else
      _ -> nil
    end
  end

  defp expand_aliases(all, shell) do
    case String.split(all, ".") do
      [hint] ->
        all = match_aliases(hint, shell) ++ match_elixir_modules(Elixir, hint)
        format_expansion(all, hint)

      parts ->
        hint = List.last(parts)
        list = Enum.take(parts, length(parts) - 1)

        value_from_alias(list, shell)
        |> match_elixir_modules(hint)
        |> format_expansion(hint)
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

  ## Formatting

  defp format_expansion([], _) do
    no()
  end

  defp format_expansion([uniq], hint) do
    case to_hint(uniq, hint) do
      "" -> yes("", to_entries(uniq))
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
    {:no, ~c"", []}
  end

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
      nil -> match?([?_ | _], Atom.to_charlist(name))
      {_, _, _, :hidden, _} -> true
      {_, _, _, _, _} -> false
    end
  end

  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(mod), do: Code.ensure_loaded?(mod)

  ## Ad-hoc conversions

  defp to_entries(%{kind: :function, name: name, arity: arity}) do
    ["#{name}/#{arity}"]
  end

  defp to_entries(%{kind: :sigil, name: name}) do
    ["~#{name} (sigil_#{name})"]
  end

  defp to_entries(%{kind: :keyword, name: name}) do
    ["#{name}:"]
  end

  defp to_entries(%{kind: _, name: name}) do
    [name]
  end

  # Add extra character only if pressing tab when done
  defp to_hint(%{kind: :module, name: hint}, hint) do
    "."
  end

  defp to_hint(%{kind: :map_key, name: hint, value_is_map: true}, hint) do
    "."
  end

  defp to_hint(%{kind: :file, name: hint}, hint) do
    "\""
  end

  # Add extra character whenever possible
  defp to_hint(%{kind: :dir, name: name}, hint) do
    format_hint(name, hint) <> "/"
  end

  defp to_hint(%{kind: :struct, name: name}, hint) do
    format_hint(name, hint) <> "{"
  end

  defp to_hint(%{kind: :keyword, name: name}, hint) do
    format_hint(name, hint) <> ": "
  end

  defp to_hint(%{kind: _, name: name}, hint) do
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

  defp value_from_binding([var | path], shell) do
    with {evaluator, server} <- IEx.Broker.evaluator(shell) do
      IEx.Evaluator.value_from_binding(evaluator, server, var, path)
    else
      _ -> :error
    end
  end

  ## Path helpers

  defp path_fragment(expr), do: path_fragment(expr, [])
  defp path_fragment([], _acc), do: []
  defp path_fragment([?{, ?# | _rest], _acc), do: []
  defp path_fragment([?", ?\\ | t], acc), do: path_fragment(t, [?\\, ?" | acc])

  defp path_fragment([?/, ?:, x, ?" | _], acc) when x in ?a..?z or x in ?A..?Z,
    do: [x, ?:, ?/ | acc]

  defp path_fragment([?/, ?., ?" | _], acc), do: [?., ?/ | acc]
  defp path_fragment([?/, ?" | _], acc), do: [?/ | acc]
  defp path_fragment([?" | _], _acc), do: []
  defp path_fragment([h | t], acc), do: path_fragment(t, [h | acc])

  defp expand_path(path) do
    path
    |> List.to_string()
    |> ls_prefix()
    |> Enum.map(fn path ->
      %{
        kind: if(File.dir?(path), do: :dir, else: :file),
        name: Path.basename(path)
      }
    end)
    |> format_expansion(path_hint(path))
  end

  defp path_hint(path) do
    if List.last(path) in [?/, ?\\] do
      ""
    else
      Path.basename(path)
    end
  end

  defp prefix_from_dir(".", <<c, _::binary>>) when c != ?., do: ""
  defp prefix_from_dir(dir, _fragment), do: dir

  defp ls_prefix(path) do
    dir = Path.dirname(path)
    prefix = prefix_from_dir(dir, path)

    case File.ls(dir) do
      {:ok, list} ->
        list
        |> Enum.map(&Path.join(prefix, &1))
        |> Enum.filter(&String.starts_with?(&1, path))

      _ ->
        []
    end
  end
end
