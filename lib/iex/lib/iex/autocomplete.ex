defmodule IEx.Autocomplete do
  @moduledoc false

  def expand('') do
    expand_import("")
  end

  def expand([h|t]=expr) do
    cond do
      h === ?. and t != []->
        expand_dot(reduce(t))
      h === ?: ->
        expand_erlang_modules()
      identifier?(h) ->
        expand_expr(reduce(expr))
      (h == ?/) and t != [] and identifier?(hd(t)) ->
        expand_expr(reduce(t))
      h in '([{' ->
        expand('')
      true ->
        no()
    end
  end

  defp identifier?(h) do
    (h in ?a..?z) or (h in ?A..?Z) or (h in ?0..?9) or h in [?_, ??, ?!]
  end

  defp expand_dot(expr) do
    case Code.string_to_quoted expr do
      {:ok, atom} when is_atom(atom) ->
        expand_call(atom, "")
      {:ok, {:__aliases__, _, list}} ->
        expand_elixir_modules(list, "")
      _ ->
        no()
    end
  end

  defp expand_expr(expr) do
    case Code.string_to_quoted expr do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules(Atom.to_string(atom))
      {:ok, {atom, _, nil}} when is_atom(atom) ->
        expand_import(Atom.to_string(atom))
      {:ok, {:__aliases__, _, [root]}} ->
        expand_elixir_modules([], Atom.to_string(root))
      {:ok, {:__aliases__, _, [h|_] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules(list, hint)
      {:ok, {{:., _, [mod, fun]}, _, []}} when is_atom(fun) ->
        expand_call(mod, Atom.to_string(fun))
      _ ->
        no()
    end
  end

  defp reduce(expr) do
    Enum.reverse Enum.reduce [' ', '(', '[', '{'], expr, fn token, acc ->
      hd(:string.tokens(acc, token))
    end
  end

  defp yes(hint, entries) do
    {:yes, String.to_char_list(hint), Enum.map(entries, &String.to_char_list/1)}
  end

  defp no do
    {:no, '', []}
  end

  ## Formatting

  defp format_expansion([], _) do
    no()
  end

  defp format_expansion([uniq], hint) do
    hint = to_hint(uniq, hint)
    uniq = if hint == "", do: to_uniq_entries(uniq), else: []
    yes(hint, uniq)
  end

  defp format_expansion([first|_]=entries, hint) do
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
  defp expand_call(mod, hint) when is_atom(mod) do
    expand_require(mod, hint)
  end

  # Elixir.fun
  defp expand_call({:__aliases__, _, list}, hint) do
    expand_require(Module.concat(list), hint)
  end

  defp expand_call(_, _) do
    no()
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

  defp expand_elixir_modules(list, hint) do
    mod = Module.concat(list)
    format_expansion match_elixir_modules(mod, hint, list == []) ++
                     match_module_funs(mod, hint), hint
  end

  defp match_elixir_modules(module, hint, root) do
    module = Atom.to_string(module)
    depth  = length(String.split(module, ".")) + 1
    base   = module <> "." <> hint

    for mod <- match_modules(base, root),
        tokens = String.split(mod, "."),
        length(tokens) == depth do
      name = List.last(tokens)
      %{kind: :module, type: :elixir, name: name}
    end
  end

  ## Helpers

  defp match_modules(hint, root) do
    get_modules(root)
    |> :lists.usort()
    |> Enum.drop_while(& not String.starts_with?(&1, hint))
    |> Enum.take_while(& String.starts_with?(&1, hint))
  end

  defp get_modules(true) do
    ["Elixir.Elixir"] ++ get_modules(false)
  end

  defp get_modules(false) do
    modules = Enum.map(:code.all_loaded, fn({m, _}) -> Atom.to_string(m) end)

    if :code.get_mode() === :interactive do
      modules ++ get_modules_from_applications()
    else
      modules
    end
  end

  defp get_modules_from_applications do
    for {app, _, _} <- :application.loaded_applications,
        {_, modules} = :application.get_key(app, :modules),
             module <- modules do
      Atom.to_string(module)
    end
  end

  defp match_module_funs(mod, hint) do
    case ensure_loaded(mod) do
      {:module, _} ->
        falist = get_module_funs(mod)

        list = Enum.reduce falist, [], fn {f, a}, acc ->
          case :lists.keyfind(f, 1, acc) do
            {f, aa} -> :lists.keyreplace(f, 1, acc, {f, [a|aa]})
            false -> [{f, [a]}|acc]
          end
        end

        for {fun, arities} <- list,
            name = Atom.to_string(fun),
            String.starts_with?(name, hint) do
          %{kind: :function, name: name, arities: arities}
        end |> :lists.sort()
      _ ->
        []
    end
  end

  defp get_module_funs(mod) do
    if function_exported?(mod, :__info__, 1) do
      if docs = Code.get_docs(mod, :docs) do
        for {tuple, _line, _kind, _sign, doc} <- docs, doc != false, do: tuple
      else
        (mod.__info__(:functions) -- [__info__: 1]) ++ mod.__info__(:macros)
      end
    else
      mod.module_info(:exports)
    end
  end

  defp ensure_loaded(Elixir), do: {:error, :nofile}
  defp ensure_loaded(mod),    do: Code.ensure_compiled(mod)

  ## Ad-hoc conversions

  defp to_entries(%{kind: :module, name: name}) do
    [name]
  end

  defp to_entries(%{kind: :function, name: name, arities: arities}) do
    for a <- :lists.sort(arities), do: "#{name}/#{a}"
  end

  defp to_uniq_entries(%{kind: :module}) do
    []
  end

  defp to_uniq_entries(%{kind: :function} = fun) do
    to_entries(fun)
  end

  defp to_hint(%{kind: :module, name: name}, hint) do
    :binary.part(name, byte_size(hint), byte_size(name) - byte_size(hint)) <> "."
  end

  defp to_hint(%{kind: :function, name: name}, hint) do
    :binary.part(name, byte_size(hint), byte_size(name) - byte_size(hint))
  end
end
