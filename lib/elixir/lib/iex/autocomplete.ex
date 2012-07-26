defmodule IEx.Autocomplete do
  @moduledoc """
  Autocompletion for the Elixir shell.
  """

  defrecord Mod, type: nil, name: ''
  defrecord Fun, name: '', arities: []

  defprotocol Entry do
    @moduledoc false
    def to_entries(entry)
    def match?(entry, hint)
  end

  defimpl Entry, for: Mod do
    def to_entries(mod) do
      if mod.type === :erlang do
        [':'++mod.name]
      else
        [mod.name]
      end
    end

    def match?(mod, hint) do
      :lists.prefix(hint, mod.name)
    end
  end

  defimpl Entry, for: Fun do
    def to_entries(fun) do
      lc a inlist fun.arities, do: '#{fun.name}/#{a}'
    end

    def match?(fun, hint) do
      :lists.prefix(hint, fun.name)
    end
  end

  def expand([]) do
    format_expansion root_modules++iex_helpers_exports
  end

  def expand([h|t]=expr) do
    cond do
      h === ?. ->
        expand_dot reduce(t)
      h === ?: ->
        expand_erlang_modules
      (h in ?a..?z) or (h in ?A..?Z) or h === ?_ ->
        expand_expr reduce(expr)
      h in '(+[' ->
        expand ''
      true ->
        no_match
    end
  end

  defp expand_dot(expr) do
    case Code.string_to_ast expr do
      {:ok, atom} when is_atom(atom) ->
        expand_module_funs atom
      {:ok, {:__aliases__,_,list}} ->
        format_expansion elixir_module_subentries(list), ''
      _ -> no_match
    end
  end

  defp expand_expr(expr) do
    case Code.string_to_ast expr do
      {:ok, atom} when is_atom(atom) ->
          expand_erlang_modules atom_to_list(atom)
      {:ok, {:__aliases__,_,[root]}} ->
        hint = atom_to_list(root)
        entries = Enum.filter root_modules, fn m ->
          :lists.prefix(hint, m.name)
        end
        format_expansion entries, hint
      {:ok, {:__aliases__,_,list}} ->
        hint = atom_to_list(:lists.last(list))
        list = :lists.sublist(list, length(list)-1)
        format_expansion elixir_module_subentries(list, hint), hint
      {:ok, {{:., _, [mod,fun]},_,[]}} when is_atom(mod) and is_atom(fun) ->
        expand_module_funs mod, atom_to_list(fun)
      {:ok, {{:.,_,[{:__aliases__,_,list},fun]},_,[]}} when is_atom(fun) ->
        expand_module_funs(Module.concat(list), atom_to_list(fun))
      _ -> no_match
    end
  end

  defp reduce(expr) do
    last_token(List.reverse(expr), [' ', '(', '[', '+', '-'])
  end

  defp last_token(s, []) do
    s
  end

  defp last_token(s, [h|t]) do
    last_token(List.last(:string.tokens(s, h)), t)
  end

  defp no_match, do: { :no, '', [] }

  defp format_expansion(list, hint // '')
  defp format_expansion([], _) do
    no_match
  end
  defp format_expansion([Fun[name: name]], hint) do
    {:yes, :lists.nthtail(length(hint), name), []}
  end
  defp format_expansion([Mod[name: name]], hint) do
    {:yes, :lists.nthtail(length(hint), name)++'.', []}
  end
  defp format_expansion([first|_]=entries, hint) do
    b = Enum.map entries, fn e -> list_to_binary(e.name) end
    lh = length hint
    n = :binary.longest_common_prefix(b)
    cond do
      n == 0 or (n == lh) ->
        {:yes, '',
         Enum.reduce entries, [], fn e, acc -> acc++Entry.to_entries(e) end}
      true ->
        {:yes, :lists.sublist(first.name, 1+lh, n-lh), []}
    end
  end

  ## Root Modules

  defp loaded_modules do
    Enum.map :code.all_loaded, fn({m,_}) -> m end
  end

  defp root_modules do
    Enum.reduce :code.all_loaded, [], fn {m,_}, acc ->
      mod = atom_to_list(m)
      case mod do
        'Elixir' ++ _ ->
          tokens = :string.tokens(mod, '-')
          if length(tokens) === 2 do
            [Mod.new(name: List.last(tokens), type: :elixir)|acc]
          else
            acc
          end
        _ ->
          [Mod.new(name: mod, type: :erlang)|acc]
      end
    end
  end

  ## Root Functions (exported in IEx.Helpers)

  defp iex_helpers_exports do
    module_funs IEx.Helpers.__info__(:self)
  end

  ## Erlang modules

  defp expand_erlang_modules(hint // '') do
    format_expansion match_erlang_modules(hint), hint
  end

  defp match_erlang_modules('') do
    Enum.filter root_modules, fn m -> m.type === :erlang end
  end

  defp match_erlang_modules(hint) do
    Enum.filter root_modules, fn m -> Entry.match?(m, hint) end
  end

  ## Elixir modules

  defp elixir_module_subentries(list, hint // '') do
    mod = Module.concat(list)

    funs = case ensure_loaded(mod) do
      { :module, _ } ->
        Enum.filter module_funs(mod), fn Fun[name: name] ->
          :lists.prefix(hint, name)
        end
      _ ->
        []
    end

    elixir_module_submodules(mod, hint) ++ funs
  end

  defp elixir_module_submodules(mod, hint) do
    modname = atom_to_list(mod)
    depth   = length(:string.tokens(modname, '-')) + 1
    base    = modname ++ [?-|hint]

    Enum.reduce map_atom_to_list(loaded_modules), [], fn m, acc ->
      if m != base and :lists.prefix(base, m) do
        tokens = :string.tokens(m, '-')
        if length(tokens) == depth do
          name = List.last(tokens)
          [Mod.new(type: :elixir, name: name)|acc]
        else
          acc
        end
      else
        acc
      end
    end
  end

  ## Functions

  defp expand_module_funs(mod, hint // '')

  defp expand_module_funs(mod, '') do
    format_expansion module_funs(mod), ''
  end

  defp expand_module_funs(mod, hint) do
    entries = Enum.filter module_funs(mod), Entry.match?(&1, hint)
    format_expansion entries, hint
  end

  defp module_funs(mod, filter // [__info__: 1]) do
    case :code.is_loaded(mod) do
      {:file, _} ->
        falist =
          if function_exported?(mod, :__info__, 1) do
            mod.__info__(:functions) ++ mod.__info__(:macros)
          else
            mod.module_info(:exports)
          end

        falist = falist -- filter

        list = Enum.reduce falist, [], fn {f,a}, acc ->
          case :lists.keyfind(f, 1, acc) do
            {f,aa} -> :lists.keyreplace(f, 1, acc, {f, [a|aa]})
            false -> [{f, [a]}|acc]
          end
        end

        lc {f, aa} inlist list do
          Fun[name: atom_to_list(f), arities: aa]
        end
      _ -> []
    end
  end

  ## Generic Helpers

  defp map_atom_to_list(list) do
    Enum.map list, atom_to_list &1
  end

  defp ensure_loaded(Elixir), do: { :error, :nofile }
  defp ensure_loaded(other),  do: Code.ensure_loaded(other)
end
