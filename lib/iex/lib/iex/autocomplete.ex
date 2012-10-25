defmodule IEx.Autocomplete do
  @moduledoc """
  Autocompletion for the Elixir shell.
  """

  defrecord Mod, name: nil, type: nil
  defrecord Fun, name: nil, arities: []

  defprotocol Entry do
    @moduledoc false
    def to_entries(entry)
    def to_uniq_entries(entry)
    def to_hint(entry, hint)
  end

  defimpl Entry, for: Mod do
    @moduledoc false

    def to_entries(mod) do
      [mod.name]
    end

    def to_uniq_entries(_fun) do
      []
    end

    def to_hint(Mod[name: name], hint) do
      :lists.nthtail(length(hint), name) ++ '.'
    end
  end

  defimpl Entry, for: Fun do
    @moduledoc false

    def to_entries(fun) do
      lc a inlist fun.arities, do: '#{fun.name}/#{a}'
    end

    def to_uniq_entries(fun) do
      to_entries(fun)
    end

    def to_hint(Fun[name: name], hint) do
      :lists.nthtail(length(hint), name)
    end
  end

  def expand([]) do
    funs = module_funs(IEx.Helpers) ++ module_funs(Kernel)
    mods = [Mod[name: 'Elixir', type: :elixir]]
    format_expansion mods ++ funs
  end

  def expand([h|t]=expr) do
    cond do
      h === ?. ->
        expand_dot reduce(t)
      h === ?: ->
        expand_erlang_modules
      (h in ?a..?z) or (h in ?A..?Z) or h in [?_, ??, ?!] ->
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
        expand_call atom, ''
      {:ok, {:__aliases__,_,list}} ->
        expand_elixir_modules list
      _ ->
        no_match
    end
  end

  defp expand_expr(expr) do
    case Code.string_to_ast expr do
      {:ok, atom} when is_atom(atom) ->
        expand_erlang_modules atom_to_list(atom)
      {:ok, { atom, _, nil }} when is_atom(atom) ->
        expand_call Kernel, atom_to_list(atom)
      {:ok, {:__aliases__,_,[root]}} ->
        expand_elixir_modules [], atom_to_list(root)
      {:ok, {:__aliases__,_,[h|_] = list}} when is_atom(h) ->
        hint = atom_to_list(List.last(list))
        list = Enum.take(list, length(list) - 1)
        expand_elixir_modules list, hint
      {:ok, {{:., _, [mod,fun]},_,[]}} when is_atom(fun) ->
        expand_call mod, atom_to_list(fun)
      _ -> no_match
    end
  end

  defp reduce(expr) do
    last_token(Enum.reverse(expr), [' ', '(', '[', '+', '-'])
  end

  defp last_token(s, []) do
    s
  end

  defp last_token(s, [h|t]) do
    last_token(List.last(:string.tokens(s, h)), t)
  end

  defp no_match, do: { :no, '', [] }

  ## Formatting

  defp format_expansion(list, hint // '')

  defp format_expansion([], _) do
    no_match
  end

  defp format_expansion([uniq], hint) do
    hint = Entry.to_hint(uniq, hint)
    uniq = if hint == '', do: Entry.to_uniq_entries(uniq), else: []
    { :yes, hint, uniq }
  end

  defp format_expansion([first|_]=entries, hint) do
    binary = Enum.map entries, fn e -> list_to_binary(e.name) end
    length = length hint
    prefix = :binary.longest_common_prefix(binary)
    if prefix == 0 or (prefix == length) do
      {:yes, '',
         Enum.reduce entries, [], fn e, acc -> Entry.to_entries(e) ++ acc end }
    else
      {:yes, :lists.sublist(first.name, 1 + length, prefix-length), [] }
    end
  end

  ## Root Modules

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

  ## Expand calls

  # :atom.fun
  defp expand_call(mod, hint) when is_atom(mod) do
    expand_module_funs mod, hint
  end

  # Elixir.fun
  defp expand_call({ :__aliases__, _, list }, hint) do
    expand_module_funs Module.concat(list), hint
  end

  defp expand_call(_, _) do
    no_match
  end

  defp expand_module_funs(mod, hint) do
    format_expansion module_funs(mod, hint), hint
  end

  ## Erlang modules

  defp expand_erlang_modules(hint // '') do
    format_expansion match_erlang_modules(hint), hint
  end

  defp match_erlang_modules('') do
    Enum.filter root_modules, fn m -> m.type === :erlang end
  end

  defp match_erlang_modules(hint) do
    Enum.filter root_modules, fn m -> :lists.prefix(hint, m.name) end
  end

  ## Elixir modules

  defp expand_elixir_modules(list, hint // '') do
    mod = Module.concat(list)
    format_expansion elixir_submodules(mod, hint, list == []) ++ module_funs(mod, hint), hint
  end

  defp elixir_submodules(mod, hint, root) do
    modname = atom_to_list(mod)
    depth   = length(:string.tokens(modname, '-')) + 1
    base    = modname ++ [?-|hint]

    Enum.reduce modules_as_lists(root), [], fn(m, acc) ->
      if :lists.prefix(base, m) do
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

  defp modules_as_lists(true) do
    ['Elixir-Elixir'] ++ modules_as_lists(false)
  end

  defp modules_as_lists(false) do
    Enum.map(:code.all_loaded, fn({ m, _ }) -> atom_to_list(m) end)
  end

  ## Helpers

  defp module_funs(mod, hint // '') do
    case ensure_loaded(mod) do
      { :module, _ } ->
        falist = get_funs(mod)

        list = Enum.reduce falist, [], fn {f,a}, acc ->
          case :lists.keyfind(f, 1, acc) do
            {f,aa} -> :lists.keyreplace(f, 1, acc, {f, [a|aa]})
            false  -> [{f, [a]}|acc]
          end
        end

        lc {fun, arities} inlist list, name = atom_to_list(fun), is_prefix?(hint, name) do
          Fun[name: name, arities: arities]
        end
      _ ->
        []
    end
  end

  defp get_funs(mod) do
    if function_exported?(mod, :__info__, 1) do
      if docs = mod.__info__(:docs) do
        lc { tuple, _line, _kind, _sign, doc } inlist docs, doc != false, do: tuple
      else
        (mod.__info__(:functions) -- [__info__: 1]) ++ mod.__info__(:macros)
      end
    else
      mod.module_info(:exports)
    end
  end

  defp is_prefix?('', _),      do: true
  defp is_prefix?(hint, name), do: :lists.prefix(hint, name)

  defp ensure_loaded(Elixir), do: { :error, :nofile }
  defp ensure_loaded(mod),    do: Code.ensure_loaded(mod)
end
