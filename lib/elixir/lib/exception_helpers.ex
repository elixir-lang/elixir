defmodule ExceptionHelpers do
  @function_similarity_threshold 2
  @module_similarity_threshold   2

  def module_functions(module) do
    try do
      module.module_info(:exports)
    rescue UndefinedFunctionError ->
      []
    end
  end

  def find_functions(nil, _function, _arity), do: []

  def find_functions(module, function, _arity) do
    module_functions(module)
    |> Keyword.take([function])
    |> case do
         # No function with this name, check for a typo
         []   -> similar_functions(module, function)
         # Check for a function with the same name but different signature
         list -> list
       end

    # # Check for a function with the same name but different signature
    # matches = module_functions(module) |> Keyword.take([function])

    # if Enum.empty?(matches) do
    #   # No function with this name, check for a typo
    #   matches = similar_functions(module, function)
    # end

    # matches |> Enum.take(6)
  end

  def similar_functions(module, function) do
    functions = module_functions(module)
    matching_names = functions
                |> Keyword.keys
                |> Enum.map(&Atom.to_string/1)
                |> within_distance_from(@function_similarity_threshold, function)
                |> Enum.map(&String.to_atom/1)
    functions |> Keyword.take(matching_names)
  end

  def find_modules(module, _function, _arity) do
    IEx.Autocomplete.get_modules_from_applications
    |> within_distance_from(@module_similarity_threshold, module)
    |> Enum.map(fn
      "Elixir." <> module -> module
      module              -> ":" <> module
    end)
  end

  def get_call_signature(module, name, args) do
    cond do
      name in [:__aliases__, :__block__] ->
        "#{name}(args)"
      name in [:__ENV__, :__MODULE__, :__DIR__, :__CALLER__, :"%", :"%{}"] ->
        "#{name}"
      true ->
        call = String.to_atom("#{inspect module}.#{name}")
        Macro.to_string({ call, [], args })
    end
  end

  def function_signatures(module, function, arity) do
    case Code.get_docs(module, :docs) do
      nil -> []
      docs ->
        docs
        |> Enum.filter(fn {{name, arty}, _doc_line, _type, _signature, _doc} ->
          name == function and arty == arity
        end)
        |> Enum.map(fn {{name, _arity}, _doc_line, _type, signature, _doc} ->
          get_call_signature(module, name, signature)
        end)
    end
  end

  def within_distance_from(list, min_distance, atom) do
    name = Atom.to_string(atom)

    list
    |> Enum.map(fn(mod) ->
      { mod, edit_distance(name, mod) }
    end)
    |> Enum.filter(fn({ _func, dist }) ->
      dist <= min_distance
    end)
    |> Keyword.keys
  end

  def perhaps(msg, _, []), do: msg

  def perhaps(msg, module, list) do
    indent = "\n       "
    extra = list
            |> Enum.take(6)
            |> Enum.map(fn
              { fun, arity } -> Exception.format_mfa(module, fun, arity)
              module         -> module
            end)
            |> Enum.join(indent)

    [msg, "\n\n    ", IO.ANSI.bright, "Perhaps you meant one of:", IO.ANSI.reset, "\n", indent, extra, "\n"]
    |> Enum.join
  end

  # ================================================================
  # = Edit distance using levenshtein, taken from discussion here: =
  # = https://github.com/elixir-lang/elixir/issues/672             =
  # ================================================================
  defp store_result(key, result, cache) do
    {result, Dict.put(cache, key, result)}
  end

  defp edit_distance(string_1, string_2) when is_binary(string_1) and is_binary(string_2) do
    string_1 = String.to_char_list(string_1)
    string_2 = String.to_char_list(string_2)
    edit_distance(string_1, string_2)
  end

  defp edit_distance(string_1, string_2) do
    {list, _} = edit_distance(string_1, string_2, HashDict.new)
    list
  end

  defp edit_distance(string_1, []=string_2, cache) do
    store_result({string_1, string_2}, length(string_1), cache)
  end

  defp edit_distance([]=string_1, string_2, cache) do
    store_result({string_1, string_2}, length(string_2), cache)
  end

  defp edit_distance([x|rest1], [x|rest2], cache) do
    edit_distance(rest1, rest2, cache)
  end

  defp edit_distance([_|rest1]=string_1, [_|rest2]=string_2, cache) do
    case Dict.has_key?(cache, {string_1, string_2}) do
      true -> {Dict.get(cache, {string_1, string_2}), cache}
      false ->
        {l1, c1} = edit_distance(string_1, rest2, cache)
        {l2, c2} = edit_distance(rest1, string_2, c1)
        {l3, c3} = edit_distance(rest1, rest2, c2)

        min = :lists.min([l1, l2, l3]) + 1
        {min, Dict.put(c3, {string_1,string_2}, min)}
    end
  end
end
