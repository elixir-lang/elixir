defmodule ExceptionHelpers do
  @function_similarity_threshold 0.8
  @module_similarity_threshold   0.9

  def module_functions(module) do
    try do
      module.module_info(:exports)
    rescue UndefinedFunctionError ->
      []
    end
  end

  def find_functions(nil, _function, _arity), do: []

  def find_functions(module, function, _arity) do
    # Check for a function with the same name but different signature
    matches = module_functions(module) |> Keyword.take([function])

    if Enum.empty?(matches) do
      # No function with this name, check for a typo
      matches = similar_functions(module, function)
    end

    matches |> Enum.take(6)
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
      module              -> module
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
      { mod, String.jaro_distance(name, (mod)) }
    end)
    |> Enum.filter(fn({ _func, dist }) ->
      dist >= min_distance
    end)
    |> Keyword.keys
  end

  def perhaps(msg, _, []), do: msg

  def perhaps(msg, module, list) do
    indent = "\n       "
    extra = list
            |> Enum.map(fn
              { fun, arity } -> Exception.format_mfa(module, fun, arity)
              module         -> module
            end)
            |> Enum.join(indent)

    [msg, "\n\n    ", IO.ANSI.bright, "Perhaps you meant one of:", IO.ANSI.reset, "\n", indent, extra, "\n"]
    |> Enum.join
  end
end
