defmodule Exception.Helpers do
  @function_similarity_threshold 0.77
  @module_similarity_threshold   0.8

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
  end

  def similar_functions(module, function) do
    functions = module_functions(module)

    functions
    |> Keyword.keys
    |> Enum.uniq
    |> Enum.map(&Atom.to_string/1)
    |> within_distance_from(@function_similarity_threshold, function)
    |> Enum.flat_map(fn name ->
      Keyword.take(functions, [String.to_atom(name)])
    end)
  end

  def find_modules(module, _function, _arity) do
    module = module |> Atom.to_string |> String.replace_leading("Elixir.", "")

    get_modules_from_applications
    |> within_distance_from(@module_similarity_threshold, module, &String.replace_leading(&1, "Elixir.", ""))
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

  @doc """
  Returns the strings in `list` which are at within `threshold` of `name`,
  calculated using `String.jaro_distance/2`.

  If `transform` is present it will be used to transform each item
  when passed to `jaro_distance`, but items will be returned unchanged.
  """
  def within_distance_from(list, threshold, name, transform \\ &(&1))

  def within_distance_from(list, threshold, name, transform) when is_atom(name) do
    name = Atom.to_string(name)
    within_distance_from(list, threshold, name, transform)
  end

  def within_distance_from(list, threshold, name, transform) do
    for str <- list,
        {str, distance} = { str, String.jaro_distance(name, transform.(str)) },
        distance >= threshold do
      {str, distance}
    end
    |> Enum.sort_by(fn { _func, dist } ->
      dist
    end, &>=/2)
    |> Keyword.keys
  end

  def perhaps(msg, _, []), do: msg

  def perhaps(msg, module, list) do
    indent = "\n       "

    extra =
      list
      |> Enum.take(6)
      |> Enum.map(fn
        { fun, arity } -> Exception.format_mfa(module, fun, arity)
        module         -> module
      end)
      |> Enum.join(indent)

    [
      msg,
      "\n\n    ",
      IO.ANSI.bright, "Perhaps you meant one of:", IO.ANSI.reset, "\n",
      indent,
      extra,
      "\n"
    ]
    |> Enum.join
  end

  # =========================
  # = From IEx.Autocomplete =
  # =========================
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
end
