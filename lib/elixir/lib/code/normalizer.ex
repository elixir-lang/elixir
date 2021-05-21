defmodule Code.Normalizer do
  @moduledoc false

  defguard is_literal(x)
           when is_integer(x) or
                  is_float(x) or
                  is_binary(x) or
                  is_atom(x)

  @doc """
  Wraps literals in the quoted expression to conform to the AST format expected
  by the formatter.
  """
  def normalize(quoted, opts \\ []) do
    line = Keyword.get(opts, :line, 1)

    do_normalize(quoted, line: line)
  end

  # Skip normalized literals
  defp do_normalize({:__block__, _, [literal]} = quoted, _parent_meta) when is_literal(literal) do
    quoted
  end

  # Skip normalized charlists
  defp do_normalize({:__block__, meta, [charlist]} = quoted, parent_meta)
       when is_list(charlist) do
    if Keyword.has_key?(meta, :delimiter) do
      quoted
    else
      do_normalize(charlist, parent_meta)
    end
  end

  # Only normalize the first argument of an alias if it's not an atom
  defp do_normalize({:__aliases, meta, [first | rest]}, parent_meta) when not is_atom(first) do
    meta = patch_meta_line(meta, parent_meta)
    first = do_normalize(first, meta)
    {:__aliases__, meta, [first | rest]}
  end

  defp do_normalize({:__aliases__, _, _} = quoted, _parent_meta) do
    quoted
  end

  # Skip captured arguments like &1
  defp do_normalize({:&, meta, [term]}, parent_meta) when is_integer(term) do
    meta = patch_meta_line(meta, parent_meta)
    {:&, meta, [term]}
  end

  # Ranges
  defp do_normalize(left..right//step, parent_meta) do
    left = do_normalize(left, parent_meta)
    right = do_normalize(right, parent_meta)

    meta = [line: parent_meta[:line]]

    if step == 1 do
      {:.., meta, [left, right]}
    else
      step = do_normalize(step, parent_meta)
      {:"..//", meta, [left, right, step]}
    end
  end

  # Bit containers
  defp do_normalize({:<<>>, meta, parts} = quoted, parent_meta) do
    meta = patch_meta_line(meta, parent_meta)

    parts =
      if interpolated?(quoted) do
        normalize_interpolation_parts(parts, meta)
      else
        Enum.map(parts, &do_normalize(&1, meta))
      end

    {:<<>>, meta, parts}
  end

  # Skip atoms with interpolations
  defp do_normalize(
         {{:., _, [:erlang, :binary_to_atom]}, _, [{:<<>>, _, _}, :utf8]} = quoted,
         _parent_meta
       ) do
    quoted
  end

  # Skip charlists with interpolations
  defp do_normalize({{:., _, [List, :to_charlist]}, _, _} = quoted, _parent_meta) do
    quoted
  end

  # Don't normalize the `Access` atom in access syntax
  defp do_normalize({:., meta, [Access, :get]}, parent_meta) do
    meta = patch_meta_line(meta, parent_meta)
    {:., meta, [Access, :get]}
  end

  # Only normalize the left side of the dot operator
  # The right hand side is an atom in the AST but it's not an atom literal, so
  # it should not be wrapped
  defp do_normalize({:., meta, [left, right]}, parent_meta) do
    meta = patch_meta_line(meta, parent_meta)

    left = do_normalize(left, meta)

    {:., meta, [left, right]}
  end

  # A list of left to right arrows is not considered as a list literal, so it's
  # not wrapped
  defp do_normalize([{:->, _, _} | _] = quoted, parent_meta) do
    Enum.map(quoted, &do_normalize(&1, parent_meta))
  end

  # left -> right
  defp do_normalize({:->, meta, [left, right]}, parent_meta) do
    meta = patch_meta_line(meta, parent_meta)

    left = Enum.map(left, &do_normalize(&1, meta))
    right = do_normalize(right, meta)
    {:->, meta, [left, right]}
  end

  # Maps
  defp do_normalize({:%{}, meta, args}, parent_meta) do
    meta =
      if meta == [] do
        line = parent_meta[:line]
        [line: line, closing: [line: line]]
      else
        meta
      end

    args =
      case args do
        [{:|, pipe_meta, [left, right]}] ->
          left = do_normalize(left, meta)
          {_, _, right} = do_normalize(right, meta)
          [{:|, pipe_meta, [left, right]}]

        [{_, _, _} = call] ->
          [do_normalize(call, meta)]

        args ->
          normalize_map_args(args, meta)
      end

    {:%{}, meta, args}
  end

  # Sigils
  defp do_normalize({sigil, meta, [{:<<>>, _, _} = string, modifiers]} = quoted, parent_meta)
       when is_atom(sigil) do
    case Atom.to_string(sigil) do
      <<"sigil_", _name>> ->
        {sigil, meta, [do_normalize(string, meta), modifiers]}

      _ ->
        normalize_call(quoted, parent_meta)
    end
  end

  # Calls
  defp do_normalize({_, _, args} = quoted, parent_meta) when is_list(args) do
    normalize_call(quoted, parent_meta)
  end

  # Integers, floats, atoms
  defp do_normalize(x, parent_meta) when is_literal(x) do
    meta = [line: parent_meta[:line]]

    meta =
      if not is_atom(x) do
        Keyword.put(meta, :token, Macro.to_string(x))
      else
        meta
      end

    meta =
      if not is_nil(parent_meta[:format]) do
        Keyword.put(meta, :format, parent_meta[:format])
      else
        meta
      end

    if module_atom?(x) do
      "Elixir." <> segments = Atom.to_string(x)

      segments =
        segments
        |> String.split(".")
        |> Enum.map(&String.to_atom/1)

      {:__aliases__, meta, segments}
    else
      {:__block__, meta, [x]}
    end
  end

  # 2-tuples
  defp do_normalize({left, right}, parent_meta) do
    meta = [line: parent_meta[:line]]

    left_parent_meta =
      if is_atom(left) and not module_atom?(left) do
        Keyword.put(parent_meta, :format, :keyword)
      else
        meta
      end

    {:__block__, meta,
     [
       {do_normalize(left, left_parent_meta), do_normalize(right, parent_meta)}
     ]}
  end

  # Lists
  defp do_normalize(list, parent_meta) when is_list(list) do
    if !Enum.empty?(list) and List.ascii_printable?(list) do
      # It's a charlist
      {:__block__, [line: parent_meta[:line], delimiter: "'"], [list]}
    else
      meta = [line: parent_meta[:line], closing: [line: parent_meta[:line]]]

      args = normalize_list_elements(list, parent_meta)

      {:__block__, meta, [args]}
    end
  end

  # Everything else
  defp do_normalize(quoted, _parent_meta) do
    quoted
  end

  defp normalize_call({form, meta, args}, parent_meta) do
    # Only normalize the form if it's a qualified call
    form =
      if is_atom(form) do
        form
      else
        do_normalize(form, meta)
      end

    meta =
      if meta == [] do
        line = parent_meta[:line]
        [line: line, closing: [line: line]]
      else
        meta
      end

    cond do
      Keyword.has_key?(meta, :do) ->
        normalize_kw_blocks(form, meta, args)

      match?([{:do, _} | _], List.last(args)) ->
        line = parent_meta[:line]
        meta = [do: [line: line], end: [line: line]]

        normalize_kw_blocks(form, meta, args)

      true ->
        args = Enum.map(args, &do_normalize(&1, meta))

        {form, meta, args}
    end
  end

  defp normalize_interpolation_parts(parts, _meta) do
    Enum.map(parts, fn
      {:"::", interpolation_meta,
       [
         {{:., dot_meta, [Kernel, :to_string]}, middle_meta, [middle]},
         {:binary, binary_meta, context}
       ]} ->
        middle = do_normalize(middle, dot_meta)

        {:"::", interpolation_meta,
         [
           {{:., dot_meta, [Kernel, :to_string]}, middle_meta, [middle]},
           {:binary, binary_meta, context}
         ]}

      part ->
        part
    end)
  end

  defp normalize_map_args([{key, value} | rest], parent_meta) do
    key =
      cond do
        is_atom(key) and not module_atom?(key) ->
          meta = [format: :keyword, line: parent_meta[:line]]
          {:__block__, meta, [key]}

        true ->
          do_normalize(key, parent_meta)
      end

    value = do_normalize(value, parent_meta)

    [{key, value} | normalize_map_args(rest, parent_meta)]
  end

  defp normalize_map_args([], _) do
    []
  end

  defp normalize_kw_blocks(form, meta, args) do
    {kw_blocks, leading_args} = List.pop_at(args, -1)

    kw_blocks =
      Enum.map(kw_blocks, fn {tag, block} ->
        block = do_normalize(block, meta)

        block =
          case block do
            {_, _, [[{:->, _, _} | _] = block]} -> block
            block -> block
          end

        # Only wrap the tag if it isn't already wrapped
        tag =
          case tag do
            {:__block__, _, _} -> tag
            _ -> {:__block__, [line: meta[:line]], [tag]}
          end

        {tag, block}
      end)

    {_, _, [leading_args]} = do_normalize(leading_args, meta)

    {form, meta, leading_args ++ [kw_blocks]}
  end

  defp normalize_list_elements(elems, parent_meta, keyword? \\ false)

  defp normalize_list_elements([[{_, _, [{_, _}]}] = first | rest], parent_meta, keyword?) do
    # Skip already normalized 2-tuples
    [first | normalize_list_elements(rest, parent_meta, keyword?)]
  end

  defp normalize_list_elements([{left, right} | rest], parent_meta, keyword?) do
    keyword? =
      if not keyword? do
        Enum.empty?(rest) or Inspect.List.keyword?(rest)
      else
        keyword?
      end

    module_atom? = module_atom?(left)

    left = do_normalize(left, parent_meta)
    right = do_normalize(right, parent_meta)

    pair =
      if keyword? and not module_atom? do
        left = Macro.update_meta(left, &Keyword.put(&1, :format, :keyword))
        {left, right}
      else
        {:__block__, [line: parent_meta[:line]], [{left, right}]}
      end

    [pair | normalize_list_elements(rest, parent_meta, keyword?)]
  end

  defp normalize_list_elements([first | rest], parent_meta, keyword?) do
    [do_normalize(first, parent_meta) | normalize_list_elements(rest, parent_meta, keyword?)]
  end

  defp normalize_list_elements([], _parent_meta, _keyword?) do
    []
  end

  defp patch_meta_line([], parent_meta) do
    [line: parent_meta[:line]]
  end

  defp patch_meta_line(meta, _) do
    meta
  end

  defp module_atom?(term) do
    is_atom(term) and match?('Elixir.' ++ _, Atom.to_charlist(term))
  end

  # Check if we have an interpolated string.
  defp interpolated?({:<<>>, _, [_ | _] = parts}) do
    Enum.all?(parts, fn
      {:"::", _, [{{:., _, [Kernel, :to_string]}, _, [_]}, {:binary, _, _}]} -> true
      binary when is_binary(binary) -> true
      _ -> false
    end)
  end

  defp interpolated?(_) do
    false
  end
end
