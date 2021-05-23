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
    escape = Keyword.get(opts, :escape, true)

    state = %{
      escape: escape,
      parent_meta: [line: line]
    }

    do_normalize(quoted, state)
  end

  # Skip normalized literals
  defp do_normalize({:__block__, _, [literal]} = quoted, _state) when is_literal(literal) do
    quoted
  end

  # Skip normalized charlists
  defp do_normalize({:__block__, meta, [charlist]} = quoted, state)
       when is_list(charlist) do
    if Keyword.has_key?(meta, :delimiter) do
      quoted
    else
      do_normalize(charlist, state)
    end
  end

  # Only normalize the first argument of an alias if it's not an atom
  defp do_normalize({:__aliases, meta, [first | rest]}, state) when not is_atom(first) do
    meta = patch_meta_line(meta, state.parent_meta)
    first = do_normalize(first, %{state | paret_meta: meta})
    {:__aliases__, meta, [first | rest]}
  end

  defp do_normalize({:__aliases__, _, _} = quoted, _state) do
    quoted
  end

  # Skip captured arguments like &1
  defp do_normalize({:&, meta, [term]}, state) when is_integer(term) do
    meta = patch_meta_line(meta, state.parent_meta)
    {:&, meta, [term]}
  end

  # Ranges
  defp do_normalize(left..right//step, state) do
    left = do_normalize(left, state)
    right = do_normalize(right, state)

    meta = [line: state.parent_meta[:line]]

    if step == 1 do
      {:.., meta, [left, right]}
    else
      step = do_normalize(step, state)
      {:"..//", meta, [left, right, step]}
    end
  end

  # Bit containers
  defp do_normalize({:<<>>, _, _} = quoted, state) do
    normalize_bitstring(quoted, state)
  end

  # Atoms with interpolations
  defp do_normalize(
         {{:., dot_meta, [:erlang, :binary_to_atom]}, call_meta, [{:<<>>, _, _} = string, :utf8]},
         state
       ) do
    dot_meta = patch_meta_line(dot_meta, state.parent_meta)
    call_meta = patch_meta_line(call_meta, dot_meta)

    string =
      if state.escape do
        normalize_bitstring(string, state, true)
      else
        normalize_bitstring(string, state)
      end

    {{:., dot_meta, [:erlang, :binary_to_atom]}, call_meta, [string, :utf8]}
  end

  # Skip charlists with interpolations
  defp do_normalize({{:., _, [List, :to_charlist]}, _, _} = quoted, _state) do
    quoted
  end

  # Don't normalize the `Access` atom in access syntax
  defp do_normalize({:., meta, [Access, :get]}, state) do
    meta = patch_meta_line(meta, state.parent_meta)
    {:., meta, [Access, :get]}
  end

  # Only normalize the left side of the dot operator
  # The right hand side is an atom in the AST but it's not an atom literal, so
  # it should not be wrapped
  defp do_normalize({:., meta, [left, right]}, state) do
    meta = patch_meta_line(meta, state.parent_meta)

    left = do_normalize(left, %{state | parent_meta: meta})

    {:., meta, [left, right]}
  end

  # A list of left to right arrows is not considered as a list literal, so it's
  # not wrapped
  defp do_normalize([{:->, _, _} | _] = quoted, state) do
    Enum.map(quoted, &do_normalize(&1, state))
  end

  # left -> right
  defp do_normalize({:->, meta, [left, right]}, state) do
    meta = patch_meta_line(meta, state.parent_meta)

    left = Enum.map(left, &do_normalize(&1, %{state | parent_meta: meta}))
    right = do_normalize(right, %{state | parent_meta: meta})
    {:->, meta, [left, right]}
  end

  # Maps
  defp do_normalize({:%{}, meta, args}, state) do
    meta =
      if meta == [] do
        line = state.parent_meta[:line]
        [line: line, closing: [line: line]]
      else
        meta
      end

    args =
      case args do
        [{:|, pipe_meta, [left, right]}] ->
          left = do_normalize(left, meta)
          {_, _, right} = do_normalize(right, %{state | parent_meta: meta})
          [{:|, pipe_meta, [left, right]}]

        [{_, _, _} = call] ->
          [do_normalize(call, %{state | parent_meta: meta})]

        args ->
          normalize_map_args(args, %{state | parent_meta: meta})
      end

    {:%{}, meta, args}
  end

  # Sigils
  defp do_normalize({sigil, meta, [{:<<>>, _, _} = string, modifiers]} = quoted, state)
       when is_atom(sigil) do
    case Atom.to_string(sigil) do
      <<"sigil_", _name>> ->
        meta = patch_meta_line(meta, state.parent_meta)

        {sigil, meta, [do_normalize(string, %{state | parent_meta: meta}), modifiers]}

      _ ->
        normalize_call(quoted, state)
    end
  end

  # Calls
  defp do_normalize({_, _, args} = quoted, state) when is_list(args) do
    normalize_call(quoted, state)
  end

  # Integers, floats, atoms
  defp do_normalize(literal, state) when is_literal(literal) do
    meta = [line: state.parent_meta[:line]]

    meta =
      if is_integer(literal) or is_float(literal) do
        Keyword.put(meta, :token, inspect(literal))
      else
        meta
      end

    meta =
      if not is_nil(state.parent_meta[:format]) do
        Keyword.put(meta, :format, state.parent_meta[:format])
      else
        meta
      end

    literal = maybe_escape_literal(literal, state)

    if module_atom?(literal) do
      "Elixir." <> segments = Atom.to_string(literal)

      segments =
        segments
        |> String.split(".")
        |> Enum.map(&String.to_atom/1)

      {:__aliases__, meta, segments}
    else
      {:__block__, meta, [literal]}
    end
  end

  # 2-tuples
  defp do_normalize({left, right}, state) do
    meta = [line: state.parent_meta[:line]]

    left_state =
      if is_atom(left) and not module_atom?(left) do
        %{state | parent_meta: Keyword.put(state.parent_meta, :format, :keyword)}
      else
        %{state | parent_meta: meta}
      end

    {:__block__, meta,
     [
       {do_normalize(left, left_state), do_normalize(right, state)}
     ]}
  end

  # Lists
  defp do_normalize(list, state) when is_list(list) do
    if list != [] and List.ascii_printable?(list) do
      # It's a charlist
      list =
        if state.escape do
          {string, _} = Code.Identifier.escape(IO.chardata_to_string(list), -1)
          IO.iodata_to_binary(string) |> to_charlist()
        else
          list
        end

      {:__block__, [line: state.parent_meta[:line], delimiter: "'"], [list]}
    else
      meta = [line: state.parent_meta[:line], closing: [line: state.parent_meta[:line]]]

      args = normalize_list_elements(list, state)

      {:__block__, meta, [args]}
    end
  end

  # Everything else
  defp do_normalize(quoted, _state) do
    quoted
  end

  defp normalize_call({form, meta, args}, state) do
    # Only normalize the form if it's a qualified call
    form =
      if is_atom(form) do
        form
      else
        do_normalize(form, %{state | parent_meta: meta})
      end

    meta =
      if meta == [] do
        line = state.parent_meta[:line]
        [line: line, closing: [line: line]]
      else
        meta
      end

    cond do
      Keyword.has_key?(meta, :do) ->
        normalize_kw_blocks(form, meta, args, state)

      match?([{:do, _} | _], List.last(args)) ->
        line = state.parent_meta[:line]
        meta = [do: [line: line], end: [line: line]]

        normalize_kw_blocks(form, meta, args, state)

      true ->
        args = Enum.map(args, &do_normalize(&1, %{state | parent_meta: state.parent_meta}))

        {form, meta, args}
    end
  end

  defp normalize_bitstring({:<<>>, meta, parts} = quoted, state, escape_interpolation \\ false) do
    meta = patch_meta_line(meta, state.parent_meta)

    parts =
      if interpolated?(quoted) do
        normalize_interpolation_parts(parts, %{state | parent_meta: meta}, escape_interpolation)
      else
        Enum.map(parts, &do_normalize(&1, %{state | parent_meta: meta}))
      end

    {:<<>>, meta, parts}
  end

  defp normalize_interpolation_parts(parts, state, escape_interpolation) do
    Enum.map(parts, fn
      {:"::", interpolation_meta,
       [
         {{:., dot_meta, [Kernel, :to_string]}, middle_meta, [middle]},
         {:binary, binary_meta, context}
       ]} ->
        middle = do_normalize(middle, %{state | parent_meta: dot_meta})

        {:"::", interpolation_meta,
         [
           {{:., dot_meta, [Kernel, :to_string]}, middle_meta, [middle]},
           {:binary, binary_meta, context}
         ]}

      part ->
        if escape_interpolation do
          maybe_escape_literal(part, state)
        else
          part
        end
    end)
  end

  defp normalize_map_args([{key, value} | rest], state) do
    key =
      cond do
        is_atom(key) and not module_atom?(key) ->
          meta = [format: :keyword, line: state.parent_meta[:line]]
          {:__block__, meta, [key]}

        true ->
          do_normalize(key, state)
      end

    value = do_normalize(value, state)

    [{key, value} | normalize_map_args(rest, state)]
  end

  defp normalize_map_args([], _) do
    []
  end

  defp normalize_kw_blocks(form, meta, args, state) do
    {kw_blocks, leading_args} = List.pop_at(args, -1)

    kw_blocks =
      Enum.map(kw_blocks, fn {tag, block} ->
        block = do_normalize(block, %{state | parent_meta: meta})

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

    {_, _, [leading_args]} = do_normalize(leading_args, %{state | parent_meta: meta})

    {form, meta, leading_args ++ [kw_blocks]}
  end

  defp normalize_list_elements(elems, state, keyword? \\ false)

  defp normalize_list_elements([[{_, _, [{_, _}]}] = first | rest], state, keyword?) do
    # Skip already normalized 2-tuples
    [first | normalize_list_elements(rest, state, keyword?)]
  end

  defp normalize_list_elements([{left, right} | rest], state, keyword?) do
    keyword? =
      if not keyword? do
        Enum.empty?(rest) or Inspect.List.keyword?(rest)
      else
        keyword?
      end

    module_atom? = module_atom?(left)

    left = do_normalize(left, state)
    right = do_normalize(right, state)

    pair =
      if keyword? and not module_atom? do
        left = Macro.update_meta(left, &Keyword.put(&1, :format, :keyword))
        {left, right}
      else
        {:__block__, [line: state.parent_meta[:line]], [{left, right}]}
      end

    [pair | normalize_list_elements(rest, state, keyword?)]
  end

  defp normalize_list_elements([first | rest], state, keyword?) do
    [do_normalize(first, state) | normalize_list_elements(rest, state, keyword?)]
  end

  defp normalize_list_elements([], _state, _keyword?) do
    []
  end

  defp maybe_escape_literal(string, %{escape: true}) when is_binary(string) do
    {string, _} = Code.Identifier.escape(string, nil)
    IO.iodata_to_binary(string)
  end

  defp maybe_escape_literal(atom, %{escape: true} = state) when is_atom(atom) do
    atom
    |> Atom.to_string()
    |> maybe_escape_literal(state)
    |> String.to_atom()
  end

  defp maybe_escape_literal(term, _) do
    term
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
