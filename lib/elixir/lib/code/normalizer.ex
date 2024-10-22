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
    line = Keyword.get(opts, :line, nil)
    escape = Keyword.get(opts, :escape, true)
    locals_without_parens = Keyword.get(opts, :locals_without_parens, [])

    state = %{
      escape: escape,
      parent_meta: [line: line],
      locals_without_parens: locals_without_parens ++ Code.Formatter.locals_without_parens()
    }

    do_normalize(quoted, state)
  end

  # Wrapped literals should receive the block meta
  defp do_normalize({:__block__, meta, [literal]}, state)
       when not is_tuple(literal) or tuple_size(literal) == 2 do
    normalize_literal(literal, meta, state)
  end

  # Only normalize the first argument of an alias if it's not an atom
  defp do_normalize({:__aliases__, meta, [first | rest]}, state) when not is_atom(first) do
    meta = patch_meta_line(meta, state.parent_meta)
    first = do_normalize(first, %{state | parent_meta: meta})
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
    meta = meta_line(state)

    if step == 1 do
      {:.., meta, [left, right]}
    else
      step = do_normalize(step, state)
      {:..//, meta, [left, right, step]}
    end
  end

  # Bit containers
  defp do_normalize({:<<>>, _, args} = quoted, state) when is_list(args) do
    normalize_bitstring(quoted, state)
  end

  # Atoms with interpolations
  defp do_normalize(
         {{:., dot_meta, [:erlang, :binary_to_atom]}, call_meta,
          [{:<<>>, _, parts} = string, :utf8]},
         state
       )
       when is_list(parts) do
    dot_meta = patch_meta_line(dot_meta, state.parent_meta)
    call_meta = patch_meta_line(call_meta, dot_meta)

    utf8 =
      if parts == [] or binary_interpolated?(parts) do
        # a non-normalized :utf8 atom signals an atom interpolation
        :utf8
      else
        normalize_literal(:utf8, [], state)
      end

    string =
      if state.escape do
        normalize_bitstring(string, state, true)
      else
        normalize_bitstring(string, state)
      end

    {{:., dot_meta, [:erlang, :binary_to_atom]}, call_meta, [string, utf8]}
  end

  # Charlists with interpolations
  # TODO: Remove this clause on Elixir v2.0 once single-quoted charlists are removed
  defp do_normalize({{:., dot_meta, [List, :to_charlist]}, call_meta, [parts]} = quoted, state) do
    if list_interpolated?(parts) do
      parts =
        Enum.map(parts, fn
          {{:., part_dot_meta, [Kernel, :to_string]}, part_call_meta, args} ->
            args = normalize_args(args, state)

            {{:., part_dot_meta, [Kernel, :to_string]}, part_call_meta, args}

          part when is_binary(part) ->
            if state.escape do
              maybe_escape_literal(part, state)
            else
              part
            end
        end)

      {{:., dot_meta, [List, :to_charlist]}, call_meta, [parts]}
    else
      normalize_call(quoted, state)
    end
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

  # left -> right
  defp do_normalize({:->, meta, [left, right]}, state) do
    meta = patch_meta_line(meta, state.parent_meta)

    left = normalize_args(left, %{state | parent_meta: meta})
    right = do_normalize(right, %{state | parent_meta: meta})
    {:->, meta, [left, right]}
  end

  # Maps
  defp do_normalize({:%{}, meta, args}, state) when is_list(args) do
    meta =
      if meta == [] do
        line = state.parent_meta[:line]
        [line: line, closing: [line: line]]
      else
        meta
      end

    state = %{state | parent_meta: meta}

    args =
      case args do
        [{:|, pipe_meta, [left, right]}] ->
          left = do_normalize(left, state)
          right = normalize_map_args(right, state)
          [{:|, pipe_meta, [left, right]}]

        args ->
          normalize_map_args(args, state)
      end

    {:%{}, meta, args}
  end

  # Sigils
  defp do_normalize({sigil, meta, [{:<<>>, _, args} = string, modifiers]} = quoted, state)
       when is_atom(sigil) and is_list(args) and is_list(modifiers) do
    with "sigil_" <> _ <- Atom.to_string(sigil),
         true <- binary_interpolated?(args),
         true <- List.ascii_printable?(modifiers) do
      meta =
        meta
        |> patch_meta_line(state.parent_meta)
        |> Keyword.put_new(:delimiter, "\"")

      {sigil, meta, [do_normalize(string, %{state | parent_meta: meta}), modifiers]}
    else
      _ ->
        normalize_call(quoted, state)
    end
  end

  # Tuples
  defp do_normalize({:{}, meta, args} = quoted, state) when is_list(args) do
    {last_arg, args} = List.pop_at(args, -1)

    if args != [] and match?([_ | _], last_arg) and keyword?(last_arg) do
      args = normalize_args(args, state)
      kw_list = normalize_kw_args(last_arg, state, true)
      {:{}, meta, args ++ kw_list}
    else
      normalize_call(quoted, state)
    end
  end

  # Module attributes
  defp do_normalize({:@, meta, [{name, name_meta, [value]}]}, state) do
    value =
      cond do
        keyword?(value) and value != [] ->
          normalize_kw_args(value, state, true)

        is_list(value) ->
          normalize_literal(value, meta, state)

        true ->
          do_normalize(value, state)
      end

    {:@, meta, [{name, name_meta, [value]}]}
  end

  # Regular blocks
  defp do_normalize({:__block__, meta, args}, state) when is_list(args) do
    {:__block__, meta, normalize_args(args, state)}
  end

  # Calls
  defp do_normalize({_, _, args} = quoted, state) when is_list(args) do
    normalize_call(quoted, state)
  end

  # Vars
  defp do_normalize({_, _, context} = quoted, _state) when is_atom(context) do
    quoted
  end

  # Literals
  defp do_normalize(quoted, state) do
    normalize_literal(quoted, [], state)
  end

  # Numbers
  defp normalize_literal(number, meta, state) when is_number(number) do
    meta =
      meta
      |> Keyword.put_new(:token, inspect(number))
      |> patch_meta_line(state.parent_meta)

    {:__block__, meta, [number]}
  end

  # Atom, Strings
  defp normalize_literal(literal, meta, state) when is_atom(literal) or is_binary(literal) do
    meta = patch_meta_line(meta, state.parent_meta)
    literal = maybe_escape_literal(literal, state)

    if is_atom(literal) and Macro.classify_atom(literal) == :alias and
         is_nil(meta[:delimiter]) do
      segments =
        case Atom.to_string(literal) do
          "Elixir" ->
            [:"Elixir"]

          "Elixir." <> segments ->
            segments
            |> String.split(".")
            |> Enum.map(&String.to_atom/1)
        end

      {:__aliases__, meta, segments}
    else
      {:__block__, meta, [literal]}
    end
  end

  # 2-tuples
  defp normalize_literal({left, right}, meta, state) do
    meta = patch_meta_line(meta, state.parent_meta)
    state = %{state | parent_meta: meta}

    if match?([_ | _], right) and keyword?(right) do
      {:__block__, meta, [{do_normalize(left, state), normalize_kw_args(right, state, true)}]}
    else
      {:__block__, meta, [{do_normalize(left, state), do_normalize(right, state)}]}
    end
  end

  # Lists
  defp normalize_literal(list, meta, state) when is_list(list) do
    if list != [] and List.ascii_printable?(list) do
      # It's a charlist, we normalize it as a ~C sigil
      string =
        if state.escape do
          {iolist, _} = Code.Identifier.escape(IO.chardata_to_string(list), nil)
          IO.iodata_to_binary(iolist)
        else
          List.to_string(list)
        end

      meta = patch_meta_line([delimiter: "\""], state.parent_meta)

      {:sigil_c, meta, [{:<<>>, [], [string]}, []]}
    else
      meta =
        if line = state.parent_meta[:line] do
          meta
          |> Keyword.put_new(:closing, line: line)
          |> patch_meta_line(state.parent_meta)
        else
          meta
        end

      {:__block__, meta, [normalize_kw_args(list, state, false)]}
    end
  end

  # Probably an invalid value, wrap it and send it upstream
  defp normalize_literal(quoted, meta, _state) do
    {:__block__, meta, [quoted]}
  end

  defp normalize_call({form, meta, args}, state) do
    meta = patch_meta_line(meta, state.parent_meta)
    arity = length(args)

    # Only normalize the form if it's a qualified call
    form =
      if is_atom(form) do
        form
      else
        do_normalize(form, %{state | parent_meta: meta})
      end

    meta =
      if is_nil(meta[:no_parens]) and is_nil(meta[:closing]) and is_nil(meta[:do]) and
           not Code.Formatter.local_without_parens?(form, arity, state.locals_without_parens) do
        [closing: [line: meta[:line]]] ++ meta
      else
        meta
      end

    last = List.last(args)

    cond do
      not allow_keyword?(form, arity) ->
        args = normalize_args(args, %{state | parent_meta: meta})
        {form, meta, args}

      Keyword.has_key?(meta, :do) ->
        # def foo do :ok end
        # def foo, do: :ok
        normalize_kw_blocks(form, meta, args, state)

      match?([{:do, _} | _], last) and Keyword.keyword?(last) ->
        # Non normalized kw blocks
        line = state.parent_meta[:line] || meta[:line]
        meta = meta ++ [do: [line: line], end: [line: line]]
        normalize_kw_blocks(form, meta, args, state)

      true ->
        args = normalize_args(args, %{state | parent_meta: meta})
        {last_arg, leading_args} = List.pop_at(args, -1, [])

        last_args =
          case last_arg do
            {:__block__, _meta, [[{{:__block__, key_meta, _}, _} | _] = keyword]} ->
              cond do
                key_meta[:format] == :keyword ->
                  [keyword]

                block_keyword?(keyword) ->
                  [
                    Enum.map(keyword, fn {{:__block__, meta, args}, value} ->
                      {{:__block__, [format: :keyword] ++ meta, args}, value}
                    end)
                  ]

                true ->
                  [last_arg]
              end

            [] ->
              []

            _ ->
              [last_arg]
          end

        {form, meta, leading_args ++ last_args}
    end
  end

  defp block_keyword?([{{:__block__, _, [key]}, _val} | tail]) when is_atom(key),
    do: block_keyword?(tail)

  defp block_keyword?([]), do: true
  defp block_keyword?(_), do: false

  defp allow_keyword?(:when, 2), do: true
  defp allow_keyword?(:{}, _), do: false
  defp allow_keyword?(op, arity), do: not is_atom(op) or not Macro.operator?(op, arity)

  defp normalize_bitstring({:<<>>, meta, parts}, state, escape_interpolation \\ false) do
    meta = patch_meta_line(meta, state.parent_meta)

    parts =
      if binary_interpolated?(parts) do
        normalize_interpolation_parts(parts, %{state | parent_meta: meta}, escape_interpolation)
      else
        state = %{state | parent_meta: meta}

        Enum.map(parts, fn part ->
          with {:"::", meta, [left, _]} <- part,
               true <- meta[:inferred_bitstring_spec] do
            do_normalize(left, state)
          else
            _ -> do_normalize(part, state)
          end
        end)
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

  defp normalize_map_args(args, state) do
    Enum.map(normalize_kw_args(args, state, false), fn
      {:__block__, _, [{_, _} = pair]} -> pair
      pair -> pair
    end)
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

    leading_args = normalize_args(leading_args, %{state | parent_meta: meta})
    {form, meta, leading_args ++ [kw_blocks]}
  end

  defp normalize_kw_args(elems, state, keyword?)

  defp normalize_kw_args(
         [{{:__block__, key_meta, [key]}, value} = first | rest] = current,
         state,
         keyword?
       )
       when is_atom(key) do
    keyword? = keyword? or keyword?(current)

    first =
      if key_meta[:format] == :keyword and not keyword? do
        key_meta = Keyword.delete(key_meta, :format)
        line = key_meta[:line] || meta_line(state)
        {:__block__, [line: line], [{{:__block__, key_meta, [key]}, value}]}
      else
        first
      end

    [first | normalize_kw_args(rest, state, keyword?)]
  end

  defp normalize_kw_args([{left, right} | rest] = current, state, keyword?) do
    keyword? = keyword? or keyword?(current)

    left =
      if keyword? do
        meta = [format: :keyword] ++ meta_line(state)
        {:__block__, meta, [maybe_escape_literal(left, state)]}
      else
        do_normalize(left, state)
      end

    right = do_normalize(right, state)

    pair =
      with {:__block__, meta, _} <- left,
           :keyword <- meta[:format] do
        {left, right}
      else
        _ -> {:__block__, meta_line(state), [{left, right}]}
      end

    [pair | normalize_kw_args(rest, state, keyword?)]
  end

  defp normalize_kw_args([first | rest], state, keyword?) do
    [do_normalize(first, state) | normalize_kw_args(rest, state, keyword?)]
  end

  defp normalize_kw_args([], _state, _keyword?) do
    []
  end

  defp normalize_args(args, state) do
    Enum.map(args, &do_normalize(&1, state))
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

  defp binary_interpolated?(parts) do
    Enum.all?(parts, fn
      {:"::", _, [{{:., _, [Kernel, :to_string]}, _, [_]}, {:binary, _, _}]} -> true
      binary when is_binary(binary) -> true
      _ -> false
    end)
  end

  defp list_interpolated?(parts) do
    Enum.all?(parts, fn
      {{:., _, [Kernel, :to_string]}, _, [_]} -> true
      binary when is_binary(binary) -> true
      _ -> false
    end)
  end

  defp patch_meta_line(meta, parent_meta) do
    with nil <- meta[:line],
         line when is_integer(line) <- parent_meta[:line] do
      [line: line] ++ meta
    else
      _ -> meta
    end
  end

  defp meta_line(state) do
    if line = state.parent_meta[:line] do
      [line: line]
    else
      []
    end
  end

  defp keyword?([{{:__block__, key_meta, [key]}, _} | rest]) when is_atom(key) do
    if key_meta[:format] == :keyword do
      keyword?(rest)
    else
      false
    end
  end

  defp keyword?([{key, _value} | rest]) when is_atom(key) do
    case Atom.to_charlist(key) do
      ~c"Elixir." ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  defp keyword?([]), do: true
  defp keyword?(_other), do: false
end
