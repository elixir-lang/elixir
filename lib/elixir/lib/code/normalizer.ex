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

  # Skip already wrapped blocks
  defp do_normalize({:__block__, _, [literal]} = quoted, _parent_meta) when is_literal(literal) do
    quoted
  end

  # Only normalize the first argument of an alias if it's not an atom
  defp do_normalize({:__aliases, meta, [first | rest]}, _parent_meta) when not is_atom(first) do
    first = do_normalize(first, meta)
    {:__aliases__, meta, [first | rest]}
  end

  defp do_normalize({:__aliases__, _, _} = quoted, _parent_meta) do
    quoted
  end

  # Don't normalize the `Access` atom in access syntax
  defp do_normalize({:., _, [Access, :get]} = quoted, _parent_meta) do
    quoted
  end

  # Only normalize the left side of the dot operator
  # The right hand side is an atom in the AST but it's not an atom literal, so
  # it should not be wrapped
  defp do_normalize({:., meta, [left, right]}, _parent_meta) do
    {:., meta, [do_normalize(left, meta), right]}
  end

  # A list of left to right arrows is not considered as a list literal, so it's
  # not wrapped
  defp do_normalize([{:->, _, _} | _] = quoted, parent_meta) do
    Enum.map(quoted, &do_normalize(&1, parent_meta))
  end

  # left -> right
  defp do_normalize({:->, meta, [left, right]}, _parent_meta) do
    left = Enum.map(left, &do_normalize(&1, meta))
    right = do_normalize(right, meta)
    {:->, meta, [left, right]}
  end

  # Maps
  defp do_normalize({:%{}, meta, args}, _parent_meta) do
    args = Enum.map(args, &do_normalize(&1, meta))

    args =
      case args do
        # Unwrap the right hand side if we're in an update syntax
        [{:|, pipe_meta, [left, {_, _, [right]}]}] ->
          [{:|, pipe_meta, [left, right]}]

        # Unwrap args so we have 2-tuples instead of blocks
        [{_, _, args}] ->
          args

        args ->
          args
      end

    {:%{}, meta, args}
  end

  # If a keyword list is an argument of a guard, we need to drop the block
  # wrapping
  defp do_normalize({:when, meta, args} = _quoted, _parent_meta) do
    args =
      Enum.map(args, fn
        arg when is_list(arg) ->
          {_, _, [arg]} = do_normalize(arg, meta)
          arg

        arg ->
          do_normalize(arg, meta)
      end)

    {:when, meta, args}
  end

  # Calls
  defp do_normalize({form, meta, args}, _parent_meta) when is_list(args) do
    # Only normalize the form if it's a qualified call
    form =
      if is_atom(form) do
        form
      else
        do_normalize(form, meta)
      end

    cond do
      Keyword.has_key?(meta, :do) ->
        {last_arg, leading_args} = List.pop_at(args, -1)

        last_arg =
          Enum.map(last_arg, fn {tag, block} ->
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

        {form, meta, leading_args ++ [last_arg]}

      true ->
        args = Enum.map(args, &do_normalize(&1, meta))

        {form, meta, args}
    end
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

    {:__block__, meta, [x]}
  end

  # 2-tuples
  defp do_normalize({left, right}, parent_meta) do
    meta = [line: parent_meta[:line]]

    left_parent_meta =
      if is_atom(left) do
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

  defp normalize_list_elements(elems, parent_meta, keyword? \\ false)

  defp normalize_list_elements([[{_, _, [{_, _}]}] = first | rest], parent_meta, keyword?) do
    # Skip already normalized 2-tuples
    [first | normalize_list_elements(rest, parent_meta, keyword?)]
  end

  defp normalize_list_elements([{left, right} | rest], parent_meta, keyword?) do
    keyword? =
      if not keyword? do
        Enum.empty?(rest) or keyword?(rest)
      else
        keyword?
      end

    pair =
      if keyword? do
        {_, _, [{left, right}]} = do_normalize({left, right}, parent_meta)
        left = Macro.update_meta(left, &Keyword.put(&1, :format, :keyword))
        {left, right}
      else
        left = do_normalize(left, parent_meta)
        right = do_normalize(right, parent_meta)
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

  defp keyword?([{_, _} | list]), do: keyword?(list)
  defp keyword?(rest), do: rest == []
end
