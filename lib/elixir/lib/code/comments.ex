defmodule Code.Comments do
  @moduledoc false

  @end_fields [:end, :closing, :end_of_expression]
  @block_names [:do, :else, :catch, :rescue, :after]
  @arrow_ops [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :"<|>", :->]

  defguardp is_arrow_op(op) when is_atom(op) and op in @arrow_ops

  @doc """
  Merges the comments into the given quoted expression.
  """
  @spec merge_comments(Macro.t(), list(map)) :: Macro.t()
  def merge_comments({:__block__, _, []} = empty_ast, comments) do
    comments = Enum.sort_by(comments, & &1.line)

    empty_ast
    |> ensure_comments_meta()
    |> put_comments(:inner_comments, comments)
  end

  def merge_comments(quoted, comments) do
    comments = Enum.sort_by(comments, & &1.line)

    state = %{
      comments: comments,
      parent_meta: []
    }

    {quoted, %{comments: leftovers}} = Macro.prewalk(quoted, state, &do_merge_comments/2)

    merge_leftovers(quoted, leftovers)
  end

  defp merge_leftovers({:__block__, _, args} = quoted, comments) when is_list(args) do
    {last_arg, args} = List.pop_at(args, -1)

    case last_arg do
      nil ->
        append_comments(quoted, :inner_comments, comments)

      {_, _, _} = last_arg ->
        last_arg = append_comments(last_arg, :trailing_comments, comments)

        args = args ++ [last_arg]
        put_args(quoted, args)

      _ ->
        append_comments(quoted, :trailing_comments, comments)
    end
  end

  defp merge_leftovers(quoted, comments) do
    append_comments(quoted, :trailing_comments, comments)
  end

  defp do_merge_comments({_, _, _} = quoted, state) do
    quoted = ensure_comments_meta(quoted)
    {quoted, state} = merge_mixed_comments(quoted, state)
    merge_leading_comments(quoted, state)
  end

  defp do_merge_comments(quoted, state) do
    {quoted, state}
  end

  defp ensure_comments_meta({form, meta, args}) do
    meta =
      meta
      |> Keyword.put_new(:leading_comments, [])
      |> Keyword.put_new(:trailing_comments, [])
      |> Keyword.put_new(:inner_comments, [])

    {form, meta, args}
  end

  defp merge_leading_comments(quoted, state) do
    # If a comment is placed on top of a pipeline or binary operator line,
    # we should not merge it with the operator itself. Instead, we should
    # merge it with the first argument of the pipeline.
    with {form, _, _} <- quoted,
         false <- is_arrow_op(form) do
      {comments, rest} = gather_leading_comments_for_node(quoted, state.comments)
      comments = Enum.sort_by(comments, & &1.line)

      quoted = put_leading_comments(quoted, comments)
      {quoted, %{state | comments: rest}}
    else
      _ ->
        {quoted, state}
    end
  end

  defp gather_leading_comments_for_node(quoted, comments) do
    line = get_line(quoted, 0)

    {comments, rest} =
      Enum.reduce(comments, {[], []}, fn
        comment, {comments, rest} ->
          if comment.line <= line do
            {[comment | comments], rest}
          else
            {comments, [comment | rest]}
          end
      end)

    rest = Enum.reverse(rest)

    {comments, rest}
  end

  defp put_leading_comments({form, meta, args}, comments) do
    with {_, _} <- Code.Identifier.binary_op(form),
         [_, _] <- args do
      put_binary_op_comments({form, meta, args}, comments)
    else
      _ ->
        append_comments({form, meta, args}, :leading_comments, comments)
    end
  end

  defp put_trailing_comments({form, meta, args}, comments) do
    with {_, _} <- Code.Identifier.binary_op(form),
         [{_, _, _} = left, {_, _, _} = right] <- args do
      right = append_comments(right, :trailing_comments, comments)

      {form, meta, [left, right]}
    else
      _ ->
        append_comments({form, meta, args}, :trailing_comments, comments)
    end
  end

  defp put_binary_op_comments({_, _, [left, right]} = binary_op, comments) do
    {leading_comments, rest} =
      Enum.split_with(comments, &(&1.line <= get_line(left)))

    right_node =
      case right do
        [{_, _, _} = first | _other] ->
          first

        [{_, right} | _other] ->
          right

        _ ->
          right
      end

    {trailing_comments, rest} =
      Enum.split_with(
        rest,
        &(&1.line > get_line(right_node) && &1.line < get_end_line(right, get_line(right_node)))
      )

    {op_leading_comments, _rest} =
      Enum.split_with(rest, &(&1.line <= get_line(binary_op)))

    left = append_comments(left, :leading_comments, leading_comments)

    # It is generally inconvenient to attach comments to the operator itself.
    # Considering the following example:
    #
    #     one
    #     # when two
    #     # when three
    #     when four
    #          # | five
    #          | six
    #
    # The AST for the above code will be equivalent to this:
    #
    #         when
    #       /     \
    #     one     :|
    #            /  \
    #         four   six
    #
    # Putting the comments on the operator makes formatting harder to perform, as
    # it would need to hoist comments from child nodes above the operator location.
    # Having the `# when two` and `# when three` comments as trailing for the left
    # node is more convenient for formatting.
    # It is also more intuitive, since those comments are related to the left node,
    # not the operator itself.
    #
    # The same applies for the `:|` operator; the `# | five` comment is attached to
    # the left node `four`, not the operator.
    left = append_comments(left, :trailing_comments, op_leading_comments)

    right =
      case right do
        [{key, value} | other] ->
          value = append_comments(value, :trailing_comments, trailing_comments)
          [{key, value} | other]

        [first | other] ->
          first = append_comments(first, :trailing_comments, trailing_comments)
          [first | other]

        _ ->
          append_comments(right, :trailing_comments, trailing_comments)
      end

    put_args(binary_op, [left, right])
  end

  # Structs
  defp merge_mixed_comments({:%, _, [name, args]} = quoted, state) do
    {args, state} = merge_mixed_comments(args, state)

    quoted = put_args(quoted, [name, args])

    {quoted, state}
  end

  # Map update
  defp merge_mixed_comments({:%{}, _, [{:|, pipe_meta, [left, right]}]} = quoted, state)
       when is_list(right) do
    {right, state} = merge_map_args_trailing_comments(quoted, right, state)

    quoted = put_args(quoted, [{:|, pipe_meta, [left, right]}])

    {quoted, state}
  end

  # Maps
  defp merge_mixed_comments({:%{}, _, [{_key, _value} | _] = args} = quoted, state) do
    {args, state} = merge_map_args_trailing_comments(quoted, args, state)

    quoted = put_args(quoted, args)
    {quoted, state}
  end

  # Binary interpolation
  defp merge_mixed_comments({:<<>>, _meta, args} = quoted, state) do
    if interpolated?(args) do
      {args, state} =
        Enum.map_reduce(args, state, &merge_interpolation_comments/2)

      quoted = put_args(quoted, args)

      {quoted, state}
    else
      merge_call_comments(quoted, state)
    end
  end

  # List interpolation
  defp merge_mixed_comments(
         {{:., _dot_meta, [List, :to_charlist]}, _meta, [args]} = quoted,
         state
       ) do
    {args, state} =
      Enum.map_reduce(args, state, &merge_interpolation_comments/2)

    quoted = put_args(quoted, [args])

    {quoted, state}
  end

  # Lists
  defp merge_mixed_comments({:__block__, _, [args]} = quoted, %{comments: comments} = state)
       when is_list(args) do
    {quoted, comments} =
      case List.pop_at(args, -1) do
        {nil, _} ->
          # There's no items in the list, merge the comments as inner comments
          start_line = get_line(quoted)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          quoted = append_comments(quoted, :inner_comments, trailing_comments)

          {quoted, comments}

        {{last_key, last_value}, args} ->
          # Partial keyword list, merge the comments as trailing for the value part
          start_line = get_line(last_value)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          last_value = append_comments(last_value, :trailing_comments, trailing_comments)

          args = args ++ [{last_key, last_value}]

          quoted = put_args(quoted, [args])
          {quoted, comments}

        {{:unquote_splicing, _, _} = unquote_splicing, other} ->
          start_line = get_line(unquote_splicing)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          unquote_splicing =
            append_comments(unquote_splicing, :trailing_comments, trailing_comments)

          args = other ++ [unquote_splicing]
          quoted = put_args(quoted, [args])

          {quoted, comments}

        {{:__block__, _, [_, _ | _]}, _args} ->
          # In the case of a block in the list, there are no comments to merge,
          # so we skip it. Otherwise we may attach trailing comments inside the
          # block to the block itself, which is not the desired behavior.
          {quoted, comments}

        {{_, _, _} = value, args} ->
          start_line = get_line(value)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          value = append_comments(value, :trailing_comments, trailing_comments)

          args = args ++ [value]
          quoted = put_args(quoted, [args])

          {quoted, comments}

        _ ->
          {quoted, comments}
      end

    {quoted, %{state | parent_meta: [], comments: comments}}
  end

  # 2-tuples
  defp merge_mixed_comments(
         {:__block__, _, [{left, right}]} = quoted,
         %{comments: comments} = state
       )
       when is_tuple(left) and is_tuple(right) do
    start_line = get_line(right)
    end_line = get_end_line(quoted, start_line)

    {trailing_comments, comments} =
      split_trailing_comments(comments, start_line, end_line)

    right = append_comments(right, :trailing_comments, trailing_comments)

    quoted = put_args(quoted, [{left, right}])
    {quoted, %{state | comments: comments}}
  end

  # Stabs
  defp merge_mixed_comments({:->, _, [left, right]} = quoted, state) do
    start_line = get_line(right)
    end_line = get_end_line({:__block__, state.parent_meta, [quoted]}, start_line)
    block_start = get_line({:__block__, state.parent_meta, [quoted]})

    {right, comments} =
      case right do
        {:__block__, _, _} ->
          merge_block_comments(right, start_line, end_line, state.comments)

        {_, meta, _} = call ->
          if !meta[:trailing_comments] do
            line = get_line(call)

            {trailing_comments, comments} =
              split_trailing_comments(state.comments, line, end_line)

            call = append_comments(call, :trailing_comments, trailing_comments)

            {call, comments}
          else
            {right, state.comments}
          end
      end

    {quoted, comments} =
      case left do
        [] ->
          {leading_comments, comments} =
            split_leading_comments(comments, block_start, start_line)

          quoted = append_comments(quoted, :leading_comments, leading_comments)

          {quoted, comments}

        _ ->
          {quoted, comments}
      end

    quoted = put_args(quoted, [left, right])

    {quoted, %{state | comments: comments}}
  end

  # Calls
  defp merge_mixed_comments({form, meta, args} = quoted, state)
       when is_list(args) and meta != [] do
    with true <- is_atom(form),
         <<"sigil_", _name::binary>> <- Atom.to_string(form),
         true <- not is_nil(meta) do
      [content | modifiers] = args

      {content, state} = merge_mixed_comments(content, state)

      quoted = put_args(quoted, [content | modifiers])

      {quoted, state}
    else
      _ ->
        merge_call_comments(quoted, state)
    end
  end

  defp merge_mixed_comments(quoted, state) do
    {quoted, state}
  end

  defp merge_call_comments({_, meta, quoted_args} = quoted, %{comments: comments} = state) do
    start_line = get_line(quoted)
    end_line = get_end_line(quoted, start_line)
    {last_arg, args} = List.pop_at(quoted_args, -1)

    meta_keys = Keyword.keys(meta)

    state =
      if Enum.any?([:do, :end, :closing], &(&1 in meta_keys)) do
        %{state | parent_meta: meta}
      else
        state
      end

    {quoted, comments} =
      case last_arg do
        [{{:__block__, _, [name]}, _block_args} | _] = blocks when name in @block_names ->
          # For do/end and else/catch/rescue/after blocks, we need to merge the comments
          # of each block with the arguments block.
          {reversed_blocks, comments} =
            each_merge_named_block_comments(blocks, quoted, comments, [])

          last_arg = Enum.reverse(reversed_blocks)

          args = args ++ [last_arg]
          quoted = put_args(quoted, args)

          {quoted, comments}

        {:->, _, _} ->
          {args, comments} =
            merge_stab_clause_comments(quoted_args, start_line, end_line, comments, [])

          quoted = put_args(quoted, args)

          {quoted, comments}

        [{_key, _value} | _] = pairs ->
          # Partial keyword list
          {{last_key, last_value}, pairs} = List.pop_at(pairs, -1)
          line = get_line(last_value)

          {trailing_comments, comments} =
            split_trailing_comments(comments, line, end_line)

          last_value = append_comments(last_value, :trailing_comments, trailing_comments)

          pairs = pairs ++ [{last_key, last_value}]

          args = args ++ [pairs]

          quoted = put_args(quoted, args)

          {quoted, comments}

        {:__block__, _, [{_, _, _} | _] = block_args} = block when args == [] ->
          # This handles cases where the last argument for a call is a block, for example:
          #
          #     assert (
          #              # comment
          #              hello
          #              world
          #            )
          #

          {last_block_arg, block_args} = List.pop_at(block_args, -1)

          start_line = get_line(last_block_arg)

          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          last_block_arg = append_comments(last_block_arg, :trailing_comments, trailing_comments)

          block_args = block_args ++ [last_block_arg]

          block = put_args(block, block_args)

          quoted = put_args(quoted, [block])

          {quoted, comments}

        {:__block__, _, [args]} when is_list(args) ->
          {quoted, comments}

        {form, _, _} ->
          if match?({_, _}, Code.Identifier.binary_op(form)) do
            {quoted, comments}
          else
            line = get_end_line(last_arg, get_line(last_arg))

            {trailing_comments, comments} =
              split_trailing_comments(comments, line, end_line)

            last_arg = append_comments(last_arg, :trailing_comments, trailing_comments)

            args = args ++ [last_arg]

            quoted = put_args(quoted, args)

            {quoted, comments}
          end

        nil ->
          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          quoted = append_comments(quoted, :inner_comments, trailing_comments)
          {quoted, comments}

        _ ->
          {quoted, comments}
      end

    {quoted, %{state | comments: comments}}
  end

  defp merge_interpolation_comments(
         {:"::", interpolation_meta, [{dot_call, inner_meta, [value]}, modifier]} = interpolation,
         state
       ) do
    start_line = get_line(interpolation)
    end_line = get_end_line(interpolation, start_line)
    value_line = get_line(value)

    {leading_comments, comments} =
      split_leading_comments(state.comments, start_line, value_line)

    {trailing_comments, comments} =
      split_trailing_comments(comments, value_line, end_line)

    value = put_leading_comments(value, leading_comments)
    value = put_trailing_comments(value, trailing_comments)

    interpolation = {:"::", interpolation_meta, [{dot_call, inner_meta, [value]}, modifier]}

    {interpolation, %{state | comments: comments}}
  end

  defp merge_interpolation_comments(
         {{:., dot_meta, [Kernel, :to_string]}, interpolation_meta, [value]} = interpolation,
         state
       ) do
    start_line = get_line(interpolation)
    end_line = get_end_line(interpolation, start_line)
    value_line = get_line(value)

    {leading_comments, comments} =
      split_leading_comments(state.comments, start_line, value_line)

    {trailing_comments, comments} =
      split_trailing_comments(comments, value_line, end_line)

    value = put_leading_comments(value, leading_comments)
    value = put_trailing_comments(value, trailing_comments)

    interpolation = {{:., dot_meta, [Kernel, :to_string]}, interpolation_meta, [value]}

    {interpolation, %{state | comments: comments}}
  end

  defp merge_interpolation_comments(quoted, state) do
    {quoted, state}
  end

  defp merge_map_args_trailing_comments(quoted, args, %{comments: comments} = state) do
    case List.pop_at(args, -1) do
      {{last_key, last_value}, args} ->
        start_line = get_line(last_value)
        end_line = get_end_line(quoted, start_line)

        {trailing_comments, comments} =
          split_trailing_comments(comments, start_line, end_line)

        last_value = append_comments(last_value, :trailing_comments, trailing_comments)

        args = args ++ [{last_key, last_value}]

        {args, %{state | comments: comments}}

      {{:unquote_splicing, _, _} = unquote_splicing, other} ->
        start_line = get_line(unquote_splicing)
        end_line = get_end_line(quoted, start_line)

        {trailing_comments, comments} =
          split_trailing_comments(comments, start_line, end_line)

        unquote_splicing =
          append_comments(unquote_splicing, :trailing_comments, trailing_comments)

        args = other ++ [unquote_splicing]

        {args, %{state | comments: comments}}
    end
  end

  defp each_merge_named_block_comments([], _, comments, acc), do: {acc, comments}

  defp each_merge_named_block_comments([{block, block_args} | rest], parent, comments, acc) do
    block_start = get_line(block)

    block_end =
      case rest do
        [{next_block, _} | _] ->
          # The parent node only has metadata about the `do` and `end` token positions,
          # but in order to know when each individual block ends, we need to look at the
          # next block.
          get_line(next_block)

        [] ->
          # If there is no next block, we can assume the `end` token follows, so we use
          # the parent node's end position.
          get_end_line(parent, 0)
      end

    {block, block_args, comments} =
      merge_named_block_comments(block, block_args, block_start, block_end, comments)

    acc = [{block, block_args} | acc]

    each_merge_named_block_comments(rest, parent, comments, acc)
  end

  defp merge_named_block_comments(
         block,
         {_, _, args} = block_args,
         block_start,
         block_end,
         comments
       )
       when is_list(args) do
    {last_arg, args} = List.pop_at(args, -1)

    case last_arg do
      nil ->
        {trailing_comments, comments} =
          split_trailing_comments(comments, block_start, block_end)

        block_args = append_comments(block_args, :inner_comments, trailing_comments)

        {block, block_args, comments}

      last_arg when not is_list(last_arg) ->
        case last_arg do
          {:__block__, _, [args]} when is_list(args) ->
            # It it's a list, we skip merging comments to avoid collecting all trailing
            # comments into the list metadata. Otherwise, we will not be able to collect
            # leading comments for the individual elements in the list.
            {block, block_args, comments}

          _ ->
            {last_arg, comments} =
              merge_comments_to_last_arg(last_arg, block_start, block_end, comments)

            args = args ++ [last_arg]
            block_args = put_args(block_args, args)

            {block, block_args, comments}
        end

      _ ->
        {block, block_args, comments}
    end
  end

  # If a do/end block has a single argument, it will not be wrapped in a `:__block__` node,
  # so we need to check for that.
  defp merge_named_block_comments(
         block,
         {_, _, ctx} = single_arg,
         block_start,
         block_end,
         comments
       )
       when not is_list(ctx) do
    {last_arg, comments} =
      merge_comments_to_last_arg(single_arg, block_start, block_end, comments)

    {block, last_arg, comments}
  end

  defp merge_named_block_comments(
         block,
         [{:->, _, _} | _] = block_args,
         block_start,
         block_end,
         comments
       ) do
    {block_args, comments} =
      merge_stab_clause_comments(block_args, block_start, block_end, comments, [])

    {block, block_args, comments}
  end

  defp merge_stab_clause_comments(
         [{:->, _stab_meta, [left, right]} = stab | rest],
         block_start,
         block_end,
         comments,
         acc
       ) do
    start_line = get_line(right)

    end_line =
      case rest do
        [{:->, _, _} | _] ->
          get_end_line(right, start_line)

        [] ->
          block_end
      end

    {stab, comments} =
      case left do
        [] ->
          stab_line = get_line(stab)

          {leading_comments, comments} =
            split_leading_comments(comments, block_start, stab_line)

          stab = append_comments(stab, :leading_comments, leading_comments)

          {stab, comments}

        _ ->
          {stab, comments}
      end

    {right, comments} =
      case right do
        {:__block__, _, _} ->
          merge_block_comments(right, start_line, end_line, comments)

        call ->
          {trailing_comments, comments} =
            split_trailing_comments(comments, start_line, end_line)

          call = append_comments(call, :trailing_comments, trailing_comments)

          {call, comments}
      end

    stab = put_args(stab, [left, right])

    acc = [stab | acc]

    merge_stab_clause_comments(rest, block_start, block_end, comments, acc)
  end

  defp merge_stab_clause_comments([], _, _, comments, acc), do: {Enum.reverse(acc), comments}

  defp merge_block_comments({:__block__, _, args} = block, block_start, block_end, comments) do
    {last_arg, args} = List.pop_at(args, -1)

    case last_arg do
      nil ->
        {trailing_comments, comments} =
          split_trailing_comments(comments, block_start, block_end)

        trailing_comments = Enum.sort_by(trailing_comments, & &1.line)

        block = append_comments(block, :inner_comments, trailing_comments)

        {block, comments}

      last_arg when not is_list(last_arg) ->
        {last_arg, comments} =
          merge_comments_to_last_arg(last_arg, block_start, block_end, comments)

        args = args ++ [last_arg]
        block = put_args(block, args)

        {block, comments}

      inner_list when is_list(inner_list) ->
        {inner_list, comments} =
          merge_comments_to_last_arg(inner_list, block_start, block_end, comments)

        args = args ++ [inner_list]
        block = put_args(block, args)

        {block, comments}

      _ ->
        {block, comments}
    end
  end

  defp merge_comments_to_last_arg(last_arg, block_start, block_end, comments) do
    line =
      case last_arg do
        [] -> block_start
        [{_key, value} | _] -> get_end_line(value, get_line(value))
        [first | _] -> get_end_line(first, get_line(first))
        {_, _, _} -> get_end_line(last_arg, get_line(last_arg))
        _ -> block_start
      end


    {trailing_comments, comments} =
      split_trailing_comments(comments, line, block_end)

    last_arg = append_comments(last_arg, :trailing_comments, trailing_comments)

    {last_arg, comments}
  end

  # =======

  defp put_comments(quoted, key, comments) do
    Macro.update_meta(quoted, &Keyword.put(&1, key, comments))
  end

  defp append_comments(quoted, key, comments) do
    Macro.update_meta(quoted, fn meta ->
      Keyword.update(meta, key, comments, &(&1 ++ comments))
    end)
  end

  defp get_meta({_, meta, _}) when is_list(meta), do: meta

  defp get_line({_, meta, _}, default \\ 1)
       when is_list(meta) and (is_integer(default) or is_nil(default)) do
    Keyword.get(meta, :line, default)
  end

  defp get_end_line(quoted, default) when is_integer(default) do
    get_end_position(quoted, line: default, column: 1)[:line]
  end

  defp get_end_position(quoted, default) do
    {_, position} =
      Macro.postwalk(quoted, default, fn
        {_, _, _} = quoted, end_position ->
          current_end_position = get_node_end_position(quoted, default)

          end_position =
            if compare_positions(end_position, current_end_position) == :gt do
              end_position
            else
              current_end_position
            end

          {quoted, end_position}

        terminal, end_position ->
          {terminal, end_position}
      end)

    position
  end

  defp get_node_end_position(quoted, default) do
    meta = get_meta(quoted)

    start_position = [
      line: meta[:line] || default[:line],
      column: meta[:column] || default[:column]
    ]

    get_meta(quoted)
    |> Keyword.take(@end_fields)
    |> Keyword.values()
    |> Enum.map(fn end_field ->
      position = Keyword.take(end_field, [:line, :column])

      # If the node contains newlines, a newline is included in the
      # column count. We subtract it so that the column represents the
      # last non-whitespace character.
      if Keyword.has_key?(end_field, :newlines) do
        Keyword.update(position, :column, nil, &(&1 - 1))
      else
        position
      end
    end)
    |> Enum.concat([start_position])
    |> Enum.max_by(
      & &1,
      fn prev, next ->
        compare_positions(prev, next) == :gt
      end,
      fn -> default end
    )
  end

  defp compare_positions(left, right) do
    left = coalesce_position(left)
    right = coalesce_position(right)

    cond do
      left == right ->
        :eq

      left[:line] > right[:line] ->
        :gt

      left[:line] == right[:line] and left[:column] > right[:column] ->
        :gt

      true ->
        :lt
    end
  end

  defp coalesce_position(position) do
    line = position[:line] || 0
    column = position[:column] || 0

    [line: line, column: column]
  end

  defp put_args({form, meta, _args}, args) do
    {form, meta, args}
  end

  defp interpolated?(entries) do
    Enum.all?(entries, fn
      {:"::", _, [{{:., _, [Kernel, :to_string]}, _, [_]}, {:binary, _, _}]} -> true
      entry when is_binary(entry) -> true
      _ -> false
    end)
  end

  defp split_leading_comments(comments, min, max) do
    Enum.split_with(comments, &(&1.line > min and &1.line <= max))
  end

  defp split_trailing_comments(comments, min, max) do
    Enum.split_with(comments, &(&1.line > min and &1.line < max))
  end
end
