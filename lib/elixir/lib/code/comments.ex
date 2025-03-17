defmodule Code.Comments do
  @moduledoc false

  @end_fields [:end, :closing, :end_of_expression]
  @block_names [:do, :else, :catch, :rescue, :after]
  @arrow_ops [:|>, :<<<, :>>>, :<~, :~>, :<<~, :~>>, :<~>, :"<|>", :->]

  defguardp is_arrow_op(op) when is_atom(op) and op in @arrow_ops

  @doc """
  Merges the comments into the given quoted expression.

  There are three types of comments:
  - `:leading_comments`: Comments that are located before a node,
    or in the same line.

    Examples:

        # This is a leading comment
        foo # This one too

  - `:trailing_comments`: Comments that are located after a node, and
    before the end of the parent enclosing the node(or the root document).

    Examples:

        foo
        # This is a trailing comment
        # This one too

  - `:inner_comments`: Comments that are located inside an empty node.

    Examples:

        foo do
          # This is an inner comment
        end

        [
          # This is an inner comment
        ]

        %{
          # This is an inner comment
        }

  A comment may be considered inner or trailing depending on wether the enclosing
  node is empty or not. For example, in the following code:

      foo do
        # This is an inner comment
      end

  The comment is considered inner because the `do` block is empty. However, in the
  following code:

      foo do
        bar
        # This is a trailing comment
      end

  The comment is considered trailing to `bar` because the `do` block is not empty.

  In the case no nodes are present in the AST but there are comments, they are
  inserted into a placeholder `:__block__` node as `:inner_comments`.
  """
  @spec merge_comments(Macro.t(), list(map)) :: Macro.t()
  def merge_comments({:__block__, _, []} = empty_ast, comments) do
    comments = Enum.sort_by(comments, & &1.line)
    put_comments(empty_ast, :inner_comments, comments)
  end
  def merge_comments(quoted, comments) do
    comments = Enum.sort_by(comments, & &1.line)

    state = %{
      comments: comments,
      parent_doend_meta: []
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
    {quoted, state} = merge_trailing_comments(quoted, state)
    merge_leading_comments(quoted, state)
  end

  defp do_merge_comments(quoted, state) do
    {quoted, state}
  end

  defp merge_leading_comments(quoted, state) do
    # If a comment is placed on top of a pipeline or binary operator line,
    # we should not merge it with the operator itself. Instead, we should
    # merge it with the first argument of the pipeline.
    #
    # This avoids the comment being moved up when formatting the code.
    with {form, _, _} <- quoted,
         false <- is_arrow_op(form),
         :error <- Code.Identifier.binary_op(form) do
      {comments, rest} = gather_leading_comments_for_node(quoted, state.comments)
      comments = Enum.sort_by(comments, & &1.line)

      quoted = put_comments(quoted, :leading_comments, comments)
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

    {comments, rest}
  end

  # Structs
  defp merge_trailing_comments({:%, _, [name, args]} = quoted, state) do
    {args, comments} = merge_trailing_comments(args, state.comments)

    quoted = put_args(quoted, [name, args])

    {quoted, %{state | comments: comments}}
  end

  # Maps
  defp merge_trailing_comments({:%{}, _, [{_key, _value} | _] = args} = quoted, %{comments: comments} = state) do
    case List.pop_at(args, -1) do 
      {{last_key, last_value}, args} -> 
        start_line = get_line(last_value)
        end_line = get_end_line(quoted, start_line)

        {trailing_comments, comments} =
          Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

        last_value = append_comments(last_value, :trailing_comments, trailing_comments)

        args = args ++ [{last_key, last_value}]

        quoted = put_args(quoted, args)
        {quoted, %{state | comments: comments}}

      {{:unquote_splicing, _, _} = unquote_splicing, other} ->
        start_line = get_line(unquote_splicing)
        end_line = get_end_line(quoted, start_line)

        {trailing_comments, comments} =
          Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

        unquote_splicing = append_comments(unquote_splicing, :trailing_comments, trailing_comments)

        args = other ++ [unquote_splicing]
        quoted = put_args(quoted, args)

        {quoted, %{state | comments: comments}}
    end
  end

  # Lists
  defp merge_trailing_comments({:__block__, _, [args]} = quoted, %{comments: comments} = state) when is_list(args) do
    {quoted, comments} =
      case List.pop_at(args, -1) do
        {nil, _} ->
          start_line = get_line(quoted)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

          quoted = append_comments(quoted, :inner_comments, trailing_comments)

          {quoted, comments}

        {{last_key, last_value}, args} -> 
          start_line = get_line(last_value)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

          last_value = append_comments(last_value, :trailing_comments, trailing_comments)

          args = args ++ [{last_key, last_value}]

          quoted = put_args(quoted, [args])
          {quoted, comments}
        {{:unquote_splicing, _, _} = unquote_splicing, other} ->
          start_line = get_line(unquote_splicing)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

          unquote_splicing = append_comments(unquote_splicing, :trailing_comments, trailing_comments)

          args = other ++ [unquote_splicing]
          quoted = put_args(quoted, [args])

          {quoted, comments}

        {{_, _, _} = value, args} ->
          start_line = get_line(value)
          end_line = get_end_line(quoted, start_line)

          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

          value = append_comments(value, :trailing_comments, trailing_comments)

          args = args ++ [value]
          quoted = put_args(quoted, [args])

          {quoted, comments}

        _ ->
          {quoted, comments}
      end

    {quoted, %{state | parent_doend_meta: [], comments: comments}}
  end


  # 2-tuples
  defp merge_trailing_comments({:__block__, _, [{left, right}]} = quoted, %{comments: comments} = state) when is_tuple(left) and is_tuple(right) do
    start_line = get_line(right)
    end_line = get_end_line(quoted, start_line)

    {trailing_comments, comments} =
      Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

    right = append_comments(right, :trailing_comments, trailing_comments)

    quoted = put_args(quoted, [{left, right}])
    {quoted, %{state | comments: comments}}
  end

  # Stabs
  defp merge_trailing_comments({:->, _, [left, right]} = quoted, state) do
    start_line = get_line(right)
    end_line = get_end_line({:__block__, state.parent_doend_meta, [quoted]}, start_line)

    {right, comments} =
      case right do
        {:__block__, _, _} ->
          merge_block_trailing_comments(right, start_line, end_line, state.comments)

        call ->
          line = get_line(call)
          {trailing_comments, comments} =
            Enum.split_with(state.comments, & &1.line > line and &1.line < end_line)

          call = append_comments(call, :trailing_comments, trailing_comments)

          {call, comments}
      end

    quoted = put_args(quoted, [left, right])

    {quoted, %{state | comments: comments}}
  end

  # Calls
  defp merge_trailing_comments({_, meta, args} = quoted, %{comments: comments} = state) when is_list(args) and meta != [] do
    start_line = get_line(quoted)
    end_line = get_end_line(quoted, start_line)
    {last_arg, args} = List.pop_at(args, -1)

    meta_keys = Keyword.keys(meta)

    state =
      if Enum.any?([:do, :closing], &(&1 in meta_keys)) do
        %{state | parent_doend_meta: meta}
      else
        state
      end

    {quoted, comments} =
        case last_arg do
        [{{:__block__, _, [name]}, _block_args} | _] = blocks when name in @block_names ->
          {reversed_blocks, comments} = each_merge_named_block_trailing_comments(blocks, quoted, comments, [])

          last_arg = Enum.reverse(reversed_blocks)

          args = args ++ [last_arg]
          quoted = put_args(quoted, args)

          {quoted, comments}

        [{_key, _value} | _] = pairs ->
          # Partial keyword list
          {{last_key, last_value}, pairs} = List.pop_at(pairs, -1)
          line = get_line(last_value)

          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > line and &1.line < end_line)

          last_value = append_comments(last_value, :trailing_comments, trailing_comments)

          pairs = pairs ++ [{last_key, last_value}]

          args = args ++ [pairs]

          quoted = put_args(quoted, args)

          {quoted, comments}

        {form, _, _} when form != :-> ->
          line = get_line(last_arg)
          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > line and &1.line < end_line)

          last_arg = append_comments(last_arg, :trailing_comments, trailing_comments)

          args = args ++ [last_arg]
          quoted = put_args(quoted, args)

          {quoted, comments}
        nil ->
          {trailing_comments, comments} =
            Enum.split_with(comments, & &1.line > start_line and &1.line < end_line)

          quoted = append_comments(quoted, :inner_comments, trailing_comments)
          {quoted, comments}

        _ ->
          {quoted, comments}
      end

    {quoted, %{state | comments: comments}}
  end

  defp merge_trailing_comments(quoted, state) do
    {quoted, state}
  end

  defp each_merge_named_block_trailing_comments([], _, comments, acc), do: {acc, comments}

  defp each_merge_named_block_trailing_comments([{block, block_args} | rest], parent, comments, acc) do
    block_start = get_line(block)
    block_end =
      case rest do
        [{next_block, _} | _] ->
          get_line(next_block)
        [] ->
          get_end_line(parent, 0)
      end

    {block, block_args, comments} = merge_named_block_trailing_comments(block, block_args, block_start, block_end, comments)

    acc = [{block, block_args} | acc]

    each_merge_named_block_trailing_comments(rest, parent, comments, acc)
  end

  defp merge_named_block_trailing_comments(block, {_, _, args} = block_args, block_start, block_end, comments) when is_list(args) do
    {last_arg, args} = List.pop_at(args, -1)

    case last_arg do
      nil ->
        {trailing_comments, comments} =
          Enum.split_with(comments, & &1.line > block_start and &1.line < block_end)

        block_args = append_comments(block_args, :inner_comments, trailing_comments)

      {block, block_args, comments}

      last_arg when not is_list(last_arg) ->
        {last_arg, comments} =
          merge_trailing_comments_to_last_arg(last_arg, block_start, block_end, comments)

        args = args ++ [last_arg]
        block_args = put_args(block_args, args)

        {block, block_args, comments}

      _ ->
        {block, block_args, comments}
    end
  end

  # If a do/end block has a single argument, it will not be wrapped in a `:__block__` node,
  # so we need to check for that.
  defp merge_named_block_trailing_comments(block, {_, _, ctx} = single_arg, block_start, block_end, comments) when not is_list(ctx) do
    {last_arg, comments} =
      merge_trailing_comments_to_last_arg(single_arg, block_start, block_end, comments)

    {block, last_arg, comments}
  end

  defp merge_named_block_trailing_comments(block, block_args, _, _, comments),
    do: {block, block_args, comments}

  defp merge_block_trailing_comments({:__block__, _, args} = block, block_start, block_end, comments) do
    {last_arg, args} = List.pop_at(args, -1)

    case last_arg do
      nil ->
        {trailing_comments, comments} =
          Enum.split_with(comments, & &1.line > block_start and &1.line < block_end)

        trailing_comments = Enum.sort_by(trailing_comments, & &1.line)

        block = append_comments(block, :inner_comments, trailing_comments)

      {block, comments}

      last_arg when not is_list(last_arg) ->
        {last_arg, comments} =
          merge_trailing_comments_to_last_arg(last_arg, block_start, block_end, comments)

        args = args ++ [last_arg]
        block = put_args(block, args)

        {block, comments}

      inner_list when is_list(inner_list) ->
        {inner_list, comments} =
          merge_trailing_comments_to_last_arg(inner_list, block_start, block_end, comments)

        args = args ++ [inner_list]
        block = put_args(block, args)

        {block, comments}

      _ ->
        {block, comments}
    end
  end

  defp merge_trailing_comments_to_last_arg(last_arg, block_start, block_end, comments) do
    line = 
          case last_arg do
            [] -> block_start
            [first | _] -> get_line(first)
            {_, _, _} -> get_line(last_arg)
            _ -> block_start
          end

    {trailing_comments, comments} =
      Enum.split_with(comments, & &1.line > line and &1.line < block_end)

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
end
