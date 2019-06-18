defmodule Code.Formatter do
  @moduledoc false
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

  @double_quote "\""
  @double_heredoc "\"\"\""
  @single_quote "'"
  @single_heredoc "'''"
  @newlines 2
  @min_line 0
  @max_line 9_999_999
  @empty empty()
  @ampersand_prec Code.Identifier.unary_op(:&) |> elem(1)

  # Operators that do not have space between operands
  @no_space_binary_operators [:..]

  # Operators that do not have newline between operands (as well as => and keywords)
  @no_newline_binary_operators [:\\, :in]

  # Left associative operators that start on the next line in case of breaks (always pipes)
  @pipeline_operators [:|>, :~>>, :<<~, :~>, :<~, :<~>, :<|>]

  # Right associative operators that start on the next line in case of breaks
  @right_new_line_before_binary_operators [:|, :when]

  # Operators that are logical cannot be mixed without parens
  @required_parens_logical_binary_operands [:||, :|||, :or, :&&, :&&&, :and]

  # Operators with next break fits. = and :: do not consider new lines though
  @next_break_fits_operators [:<-, :==, :!=, :=~, :===, :!==, :<, :>, :<=, :>=, :=, :"::"]

  # Operators that always require parens on operands when they are the parent
  @required_parens_on_binary_operands [
    :|>,
    :<<<,
    :>>>,
    :<~,
    :~>,
    :<<~,
    :~>>,
    :<~>,
    :<|>,
    :^^^,
    :in,
    :++,
    :--,
    :..,
    :<>
  ]

  @locals_without_parens [
    # Special forms
    alias: 1,
    alias: 2,
    case: 2,
    cond: 1,
    for: :*,
    import: 1,
    import: 2,
    quote: 1,
    quote: 2,
    receive: 1,
    require: 1,
    require: 2,
    try: 1,
    with: :*,

    # Kernel
    def: 1,
    def: 2,
    defp: 1,
    defp: 2,
    defguard: 1,
    defguardp: 1,
    defmacro: 1,
    defmacro: 2,
    defmacrop: 1,
    defmacrop: 2,
    defmodule: 2,
    defdelegate: 2,
    defexception: 1,
    defoverridable: 1,
    defstruct: 1,
    destructure: 2,
    raise: 1,
    raise: 2,
    reraise: 2,
    reraise: 3,
    if: 2,
    unless: 2,
    use: 1,
    use: 2,

    # Stdlib,
    defrecord: 2,
    defrecord: 3,
    defrecordp: 2,
    defrecordp: 3,

    # Testing
    assert: 1,
    assert: 2,
    assert_in_delta: 3,
    assert_in_delta: 4,
    assert_raise: 2,
    assert_raise: 3,
    assert_receive: 1,
    assert_receive: 2,
    assert_receive: 3,
    assert_received: 1,
    assert_received: 2,
    doctest: 1,
    doctest: 2,
    refute: 1,
    refute: 2,
    refute_in_delta: 3,
    refute_in_delta: 4,
    refute_receive: 1,
    refute_receive: 2,
    refute_receive: 3,
    refute_received: 1,
    refute_received: 2,
    setup: 1,
    setup: 2,
    setup_all: 1,
    setup_all: 2,
    test: 1,
    test: 2,

    # Mix config
    config: 2,
    config: 3,
    import_config: 1
  ]

  @do_end_keywords [:rescue, :catch, :else, :after]

  @doc """
  Checks if two strings are equivalent.
  """
  def equivalent(string1, string2) when is_binary(string1) and is_binary(string2) do
    quoted1 = :elixir.string_to_quoted!(to_charlist(string1), 1, "nofile", [])
    quoted2 = :elixir.string_to_quoted!(to_charlist(string2), 1, "nofile", [])

    case not_equivalent(quoted1, quoted2) do
      {left, right} -> {:error, left, right}
      nil -> :ok
    end
  end

  defp not_equivalent({:__block__, _, [left]}, right) do
    not_equivalent(left, right)
  end

  defp not_equivalent(left, {:__block__, _, [right]}) do
    not_equivalent(left, right)
  end

  defp not_equivalent({:__block__, _, []}, nil) do
    nil
  end

  defp not_equivalent(nil, {:__block__, _, []}) do
    nil
  end

  defp not_equivalent([left | lefties], [right | righties]) do
    not_equivalent(left, right) || not_equivalent(lefties, righties)
  end

  defp not_equivalent({left_name, _, left_args}, {right_name, _, right_args}) do
    not_equivalent(left_name, right_name) || not_equivalent(left_args, right_args)
  end

  defp not_equivalent({left1, left2}, {right1, right2}) do
    not_equivalent(left1, right1) || not_equivalent(left2, right2)
  end

  defp not_equivalent(side, side) do
    nil
  end

  defp not_equivalent(left, right) do
    {left, right}
  end

  @doc """
  Converts `string` to an algebra document.

  Returns `{:ok, doc}` or `{:error, parser_error}`.

  See `Code.format_string!/2` for the list of options.
  """
  def to_algebra(string, opts \\ []) when is_binary(string) and is_list(opts) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    charlist = String.to_charlist(string)

    Process.put(:code_formatter_comments, [])

    tokenizer_options = [
      unescape: false,
      preserve_comments: &preserve_comments/5,
      warn_on_unnecessary_quotes: false
    ]

    parser_options = [
      literal_encoder: &{:ok, {:__block__, &2, [&1]}},
      token_metadata: true
    ]

    with {:ok, tokens} <- :elixir.string_to_tokens(charlist, line, file, tokenizer_options),
         {:ok, forms} <- :elixir.tokens_to_quoted(tokens, file, parser_options) do
      state =
        Process.get(:code_formatter_comments)
        |> Enum.reverse()
        |> gather_comments()
        |> state(opts)

      {doc, _} = block_to_algebra(forms, @min_line, @max_line, state)
      {:ok, doc}
    end
  after
    Process.delete(:code_formatter_comments)
  end

  @doc """
  Converts `string` to an algebra document.

  Raises if the `string` cannot be parsed.

  See `Code.format_string!/2` for the list of options.
  """
  def to_algebra!(string, opts \\ []) do
    case to_algebra(string, opts) do
      {:ok, doc} ->
        doc

      {:error, {line, error, token}} ->
        :elixir_errors.parse_error(line, Keyword.get(opts, :file, "nofile"), error, token)
    end
  end

  defp state(comments, opts) do
    force_do_end_blocks = Keyword.get(opts, :force_do_end_blocks, false)

    rename_deprecated_at =
      if version = opts[:rename_deprecated_at] do
        case Version.parse(version) do
          {:ok, parsed} ->
            parsed

          :error ->
            raise ArgumentError,
                  "invalid version #{inspect(version)} given to :rename_deprecated_at"
        end
      end

    locals_without_parens =
      Keyword.get(opts, :locals_without_parens, []) ++ @locals_without_parens

    %{
      force_do_end_blocks: force_do_end_blocks,
      locals_without_parens: locals_without_parens,
      operand_nesting: 2,
      rename_deprecated_at: rename_deprecated_at,
      comments: comments
    }
  end

  # Code comment handling

  defp preserve_comments(line, _column, tokens, comment, rest) do
    comments = Process.get(:code_formatter_comments)
    comment = {line, {previous_eol(tokens), next_eol(rest, 0)}, format_comment(comment, [])}
    Process.put(:code_formatter_comments, [comment | comments])
  end

  defp next_eol('\s' ++ rest, count), do: next_eol(rest, count)
  defp next_eol('\t' ++ rest, count), do: next_eol(rest, count)
  defp next_eol('\n' ++ rest, count), do: next_eol(rest, count + 1)
  defp next_eol('\r\n' ++ rest, count), do: next_eol(rest, count + 1)
  defp next_eol(_, count), do: count

  defp previous_eol([{token, {_, _, count}} | _])
       when token in [:eol, :",", :";"] and count > 0 do
    count
  end

  defp previous_eol([]), do: 1
  defp previous_eol(_), do: nil

  defp format_comment('##' ++ rest, acc), do: format_comment([?# | rest], [?# | acc])

  defp format_comment('#!', acc), do: reverse_to_string(acc, '#!')
  defp format_comment('#! ' ++ _ = rest, acc), do: reverse_to_string(acc, rest)
  defp format_comment('#!' ++ rest, acc), do: reverse_to_string(acc, [?#, ?!, ?\s, rest])

  defp format_comment('#', acc), do: reverse_to_string(acc, '#')
  defp format_comment('# ' ++ _ = rest, acc), do: reverse_to_string(acc, rest)
  defp format_comment('#' ++ rest, acc), do: reverse_to_string(acc, [?#, ?\s, rest])

  defp reverse_to_string(acc, prefix) do
    acc |> Enum.reverse(prefix) |> List.to_string()
  end

  # If there is a no new line before, we can't gather all followup comments.
  defp gather_comments([{line, {nil, next_eol}, doc} | comments]) do
    comment = {line, {@newlines, next_eol}, doc}
    [comment | gather_comments(comments)]
  end

  defp gather_comments([{line, {previous_eol, next_eol}, doc} | comments]) do
    {next_eol, comments, doc} = gather_followup_comments(line + 1, next_eol, comments, doc)
    comment = {line, {previous_eol, next_eol}, doc}
    [comment | gather_comments(comments)]
  end

  defp gather_comments([]) do
    []
  end

  defp gather_followup_comments(line, _, [{line, {previous_eol, next_eol}, text} | comments], doc)
       when previous_eol != nil do
    gather_followup_comments(line + 1, next_eol, comments, line(doc, text))
  end

  defp gather_followup_comments(_line, next_eol, comments, doc) do
    {next_eol, comments, doc}
  end

  # Special AST nodes from compiler feedback.

  defp quoted_to_algebra({{:special, :clause_args}, _meta, [args]}, _context, state) do
    {doc, state} = clause_args_to_algebra(args, state)
    {group(doc), state}
  end

  defp quoted_to_algebra({{:special, :bitstring_segment}, _meta, [arg, last]}, _context, state) do
    bitstring_segment_to_algebra({arg, -1}, state, last)
  end

  defp quoted_to_algebra({var, _meta, var_context}, _context, state) when is_atom(var_context) do
    {var |> Atom.to_string() |> string(), state}
  end

  defp quoted_to_algebra({:<<>>, meta, entries}, _context, state) do
    cond do
      entries == [] ->
        {"<<>>", state}

      not interpolated?(entries) ->
        bitstring_to_algebra(meta, entries, state)

      meta[:delimiter] == ~s["""] ->
        {doc, state} =
          entries
          |> prepend_heredoc_line()
          |> interpolation_to_algebra(:heredoc, state, @double_heredoc, @double_heredoc)

        {force_unfit(doc), state}

      true ->
        interpolation_to_algebra(entries, @double_quote, state, @double_quote, @double_quote)
    end
  end

  defp quoted_to_algebra(
         {{:., _, [List, :to_charlist]}, meta, [entries]} = quoted,
         context,
         state
       ) do
    cond do
      not list_interpolated?(entries) ->
        remote_to_algebra(quoted, context, state)

      meta[:delimiter] == ~s['''] ->
        {doc, state} =
          entries
          |> prepend_heredoc_line()
          |> list_interpolation_to_algebra(:heredoc, state, @single_heredoc, @single_heredoc)

        {force_unfit(doc), state}

      true ->
        list_interpolation_to_algebra(entries, @single_quote, state, @single_quote, @single_quote)
    end
  end

  defp quoted_to_algebra(
         {{:., _, [:erlang, :binary_to_atom]}, _, [{:<<>>, _, entries}, :utf8]} = quoted,
         context,
         state
       ) do
    if interpolated?(entries) do
      interpolation_to_algebra(entries, @double_quote, state, ":\"", @double_quote)
    else
      remote_to_algebra(quoted, context, state)
    end
  end

  # foo[bar]
  defp quoted_to_algebra({{:., _, [Access, :get]}, meta, [target | args]}, _context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {call_doc, state} = list_to_algebra(meta, args, state)
    {concat(target_doc, call_doc), state}
  end

  # %Foo{}
  # %name{foo: 1}
  # %name{bar | foo: 1}
  defp quoted_to_algebra({:%, _, [name, {:%{}, meta, args}]}, _context, state) do
    {name_doc, state} = quoted_to_algebra(name, :parens_arg, state)
    map_to_algebra(meta, name_doc, args, state)
  end

  # %{foo: 1}
  # %{foo => bar}
  # %{name | foo => bar}
  defp quoted_to_algebra({:%{}, meta, args}, _context, state) do
    map_to_algebra(meta, @empty, args, state)
  end

  # {}
  # {1, 2}
  defp quoted_to_algebra({:{}, meta, args}, _context, state) do
    tuple_to_algebra(meta, args, :flex_break, state)
  end

  defp quoted_to_algebra({:__block__, meta, [{left, right}]}, _context, state) do
    tuple_to_algebra(meta, [left, right], :flex_break, state)
  end

  defp quoted_to_algebra({:__block__, meta, [list]}, _context, state) when is_list(list) do
    case meta[:delimiter] do
      ~s['''] ->
        string = list |> List.to_string() |> escape_heredoc()
        {@single_heredoc |> concat(string) |> concat(@single_heredoc) |> force_unfit(), state}

      ~s['] ->
        string = list |> List.to_string() |> escape_string(@single_quote)
        {@single_quote |> concat(string) |> concat(@single_quote), state}

      _other ->
        list_to_algebra(meta, list, state)
    end
  end

  defp quoted_to_algebra({:__block__, meta, [string]}, _context, state) when is_binary(string) do
    if meta[:delimiter] == ~s["""] do
      string = escape_heredoc(string)
      {@double_heredoc |> concat(string) |> concat(@double_heredoc) |> force_unfit(), state}
    else
      string = escape_string(string, @double_quote)
      {@double_quote |> concat(string) |> concat(@double_quote), state}
    end
  end

  defp quoted_to_algebra({:__block__, _, [atom]}, _context, state) when is_atom(atom) do
    {atom_to_algebra(atom), state}
  end

  defp quoted_to_algebra({:__block__, meta, [integer]}, _context, state)
       when is_integer(integer) do
    {integer_to_algebra(Keyword.fetch!(meta, :token)), state}
  end

  defp quoted_to_algebra({:__block__, meta, [float]}, _context, state) when is_float(float) do
    {float_to_algebra(Keyword.fetch!(meta, :token)), state}
  end

  defp quoted_to_algebra(
         {:__block__, _meta, [{:unquote_splicing, meta, [_] = args}]},
         context,
         state
       ) do
    {doc, state} = local_to_algebra(:unquote_splicing, meta, args, context, state)
    {wrap_in_parens(doc), state}
  end

  defp quoted_to_algebra({:__block__, _meta, [arg]}, context, state) do
    quoted_to_algebra(arg, context, state)
  end

  defp quoted_to_algebra({:__block__, meta, _} = block, _context, state) do
    {block, state} = block_to_algebra(block, line(meta), closing_line(meta), state)
    {surround("(", block, ")"), state}
  end

  defp quoted_to_algebra({:__aliases__, _meta, [head | tail]}, context, state) do
    {doc, state} =
      if is_atom(head) do
        {Atom.to_string(head), state}
      else
        quoted_to_algebra_with_parens_if_operator(head, context, state)
      end

    {Enum.reduce(tail, doc, &concat(&2, "." <> Atom.to_string(&1))), state}
  end

  # &1
  # &local(&1)
  # &local/1
  # &Mod.remote/1
  # & &1
  # & &1 + &2
  defp quoted_to_algebra({:&, _, [arg]}, context, state) do
    capture_to_algebra(arg, context, state)
  end

  defp quoted_to_algebra({:@, meta, [arg]}, context, state) do
    module_attribute_to_algebra(meta, arg, context, state)
  end

  # not(left in right)
  # left not in right
  defp quoted_to_algebra({:not, meta, [{:in, _, [left, right]} = arg]}, context, state) do
    %{rename_deprecated_at: since} = state

    # TODO: Remove metadata and always rewrite to left not in right in Elixir v2.0.
    if meta[:operator] == :"not in" || (since && Version.match?(since, "~> 1.5")) do
      binary_op_to_algebra(:in, "not in", meta, left, right, context, state)
    else
      unary_op_to_algebra(:not, meta, arg, context, state)
    end
  end

  defp quoted_to_algebra({:fn, meta, [_ | _] = clauses}, _context, state) do
    anon_fun_to_algebra(clauses, line(meta), closing_line(meta), state, eol?(meta))
  end

  defp quoted_to_algebra({fun, meta, args}, context, state) when is_atom(fun) and is_list(args) do
    with :error <- maybe_sigil_to_algebra(fun, meta, args, state),
         :error <- maybe_unary_op_to_algebra(fun, meta, args, context, state),
         :error <- maybe_binary_op_to_algebra(fun, meta, args, context, state),
         do: local_to_algebra(fun, meta, args, context, state)
  end

  defp quoted_to_algebra({_, _, args} = quoted, context, state) when is_list(args) do
    remote_to_algebra(quoted, context, state)
  end

  # (left -> right)
  defp quoted_to_algebra([{:->, _, _} | _] = clauses, _context, state) do
    type_fun_to_algebra(clauses, @max_line, @min_line, state)
  end

  # [keyword: :list] (inner part)
  # %{:foo => :bar} (inner part)
  defp quoted_to_algebra(list, context, state) when is_list(list) do
    many_args_to_algebra(list, state, &quoted_to_algebra(&1, context, &2))
  end

  # keyword: :list
  # key => value
  defp quoted_to_algebra({left_arg, right_arg}, context, state) do
    {left, op, right, state} =
      if keyword_key?(left_arg) do
        {left, state} =
          case left_arg do
            {:__block__, _, [atom]} when is_atom(atom) ->
              key =
                case Code.Identifier.classify(atom) do
                  type when type in [:callable_local, :callable_operator, :not_callable] ->
                    IO.iodata_to_binary([Atom.to_string(atom), ?:])

                  _ ->
                    IO.iodata_to_binary([?", Atom.to_string(atom), ?", ?:])
                end

              {string(key), state}

            {{:., _, [:erlang, :binary_to_atom]}, _, [{:<<>>, _, entries}, :utf8]} ->
              interpolation_to_algebra(entries, @double_quote, state, "\"", "\":")
          end

        {right, state} = quoted_to_algebra(right_arg, context, state)
        {left, "", right, state}
      else
        {left, state} = quoted_to_algebra(left_arg, context, state)
        {right, state} = quoted_to_algebra(right_arg, context, state)
        left = wrap_in_parens_if_binary_operator(left, left_arg)
        {left, " =>", right, state}
      end

    doc =
      with_next_break_fits(next_break_fits?(right_arg, state), right, fn right ->
        concat(group(left), group(nest(glue(op, group(right)), 2, :break)))
      end)

    {doc, state}
  end

  ## Blocks

  defp block_to_algebra([{:->, _, _} | _] = type_fun, min_line, max_line, state) do
    type_fun_to_algebra(type_fun, min_line, max_line, state)
  end

  defp block_to_algebra({:__block__, _, []}, min_line, max_line, state) do
    block_args_to_algebra([], min_line, max_line, state)
  end

  defp block_to_algebra({:__block__, _, [_, _ | _] = args}, min_line, max_line, state) do
    block_args_to_algebra(args, min_line, max_line, state)
  end

  defp block_to_algebra(block, min_line, max_line, state) do
    block_args_to_algebra([block], min_line, max_line, state)
  end

  defp block_args_to_algebra(args, min_line, max_line, state) do
    quoted_to_algebra = fn {kind, meta, _} = arg, _args, state ->
      newlines = meta[:end_of_expression][:newlines] || 1
      {doc, state} = quoted_to_algebra(arg, :block, state)
      {{doc, block_next_line(kind), newlines}, state}
    end

    {args_docs, _comments?, state} =
      quoted_to_algebra_with_comments(args, [], min_line, max_line, state, quoted_to_algebra)

    case args_docs do
      [] -> {@empty, state}
      [line] -> {line, state}
      lines -> {lines |> Enum.reduce(&line(&2, &1)) |> force_unfit(), state}
    end
  end

  defp block_next_line(:@), do: @empty
  defp block_next_line(_), do: break("")

  ## Operators

  defp maybe_unary_op_to_algebra(fun, meta, args, context, state) do
    with [arg] <- args,
         {_, _} <- Code.Identifier.unary_op(fun) do
      unary_op_to_algebra(fun, meta, arg, context, state)
    else
      _ -> :error
    end
  end

  defp unary_op_to_algebra(op, _meta, arg, context, state) do
    {doc, state} = quoted_to_algebra(arg, force_many_args_or_operand(context, :operand), state)

    # not and ! are nestable, all others are not.
    doc =
      case arg do
        {^op, _, [_]} when op in [:!, :not] -> doc
        _ -> wrap_in_parens_if_operator(doc, arg)
      end

    # not requires a space unless the doc was wrapped in parens.
    op_string =
      if op == :not do
        "not "
      else
        Atom.to_string(op)
      end

    {concat(op_string, doc), state}
  end

  defp maybe_binary_op_to_algebra(fun, meta, args, context, state) do
    with [left, right] <- args,
         {_, _} <- Code.Identifier.binary_op(fun) do
      binary_op_to_algebra(fun, Atom.to_string(fun), meta, left, right, context, state)
    else
      _ -> :error
    end
  end

  # There are five kinds of operators.
  #
  #   1. no space binary operators, e.g. 1..2
  #   2. no newline binary operators, e.g. left in right
  #   3. strict newlines before a left precedent operator, e.g. foo |> bar |> baz
  #   4. strict newlines before a right precedent operator, e.g. foo when bar when baz
  #   5. flex newlines after the operator, e.g. foo ++ bar ++ baz
  #
  # Cases 1, 2 and 5 are handled fairly easily by relying on the
  # operator precedence and making sure nesting is applied only once.
  #
  # Cases 3 and 4 are the complex ones, as it requires passing the
  # strict or flex mode around.
  defp binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state) do
    %{operand_nesting: nesting} = state
    binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state, nesting)
  end

  defp binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state, _nesting)
       when op in @right_new_line_before_binary_operators do
    op_info = Code.Identifier.binary_op(op)
    op_string = op_string <> " "
    left_context = left_op_context(context)
    right_context = right_op_context(context)

    min_line =
      case left_arg do
        {_, left_meta, _} -> line(left_meta)
        _ -> line(meta)
      end

    {operands, max_line} =
      unwrap_right(right_arg, op, meta, right_context, [{{:root, left_context}, left_arg}])

    operand_to_algebra = fn
      {{:root, context}, arg}, _args, state ->
        {doc, state} = binary_operand_to_algebra(arg, context, state, op, op_info, :left, 2)
        {{doc, @empty, 1}, state}

      {{kind, context}, arg}, _args, state ->
        {doc, state} = binary_operand_to_algebra(arg, context, state, op, op_info, kind, 0)
        doc = doc |> nest_by_length(op_string) |> force_keyword(arg)
        {{concat(op_string, doc), @empty, 1}, state}
    end

    {doc, state} =
      operand_to_algebra_with_comments(
        operands,
        meta,
        min_line,
        max_line,
        state,
        operand_to_algebra
      )

    if keyword?(right_arg) and context in [:parens_arg, :no_parens_arg] do
      {wrap_in_parens(doc), state}
    else
      {doc, state}
    end
  end

  defp binary_op_to_algebra(op, _, meta, left_arg, right_arg, context, state, _nesting)
       when op in @pipeline_operators do
    op_info = Code.Identifier.binary_op(op)
    left_context = left_op_context(context)
    right_context = right_op_context(context)
    max_line = line(meta)

    {pipes, min_line} =
      unwrap_pipes(left_arg, meta, left_context, [{{op, right_context}, right_arg}])

    operand_to_algebra = fn
      {{:root, context}, arg}, _args, state ->
        {doc, state} = binary_operand_to_algebra(arg, context, state, op, op_info, :left, 2)
        {{doc, @empty, 1}, state}

      {{op, context}, arg}, _args, state ->
        op_info = Code.Identifier.binary_op(op)
        op_string = Atom.to_string(op) <> " "
        {doc, state} = binary_operand_to_algebra(arg, context, state, op, op_info, :right, 0)
        {{concat(op_string, doc), @empty, 1}, state}
    end

    operand_to_algebra_with_comments(pipes, meta, min_line, max_line, state, operand_to_algebra)
  end

  defp binary_op_to_algebra(op, op_string, meta, left_arg, right_arg, context, state, nesting) do
    op_info = Code.Identifier.binary_op(op)
    left_context = left_op_context(context)
    right_context = right_op_context(context)

    {left, state} =
      binary_operand_to_algebra(left_arg, left_context, state, op, op_info, :left, 2)

    {right, state} =
      binary_operand_to_algebra(right_arg, right_context, state, op, op_info, :right, 0)

    doc =
      cond do
        op in @no_space_binary_operators ->
          concat(concat(group(left), op_string), group(right))

        op in @no_newline_binary_operators ->
          op_string = " " <> op_string <> " "
          concat(concat(group(left), op_string), group(right))

        true ->
          eol? = eol?(meta)

          next_break_fits? =
            op in @next_break_fits_operators and next_break_fits?(right_arg, state) and not eol?

          with_next_break_fits(next_break_fits?, right, fn right ->
            op_string = " " <> op_string
            right = nest(glue(op_string, group(right)), nesting, :break)
            right = if eol?, do: force_unfit(right), else: right
            concat(group(left), group(right))
          end)
      end

    {doc, state}
  end

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser on v2.0.
  # (! left) in right
  # (not left) in right
  defp binary_operand_to_algebra(
         {:__block__, _, [{op, meta, [arg]}]},
         context,
         state,
         :in,
         _parent_info,
         :left,
         _nesting
       )
       when op in [:not, :!] do
    {doc, state} = unary_op_to_algebra(op, meta, arg, context, state)
    {wrap_in_parens(doc), state}
  end

  defp binary_operand_to_algebra(operand, context, state, parent_op, parent_info, side, nesting) do
    {parent_assoc, parent_prec} = parent_info

    with {op, meta, [left, right]} <- operand,
         op_info = Code.Identifier.binary_op(op),
         {_assoc, prec} <- op_info do
      op_string = Atom.to_string(op)

      cond do
        # If the operator has the same precedence as the parent and is on
        # the correct side, we respect the nesting rule to avoid multiple
        # nestings. This only applies for left associativity or same operator.
        parent_prec == prec and parent_assoc == side and (side == :left or op == parent_op) ->
          binary_op_to_algebra(op, op_string, meta, left, right, context, state, nesting)

        # If the parent requires parens or the precedence is inverted or
        # it is in the wrong side, then we *need* parenthesis.
        (parent_op in @required_parens_on_binary_operands and op not in @no_space_binary_operators) or
          (op in @required_parens_logical_binary_operands and
             parent_op in @required_parens_logical_binary_operands) or parent_prec > prec or
            (parent_prec == prec and parent_assoc != side) ->
          {operand, state} =
            binary_op_to_algebra(op, op_string, meta, left, right, context, state, 2)

          {wrap_in_parens(operand), state}

        # Otherwise, we rely on precedence but also nest.
        true ->
          binary_op_to_algebra(op, op_string, meta, left, right, context, state, 2)
      end
    else
      {:&, _, [arg]}
      when not is_integer(arg) and side == :left
      when not is_integer(arg) and parent_assoc == :left and parent_prec > @ampersand_prec ->
        {doc, state} = quoted_to_algebra(operand, context, state)
        {wrap_in_parens(doc), state}

      _ ->
        quoted_to_algebra(operand, context, state)
    end
  end

  defp unwrap_pipes({op, meta, [left, right]}, _meta, context, acc)
       when op in @pipeline_operators do
    left_context = left_op_context(context)
    right_context = right_op_context(context)
    unwrap_pipes(left, meta, left_context, [{{op, right_context}, right} | acc])
  end

  defp unwrap_pipes(left, meta, context, acc) do
    min_line =
      case left do
        {_, meta, _} -> line(meta)
        _ -> line(meta)
      end

    {[{{:root, context}, left} | acc], min_line}
  end

  defp unwrap_right({op, meta, [left, right]}, op, _meta, context, acc) do
    left_context = left_op_context(context)
    right_context = right_op_context(context)
    unwrap_right(right, op, meta, right_context, [{{:left, left_context}, left} | acc])
  end

  defp unwrap_right(right, _op, meta, context, acc) do
    acc = [{{:right, context}, right} | acc]
    {Enum.reverse(acc), line(meta)}
  end

  defp operand_to_algebra_with_comments(operands, meta, min_line, max_line, state, fun) do
    {docs, comments?, state} =
      quoted_to_algebra_with_comments(operands, [], min_line, max_line, state, fun)

    if comments? or eol?(meta) do
      {docs |> Enum.reduce(&line(&2, &1)) |> force_unfit(), state}
    else
      {docs |> Enum.reduce(&glue(&2, &1)), state}
    end
  end

  ## Module attributes

  # @Foo
  # @Foo.Bar
  defp module_attribute_to_algebra(_meta, {:__aliases__, _, [_, _ | _]} = quoted, _context, state) do
    {doc, state} = quoted_to_algebra(quoted, :parens_arg, state)
    {concat(concat("@(", doc), ")"), state}
  end

  # @foo bar
  # @foo(bar)
  defp module_attribute_to_algebra(meta, {name, call_meta, [_] = args} = expr, context, state)
       when is_atom(name) and name not in [:__block__, :__aliases__] do
    if Code.Identifier.classify(name) == :callable_local do
      {{call_doc, state}, wrap_in_parens?} =
        call_args_to_algebra(args, call_meta, context, :skip_unless_many_args, false, state)

      doc =
        "@#{name}"
        |> string()
        |> concat(call_doc)

      doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
      {doc, state}
    else
      unary_op_to_algebra(:@, meta, expr, context, state)
    end
  end

  # @foo
  # @(foo.bar())
  defp module_attribute_to_algebra(meta, quoted, context, state) do
    unary_op_to_algebra(:@, meta, quoted, context, state)
  end

  ## Capture operator

  defp capture_to_algebra(integer, _context, state) when is_integer(integer) do
    {"&" <> Integer.to_string(integer), state}
  end

  defp capture_to_algebra(arg, context, state) do
    {doc, state} = capture_target_to_algebra(arg, context, state)

    if doc |> format_to_string() |> String.starts_with?("&") do
      {concat("& ", doc), state}
    else
      {concat("&", doc), state}
    end
  end

  defp capture_target_to_algebra(
         {:/, _, [{{:., _, [target, fun]}, _, []}, {:__block__, _, [arity]}]},
         _context,
         state
       )
       when is_atom(fun) and is_integer(arity) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    fun = remote_fun_to_algebra(target, fun, arity, state)
    {target_doc |> nest(1) |> concat(string(".#{fun}/#{arity}")), state}
  end

  defp capture_target_to_algebra(
         {:/, _, [{name, _, var_context}, {:__block__, _, [arity]}]},
         _context,
         state
       )
       when is_atom(name) and is_atom(var_context) and is_integer(arity) do
    {string("#{name}/#{arity}"), state}
  end

  defp capture_target_to_algebra(arg, context, state) do
    {doc, state} = quoted_to_algebra(arg, context, state)
    {wrap_in_parens_if_operator(doc, arg), state}
  end

  ## Calls (local, remote and anonymous)

  # expression.{arguments}
  defp remote_to_algebra({{:., _, [target, :{}]}, meta, args}, _context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    {call_doc, state} = tuple_to_algebra(meta, args, :break, state)
    {concat(concat(target_doc, "."), call_doc), state}
  end

  # expression.(arguments)
  defp remote_to_algebra({{:., _, [target]}, meta, args}, context, state) do
    {target_doc, state} = remote_target_to_algebra(target, state)

    {{call_doc, state}, wrap_in_parens?} =
      call_args_to_algebra(args, meta, context, :skip_if_do_end, true, state)

    doc = concat(concat(target_doc, "."), call_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  # Mod.function()
  # var.function
  # expression.function(arguments)
  defp remote_to_algebra({{:., _, [target, fun]}, meta, args}, context, state)
       when is_atom(fun) do
    {target_doc, state} = remote_target_to_algebra(target, state)
    fun = remote_fun_to_algebra(target, fun, length(args), state)
    remote_doc = target_doc |> concat(".") |> concat(string(fun))

    if args == [] and not remote_target_is_a_module?(target) and not meta?(meta, :closing) do
      {remote_doc, state}
    else
      {{call_doc, state}, wrap_in_parens?} =
        call_args_to_algebra(args, meta, context, :skip_if_do_end, true, state)

      doc = concat(remote_doc, call_doc)
      doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
      {doc, state}
    end
  end

  # call(call)(arguments)
  defp remote_to_algebra({target, meta, args}, context, state) do
    {target_doc, state} = quoted_to_algebra(target, :no_parens_arg, state)

    {{call_doc, state}, wrap_in_parens?} =
      call_args_to_algebra(args, meta, context, :required, true, state)

    doc = concat(target_doc, call_doc)
    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  defp remote_target_is_a_module?(target) do
    case target do
      {:__MODULE__, _, context} when is_atom(context) -> true
      {:__block__, _, [atom]} when is_atom(atom) -> true
      {:__aliases__, _, _} -> true
      _ -> false
    end
  end

  defp remote_fun_to_algebra(target, fun, arity, state) do
    %{rename_deprecated_at: since} = state

    atom_target =
      case since && target do
        {:__aliases__, _, [alias | _] = aliases} when is_atom(alias) ->
          Module.concat(aliases)

        {:__block__, _, [atom]} when is_atom(atom) ->
          atom

        _ ->
          nil
      end

    with {fun, requirement} <- deprecated(atom_target, fun, arity),
         true <- Version.match?(since, requirement) do
      fun
    else
      _ -> Code.Identifier.inspect_as_function(fun)
    end
  end

  # We can only rename functions in the same module because
  # introducing a new module may be wrong due to aliases.
  defp deprecated(Enum, :partition, 2), do: {"split_with", "~> 1.4"}
  defp deprecated(Code, :unload_files, 2), do: {"unrequire_files", "~> 1.7"}
  defp deprecated(Code, :loaded_files, 2), do: {"required_files", "~> 1.7"}
  defp deprecated(Kernel.ParallelCompiler, :files, 2), do: {"compile", "~> 1.6"}
  defp deprecated(Kernel.ParallelCompiler, :files_to_path, 2), do: {"compile_to_path", "~> 1.6"}
  defp deprecated(_, _, _), do: :error

  defp remote_target_to_algebra({:fn, _, [_ | _]} = quoted, state) do
    # This change is not semantically required but for beautification.
    {doc, state} = quoted_to_algebra(quoted, :no_parens_arg, state)
    {wrap_in_parens(doc), state}
  end

  defp remote_target_to_algebra(quoted, state) do
    quoted_to_algebra_with_parens_if_operator(quoted, :no_parens_arg, state)
  end

  # function(arguments)
  defp local_to_algebra(fun, meta, args, context, state) when is_atom(fun) do
    skip_parens =
      cond do
        meta?(meta, :closing) -> :required
        local_without_parens?(fun, args, state) -> :skip_unless_many_args
        true -> :skip_if_do_end
      end

    {{call_doc, state}, wrap_in_parens?} =
      call_args_to_algebra(args, meta, context, skip_parens, true, state)

    doc =
      fun
      |> Atom.to_string()
      |> string()
      |> concat(call_doc)

    doc = if wrap_in_parens?, do: wrap_in_parens(doc), else: doc
    {doc, state}
  end

  # parens may one of:
  #
  #   * :skip_unless_many_args - skips parens unless we are the argument context
  #   * :skip_if_do_end - skip parens if we are do-end
  #   * :required - never skip parens
  #
  defp call_args_to_algebra([], meta, _context, _parens, _list_to_keyword?, state) do
    {args_doc, _join, state} =
      args_to_algebra_with_comments([], meta, false, :none, :break, state, &{&1, &2})

    {{surround("(", args_doc, ")"), state}, false}
  end

  defp call_args_to_algebra(args, meta, context, parens, list_to_keyword?, state) do
    {rest, last} = split_last(args)

    if blocks = do_end_blocks(meta, last, state) do
      {call_doc, state} =
        if rest == [] do
          {" do", state}
        else
          no_parens? = parens != :required
          call_args_to_algebra_no_blocks(meta, rest, no_parens?, list_to_keyword?, " do", state)
        end

      {blocks_doc, state} = do_end_blocks_to_algebra(blocks, state)
      call_doc = call_doc |> concat(blocks_doc) |> line("end") |> force_unfit()
      {{call_doc, state}, context in [:no_parens_arg, :no_parens_one_arg]}
    else
      no_parens? =
        parens == :skip_unless_many_args and
          context in [:block, :operand, :no_parens_one_arg, :parens_one_arg]

      res =
        call_args_to_algebra_no_blocks(meta, args, no_parens?, list_to_keyword?, @empty, state)

      {res, false}
    end
  end

  defp call_args_to_algebra_no_blocks(meta, args, skip_parens?, list_to_keyword?, extra, state) do
    {left, right} = split_last(args)
    {keyword?, right} = last_arg_to_keyword(right, list_to_keyword?)

    context =
      if left == [] and not keyword? do
        if skip_parens?, do: :no_parens_one_arg, else: :parens_one_arg
      else
        if skip_parens?, do: :no_parens_arg, else: :parens_arg
      end

    args = if keyword?, do: left ++ right, else: left ++ [right]
    many_eol? = match?([_, _ | _], args) and eol?(meta)
    no_generators? = no_generators?(args)
    to_algebra_fun = &quoted_to_algebra(&1, context, &2)

    {args_doc, next_break_fits?, state} =
      if left != [] and keyword? and no_generators? do
        join = if force_args?(left) or many_eol?, do: :line, else: :break

        {left_doc, _join, state} =
          args_to_algebra_with_comments(
            left,
            Keyword.delete(meta, :closing),
            skip_parens?,
            :force_comma,
            join,
            state,
            to_algebra_fun
          )

        join = if force_args?(right) or force_args?(args) or many_eol?, do: :line, else: :break

        {right_doc, _join, state} =
          args_to_algebra_with_comments(right, meta, false, :none, join, state, to_algebra_fun)

        right_doc = apply(Inspect.Algebra, join, []) |> concat(right_doc)

        args_doc =
          if skip_parens? do
            left_doc
            |> concat(next_break_fits(group(right_doc, :inherit), :enabled))
            |> nest(:cursor, :break)
          else
            right_doc =
              right_doc
              |> nest(2, :break)
              |> concat(break(""))
              |> group(:inherit)
              |> next_break_fits(:enabled)

            concat(nest(left_doc, 2, :break), right_doc)
          end

        {args_doc, true, state}
      else
        join = if force_args?(args) or many_eol?, do: :line, else: :break
        next_break_fits? = join == :break and next_break_fits?(right, state)
        last_arg_mode = if next_break_fits?, do: :next_break_fits, else: :none

        {args_doc, _join, state} =
          args_to_algebra_with_comments(
            args,
            meta,
            skip_parens?,
            last_arg_mode,
            join,
            state,
            to_algebra_fun
          )

        # If we have a single argument, then we won't have an option to break
        # before the "extra" part, so we ungroup it and build it later.
        args_doc = ungroup_if_group(args_doc)

        args_doc =
          if skip_parens? do
            nest(args_doc, :cursor, :break)
          else
            nest(args_doc, 2, :break) |> concat(break(""))
          end

        {args_doc, next_break_fits?, state}
      end

    doc =
      cond do
        left != [] and keyword? and skip_parens? and no_generators? ->
          " "
          |> concat(args_doc)
          |> nest(2)
          |> concat(extra)
          |> group()

        skip_parens? ->
          " "
          |> concat(args_doc)
          |> concat(extra)
          |> group()

        true ->
          "("
          |> concat(break(""))
          |> nest(2, :break)
          |> concat(args_doc)
          |> concat(")")
          |> concat(extra)
          |> group()
      end

    if next_break_fits? do
      {next_break_fits(doc, :disabled), state}
    else
      {doc, state}
    end
  end

  defp local_without_parens?(fun, args, %{locals_without_parens: locals_without_parens}) do
    length = length(args)

    length > 0 and
      Enum.any?(locals_without_parens, fn {key, val} ->
        key == fun and (val == :* or val == length)
      end)
  end

  defp no_generators?(args) do
    not Enum.any?(args, &match?({:<-, _, [_, _]}, &1))
  end

  defp do_end_blocks(meta, [{{:__block__, _, [:do]}, _} | rest] = blocks, state) do
    if meta?(meta, :do) or can_force_do_end_blocks?(rest, state) do
      blocks
      |> Enum.map(fn {{:__block__, meta, [key]}, value} -> {key, line(meta), value} end)
      |> do_end_blocks_with_range(end_line(meta))
    end
  end

  defp do_end_blocks(_, _, _), do: nil

  defp can_force_do_end_blocks?(rest, state) do
    state.force_do_end_blocks and
      Enum.all?(rest, fn {{:__block__, _, [key]}, _} -> key in @do_end_keywords end)
  end

  defp do_end_blocks_with_range([{key1, line1, value1}, {_, line2, _} = h | t], end_line) do
    [{key1, line1, line2, value1} | do_end_blocks_with_range([h | t], end_line)]
  end

  defp do_end_blocks_with_range([{key, line, value}], end_line) do
    [{key, line, end_line, value}]
  end

  defp do_end_blocks_to_algebra([{:do, line, end_line, value} | blocks], state) do
    {acc, state} = do_end_block_to_algebra(@empty, line, end_line, value, state)

    Enum.reduce(blocks, {acc, state}, fn {key, line, end_line, value}, {acc, state} ->
      {doc, state} = do_end_block_to_algebra(Atom.to_string(key), line, end_line, value, state)
      {line(acc, doc), state}
    end)
  end

  defp do_end_block_to_algebra(key_doc, line, end_line, value, state) do
    case clauses_to_algebra(value, line, end_line, state) do
      {@empty, state} -> {key_doc, state}
      {value_doc, state} -> {key_doc |> line(value_doc) |> nest(2), state}
    end
  end

  ## Interpolation

  defp list_interpolated?(entries) do
    Enum.all?(entries, fn
      {{:., _, [Kernel, :to_string]}, _, [_]} -> true
      entry when is_binary(entry) -> true
      _ -> false
    end)
  end

  defp interpolated?(entries) do
    Enum.all?(entries, fn
      {:"::", _, [{{:., _, [Kernel, :to_string]}, _, [_]}, {:binary, _, _}]} -> true
      entry when is_binary(entry) -> true
      _ -> false
    end)
  end

  defp prepend_heredoc_line([entry | entries]) when is_binary(entry) do
    ["\n" <> entry | entries]
  end

  defp prepend_heredoc_line(entries) do
    ["\n" | entries]
  end

  defp list_interpolation_to_algebra([entry | entries], escape, state, acc, last)
       when is_binary(entry) do
    acc = concat(acc, escape_string(entry, escape))
    list_interpolation_to_algebra(entries, escape, state, acc, last)
  end

  defp list_interpolation_to_algebra([entry | entries], escape, state, acc, last) do
    {{:., _, [Kernel, :to_string]}, meta, [quoted]} = entry
    {doc, state} = block_to_algebra(quoted, line(meta), closing_line(meta), state)
    doc = surround("\#{", doc, "}")
    list_interpolation_to_algebra(entries, escape, state, concat(acc, doc), last)
  end

  defp list_interpolation_to_algebra([], _escape, state, acc, last) do
    {concat(acc, last), state}
  end

  defp interpolation_to_algebra([entry | entries], escape, state, acc, last)
       when is_binary(entry) do
    acc = concat(acc, escape_string(entry, escape))
    interpolation_to_algebra(entries, escape, state, acc, last)
  end

  defp interpolation_to_algebra([entry | entries], escape, state, acc, last) do
    {:"::", _, [{{:., _, [Kernel, :to_string]}, meta, [quoted]}, {:binary, _, _}]} = entry
    {doc, state} = block_to_algebra(quoted, line(meta), closing_line(meta), state)
    doc = surround("\#{", doc, "}")
    interpolation_to_algebra(entries, escape, state, concat(acc, doc), last)
  end

  defp interpolation_to_algebra([], _escape, state, acc, last) do
    {concat(acc, last), state}
  end

  ## Sigils

  defp maybe_sigil_to_algebra(fun, meta, args, state) do
    with <<"sigil_", name>> <- Atom.to_string(fun),
         [{:<<>>, _, entries}, modifiers] when is_list(modifiers) <- args,
         opening_delimiter when not is_nil(opening_delimiter) <- meta[:delimiter] do
      doc = <<?~, name, opening_delimiter::binary>>

      if opening_delimiter in [@double_heredoc, @single_heredoc] do
        closing_delimiter = concat(opening_delimiter, List.to_string(modifiers))

        {doc, state} =
          entries
          |> prepend_heredoc_line()
          |> interpolation_to_algebra(:heredoc, state, doc, closing_delimiter)

        {force_unfit(doc), state}
      else
        escape = closing_sigil_delimiter(opening_delimiter)
        closing_delimiter = concat(escape, List.to_string(modifiers))
        interpolation_to_algebra(entries, escape, state, doc, closing_delimiter)
      end
    else
      _ ->
        :error
    end
  end

  defp closing_sigil_delimiter("("), do: ")"
  defp closing_sigil_delimiter("["), do: "]"
  defp closing_sigil_delimiter("{"), do: "}"
  defp closing_sigil_delimiter("<"), do: ">"
  defp closing_sigil_delimiter(other) when other in ["\"", "'", "|", "/"], do: other

  ## Bitstrings

  defp bitstring_to_algebra(meta, args, state) do
    last = length(args) - 1
    join = if eol?(meta), do: :line, else: :flex_break
    to_algebra_fun = &bitstring_segment_to_algebra(&1, &2, last)

    {args_doc, join, state} =
      args
      |> Enum.with_index()
      |> args_to_algebra_with_comments(meta, false, :none, join, state, to_algebra_fun)

    if join == :flex_break do
      {"<<" |> concat(args_doc) |> nest(2) |> concat(">>") |> group(), state}
    else
      {surround("<<", args_doc, ">>"), state}
    end
  end

  defp bitstring_segment_to_algebra({{:<-, meta, [left, right]}, i}, state, last) do
    left = {{:special, :bitstring_segment}, meta, [left, last]}
    {doc, state} = quoted_to_algebra({:<-, meta, [left, right]}, :parens_arg, state)
    {bitstring_wrap_parens(doc, i, last), state}
  end

  defp bitstring_segment_to_algebra({{:"::", _, [segment, spec]}, i}, state, last) do
    {doc, state} = quoted_to_algebra(segment, :parens_arg, state)
    {spec, state} = bitstring_spec_to_algebra(spec, state)

    spec = wrap_in_parens_if_inspected_atom(spec)
    spec = if i == last, do: bitstring_wrap_parens(spec, i, last), else: spec

    doc =
      doc
      |> bitstring_wrap_parens(i, -1)
      |> concat("::")
      |> concat(spec)

    {doc, state}
  end

  defp bitstring_segment_to_algebra({segment, i}, state, last) do
    {doc, state} = quoted_to_algebra(segment, :parens_arg, state)
    {bitstring_wrap_parens(doc, i, last), state}
  end

  defp bitstring_spec_to_algebra({op, _, [left, right]}, state) when op in [:-, :*] do
    {left, state} = bitstring_spec_to_algebra(left, state)
    {right, state} = quoted_to_algebra_with_parens_if_operator(right, :parens_arg, state)
    {concat(concat(left, Atom.to_string(op)), right), state}
  end

  defp bitstring_spec_to_algebra(spec, state) do
    quoted_to_algebra_with_parens_if_operator(spec, :parens_arg, state)
  end

  defp bitstring_wrap_parens(doc, i, last) when i == 0 or i == last do
    string = format_to_string(doc)

    if (i == 0 and String.starts_with?(string, ["~", "<<"])) or
         (i == last and String.ends_with?(string, [">>"])) do
      wrap_in_parens(doc)
    else
      doc
    end
  end

  defp bitstring_wrap_parens(doc, _, _), do: doc

  ## Literals

  defp list_to_algebra(meta, args, state) do
    join = if eol?(meta), do: :line, else: :break
    fun = &quoted_to_algebra(&1, :parens_arg, &2)

    {args_doc, _join, state} =
      args_to_algebra_with_comments(args, meta, false, :none, join, state, fun)

    {surround("[", args_doc, "]"), state}
  end

  defp map_to_algebra(meta, name_doc, [{:|, _, [left, right]}], state) do
    join = if eol?(meta), do: :line, else: :break
    fun = &quoted_to_algebra(&1, :parens_arg, &2)
    {left_doc, state} = fun.(left, state)

    {right_doc, _join, state} =
      args_to_algebra_with_comments(right, meta, false, :none, join, state, fun)

    args_doc =
      left_doc
      |> wrap_in_parens_if_binary_operator(left)
      |> glue(concat("| ", nest(right_doc, 2)))

    name_doc = "%" |> concat(name_doc) |> concat("{")
    {surround(name_doc, args_doc, "}"), state}
  end

  defp map_to_algebra(meta, name_doc, args, state) do
    join = if eol?(meta), do: :line, else: :break
    fun = &quoted_to_algebra(&1, :parens_arg, &2)

    {args_doc, _join, state} =
      args_to_algebra_with_comments(args, meta, false, :none, join, state, fun)

    name_doc = "%" |> concat(name_doc) |> concat("{")
    {surround(name_doc, args_doc, "}"), state}
  end

  defp tuple_to_algebra(meta, args, join, state) do
    join = if eol?(meta), do: :line, else: join
    fun = &quoted_to_algebra(&1, :parens_arg, &2)

    {args_doc, join, state} =
      args_to_algebra_with_comments(args, meta, false, :none, join, state, fun)

    if join == :flex_break do
      {"{" |> concat(args_doc) |> nest(1) |> concat("}") |> group(), state}
    else
      {surround("{", args_doc, "}"), state}
    end
  end

  defp atom_to_algebra(atom) when atom in [nil, true, false] do
    Atom.to_string(atom)
  end

  defp atom_to_algebra(atom) do
    string = Atom.to_string(atom)

    iodata =
      case Code.Identifier.classify(atom) do
        type when type in [:callable_local, :callable_operator, :not_callable] ->
          [?:, string]

        _ ->
          [?:, ?", String.replace(string, "\"", "\\\""), ?"]
      end

    iodata |> IO.iodata_to_binary() |> string()
  end

  defp integer_to_algebra(text) do
    case text do
      <<?0, ?x, rest::binary>> ->
        "0x" <> String.upcase(rest)

      <<?0, base, _::binary>> = digits when base in [?b, ?o] ->
        digits

      <<??, _::binary>> = char ->
        char

      decimal ->
        insert_underscores(decimal)
    end
  end

  defp float_to_algebra(text) do
    [int_part, decimal_part] = :binary.split(text, ".")
    decimal_part = String.downcase(decimal_part)
    insert_underscores(int_part) <> "." <> decimal_part
  end

  defp insert_underscores(digits) do
    cond do
      digits =~ "_" ->
        digits

      byte_size(digits) >= 6 ->
        digits
        |> String.to_charlist()
        |> Enum.reverse()
        |> Enum.chunk_every(3)
        |> Enum.intersperse('_')
        |> List.flatten()
        |> Enum.reverse()
        |> List.to_string()

      true ->
        digits
    end
  end

  defp escape_heredoc(string) do
    heredoc_to_algebra(["" | String.split(string, "\n")])
  end

  defp escape_string(string, :heredoc) do
    heredoc_to_algebra(String.split(string, "\n"))
  end

  defp escape_string(string, escape) when is_binary(escape) do
    string
    |> String.replace(escape, "\\" <> escape)
    |> String.split("\n")
    |> Enum.reverse()
    |> Enum.map(&string/1)
    |> Enum.reduce(&concat(&1, concat(nest(line(), :reset), &2)))
  end

  defp heredoc_to_algebra([string]) do
    string(string)
  end

  defp heredoc_to_algebra(["" | rest]) do
    rest
    |> heredoc_line()
    |> concat(heredoc_to_algebra(rest))
  end

  defp heredoc_to_algebra([string | rest]) do
    string
    |> string()
    |> concat(heredoc_line(rest))
    |> concat(heredoc_to_algebra(rest))
  end

  defp heredoc_line(["", _ | _]), do: nest(line(), :reset)
  defp heredoc_line(_), do: line()

  defp args_to_algebra_with_comments(args, meta, skip_parens?, last_arg_mode, join, state, fun) do
    min_line = line(meta)
    max_line = closing_line(meta)

    arg_to_algebra = fn arg, args, state ->
      {doc, state} = fun.(arg, state)

      doc =
        case args do
          [_ | _] -> concat_to_last_group(doc, ",")
          [] when last_arg_mode == :force_comma -> concat_to_last_group(doc, ",")
          [] when last_arg_mode == :next_break_fits -> next_break_fits(doc, :enabled)
          [] when last_arg_mode == :none -> doc
        end

      {{doc, @empty, 1}, state}
    end

    # If skipping parens, we cannot extract the comments of the first
    # argument as there is no place to move them to, so we handle it now.
    {args, acc, state} =
      case args do
        [head | tail] when skip_parens? ->
          {doc_triplet, state} = arg_to_algebra.(head, tail, state)
          {tail, [doc_triplet], state}

        _ ->
          {args, [], state}
      end

    {args_docs, comments?, state} =
      quoted_to_algebra_with_comments(args, acc, min_line, max_line, state, arg_to_algebra)

    cond do
      args_docs == [] ->
        {@empty, :empty, state}

      join == :line or comments? ->
        {args_docs |> Enum.reduce(&line(&2, &1)) |> force_unfit(), :line, state}

      join == :break ->
        {args_docs |> Enum.reduce(&glue(&2, &1)), :break, state}

      join == :flex_break ->
        {args_docs |> Enum.reduce(&flex_glue(&2, &1)), :flex_break, state}
    end
  end

  ## Anonymous functions

  # fn -> block end
  defp anon_fun_to_algebra(
         [{:->, meta, [[], body]}] = clauses,
         _min_line,
         max_line,
         state,
         _multi_clauses_style
       ) do
    min_line = line(meta)
    {body_doc, state} = block_to_algebra(body, min_line, max_line, state)

    doc =
      "fn ->"
      |> glue(body_doc)
      |> nest(2)
      |> glue("end")
      |> maybe_force_clauses(clauses)
      |> group()

    {doc, state}
  end

  # fn x -> y end
  # fn x ->
  #   y
  # end
  defp anon_fun_to_algebra(
         [{:->, meta, [args, body]}] = clauses,
         _min_line,
         max_line,
         state,
         false = _multi_clauses_style
       ) do
    min_line = line(meta)
    {args_doc, state} = clause_args_to_algebra(args, min_line, state)
    {body_doc, state} = block_to_algebra(body, min_line, max_line, state)

    head =
      args_doc
      |> ungroup_if_group()
      |> concat(" ->")
      |> nest(:cursor)
      |> group()

    doc =
      "fn "
      |> concat(head)
      |> glue(body_doc)
      |> nest(2)
      |> glue("end")
      |> maybe_force_clauses(clauses)
      |> group()

    {doc, state}
  end

  # fn
  #   args1 ->
  #     block1
  #   args2 ->
  #     block2
  # end
  defp anon_fun_to_algebra(clauses, min_line, max_line, state, _multi_clauses_style) do
    {clauses_doc, state} = clauses_to_algebra(clauses, min_line, max_line, state)
    {"fn" |> line(clauses_doc) |> nest(2) |> line("end") |> force_unfit(), state}
  end

  ## Type functions

  # (() -> block)
  defp type_fun_to_algebra([{:->, meta, [[], body]}] = clauses, _min_line, max_line, state) do
    min_line = line(meta)
    {body_doc, state} = block_to_algebra(body, min_line, max_line, state)

    doc =
      "(() -> "
      |> concat(nest(body_doc, :cursor))
      |> concat(")")
      |> maybe_force_clauses(clauses)
      |> group()

    {doc, state}
  end

  # (x -> y)
  # (x ->
  #    y)
  defp type_fun_to_algebra([{:->, meta, [args, body]}] = clauses, _min_line, max_line, state) do
    min_line = line(meta)
    {args_doc, state} = clause_args_to_algebra(args, min_line, state)
    {body_doc, state} = block_to_algebra(body, min_line, max_line, state)

    doc =
      args_doc
      |> ungroup_if_group()
      |> concat(" ->")
      |> group()
      |> concat(break() |> concat(body_doc) |> nest(2))
      |> wrap_in_parens()
      |> maybe_force_clauses(clauses)
      |> group()

    {doc, state}
  end

  # (
  #   args1 ->
  #     block1
  #   args2 ->
  #     block2
  # )
  defp type_fun_to_algebra(clauses, min_line, max_line, state) do
    {clauses_doc, state} = clauses_to_algebra(clauses, min_line, max_line, state)
    {"(" |> line(clauses_doc) |> nest(2) |> line(")") |> force_unfit(), state}
  end

  ## Clauses

  defp maybe_force_clauses(doc, clauses) do
    if Enum.any?(clauses, fn {:->, meta, _} -> eol?(meta) end) do
      force_unfit(doc)
    else
      doc
    end
  end

  defp clauses_to_algebra([{:->, _, _} | _] = clauses, min_line, max_line, state) do
    [clause | clauses] = add_max_line_to_last_clause(clauses, max_line)
    {clause_doc, state} = clause_to_algebra(clause, min_line, state)

    {clauses_doc, state} =
      Enum.reduce(clauses, {clause_doc, state}, fn clause, {doc_acc, state_acc} ->
        {clause_doc, state_acc} = clause_to_algebra(clause, min_line, state_acc)

        doc_acc =
          doc_acc
          |> concat(maybe_empty_line())
          |> line(clause_doc)

        {doc_acc, state_acc}
      end)

    {clauses_doc |> maybe_force_clauses([clause | clauses]) |> group(), state}
  end

  defp clauses_to_algebra(other, min_line, max_line, state) do
    case block_to_algebra(other, min_line, max_line, state) do
      {@empty, state} -> {@empty, state}
      {doc, state} -> {group(doc), state}
    end
  end

  defp clause_to_algebra({:->, meta, [[], body]}, _min_line, state) do
    {body_doc, state} = block_to_algebra(body, line(meta), closing_line(meta), state)
    {"() ->" |> glue(body_doc) |> nest(2), state}
  end

  defp clause_to_algebra({:->, meta, [args, body]}, min_line, state) do
    %{operand_nesting: nesting} = state

    state = %{state | operand_nesting: nesting + 2}
    {args_doc, state} = clause_args_to_algebra(args, min_line, state)

    state = %{state | operand_nesting: nesting}
    {body_doc, state} = block_to_algebra(body, min_line, closing_line(meta), state)

    doc =
      args_doc
      |> ungroup_if_group()
      |> concat(" ->")
      |> group()
      |> concat(break() |> concat(body_doc) |> nest(2))

    {doc, state}
  end

  defp add_max_line_to_last_clause([{op, meta, args}], max_line) do
    [{op, [closing: [line: max_line]] ++ meta, args}]
  end

  defp add_max_line_to_last_clause([clause | clauses], max_line) do
    [clause | add_max_line_to_last_clause(clauses, max_line)]
  end

  defp clause_args_to_algebra(args, min_line, state) do
    arg_to_algebra = fn arg, _args, state ->
      {doc, state} = clause_args_to_algebra(arg, state)
      {{doc, @empty, 1}, state}
    end

    {args_docs, comments?, state} =
      quoted_to_algebra_with_comments([args], [], min_line, @min_line, state, arg_to_algebra)

    if comments? do
      {Enum.reduce(args_docs, &line(&2, &1)), state}
    else
      {Enum.reduce(args_docs, &glue(&2, &1)), state}
    end
  end

  # fn a, b, c when d -> e end
  defp clause_args_to_algebra([{:when, meta, args}], state) do
    {args, right} = split_last(args)
    left = {{:special, :clause_args}, meta, [args]}
    binary_op_to_algebra(:when, "when", meta, left, right, :no_parens_arg, state)
  end

  # fn () -> e end
  defp clause_args_to_algebra([], state) do
    {"()", state}
  end

  # fn a, b, c -> e end
  defp clause_args_to_algebra(args, state) do
    many_args_to_algebra(args, state, &quoted_to_algebra(&1, :no_parens_arg, &2))
  end

  ## Quoted helpers for comments

  defp quoted_to_algebra_with_comments(args, acc, min_line, max_line, state, fun) do
    {pre_comments, state} =
      get_and_update_in(state.comments, fn comments ->
        Enum.split_while(comments, fn {line, _, _} -> line <= min_line end)
      end)

    {docs, comments?, state} =
      each_quoted_to_algebra_with_comments(args, acc, max_line, state, false, fun)

    {docs, comments?, update_in(state.comments, &(pre_comments ++ &1))}
  end

  defp each_quoted_to_algebra_with_comments([], acc, max_line, state, comments?, _fun) do
    {acc, comments, comments?} = extract_comments_before(max_line, acc, state.comments, comments?)
    args_docs = merge_algebra_with_comments(Enum.reverse(acc), @empty)
    {args_docs, comments?, %{state | comments: comments}}
  end

  defp each_quoted_to_algebra_with_comments(args, acc, max_line, state, comments?, fun) do
    [arg | args] = args
    {doc_start, doc_end} = traverse_line(arg, {@max_line, @min_line})

    {acc, comments, comments?} =
      extract_comments_before(doc_start, acc, state.comments, comments?)

    {doc_triplet, state} = fun.(arg, args, %{state | comments: comments})

    {acc, comments, comments?} =
      extract_comments_trailing(doc_start, doc_end, acc, state.comments, comments?)

    acc = [doc_triplet | acc]
    state = %{state | comments: comments}
    each_quoted_to_algebra_with_comments(args, acc, max_line, state, comments?, fun)
  end

  defp extract_comments_before(max, acc, [{line, _, _} = comment | rest], _) when line < max do
    {_, {previous, next}, doc} = comment
    acc = [{doc, @empty, next} | add_previous_to_acc(acc, previous)]
    extract_comments_before(max, acc, rest, true)
  end

  defp extract_comments_before(_max, acc, rest, comments?) do
    {acc, rest, comments?}
  end

  defp add_previous_to_acc([{doc, next_line, newlines} | acc], previous) when newlines < previous,
    do: [{doc, next_line, previous} | acc]

  defp add_previous_to_acc(acc, _previous),
    do: acc

  defp extract_comments_trailing(min, max, acc, [{line, _, doc_comment} | rest], _)
       when line >= min and line <= max do
    acc = [{doc_comment, @empty, 1} | acc]
    extract_comments_trailing(min, max, acc, rest, true)
  end

  defp extract_comments_trailing(_min, _max, acc, rest, comments?) do
    {acc, rest, comments?}
  end

  defp traverse_line({expr, meta, args}, {min, max}) do
    acc =
      case Keyword.fetch(meta, :line) do
        {:ok, line} -> {min(line, min), max(line, max)}
        :error -> {min, max}
      end

    traverse_line(args, traverse_line(expr, acc))
  end

  defp traverse_line({left, right}, acc) do
    traverse_line(right, traverse_line(left, acc))
  end

  defp traverse_line(args, acc) when is_list(args) do
    Enum.reduce(args, acc, &traverse_line/2)
  end

  defp traverse_line(_, acc) do
    acc
  end

  # Below are the rules for line rendering in the formatter:
  #
  #   1. respect the user's choice
  #   2. and add empty lines around expressions that take multiple lines
  #      (except for module attributes)
  #   3. empty lines are collapsed as to not exceed more than one
  #
  defp merge_algebra_with_comments([{doc, next_line, newlines} | docs], left) do
    right = if newlines >= @newlines, do: line(), else: next_line

    doc =
      if left != @empty do
        concat(left, doc)
      else
        doc
      end

    doc =
      if docs != [] and right != @empty do
        concat(doc, concat(collapse_lines(2), right))
      else
        doc
      end

    [group(doc) | merge_algebra_with_comments(docs, right)]
  end

  defp merge_algebra_with_comments([], _) do
    []
  end

  ## Quoted helpers

  defp left_op_context(context), do: force_many_args_or_operand(context, :parens_arg)
  defp right_op_context(context), do: force_many_args_or_operand(context, :operand)

  defp force_many_args_or_operand(:no_parens_one_arg, _choice), do: :no_parens_arg
  defp force_many_args_or_operand(:parens_one_arg, _choice), do: :parens_arg
  defp force_many_args_or_operand(:no_parens_arg, _choice), do: :no_parens_arg
  defp force_many_args_or_operand(:parens_arg, _choice), do: :parens_arg
  defp force_many_args_or_operand(:operand, choice), do: choice
  defp force_many_args_or_operand(:block, choice), do: choice

  defp quoted_to_algebra_with_parens_if_operator(ast, context, state) do
    {doc, state} = quoted_to_algebra(ast, context, state)
    {wrap_in_parens_if_operator(doc, ast), state}
  end

  # TODO: We can remove this workaround once we remove
  # ?rearrange_uop from the parser on v2.0.
  defp wrap_in_parens_if_operator(doc, {:__block__, _, [expr]}) do
    wrap_in_parens_if_operator(doc, expr)
  end

  defp wrap_in_parens_if_operator(doc, quoted) do
    if operator?(quoted) and not module_attribute_read?(quoted) and not integer_capture?(quoted) do
      wrap_in_parens(doc)
    else
      doc
    end
  end

  defp wrap_in_parens_if_binary_operator(doc, quoted) do
    if binary_operator?(quoted) do
      wrap_in_parens(doc)
    else
      doc
    end
  end

  defp wrap_in_parens_if_inspected_atom(":" <> _ = doc) do
    "(" <> doc <> ")"
  end

  defp wrap_in_parens_if_inspected_atom(doc) do
    doc
  end

  defp wrap_in_parens(doc) do
    concat(concat("(", nest(doc, :cursor)), ")")
  end

  defp many_args_to_algebra([arg | args], state, fun) do
    Enum.reduce(args, fun.(arg, state), fn arg, {doc_acc, state_acc} ->
      {arg_doc, state_acc} = fun.(arg, state_acc)
      {glue(concat(doc_acc, ","), arg_doc), state_acc}
    end)
  end

  defp module_attribute_read?({:@, _, [{var, _, var_context}]})
       when is_atom(var) and is_atom(var_context) do
    Code.Identifier.classify(var) == :callable_local
  end

  defp module_attribute_read?(_), do: false

  defp integer_capture?({:&, _, [integer]}) when is_integer(integer), do: true
  defp integer_capture?(_), do: false

  defp operator?(quoted) do
    unary_operator?(quoted) or binary_operator?(quoted)
  end

  defp binary_operator?(quoted) do
    case quoted do
      {op, _, [_, _]} when is_atom(op) ->
        Code.Identifier.binary_op(op) != :error

      _ ->
        false
    end
  end

  defp unary_operator?(quoted) do
    case quoted do
      {op, _, [_]} when is_atom(op) ->
        Code.Identifier.unary_op(op) != :error

      _ ->
        false
    end
  end

  defp with_next_break_fits(condition, doc, fun) do
    if condition do
      doc
      |> next_break_fits(:enabled)
      |> fun.()
      |> next_break_fits(:disabled)
    else
      fun.(doc)
    end
  end

  defp next_break_fits?({:{}, meta, _args}, state) do
    eol_or_comments?(meta, state)
  end

  defp next_break_fits?({:__block__, meta, [{_, _}]}, state) do
    eol_or_comments?(meta, state)
  end

  defp next_break_fits?({:<<>>, meta, [_ | _] = entries}, state) do
    meta[:delimiter] == ~s["""] or
      (not interpolated?(entries) and eol_or_comments?(meta, state))
  end

  defp next_break_fits?({{:., _, [List, :to_charlist]}, meta, [[_ | _]]}, _state) do
    meta[:delimiter] == ~s[''']
  end

  defp next_break_fits?({{:., _, [_left, :{}]}, _, _}, _state) do
    true
  end

  defp next_break_fits?({:__block__, meta, [string]}, _state) when is_binary(string) do
    meta[:delimiter] == ~s["""]
  end

  defp next_break_fits?({:__block__, meta, [list]}, _state) when is_list(list) do
    meta[:delimeter] != ~s[']
  end

  defp next_break_fits?({form, _, [_ | _]}, _state) when form in [:fn, :%{}, :%] do
    true
  end

  defp next_break_fits?({fun, meta, args}, _state) when is_atom(fun) and is_list(args) do
    meta[:delimiter] in [@double_heredoc, @single_heredoc] and
      fun |> Atom.to_string() |> String.starts_with?("sigil_")
  end

  defp next_break_fits?({{:__block__, _, [atom]}, expr}, state) when is_atom(atom) do
    next_break_fits?(expr, state)
  end

  defp next_break_fits?(_, _state) do
    false
  end

  defp eol_or_comments?(meta, %{comments: comments}) do
    eol?(meta) or
      (
        min_line = line(meta)
        max_line = closing_line(meta)
        Enum.any?(comments, fn {line, _, _} -> line > min_line and line < max_line end)
      )
  end

  # A literal list is a keyword or (... -> ...)
  defp last_arg_to_keyword([_ | _] = arg, _list_to_keyword?) do
    {keyword?(arg), arg}
  end

  # This is a list of tuples, it can be converted to keywords.
  defp last_arg_to_keyword({:__block__, _, [[_ | _] = arg]} = block, true) do
    if keyword?(arg), do: {true, arg}, else: {false, block}
  end

  # Otherwise we don't have a keyword.
  defp last_arg_to_keyword(arg, _list_to_keyword?) do
    {false, arg}
  end

  defp force_args?(args) do
    match?([_, _ | _], args) and force_args?(args, MapSet.new())
  end

  defp force_args?([[arg | _] | args], lines) do
    force_args?([arg | args], lines)
  end

  defp force_args?([arg | args], lines) do
    line =
      case arg do
        {{_, meta, _}, _} -> line(meta)
        {_, meta, _} -> line(meta)
      end

    if MapSet.member?(lines, line) do
      false
    else
      force_args?(args, MapSet.put(lines, line))
    end
  end

  defp force_args?([], _lines), do: true

  defp force_keyword(doc, arg) do
    if force_args?(arg), do: force_unfit(doc), else: doc
  end

  defp keyword?([{_, _} | list]), do: keyword?(list)
  defp keyword?(rest), do: rest == []

  defp keyword_key?({:__block__, meta, [atom]}) when is_atom(atom),
    do: meta[:delimiter] != ":"

  defp keyword_key?({{:., _, [:erlang, :binary_to_atom]}, meta, [{:<<>>, _, _}, :utf8]}),
    do: meta[:delimiter] != ":"

  defp keyword_key?(_),
    do: false

  defp eol?(meta) do
    Keyword.get(meta, :newlines, 0) > 0
  end

  defp meta?(meta, key) do
    is_list(meta[key])
  end

  defp line(meta) do
    meta[:line] || @max_line
  end

  defp end_line(meta) do
    meta[:end][:line] || @min_line
  end

  defp closing_line(meta) do
    meta[:closing][:line] || @min_line
  end

  ## Algebra helpers

  # Relying on the inner document is brittle and error prone.
  # It would be best if we had a mechanism to apply this.
  defp concat_to_last_group({:doc_cons, left, right}, concat) do
    {:doc_cons, left, concat_to_last_group(right, concat)}
  end

  defp concat_to_last_group({:doc_group, group, mode}, concat) do
    {:doc_group, {:doc_cons, group, concat}, mode}
  end

  defp concat_to_last_group(other, concat) do
    {:doc_cons, other, concat}
  end

  defp ungroup_if_group({:doc_group, group, _mode}), do: group
  defp ungroup_if_group(other), do: other

  defp format_to_string(doc) do
    doc |> Inspect.Algebra.format(:infinity) |> IO.iodata_to_binary()
  end

  defp maybe_empty_line() do
    nest(break(""), :reset)
  end

  defp surround(left, doc, right) do
    if doc == @empty do
      concat(left, right)
    else
      group(glue(nest(glue(left, "", doc), 2, :break), "", right))
    end
  end

  defp nest_by_length(doc, string) do
    nest(doc, String.length(string))
  end

  defp split_last(list) do
    {left, [right]} = Enum.split(list, -1)
    {left, right}
  end
end
