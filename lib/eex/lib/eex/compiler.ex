defmodule EEx.Compiler do
  @moduledoc false

  # When changing this setting, don't forget to update the docs for EEx
  @default_engine EEx.SmartEngine
  @h_spaces [?\s, ?\t]
  @all_spaces [?\s, ?\t, ?\n, ?\r]

  @doc """
  Tokenize EEx contents.
  """
  def tokenize(contents, opts) when is_binary(contents) do
    tokenize(String.to_charlist(contents), contents, opts)
  end

  def tokenize(contents, opts) when is_list(contents) do
    tokenize(contents, List.to_string(contents), opts)
  end

  def tokenize(contents, source, opts) when is_list(contents) do
    file = opts[:file] || "nofile"
    line = opts[:line] || 1
    trim = opts[:trim] || false
    indentation = opts[:indentation] || 0
    column = indentation + (opts[:column] || 1)

    state = %{trim: trim, indentation: indentation, file: file, source: source}

    {contents, line, column} =
      (trim && trim_init(contents, line, column, state)) || {contents, line, column}

    tokenize(contents, line, column, state, [{line, column}], [])
  end

  defp tokenize(~c"<%%" ++ t, line, column, state, buffer, acc) do
    tokenize(t, line, column + 3, state, [?%, ?< | buffer], acc)
  end

  defp tokenize(~c"<%!--" ++ t, line, column, state, buffer, acc) do
    case comment(t, line, column + 5, state, []) do
      {:error, message} ->
        meta = %{line: line, column: column}
        {:error, message <> code_snippet(state.source, state.indentation, meta), meta}

      {:ok, new_line, new_column, rest, comments} ->
        token = {:comment, Enum.reverse(comments), %{line: line, column: column}}
        trim_and_tokenize(rest, new_line, new_column, state, buffer, acc, &[token | &1])
    end
  end

  # TODO: Remove me on Elixir v2.0
  defp tokenize(~c"<%#" ++ t, line, column, state, buffer, acc) do
    IO.warn("<%# is deprecated, use <%!-- or add a space between <% and # instead",
      line: line,
      column: column,
      file: state.file
    )

    case expr(t, line, column + 3, state, []) do
      {:error, message} ->
        {:error, message, %{line: line, column: column}}

      {:ok, _, new_line, new_column, rest} ->
        trim_and_tokenize(rest, new_line, new_column, state, buffer, acc, & &1)
    end
  end

  defp tokenize(~c"<%" ++ t, line, column, state, buffer, acc) do
    {marker, t} = retrieve_marker(t)
    marker_length = length(marker)

    case expr(t, line, column + 2 + marker_length, state, []) do
      {:error, message} ->
        meta = %{line: line, column: column}
        {:error, message <> code_snippet(state.source, state.indentation, meta), meta}

      {:ok, expr, new_line, new_column, rest} ->
        {key, expr} =
          case :elixir_tokenizer.tokenize(expr, 1, file: "eex", check_terminators: false) do
            {:ok, _line, _column, _warnings, rev_tokens, []} ->
              # We ignore warnings because the code will be tokenized
              # again later with the right line+column info
              token_key(rev_tokens, expr)

            {:error, _, _, _, _} ->
              {:expr, expr}
          end

        marker =
          if key in [:middle_expr, :end_expr] and marker != ~c"" do
            message =
              "unexpected beginning of EEx tag \"<%#{marker}\" on \"<%#{marker}#{expr}%>\", " <>
                "please remove \"#{marker}\""

            :elixir_errors.erl_warn({line, column}, state.file, message)
            ~c""
          else
            marker
          end

        token = {key, marker, expr, %{line: line, column: column}}
        trim_and_tokenize(rest, new_line, new_column, state, buffer, acc, &[token | &1])
    end
  end

  defp tokenize([?\n | t], line, _column, state, buffer, acc) do
    tokenize(t, line + 1, state.indentation + 1, state, [?\n | buffer], acc)
  end

  defp tokenize([h | t], line, column, state, buffer, acc) do
    tokenize(t, line, column + 1, state, [h | buffer], acc)
  end

  defp tokenize([], line, column, _state, buffer, acc) do
    eof = {:eof, %{line: line, column: column}}
    {:ok, Enum.reverse([eof | tokenize_text(buffer, acc)])}
  end

  defp trim_and_tokenize(rest, line, column, state, buffer, acc, fun) do
    {rest, line, column, buffer} = trim_if_needed(rest, line, column, state, buffer)

    acc = tokenize_text(buffer, acc)
    tokenize(rest, line, column, state, [{line, column}], fun.(acc))
  end

  # Retrieve marker for <%

  defp retrieve_marker([marker | t]) when marker in [?=, ?/, ?|] do
    {[marker], t}
  end

  defp retrieve_marker(t) do
    {~c"", t}
  end

  # Tokenize a multi-line comment until we find --%>

  defp comment([?-, ?-, ?%, ?> | t], line, column, _state, buffer) do
    {:ok, line, column + 4, t, buffer}
  end

  defp comment([?\n | t], line, _column, state, buffer) do
    comment(t, line + 1, state.indentation + 1, state, [?\n | buffer])
  end

  defp comment([head | t], line, column, state, buffer) do
    comment(t, line, column + 1, state, [head | buffer])
  end

  defp comment([], _line, _column, _state, _buffer) do
    {:error, "expected closing '--%>' for EEx expression"}
  end

  # Tokenize an expression until we find %>

  defp expr([?%, ?> | t], line, column, _state, buffer) do
    {:ok, Enum.reverse(buffer), line, column + 2, t}
  end

  defp expr([?\n | t], line, _column, state, buffer) do
    expr(t, line + 1, state.indentation + 1, state, [?\n | buffer])
  end

  defp expr([h | t], line, column, state, buffer) do
    expr(t, line, column + 1, state, [h | buffer])
  end

  defp expr([], _line, _column, _state, _buffer) do
    {:error, "expected closing '%>' for EEx expression"}
  end

  # Receives tokens and check if it is a start, middle or an end token.
  defp token_key(rev_tokens, expr) do
    case {Enum.reverse(rev_tokens), drop_eol(rev_tokens)} do
      {[{:end, _} | _], [{:do, _} | _]} ->
        {:middle_expr, expr}

      {_, [{:do, _} | _]} ->
        {:start_expr, maybe_append_space(expr)}

      {_, [{:block_identifier, _, _} | _]} ->
        {:middle_expr, maybe_append_space(expr)}

      {[{:end, _} | _], [{:stab_op, _, _} | _]} ->
        {:middle_expr, expr}

      {_, [{:stab_op, _, _} | reverse_tokens]} ->
        fn_index = Enum.find_index(reverse_tokens, &match?({:fn, _}, &1)) || :infinity
        end_index = Enum.find_index(reverse_tokens, &match?({:end, _}, &1)) || :infinity

        if end_index > fn_index do
          {:start_expr, expr}
        else
          {:middle_expr, expr}
        end

      {tokens, _} ->
        case Enum.drop_while(tokens, &closing_bracket?/1) do
          [{:end, _} | _] -> {:end_expr, expr}
          _ -> {:expr, expr}
        end
    end
  end

  defp drop_eol([{:eol, _} | rest]), do: drop_eol(rest)
  defp drop_eol(rest), do: rest

  defp maybe_append_space([?\s]), do: [?\s]
  defp maybe_append_space([h]), do: [h, ?\s]
  defp maybe_append_space([h | t]), do: [h | maybe_append_space(t)]

  defp closing_bracket?({closing, _}) when closing in ~w"( [ {"a, do: true
  defp closing_bracket?(_), do: false

  # Tokenize the buffered text by appending
  # it to the given accumulator.

  defp tokenize_text([{_line, _column}], acc) do
    acc
  end

  defp tokenize_text(buffer, acc) do
    [{line, column} | buffer] = Enum.reverse(buffer)
    [{:text, buffer, %{line: line, column: column}} | acc]
  end

  ## Trim

  defp trim_if_needed(rest, line, column, state, buffer) do
    if state.trim do
      buffer = trim_left(buffer, 0)
      {rest, line, column} = trim_right(rest, line, column, 0, state)
      {rest, line, column, buffer}
    else
      {rest, line, column, buffer}
    end
  end

  defp trim_init([h | t], line, column, state) when h in @h_spaces,
    do: trim_init(t, line, column + 1, state)

  defp trim_init([?\r, ?\n | t], line, _column, state),
    do: trim_init(t, line + 1, state.indentation + 1, state)

  defp trim_init([?\n | t], line, _column, state),
    do: trim_init(t, line + 1, state.indentation + 1, state)

  defp trim_init([?<, ?% | _] = rest, line, column, _state),
    do: {rest, line, column}

  defp trim_init(_, _, _, _), do: false

  defp trim_left(buffer, count) do
    case trim_whitespace(buffer, 0) do
      {[?\n, ?\r | rest], _} -> trim_left(rest, count + 1)
      {[?\n | rest], _} -> trim_left(rest, count + 1)
      _ when count > 0 -> [?\n | buffer]
      _ -> buffer
    end
  end

  defp trim_right(rest, line, column, last_column, state) do
    case trim_whitespace(rest, column) do
      {[?\r, ?\n | rest], column} ->
        trim_right(rest, line + 1, state.indentation + 1, column + 1, state)

      {[?\n | rest], column} ->
        trim_right(rest, line + 1, state.indentation + 1, column, state)

      {[], column} ->
        {[], line, column}

      _ when last_column > 0 ->
        {[?\n | rest], line - 1, last_column}

      _ ->
        {rest, line, column}
    end
  end

  defp trim_whitespace([h | t], column) when h in @h_spaces, do: trim_whitespace(t, column + 1)
  defp trim_whitespace(list, column), do: {list, column}

  @doc """
  This is the compilation entry point. It glues the tokenizer
  and the engine together by handling the tokens and invoking
  the engine every time a full expression or text is received.
  """
  @spec compile([EEx.token()], String.t(), keyword) :: Macro.t()
  def compile(tokens, source, opts) do
    file = opts[:file] || "nofile"
    line = opts[:line] || 1
    indentation = opts[:indentation] || 0
    parser_options = opts[:parser_options] || Code.get_compiler_option(:parser_options)
    engine = opts[:engine] || @default_engine

    state = %{
      engine: engine,
      file: file,
      source: source,
      line: line,
      quoted: [],
      parser_options: parser_options,
      indentation: indentation
    }

    init = state.engine.init(opts)

    if function_exported?(state.engine, :handle_text, 2) and
         not function_exported?(state.engine, :handle_text, 3) do
      IO.warn(
        "#{inspect(state.engine)}.handle_text/2 is deprecated, implement handle_text/3 instead"
      )
    end

    generate_buffer(tokens, init, [], state)
  end

  # Ignore tokens related to comment.
  defp generate_buffer([{:comment, _chars, _meta} | rest], buffer, scope, state) do
    generate_buffer(rest, buffer, scope, state)
  end

  # Generates the buffers by handling each expression from the tokenizer.
  # It returns Macro.t/0 or it raises.

  defp generate_buffer([{:text, chars, meta} | rest], buffer, scope, state) do
    buffer =
      if function_exported?(state.engine, :handle_text, 3) do
        meta = [line: meta.line, column: meta.column]
        state.engine.handle_text(buffer, meta, IO.chardata_to_string(chars))
      else
        # TODO: Remove this on Elixir v2.0. The deprecation is on init.
        state.engine.handle_text(buffer, IO.chardata_to_string(chars))
      end

    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer([{:expr, mark, chars, meta} | rest], buffer, scope, state) do
    options =
      [file: state.file, line: meta.line, column: column(meta.column, mark)] ++
        state.parser_options

    expr = Code.string_to_quoted!(chars, options)
    buffer = state.engine.handle_expr(buffer, IO.chardata_to_string(mark), expr)
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer(
         [{:start_expr, mark, chars, meta} | rest],
         buffer,
         scope,
         state
       ) do
    {rest, line, contents} = look_ahead_middle(rest, meta.line, chars) || {rest, meta.line, chars}
    start_line = meta.line
    start_column = column(meta.column, mark)

    {contents, rest} =
      generate_buffer(
        rest,
        state.engine.handle_begin(buffer),
        [{contents, start_line, start_column} | scope],
        %{state | quoted: [], line: line}
      )

    if mark == ~c"" and not match?({:=, _, [_, _]}, contents) do
      message =
        "the contents of this expression won't be output unless the EEx block starts with \"<%=\""

      :elixir_errors.erl_warn({meta.line, meta.column}, state.file, message)
    end

    buffer = state.engine.handle_expr(buffer, IO.chardata_to_string(mark), contents)
    generate_buffer(rest, buffer, scope, state)
  end

  defp generate_buffer(
         [{:middle_expr, ~c"", chars, meta} | rest],
         buffer,
         [{current, current_line, current_column} | scope],
         state
       ) do
    {wrapped, state} = wrap_expr(current, meta.line, buffer, chars, state)
    state = %{state | line: meta.line}

    generate_buffer(
      rest,
      state.engine.handle_begin(buffer),
      [{wrapped, current_line, current_column} | scope],
      state
    )
  end

  defp generate_buffer([{:middle_expr, _, chars, meta} | _tokens], _buffer, [], state) do
    message = "unexpected middle of expression <%#{chars}%>"
    syntax_error!(message, meta, state)
  end

  defp generate_buffer(
         [{:end_expr, ~c"", chars, meta} | rest],
         buffer,
         [{current, line, column} | _],
         state
       ) do
    {wrapped, state} = wrap_expr(current, meta.line, buffer, chars, state)
    options = [file: state.file, line: line, column: column] ++ state.parser_options
    tuples = Code.string_to_quoted!(wrapped, options)
    buffer = insert_quoted(tuples, state.quoted)
    {buffer, rest}
  end

  defp generate_buffer([{:end_expr, _, chars, meta} | _], _buffer, [], state) do
    message = "unexpected end of expression <%#{chars}%>"
    syntax_error!(message, meta, state)
  end

  defp generate_buffer([{:eof, _meta}], buffer, [], state) do
    state.engine.handle_body(buffer)
  end

  defp generate_buffer([{:eof, _meta}], _buffer, [{content, line, column} | _scope], state) do
    message = "expected a closing '<% end %>' for block expression in EEx"
    expr_meta = non_whitespace_meta(content, line, column, state)
    syntax_error!(message, expr_meta, state)
  end

  defp non_whitespace_meta([space | rest], line, column, state) when space in @h_spaces,
    do: non_whitespace_meta(rest, line, column + 1, state)

  defp non_whitespace_meta([?\n | rest], line, _column, state),
    do: non_whitespace_meta(rest, line + 1, state.indentation + 1, state)

  defp non_whitespace_meta(_, line, column, _),
    do: %{line: line, column: column}

  # Creates a placeholder and wrap it inside the expression block

  defp wrap_expr(current, line, buffer, chars, state) do
    new_lines = List.duplicate(?\n, line - state.line)
    key = length(state.quoted)
    placeholder = ~c"__EEX__(" ++ Integer.to_charlist(key) ++ ~c");"
    count = current ++ placeholder ++ new_lines ++ chars
    new_state = %{state | quoted: [{key, state.engine.handle_end(buffer)} | state.quoted]}

    {count, new_state}
  end

  # Look middle expressions that immediately follow a start_expr

  defp look_ahead_middle([{:comment, _comment, _meta} | rest], start, contents),
    do: look_ahead_middle(rest, start, contents)

  defp look_ahead_middle([{:text, text, _meta} | rest], start, contents) do
    if only_spaces?(text) do
      look_ahead_middle(rest, start, contents ++ text)
    else
      nil
    end
  end

  defp look_ahead_middle([{:middle_expr, _, chars, meta} | rest], _start, contents) do
    {rest, meta.line, contents ++ chars}
  end

  defp look_ahead_middle(_tokens, _start, _contents) do
    nil
  end

  defp only_spaces?(chars) do
    Enum.all?(chars, &(&1 in @all_spaces))
  end

  # Changes placeholder to real expression

  defp insert_quoted({:__EEX__, _, [key]}, quoted) do
    {^key, value} = List.keyfind(quoted, key, 0)
    value
  end

  defp insert_quoted({left, line, right}, quoted) do
    {insert_quoted(left, quoted), line, insert_quoted(right, quoted)}
  end

  defp insert_quoted({left, right}, quoted) do
    {insert_quoted(left, quoted), insert_quoted(right, quoted)}
  end

  defp insert_quoted(list, quoted) when is_list(list) do
    Enum.map(list, &insert_quoted(&1, quoted))
  end

  defp insert_quoted(other, _quoted) do
    other
  end

  defp column(column, mark) do
    # length(~c"<%") == 2
    column + 2 + length(mark)
  end

  defp syntax_error!(message, meta, state) do
    raise EEx.SyntaxError,
      message: message,
      snippet: code_snippet(state.source, state.indentation, meta),
      file: state.file,
      line: meta.line,
      column: meta.column
  end

  defp code_snippet(source, indentation, meta) do
    line_start = max(meta.line - 3, 1)
    line_end = meta.line
    digits = line_end |> Integer.to_string() |> byte_size()
    number_padding = String.duplicate(" ", digits)
    indentation = String.duplicate(" ", indentation)

    source
    |> String.split(["\r\n", "\n"])
    |> Enum.slice((line_start - 1)..(line_end - 1))
    |> Enum.map_reduce(line_start, fn
      expr, line_number when line_number == line_end ->
        arrow = String.duplicate(" ", meta.column - 1) <> "^"
        {"#{line_number} | #{indentation}#{expr}\n #{number_padding}| #{arrow}", line_number + 1}

      expr, line_number ->
        line_number_padding = String.pad_leading("#{line_number}", digits)
        {"#{line_number_padding} | #{indentation}#{expr}", line_number + 1}
    end)
    |> case do
      {[], _} -> ""
      {snippet, _} -> Enum.join(["\n #{number_padding}|" | snippet], "\n")
    end
  end
end
