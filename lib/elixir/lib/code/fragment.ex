defmodule Code.Fragment do
  @moduledoc """
  This module provides conveniences for analyzing fragments of
  textual code and extract available information whenever possible.

  This module should be considered experimental.
  """

  @type position :: {line :: pos_integer(), column :: pos_integer()}

  @doc """
  Receives a string and returns the cursor context.

  This function receives a string with an Elixir code fragment,
  representing a cursor position, and based on the string, it
  provides contextual information about the latest token.
  The return of this function can then be used to provide tips,
  suggestions, and autocompletion functionality.

  This function performs its analyses on tokens. This means
  it does not understand how constructs are nested within each
  other. See the "Limitations" section below.

  Consider adding a catch-all clause when handling the return
  type of this function as new cursor information may be added
  in future releases.

  ## Examples

      iex> Code.Fragment.cursor_context("")
      :expr

      iex> Code.Fragment.cursor_context("hello_wor")
      {:local_or_var, ~c"hello_wor"}

  ## Return values

    * `{:alias, charlist}` - the context is an alias, potentially
      a nested one, such as `Hello.Wor` or `HelloWor`

    * `{:alias, inside_alias, charlist}` - the context is an alias, potentially
      a nested one, where `inside_alias` is an expression `{:module_attribute, charlist}`
      or `{:local_or_var, charlist}` and `charlist` is a static part
      Examples are `__MODULE__.Submodule` or `@hello.Submodule`

    * `{:dot, inside_dot, charlist}` - the context is a dot
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, this may either be a remote call or a map
      field access. Examples are `Hello.wor`, `:hello.wor`, `hello.wor`,
      `Hello.nested.wor`, `hello.nested.wor`, and `@hello.world`. If `charlist`
      is empty and `inside_dot` is an alias, then the autocompletion may either
      be an alias or a remote call.

    * `{:dot_arity, inside_dot, charlist}` - the context is a dot arity
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, it must be a remote arity. Examples are
      `Hello.world/`, `:hello.world/`, `hello.world/2`, and `@hello.world/2`

    * `{:dot_call, inside_dot, charlist}` - the context is a dot
      call. This means parentheses or space have been added after the expression.
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, it must be a remote call. Examples are
      `Hello.world(`, `:hello.world(`, `Hello.world `, `hello.world(`, `hello.world `,
      and `@hello.world(`

    * `:expr` - may be any expression. Autocompletion may suggest an alias,
      local or var

    * `{:local_or_var, charlist}` - the context is a variable or a local
      (import or local) call, such as `hello_wor`

    * `{:local_arity, charlist}` - the context is a local (import or local)
      arity, such as `hello_world/`

    * `{:local_call, charlist}` - the context is a local (import or local)
      call, such as `hello_world(` and `hello_world `

    * `{:anonymous_call, inside_caller}` - the context is an anonymous
      call, such as `fun.(` and `@fun.(`.

    * `{:module_attribute, charlist}` - the context is a module attribute,
      such as `@hello_wor`

    * `{:operator, charlist}` - the context is an operator, such as `+` or
      `==`. Note textual operators, such as `when` do not appear as operators
      but rather as `:local_or_var`. `@` is never an `:operator` and always a
      `:module_attribute`

    * `{:operator_arity, charlist}` - the context is an operator arity, which
      is an operator followed by /, such as `+/`, `not/` or `when/`

    * `{:operator_call, charlist}` - the context is an operator call, which is
      an operator followed by space, such as `left + `, `not ` or `x when `

    * `:none` - no context possible

    * `{:sigil, charlist}` - the context is a sigil. It may be either the beginning
      of a sigil, such as `~` or `~s`, or an operator starting with `~`, such as
      `~>` and `~>>`

    * `{:struct, inside_struct}` - the context is a struct, such as `%`, `%UR` or `%URI`.
      `inside_struct` can either be a `charlist` in case of a static alias or an
      expression `{:alias, inside_alias, charlist}`, `{:module_attribute, charlist}`,
      `{:local_or_var, charlist}`, `{:dot, inside_dot, charlist}`

    * `{:unquoted_atom, charlist}` - the context is an unquoted atom. This
      can be any atom or an atom representing a module

  We recommend looking at the test suite of this function for a complete list
  of examples and their return values.

  ## Limitations

  The analysis is based on the current token, by analysing the last line of
  the input. For example, this code:

      iex> Code.Fragment.cursor_context("%URI{")
      :expr

  returns `:expr`, which suggests any variable, local function or alias
  could be used. However, given we are inside a struct, the best suggestion
  would be a struct field. In such cases, you can use
  `container_cursor_to_quoted`, which will return the container of the AST
  the cursor is currently within. You can then analyse this AST to provide
  completion of field names.

  As a consequence of its token-based implementation, this function considers
  only the last line of the input. This means it will show suggestions inside
  strings, heredocs, etc, which is intentional as it helps with doctests,
  references, and more.
  """
  @doc since: "1.13.0"
  @spec cursor_context(List.Chars.t(), keyword()) ::
          {:alias, charlist}
          | {:alias, inside_alias, charlist}
          | {:dot, inside_dot, charlist}
          | {:dot_arity, inside_dot, charlist}
          | {:dot_call, inside_dot, charlist}
          | :expr
          | {:local_or_var, charlist}
          | {:local_arity, charlist}
          | {:local_call, charlist}
          | {:anonymous_call, inside_caller}
          | {:module_attribute, charlist}
          | {:operator, charlist}
          | {:operator_arity, charlist}
          | {:operator_call, charlist}
          | :none
          | {:sigil, charlist}
          | {:struct, inside_struct}
          | {:unquoted_atom, charlist}
        when inside_dot:
               {:alias, charlist}
               | {:alias, inside_alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:module_attribute, charlist}
               | {:unquoted_atom, charlist}
               | {:var, charlist}
               | :expr,
             inside_alias:
               {:local_or_var, charlist}
               | {:module_attribute, charlist},
             inside_struct:
               charlist
               | {:alias, inside_alias, charlist}
               | {:local_or_var, charlist}
               | {:module_attribute, charlist}
               | {:dot, inside_dot, charlist},
             inside_caller: {:var, charlist} | {:module_attribute, charlist}
  def cursor_context(fragment, opts \\ [])

  def cursor_context(fragment, opts)
      when (is_binary(fragment) or is_list(fragment)) and is_list(opts) do
    fragment
    |> last_line()
    |> :lists.reverse()
    |> codepoint_cursor_context(opts)
    |> elem(0)
  end

  def cursor_context(other, opts) when is_list(opts) do
    cursor_context(to_charlist(other), opts)
  end

  @operators ~c"\\<>+-*/:=|&~^%!"
  @starter_punctuation ~c",([{;"
  @non_starter_punctuation ~c")]}\"'.$"
  @space ~c"\t\s"
  @trailing_identifier ~c"?!"
  @tilde_op_prefix ~c"<=~"

  @non_identifier @trailing_identifier ++
                    @operators ++ @starter_punctuation ++ @non_starter_punctuation ++ @space

  @textual_operators ~w(when not and or in)c
  @keywords ~w(do end after else catch rescue fn true false nil)c

  defp codepoint_cursor_context(reverse, _opts) do
    {stripped, spaces} = strip_spaces(reverse, 0)

    case stripped do
      # It is empty
      [] -> {:expr, 0}
      # Structs
      [?%, ?:, ?: | _] -> {{:struct, ~c""}, 1}
      [?%, ?: | _] -> {{:unquoted_atom, ~c"%"}, 2}
      [?% | _] -> {{:struct, ~c""}, 1}
      # Token/AST only operators
      [?>, ?= | rest] when rest == [] or hd(rest) != ?: -> {:expr, 0}
      [?>, ?- | rest] when rest == [] or hd(rest) != ?: -> {:expr, 0}
      # Two-digit containers
      [?<, ?< | rest] when rest == [] or hd(rest) != ?< -> {:expr, 0}
      # Ambiguity around :
      [?: | rest] when rest == [] or hd(rest) != ?: -> unquoted_atom_or_expr(spaces)
      # Dots
      [?.] -> {:none, 0}
      [?. | rest] when hd(rest) not in ~c".:" -> dot(rest, spaces + 1, ~c"")
      # It is a local or remote call with parens
      [?( | rest] -> call_to_cursor_context(strip_spaces(rest, spaces + 1))
      # A local arity definition
      [?/ | rest] -> arity_to_cursor_context(strip_spaces(rest, spaces + 1))
      # Starting a new expression
      [h | _] when h in @starter_punctuation -> {:expr, 0}
      # It is a local or remote call without parens
      rest when spaces > 0 -> call_to_cursor_context({rest, spaces})
      # It is an identifier
      _ -> identifier_to_cursor_context(reverse, 0, false)
    end
  end

  defp strip_spaces([h | rest], count) when h in @space, do: strip_spaces(rest, count + 1)
  defp strip_spaces(rest, count), do: {rest, count}

  defp unquoted_atom_or_expr(0), do: {{:unquoted_atom, ~c""}, 1}
  defp unquoted_atom_or_expr(_), do: {:expr, 0}

  defp arity_to_cursor_context({reverse, spaces}) do
    case identifier_to_cursor_context(reverse, spaces, true) do
      {{:local_or_var, acc}, count} -> {{:local_arity, acc}, count}
      {{:dot, base, acc}, count} -> {{:dot_arity, base, acc}, count}
      {{:operator, acc}, count} -> {{:operator_arity, acc}, count}
      {_, _} -> {:none, 0}
    end
  end

  defp call_to_cursor_context({reverse, spaces}) do
    with [?. | rest] <- reverse,
         {rest, spaces} = strip_spaces(rest, spaces),
         [h | _] when h not in @non_identifier <- rest do
      case identifier_to_cursor_context(rest, spaces, true) do
        {{:local_or_var, acc}, count} -> {{:anonymous_call, {:var, acc}}, count + 1}
        {{:module_attribute, _} = attr, count} -> {{:anonymous_call, attr}, count + 1}
        {_, _} -> {:none, 0}
      end
    else
      _ ->
        case identifier_to_cursor_context(reverse, spaces, true) do
          {{:local_or_var, acc}, count} -> {{:local_call, acc}, count}
          {{:dot, base, acc}, count} -> {{:dot_call, base, acc}, count}
          {{:operator, acc}, count} -> {{:operator_call, acc}, count}
          {_, _} -> {:none, 0}
        end
    end
  end

  defp identifier_to_cursor_context([?., ?., ?: | _], n, _), do: {{:unquoted_atom, ~c".."}, n + 3}
  defp identifier_to_cursor_context([?., ?., ?. | _], n, _), do: {{:local_or_var, ~c"..."}, n + 3}
  defp identifier_to_cursor_context([?., ?: | _], n, _), do: {{:unquoted_atom, ~c"."}, n + 2}
  defp identifier_to_cursor_context([?., ?. | _], n, _), do: {{:operator, ~c".."}, n + 2}

  defp identifier_to_cursor_context(reverse, count, call_op?) do
    case identifier(reverse, count) do
      :none ->
        {:none, 0}

      :operator ->
        operator(reverse, count, [], call_op?)

      {:struct, {:module_attribute, acc}, count} ->
        {{:struct, {:module_attribute, acc}}, count + 1}

      {:module_attribute, acc, count} ->
        {{:module_attribute, acc}, count}

      {:sigil, acc, count} ->
        {{:sigil, acc}, count}

      {:unquoted_atom, acc, count} ->
        {{:unquoted_atom, acc}, count}

      {:alias, rest, acc, count} ->
        case strip_spaces(rest, count) do
          {~c"." ++ rest, count} when rest == [] or hd(rest) != ?. ->
            nested_alias(rest, count + 1, acc)

          {~c"%" ++ _, count} ->
            {{:struct, acc}, count + 1}

          _ ->
            {{:alias, acc}, count}
        end

      {:identifier, _, acc, count} when call_op? and acc in @textual_operators ->
        {{:operator, acc}, count}

      {:identifier, [?%], acc, count} ->
        case identifier_to_cursor_context(acc |> Enum.reverse(), count, true) do
          {{:local_or_var, _} = identifier, _} -> {{:struct, identifier}, count + 1}
          _ -> {:none, 0}
        end

      {:identifier, rest, acc, count} ->
        case strip_spaces(rest, count) do
          {~c"." ++ rest, count} when rest == [] or hd(rest) != ?. ->
            dot(rest, count + 1, acc)

          _ ->
            {{:local_or_var, acc}, count}
        end

      {:capture_arg, acc, count} ->
        {{:capture_arg, acc}, count}
    end
  end

  defp identifier([?? | rest], count), do: check_identifier(rest, count + 1, [??])
  defp identifier([?! | rest], count), do: check_identifier(rest, count + 1, [?!])
  defp identifier(rest, count), do: check_identifier(rest, count, [])

  defp check_identifier([h | t], count, acc) when h not in @non_identifier,
    do: rest_identifier(t, count + 1, [h | acc])

  defp check_identifier(_, _, _), do: :operator

  defp rest_identifier([h | rest], count, acc) when h not in @non_identifier do
    rest_identifier(rest, count + 1, [h | acc])
  end

  defp rest_identifier(rest, count, [?@ | acc]) do
    case tokenize_identifier(rest, count, acc) do
      {:identifier, [?% | _rest], acc, count} -> {:struct, {:module_attribute, acc}, count}
      {:identifier, _rest, acc, count} -> {:module_attribute, acc, count}
      :none when acc == [] -> {:module_attribute, ~c"", count}
      _ -> :none
    end
  end

  defp rest_identifier([?~ | rest], count, [letter])
       when (letter in ?A..?Z or letter in ?a..?z) and
              (rest == [] or hd(rest) not in @tilde_op_prefix) do
    {:sigil, [letter], count + 1}
  end

  defp rest_identifier([?: | rest], count, acc) when rest == [] or hd(rest) != ?: do
    case String.Tokenizer.tokenize(acc) do
      {_, _, [], _, _, _} -> {:unquoted_atom, acc, count + 1}
      _ -> :none
    end
  end

  defp rest_identifier([?? | _], _count, _acc) do
    :none
  end

  defp rest_identifier([?& | tail] = rest, count, acc) when tail == [] or hd(tail) != ?& do
    if Enum.all?(acc, &(&1 in ?0..?9)) do
      {:capture_arg, [?& | acc], count + 1}
    else
      tokenize_identifier(rest, count, acc)
    end
  end

  defp rest_identifier(rest, count, acc) do
    tokenize_identifier(rest, count, acc)
  end

  defp tokenize_identifier(rest, count, acc) do
    case String.Tokenizer.tokenize(acc) do
      # Not actually an atom cause rest is not a :
      {:atom, _, _, _, _, _} ->
        :none

      # Aliases must be ascii only
      {:alias, _, _, _, false, _} ->
        :none

      {kind, _, [], _, _, extra} ->
        if :at in extra do
          :none
        else
          {kind, rest, acc, count}
        end

      _ ->
        :none
    end
  end

  defp nested_alias(rest, count, acc) do
    {rest, count} = strip_spaces(rest, count)

    case identifier_to_cursor_context(rest, count, true) do
      {{:struct, prev}, count} when is_list(prev) ->
        {{:struct, prev ++ ~c"." ++ acc}, count}

      {{:struct, {:alias, parent, prev}}, count} ->
        {{:struct, {:alias, parent, prev ++ ~c"." ++ acc}}, count}

      {{:struct, prev}, count} ->
        {{:struct, {:alias, prev, acc}}, count}

      {{:alias, prev}, count} ->
        {{:alias, prev ++ ~c"." ++ acc}, count}

      {{:alias, parent, prev}, count} ->
        {{:alias, parent, prev ++ ~c"." ++ acc}, count}

      {{:local_or_var, prev}, count} ->
        {{:alias, {:local_or_var, prev}, acc}, count}

      {{:module_attribute, prev}, count} ->
        {{:alias, {:module_attribute, prev}, acc}, count}

      _ ->
        {:none, 0}
    end
  end

  defp dot(rest, count, acc) do
    {rest, count} = strip_spaces(rest, count)

    case identifier_to_cursor_context(rest, count, true) do
      {{:local_or_var, var}, count} ->
        {{:dot, {:var, var}, acc}, count}

      {{:unquoted_atom, _} = prev, count} ->
        {{:dot, prev, acc}, count}

      {{:alias, _} = prev, count} ->
        {{:dot, prev, acc}, count}

      {{:alias, _, _} = prev, count} ->
        {{:dot, prev, acc}, count}

      {{:struct, inner}, count} when is_list(inner) ->
        {{:struct, {:dot, {:alias, inner}, acc}}, count}

      {{:struct, inner}, count} ->
        {{:struct, {:dot, inner, acc}}, count}

      {{:dot, _, _} = prev, count} ->
        {{:dot, prev, acc}, count}

      {{:module_attribute, _} = prev, count} ->
        {{:dot, prev, acc}, count}

      {:expr, count} ->
        {{:dot, :expr, acc}, count}

      {_, _} ->
        {:none, 0}
    end
  end

  defp operator([h | rest], count, acc, call_op?) when h in @operators do
    operator(rest, count + 1, [h | acc], call_op?)
  end

  # If we are opening a sigil, ignore the operator.
  defp operator([letter, ?~ | rest], _count, [op], _call_op?)
       when op in ~c"<|/" and (letter in ?A..?Z or letter in ?a..?z) and
              (rest == [] or hd(rest) not in @tilde_op_prefix) do
    {:none, 0}
  end

  defp operator(rest, count, ~c"~", call_op?) do
    {rest, _} = strip_spaces(rest, count)

    if call_op? or match?([?. | rest] when rest == [] or hd(rest) != ?., rest) do
      {:none, 0}
    else
      {{:sigil, ~c""}, count}
    end
  end

  defp operator([?) | rest], _, [], true) when hd(rest) != ?? do
    {:expr, 0}
  end

  defp operator(rest, count, acc, _call_op?) do
    case :elixir_tokenizer.tokenize(acc, 1, 1, []) do
      {:ok, _, _, _, [{:atom, _, _}], []} ->
        {{:unquoted_atom, tl(acc)}, count}

      {:ok, _, _, _, [{_, _, op}], []} ->
        {rest, dot_count} = strip_spaces(rest, count)

        cond do
          Code.Identifier.unary_op(op) == :error and Code.Identifier.binary_op(op) == :error ->
            {:none, 0}

          match?([?. | rest] when rest == [] or hd(rest) != ?., rest) ->
            dot(tl(rest), dot_count + 1, acc)

          true ->
            {{:operator, acc}, count}
        end

      _ ->
        {:none, 0}
    end
  end

  @doc """
  Receives a string and returns the surround context.

  This function receives a string with an Elixir code fragment
  and a `position`. It returns a map containing the beginning
  and ending of the identifier alongside its context, or `:none`
  if there is nothing with a known context. This is useful to
  provide mouse-over and highlight functionality in editors.

  The difference between `cursor_context/2` and `surround_context/3`
  is that the former assumes the expression in the code fragment
  is incomplete. For example, `do` in `cursor_context/2` may be
  a keyword or a variable or a local call, while `surround_context/3`
  assumes the expression in the code fragment is complete, therefore
  `do` would always be a keyword.

  The `position` contains both the `line` and `column`, both starting
  with the index of 1. The column must precede the surrounding expression.
  For example, the expression `foo`, will return something for the columns
  1, 2, and 3, but not 4:

      foo
      ^ column 1

      foo
       ^ column 2

      foo
        ^ column 3

      foo
         ^ column 4

  The returned map contains the column the expression starts and the
  first column after the expression ends.

  Similar to `cursor_context/2`, this function is also token-based
  and may not be accurate under all circumstances. See the
  "Return values" and "Limitations" section under `cursor_context/2`
  for more information.

  ## Examples

      iex> Code.Fragment.surround_context("foo", {1, 1})
      %{begin: {1, 1}, context: {:local_or_var, ~c"foo"}, end: {1, 4}}

  ## Differences to `cursor_context/2`

  Because `surround_context/3` attempts to capture complex expressions,
  it has some differences to `cursor_context/2`:

    * `dot_call`/`dot_arity` and `operator_call`/`operator_arity`
      are collapsed into `dot` and `operator` contexts respectively
      as there aren't any meaningful distinctions between them

    * On the other hand, this function still makes a distinction between
      `local_call`/`local_arity` and `local_or_var`, since the latter can
      be a local or variable

    * `@` when not followed by any identifier is returned as `{:operator, ~c"@"}`
      (in contrast to `{:module_attribute, ~c""}` in `cursor_context/2`

    * This function never returns empty sigils `{:sigil, ~c""}` or empty structs
      `{:struct, ~c""}` as context

    * This function returns keywords as `{:keyword, ~c"do"}`

    * This function never returns `:expr`

  We recommend looking at the test suite of this function for a complete list
  of examples and their return values.
  """
  @doc since: "1.13.0"
  @spec surround_context(List.Chars.t(), position(), keyword()) ::
          %{begin: position, end: position, context: context} | :none
        when context:
               {:alias, charlist}
               | {:alias, inside_alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:local_or_var, charlist}
               | {:local_arity, charlist}
               | {:local_call, charlist}
               | {:module_attribute, charlist}
               | {:operator, charlist}
               | {:sigil, charlist}
               | {:struct, inside_struct}
               | {:unquoted_atom, charlist}
               | {:keyword, charlist}
               | {:key, charlist}
               | {:capture_arg, charlist},
             inside_dot:
               {:alias, charlist}
               | {:alias, inside_alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:module_attribute, charlist}
               | {:unquoted_atom, charlist}
               | {:var, charlist}
               | :expr,
             inside_alias:
               {:local_or_var, charlist}
               | {:module_attribute, charlist},
             inside_struct:
               charlist
               | {:alias, inside_alias, charlist}
               | {:local_or_var, charlist}
               | {:module_attribute, charlist}
               | {:dot, inside_dot, charlist}
  def surround_context(fragment, position, options \\ [])

  def surround_context(string, {line, column}, opts)
      when (is_binary(string) or is_list(string)) and is_list(opts) do
    {charlist, lines_before_lengths, lines_current_and_after_lengths} =
      surround_line(string, line, column)

    prepended_columns = Enum.sum(lines_before_lengths)

    charlist
    |> position_surround_context(line, column + prepended_columns, opts)
    |> to_multiline_range(
      prepended_columns,
      lines_before_lengths,
      lines_current_and_after_lengths
    )
  end

  def surround_context(other, {_, _} = position, opts) do
    surround_context(to_charlist(other), position, opts)
  end

  defp position_surround_context(charlist, line, column, opts)
       when is_integer(line) and line >= 1 and is_integer(column) and column >= 1 do
    {reversed_pre, post} = string_reverse_at(charlist, column - 1, [])
    {reversed_pre, post} = adjust_position(reversed_pre, post)

    case take_identifier(post, []) do
      {_, [], _} ->
        maybe_operator(reversed_pre, post, line, opts)

      {:identifier, reversed_post, rest} ->
        {keyword_key?, rest} =
          case rest do
            [?: | tail] when tail == [] or hd(tail) in @space ->
              {true, rest}

            _ ->
              {rest, _} = strip_spaces(rest, 0)
              {false, rest}
          end

        reversed = reversed_post ++ reversed_pre

        case codepoint_cursor_context(reversed, opts) do
          {{:struct, acc}, offset} ->
            build_surround({:struct, acc}, reversed, line, offset)

          {{:alias, acc}, offset} ->
            build_surround({:alias, acc}, reversed, line, offset)

          {{:alias, parent, acc}, offset} ->
            build_surround({:alias, parent, acc}, reversed, line, offset)

          {{:dot, _, [_ | _]} = dot, offset} ->
            build_surround(dot, reversed, line, offset)

          {{:local_or_var, acc}, offset} when keyword_key? ->
            build_surround({:key, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when hd(rest) == ?( ->
            build_surround({:local_call, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when hd(rest) == ?/ ->
            build_surround({:local_arity, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when acc in @textual_operators ->
            build_surround({:operator, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when acc in @keywords ->
            build_surround({:keyword, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} ->
            build_surround({:local_or_var, acc}, reversed, line, offset)

          {{:module_attribute, ~c""}, offset} ->
            build_surround({:operator, ~c"@"}, reversed, line, offset)

          {{:module_attribute, acc}, offset} ->
            build_surround({:module_attribute, acc}, reversed, line, offset)

          {{:sigil, acc}, offset} ->
            build_surround({:sigil, acc}, reversed, line, offset)

          {{:unquoted_atom, acc}, offset} ->
            build_surround({:unquoted_atom, acc}, reversed, line, offset)

          {{:capture_arg, acc}, offset} ->
            build_surround({:capture_arg, acc}, reversed, line, offset)

          _ ->
            maybe_operator(reversed_pre, post, line, opts)
        end

      {:alias, reversed_post, _rest} ->
        reversed = reversed_post ++ reversed_pre

        case codepoint_cursor_context(reversed, opts) do
          {{:alias, acc}, offset} ->
            build_surround({:alias, acc}, reversed, line, offset)

          {{:alias, parent, acc}, offset} ->
            build_surround({:alias, parent, acc}, reversed, line, offset)

          {{:struct, acc}, offset} ->
            build_surround({:struct, acc}, reversed, line, offset)

          _ ->
            :none
        end
    end
  end

  defp maybe_operator(reversed_pre, post, line, opts) do
    case take_operator(post, []) do
      {[], _rest} ->
        :none

      {reversed_post, rest} ->
        reversed = reversed_post ++ reversed_pre

        case codepoint_cursor_context(reversed, opts) do
          {{:operator, ~c"&"}, offset} when hd(rest) in ?0..?9 ->
            arg = Enum.take_while(rest, &(&1 in ?0..?9))

            build_surround(
              {:capture_arg, ~c"&" ++ arg},
              :lists.reverse(arg, reversed),
              line,
              offset + length(arg)
            )

          {{:operator, acc}, offset} ->
            build_surround({:operator, acc}, reversed, line, offset)

          {{:sigil, ~c""}, offset} when hd(rest) in ?A..?Z or hd(rest) in ?a..?z ->
            build_surround({:sigil, [hd(rest)]}, [hd(rest) | reversed], line, offset + 1)

          {{:dot, _, [_ | _]} = dot, offset} ->
            build_surround(dot, reversed, line, offset)

          _ ->
            :none
        end
    end
  end

  defp build_surround(context, reversed, line, offset) do
    {post, reversed_pre} = enum_reverse_at(reversed, offset, [])
    pre = :lists.reverse(reversed_pre)
    pre_length = :string.length(pre) + 1

    %{
      context: context,
      begin: {line, pre_length},
      end: {line, pre_length + :string.length(post)}
    }
  end

  defp take_identifier([h | t], acc) when h in @trailing_identifier,
    do: {:identifier, [h | acc], t}

  defp take_identifier([h | t], acc) when h not in @non_identifier,
    do: take_identifier(t, [h | acc])

  defp take_identifier(rest, acc) do
    with {[?. | t], _} <- strip_spaces(rest, 0),
         {[h | _], _} when h in ?A..?Z <- strip_spaces(t, 0) do
      take_alias(rest, acc)
    else
      _ -> {:identifier, acc, rest}
    end
  end

  defp take_alias([h | t], acc) when h in ?A..?Z or h in ?a..?z or h in ?0..?9 or h == ?_,
    do: take_alias(t, [h | acc])

  defp take_alias(rest, acc) do
    with {[?. | t], acc} <- move_spaces(rest, acc),
         {[h | t], acc} when h in ?A..?Z <- move_spaces(t, [?. | acc]) do
      take_alias(t, [h | acc])
    else
      _ -> {:alias, acc, rest}
    end
  end

  defp take_operator([h | t], acc) when h in @operators, do: take_operator(t, [h | acc])
  defp take_operator([h | t], acc) when h == ?., do: take_operator(t, [h | acc])
  defp take_operator(rest, acc), do: {acc, rest}

  # Unquoted atom handling
  defp adjust_position(reversed_pre, [?: | post])
       when hd(post) != ?: and (reversed_pre == [] or hd(reversed_pre) != ?:) do
    {[?: | reversed_pre], post}
  end

  defp adjust_position(reversed_pre, [?% | post]) do
    adjust_position([?% | reversed_pre], post)
  end

  # Dot/struct handling
  defp adjust_position(reversed_pre, post) do
    case move_spaces(post, reversed_pre) do
      # If we are between spaces and a dot, move past the dot
      {[?. | post], reversed_pre} when hd(post) != ?. and hd(reversed_pre) != ?. ->
        {post, reversed_pre} = move_spaces(post, [?. | reversed_pre])
        {reversed_pre, post}

      _ ->
        case strip_spaces(reversed_pre, 0) do
          # If there is a dot to our left, make sure to move to the first character
          {[?. | rest], _} when rest == [] or hd(rest) not in ~c".:" ->
            {post, reversed_pre} = move_spaces(post, reversed_pre)
            {reversed_pre, post}

          # If there is a % to our left, make sure to move to the first character
          {[?% | _], _} ->
            case move_spaces(post, reversed_pre) do
              {[h | _] = post, reversed_pre} when h in ?A..?Z ->
                {reversed_pre, post}

              _ ->
                {reversed_pre, post}
            end

          _ ->
            {reversed_pre, post}
        end
    end
  end

  defp move_spaces([h | t], acc) when h in @space, do: move_spaces(t, [h | acc])
  defp move_spaces(t, acc), do: {t, acc}

  defp string_reverse_at(charlist, 0, acc), do: {acc, charlist}

  defp string_reverse_at(charlist, n, acc) do
    case :unicode_util.gc(charlist) do
      [gc | cont] when is_integer(gc) -> string_reverse_at(cont, n - 1, [gc | acc])
      [gc | cont] when is_list(gc) -> string_reverse_at(cont, n - 1, :lists.reverse(gc, acc))
      [] -> {acc, []}
    end
  end

  defp enum_reverse_at([h | t], n, acc) when n > 0, do: enum_reverse_at(t, n - 1, [h | acc])
  defp enum_reverse_at(rest, _, acc), do: {acc, rest}

  defp last_line(binary) when is_binary(binary) do
    [last_line | lines_reverse] =
      binary
      |> String.split(["\r\n", "\n"])
      |> Enum.reverse()

    prepend_cursor_lines(lines_reverse, String.to_charlist(last_line))
  end

  defp last_line(charlist) when is_list(charlist) do
    [last_line | lines_reverse] =
      charlist
      |> :string.replace(~c"\r\n", ~c"\n", :all)
      |> :string.join(~c"")
      |> :string.split(~c"\n", :all)
      |> Enum.reverse()

    prepend_cursor_lines(lines_reverse, last_line)
  end

  defp prepend_cursor_lines(lines, last_line) do
    with [line | lines] <- lines,
         {trimmed_line, incomplete?} = ends_as_incomplete(to_charlist(line), [], true),
         true <- incomplete? or starts_with_dot?(last_line) do
      prepend_cursor_lines(lines, Enum.reverse(trimmed_line, last_line))
    else
      _ -> last_line
    end
  end

  defp starts_with_dot?([?. | _]), do: true
  defp starts_with_dot?([h | t]) when h in @space, do: starts_with_dot?(t)
  defp starts_with_dot?(_), do: false

  defp ends_as_incomplete([?# | _], acc, incomplete?),
    do: {acc, incomplete?}

  defp ends_as_incomplete([h | t], acc, _incomplete?) when h in [?(, ?.],
    do: ends_as_incomplete(t, [h | acc], true)

  defp ends_as_incomplete([h | t], acc, incomplete?) when h in @space,
    do: ends_as_incomplete(t, [h | acc], incomplete?)

  defp ends_as_incomplete([h | t], acc, _incomplete?),
    do: ends_as_incomplete(t, [h | acc], false)

  defp ends_as_incomplete([], acc, incomplete?),
    do: {acc, incomplete?}

  defp surround_line(binary, line, column) when is_binary(binary) do
    binary
    |> String.split(["\r\n", "\n"])
    |> Enum.map(&String.to_charlist/1)
    |> surround_lines(line, column)
  end

  defp surround_line(charlist, line, column) when is_list(charlist) do
    charlist
    |> :string.replace(~c"\r\n", ~c"\n", :all)
    |> :string.join(~c"")
    |> :string.split(~c"\n", :all)
    |> surround_lines(line, column)
  end

  defp surround_lines(lines, line, column) do
    {lines_before_reverse, cursor_line, lines_after} = split_at(lines, line, [])
    {trimmed_cursor_line, incomplete?} = ends_as_incomplete(to_charlist(cursor_line), [], true)

    reversed_cursor_line =
      if column - 1 > length(trimmed_cursor_line) do
        # Don't strip comments if cursor is inside a comment
        Enum.reverse(cursor_line)
      else
        trimmed_cursor_line
      end

    {cursor_line, after_lengths} =
      append_surround_lines(lines_after, [], [reversed_cursor_line], incomplete?)

    {cursor_line, before_lengths} = prepend_surround_lines(lines_before_reverse, [], cursor_line)
    {cursor_line, before_lengths, [length(reversed_cursor_line) | after_lengths]}
  end

  defp split_at([line], _, acc), do: {acc, line, []}
  defp split_at([line | lines], 1, acc), do: {acc, line, lines}
  defp split_at([line | lines], count, acc), do: split_at(lines, count - 1, [line | acc])

  defp prepend_surround_lines(lines, lengths, last_line) do
    with [line | lines] <- lines,
         {trimmed_line, incomplete?} = ends_as_incomplete(to_charlist(line), [], true),
         true <- incomplete? or starts_with_dot?(last_line) do
      lengths = [length(trimmed_line) | lengths]
      prepend_surround_lines(lines, lengths, Enum.reverse(trimmed_line, last_line))
    else
      _ -> {last_line, Enum.reverse(lengths)}
    end
  end

  defp append_surround_lines(lines, lengths, acc_lines, incomplete?) do
    with [line | lines] <- lines,
         line = to_charlist(line),
         true <- incomplete? or starts_with_dot?(line) do
      {trimmed_line, incomplete?} = ends_as_incomplete(line, [], true)
      lengths = [length(trimmed_line) | lengths]
      append_surround_lines(lines, lengths, [trimmed_line | acc_lines], incomplete?)
    else
      _ -> {Enum.reduce(acc_lines, [], &Enum.reverse/2), Enum.reverse(lengths)}
    end
  end

  defp to_multiline_range(:none, _, _, _), do: :none

  defp to_multiline_range(
         %{begin: {begin_line, begin_column}, end: {end_line, end_column}} = context,
         prepended,
         lines_before_lengths,
         lines_current_and_after_lengths
       ) do
    {begin_line, begin_column} =
      Enum.reduce_while(lines_before_lengths, {begin_line, begin_column - prepended}, fn
        line_length, {acc_line, acc_column} ->
          if acc_column < 1 do
            {:cont, {acc_line - 1, acc_column + line_length}}
          else
            {:halt, {acc_line, acc_column}}
          end
      end)

    {end_line, end_column} =
      Enum.reduce_while(lines_current_and_after_lengths, {end_line, end_column - prepended}, fn
        line_length, {acc_line, acc_column} ->
          if acc_column > line_length + 1 do
            {:cont, {acc_line + 1, acc_column - line_length}}
          else
            {:halt, {acc_line, acc_column}}
          end
      end)

    %{context | begin: {begin_line, begin_column}, end: {end_line, end_column}}
  end

  @doc """
  Receives a string and returns a quoted expression
  with the cursor AST position within its parent expression.

  This function receives a string with an Elixir code fragment,
  representing a cursor position, and converts such string to
  AST with the inclusion of special `__cursor__()` node representing
  the cursor position within its container (i.e. its parent).

  For example, take this code, which would be given as input:

      max(some_value,

  This function will return the AST equivalent to:

      max(some_value, __cursor__())

  In other words, this function is capable of closing any open
  brackets and insert the cursor position. Other content at the
  cursor position which is not a parent is discarded.
  For example, if this is given as input:

      max(some_value, another_val

  It will return the same AST:

      max(some_value, __cursor__())

  Similarly, if only this is given:

      max(some_va

  Then it returns:

      max(__cursor__())

  Calls without parenthesis are also supported, as we assume the
  brackets are implicit.

  Tuples, lists, maps, and binaries all retain the cursor position:

      max(some_value, [1, 2,

  Returns the following AST:

      max(some_value, [1, 2, __cursor__()])

  Keyword lists (and do-end blocks) are also retained. The following:

      if(some_value, do:
      if(some_value, do: :token
      if(some_value, do: 1 + val

  all return:

      if(some_value, do: __cursor__())

  For multi-line blocks, all previous lines are preserved.

  The AST returned by this function is not safe to evaluate but
  it can be analyzed and expanded.

  ## Examples

  Function call:

      iex> Code.Fragment.container_cursor_to_quoted("max(some_value, ")
      {:ok, {:max, [line: 1], [{:some_value, [line: 1], nil}, {:__cursor__, [line: 1], []}]}}

  Containers (for example, a list):

      iex> Code.Fragment.container_cursor_to_quoted("[some, value")
      {:ok, [{:some, [line: 1], nil}, {:__cursor__, [line: 1], []}]}

  If an expression is complete, then the whole expression is discarded
  and only the parent is returned:

      iex> Code.Fragment.container_cursor_to_quoted("if(is_atom(var)")
      {:ok, {:if, [line: 1], [{:__cursor__, [line: 1], []}]}}

  this means complete expressions themselves return only the cursor:

      iex> Code.Fragment.container_cursor_to_quoted("if(is_atom(var))")
      {:ok, {:__cursor__, [line: 1], []}}

  Operators are also included from Elixir v1.15:

      iex> Code.Fragment.container_cursor_to_quoted("foo +")
      {:ok, {:+, [line: 1], [{:foo, [line: 1], nil}, {:__cursor__, [line: 1], []}]}}

  ## Options

    * `:file` - the filename to be reported in case of parsing errors.
      Defaults to `"nofile"`.

    * `:line` - the starting line of the string being parsed.
      Defaults to 1.

    * `:column` - the starting column of the string being parsed.
      Defaults to 1.

    * `:columns` - when `true`, attach a `:column` key to the quoted
      metadata. Defaults to `false`.

    * `:token_metadata` - when `true`, includes token-related
      metadata in the expression AST, such as metadata for `do` and `end`
      tokens, for closing tokens, end of expressions, as well as delimiters
      for sigils. See `t:Macro.metadata/0`. Defaults to `false`.

    * `:literal_encoder` - a function to encode literals in the AST.
      See the documentation for `Code.string_to_quoted/2` for more information.

  """
  @doc since: "1.13.0"
  @spec container_cursor_to_quoted(List.Chars.t(), keyword()) ::
          {:ok, Macro.t()} | {:error, {location :: keyword, binary | {binary, binary}, binary}}
  def container_cursor_to_quoted(fragment, opts \\ []) do
    opts = Keyword.take(opts, [:columns, :token_metadata, :literal_encoder])
    opts = [cursor_completion: true, emit_warnings: false] ++ opts

    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    column = Keyword.get(opts, :column, 1)

    case :elixir_tokenizer.tokenize(to_charlist(fragment), line, column, opts) do
      {:ok, line, column, _warnings, rev_tokens, rev_terminators} ->
        tokens = :lists.reverse(rev_tokens, rev_terminators)

        case :elixir.tokens_to_quoted(tokens, file, opts) do
          {:ok, ast} ->
            {:ok, ast}

          {:error, error} ->
            # In case parsing fails, we give it another shot but handling fn/do/else/catch/rescue/after.
            tokens =
              :lists.reverse(
                rev_tokens,
                [{:stab_op, {line, column, nil}, :->}, {nil, {line, column + 2, nil}}] ++
                  Enum.map(rev_terminators, fn tuple ->
                    {line, column, info} = elem(tuple, 1)
                    put_elem(tuple, 1, {line, column + 5, info})
                  end)
              )

            case :elixir.tokens_to_quoted(tokens, file, opts) do
              {:ok, ast} -> {:ok, ast}
              {:error, _} -> {:error, error}
            end
        end

      {:error, info, _rest, _warnings, _so_far} ->
        {:error, :elixir.format_token_error(info)}
    end
  end
end
