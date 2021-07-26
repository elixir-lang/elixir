defmodule Code.Fragment do
  @moduledoc """
  This module provides conveniences for analyzing fragments of
  textual code and extract available information whenever possible.

  Most of the functions in this module provide a best-effort
  and may not be accurate under all circumstances. Read each
  documentation for more information.

  This module should be considered experimental.
  """

  @doc """
  Receives a string and returns the cursor context.

  This function receives a string with an Elixir code fragment,
  representing a cursor position, and based on the string, it
  provides contextual information about said position. The
  return of this function can then be used to provide tips,
  suggestions, and autocompletion functionality.

  This function provides a best-effort detection and may not be
  accurate under all circumstances. See the "Limitations"
  section below.

  Consider adding a catch-all clause when handling the return
  type of this function as new cursor information may be added
  in future releases.

  ## Examples

      iex> Code.Fragment.cursor_context("")
      :expr

      iex> Code.Fragment.cursor_context("hello_wor")
      {:local_or_var, 'hello_wor'}

  ## Return values

    * `{:alias, charlist}` - the context is an alias, potentially
      a nested one, such as `Hello.Wor` or `HelloWor`

    * `{:dot, inside_dot, charlist}` - the context is a dot
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, this may either be a remote call or a map
      field access. Examples are `Hello.wor`, `:hello.wor`, `hello.wor`,
      `Hello.nested.wor`, `hello.nested.wor`, and `@hello.world`

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

    * `{:module_attribute, charlist}` - the context is a module attribute, such
      as `@hello_wor`

    * `{:operator, charlist}` (since v1.13.0) - the context is an operator,
      such as `+` or `==`. Note textual operators, such as `when` do not
      appear as operators but rather as `:local_or_var`. `@` is never an
      `:operator` and always a `:module_attribute`

    * `{:operator_arity, charlist}` (since v1.13.0)  - the context is an
      operator arity, which is an operator followed by /, such as `+/`,
      `not/` or `when/`

    * `{:operator_call, charlist}` (since v1.13.0)  - the context is an
      operator call, which is an operator followed by space, such as
      `left + `, `not ` or `x when `

    * `:none` - no context possible

    * `:unquoted_atom` - the context is an unquoted atom. This can be any atom
      or an atom representing a module

  ## Limitations

    * The current algorithm only considers the last line of the input
    * Context does not yet track strings and sigils
    * Arguments of functions calls are not currently recognized

  """
  @doc since: "1.13.0"
  @spec cursor_context(List.Chars.t(), keyword()) ::
          {:alias, charlist}
          | {:dot, inside_dot, charlist}
          | {:dot_arity, inside_dot, charlist}
          | {:dot_call, inside_dot, charlist}
          | :expr
          | {:local_or_var, charlist}
          | {:local_arity, charlist}
          | {:local_call, charlist}
          | {:module_attribute, charlist}
          | {:operator, charlist}
          | {:operator_arity, charlist}
          | {:operator_call, charlist}
          | :none
          | {:unquoted_atom, charlist}
        when inside_dot:
               {:alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:module_attribute, charlist}
               | {:unquoted_atom, charlist}
               | {:var, charlist}
  def cursor_context(string, opts \\ [])

  def cursor_context(binary, opts) when is_binary(binary) and is_list(opts) do
    binary =
      case :binary.matches(binary, "\n") do
        [] ->
          binary

        matches ->
          {position, _} = List.last(matches)
          binary_part(binary, position + 1, byte_size(binary) - position - 1)
      end

    do_cursor_context(String.to_charlist(binary), opts)
  end

  def cursor_context(charlist, opts) when is_list(charlist) and is_list(opts) do
    chunked = Enum.chunk_by(charlist, &(&1 == ?\n))

    case List.last(chunked, []) do
      [?\n | _] -> do_cursor_context([], opts)
      rest -> do_cursor_context(rest, opts)
    end
  end

  def cursor_context(other, opts) do
    cursor_context(to_charlist(other), opts)
  end

  @operators '\\<>+-*/:=|&~^%!'
  @starter_punctuation ',([{;'
  @non_starter_punctuation ')]}"\'.'
  @space '\t\s'
  @trailing_identifier '?!'

  @non_identifier @trailing_identifier ++
                    @operators ++ @starter_punctuation ++ @non_starter_punctuation ++ @space

  @incomplete_operators ['^^', '~~', '~']
  @textual_operators ['when', 'not', 'and', 'or']

  defp do_cursor_context(list, _opts) do
    reverse = Enum.reverse(list)

    case strip_spaces(reverse, 0) do
      # It is empty
      {[], _} -> :expr
      # Token/AST only operators
      {[?>, ?= | rest], _} when rest == [] or hd(rest) != ?: -> :expr
      {[?>, ?- | rest], _} when rest == [] or hd(rest) != ?: -> :expr
      # Two-digit containers
      {[?<, ?< | rest], _} when rest == [] or hd(rest) != ?< -> :expr
      # Ambiguity around :
      {[?: | rest], spaces} when rest == [] or hd(rest) != ?: -> unquoted_atom_or_expr(spaces)
      # Dots
      {[?.], _} -> :none
      {[?. | rest], _} when hd(rest) not in '.:' -> dot(rest, '')
      # It is a local or remote call with parens
      {[?( | rest], spaces} -> call_to_cursor_context(strip_spaces(rest, spaces + 1))
      # A local arity definition
      {[?/ | rest], spaces} -> arity_to_cursor_context(strip_spaces(rest, spaces + 1))
      # Starting a new expression
      {[h | _], _} when h in @starter_punctuation -> :expr
      # It is a local or remote call without parens
      {rest, spaces} when spaces > 0 -> call_to_cursor_context({rest, spaces})
      # It is an identifier
      _ -> identifier_to_cursor_context(reverse, false)
    end
  end

  defp strip_spaces([h | rest], count) when h in @space, do: strip_spaces(rest, count + 1)
  defp strip_spaces(rest, count), do: {rest, count}

  defp unquoted_atom_or_expr(0), do: {:unquoted_atom, ''}
  defp unquoted_atom_or_expr(_), do: :expr

  defp arity_to_cursor_context({reverse, _}) do
    case identifier_to_cursor_context(reverse, true) do
      {:local_or_var, acc} -> {:local_arity, acc}
      {:operator, acc} -> {:operator_arity, acc}
      {:dot, base, acc} -> {:dot_arity, base, acc}
      _ -> :none
    end
  end

  defp call_to_cursor_context({reverse, _}) do
    case identifier_to_cursor_context(reverse, true) do
      {:local_or_var, acc} -> {:local_call, acc}
      {:dot, base, acc} -> {:dot_call, base, acc}
      {:operator, acc} -> {:operator_call, acc}
      _ -> :none
    end
  end

  defp identifier_to_cursor_context([?., ?., ?: | _], _), do: {:unquoted_atom, '..'}
  defp identifier_to_cursor_context([?., ?., ?. | _], _), do: {:local_or_var, '...'}
  defp identifier_to_cursor_context([?., ?: | _], _), do: {:unquoted_atom, '.'}
  defp identifier_to_cursor_context([?., ?. | _], _), do: {:operator, '..'}

  defp identifier_to_cursor_context(reverse, call_op?) do
    case identifier(reverse) do
      :none -> :none
      :operator -> operator(reverse, [], call_op?)
      {:module_attribute, acc} -> {:module_attribute, acc}
      {:unquoted_atom, acc} -> {:unquoted_atom, acc}
      {:alias, '.' ++ rest, acc} when rest == [] or hd(rest) != ?. -> nested_alias(rest, acc)
      {:identifier, '.' ++ rest, acc} when rest == [] or hd(rest) != ?. -> dot(rest, acc)
      {:alias, _, acc} -> {:alias, acc}
      {:identifier, _, acc} when call_op? and acc in @textual_operators -> {:operator, acc}
      {:identifier, _, acc} -> {:local_or_var, acc}
    end
  end

  defp identifier([?? | rest]), do: check_identifier(rest, [??])
  defp identifier([?! | rest]), do: check_identifier(rest, [?!])
  defp identifier(rest), do: check_identifier(rest, [])

  defp check_identifier([h | _], _acc) when h in @operators, do: :operator
  defp check_identifier([h | _], _acc) when h in @non_identifier, do: :none
  defp check_identifier([], _acc), do: :operator
  defp check_identifier(rest, acc), do: rest_identifier(rest, acc)

  defp rest_identifier([h | rest], acc) when h not in @non_identifier do
    rest_identifier(rest, [h | acc])
  end

  defp rest_identifier(rest, [?@ | acc]) do
    case tokenize_identifier(rest, acc) do
      {:identifier, _rest, acc} -> {:module_attribute, acc}
      :none when acc == [] -> {:module_attribute, ''}
      _ -> :none
    end
  end

  defp rest_identifier([?: | rest], acc) when rest == [] or hd(rest) != ?: do
    case String.Tokenizer.tokenize(acc) do
      {_, _, [], _, _, _} -> {:unquoted_atom, acc}
      _ -> :none
    end
  end

  defp rest_identifier([?? | _], _acc) do
    :none
  end

  defp rest_identifier(rest, acc) do
    tokenize_identifier(rest, acc)
  end

  defp tokenize_identifier(rest, acc) do
    case String.Tokenizer.tokenize(acc) do
      # Not actually an atom cause rest is not a :
      {:atom, _, _, _, _, _} ->
        :none

      # Aliases must be ascii only
      {:alias, _, _, _, false, _} ->
        :none

      {kind, _, [], _, _, extra} ->
        if ?@ in extra do
          :none
        else
          {rest, _} = strip_spaces(rest, 0)
          {kind, rest, acc}
        end

      _ ->
        :none
    end
  end

  defp nested_alias(rest, acc) do
    {rest, _} = strip_spaces(rest, 0)

    case identifier_to_cursor_context(rest, true) do
      {:alias, prev} -> {:alias, prev ++ '.' ++ acc}
      _ -> :none
    end
  end

  defp dot(rest, acc) do
    {rest, _} = strip_spaces(rest, 0)

    case identifier_to_cursor_context(rest, true) do
      {:local_or_var, prev} -> {:dot, {:var, prev}, acc}
      {:unquoted_atom, _} = prev -> {:dot, prev, acc}
      {:alias, _} = prev -> {:dot, prev, acc}
      {:dot, _, _} = prev -> {:dot, prev, acc}
      {:module_attribute, _} = prev -> {:dot, prev, acc}
      _ -> :none
    end
  end

  defp operator([h | rest], acc, call_op?) when h in @operators do
    operator(rest, [h | acc], call_op?)
  end

  defp operator(rest, acc, call_op?) when acc in @incomplete_operators do
    {rest, _} = strip_spaces(rest, 0)

    cond do
      call_op? -> :none
      match?([?. | rest] when rest == [] or hd(rest) != ?., rest) -> dot(tl(rest), acc)
      true -> {:operator, acc}
    end
  end

  defp operator(rest, acc, _call_op?) do
    case :elixir_tokenizer.tokenize(acc, 1, 1, []) do
      {:ok, _, [{:atom, _, _}]} ->
        {:unquoted_atom, tl(acc)}

      {:ok, _, [{_, _, op}]} ->
        {rest, _} = strip_spaces(rest, 0)

        cond do
          Code.Identifier.unary_op(op) == :error and Code.Identifier.binary_op(op) == :error ->
            :none

          match?([?. | rest] when rest == [] or hd(rest) != ?., rest) ->
            dot(tl(rest), acc)

          true ->
            {:operator, acc}
        end

      _ ->
        :none
    end
  end
end
