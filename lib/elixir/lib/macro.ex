import Kernel, except: [to_string: 1]

defmodule Macro do
  @moduledoc """
  This module provides conveniences for working with macros.
  """

  @typedoc "Abstract Syntax Tree (AST) node"
  @type t :: { t, t } | { t, Keyword.t, t } | atom | number | binary | list

  @doc false
  defmacro binary_ops do
    [ :===, :!==,
      :==, :!=, :<=, :>=,
      :&&, :||, :<>, :++, :--, :**, ://, :::, :<-, :.., :|>, :=~,
      :<, :>, :->,
      :+, :-, :*, :/, :=, :|, :.,
      :and, :or, :xor, :when, :in, :inlist, :inbits,
      :<<<, :>>>, :|||, :&&&, :^^^, :~~~ ]
  end

  @doc false
  defmacro unary_ops do
    [:!, :@, :^, :not, :+, :-, :~~~, :&]
  end

  @spec binary_op_props(atom) :: { :left | :right, precedence :: integer }
  defp binary_op_props(o) do
    case o do
      :::                                                       -> {:right, 30}
      :when                                                     -> {:right, 40}
      o when o in [:inlist, :inbits]                            -> {:left, 50}
      ://                                                       -> {:right, 60}
      :|                                                        -> {:left, 70}
      :=                                                        -> {:right, 80}
      o when o in [:||, :|||, :or, :xor]                        -> {:left, 130}
      o when o in [:&&, :&&&, :and]                             -> {:left, 140}
      o when o in [:==, :!=, :<, :<=, :>=, :>, :=~, :===, :!==] -> {:left, 150}
      o when o in [:<-, :|>, :<<<, :>>>]                        -> {:right, 160}
      :in                                                       -> {:left, 170}
      :..                                                       -> {:left, 200}
      o when o in [:+, :-]                                      -> {:left, 210}
      o when o in [:*, :/]                                      -> {:left, 220}
      o when o in [:<>]                                         -> {:right, 230}
      :^^^                                                      -> {:left, 250}
      :.                                                        -> {:left, 310}
    end
  end

  @doc """
  Breaks a pipeline expression into a list. Raises if
  the pipeline is ill-formed.
  """
  @spec unpipe(Macro.t) :: [Macro.t]
  def unpipe({ :|> , _, [left, right] }) do
    [left|unpipe(right)]
  end

  def unpipe(other) do
    [other]
  end

  @doc """
  Pipes the given `expr` in to the `call_expr` as the
  argument in the given `position`.
  """
  @spec pipe(Macro.t, Macro.t, integer) :: Macro.t | no_return
  def pipe(expr, call_args, integer // 0)

  def pipe(expr, { call, line, atom }, integer) when is_atom(atom) do
    { call, line, List.insert_at([], integer, expr) }
  end

  def pipe(expr, { call, line, args }, integer) when is_list(args) do
    { call, line, List.insert_at(args, integer, expr) }
  end

  def pipe(expr, call_args, _integer) do
    raise ArgumentError,
      message: "cannot pipe #{to_string expr} into #{to_string call_args}"
  end

  @doc """
  Receives an expression representing a possible definition
  and extracts its arguments. It returns a tuple with the
  function name and the arguments list or `:error` if not
  a valid call syntax.

  This is useful for macros that want to provide the same
  argument syntax available in def/defp/defmacro and friends.

  ## Examples

      extract_args(quote do: foo)        == { :foo, [] }
      extract_args(quote do: foo())      == { :foo, [] }
      extract_args(quote do: foo(1, 2, 3)) == { :foo, [1, 2, 3] }
      extract_args(quote do: 1.(1, 2, 3))  == :error

  """
  @spec extract_args(Macro.t) :: { atom, [Macro.t] } | :error
  def extract_args(expr) do
    :elixir_clauses.extract_args(expr)
  end

  @doc """
  Recursively escapes a value so it can be inserted
  into a syntax tree.

  One may pass `unquote: true` to `escape/2`
  which leaves unquote statements unescaped, effectively
  unquoting the contents on escape.

  ## Examples

      iex> Macro.escape(:foo)
      :foo

      iex> Macro.escape({ :a, :b, :c })
      { :{}, [], [:a, :b, :c] }

      iex> Macro.escape({ :unquote, [], [1] }, unquote: true)
      1

  """
  @spec escape(term) :: Macro.t
  @spec escape(term, Keyword.t) :: Macro.t
  def escape(expr, opts // []) do
    elem(:elixir_quote.escape(expr, Keyword.get(opts, :unquote, false)), 0)
  end

  @doc %S"""
  Unescape the given chars. This is the unescaping behavior
  used by default in Elixir single- and double-quoted strings.
  Check `unescape_string/2` for information on how to customize
  the escaping map.

  In this setup, Elixir will escape the following: `\a`, `\b`,
  `\d`, `\e`, `\f`, `\n`, `\r`, `\s`, `\t` and `\v`. Octals are
  also escaped according to the latin1 set they represent.

  This function is commonly used on sigil implementations
  (like `%r`, `%b` and others) which receive a raw, unescaped
  string.

  ## Examples

      iex> Macro.unescape_string("example\\n")
      "example\n"

  In the example above, we pass a string with `\n` escaped
  and we return a version with it unescaped.
  """
  @spec unescape_string(String.t) :: String.t
  def unescape_string(chars) do
    :elixir_interpolation.unescape_chars(chars)
  end

  @doc %S"""
  Unescape the given chars according to the map given.
  Check `unescape_string/1` if you want to use the same map
  as Elixir single- and double-quoted strings.

  ## Map

  The map must be a function. The function receives an integer
  representing the number of the characters it wants to unescape.
  Here is the default mapping function implemented by Elixir:

      def unescape_map(?a), do: ?\a
      def unescape_map(?b), do: ?\b
      def unescape_map(?d), do: ?\d
      def unescape_map(?e), do: ?\e
      def unescape_map(?f), do: ?\f
      def unescape_map(?n), do: ?\n
      def unescape_map(?r), do: ?\r
      def unescape_map(?s), do: ?\s
      def unescape_map(?t), do: ?\t
      def unescape_map(?v), do: ?\v
      def unescape_map(e),  do: e

  If the `unescape_map` function returns `false`. The char is
  not escaped and `\` is kept in the char list.

  ## Octals

  Octals will by default be escaped unless the map function
  returns false for ?0.

  ## Hex

  Hexadecimals will by default be escaped unless the map function
  returns false for ?x.

  ## Examples

  Using the unescape_map defined above is easy:

      Macro.unescape_string "example\\n", &unescape_map(&1)

  """
  @spec unescape_string(String.t, (non_neg_integer -> non_neg_integer | false)) :: String.t
  def unescape_string(chars, map) do
    :elixir_interpolation.unescape_chars(chars, map)
  end

  @doc """
  Unescape the given tokens according to the default map.
  Check `unescape_string/1` and `unescape_string/2` for more
  information about unescaping.

  Only tokens that are binaries are unescaped, all others are
  ignored. This function is useful when implementing your own
  sigils. Check the implementation of `Kernel.sigil_b`
  for examples.
  """
  @spec unescape_tokens([Macro.t]) :: [Macro.t]
  def unescape_tokens(tokens) do
    :elixir_interpolation.unescape_tokens(tokens)
  end

  @doc """
  Unescape the given tokens according to the given map.
  Check `unescape_tokens/1` and `unescape_string/2` for more information.
  """
  @spec unescape_tokens([Macro.t], (non_neg_integer -> non_neg_integer | false)) :: [Macro.t]
  def unescape_tokens(tokens, map) do
    :elixir_interpolation.unescape_tokens(tokens, map)
  end

  @doc """
  Converts the given expression to a binary.

  ## Examples

      iex> Macro.to_string(quote do: foo.bar(1, 2, 3))
      "foo.bar(1, 2, 3)"

  """
  @spec to_string(Macro.t) :: String.t
  @spec to_string(Macro.t, (Macro.t, String.t -> String.t)) :: String.t
  def to_string(tree, fun // fn(_ast, string) -> string end)

  # Variables
  def to_string({ var, _, atom } = ast, fun) when is_atom(atom) do
    fun.(ast, atom_to_binary(var))
  end

  # Aliases
  def to_string({ :__aliases__, _, refs } = ast, fun) do
    fun.(ast, Enum.map_join(refs, ".", &call_to_string(&1, fun)))
  end

  # Blocks
  def to_string({ :__block__, _, [expr] } = ast, fun) do
    fun.(ast, to_string(expr, fun))
  end

  def to_string({ :__block__, _, _ } = ast, fun) do
    block = adjust_new_lines block_to_string(ast, fun), "\n  "
    fun.(ast, "(\n  " <> block <> "\n)")
  end

  # Bits containers
  def to_string({ :<<>>, _, args } = ast, fun) do
    fun.(ast, case Enum.map_join(args, ", ", &to_string(&1, fun)) do
      "<" <> rest -> "<< <" <> rest  <> " >>"
      rest -> "<<" <> rest <> ">>"
    end)
  end

  # Tuple containers
  def to_string({ :{}, _, args } = ast, fun) do
    fun.(ast, "{" <> Enum.map_join(args, ", ", &to_string(&1, fun)) <> "}")
  end

  # Fn keyword
  def to_string({ :fn, _, [{ :->, _, [{_, _, tuple}] } = arrow] } = ast, fun)
      when not is_tuple(tuple) or elem(tuple, 0) != :__block__ do
    fun.(ast, "fn " <> arrow_to_string(arrow, fun) <> " end")
  end

  def to_string({ :fn, _, [{ :->, _, [_] } = block] } = ast, fun) do
    fun.(ast, "fn " <> block_to_string(block, fun) <> "\nend")
  end

  def to_string({ :fn, _, [block] } = ast, fun) do
    block = adjust_new_lines block_to_string(block, fun), "\n  "
    fun.(ast, "fn\n  " <> block <> "\nend")
  end

  # left -> right
  def to_string({ :->, _, _ } = ast, fun) do
    fun.(ast, "(" <> arrow_to_string(ast, fun, true) <> ")")
  end

  # Binary ops
  def to_string({ op, _, [left, right] } = ast, fun) when op in binary_ops do
    fun.(ast, op_to_string(left, fun, op, :left) <> " #{op} " <> op_to_string(right, fun, op, :right))
  end

  # Splat when
  def to_string({ :when, _, args } = ast, fun) do
    { left, right } = :elixir_utils.split_last(args)
    fun.(ast, "(" <> Enum.map_join(left, ", ", &to_string(&1, fun)) <> ") when " <> to_string(right, fun))
  end

  # Unary ops
  def to_string({ :not, _, [arg] } = ast, fun)  do
    fun.(ast, "not " <> to_string(arg, fun))
  end

  def to_string({ op, _, [arg] } = ast, fun) when op in unary_ops do
    fun.(ast, atom_to_binary(op) <> to_string(arg, fun))
  end

  # Access
  def to_string({ { :., _, [Kernel, :access] }, _, [left, right] } = ast, fun) do
    fun.(ast, to_string(left, fun) <> to_string(right, fun))
  end

  # All other calls
  def to_string({ target, _, args } = ast, fun) when is_list(args) do
    { list, last } = :elixir_utils.split_last(args)
    fun.(ast, case is_kw_blocks?(last) do
      true  -> call_to_string_with_args(target, list, fun) <> kw_blocks_to_string(last, fun)
      false -> call_to_string_with_args(target, args, fun)
    end)
  end

  # Two-item tuples
  def to_string({ left, right }, fun) do
    to_string({ :{}, [], [left, right] }, fun)
  end

  # Lists
  def to_string(list, fun) when is_list(list) do
    if Keyword.keyword?(list) do
      fun.(list, "[" <> kw_list_to_string(list, fun) <> "]")
    else
      fun.(list, "[" <> Enum.map_join(list, ", ", &to_string(&1, fun)) <> "]")
    end
  end

  # All other structures
  def to_string(other, fun), do: fun.(other, inspect(other, raw: true))

  # Block keywords
  defmacrop kw_keywords, do: [:do, :catch, :rescue, :after, :else]

  defp is_kw_blocks?([_|_] = kw) do
    Enum.all?(kw, &match?({x, _} when x in kw_keywords, &1))
  end
  defp is_kw_blocks?(_), do: false

  defp module_to_string(atom, _fun) when is_atom(atom), do: inspect(atom, raw: true)
  defp module_to_string(other, fun), do: call_to_string(other, fun)

  defp call_to_string(atom, _fun) when is_atom(atom), do: atom_to_binary(atom)
  defp call_to_string({ :., _, [arg] }, fun),         do: module_to_string(arg, fun) <> "."
  defp call_to_string({ :., _, [left, right] }, fun), do: module_to_string(left, fun) <> "." <> call_to_string(right, fun)
  defp call_to_string(other, fun),                    do: to_string(other, fun)

  defp call_to_string_with_args(target, args, fun) do
    { list, last } = :elixir_utils.split_last(args)
    target = call_to_string(target, fun)

    case last != [] and Keyword.keyword?(last) do
      true  ->
        args = Enum.map_join(list, ", ", &to_string(&1, fun))
        if list != [], do: args = args <> ", "
        args = args <> kw_list_to_string(last, fun)
        target <> "(" <> args <> ")"
      false ->
        args = Enum.map_join(args, ", ", &to_string(&1, fun))
        target <> "(" <> args <> ")"
    end
  end

  defp kw_blocks_to_string(kw, fun) do
    Enum.reduce(kw_keywords, " ", fn(x, acc) ->
      case Keyword.has_key?(kw, x) do
        true  -> acc <> kw_block_to_string(x, Keyword.get(kw, x), fun)
        false -> acc
      end
    end) <> "end"
  end

  defp kw_block_to_string(key, value, fun) do
    block = adjust_new_lines block_to_string(value, fun), "\n  "
    atom_to_binary(key) <> "\n  " <> block <> "\n"
  end

  defp block_to_string({ :->, _, exprs }, fun) do
    Enum.map_join(exprs, "\n", fn({ left, _, right }) ->
      left = comma_join_or_empty_paren(left, fun, false)
      left <> "->\n  " <> adjust_new_lines block_to_string(right, fun), "\n  "
    end)
  end

  defp block_to_string({ :__block__, _, exprs }, fun) do
    Enum.map_join(exprs, "\n", &to_string(&1, fun))
  end

  defp block_to_string(other, fun), do: to_string(other, fun)

  defp kw_list_to_string(list, fun) do
    Enum.map_join(list, ", ", fn { key, value } ->
      atom_to_binary(key) <> ": " <> to_string(value, fun)
    end)
  end

  defp parenthise(expr, fun) do
    "(" <> to_string(expr, fun) <> ")"
  end

  defp op_to_string({ op, _, [_, _] } = expr, fun, parent_op, side) when op in binary_ops do
    { parent_assoc, parent_prec } = binary_op_props(parent_op)
    { _, prec }                   = binary_op_props(op)
    cond do
      parent_prec < prec -> to_string(expr, fun)
      parent_prec > prec -> parenthise(expr, fun)
      true ->
        # parent_prec == prec, so look at associativity.
        if parent_assoc == side do
          to_string(expr, fun)
        else
          parenthise(expr, fun)
        end
    end
  end

  defp op_to_string(expr, fun, _, _), do: to_string(expr, fun)

  defp arrow_to_string({ :->, _, pairs }, fun, paren // false) do
    Enum.map_join(pairs, "; ", fn({ left, _, right }) ->
      left = comma_join_or_empty_paren(left, fun, paren)
      left <> "-> " <> to_string(right, fun)
    end)
  end

  defp comma_join_or_empty_paren([], _fun, true),  do: "() "
  defp comma_join_or_empty_paren([], _fun, false), do: ""

  defp comma_join_or_empty_paren(left, fun, _) do
    Enum.map_join(left, ", ", &to_string(&1, fun)) <> " "
  end

  defp adjust_new_lines(block, replacement) do
    bc <<x>> inbits block do
      << case x == ?\n do
        true  -> replacement
        false -> <<x>>
      end :: binary >>
    end
  end

  @doc """
  Receives a AST node and expands it once. The following contents are expanded:

  * Macros (local or remote);
  * Aliases are expanded (if possible) and return atoms;
  * All pseudo-variables (`__FILE__`, `__MODULE__`, etc);
  * Module attributes reader (`@foo`);

  In case the expression cannot be expanded, it returns the expression
  itself. Notice that `expand_once/2` performs the expansion just
  once and it is not recursive. Check `expand/2` for expansion
  until the node no longer represents a macro and `expand_all/2`
  for recursive expansion.

  ## Examples

  In the example below, we have a macro that generates a module
  with a function named `name_length` that returns the length
  of the module name. The value of this function will be calculated
  at compilation time and not at runtime.

  Consider the implementation below:

      defmacro defmodule_with_length(name, do: block) do
        length = length(atom_to_list(name))

        quote do
          defmodule unquote(name) do
            def name_length, do: unquote(length)
            unquote(block)
          end
        end
      end

  When invoked like this:

      defmodule_with_length My.Module do
        def other_function, do: ...
      end

  The compilation will fail because `My.Module` when quoted
  is not an atom, but a syntax tree as follow:

      {:__aliases__, [], [:My, :Module] }

  That said, we need to expand the aliases node above to an
  atom, so we can retrieve its length. Expanding the node is
  not straight-forward because we also need to expand the
  caller aliases. For example:

      alias MyHelpers, as: My

      defmodule_with_length My.Module do
        def other_function, do: ...
      end

  The final module name will be `MyHelpers.Module` and not
  `My.Module`. With `Macro.expand`, such aliases are taken
  into consideration. Local and remote macros are also
  expanded. We could rewrite our macro above to use this
  function as:

      defmacro defmodule_with_length(name, do: block) do
        expanded = Macro.expand(name, __CALLER__)
        length   = length(atom_to_list(expanded))

        quote do
          defmodule unquote(name) do
            def name_length, do: unquote(length)
            unquote(block)
          end
        end
      end

  """
  def expand_once(ast, env) do
    elem(expand_once(ast, env, nil), 0)
  end

  defp expand_once({ :__aliases__, _, _ } = original, env, cache) do
    case :elixir_aliases.expand(original, env.aliases, env.macro_aliases) do
      receiver when is_atom(receiver) ->
        :elixir_lexical.record_alias(env.file, receiver)
        { receiver, true, cache }
      aliases ->
        aliases = lc alias inlist aliases, do: elem(expand_once(alias, env, cache), 0)

        case :lists.all(&is_atom/1, aliases) do
          true ->
            receiver = :elixir_aliases.concat(aliases)
            :elixir_lexical.record_alias(env.file, receiver)
            { receiver, true, cache }
          false -> { original, false, cache }
        end
    end
  end

  # Expand @ calls
  defp expand_once({ :@, _, [{ name, _, args }] } = original, env, cache) when is_atom(args) or args == [] do
    case (module = env.module) && Module.open?(module) do
      true  -> { Module.get_attribute(module, name), true, cache }
      false -> { original, false, cache }
    end
  end

  # Expand pseudo-variables
  defp expand_once({ :__MODULE__, _, atom }, env, cache) when is_atom(atom),
    do: { env.module, true, cache }
  defp expand_once({ :__FILE__, _, atom }, env, cache)   when is_atom(atom),
    do: { env.file, true, cache }
  defp expand_once({ :__DIR__, _, atom }, env, cache)    when is_atom(atom),
    do: { :filename.dirname(env.file), true, cache }
  defp expand_once({ :__ENV__, _, atom }, env, cache)    when is_atom(atom),
    do: { env, true, cache }

  # Expand possible macro import invocation
  defp expand_once({ atom, line, args } = original, env, cache) when is_atom(atom) do
    args = case is_atom(args) do
      true  -> []
      false -> args
    end

    case not is_partial?(args) do
      false -> { original, false, cache }
      true  ->
        module = env.module

        extra  = if function_exported?(module, :__info__, 1) do
          [{ module, module.__info__(:macros) }]
        else
          []
        end

        cache  = to_erl_env(env, cache)
        expand = :elixir_dispatch.expand_import(line, { atom, length(args) }, args,
          env.module, extra, cache)

        case expand do
          { :ok, _, expanded } -> { expanded, true, cache }
          { :error, _ }        -> { original, false, cache }
        end
    end
  end

  # Expand possible macro require invocation
  defp expand_once({ { :., _, [left, right] }, line, args } = original, env, cache) when is_atom(right) do
    { receiver, _, _ } = expand_once(left, env, cache)

    case is_atom(receiver) and not is_partial?(args) do
      false -> { original, false, cache }
      true  ->
        cache  = to_erl_env(env, cache)
        expand = :elixir_dispatch.expand_require(line, receiver, { right, length(args) },
          args, env.module, cache)

        case expand do
          { :ok, _receiver, expanded } -> { expanded, true, cache }
          { :error, _ }                -> { original, false, cache }
        end
    end
  end

  # Anything else is just returned
  defp expand_once(other, _env, cache), do: { other, false, cache }

  defp to_erl_env(env, nil),    do: :elixir_scope.to_erl_env(env)
  defp to_erl_env(_env, cache), do: cache

  defp is_partial?(args) do
    :lists.any(&match?({ :&, _, [_] }, &1), args)
  end

  @doc """
  Receives a AST node and expands it until it no longer represents
  a macro. Check `expand_once/2` for more information on how
  expansion works and `expand_all/2` for recursive expansion.
  """
  def expand(tree, env) do
    elem(expand(tree, env, nil), 0)
  end

  @doc false # Used internally by Elixir
  def expand(tree, env, cache) do
    expand_until({ tree, true, cache }, env)
  end

  defp expand_until({ tree, true, cache }, env) do
    expand_until(expand_once(tree, env, cache), env)
  end

  defp expand_until({ tree, false, cache }, _env) do
    { tree, cache }
  end

  @doc false
  def expand_all(tree, env) do
    IO.write "Macro.expand_all/2 is deprecated, please avoid recursive code expansion\n#{Exception.format_stacktrace}"
    elem(expand_all(tree, env, nil), 0)
  end

  @doc false # Used internally by Elixir
  def expand_all(tree, env, cache) do
    expand_all_until(expand(tree, env, cache), env)
  end

  defp expand_all_until({ { left, meta, right }, cache }, env) do
    { left, cache }  = expand_all(left, env, cache)
    { right, cache } = expand_all(right, env, cache)
    { { left, meta, right }, cache }
  end

  defp expand_all_until({ { left, right }, cache }, env) do
    { left, cache }  = expand_all(left, env, cache)
    { right, cache } = expand_all(right, env, cache)
    { { left, right }, cache }
  end

  defp expand_all_until({ list, cache }, env) when is_list(list) do
    :lists.mapfoldl(&expand_all(&1, env, &2), cache, list)
  end

  defp expand_all_until({ other, cache }, _env) do
    { other, cache }
  end

  @doc """
  Recurs the quoted expression checking if all sub-terms are
  safe (i.e. they represent data structures and don't actually
  evaluate code) and returns `:ok` unless a given term is unsafe,
  which is returned as `{ :unsafe, term }`.
  """
  def safe_term(terms) do
    do_safe_term(terms) || :ok
  end

  defp do_safe_term({ local, _, terms }) when local in [:{}, :__aliases__] do
    do_safe_term(terms)
  end

  defp do_safe_term({ unary, _, [term] }) when unary in [:+, :-] do
    do_safe_term(term)
  end

  defp do_safe_term({ left, right }), do: do_safe_term(left) || do_safe_term(right)
  defp do_safe_term(terms) when is_list(terms),  do: Enum.find_value(terms, &do_safe_term(&1))
  defp do_safe_term(terms) when is_tuple(terms), do: { :unsafe, terms }
  defp do_safe_term(_), do: nil
end
