import Kernel, except: [to_string: 1]

defmodule Macro do
  @moduledoc """
  Conveniences for working with macros.

  ## Custom Sigils

  To create a custom sigil, define a function with the name
  `sigil_{identifier}` that takes two arguments. The first argument will be
  the interpolated string, the second will be a char list containing any
  modifiers.

  Valid modifiers include only lower and upper case letters. Other characters
  will cause a syntax error.

  The module containing the custom sigil must be imported before the sigil
  syntax can be used.
  """

  @typedoc "Abstract Syntax Tree (AST)"
  @type t :: expr | {t, t} | atom | number | binary | pid | fun | [t]

  @typedoc "Expr node (remaining ones are literals)"
  @type expr :: {expr | atom, Keyword.t, atom | [t]}

  @binary_ops [:===, :!==,
    :==, :!=, :<=, :>=,
    :&&, :||, :<>, :++, :--, :\\, :::, :<-, :.., :|>, :=~,
    :<, :>, :->,
    :+, :-, :*, :/, :=, :|, :.,
    :and, :or, :when, :in,
    :~>>, :<<~, :~>, :<~, :<~>, :<|>,
    :<<<, :>>>, :|||, :&&&, :^^^, :~~~]

  @doc false
  defmacro binary_ops, do: @binary_ops

  @unary_ops [:!, :@, :^, :not, :+, :-, :~~~, :&]

  @doc false
  defmacro unary_ops, do: @unary_ops

  @spec binary_op_props(atom) :: {:left | :right, precedence :: integer}
  defp binary_op_props(o) do
    case o do
      o when o in [:<-, :\\]                  -> {:left,  40}
      :when                                   -> {:right, 50}
      :::                                     -> {:right, 60}
      :|                                      -> {:right, 70}
      :=                                      -> {:right, 90}
      o when o in [:||, :|||, :or]            -> {:left, 130}
      o when o in [:&&, :&&&, :and]           -> {:left, 140}
      o when o in [:==, :!=, :=~, :===, :!==] -> {:left, 150}
      o when o in [:<, :<=, :>=, :>]          -> {:left, 160}
      o when o in [:|>, :<<<, :>>>, :<~, :~>,
                :<<~, :~>>, :<~>, :<|>, :^^^] -> {:left, 170}
      :in                                     -> {:left, 180}
      o when o in [:++, :--, :.., :<>]        -> {:right, 200}
      o when o in [:+, :-]                    -> {:left, 210}
      o when o in [:*, :/]                    -> {:left, 220}
      :.                                      -> {:left, 310}
    end
  end

  @doc """
  Breaks a pipeline expression into a list.

  Raises if the pipeline is ill-formed.
  """
  @spec unpipe(Macro.t) :: [Macro.t]
  def unpipe(expr) do
    :lists.reverse(unpipe(expr, []))
  end

  defp unpipe({:|>, _, [left, right]}, acc) do
    unpipe(right, unpipe(left, acc))
  end

  defp unpipe(other, acc) do
    [{other, 0}|acc]
  end

  @doc """
  Pipes `expr` into the `call_args` at the given `position`.
  """
  @spec pipe(Macro.t, Macro.t, integer) :: Macro.t | no_return
  def pipe(expr, call_args, position)

  def pipe(expr, {:&, _, _} = call_args, _integer) do
    bad_pipe(expr, call_args)
  end

  def pipe(expr, {call, _, [_, _]} = call_args, _integer)
      when call in unquote(@binary_ops) do
    raise ArgumentError, "cannot pipe #{to_string expr} into #{to_string call_args}, " <>
      "the #{to_string call} operator can only take two arguments"
  end

  def pipe(expr, {call, line, atom}, integer) when is_atom(atom) do
    {call, line, List.insert_at([], integer, expr)}
  end

  def pipe(expr, {call, line, args}, integer) when is_list(args) do
    {call, line, List.insert_at(args, integer, expr)}
  end

  def pipe(expr, call_args, _integer) do
    bad_pipe(expr, call_args)
  end

  defp bad_pipe(expr, call_args) do
    raise ArgumentError, "cannot pipe #{to_string expr} into #{to_string call_args}, " <>
      "can only pipe into local calls foo(), remote calls Foo.bar() or anonymous functions calls foo.()"
  end

  @doc """
  Applies the given function to the node metadata if it contains one.

  This is often useful when used with `Macro.prewalk/1` to remove
  information like lines and hygienic counters from the expression
  for either storage or comparison.

  ## Examples

      iex> quoted = quote line: 10, do: sample()
      {:sample, [line: 10], []}
      iex> Macro.update_meta(quoted, &Keyword.delete(&1, :line))
      {:sample, [], []}

  """
  @spec update_meta(t, (Keyword.t -> Keyword.t)) :: t
  def update_meta(quoted, fun)

  def update_meta({left, meta, right}, fun) when is_list(meta) do
    {left, fun.(meta), right}
  end

  def update_meta(other, _fun) do
    other
  end

  @doc """
  Generates an AST node representing the variable given
  by the atoms `var` and `context`.

  ## Examples

  In order to build a variable, a context is expected.
  Most of the times, in order to preserve hygiene, the
  context must be `__MODULE__`:

      iex> Macro.var(:foo, __MODULE__)
      {:foo, [], __MODULE__}

  However, if there is a need to access the user variable,
  nil can be given:

      iex> Macro.var(:foo, nil)
      {:foo, [], nil}

  """
  @spec var(var, context) :: {var, [], context} when var: atom, context: atom
  def var(var, context) when is_atom(var) and is_atom(context) do
    {var, [], context}
  end

  @doc """
  Performs a depth-first, pre-order traversal of quoted expressions.
  """
  @spec prewalk(t, (t -> t)) :: t
  def prewalk(ast, fun) when is_function(fun, 1) do
    elem(prewalk(ast, nil, fn x, nil -> {fun.(x), nil} end), 0)
  end

  @doc """
  Performs a depth-first, pre-order traversal of quoted expressions
  using an accumulator.
  """
  @spec prewalk(t, any, (t, any -> {t, any})) :: {t, any}
  def prewalk(ast, acc, fun) when is_function(fun, 2) do
    {ast, acc} = fun.(ast, acc)
    do_prewalk(ast, acc, fun)
  end

  defp do_prewalk({form, meta, args}, acc, fun) do
    unless is_atom(form) do
      {form, acc} = fun.(form, acc)
      {form, acc} = do_prewalk(form, acc, fun)
    end

    unless is_atom(args) do
      {args, acc} = Enum.map_reduce(args, acc, fn x, acc ->
        {x, acc} = fun.(x, acc)
        do_prewalk(x, acc, fun)
      end)
    end

    {{form, meta, args}, acc}
  end

  defp do_prewalk({left, right}, acc, fun) do
    {left, acc} = fun.(left, acc)
    {left, acc} = do_prewalk(left, acc, fun)
    {right, acc} = fun.(right, acc)
    {right, acc} = do_prewalk(right, acc, fun)
    {{left, right}, acc}
  end

  defp do_prewalk(list, acc, fun) when is_list(list) do
    Enum.map_reduce(list, acc, fn x, acc ->
      {x, acc} = fun.(x, acc)
      do_prewalk(x, acc, fun)
    end)
  end

  defp do_prewalk(x, acc, _fun) do
    {x, acc}
  end

  @doc """
  Performs a depth-first, post-order traversal of quoted expressions.
  """
  @spec postwalk(t, (t -> t)) :: t
  def postwalk(ast, fun) when is_function(fun, 1) do
    elem(postwalk(ast, nil, fn x, nil -> {fun.(x), nil} end), 0)
  end

  @doc """
  Performs a depth-first, post-order traversal of quoted expressions
  using an accumulator.
  """
  @spec postwalk(t, any, (t, any -> {t, any})) :: {t, any}
  def postwalk(ast, acc, fun) when is_function(fun, 2) do
    do_postwalk(ast, acc, fun)
  end

  defp do_postwalk({form, meta, args}, acc, fun) do
    unless is_atom(form) do
      {form, acc} = do_postwalk(form, acc, fun)
    end

    unless is_atom(args) do
      {args, acc} = Enum.map_reduce(args, acc, &do_postwalk(&1, &2, fun))
    end

    fun.({form, meta, args}, acc)
  end

  defp do_postwalk({left, right}, acc, fun) do
    {left, acc} = do_postwalk(left, acc, fun)
    {right, acc} = do_postwalk(right, acc, fun)
    fun.({left, right}, acc)
  end

  defp do_postwalk(list, acc, fun) when is_list(list) do
    {list, acc} = Enum.map_reduce(list, acc, &do_postwalk(&1, &2, fun))
    fun.(list, acc)
  end

  defp do_postwalk(x, acc, fun) do
    fun.(x, acc)
  end

  @doc """
  Decomposes a local or remote call into its remote part (when provided),
  function name and argument list.

  Returns `:error` when an invalid call syntax is provided.

  ## Examples

      iex> Macro.decompose_call(quote do: foo)
      {:foo, []}

      iex> Macro.decompose_call(quote do: foo())
      {:foo, []}

      iex> Macro.decompose_call(quote do: foo(1, 2, 3))
      {:foo, [1, 2, 3]}

      iex> Macro.decompose_call(quote do: Elixir.M.foo(1, 2, 3))
      {{:__aliases__, [], [:Elixir, :M]}, :foo, [1, 2, 3]}

      iex> Macro.decompose_call(quote do: 42)
      :error

  """
  @spec decompose_call(Macro.t) :: {atom, [Macro.t]} | {Macro.t, atom, [Macro.t]} | :error
  def decompose_call(ast)

  def decompose_call({{:., _, [remote, function]}, _, args}) when is_tuple(remote) or is_atom(remote),
    do: {remote, function, args}

  def decompose_call({name, _, args}) when is_atom(name) and is_atom(args),
    do: {name, []}

  def decompose_call({name, _, args}) when is_atom(name) and is_list(args),
    do: {name, args}

  def decompose_call(_),
    do: :error

  @doc """
  Recursively escapes a value so it can be inserted
  into a syntax tree.

  One may pass `unquote: true` to `escape/2`
  which leaves `unquote` statements unescaped, effectively
  unquoting the contents on escape.

  ## Examples

      iex> Macro.escape(:foo)
      :foo

      iex> Macro.escape({:a, :b, :c})
      {:{}, [], [:a, :b, :c]}

      iex> Macro.escape({:unquote, [], [1]}, unquote: true)
      1

  """
  @spec escape(term) :: Macro.t
  @spec escape(term, Keyword.t) :: Macro.t
  def escape(expr, opts \\ []) do
    elem(:elixir_quote.escape(expr, Keyword.get(opts, :unquote, false)), 0)
  end

  @doc """
  Validates the given expressions are valid quoted expressions.

  Check the `type:Macro.t` for the specification of a valid
  quoted expression.
  """
  @spec validate(term) :: :ok | {:error, term}
  def validate(expr) do
    find_invalid(expr) || :ok
  end

  defp find_invalid({left, right}), do:
    find_invalid(left) || find_invalid(right)

  defp find_invalid({left, meta, right}) when is_list(meta) and (is_atom(right) or is_list(right)), do:
    find_invalid(left) || find_invalid(right)

  defp find_invalid(list) when is_list(list), do:
    Enum.find_value(list, &find_invalid/1)

  defp find_invalid(pid)  when is_pid(pid),    do: nil
  defp find_invalid(atom) when is_atom(atom),  do: nil
  defp find_invalid(num)  when is_number(num), do: nil
  defp find_invalid(bin)  when is_binary(bin), do: nil

  defp find_invalid(fun) when is_function(fun) do
    unless :erlang.fun_info(fun, :env) == {:env, []} and
           :erlang.fun_info(fun, :type) == {:type, :external} do
      {:error, fun}
    end
  end

  defp find_invalid(other), do: {:error, other}

  @doc ~S"""
  Unescape the given chars.

  This is the unescaping behaviour used by default in Elixir
  single- and double-quoted strings. Check `unescape_string/2`
  for information on how to customize the escaping map.

  In this setup, Elixir will escape the following: `\0`, `\a`, `\b`,
  `\d`, `\e`, `\f`, `\n`, `\r`, `\s`, `\t` and `\v`. Unicode codepoints
  can be given as hexadecimals via `\xNN` and `\x{NN...}` escapes.

  This function is commonly used on sigil implementations
  (like `~r`, `~s` and others) which receive a raw, unescaped
  string.

  ## Examples

      iex> Macro.unescape_string("example\\n")
      "example\n"

  In the example above, we pass a string with `\n` escaped
  and return a version with it unescaped.
  """
  @spec unescape_string(String.t) :: String.t
  def unescape_string(chars) do
    :elixir_interpolation.unescape_chars(chars)
  end

  @doc ~S"""
  Unescapes the given chars according to the map given.

  Check `unescape_string/1` if you want to use the same map
  as Elixir single- and double-quoted strings.

  ## Map

  The map must be a function. The function receives an integer
  representing the codepoint of the character it wants to unescape.
  Here is the default mapping function implemented by Elixir:

      def unescape_map(?0), do: ?0
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
      def unescape_map(?x), do: true
      def unescape_map(e),  do: e

  If the `unescape_map` function returns `false`. The char is
  not escaped and `\` is kept in the char list.

  Hexadecimals will be escaped if the map function returns `true`
  for `?x`.

  ## Examples

  Using the `unescape_map` function defined above is easy:

      Macro.unescape_string "example\\n", &unescape_map(&1)

  """
  @spec unescape_string(String.t, (non_neg_integer -> non_neg_integer | false)) :: String.t
  def unescape_string(chars, map) do
    :elixir_interpolation.unescape_chars(chars, map)
  end

  @doc """
  Unescapes the given tokens according to the default map.

  Check `unescape_string/1` and `unescape_string/2` for more
  information about unescaping.

  Only tokens that are binaries are unescaped, all others are
  ignored. This function is useful when implementing your own
  sigils. Check the implementation of `Kernel.sigil_s/2`
  for examples.
  """
  @spec unescape_tokens([Macro.t]) :: [Macro.t]
  def unescape_tokens(tokens) do
    :elixir_interpolation.unescape_tokens(tokens)
  end

  @doc """
  Unescapes the given tokens according to the given map.

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
  def to_string(tree, fun \\ fn(_ast, string) -> string end)

  # Variables
  def to_string({var, _, atom} = ast, fun) when is_atom(atom) do
    fun.(ast, Atom.to_string(var))
  end

  # Aliases
  def to_string({:__aliases__, _, refs} = ast, fun) do
    fun.(ast, Enum.map_join(refs, ".", &call_to_string(&1, fun)))
  end

  # Blocks
  def to_string({:__block__, _, [expr]} = ast, fun) do
    fun.(ast, to_string(expr, fun))
  end

  def to_string({:__block__, _, _} = ast, fun) do
    block = adjust_new_lines block_to_string(ast, fun), "\n  "
    fun.(ast, "(\n  " <> block <> "\n)")
  end

  # Bits containers
  def to_string({:<<>>, _, args} = ast, fun) do
    if interpolated?(ast) do
      fun.(ast, interpolate(ast, fun))
    else
      fun.(ast, case Enum.map_join(args, ", ", &to_string(&1, fun)) do
        "<" <> rest -> "<< <" <> rest  <> " >>"
        rest -> "<<" <> rest <> ">>"
      end)
    end
  end

  # Tuple containers
  def to_string({:{}, _, args} = ast, fun) do
    tuple = "{" <> Enum.map_join(args, ", ", &to_string(&1, fun)) <> "}"
    fun.(ast, tuple)
  end

  # Map containers
  def to_string({:%{}, _, args} = ast, fun) do
    map = "%{" <> map_to_string(args, fun) <> "}"
    fun.(ast, map)
  end

  def to_string({:%, _, [structname, map]} = ast, fun) do
    {:%{}, _, args} = map
    struct = "%" <> to_string(structname, fun) <> "{" <> map_to_string(args, fun) <> "}"
    fun.(ast, struct)
  end

  # Fn keyword
  def to_string({:fn, _, [{:->, _, [_, tuple]}] = arrow} = ast, fun)
      when not is_tuple(tuple) or elem(tuple, 0) != :__block__ do
    fun.(ast, "fn " <> arrow_to_string(arrow, fun) <> " end")
  end

  def to_string({:fn, _, [{:->, _, _}] = block} = ast, fun) do
    fun.(ast, "fn " <> block_to_string(block, fun) <> "\nend")
  end

  def to_string({:fn, _, block} = ast, fun) do
    block = adjust_new_lines block_to_string(block, fun), "\n  "
    fun.(ast, "fn\n  " <> block <> "\nend")
  end

  # left -> right
  def to_string([{:->, _, _}|_] = ast, fun) do
    fun.(ast, "(" <> arrow_to_string(ast, fun, true) <> ")")
  end

  # left when right
  def to_string({:when, _, [left, right]} = ast, fun) do
    if right != [] and Keyword.keyword?(right) do
      right = kw_list_to_string(right, fun)
    else
      right = fun.(ast, op_to_string(right, fun, :when, :right))
    end

    fun.(ast, op_to_string(left, fun, :when, :left) <> " when " <> right)
  end

  # Binary ops
  def to_string({op, _, [left, right]} = ast, fun) when op in unquote(@binary_ops) do
    fun.(ast, op_to_string(left, fun, op, :left) <> " #{op} " <> op_to_string(right, fun, op, :right))
  end

  # Splat when
  def to_string({:when, _, args} = ast, fun) do
    {left, right} = :elixir_utils.split_last(args)
    fun.(ast, "(" <> Enum.map_join(left, ", ", &to_string(&1, fun)) <> ") when " <> to_string(right, fun))
  end

  # Unary ops
  def to_string({unary, _, [{binary, _, [_, _]} = arg]} = ast, fun)
      when unary in unquote(@unary_ops) and binary in unquote(@binary_ops) do
    fun.(ast, Atom.to_string(unary) <> "(" <> to_string(arg, fun) <> ")")
  end

  def to_string({:not, _, [arg]} = ast, fun)  do
    fun.(ast, "not " <> to_string(arg, fun))
  end

  def to_string({op, _, [arg]} = ast, fun) when op in unquote(@unary_ops) do
    fun.(ast, Atom.to_string(op) <> to_string(arg, fun))
  end

  # Access
  def to_string({{:., _, [Access, :get]}, _, [left, right]} = ast, fun) do
    fun.(ast, to_string(left, fun) <> to_string([right], fun))
  end

  # All other calls
  def to_string({target, _, args} = ast, fun) when is_list(args) do
    if sigil = sigil_call(ast, fun) do
      sigil
    else
      {list, last} = :elixir_utils.split_last(args)
      fun.(ast, case kw_blocks?(last) do
        true  -> call_to_string_with_args(target, list, fun) <> kw_blocks_to_string(last, fun)
        false -> call_to_string_with_args(target, args, fun)
      end)
    end
  end

  # Two-item tuples
  def to_string({left, right}, fun) do
    to_string({:{}, [], [left, right]}, fun)
  end

  # Lists
  def to_string(list, fun) when is_list(list) do
    fun.(list, cond do
      list == [] ->
        "[]"
      :io_lib.printable_list(list) ->
        "'" <> Inspect.BitString.escape(IO.chardata_to_string(list), ?') <> "'"
      Keyword.keyword?(list) ->
        "[" <> kw_list_to_string(list, fun) <> "]"
      true ->
        "[" <> Enum.map_join(list, ", ", &to_string(&1, fun)) <> "]"
    end)
  end

  # All other structures
  def to_string(other, fun), do: fun.(other, inspect(other, []))

  # Block keywords
  @kw_keywords [:do, :catch, :rescue, :after, :else]

  defp kw_blocks?([_|_] = kw) do
    Enum.all?(kw, &match?({x, _} when x in unquote(@kw_keywords), &1))
  end
  defp kw_blocks?(_), do: false

  # Check if we have an interpolated string.
  defp interpolated?({:<<>>, _, [_|_] = parts}) do
    Enum.all?(parts, fn
      {:::, _, [{{:., _, [Kernel, :to_string]}, _, [_]},
                {:binary, _, _}]} -> true
      binary when is_binary(binary) -> true
      _ -> false
    end)
  end

  defp interpolated?(_) do
    false
  end

  defp interpolate({:<<>>, _, parts}, fun) do
    parts = Enum.map_join(parts, "", fn
      {:::, _, [{{:., _, [Kernel, :to_string]}, _, [arg]}, {:binary, _, _}]} ->
        "\#{" <> to_string(arg, fun) <> "}"
      binary when is_binary(binary) ->
        binary = inspect(binary, [])
        :binary.part(binary, 1, byte_size(binary) - 2)
    end)

    <<?", parts::binary, ?">>
  end

  defp module_to_string(atom, _fun) when is_atom(atom), do: inspect(atom, [])
  defp module_to_string(other, fun), do: call_to_string(other, fun)

  defp sigil_call({func, _, [{:<<>>, _, _} = bin, args]} = ast, fun) when is_list(args) do
    sigil =
      case Atom.to_string(func) do
        <<"sigil_", name>> ->
          "~" <> <<name>> <>
          interpolate(bin, fun) <>
          sigil_args(args, fun)
        _ ->
          nil
      end
    fun.(ast, sigil)
  end

  defp sigil_call(_other, _fun) do
    nil
  end

  defp sigil_args([], _fun),   do: ""
  defp sigil_args(args, fun), do: fun.(args, List.to_string(args))

  defp call_to_string(atom, _fun) when is_atom(atom),
    do: Atom.to_string(atom)
  defp call_to_string({:., _, [arg]}, fun),
    do: module_to_string(arg, fun) <> "."
  defp call_to_string({:., _, [left, right]}, fun),
    do: module_to_string(left, fun) <> "." <> call_to_string(right, fun)
  defp call_to_string(other, fun),
    do: to_string(other, fun)

  defp call_to_string_with_args(target, args, fun) do
    target = call_to_string(target, fun)
    args = args_to_string(args, fun)
    target <> "(" <> args <> ")"
  end

  defp args_to_string(args, fun) do
    {list, last} = :elixir_utils.split_last(args)

    if last != [] and Keyword.keyword?(last) do
      args = Enum.map_join(list, ", ", &to_string(&1, fun))
      if list != [], do: args = args <> ", "
      args <> kw_list_to_string(last, fun)
    else
      Enum.map_join(args, ", ", &to_string(&1, fun))
    end
  end

  defp kw_blocks_to_string(kw, fun) do
    Enum.reduce(@kw_keywords, " ", fn(x, acc) ->
      case Keyword.has_key?(kw, x) do
        true  -> acc <> kw_block_to_string(x, Keyword.get(kw, x), fun)
        false -> acc
      end
    end) <> "end"
  end

  defp kw_block_to_string(key, value, fun) do
    block = adjust_new_lines block_to_string(value, fun), "\n  "
    Atom.to_string(key) <> "\n  " <> block <> "\n"
  end

  defp block_to_string([{:->, _, _}|_] = block, fun) do
    Enum.map_join(block, "\n", fn({:->, _, [left, right]}) ->
      left = comma_join_or_empty_paren(left, fun, false)
      left <> "->\n  " <> adjust_new_lines block_to_string(right, fun), "\n  "
    end)
  end

  defp block_to_string({:__block__, _, exprs}, fun) do
    Enum.map_join(exprs, "\n", &to_string(&1, fun))
  end

  defp block_to_string(other, fun), do: to_string(other, fun)

  defp map_to_string([{:|, _, [update_map, update_args]}], fun) do
    to_string(update_map, fun) <> " | " <> map_to_string(update_args, fun)
  end

  defp map_to_string(list, fun) do
    cond do
      Keyword.keyword?(list) -> kw_list_to_string(list, fun)
      true -> map_list_to_string(list, fun)
    end
  end

  defp kw_list_to_string(list, fun) do
    Enum.map_join(list, ", ", fn {key, value} ->
      atom_name = case Inspect.Atom.inspect(key) do
        ":" <> rest -> rest
        other       -> other
      end
      atom_name <> ": " <> to_string(value, fun)
    end)
  end

  defp map_list_to_string(list, fun) do
    Enum.map_join(list, ", ", fn {key, value} ->
      to_string(key, fun) <> " => " <> to_string(value, fun)
    end)
  end

  defp parenthise(expr, fun) do
    "(" <> to_string(expr, fun) <> ")"
  end

  defp op_to_string({op, _, [_, _]} = expr, fun, parent_op, side) when op in unquote(@binary_ops) do
    {parent_assoc, parent_prec} = binary_op_props(parent_op)
    {_, prec}                   = binary_op_props(op)
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

  defp arrow_to_string(pairs, fun, paren \\ false) do
    Enum.map_join(pairs, "; ", fn({:->, _, [left, right]}) ->
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
    for <<x <- block>>, into: "" do
      case x == ?\n do
        true  -> replacement
        false -> <<x>>
      end
    end
  end

  @doc """
  Receives an AST node and expands it once.

  The following contents are expanded:

    * Macros (local or remote)
    * Aliases are expanded (if possible) and return atoms
    * Pseudo-variables (`__ENV__`, `__MODULE__` and `__DIR__`)
    * Module attributes reader (`@foo`)

  If the expression cannot be expanded, it returns the expression
  itself. Notice that `expand_once/2` performs the expansion just
  once and it is not recursive. Check `expand/2` for expansion
  until the node can no longer be expanded.

  ## Examples

  In the example below, we have a macro that generates a module
  with a function named `name_length` that returns the length
  of the module name. The value of this function will be calculated
  at compilation time and not at runtime.

  Consider the implementation below:

      defmacro defmodule_with_length(name, do: block) do
        length = length(Atom.to_char_list(name))

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

      {:__aliases__, [], [:My, :Module]}

  That said, we need to expand the aliases node above to an
  atom, so we can retrieve its length. Expanding the node is
  not straight-forward because we also need to expand the
  caller aliases. For example:

      alias MyHelpers, as: My

      defmodule_with_length My.Module do
        def other_function, do: ...
      end

  The final module name will be `MyHelpers.Module` and not
  `My.Module`. With `Macro.expand/2`, such aliases are taken
  into consideration. Local and remote macros are also
  expanded. We could rewrite our macro above to use this
  function as:

      defmacro defmodule_with_length(name, do: block) do
        expanded = Macro.expand(name, __CALLER__)
        length   = length(Atom.to_char_list(expanded))

        quote do
          defmodule unquote(name) do
            def name_length, do: unquote(length)
            unquote(block)
          end
        end
      end

  """
  def expand_once(ast, env) do
    elem(do_expand_once(ast, env), 0)
  end

  defp do_expand_once({:__aliases__, _, _} = original, env) do
    case :elixir_aliases.expand(original, env.aliases, env.macro_aliases, env.lexical_tracker) do
      receiver when is_atom(receiver) ->
        :elixir_lexical.record_remote(receiver, env.function, env.lexical_tracker)
        {receiver, true}
      aliases ->
        aliases = :lists.map(&elem(do_expand_once(&1, env), 0), aliases)

        case :lists.all(&is_atom/1, aliases) do
          true ->
            receiver = :elixir_aliases.concat(aliases)
            :elixir_lexical.record_remote(receiver, env.function, env.lexical_tracker)
            {receiver, true}
          false ->
            {original, false}
        end
    end
  end

  # Expand @ calls
  defp do_expand_once({:@, _, [{name, _, args}]} = original, env) when is_atom(args) or args == [] do
    case (module = env.module) && Module.open?(module) do
      true  -> {escape(Module.get_attribute(module, name)), true}
      false -> {original, false}
    end
  end

  # Expand pseudo-variables
  defp do_expand_once({:__MODULE__, _, atom}, env) when is_atom(atom),
    do: {env.module, true}
  defp do_expand_once({:__DIR__, _, atom}, env) when is_atom(atom),
    do: {:filename.dirname(env.file), true}
  defp do_expand_once({:__ENV__, _, atom}, env) when is_atom(atom),
    do: {{:%{}, [], Map.to_list(env)}, true}
  defp do_expand_once({{:., _, [{:__ENV__, _, atom}, field]}, _, []} = original, env) when
      is_atom(atom) and is_atom(field) do
    if Map.has_key?(env, field) do
      {Map.get(env, field), true}
    else
      {original, false}
    end
  end

  # Expand possible macro import invocation
  defp do_expand_once({atom, meta, context} = original, env)
      when is_atom(atom) and is_list(meta) and is_atom(context) do
    if :lists.member({atom, Keyword.get(meta, :counter, context)}, env.vars) do
      {original, false}
    else
      case do_expand_once({atom, meta, []}, env) do
        {_, true} = exp -> exp
        {_, false}      -> {original, false}
      end
    end
  end

  defp do_expand_once({atom, meta, args} = original, env)
      when is_atom(atom) and is_list(args) and is_list(meta) do
    arity = length(args)

    if :elixir_import.special_form(atom, arity) do
      {original, false}
    else
      module = env.module
      extra  = if function_exported?(module, :__info__, 1) do
        [{module, module.__info__(:macros)}]
      else
        []
      end

      expand = :elixir_dispatch.expand_import(meta, {atom, length(args)}, args,
                                              env, extra, true)

      case expand do
        {:ok, receiver, quoted} ->
          next = :elixir_counter.next
          {:elixir_quote.linify_with_context_counter(0, {receiver, next}, quoted), true}
        {:ok, _receiver, _name, _args} ->
          {original, false}
        :error ->
          {original, false}
      end
    end
  end

  # Expand possible macro require invocation
  defp do_expand_once({{:., _, [left, right]}, meta, args} = original, env) when is_atom(right) do
    {receiver, _} = do_expand_once(left, env)

    case is_atom(receiver) do
      false -> {original, false}
      true  ->
        expand = :elixir_dispatch.expand_require(meta, receiver, {right, length(args)}, args, env)

        case expand do
          {:ok, receiver, quoted} ->
            next = :elixir_counter.next
            {:elixir_quote.linify_with_context_counter(0, {receiver, next}, quoted), true}
          :error ->
            {original, false}
        end
    end
  end

  # Anything else is just returned
  defp do_expand_once(other, _env), do: {other, false}

  @doc """
  Receives an AST node and expands it until it can no longer
  be expanded.

  This function uses `expand_once/2` under the hood. Check
  it out for more information and examples.
  """
  def expand(tree, env) do
    expand_until({tree, true}, env)
  end

  defp expand_until({tree, true}, env) do
    expand_until(do_expand_once(tree, env), env)
  end

  defp expand_until({tree, false}, _env) do
    tree
  end
end
