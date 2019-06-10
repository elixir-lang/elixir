Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.IntegrationTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  test "empty documents" do
    assert_format "   ", ""
    assert_format "\n", ""
    assert_format ";", ""
  end

  test "function with multiple calls and case" do
    assert_same """
    def equivalent(string1, string2) when is_binary(string1) and is_binary(string2) do
      quoted1 = Code.string_to_quoted!(string1)
      quoted2 = Code.string_to_quoted!(string2)

      case not_equivalent(quoted1, quoted2) do
        {left, right} -> {:error, left, right}
        nil -> :ok
      end
    end
    """
  end

  test "function with long pipeline" do
    assert_same ~S"""
    def to_algebra!(string, opts \\ []) when is_binary(string) and is_list(opts) do
      string
      |> Code.string_to_quoted!(wrap_literals_in_blocks: true, unescape: false)
      |> block_to_algebra(state(opts))
      |> elem(0)
    end
    """
  end

  test "case with multiple multi-line arrows" do
    assert_same ~S"""
    case meta[:format] do
      :list_heredoc ->
        string = list |> List.to_string() |> escape_string(:heredoc)
        {@single_heredoc |> line(string) |> concat(@single_heredoc) |> force_unfit(), state}

      :charlist ->
        string = list |> List.to_string() |> escape_string(@single_quote)
        {@single_quote |> concat(string) |> concat(@single_quote), state}

      _other ->
        list_to_algebra(list, state)
    end
    """
  end

  test "function with long guards" do
    assert_same """
    defp module_attribute_read?({:@, _, [{var, _, var_context}]})
         when is_atom(var) and is_atom(var_context) do
      Code.Identifier.classify(var) == :callable_local
    end
    """
  end

  test "anonymous function with single clause and blocks" do
    assert_same """
    {args_doc, state} =
      Enum.reduce(args, {[], state}, fn quoted, {acc, state} ->
        {doc, state} = quoted_to_algebra(quoted, :block, state)
        doc = doc |> concat(nest(break(""), :reset)) |> group()
        {[doc | acc], state}
      end)
    """
  end

  test "anonymous function with long single clause and blocks" do
    assert_same """
    {function_count, call_count, total_time} =
      Enum.reduce(call_results, {0, 0, 0}, fn {_, {count, time}},
                                              {function_count, call_count, total_time} ->
        {function_count + 1, call_count + count, total_time + time}
      end)
    """
  end

  test "cond with long clause args" do
    assert_same """
    cond do
      parent_prec == prec and parent_assoc == side ->
        binary_op_to_algebra(op, op_string, left, right, context, state, op_info, nesting)

      parent_op in @required_parens_on_binary_operands or parent_prec > prec or
          (parent_prec == prec and parent_assoc != side) ->
        {operand, state} =
          binary_op_to_algebra(op, op_string, left, right, context, state, op_info, 2)

        {concat(concat("(", nest(operand, 1)), ")"), state}

      true ->
        binary_op_to_algebra(op, op_string, left, right, context, state, op_info, 2)
    end
    """
  end

  test "type with multiple |" do
    assert_same """
    @type t ::
            binary
            | :doc_nil
            | :doc_line
            | doc_string
            | doc_cons
            | doc_nest
            | doc_break
            | doc_group
            | doc_color
            | doc_force
            | doc_cancel
    """
  end

  test "spec with when keywords and |" do
    assert_same """
    @spec send(dest, msg, [option]) :: :ok | :noconnect | :nosuspend
          when dest: pid | port | atom | {atom, node},
               msg: any,
               option: :noconnect | :nosuspend
    """

    assert_same """
    @spec send(dest, msg, [option]) :: :ok | :noconnect | :nosuspend
          when dest:
                 pid
                 | port
                 | atom
                 | {atom, node}
                 | and_a_really_long_type_to_force_a_line_break
                 | followed_by_another_really_long_type
    """

    assert_same """
    @callback get_and_update(data, key, (value -> {get_value, value} | :pop)) :: {get_value, data}
              when get_value: var, data: container
    """
  end

  test "spec with multiple keys on type" do
    assert_same """
    @spec foo(%{(String.t() | atom) => any}) :: any
    """
  end

  test "multiple whens with new lines" do
    assert_same """
    def sleep(timeout)
        when is_integer(timeout) and timeout >= 0
        when timeout == :infinity do
      receive after: (timeout -> :ok)
    end
    """
  end

  test "function with operator and pipeline" do
    assert_same """
    defp apply_next_break_fits?({fun, meta, args}) when is_atom(fun) and is_list(args) do
      meta[:terminator] in [@double_heredoc, @single_heredoc] and
        fun |> Atom.to_string() |> String.starts_with?("sigil_")
    end
    """
  end

  test "mixed parens and no parens calls with anonymous function" do
    assert_same ~S"""
    node interface do
      resolve_type(fn
        %{__struct__: str}, _ ->
          str |> Model.Node.model_to_node_type()

        value, _ ->
          Logger.warn("Could not extract node type from value: #{inspect(value)}")
          nil
      end)
    end
    """
  end

  test "long defstruct definition" do
    assert_same """
    defstruct name: nil,
              module: nil,
              schema: nil,
              alias: nil,
              base_module: nil,
              web_module: nil,
              basename: nil,
              file: nil,
              test_file: nil
    """
  end

  test "mix of operators and arguments" do
    assert_same """
    def count(%{path: path, line_or_bytes: bytes}) do
      case File.stat(path) do
        {:ok, %{size: 0}} -> {:error, __MODULE__}
        {:ok, %{size: size}} -> {:ok, div(size, bytes) + if(rem(size, bytes) == 0, do: 0, else: 1)}
        {:error, reason} -> raise File.Error, reason: reason, action: "stream", path: path
      end
    end
    """
  end

  test "mix of left and right operands" do
    assert_same """
    defp server_get_modules(handlers) do
      for(handler(module: module) <- handlers, do: module)
      |> :ordsets.from_list()
      |> :ordsets.to_list()
    end
    """

    assert_same """
    neighbours = for({_, _} = t <- neighbours, do: t) |> :sets.from_list()
    """
  end

  test "long expression with single line anonymous function" do
    assert_same """
    for_many(uniq_list_of(integer(1..10000)), fn list ->
      assert Enum.uniq(list) == list
    end)
    """
  end

  test "long comprehension" do
    assert_same """
    for %{app: app, opts: opts, top_level: true} <- Mix.Dep.cached(),
        Keyword.get(opts, :app, true),
        Keyword.get(opts, :runtime, true),
        not Keyword.get(opts, :optional, false),
        app not in included_applications,
        app not in included_applications,
        do: app
    """
  end

  test "short comprehensions" do
    assert_same """
    for {protocol, :protocol, _beam} <- removed_metadata,
        remove_consolidated(protocol, output),
        do: {protocol, true},
        into: %{}
    """
  end

  test "comprehensions with when" do
    assert_same """
    for {key, value} when is_atom(key) <- Map.to_list(map),
        key = Atom.to_string(key),
        String.starts_with?(key, hint) do
      %{kind: :map_key, name: key, value_is_map: is_map(value)}
    end
    """

    assert_same """
    with {_, doc} when unquote(doc_attr?) <-
           Module.get_attribute(__MODULE__, unquote(name), unquote(escaped)),
         do: doc
    """
  end

  test "next break fits followed by inline tuple" do
    assert_same """
    assert ExUnit.Filters.eval([line: "1"], [:line], %{line: 3, describe_line: 2}, tests) ==
             {:error, "due to line filter"}
    """
  end

  test "try/catch with clause comment" do
    assert_same """
    def format_error(reason) do
      try do
        do_format_error(reason)
      catch
        # A user could create an error that looks like a built-in one
        # causing an error.
        :error, _ ->
          inspect(reason)
      end
    end
    """
  end

  test "case with when and clause comment" do
    assert_same """
    case decomposition do
      # Decomposition
      <<h, _::binary>> when h != ?< ->
        decomposition =
          decomposition
          |> :binary.split(" ", [:global])
          |> Enum.map(&String.to_integer(&1, 16))

        Map.put(dacc, String.to_integer(codepoint, 16), decomposition)

      _ ->
        dacc
    end
    """
  end

  test "anonymous function with parens around integer argument" do
    bad = """
    fn (1) -> "hello" end
    """

    assert_format bad, """
    fn 1 -> "hello" end
    """
  end

  test "no parens keywords at the end of the line" do
    bad = """
    defmodule Mod do
      def token_list_downcase(<<char, rest::binary>>, acc) when is_whitespace(char) or is_comma(char), do: token_list_downcase(rest, acc)
      def token_list_downcase(some_really_long_arg11, some_really_long_arg22, some_really_long_arg33), do: token_list_downcase(rest, acc)
    end
    """

    assert_format bad, """
    defmodule Mod do
      def token_list_downcase(<<char, rest::binary>>, acc) when is_whitespace(char) or is_comma(char),
        do: token_list_downcase(rest, acc)

      def token_list_downcase(some_really_long_arg11, some_really_long_arg22, some_really_long_arg33),
        do: token_list_downcase(rest, acc)
    end
    """
  end

  test "do at the end of the line" do
    bad = """
    foo bar, baz, quux do
      :ok
    end
    """

    good = """
    foo bar,
        baz,
        quux do
      :ok
    end
    """

    assert_format bad, good, line_length: 18
  end

  test "keyword lists in last line" do
    assert_same """
    content =
      config(VeryLongModuleNameThatWillCauseBreak, "new.html",
        conn: conn,
        changeset: changeset,
        categories: categories
      )
    """

    assert_same """
    content =
      config VeryLongModuleNameThatWillCauseBreak, "new.html",
        conn: conn,
        changeset: changeset,
        categories: categories
    """
  end

  test "do at the end of the line with single argument" do
    bad = """
    defmodule Location do
      def new(line, column) when is_integer(line) and line >= 0 and is_integer(column) and column >= 0 do
        %{column: column, line: line}
      end
    end
    """

    assert_format bad, """
    defmodule Location do
      def new(line, column)
          when is_integer(line) and line >= 0 and is_integer(column) and column >= 0 do
        %{column: column, line: line}
      end
    end
    """
  end

  test "tuples as trees" do
    bad = """
    @document Parser.parse(
      {"html", [], [
         {"head", [], []},
         {"body", [], [
              {"div", [], [
                  {"p", [], ["1"]},
                  {"p", [], ["2"]},
                  {"div", [], [
                      {"p", [], ["3"]},
                      {"p", [], ["4"]}]},
                  {"p", [], ["5"]}]}]}]})
    """

    assert_format bad, """
    @document Parser.parse(
                {"html", [],
                 [
                   {"head", [], []},
                   {"body", [],
                    [
                      {"div", [],
                       [
                         {"p", [], ["1"]},
                         {"p", [], ["2"]},
                         {"div", [], [{"p", [], ["3"]}, {"p", [], ["4"]}]},
                         {"p", [], ["5"]}
                       ]}
                    ]}
                 ]}
              )
    """
  end

  test "first argument in a call without parens with comments" do
    assert_same """
    with bar ::
           :ok
           | :invalid
           # | :unknown
           | :other
    """

    assert_same """
    @spec bar ::
            :ok
            | :invalid
            # | :unknown
            | :other
    """
  end

  test "when with keywords inside call" do
    assert_same """
    quote((bar(foo(1)) when bat: foo(1)), [])
    """

    assert_same """
    quote(do: (bar(foo(1)) when bat: foo(1)), line: 1)
    """

    assert_same """
    typespec(quote(do: (bar(foo(1)) when bat: foo(1))), [foo: 1], [])
    """
  end

  test "false positive sigil" do
    assert_same """
    def sigil_d(<<year::2-bytes, "-", month::2-bytes, "-", day::2-bytes>>, calendar) do
      ymd(year, month, day, calendar)
    end
    """
  end

  test "capture with operators" do
    assert_same """
    "this works" |> (&String.upcase/1) |> (&String.downcase/1)
    """

    assert_same """
    "this works" || (&String.upcase/1) || (&String.downcase/1)
    """

    assert_same """
    "this works" == (&String.upcase/1) == (&String.downcase/1)
    """

    bad = """
    "this works" = (&String.upcase/1) = (&String.downcase/1)
    """

    assert_format bad, """
    "this works" = (&String.upcase/1) = &String.downcase/1
    """

    bad = """
    "this works" ++ (&String.upcase/1) ++ (&String.downcase/1)
    """

    assert_format bad, """
    "this works" ++ (&String.upcase/1) ++ &String.downcase/1
    """

    bad = """
    "this works" | (&String.upcase/1) | (&String.downcase/1)
    """

    assert_format bad, """
    "this works" | (&String.upcase/1) | &String.downcase/1
    """

    bad = ~S"""
    "this works" \\ (&String.upcase/1) \\ (&String.downcase/1)
    """

    assert_format bad, ~S"""
    "this works" \\ &String.upcase/1 \\ &String.downcase/1
    """
  end
end
