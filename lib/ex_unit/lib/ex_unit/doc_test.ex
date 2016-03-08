defmodule ExUnit.DocTest do
  @moduledoc """
  ExUnit.DocTest implements functionality similar to [Python's
  doctest](https://docs.python.org/2/library/doctest.html).

  In a nutshell, it allows us to generate tests from the code
  examples existing in a module/function/macro's documentation.
  In order to do that, one needs to invoke the `doctest/1` macro
  from their test case and write their examples according
  to some guidelines.

  The syntax for examples is as follows. Every new test starts
  on a new line, with an `iex>` prefix. Multiline expressions
  can be employed if the following lines start with either
  `...>` (recommended) or `iex>` prefix.

  The expected result should start at the next line after `iex>`
  or `...>` line(s) and is terminated either by a newline, new
  `iex>` prefix or end of the string literal.

  ## Examples

  Currently, the only way to run doctests is to include them into
  an ExUnit case with a `doctest` macro:

      defmodule MyModule.Test do
        use ExUnit.Case, async: true
        doctest MyModule
      end

  The `doctest` macro is going to loop through all functions and
  macros defined in `MyModule`, parsing their documentation in
  search of code examples.

  A very basic example is:

      iex> 1+1
      2

  Expressions on multiple lines are also supported:

      iex> Enum.map [1, 2, 3], fn(x) ->
      ...>   x * 2
      ...> end
      [2, 4, 6]

  Multiple results can be checked within the same test:

      iex> a = 1
      1
      iex> a + 1
      2

  If you want to keep any two tests separate,
  add an empty line between them:

      iex> a = 1
      1

      iex> a + 1  # will fail with a "undefined function a/0" error
      2

  If you don't want to assert for every result in a doctest, you can just omit
  the result:

      iex> pid = spawn fn -> :ok end
      iex> is_pid(pid)
      true

  This is useful when the result is something variable (like a pid in the
  example above) or when the result is a complicated data structure and you
  don't want to show it all, but just parts of it or some of its properties.

  Similarly to IEx you can use numbers in your "prompts":

      iex(1)> [1 + 2,
      ...(1)>  3]
      [3, 3]

  This is useful in two use cases:

    * being able to refer to specific numbered scenarios
    * copy-pasting examples from an actual IEx session

  We also allow you to select or skip some functions when calling
  `doctest`. See the documentation for more info.

  ## Opaque types

  Some types internal structure are kept hidden and instead show a
  user-friendly structure when inspecting the value. The idiom in
  Elixir is to print those data types as `#Name<...>`. Because those
  values are treated as comments in Elixir code due to the leading
  `#` sign, they require special care when used in doctests.

  Imagine you have a map with a MapSet inside which is printed as:

      %{users: #MapSet<[:foo, :bar]>}

  If you try to match on such expression, `doctest` will fail to compile.
  You have two options to solve this.

  The first one is to rely on the fact that doctest can compare internal
  structures as long as they are at the root. So one could write:

      iex> map = %{users: Enum.into([:foo, :bar], MapSet.new)}
      iex> map.users
      #MapSet<[:foo, :bar]>

  Whenever a doctest starts with "#Name<", `doctest` will perform a string
  comparison. For example, the above test will perform the following match:

      inspect(map.users) == "#MapSet<[:foo, :bar]>"

  Alternatively, since doctest results are actually evaluated, you can have
  the MapSet building expression as the doctest result:

      iex> %{users: Enum.into([:foo, :bar], MapSet.new)}
      %{users: Enum.into([:foo, :bar], MapSet.new)}

  The downside of this approach is that the doctest result is not really
  what users would see in the terminal.

  ## Exceptions

  You can also showcase expressions raising an exception, for example:

      iex(1)> String.to_atom((fn() -> 1 end).())
      ** (ArgumentError) argument error

  What DocTest will be looking for is a line starting with `** (` and it
  will parse it accordingly to extract the exception name and message.
  At this moment, the exception parser would make the parser treat the next
  line as a start of a completely new expression (if it is prefixed with `iex>`)
  or a no-op line with documentation. Thus, multiline messages are not
  supported.

  ## When not to use doctest

  In general, doctests are not recommended when your code examples contain
  side effects. For example, if a doctest prints to standard output, doctest
  will not try to capture the output.

  Similarly, doctests do not run in any kind of sandbox. So any module
  defined in a code example is going to linger throughout the whole test
  suite run.
  """

  defmodule Error do
    defexception [:message]

    def exception(opts) do
      module  = Keyword.fetch!(opts, :module)
      message = Keyword.fetch!(opts, :message)

      file = module.__info__(:compile)[:source] |> Path.relative_to_cwd
      info = Exception.format_file_line(file, opts[:line])
      %__MODULE__{message: info <> " " <> message}
    end
  end

  @doc """
  This macro is used to generate ExUnit test cases for doctests.

  Calling `doctest(Module)` will generate tests for all doctests found
  in the module `Module`

  Options can also be given:

    * `:except` - generate tests for all functions except those listed
      (list of `{function, arity}` tuples, and/or `:moduledoc`).

    * `:only`   - generate tests only for functions listed
      (list of `{function, arity}` tuples, and/or `:moduledoc`).

    * `:import` - when `true`, one can test a function defined in the module
      without referring to the module name. However, this is not feasible when
      there is a clash with a module like Kernel. In these cases, `:import`
      should be set to `false` and a full `M.f` construct should be used.

  ## Examples

      doctest MyModule, except: [:moduledoc, trick_fun: 1]

  This macro is auto-imported with every `ExUnit.Case`.
  """
  defmacro doctest(mod, opts \\ []) do
    require =
      if is_atom Macro.expand(mod, __CALLER__) do
        quote do
          require unquote(mod)
        end
      end

    tests = quote bind_quoted: binding do
      file = "(for doctest at) " <> Path.relative_to_cwd(mod.__info__(:compile)[:source])
      for {name, test} <- ExUnit.DocTest.__doctests__(mod, opts) do
        @tag :doctest
        @file file
        test name, do: unquote(test)
      end
    end

    [require, tests]
  end

  @doc false
  def __doctests__(module, opts) do
    do_import = Keyword.get(opts, :import, false)

    extract(module)
    |> filter_by_opts(opts)
    |> Stream.with_index
    |> Enum.map(fn {test, acc} ->
      compile_test(test, module, do_import, acc + 1)
    end)
  end

  defp filter_by_opts(tests, opts) do
    only   = opts[:only] || []
    except = opts[:except] || []
    tests
    |> Stream.reject(&(&1.fun_arity in except))
    |> Stream.filter(&(Enum.empty?(only) or &1.fun_arity in only))
  end

  ## Compilation of extracted tests

  defp compile_test(test, module, do_import, n) do
    {test_name(test, module, n), test_content(test, module, do_import)}
  end

  defp test_name(%{fun_arity: :moduledoc}, m, n) do
    "moduledoc at #{inspect m} (#{n})"
  end

  defp test_name(%{fun_arity: {f, a}}, m, n) do
    "doc at #{inspect m}.#{f}/#{a} (#{n})"
  end

  defp test_content(%{exprs: exprs, line: line, fun_arity: fun_arity}, module, do_import) do
    file     = module.__info__(:compile)[:source] |> Path.relative_to_cwd
    location = [line: line, file: file]
    stack    = Macro.escape [{module, :__MODULE__, 0, location}]

    if multiple_exceptions?(exprs) do
      {fun, arity} = fun_arity
      raise Error, line: line, module: module,
                   message: "multiple exceptions in one doctest case are not supported"
    end

    tests = Enum.map exprs, fn {expr, expected} ->
      test_case_content(expr, expected, location, stack)
    end

    quote do
      unquote_splicing(test_import(module, do_import))
      unquote(gen_code_for_tests(tests, whole_expr(exprs), stack))
    end
  end

  defp whole_expr(exprs) do
    Enum.map_join(exprs, "\n", &elem(&1, 0))
  end

  defp multiple_exceptions?(exprs) do
    Enum.count(exprs, fn
      {_, {:error, _, _}} -> true
      _ -> false
    end) > 1
  end

  defp gen_code_for_tests(tests, whole_expr, stack) do
    quote do
      stack = unquote(stack)
      try do
        # Put all tests into one context
        unquote_splicing(tests)
      rescue
        e in [ExUnit.AssertionError] ->
          reraise e, stack

        error ->
          original_stack = System.stacktrace
          reraise ExUnit.AssertionError,
            [message: "Doctest failed: got #{inspect(error.__struct__)} with message #{Exception.message(error)}",
             expr: unquote(whole_expr)],
            original_stack
      end
    end
  end

  defp test_case_content(expr, {:test, expected}, location, stack) do
    expr_ast     = string_to_quoted(location, stack, expr)
    expected_ast = string_to_quoted(location, stack, expected)

    quote do
      expected = unquote(expected_ast)
      case unquote(expr_ast) do
        ^expected -> :ok
        actual ->
          reraise ExUnit.AssertionError,
            [message: "Doctest failed",
             expr: "#{unquote(String.strip(expr))} === #{unquote(String.strip(expected))}",
             left: actual],
            unquote(stack)
      end
    end
  end

  defp test_case_content(expr, {:inspect, expected}, location, stack) do
    expr_ast     = quote do: inspect(unquote(string_to_quoted(location, stack, expr)))
    expected_ast = string_to_quoted(location, stack, expected)

    quote do
      expected = unquote(expected_ast)
      case unquote(expr_ast) do
        ^expected -> :ok
        actual ->
          reraise ExUnit.AssertionError,
            [message: "Doctest failed",
             expr: "inspect(#{unquote(String.strip(expr))}) === #{unquote(String.strip(expected))}",
             left: actual],
            unquote(stack)
      end
    end
  end

  defp test_case_content(expr, {:error, exception, message}, location, stack) do
    expr_ast = string_to_quoted(location, stack, expr)

    quote do
      stack = unquote(stack)
      expr  = unquote(String.strip(expr))
      spec  = inspect(unquote(exception)) <> " with message " <> inspect(unquote(message))

      try do
        unquote(expr_ast)
      rescue
        error ->
          unless error.__struct__ == unquote(exception) and
                 Exception.message(error) == unquote(message) do
            got = inspect(error.__struct__) <> " with message " <> inspect(Exception.message(error))
            reraise ExUnit.AssertionError,
              [message: "Doctest failed: expected exception #{spec} but got #{got}",
               expr: expr],
              stack
          end
      else
        _ ->
          reraise ExUnit.AssertionError,
            [message: "Doctest failed: expected exception #{spec} but nothing was raised",
             expr: expr],
            stack
      end
    end
  end

  defp test_import(_mod, false), do: []
  defp test_import(mod, _) do
    [quote do: import(unquote(mod))]
  end

  defp string_to_quoted(location, stack, expr) do
    try do
      Code.string_to_quoted!(expr, location)
    rescue
      e ->
        message = "(#{inspect e.__struct__}) #{Exception.message(e)}"
        quote do
          reraise ExUnit.AssertionError,
            [message: "Doctest did not compile, got: #{unquote(message)}",
             expr: unquote(String.strip(expr))],
            unquote(stack)
        end
    end
  end

  ## Extraction of the tests

  defp extract(module) do
    all_docs = Code.get_docs(module, :all)

    unless all_docs do
      raise Error, module: module, message:
        "could not retrieve the documentation for module #{inspect module}. " <>
        "The module was not compiled with documentation or its beam file cannot be accessed"
    end

    moduledocs = extract_from_moduledoc(all_docs[:moduledoc], module)

    docs = for doc <- all_docs[:docs],
               doc <- extract_from_doc(doc, module),
               do: doc

    moduledocs ++ docs
  end

  defp extract_from_moduledoc({_, doc}, _module) when doc in [false, nil], do: []

  defp extract_from_moduledoc({line, doc}, module) do
    for test <- extract_tests(line, doc, module) do
      %{test | fun_arity: :moduledoc}
    end
  end

  defp extract_from_doc({_, _, _, _, doc}, _module) when doc in [false, nil], do: []

  defp extract_from_doc({fa, line, _, _, doc}, module) do
    for test <- extract_tests(line, doc, module) do
      %{test | fun_arity: fa}
    end
  end

  defp extract_tests(line_no, doc, module) do
    all_lines = String.split(doc, ~r/\n/, trim: false)
    lines = adjust_indent(all_lines, line_no + 1, module)
    extract_tests(lines, "", "", [], true)
  end

  defp adjust_indent(lines, line_no, module) do
    adjust_indent(:text, lines, line_no, [], 0, module)
  end

  defp adjust_indent(_kind, [], _line_no, adjusted_lines, _indent, _module) do
    Enum.reverse adjusted_lines
  end

  @iex_prompt ["iex>", "iex("]
  @dot_prompt ["...>", "...("]

  defp adjust_indent(:text, [line|rest], line_no, adjusted_lines, indent, module) do
    case String.starts_with?(String.lstrip(line), @iex_prompt) do
      true  ->
        adjust_indent(:prompt, [line|rest], line_no, adjusted_lines, get_indent(line, indent), module)
      false ->
        adjust_indent(:text, rest, line_no + 1, adjusted_lines, indent, module)
    end
  end

  defp adjust_indent(kind, [line|rest], line_no, adjusted_lines, indent, module) when kind in [:prompt, :after_prompt] do
    stripped_line = strip_indent(line, indent)

    case String.lstrip(line) do
      "" ->
        raise Error, line: line_no, module: module,
                     message: "expected non-blank line to follow iex> prompt"
      ^stripped_line ->
        :ok
      _ ->
        n_spaces = if indent == 1,
          do: "#{indent} space",
          else: "#{indent} spaces"

        raise Error, line: line_no, module: module,
                     message: "indentation level mismatch: #{inspect line}, should have been #{n_spaces}"
    end

    adjusted_lines = [{stripped_line, line_no}|adjusted_lines]

    if String.starts_with?(stripped_line, @iex_prompt ++ @dot_prompt) do
      adjust_indent(:after_prompt, rest, line_no + 1, adjusted_lines, indent, module)
    else
      next = if kind == :prompt, do: :after_prompt, else: :code
      adjust_indent(next, rest, line_no + 1, adjusted_lines, indent, module)
    end
  end

  defp adjust_indent(:code, [line|rest], line_no, adjusted_lines, indent, module) do
    stripped_line = strip_indent(line, indent)
    cond do
      stripped_line == "" ->
        adjust_indent(:text, rest, line_no + 1, [{stripped_line, line_no}|adjusted_lines], 0, module)
      String.starts_with?(String.lstrip(line), @iex_prompt) ->
        adjust_indent(:prompt, [line|rest], line_no, adjusted_lines, indent, module)
      true ->
        adjust_indent(:code, rest, line_no + 1, [{stripped_line, line_no}|adjusted_lines], indent, module)
    end
  end

  defp get_indent(line, current_indent) do
    case Regex.run ~r/iex/, line, return: :index do
      [{pos, _len}] -> pos
      nil -> current_indent
    end
  end

  defp strip_indent(line, indent) do
    length = byte_size(line) - indent
    if length > 0 do
      :binary.part(line, indent, length)
    else
      ""
    end
  end

  defp extract_tests([], "", "", [], _) do
    []
  end

  defp extract_tests([], "", "", acc, _) do
    Enum.reverse(reverse_last_test(acc))
  end

  # End of input and we've still got a test pending.
  defp extract_tests([], expr_acc, expected_acc, [test=%{exprs: exprs}|t], _) do
    test = %{test | exprs: [{expr_acc, {:test, expected_acc}} | exprs]}
    Enum.reverse(reverse_last_test([test|t]))
  end

  # We've encountered the next test on an adjacent line. Put them into one group.
  defp extract_tests([{"iex>" <> _, _} | _] = list, expr_acc, expected_acc, [test=%{exprs: exprs}|t], newtest)
  when expr_acc != "" and expected_acc != "" do
    test = %{test | exprs: [{expr_acc, {:test, expected_acc}} | exprs]}
    extract_tests(list, "", "", [test|t], newtest)
  end

  # Store expr_acc and start a new test case.
  defp extract_tests([{"iex>" <> string, line} | lines], "", expected_acc, acc, true) do
    acc = reverse_last_test(acc)
    test = %{line: line, fun_arity: nil, exprs: []}
    extract_tests(lines, string, expected_acc, [test|acc], false)
  end

  # Store expr_acc.
  defp extract_tests([{"iex>" <> string, _} | lines], "", expected_acc, acc, false) do
    extract_tests(lines, string, expected_acc, acc, false)
  end

  # Still gathering expr_acc. Synonym for the next clause.
  defp extract_tests([{"iex>" <> string, _} | lines], expr_acc, expected_acc, acc, newtest) do
    extract_tests(lines, expr_acc <> "\n" <> string, expected_acc, acc, newtest)
  end

  # Still gathering expr_acc. Synonym for the previous clause.
  defp extract_tests([{"...>" <> string, _} | lines], expr_acc, expected_acc, acc, newtest) when expr_acc != "" do
    extract_tests(lines, expr_acc <> "\n" <> string, expected_acc, acc, newtest)
  end

  # Expression numbers are simply skipped.
  defp extract_tests([{<<"iex(", _>> <> string, line} | lines], expr_acc, expected_acc, acc, newtest) do
    extract_tests([{"iex" <> skip_iex_number(string), line} | lines], expr_acc, expected_acc, acc, newtest)
  end

  # Expression numbers are simply skipped redux.
  defp extract_tests([{<<"...(", _>> <> string, line} | lines], expr_acc, expected_acc, acc, newtest) do
    extract_tests([{"..." <> skip_iex_number(string), line} | lines], expr_acc, expected_acc, acc, newtest)
  end

  # Skip empty or documentation line.
  defp extract_tests([_|lines], "", "", acc, _) do
    extract_tests(lines, "", "", acc, true)
  end

  # Encountered an empty line, store pending test
  defp extract_tests([{"",_}|lines], expr_acc, expected_acc, [test=%{exprs: exprs}|t], _) do
    test = %{test | exprs: [{expr_acc, {:test, expected_acc}} | exprs]}
    extract_tests(lines, "", "", [test|t], true)
  end

  # Exception test.
  defp extract_tests([{"** (" <> string, _} | lines], expr_acc, "", [test=%{exprs: exprs}|t], newtest) do
    test = %{test | exprs: [{expr_acc, extract_error(string, "")} | exprs]}
    extract_tests(lines, "", "", [test|t], newtest)
  end

  # Finally, parse expected_acc.
  defp extract_tests([{expected, _}|lines], expr_acc, expected_acc, [test=%{exprs: exprs}|t]=acc, newtest) do
    if expected =~ ~r/^#[A-Z][\w\.]*<.*>$/ do
      expected = expected_acc <> "\n" <> inspect(expected)
      test = %{test | exprs: [{expr_acc, {:inspect, expected}} | exprs]}
      extract_tests(lines, "", "", [test|t], newtest)
    else
      extract_tests(lines, expr_acc, expected_acc <> "\n" <> expected, acc, newtest)
    end
  end

  defp extract_error(")" <> rest, acc) do
    {:error, Module.concat([acc]), String.strip(rest)}
  end

  defp extract_error(<<char>> <> rest, acc) do
    extract_error(rest, <<acc::binary, char>>)
  end

  defp skip_iex_number(")>" <> string) do
    ">" <> string
  end

  defp skip_iex_number(<<_>> <> string) do
    skip_iex_number(string)
  end

  defp reverse_last_test([]), do: []
  defp reverse_last_test([test=%{exprs: exprs} | t]) do
    test = %{test | exprs: Enum.reverse(exprs)}
    [test | t]
  end
end
