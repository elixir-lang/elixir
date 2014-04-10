defmodule ExUnit.DocTest do
  @moduledoc """
  ExUnit.DocTest implements functionality similar to [Python's
  doctest](http://docs.python.org/2/library/doctest.html).

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
      [2,4,6]

  Multiple results can be checked within the same test:

      iex> a = 1
      1
      iex> a + 1
      2

  If you want to keep any two tests separate,
  add an empty line between them:

      iex> a = 1
      1

      iex> a + 1  # will fail with a "function a/0 undefined" error
      2

  Similarly to iex you can use numbers in your "prompts":

      iex(1)> [1+2,
      ...(1)>  3]
      [3,3]

  This is useful in two use cases:

  * Being able to refer to specific numbered scenarios
  * Copy-pasting examples from an actual iex sessions

  We also allow you to select or skip some functions when calling
  `doctest`. See the documentation for more info.

  ## Opaque types

  Some types internal structure are kept hidden and instead show a
  user-friendly structure when inspecting the value. The idiom in
  Elixir is to print those data types as `#Name<...>`. Doctest will
  test these values by doing a string compare.

      iex> HashDict.new(a: 10, b: 20)
      #HashDict<[a: 10, b: 20]>

  The above example will be tested with the following match:
  `"#HashDict<[a: 10, b: 20]>" = inspect (HashDict.new(a: 10, b: 20))`.

  ## Exceptions

  You can also showcase expressions raising an exception, for example:

      iex(1)> binary_to_atom((fn() -> 1 end).())
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

  Similarly, doctest does not run in any kind of sandbox. So any module
  defined in a code example is going to linger throughout the whole test
  suite run.
  """

  defexception Error, message: nil

  defrecord Test, fun_arity: nil, line: nil, exprs: []

  @doc """
  This macro is used to generate ExUnit test cases for doctests.

  Calling `doctest(Module)` will generate tests for all doctests found
  in the module `Module`

  Options can also be supplied:

  * `:except` — generate tests for all functions except those listed
                (list of `{function, arity}` tuples)

  * `:only`   — generate tests only for functions listed
                (list of `{function, arity}` tuples)

  * `:import` — when true, one can test a function defined in the module
                without referring to the module name. However, this is not
                feasible when there is a clash with a module like
                Kernel. In these cases, `import` should be set to `false`
                and a full `M.f` construct should be used.

  ## Examples

      doctest MyModule, except: [trick_fun: 1]

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
      for { name, test } <- ExUnit.DocTest.__doctests__(mod, opts) do
        @file '(for doctest at) ' ++ Path.relative_to_cwd(mod.__info__(:compile)[:source])
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
    |> Enum.map(fn { test, acc } ->
      compile_test(test, module, do_import, acc + 1)
    end)
  end

  defp filter_by_opts(tests, opts) do
    only   = opts[:only] || []
    except = opts[:except] || []

    Stream.filter(tests, fn(test) ->
      fa = test.fun_arity
      Enum.all?(except, &(&1 != fa)) and Enum.all?(only, &(&1 == fa))
    end)
  end

  ## Compilation of extracted tests

  defp compile_test(test, module, do_import, n) do
    { test_name(test, module, n), test_content(test, module, do_import) }
  end

  defp test_name(Test[fun_arity: nil], m, n) do
    "moduledoc at #{inspect m} (#{n})"
  end

  defp test_name(Test[fun_arity: { f, a }], m, n) do
    "doc at #{inspect m}.#{f}/#{a} (#{n})"
  end

  defp test_content(Test[exprs: exprs, line: line, fun_arity: fun_arity], module, do_import) do
    file     = module.__info__(:compile)[:source] |> String.from_char_list!
    location = [line: line, file: Path.relative_to_cwd(file)]
    stack    = Macro.escape [{ module, :__MODULE__, 0, location }]

    if multiple_exceptions?(exprs) do
      { fun, arity } = fun_arity
      raise Error, message: "multiple exceptions in one doctest case are not supported. "
                            "Invalid doctest for #{inspect module}.#{fun}/#{arity}"
    end

    tests = Enum.map exprs, fn { expr, expected } ->
      test_case_content(expr, expected, module, line, file, stack)
    end

    quote do
      unquote_splicing(test_import(module, do_import))
      unquote(gen_code_for_tests(tests, whole_expr(exprs), exception_expr(exprs), stack))
    end
  end

  defp whole_expr(exprs) do
    Enum.map_join(exprs, "\n", &elem(&1, 0))
  end

  defp exception_expr(exprs) do
    Enum.find_value(exprs, "nothing", fn
      { _, {:error, exception, message} } ->
        inspect(exception) <> " with message " <> message
      _ ->
        nil
    end)
  end

  defp multiple_exceptions?(exprs) do
    Enum.count(exprs, fn
      { _, {:error, _, _} } -> true
      _ -> false
    end) > 1
  end

  defp gen_code_for_tests(tests, whole_expr, exception, stack) do
    quote do
      stack = unquote(stack)
      try do
        # Put all tests into one context
        unquote_splicing(tests)
      rescue
        e in [ExUnit.ExpectationError] ->
          raise e, [], stack

        error ->
          raise ExUnit.ExpectationError,
            [ prelude: "Expected doctest",
              expr: unquote(whole_expr),
              expected: unquote(exception),
              # We're using a combined message here because all expressions
              # (those that are expected to raise and those that aren't) are in
              # the same try block above.
              assertion: "complete or raise",
              actual: inspect(elem(error, 0)) <> " with message " <> inspect(error.message) ],
            stack
      end
    end
  end

  defp test_case_content(expr, { :test, expected }, module, line, file, stack) do
    expr_ast     = string_to_quoted(module, line, file, expr)
    expected_ast = string_to_quoted(module, line, file, expected)

    quote do
      v = unquote(expected_ast)
      case unquote(expr_ast) do
        ^v -> :ok
        actual ->
          raise ExUnit.ExpectationError,
            [ prelude: "Expected doctest",
              expr: unquote(expr),
              expected: inspect(v),
              assertion: "evaluate to",
              actual: inspect(actual) ],
            unquote(stack)
      end
    end
  end

  defp test_case_content(expr, { :inspect, expected }, module, line, file, stack) do
    expr_ast     = string_to_quoted(module, line, file, expr)
    expr_ast     = quote do: inspect(unquote(expr_ast))
    expected_ast = string_to_quoted(module, line, file, expected)

    quote do
      v = unquote(expected_ast)
      case unquote(expr_ast) do
        ^v -> :ok
        actual ->
          raise ExUnit.ExpectationError,
            [ prelude: "Expected doctest",
              expr: unquote(expr),
              expected: inspect(v),
              assertion: "inspect as",
              actual: inspect(actual) ],
            unquote(stack)
      end
    end
  end

  defp test_case_content(expr, { :error, exception, message }, module, line, file, stack) do
    expr_ast = string_to_quoted(module, line, file, expr)

    quote do
      stack = unquote(stack)
      expr = unquote(expr)
      exception = inspect(unquote(exception)) <> " with message " <> inspect(unquote(message))
      try do
        v = unquote(expr_ast)
        raise ExUnit.ExpectationError,
          [ prelude: "Expected doctest",
            expr: expr,
            expected: exception,
            assertion: "raise",
            actual: inspect(v) ],
          stack
      rescue
        error in [unquote(exception)] ->
          unless error.message == unquote(message) do
            raise ExUnit.ExpectationError,
              [ prelude: "Expected doctest",
                expr: expr,
                expected: exception,
                assertion: "raise",
                actual: inspect(elem(error, 0)) <> " with message " <> inspect(error.message) ],
              stack
          end

        other -> raise other
      end
    end
  end

  defp test_import(_mod, false), do: []
  defp test_import(mod, _) do
    [quote do: import(unquote(mod))]
  end

  defp string_to_quoted(module, line, file, expr) do
    location = [line: line, file: Path.relative_to_cwd(file)]
    stack    = Macro.escape [{ module, :__MODULE__, 0, location }]
    try do
      Code.string_to_quoted!(expr, line: line, file: file)
    rescue e ->
      quote do
        raise ExUnit.ExpectationError,
          [ prelude: "Expected doctest",
            expr: unquote(String.strip(expr)),
            expected: "successfully",
            assertion: "compile",
            actual: unquote("** #{inspect e.__record__(:name)} #{e.message}") ],
          unquote(stack)
      end
    end
  end

  ## Extraction of the tests

  defp extract(module) do
    moduledocs = extract_from_moduledoc(module.__info__(:moduledoc))

    docs = for doc <- module.__info__(:docs) do
      extract_from_doc(doc)
    end |> Enum.concat

    moduledocs ++ docs
  end

  defp extract_from_moduledoc({_, doc}) when doc in [false, nil], do: []

  defp extract_from_moduledoc({line, doc}) do
    extract_tests(line, doc)
  end

  defp extract_from_doc({_, _, _, _, doc}) when doc in [false, nil], do: []

  defp extract_from_doc({ fa, line, _, _, doc}) do
    for test <- extract_tests(line, doc) do
      test.fun_arity(fa)
    end
  end

  defp extract_tests(line, doc) do
    lines = String.split(doc, ~r/\n/, trim: false) |> adjust_indent
    extract_tests(lines, line, "", "", [], true)
  end

  defp adjust_indent(lines) do
    adjust_indent(lines, [], 0, :text)
  end

  defp adjust_indent([], adjusted_lines, _indent, _) do
    Enum.reverse adjusted_lines
  end

  @iex_prompt ["iex>", "iex("]
  @dot_prompt ["...>", "...("]

  defp adjust_indent([line|rest], adjusted_lines, indent, :text) do
    case String.starts_with?(String.lstrip(line), @iex_prompt) do
      true  -> adjust_indent([line|rest], adjusted_lines, get_indent(line, indent), :prompt)
      false -> adjust_indent(rest, adjusted_lines, indent, :text)
    end
  end

  defp adjust_indent([line|rest], adjusted_lines, indent, check) when check in [:prompt, :after_prompt] do
    stripped_line = strip_indent(line, indent)

    case String.lstrip(line) do
      "" ->
        raise Error, message: "expected non-blank line to follow iex> prompt"
      ^stripped_line ->
        :ok
      _ ->
        raise Error, message: "indentation level mismatch: #{inspect line}, should have been #{indent} spaces"
    end

    if String.starts_with?(stripped_line, @iex_prompt ++ @dot_prompt) do
      adjust_indent(rest, [stripped_line|adjusted_lines], indent, :after_prompt)
    else
      next = if check == :prompt, do: :after_prompt, else: :code
      adjust_indent(rest, [stripped_line|adjusted_lines], indent, next)
    end
  end

  defp adjust_indent([line|rest], adjusted_lines, indent, :code) do
    stripped_line = strip_indent(line, indent)
    cond do
      stripped_line == "" ->
        adjust_indent(rest, [stripped_line|adjusted_lines], 0, :text)
      String.starts_with?(String.lstrip(line), @iex_prompt) ->
        adjust_indent([line|rest], adjusted_lines, indent, :prompt)
      true ->
        adjust_indent(rest, [stripped_line|adjusted_lines], indent, :code)
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

  defp extract_tests([], _line, "", "", [], _) do
    []
  end

  defp extract_tests([], _line, "", "", acc, _) do
    Enum.reverse(reverse_last_test(acc))
  end

  # End of input and we've still got a test pending.
  defp extract_tests([], _, expr_acc, expected_acc, [test=Test[exprs: exprs]|t], _) do
    test = test.exprs([{ expr_acc, {:test, expected_acc} } | exprs])
    Enum.reverse(reverse_last_test([test|t]))
  end

  # We've encountered the next test on an adjacent line. Put them into one group.
  defp extract_tests([<< "iex>", _ :: binary>>|_] = list, line, expr_acc, expected_acc, [test=Test[exprs: exprs]|t], newtest) when expr_acc != "" and expected_acc != "" do
    test = test.exprs([{ expr_acc, {:test, expected_acc} } | exprs])
    extract_tests(list, line, "", "", [test|t], newtest)
  end

  # Store expr_acc and start a new test case.
  defp extract_tests([<< "iex>", string :: binary>>|lines], line, "", expected_acc, acc, true) do
    acc = reverse_last_test(acc)
    test = Test[line: line]
    extract_tests(lines, line, string, expected_acc, [test|acc], false)
  end

  # Store expr_acc.
  defp extract_tests([<< "iex>", string :: binary>>|lines], line, "", expected_acc, acc, false) do
    extract_tests(lines, line, string, expected_acc, acc, false)
  end

  # Still gathering expr_acc. Synonym for the next clause.
  defp extract_tests([<< "iex>", string :: binary>>|lines], line, expr_acc, expected_acc, acc, newtest) do
    extract_tests(lines, line, expr_acc <> "\n" <> string, expected_acc, acc, newtest)
  end

  # Still gathering expr_acc. Synonym for the previous clause.
  defp extract_tests([<< "...>", string :: binary>>|lines], line, expr_acc, expected_acc, acc, newtest) when expr_acc != "" do
    extract_tests(lines, line, expr_acc <> "\n" <> string, expected_acc, acc, newtest)
  end

  # Expression numbers are simply skipped.
  defp extract_tests([<< "iex(", _ :: 8, string :: binary>>|lines], line, expr_acc, expected_acc, acc, newtest) do
    extract_tests(["iex" <> skip_iex_number(string)|lines], line, expr_acc, expected_acc, acc, newtest)
  end

  # Expression numbers are simply skipped redux.
  defp extract_tests([<< "...(", _ :: 8, string :: binary>>|lines], line, expr_acc, expected_acc, acc, newtest) do
    extract_tests(["..." <> skip_iex_number(string)|lines], line, expr_acc, expected_acc, acc, newtest)
  end

  # Skip empty or documentation line.
  defp extract_tests([_|lines], line, "", "", acc, _) do
    extract_tests(lines, line, "", "", acc, true)
  end

  # Encountered an empty line, store pending test
  defp extract_tests([""|lines], line, expr_acc, expected_acc, [test=Test[exprs: exprs]|t], _) do
    test = test.exprs([{ expr_acc, {:test, expected_acc} } | exprs])
    extract_tests(lines, line,  "", "", [test|t], true)
  end

  # Exception test.
  defp extract_tests([<< "** (", string :: binary >>|lines], line, expr_acc, "", [test=Test[exprs: exprs]|t], newtest) do
    test = test.exprs([{ expr_acc, extract_error(string, "") } | exprs])
    extract_tests(lines, line,  "", "", [test|t], newtest)
  end

  # Finally, parse expected_acc.
  defp extract_tests([expected|lines], line, expr_acc, expected_acc, [test=Test[exprs: exprs]|t]=acc, newtest) do
    if expected =~ ~r/^#[A-Z][\w\.]*<.*>$/ do
      expected = expected_acc <> "\n" <> inspect(expected)
      test = test.exprs([{ expr_acc, { :inspect, expected } } | exprs])
      extract_tests(lines, line,  "", "", [test|t], newtest)
    else
      extract_tests(lines, line, expr_acc, expected_acc <> "\n" <> expected, acc, newtest)
    end
  end

  defp extract_error(<< ")", t :: binary >>, acc) do
    { :error, Module.concat([acc]), String.strip(t) }
  end

  defp extract_error(<< h, t :: binary >>, acc) do
    extract_error(t, << acc :: binary, h >>)
  end

  defp skip_iex_number(<< ")", ">", string :: binary >>) do
    ">" <> string
  end

  defp skip_iex_number(<< _ :: 8, string :: binary >>) do
    skip_iex_number(string)
  end

  defp reverse_last_test([]), do: []
  defp reverse_last_test([test=Test[exprs: exprs] | t]) do
    test = test.exprs(Enum.reverse(exprs))
    [test | t]
  end
end
