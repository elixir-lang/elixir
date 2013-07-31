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
  defined in a code example is going to linger throughout the whole test suite
  run.
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
  defmacro doctest(mod, opts // []) do
    quote do
      lc { name, test } inlist unquote(__MODULE__).__doctests__(unquote(mod), unquote(opts)) do
        def name, quote(do: [_]), [], do: test
      end
    end
  end

  @doc false
  def __doctests__(module, opts) do
    only   = opts[:only] || []
    except = opts[:except] || []
    do_import = Keyword.get(opts, :import, false)

    tests = Enum.filter(extract(module), fn(test) ->
      fa = test.fun_arity
      Enum.all?(except, &1 != fa) and Enum.all?(only, &1 == fa)
    end)

    Enum.map_reduce(tests, 1, fn(test, acc) ->
      { compile_test(test, module, do_import, acc), acc + 1 }
    end) |> elem(0)
  end

  ## Compilation of extracted tests

  defp compile_test(test, module, do_import, n) do
    { test_name(test, module, n), test_content(test, module, do_import) }
  end

  defp test_name(Test[fun_arity: nil], m, n) do
    :"test moduledoc at #{inspect m} (#{n})"
  end

  defp test_name(Test[fun_arity: { f, a }], m, n) do
    :"test doc at #{inspect m}.#{f}/#{a} (#{n})"
  end

  defp test_content(Test[exprs: exprs, line: line, fun_arity: fun_arity], module, do_import) do
    file     = module.__info__(:compile)[:source]
    location = [line: line, file: Path.relative_to(file, System.cwd!)]
    stack    = Macro.escape [{ module, :__MODULE__, 0, location }]

    exc_filter_fn = fn
      { _, {:error, _, _} } -> true
      _ -> false
    end

    exceptions_num = Enum.count exprs, exc_filter_fn
    if exceptions_num > 1 do
      # Format the info about error location as if it were a part of the stacktrace
      { fun, arity } = fun_arity
      error_info = "    #{file}:#{line}: #{inspect module}.#{fun}/#{arity}"
      raise Error, message: "multiple exceptions in one doctest case are not supported.\n#{error_info}"
    end

    { tests, whole_expr } = Enum.map_reduce exprs, "", fn {expr, expected}, acc ->
      { test_case_content(expr, expected, module, line, file, stack), acc <> expr <> "\n" }
    end
    whole_expr = String.strip(whole_expr)

    exception = case Enum.find(exprs, exc_filter_fn) do
      { _, {:error, exception, message} } ->
        inspect(exception) <> " with message " <> message
      nil ->
        "nothing"
    end

    quote do
      unquote_splicing(test_import(module, do_import))
      unquote(gen_code_for_tests(tests, whole_expr, exception, stack))
    end
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
              description: unquote(whole_expr),
              expected: unquote(exception),
              # We're using a combined message here because all expressions
              # (those that are expected to raise and those that aren't) are in
              # the same try block above.
              reason: "complete or raise",
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
              description: unquote(expr),
              expected: inspect(v),
              reason: "evaluate to",
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
            description: expr,
            expected: exception,
            reason: "raise",
            actual: inspect(v) ],
          stack
      rescue
        error in [unquote(exception)] ->
          unless error.message == unquote(message) do
            raise ExUnit.ExpectationError,
              [ prelude: "Expected doctest",
                description: expr,
                expected: exception,
                reason: "raise",
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
    location = [line: line, file: Path.relative_to(file, System.cwd!)]
    stack    = Macro.escape [{ module, :__MODULE__, 0, location }]
    try do
      Code.string_to_quoted!(expr, line: line, file: file)
    rescue e ->
      quote do
        raise ExUnit.ExpectationError,
          [ prelude: "Expected doctest",
            description: unquote(String.strip(expr)),
            expected: "successfully",
            reason: "compile",
            actual: unquote("** #{inspect e.__record__(:name)} #{e.message}") ],
          unquote(stack)
      end
    end
  end

  ## Extraction of the tests

  defp extract(module) do
    moduledocs = extract_from_moduledoc(module.__info__(:moduledoc))

    docs = lc doc inlist module.__info__(:docs) do
      extract_from_doc(doc)
    end |> List.concat

    moduledocs ++ docs
  end

  defp extract_from_moduledoc({_, negative}) when negative in [false, nil], do: []

  defp extract_from_moduledoc({line, doc}) do
    extract_tests(line, doc)
  end

  defp extract_from_doc({_, _, _, _, negative}) when negative in [false, nil], do: []

  defp extract_from_doc({ fa, line, _, _, doc}) do
    lc test inlist extract_tests(line, doc) do
      test.fun_arity(fa)
    end
  end

  defp extract_tests(line, doc) do
    lines = String.split(doc, %r/\n/) |> Enum.map(&String.strip/1)
    extract_tests(lines, line, "", "", [], true)
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
  defp extract_tests([expected|lines], line, expr_acc, expected_acc, acc, newtest) do
    extract_tests(lines, line, expr_acc, expected_acc <> "\n" <> expected, acc, newtest)
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
