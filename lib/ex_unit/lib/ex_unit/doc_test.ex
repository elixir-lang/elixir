defmodule ExUnit.DocTest do
  @moduledoc """
  ExUnit.DocTest implements functionality similar to [Python's
  doctest](http://docs.python.org/2/library/doctest.html).

  In a nutshell, it allows us to generate tests from the code
  examples existing in module/function/macro's documentation.
  In order to do that, one needs to invoke `doctest/1` macro
  from their test case and write their examples according
  to some guidelines.

  The syntax for examples is as follows. Every new test starts
  on a new line, with an "iex>" prefix. Multiline expressions
  can be employed if the following lines start with either
  "...>" (recommended) or "iex>" prefix.

  The expected result should start at the next line after "iex>"
  or "...>" line(s) and is terminated either by a newline, new
  "iex>" prefix or end of the string literal.

  ## Examples

  Currently, the only way to run doctests is to include them into
  an ExUnit case with a `doctest` macro:

      defmodule MyModule.Test do
        use ExUnit.Case, async: true
        doctest MyModule
      end

  The `doctest` macro is going to loop all functions and macros
  defined in `MyModule`, parsing their documentation in search for
  code examples.

  A very basic example is:

      iex> 1+1
      2

  Multiline is also supported:

      iex> Enum.map [1,2,3], fn(x) ->
      ...>   x * 2
      ...> end
      [2,4,6]

  Similarly to iex you can use numbers in your "prompts":

      iex(1)> [1+2,
      ...(1)>  3]
      [3,3]

  This is useful in two use cases:

  * Being able to refer to specific numbered scenarios
  * Copy-pasting examples from an actual iex sessions

  We also allow you to select or skip some functions when calling
  `doctest`. See its documentation documentation for more info.

  ## Exceptions

  You can also showcase expressions raising an exception, for example:

      iex(1)> binary_to_atom((fn() -> 1 end).())
      ** (ArgumentError) argument error

  What DocTest will be looking for is a line starting with "** (" and it
  will parse it accordingly to extract the exception name and the message.
  At this moment, the exception parser would make the parser treat the next
  line as a start of a completely new expression (if it is prefixed with iex>)
  or a no-op line with documentation. Thus, multiline messages are not
  supported.

  ## When not to use doctest

  In general, doctests are not recommended when your code examples contain
  side effects. For example, if a doctest prints to standard output, doctest
  will not try to capture the output.

  Similarly, doctest does not run in any kind of side box. So any module
  defined in a code example is going to linger throughout the whole test suite
  run.
  """

  defexception CompileError, error: nil, expression: nil do
    def message(__MODULE__[error: error, expression: expr]) do
      "#{inspect elem(error, 0)}: #{error.message} while parsing expression `#{expr}`"
    end
  end

  defrecord Test, fun_arity: nil, line: nil, expr: nil, expected: nil

  @doc """
  This macro is used to generate ExUnit test cases for doctests.

  There are three ways this macro can be used:

  * `doctest(Module)` — will generate tests for all doctests found
     in the module `Module`

  Options can also be supplied:

  * `:except` — generate tests for all functions except those listed
                (list of `{function, arity}` tuples)

  * `:only` — generate tests only forfunctions listed
              (list of `{function, arity}` tuples)

  ## Examples

      doctest MyModule, except: [trick_fun: 1]

  This macro is auto-imported into every `ExUnit.Case`.
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

    tests = Enum.filter(extract(module), fn(test) ->
      fa = test.fun_arity
      Enum.all?(except, &1 != fa) and Enum.all?(only, &1 == fa)
    end)

    Enum.map_reduce(tests, 1, fn(test, acc) ->
      { compile_test(test, module, acc), acc + 1 }
    end) |> elem(0)
  end

  ## Compilation of extracted tests

  defp compile_test(test, module, n) do
    { test_name(test, module, n), test_content(test, module) }
  end

  defp test_name(Test[fun_arity: nil], m, n) do
    :"test moduledoc at #{inspect m} (#{n})"
  end

  defp test_name(Test[fun_arity: { f, a }], m, n) do
    :"test doc at #{inspect m}.#{f}/#{a} (#{n})"
  end

  defp test_content(Test[expected: { :ok, expected }] = test, module) do
    line = test.line
    file = module.__info__(:compile)[:source]

    expr_ast     = string_to_ast(line, file, test.expr)
    expected_ast = string_to_ast(line, file, expected)

    quote do
      try do
        import unquote(module)
        v = unquote(expected_ast)
        case unquote(expr_ast) do
          ^v -> :ok
          instead ->
            location = [line: unquote(line), file: Path.relative_to(unquote(file), System.cwd!)]
            stack    = [{ unquote(module), :__MODULE__, 0, location }]
            raise ExUnit.ExpectationError,
              [ prelude: "Expected doctest",
                expected: unquote(test.expr),
                actual: inspect(v),
                reason: "evaluate to",
                instead: instead ],
              stack
        end
      rescue e ->
        location = [line: unquote(line), file: Path.relative_to(unquote(file), System.cwd!)]
        stack    = [{ unquote(module), :__MODULE__, 0, location }]
        raise ExUnit.ExpectationError,
          [ prelude: "Expected doctest",
            expected: unquote(test.expr),
            actual: "without an exception",
            reason: "complete",
            instead: e ],
          stack
      end
    end
  end

  defp test_content(Test[expected: { :error, exception, message }] = test, module) do
    line = test.line
    file = module.__info__(:compile)[:source]

    expr_ast = string_to_ast(line, file, test.expr)

    quote do
      import unquote(module)

      assert_raise unquote(exception), unquote(message), fn ->
        unquote(expr_ast)
      end
    end
  end

  defp string_to_ast(line, file, expr) do
    try do
      Code.string_to_ast!(expr, line: line, file: file)
    rescue e ->
      raise CompileError, error: e, expression: expr
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
    lines = String.split(doc, %r/\n/) |> Enum.map(function(String.strip/1))
    extract_tests(lines, line, "", "", [])
  end

  defp extract_tests([], _line, "", "", acc), do: Enum.reverse(acc)

  defp extract_tests([], line, expr_acc, expected_acc, acc) do
    test = Test[expr: expr_acc, line: line, expected: { :ok, expected_acc }]
    Enum.reverse([test|acc])
  end

  defp extract_tests([<< "iex>", _ :: binary>>|_] = list, line, expr_acc, expected_acc, acc) when expr_acc != "" and expected_acc != "" do
    test = Test[expr: expr_acc, line: line, expected: { :ok, expected_acc }]
    extract_tests(list, line, "", "", [test|acc])
  end

  defp extract_tests([<< "iex>", string :: binary>>|lines], line, expr_acc, expected_acc, acc) when expr_acc == "" do
    extract_tests(lines, line, string, expected_acc, acc)
  end

  defp extract_tests([<< "iex>", string :: binary>>|lines], line, expr_acc, expected_acc, acc) do
    extract_tests(lines, line, expr_acc <> "\n" <> string, expected_acc, acc)
  end

  defp extract_tests([<< "...>", string :: binary>>|lines], line, expr_acc, expected_acc, acc) when expr_acc != "" do
    extract_tests(lines, line, expr_acc <> "\n" <> string, expected_acc, acc)
  end

  defp extract_tests([<< "iex(", _ :: 8, string :: binary>>|lines], line, expr_acc, expected_acc, acc) do
    extract_tests(["iex" <> skip_iex_number(string)|lines], line, expr_acc, expected_acc, acc)
  end

  defp extract_tests([<< "...(", _ :: 8, string :: binary>>|lines], line, expr_acc, expected_acc, acc) do
    extract_tests(["..." <> skip_iex_number(string)|lines], line, expr_acc, expected_acc, acc)
  end

  defp extract_tests([_|lines], line, "", "", acc) do
    extract_tests(lines, line, "", "", acc)
  end

  defp extract_tests([""|lines], line, expr_acc, expected_acc, acc) do
    test = Test[expr: expr_acc, line: line, expected: { :ok, expected_acc }]
    extract_tests(lines, line,  "", "", [test|acc])
  end

  defp extract_tests([<< "** (", string :: binary >>|lines], line, expr_acc, "", acc) do
    test = Test[expr: expr_acc, line: line, expected: extract_error(string, "")]
    extract_tests(lines, line,  "", "", [test|acc])
  end

  defp extract_tests([expected|lines], line, expr_acc, expected_acc, acc) do
    extract_tests(lines, line, expr_acc, expected_acc <> "\n" <> expected, acc)
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
end
