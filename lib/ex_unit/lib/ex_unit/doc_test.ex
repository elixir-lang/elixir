defmodule ExUnit.DocTest do
  @moduledoc """
  ExUnit.DocTest implements functionality similar to Python's doctest (http://docs.python.org/2/library/doctest.html)

  In a nutshell, it allows to use examples in module/function/macro definitions as tests. In order
  to do that, one needs to invoke `doctest/1` macro from their test case.

  The syntax for examples is as follows. Every new test starts on a new line, with
  an "iex>" prefix. Multiline expressions can be employed if the following lines
  start with either "...>" (recommended) or "iex>" prefix.

  The expected result should start at the next line after "iex>" or "...>" line(s) and
  is terminated either by a newline, new "iex>" prefix or end of the string literal.

  Example:

      iex> [1+1,
      ...>  2]
      [2,
      2]

  Similarly to the interactive use of iex you can use numbers in your "prompts":

      iex(1)> [1+2, 
      ...(1)>  3]
      [3,3]

  This is useful in two use cases:

  * Being able to refer to specific numbered scenarios in the documentation piece
  * Copy-pasting examples from an actual iex sessions

  ## Exceptions

  You can also showcase expressions raising an exception, for example:

      iex(1)> binary_to_atom((fn() -> 1 end).())
      ** (ArgumentError) argument error

  What DocTest will be looking for is a line starting with "** (" and it will parse
  it accordingly to extract the exception name and the message. At this moment,
  the exception parser would make the parser treat the next line as a start of a completely
  new expression (if it is prefixed with iex>) or a no-op line with documentation. Thus,
  multiline messages are not supported.

  ## Standard output

  Please note that DocTest will not try to capture the output, it will only check if the
  expression given evaluates to the expected result.

  ## Usage

  Currently, the only way to run doctests is to include them into an ExUnit case
  with a `doctest` macro, for example:

      defmodule MyModule.Test do
        use ExUnit.Case
        doctest MyModule
      end

  See `doctest` macro documentation for more details on how to use it
  """

  defexception CompileError, error: nil, expression: nil do
    def message(__MODULE__[error: error, expression: expr]) do
      "#{inspect elem(error, 0)}: #{error.message} while parsing expression `#{expr}`"
    end
  end

  defrecord Test, location: nil, line: nil, expr: nil, expected: nil

  @doc """
  This macro is used to generate ExUnit test cases for doctests.

  There are three ways this macro can be used:

  * `doctest(Module)` — will generate tests for all doctests found
     in the module `Module`
  * doctest(Module.function) — will generate tests for all doctests found
     in all arities of function `Module.function`
  * doctest(Module.function/arity) — will generate tests for all doctests found
     in the documentation of function `Module.function/arity`

  Options can also be supplied:

  * `:except` — generate tests for all functions except those listed (list of `{function, arity}` tuples)
  * `:only` — generate tests only forfunctions listed (list of `{function, arity}` tuples)

  This macro is auto-imported into every ExUnit.Case
  """
  defmacro doctest(testspec, options // []) do
    tests = tests(testspec, __CALLER__)

    tests = Enum.filter(tests, fn(test) ->
      {_, f, a} = test.location
      fa = {f,a}
      Enum.all?(options[:except] || [], fn(fa1) -> fa1 != fa end) and
      Enum.all?(options[:only] || [], fn(fa1) -> fa1 == fa end)
    end)

    bodies =
    lc test inlist tests do
       {m, _, _} = test.location
       expr_ast = 
       try do
         Code.string_to_ast!(test.expr, line: test.line)
       rescue e ->
         raise CompileError, error: e, expression: test.expr
       end

       expected_ast = 
       try do
         Code.string_to_ast!(test.expected, line: test.line)
       rescue e ->
         raise CompileError, error: e, expression: test.expected
       end
       quote do
         import unquote(m)
         v = unquote(expected_ast)
         case unquote(expr_ast) do
           ^v -> :ok
           instead ->
             raise ExUnit.ExpectationError,
               prelude: "Expected doctest",
               expected: unquote(test.expr),
               actual: inspect(v),
               reason: "evaluate to",
               instead: instead
         end
       end
    end
    tests = Enum.zip(:lists.seq(1, length(tests)), tests)
    lc {{n, test}, body} inlist Enum.zip(tests, bodies) do
      quote do
        test unquote("#{type(test)} (#{n}) at #{mfa(test)}"), do: unquote(body)
      end
    end
  end

  defp tests(module, _) when is_atom(module), do: extract(module)
  defp tests({:__aliases__, _, _} = module, env) do
    {m, _} = Code.eval_quoted(module, [], env)
    tests(m, env)
  end
  defp tests({:., _, [m, f]}, env) do
    {m, _} = Code.eval_quoted(m, [], env)
    extract(m, f)
  end

  defp tests({:/, _, [{:., _, [m, f]}, a]}, env) do
    {m, _} = Code.eval_quoted(m, [], env)
    extract(m, f, a)
  end

  defp mfa(Test[location: {m,nil,nil}, line: line]) do
    "#{inspect m} (#{m.module_info[:compile][:source]}:#{line})"
  end
  defp mfa(Test[location: {m,f,a}, line: line]) do
    "#{inspect m}.#{f}/#{a} (#{m.module_info[:compile][:source]}:#{line})"
  end

  defp type(Test[location: {_, nil, nil}]), do: "moduledoc"
  defp type(_), do: "doc"


  @doc false
  def extract(module) do
    Enum.map(extract_from_doc(module.__info__(:moduledoc)), fn(test) ->
      test.location({module, nil, nil})
    end) ++
    lc {{f, a}, _, _, _, _} inlist module.__info__(:docs) do
       extract(module, f, a)
    end |> List.concat
  end

  def extract(module, function) do
    lc {{f, a}, _, _, _, _} inlist module.__info__(:docs), f == function do
       extract(module, function, a)
    end |> List.concat
  end

  def extract(module, function, arity) do
    doc = List.keyfind(module.__info__(:docs), {function, arity}, 0)
    Enum.map(extract_from_doc(doc), fn(test) -> test.location({module, function, arity}) end)
  end

  defp extract_from_doc(nil), do: []
  defp extract_from_doc({_, negative}) when negative in [false, nil], do: []
  defp extract_from_doc({line, doc}) do
    lines = String.split(doc, %r/\n/) |> Enum.map(function(String.strip/1))
    extract_from_doc(lines, line, "", "",[])
  end
  defp extract_from_doc({_, _, _, _, negative}) when negative in [false, nil], do: []
  defp extract_from_doc({_, line, _, _, doc}) do
    lines = String.split(doc, %r/\n/) |> Enum.map(function(String.strip/1))
    extract_from_doc(lines, line, "", "",[])
  end

  defp extract_from_doc([], _line, "", "", acc), do: Enum.reverse(acc)
  defp extract_from_doc([], line, expr_acc, expected_acc, acc) do
    Enum.reverse([Test.new(expr: expr_acc, line: line, expected: expected_acc)|acc])
  end
  defp extract_from_doc([<< "iex>", _ :: binary>>|_] = list, line, expr_acc, expected_acc, acc) when expr_acc != "" and expected_acc != "" do
    extract_from_doc(list, line, "", "", [Test.new(expr: expr_acc, line: line, expected: expected_acc)|acc])
  end
  defp extract_from_doc([<< "iex>", string :: binary>>|lines], line, expr_acc, expected_acc, acc) when expr_acc == "" do
    extract_from_doc(lines, line, string, expected_acc, acc)
  end
  defp extract_from_doc([<< "iex>", string :: binary>>|lines], line, expr_acc, expected_acc, acc) do
    extract_from_doc(lines, line, expr_acc <> "\n" <> string, expected_acc, acc)
  end
  defp extract_from_doc([<< "...>", string :: binary>>|lines], line, expr_acc, expected_acc, acc) when expr_acc != "" do
    extract_from_doc(lines, line, expr_acc <> "\n" <> string, expected_acc, acc)
  end
  defp extract_from_doc([<< "iex(", _ :: 8, string :: binary>>|lines], line, expr_acc, expected_acc, acc) do
    extract_from_doc(["iex" <> skip_iex_number(string)|lines], line, expr_acc, expected_acc, acc)
  end
  defp extract_from_doc([<< "...(", _ :: 8, string :: binary>>|lines], line, expr_acc, expected_acc, acc) do
    extract_from_doc(["..." <> skip_iex_number(string)|lines], line, expr_acc, expected_acc, acc)
  end
  defp extract_from_doc([_|lines], line, "", "", acc) do
    extract_from_doc(lines, line, "", "", acc)
  end
  defp extract_from_doc([""|lines], line, expr_acc, expected_acc, acc) do
    extract_from_doc(lines, line,  "", "", [Test.new(expr: expr_acc, line: line, expected: expected_acc)|acc])
  end
  defp extract_from_doc([<< "** (", string :: binary >>|lines], line, expr_acc, "", acc) do
    opts = Regex.captures %r/(?<exception>.+)\) (?<message>.*)/g, string
    expected = "#{inspect Module.concat([opts[:exception]])}[message: #{inspect String.strip(opts[:message])}]"
    expr_acc = "try do ; #{expr_acc} ; rescue _e -> _e ; end"
    extract_from_doc(lines, line,  "", "", [Test.new(expr: expr_acc, line: line, expected: expected)|acc])
  end
  defp extract_from_doc([expected|lines], line, expr_acc, expected_acc, acc) do
    extract_from_doc(lines, line, expr_acc, expected_acc <> "\n" <> expected, acc)
  end

  defp skip_iex_number(<< ")", ">", string :: binary >>) do
    ">" <> string
  end
  defp skip_iex_number(<< _ :: 8, string :: binary >>) do
    skip_iex_number(string)
  end

end
