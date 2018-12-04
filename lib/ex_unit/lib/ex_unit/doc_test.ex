defmodule ExUnit.DocTest do
  @moduledoc """
  ExUnit.DocTest implements functionality similar to [Python's
  doctest](https://docs.python.org/2/library/doctest.html).

  It allows us to generate tests from the code
  examples in a module/function/macro's documentation.
  To do this, invoke the `doctest/1` macro from within
  your test case and ensure your code examples are written
  according to the syntax and guidelines below.

  ## Syntax

  Every new test starts on a new line, with an `iex>` prefix.
  Multiline expressions can be used by prefixing subsequent lines with either
  `...>` (recommended) or `iex>`.

  The expected result should start at the next line after the `iex>`
  or `...>` line(s) and is terminated either by a newline, new
  `iex>` prefix or the end of the string literal.

  ## Examples

  To run doctests include them in an ExUnit case with a `doctest` macro:

      defmodule MyModuleTest do
        use ExUnit.Case, async: true
        doctest MyModule
      end

  The `doctest` macro loops through all functions and
  macros defined in `MyModule`, parsing their documentation in
  search of code examples.

  A very basic example is:

      iex> 1 + 1
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

      iex> a + 1 # will fail with a "undefined function a/0" error
      2

  If you don't want to assert for every result in a doctest, you can omit
  the result:

      iex> pid = spawn(fn -> :ok end)
      iex> is_pid(pid)
      true

  This is useful when the result is something variable (like a PID in the
  example above) or when the result is a complicated data structure and you
  don't want to show it all, but just parts of it or some of its properties.

  Similarly to IEx you can use numbers in your "prompts":

      iex(1)> [1 + 2,
      ...(1)>  3]
      [3, 3]

  This is useful in two cases:

    * being able to refer to specific numbered scenarios
    * copy-pasting examples from an actual IEx session

  You can also select or skip functions when calling
  `doctest`. See the documentation on the `:except` and `:only` options below
  for more information.

  ## Opaque types

  Some types' internal structures are kept hidden and instead show a
  user-friendly structure when inspected. The idiom in
  Elixir is to print those data types in the format `#Name<...>`. Because those
  values are treated as comments in Elixir code due to the leading
  `#` sign, they require special care when being used in doctests.

  Imagine you have a map that contains a MapSet and is printed as:

      %{users: #MapSet<[:foo, :bar]>}

  If you try to match on such an expression, `doctest` will fail to compile.
  There are two ways to resolve this.

  The first is to rely on the fact that doctest can compare internal
  structures as long as they are at the root. So one could write:

      iex> map = %{users: Enum.into([:foo, :bar], MapSet.new())}
      iex> map.users
      #MapSet<[:foo, :bar]>

  Whenever a doctest starts with "#Name<", `doctest` will perform a string
  comparison. For example, the above test will perform the following match:

      inspect(map.users) == "#MapSet<[:foo, :bar]>"

  Alternatively, since doctest results are actually evaluated, you can have
  the MapSet building expression as the doctest result:

      iex> %{users: Enum.into([:foo, :bar], MapSet.new())}
      %{users: Enum.into([:foo, :bar], MapSet.new())}

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

    @impl true
    def exception(opts) do
      module = Keyword.fetch!(opts, :module)
      message = Keyword.fetch!(opts, :message)

      file = module.__info__(:compile)[:source] |> Path.relative_to_cwd()
      info = Exception.format_file_line(file, opts[:line])
      %__MODULE__{message: info <> " " <> message}
    end
  end

  @doc """
  This macro is used to generate ExUnit test cases for doctests.

  Calling `doctest(Module)` will generate tests for all doctests found
  in the `module`.

  Options can also be given:

    * `:except` - generates tests for all functions except those listed
      (list of `{function, arity}` tuples, and/or `:moduledoc`).

    * `:only` - generates tests only for functions listed
      (list of `{function, arity}` tuples, and/or `:moduledoc`).

    * `:import` - when `true`, one can test a function defined in the module
      without referring to the module name. However, this is not feasible when
      there is a clash with a module like Kernel. In these cases, `:import`
      should be set to `false` and a full `Module.function` construct should be
      used.

  ## Examples

      doctest MyModule, except: [:moduledoc, trick_fun: 1]

  This macro is auto-imported with every `ExUnit.Case`.
  """
  defmacro doctest(module, opts \\ []) do
    require =
      if is_atom(Macro.expand(module, __CALLER__)) do
        quote do
          require unquote(module)
        end
      end

    tests =
      quote bind_quoted: [module: module, opts: opts] do
        env = __ENV__
        file = ExUnit.DocTest.__file__(module)

        for {name, test} <- ExUnit.DocTest.__doctests__(module, opts) do
          @file file
          doc = ExUnit.Case.register_test(env, :doctest, name, [])
          def unquote(doc)(_), do: unquote(test)
        end
      end

    [require, tests]
  end

  @doc false
  def __file__(module) do
    source =
      module.__info__(:compile)[:source] ||
        raise "#{inspect(module)} does not have compile-time source information"

    "(for doctest at) " <> Path.relative_to_cwd(source)
  end

  @doc false
  def __doctests__(module, opts) do
    do_import = Keyword.get(opts, :import, false)

    extract(module)
    |> filter_by_opts(opts)
    |> Stream.with_index()
    |> Enum.map(fn {test, acc} ->
      compile_test(test, module, do_import, acc + 1)
    end)
  end

  defp filter_by_opts(tests, opts) do
    except = Keyword.get(opts, :except, [])

    case Keyword.fetch(opts, :only) do
      {:ok, []} ->
        []

      {:ok, only} ->
        tests
        |> Stream.reject(&(&1.fun_arity in except))
        |> Stream.filter(&(&1.fun_arity in only))

      :error ->
        Stream.reject(tests, &(&1.fun_arity in except))
    end
  end

  ## Compilation of extracted tests

  defp compile_test(test, module, do_import, n) do
    {test_name(test, module, n), test_content(test, module, do_import)}
  end

  defp test_name(%{fun_arity: :moduledoc}, m, n) do
    "module #{inspect(m)} (#{n})"
  end

  defp test_name(%{fun_arity: {f, a}}, m, n) do
    "#{inspect(m)}.#{f}/#{a} (#{n})"
  end

  defp test_content(%{exprs: exprs, line: line}, module, do_import) do
    file = module.__info__(:compile)[:source] |> Path.relative_to_cwd()
    location = [line: line, file: file]
    stack = Macro.escape([{module, :__MODULE__, 0, location}])

    if multiple_exceptions?(exprs) do
      raise Error,
        line: line,
        module: module,
        message: "multiple exceptions in one doctest case are not supported"
    end

    tests =
      Enum.map(exprs, fn {expr, expected} ->
        test_case_content(expr, expected, location, stack)
      end)

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
        (unquote_splicing(tests))
      rescue
        e in ExUnit.AssertionError ->
          reraise e, stack

        error ->
          message =
            "Doctest failed: got #{inspect(error.__struct__)} with message " <>
              inspect(Exception.message(error))

          error = [message: message, expr: unquote(String.trim(whole_expr))]
          reraise ExUnit.AssertionError, error, __STACKTRACE__
      end
    end
  end

  defp test_case_content(expr, {:test, expected}, location, stack) do
    expr_ast = string_to_quoted(location, stack, expr)
    expected_ast = string_to_quoted(location, stack, expected)

    quote do
      expected = unquote(expected_ast)

      case unquote(expr_ast) do
        ^expected ->
          :ok

        actual ->
          expr = "#{unquote(String.trim(expr))} === #{unquote(String.trim(expected))}"
          error = [message: "Doctest failed", expr: expr, left: actual, right: expected]
          reraise ExUnit.AssertionError, error, unquote(stack)
      end
    end
  end

  defp test_case_content(expr, {:inspect, expected}, location, stack) do
    expr_ast =
      quote do
        inspect(unquote(string_to_quoted(location, stack, expr)))
      end

    expected_ast = string_to_quoted(location, stack, expected)

    quote do
      expected = unquote(expected_ast)

      case unquote(expr_ast) do
        ^expected ->
          :ok

        actual ->
          expr = "inspect(#{unquote(String.trim(expr))}) === #{unquote(String.trim(expected))}"
          error = [message: "Doctest failed", expr: expr, left: actual, right: expected]
          reraise ExUnit.AssertionError, error, unquote(stack)
      end
    end
  end

  defp test_case_content(expr, {:error, exception, message}, location, stack) do
    expr_ast = string_to_quoted(location, stack, expr)

    quote do
      stack = unquote(stack)
      expr = unquote(String.trim(expr))

      try do
        unquote(expr_ast)
      rescue
        error ->
          actual_exception = error.__struct__
          actual_message = Exception.message(error)

          message =
            cond do
              actual_exception != unquote(exception) ->
                "Doctest failed: expected exception #{inspect(unquote(exception))} but got #{
                  inspect(actual_exception)
                } with message #{inspect(actual_message)}"

              actual_message != unquote(message) ->
                "Doctest failed: wrong message for #{inspect(actual_exception)}\n" <>
                  "expected:\n" <>
                  "  #{inspect(unquote(message))}\n" <>
                  "actual:\n" <> "  #{inspect(actual_message)}"

              true ->
                nil
            end

          if message do
            reraise ExUnit.AssertionError, [message: message, expr: expr], stack
          end
      else
        _ ->
          message =
            "Doctest failed: expected exception #{inspect(unquote(exception))} but nothing was raised"

          error = [message: message, expr: expr]
          reraise ExUnit.AssertionError, error, stack
      end
    end
  end

  defp test_import(_mod, false), do: []

  defp test_import(mod, _) do
    [quote(do: import(unquote(mod)))]
  end

  defp string_to_quoted(location, stack, expr) do
    try do
      Code.string_to_quoted!(expr, location)
    rescue
      e ->
        ex_message = "(#{inspect(e.__struct__)}) #{Exception.message(e)}"
        message = "Doctest did not compile, got: #{ex_message}"

        opts =
          if String.valid?(expr) do
            [message: message, expr: String.trim(expr)]
          else
            [message: message]
          end

        quote do
          reraise ExUnit.AssertionError, unquote(opts), unquote(stack)
        end
    end
  end

  ## Extraction of the tests

  defp extract(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, annotation, _, _, moduledoc, _, docs} ->
        extract_from_moduledoc(annotation, moduledoc, module) ++
          extract_from_docs(Enum.sort(docs), module)

      {:error, reason} ->
        raise Error,
          module: module,
          message:
            "could not retrieve the documentation for module #{inspect(module)}. " <>
              explain_docs_error(reason)
    end
  end

  defp explain_docs_error(:module_not_found),
    do: "The BEAM file of the module cannot be accessed"

  defp explain_docs_error(:chunk_not_found),
    do: "The module was not compiled with documentation"

  defp explain_docs_error({:invalid_chunk, _}),
    do: "The documentation chunk in the module is invalid"

  defp extract_from_moduledoc(_, doc, _module) when doc in [:none, :hidden], do: []

  defp extract_from_moduledoc(annotation, %{"en" => doc}, module) do
    for test <- extract_tests(:erl_anno.line(annotation), doc, module) do
      normalize_test(test, :moduledoc)
    end
  end

  defp extract_from_docs(docs, module) do
    for doc <- docs, doc <- extract_from_doc(doc, module), do: doc
  end

  defp extract_from_doc({{kind, _, _}, _, _, doc, _}, _module)
       when kind not in [:function, :macro] or doc in [:none, :hidden],
       do: []

  defp extract_from_doc({{_, name, arity}, annotation, _, %{"en" => doc}, _}, module) do
    line = :erl_anno.line(annotation)

    for test <- extract_tests(line, doc, module) do
      normalize_test(test, {name, arity})
    end
  end

  defp extract_tests(line_no, doc, module) do
    all_lines = String.split(doc, "\n", trim: false)
    lines = adjust_indent(all_lines, line_no + 1, module)
    extract_tests(lines, "", "", [], true, module)
  end

  @iex_prompt ["iex>", "iex("]
  @dot_prompt ["...>", "...("]

  defp adjust_indent(lines, line_no, module) do
    adjust_indent(:text, lines, line_no, [], 0, module)
  end

  defp adjust_indent(:after_prompt, [], line_no, _adjusted_lines, _indent, module) do
    raise_incomplete_doctest(line_no, module)
  end

  defp adjust_indent(_kind, [], _line_no, adjusted_lines, _indent, _module) do
    Enum.reverse(adjusted_lines)
  end

  defp adjust_indent(:text, [line | rest], line_no, adjusted_lines, indent, module) do
    case String.starts_with?(String.trim_leading(line), @iex_prompt) do
      true ->
        line_indent = get_indent(line, indent)
        adjust_indent(:prompt, [line | rest], line_no, adjusted_lines, line_indent, module)

      false ->
        adjust_indent(:text, rest, line_no + 1, adjusted_lines, indent, module)
    end
  end

  defp adjust_indent(kind, [line | rest], line_no, adjusted_lines, indent, module)
       when kind in [:prompt, :after_prompt] do
    stripped_line = strip_indent(line, indent)

    case String.trim_leading(line) do
      "" ->
        raise_incomplete_doctest(line_no, module)

      ^stripped_line ->
        :ok

      _ ->
        n_spaces = if indent == 1, do: "#{indent} space", else: "#{indent} spaces"

        raise Error,
          line: line_no,
          module: module,
          message: """
          indentation level mismatch on doctest line: #{inspect(line)}

          If you are planning to assert on the result of an `iex>` expression, \
          make sure the result is indented at the beginning of `iex>`, which \
          in this case is exactly #{n_spaces} spaces.

          If instead you have an `iex>` expression that spans over multiple lines, \
          please make sure that each line after the first one begins with `...>`.
          """
    end

    adjusted_lines = [{stripped_line, line_no} | adjusted_lines]

    next =
      cond do
        kind == :prompt -> :after_prompt
        String.starts_with?(stripped_line, @iex_prompt ++ @dot_prompt) -> :after_prompt
        true -> :code
      end

    adjust_indent(next, rest, line_no + 1, adjusted_lines, indent, module)
  end

  defp adjust_indent(:code, [line | rest], line_no, adjusted_lines, indent, module) do
    stripped_line = strip_indent(line, indent)

    cond do
      stripped_line == "" ->
        adjusted_lines = [{stripped_line, line_no} | adjusted_lines]
        adjust_indent(:text, rest, line_no + 1, adjusted_lines, 0, module)

      String.starts_with?(String.trim_leading(line), @iex_prompt) ->
        adjust_indent(:prompt, [line | rest], line_no, adjusted_lines, indent, module)

      true ->
        adjusted_lines = [{stripped_line, line_no} | adjusted_lines]
        adjust_indent(:code, rest, line_no + 1, adjusted_lines, indent, module)
    end
  end

  defp get_indent(line, current_indent) do
    case :binary.match(line, "iex") do
      {pos, _len} -> pos
      :nomatch -> current_indent
    end
  end

  defp strip_indent(line, indent) do
    length = byte_size(line) - indent

    if length > 0 do
      binary_part(line, indent, length)
    else
      ""
    end
  end

  @fences ["```", "~~~"]

  defp extract_tests(lines, expr_acc, expected_acc, acc, new_test, module)

  defp extract_tests([], "", "", [], _, _) do
    []
  end

  defp extract_tests([], "", "", acc, _, _) do
    Enum.reverse(acc)
  end

  # End of input and we've still got a test pending.
  defp extract_tests([], expr_acc, expected_acc, [test | rest], _, _) do
    test = add_expr(test, expr_acc, expected_acc)
    Enum.reverse([test | rest])
  end

  # We've encountered the next test on an adjacent line. Put them into one group.
  defp extract_tests(
         [{"iex>" <> _, _} | _] = list,
         expr_acc,
         expected_acc,
         [test | rest],
         new_test,
         module
       )
       when expr_acc != "" and expected_acc != "" do
    test = add_expr(test, expr_acc, expected_acc)
    extract_tests(list, "", "", [test | rest], new_test, module)
  end

  # Store expr_acc and start a new test case.
  defp extract_tests([{"iex>" <> string, line_no} | lines], "", expected_acc, acc, true, module) do
    test = %{line: line_no, fun_arity: nil, exprs: []}
    extract_tests(lines, string, expected_acc, [test | acc], false, module)
  end

  # Store expr_acc.
  defp extract_tests([{"iex>" <> string, _} | lines], "", expected_acc, acc, false, module) do
    extract_tests(lines, string, expected_acc, acc, false, module)
  end

  # Still gathering expr_acc. Synonym for the next clause.
  defp extract_tests(
         [{"iex>" <> string, _} | lines],
         expr_acc,
         expected_acc,
         acc,
         new_test,
         module
       ) do
    extract_tests(lines, expr_acc <> "\n" <> string, expected_acc, acc, new_test, module)
  end

  # Still gathering expr_acc. Synonym for the previous clause.
  defp extract_tests(
         [{"...>" <> string, _} | lines],
         expr_acc,
         expected_acc,
         acc,
         new_test,
         module
       )
       when expr_acc != "" do
    extract_tests(lines, expr_acc <> "\n" <> string, expected_acc, acc, new_test, module)
  end

  # Expression numbers are simply skipped.
  defp extract_tests(
         [{<<"iex(", _>> <> string = line, line_no} | lines],
         expr_acc,
         expected_acc,
         acc,
         new_test,
         module
       ) do
    new_line = {"iex" <> skip_iex_number(string, module, line_no, line), line_no}
    extract_tests([new_line | lines], expr_acc, expected_acc, acc, new_test, module)
  end

  # Expression numbers are simply skipped redux.
  defp extract_tests(
         [{<<"...(", _>> <> string, line_no} = line | lines],
         expr_acc,
         expected_acc,
         acc,
         new_test,
         module
       ) do
    new_line = {"..." <> skip_iex_number(string, module, line_no, line), line_no}
    extract_tests([new_line | lines], expr_acc, expected_acc, acc, new_test, module)
  end

  # Skip empty or documentation line.
  defp extract_tests([_ | lines], "", "", acc, _, module) do
    extract_tests(lines, "", "", acc, true, module)
  end

  # Encountered end of fenced code block, store pending test
  defp extract_tests(
         [{<<fence::3-bytes>> <> _, _} | lines],
         expr_acc,
         expected_acc,
         [test | rest],
         _new_test,
         module
       )
       when fence in @fences and expr_acc != "" do
    test = add_expr(test, expr_acc, expected_acc)
    extract_tests(lines, "", "", [test | rest], true, module)
  end

  # Encountered an empty line, store pending test
  defp extract_tests([{"", _} | lines], expr_acc, expected_acc, [test | rest], _new_test, module) do
    test = add_expr(test, expr_acc, expected_acc)
    extract_tests(lines, "", "", [test | rest], true, module)
  end

  # Finally, parse expected_acc.
  defp extract_tests([{expected, _} | lines], expr_acc, "", acc, new_test, module) do
    extract_tests(lines, expr_acc, expected, acc, new_test, module)
  end

  defp extract_tests([{expected, _} | lines], expr_acc, expected_acc, acc, new_test, module) do
    extract_tests(lines, expr_acc, expected_acc <> "\n" <> expected, acc, new_test, module)
  end

  defp skip_iex_number(")>" <> string, _module, _line_no, _line) do
    ">" <> string
  end

  defp skip_iex_number("", module, line_no, line) do
    message =
      "unknown IEx prompt: #{inspect(line)}.\nAccepted formats are: iex>, iex(1)>, ...>, ...(1)>}"

    raise Error, line: line_no, module: module, message: message
  end

  defp skip_iex_number(<<_>> <> string, module, line_no, line) do
    skip_iex_number(string, module, line_no, line)
  end

  defp normalize_test(%{exprs: exprs} = test, fa) do
    %{test | fun_arity: fa, exprs: Enum.reverse(exprs)}
  end

  defp add_expr(%{exprs: exprs} = test, expr, expected) do
    %{test | exprs: [{expr, tag_expected(expected)} | exprs]}
  end

  defp tag_expected(string) do
    case string do
      "** (" <> error ->
        [mod, message] = :binary.split(error, ")")
        {:error, Module.concat([mod]), String.trim_leading(message)}

      _ ->
        if inspectable?(string) do
          {:inspect, inspect(string)}
        else
          {:test, string}
        end
    end
  end

  defp inspectable?(<<?#, char, rest::binary>>) when char in ?A..?Z, do: inspectable_end?(rest)
  defp inspectable?(_), do: false

  defp inspectable_end?(<<?., char, rest::binary>>) when char in ?A..?Z,
    do: inspectable_end?(rest)

  defp inspectable_end?(<<char, rest::binary>>)
       when char in ?A..?Z
       when char in ?a..?z
       when char in ?0..?9
       when char == ?_,
       do: inspectable_end?(rest)

  defp inspectable_end?(<<?<, _::binary>>), do: true
  defp inspectable_end?(_), do: false

  defp raise_incomplete_doctest(line_no, module) do
    raise Error,
      line: line_no,
      module: module,
      message: "expected non-blank line to follow iex> prompt"
  end
end
