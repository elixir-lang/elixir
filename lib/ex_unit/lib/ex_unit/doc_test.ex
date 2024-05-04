defmodule ExUnit.DocTest do
  @moduledoc """
  Extract test cases from the documentation.

  Doctests allow us to generate tests from code examples found
  in `@moduledoc` and `@doc` attributes. To do this, invoke the
  `doctest/1` macro from within your test case and ensure your
  code examples are written according to the syntax and guidelines
  below.

  ## Syntax

  Every new test starts on a new line, with an `iex>` prefix.
  Multiline expressions can be used by prefixing subsequent lines
  with either `...>` (recommended) or `iex>`.

  The expected result should start the line after the `iex>`
  and `...>` line(s) and be terminated by a newline.

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

      iex> Enum.map([1, 2, 3], fn x ->
      ...>   x * 2
      ...> end)
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

      iex> a + 1 # will fail with a `undefined variable "a"` error
      2

  If you don't want to assert for every result in a doctest, you can omit
  the result. You can do so between expressions:

      iex> pid = spawn(fn -> :ok end)
      iex> is_pid(pid)
      true

  As well as at the end:

      iex> Mod.do_a_call_that_should_not_raise!(...)

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

  Imagine you have a map that contains a `DateTime` and is printed as:

      %{datetime: #DateTime<2023-06-26 09:30:00+09:00 JST Asia/Tokyo>}

  If you try to match on such an expression, `doctest` will fail to compile.
  There are two ways to resolve this.

  The first is to rely on the fact that doctest can compare internal
  structures as long as they are at the root. So one could write:

      iex> map = %{datetime: DateTime.from_naive!(~N[2023-06-26T09:30:00], "Asia/Tokyo")}
      iex> map.datetime
      #DateTime<2023-06-26 09:30:00+09:00 JST Asia/Tokyo>

  Whenever a doctest starts with "#Name<", `doctest` will perform a string
  comparison. For example, the above test will perform the following match:

      inspect(map.datetime) == "#DateTime<2023-06-26 09:30:00+09:00 JST Asia/Tokyo>"

  Alternatively, since doctest results are actually evaluated, you can have
  the `DateTime` building expression as the doctest result:

      iex> %{datetime: DateTime.from_naive!(~N[2023-06-26T09:30:00], "Asia/Tokyo")}
      %{datetime: DateTime.from_naive!(~N[2023-06-26T09:30:00], "Asia/Tokyo")}

  The downside of this approach is that the doctest result is not really
  what users would see in the terminal.

  ## Exceptions

  You can also showcase expressions raising an exception, for example:

      iex(1)> raise "some error"
      ** (RuntimeError) some error

  Doctest will look for a line starting with `** (` and it will parse it
  accordingly to extract the exception name and message. The exception parser
  will consider all following lines part of the exception message until there
  is an empty line or there is a new expression prefixed with `iex>`.
  Therefore, it is possible to match on multiline messages as long as there
  are no empty lines on the message itself.

  ## When not to use doctest

  In general, doctests are not recommended when your code examples contain
  side effects. For example, if a doctest prints to standard output, doctest
  will not try to capture the output.

  Similarly, doctests do not run in any kind of sandbox. So any module
  defined in a code example is going to linger throughout the whole test
  suite run.
  """

  @opaque_type_regex ~r/#[\w\.]+</

  defmodule Error do
    @moduledoc """
    Exception raised when there's an error with the syntax or semantics of a doctest.
    """

    @typedoc since: "1.16.0"
    @type t :: %__MODULE__{message: String.t()}
    defexception [:message]

    @impl true
    def exception(opts) do
      module = Keyword.fetch!(opts, :module)
      message = Keyword.fetch!(opts, :message)

      file = module.module_info(:compile)[:source] |> Path.relative_to_cwd()
      info = Exception.format_file_line(file, opts[:line])
      %__MODULE__{message: info <> " " <> message}
    end
  end

  @doc """
  Generate test cases from module documentation.

  Calling `doctest(Module)` will generate tests for all doctests found
  in the `module`.

  ## Options

    * `:except` - generates tests for all functions except those listed
      (list of `{function, arity}` tuples, and/or `:moduledoc`).

    * `:only` - generates tests only for functions listed
      (list of `{function, arity}` tuples, and/or `:moduledoc`).

    * `:import` - when `true`, one can test a function defined in the module
      without referring to the module name. However, this is not feasible when
      there is a clash with a module like `Kernel`. In these cases, `:import`
      should be set to `false` and `Module.function(...)` should be used instead.

    * `:tags` - a list of tags to apply to all generated doctests.

  ## Examples

      defmodule MyModuleTest do
        use ExUnit.Case
        doctest MyModule, except: [:moduledoc, trick_fun: 1]
      end

  This macro is auto-imported with every `ExUnit.Case`.
  """
  defmacro doctest(module, opts \\ []) do
    caller = __CALLER__

    require =
      if is_atom(Macro.expand(module, caller)) do
        quote do
          require unquote(module)
        end
      end

    tests =
      quote bind_quoted: [
              module: module,
              opts: opts,
              env_line: caller.line,
              env_file: caller.file
            ] do
        file = ExUnit.DocTest.__file__(module)

        for {name, test, tags} <- ExUnit.DocTest.__doctests__(module, opts) do
          @file file
          doc = ExUnit.Case.register_test(__MODULE__, env_file, env_line, :doctest, name, tags)
          def unquote(doc)(_), do: unquote(test)
        end
      end

    [require, tests]
  end

  @doc """
  Generate test cases from a markdown file.

  ## Options

    * `:tags` - a list of tags to apply to all generated doctests.

  ## Examples

      defmodule ReadmeTest do
        use ExUnit.Case
        doctest_file "README.md"
      end

  This macro is auto-imported with every `ExUnit.Case`.
  """
  @doc since: "1.15.0"
  defmacro doctest_file(file, opts \\ []) do
    caller = __CALLER__

    tests =
      quote bind_quoted: [file: file, opts: opts, env_line: caller.line, env_file: caller.file] do
        for {name, test, tags} <- ExUnit.DocTest.__doctest_file__(file, __MODULE__, opts) do
          doc = ExUnit.Case.register_test(__MODULE__, env_file, env_line, :doctest, name, tags)
          def unquote(doc)(_), do: unquote(test)
        end
      end

    tests
  end

  @doc false
  def __doctest_file__(file, module, opts) do
    doc = File.read!(file)
    file = Path.relative_to_cwd(file)
    tags = [doctest: file] ++ Keyword.get(opts, :tags, [])

    extract_tests(1, doc, module, :moduledoc)
    |> Enum.with_index(fn test, acc ->
      {"#{file} (#{acc + 1})", test_content(test, module, false, file), test_tags(test, tags)}
    end)
  end

  @doc false
  def __file__(module) do
    source =
      module.module_info(:compile)[:source] ||
        raise "#{inspect(module)} does not have compile-time source information"

    "(for doctest at) " <> Path.relative_to_cwd(source)
  end

  @doc false
  def __doctests__(module, opts) do
    tags = [doctest: module] ++ Keyword.get(opts, :tags, [])
    import = Keyword.get(opts, :import, false)
    file = module.module_info(:compile)[:source] |> Path.relative_to_cwd()

    extract(module)
    |> filter_by_opts(module, opts)
    |> Enum.sort_by(& &1.line)
    |> Enum.with_index(fn test, index ->
      compile_test(test, module, import, index + 1, file, tags)
    end)
  end

  defp filter_by_opts(tests, module, opts) do
    except = Keyword.get(opts, :except, [])

    case Keyword.fetch(opts, :only) do
      {:ok, []} -> []
      {:ok, only} -> filter_tests(module, tests, except, only)
      :error -> Stream.reject(tests, &(&1.fun_arity in except))
    end
  end

  defp filter_tests(module, tests, except, only) do
    {filtered_tests, fun_arities} =
      for test <- tests,
          test.fun_arity not in except,
          test.fun_arity in only,
          reduce: {[], []} do
        {tests, fun_arities} -> {[test | tests], [test.fun_arity | fun_arities]}
      end

    case only -- [:moduledoc | fun_arities] do
      [] ->
        filtered_tests

      undefined_fun_arities ->
        pluralized = pluralize_list_name("function", undefined_fun_arities)

        functions =
          Enum.map_join(undefined_fun_arities, "\n    ", fn {fun, arity} ->
            Exception.format_mfa(module, fun, arity)
          end)

        raise Error,
          module: module,
          message: "undefined or private #{pluralized} given to doctest:\n\n    #{functions}\n\n"
    end
  end

  defp pluralize_list_name(name, [_]), do: name
  defp pluralize_list_name(name, _), do: ExUnit.plural_rule(name)

  ## Compilation of extracted tests

  defp compile_test(test, module, do_import, n, file, tags) do
    {test_name(test, module, n), test_content(test, module, do_import, file),
     test_tags(test, tags)}
  end

  defp test_name(%{fun_arity: :moduledoc}, m, n) do
    "module #{inspect(m)} (#{n})"
  end

  defp test_name(%{fun_arity: {f, a}}, m, n) do
    "#{inspect(m)}.#{f}/#{a} (#{n})"
  end

  defp test_content(%{exprs: exprs, line: line}, module, do_import, file) do
    if multiple_exceptions?(exprs) do
      raise Error,
        line: line,
        module: module,
        message:
          "multiple exceptions in the same doctest example are not supported, " <>
            "please separate your iex> prompts by multiple newlines to start new examples"
    end

    tests = Enum.map(exprs, fn expr -> test_case_content(expr, module, file) end)
    {:__block__, [], test_import(module, do_import) ++ tests}
  end

  defp test_tags(test, tags) do
    [doctest_line: test.line, doctest_data: %{end_line: test.end_line}] ++ tags
  end

  defp multiple_exceptions?(exprs) do
    Enum.count(exprs, fn
      %{expected: {:error, _, _}} -> true
      _ -> false
    end) > 1
  end

  defp test_case_content(%{expected: :test} = data, module, file) do
    %{expr: expr, expr_line: expr_line, doctest: doctest} = data
    string_to_quoted(module, file, expr_line, expr, doctest) |> insert_assertions()
  end

  defp test_case_content(%{expected: {:test, expected}} = data, module, file) do
    %{expr: expr, expr_line: expr_line, expected_line: expected_line, doctest: doctest} = data
    expr_ast = string_to_quoted(module, file, expr_line, expr, doctest) |> insert_assertions()
    expected_ast = string_to_quoted(module, file, expected_line, expected, doctest)
    last_expr = Macro.to_string(last_expr(expr_ast))

    quote do
      # `expr_ast` may introduce variables that may be used
      # within `expected_ast` so it needs to be unquoted here.
      value = unquote(expr_ast)

      ExUnit.DocTest.__test__(
        value,
        unquote(expected_ast),
        unquote(doctest),
        unquote(last_expr),
        unquote(expected),
        unquote(module),
        unquote(file),
        unquote(expr_line)
      )
    end
  end

  defp test_case_content(%{expected: {:inspect, expected}} = data, module, file) do
    %{expr: expr, expr_line: expr_line, doctest: doctest} = data
    expr_ast = string_to_quoted(module, file, expr_line, expr, doctest) |> insert_assertions()
    last_expr = Macro.to_string(last_expr(expr_ast))

    quote do
      ExUnit.DocTest.__inspect__(
        unquote(expr_ast),
        unquote(expected),
        unquote(doctest),
        unquote(last_expr),
        unquote(inspect(expected)),
        unquote(module),
        unquote(file),
        unquote(expr_line)
      )
    end
  end

  defp test_case_content(%{expected: {:error, exception, message}} = data, module, file) do
    %{expr: expr, expr_line: expr_line, doctest: doctest} = data
    expr_ast = string_to_quoted(module, file, expr_line, expr, doctest)

    quote do
      ExUnit.DocTest.__error__(
        fn -> unquote(expr_ast) end,
        unquote(message),
        unquote(exception),
        unquote(doctest),
        unquote(module),
        unquote(file),
        unquote(expr_line)
      )
    end
  end

  @doc false
  def __test__(value, expected, doctest, last_expr, expected_expr, module, file, line) do
    case value do
      ^expected ->
        {:ok, value}

      _ ->
        error = [
          message: "Doctest failed",
          doctest: doctest,
          expr: "#{last_expr} === #{String.trim(expected_expr)}",
          left: value,
          right: expected
        ]

        reraise ExUnit.AssertionError, error, stack(module, file, line)
    end
  end

  @doc false
  def __inspect__(value, expected, doctest, last_expr, expected_expr, module, file, line) do
    result =
      try do
        inspect(value, safe: false)
      rescue
        e ->
          stack = Enum.drop(__STACKTRACE__, 1)
          {[message: Exception.message(e)], ExUnit.Runner.prune_stacktrace(stack)}
      else
        ^expected -> :ok
        actual -> {[left: actual, right: expected, message: "Doctest failed"], []}
      end

    case result do
      :ok ->
        {:ok, value}

      {extra, stack} ->
        expr = "inspect(#{last_expr}) === #{String.trim(expected_expr)}"
        error = [doctest: doctest, expr: expr] ++ extra
        reraise ExUnit.AssertionError, error, stack ++ stack(module, file, line)
    end
  end

  @doc false
  def __error__(fun, message, exception, doctest, module, file, line) do
    try do
      fun.()
    rescue
      error ->
        actual_exception = error.__struct__
        actual_message = Exception.message(error)

        failed =
          cond do
            actual_exception != exception ->
              "Doctest failed: expected exception #{inspect(exception)} but got " <>
                "#{inspect(actual_exception)} with message #{inspect(actual_message)}"

            actual_message != message ->
              "Doctest failed: wrong message for #{inspect(actual_exception)}\n" <>
                "expected:\n" <>
                "  #{inspect(message)}\n" <>
                "actual:\n" <> "  #{inspect(actual_message)}"

            true ->
              nil
          end

        if failed do
          reraise ExUnit.AssertionError,
                  [message: failed, doctest: doctest],
                  stack(module, file, line)
        end
    else
      _ ->
        failed = "Doctest failed: expected exception #{inspect(exception)} but nothing was raised"
        error = [message: failed, doctest: doctest]
        reraise ExUnit.AssertionError, error, stack(module, file, line)
    end
  end

  defp test_import(_mod, false), do: []
  defp test_import(mod, _), do: [quote(do: import(unquote(mod)))]

  defp string_to_quoted(module, file, line, expr, doctest) when is_binary(expr) do
    try do
      Code.string_to_quoted!(expr, file: file, line: line)
    rescue
      e ->
        ex_message = "(#{inspect(e.__struct__)}) #{Exception.message(e)}"
        message = "Doctest did not compile, got: #{ex_message}"

        message =
          if e.__struct__ == TokenMissingError and expr =~ @opaque_type_regex do
            message <>
              """
              \nIf you are planning to assert on the result of an iex> expression \
              which contains a value inspected as #Name<...>, please make sure \
              the inspected value is placed at the beginning of the expression, \
              otherwise Elixir will treat it as a comment due to the leading sign #.\
              """
          else
            message
          end

        opts =
          if String.valid?(doctest) do
            [message: message, doctest: doctest]
          else
            [message: message]
          end

        quote do
          reraise ExUnit.AssertionError,
                  unquote(opts),
                  unquote(Macro.escape(stack(module, file, line)))
        end
    end
  end

  defp stack(module, file, line) do
    location = [line: line, file: Path.relative_to_cwd(file)]
    [{module, :__MODULE__, 0, location}]
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

  defp extract_from_moduledoc(annotation, %{"en" => doc}, module) do
    extract_tests(:erl_anno.line(annotation), doc, module, :moduledoc)
  end

  defp extract_from_moduledoc(_, _doc, _module), do: []

  defp extract_from_docs(docs, module) do
    for doc <- docs, doc <- extract_from_doc(doc, module), do: doc
  end

  defp extract_from_doc({{_, name, arity}, annotation, _, %{"en" => doc}, _}, module) do
    line = :erl_anno.line(annotation)
    extract_tests(line, doc, module, {name, arity})
  end

  defp extract_from_doc(_doc, _module),
    do: []

  @iex_prompt ["iex>", "iex("]
  @dot_prompt ["...>", "...("]
  @fences ["```", "~~~"]

  defp adjust_indent(lines, line_no, module) do
    adjust_text(lines, line_no, [], module)
  end

  defp adjust_text([], line_no, adjusted_lines, _module) do
    {Enum.reverse(adjusted_lines), line_no - 1}
  end

  defp adjust_text([line | rest], line_no, adjusted_lines, module) do
    case String.starts_with?(String.trim_leading(line), @iex_prompt) do
      true ->
        {indent, _len} = :binary.match(line, "iex")
        adjust_code(:prompt, [line | rest], line_no, adjusted_lines, indent, module)

      false ->
        adjust_text(rest, line_no + 1, adjusted_lines, module)
    end
  end

  defp adjust_code(_kind, [], line_no, adjusted_lines, _indent, _module) do
    {Enum.reverse(adjusted_lines), line_no - 1}
  end

  defp adjust_code(kind, [line | rest], line_no, adjusted_lines, indent, module) do
    stripped_line = strip_indent(line, indent)
    trimmed_line = String.trim_leading(line)
    done? = stripped_line == "" or String.starts_with?(stripped_line, @fences)

    overrun? =
      if kind == :code,
        do: not done? and byte_size(trimmed_line) > byte_size(stripped_line),
        else: byte_size(trimmed_line) != byte_size(stripped_line)

    if overrun? do
      n_spaces = if indent == 1, do: "#{indent} space", else: "#{indent} spaces"

      raise Error,
        line: line_no,
        module: module,
        message: """
        indentation level mismatch on doctest line: #{inspect(line)}

        If you are planning to assert on the result of an `iex>` expression, \
        make sure the result is indented at the same level as `iex>`, which \
        in this case is exactly #{n_spaces}.

        If instead you have an `iex>` expression that spans over multiple lines, \
        please make sure that each line after the first one begins with `...>`.
        """
    end

    cond do
      done? ->
        adjusted_lines = [{"", line_no} | adjusted_lines]
        adjust_text(rest, line_no + 1, adjusted_lines, module)

      kind == :prompt or String.starts_with?(trimmed_line, @iex_prompt) or
          (kind == :maybe_prompt and String.starts_with?(trimmed_line, @dot_prompt)) ->
        line = {adjust_prompt(stripped_line, line_no, module), line_no}
        adjust_code(:maybe_prompt, rest, line_no + 1, [line | adjusted_lines], indent, module)

      true ->
        adjusted_lines = [{stripped_line, line_no} | adjusted_lines]
        adjust_code(:code, rest, line_no + 1, adjusted_lines, indent, module)
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

  defp adjust_prompt("iex(" <> rest = line, line_no, module),
    do: "iex>" <> skip_iex_number(rest, line_no, module, line)

  defp adjust_prompt("...(" <> rest = line, line_no, module),
    do: "...>" <> skip_iex_number(rest, line_no, module, line)

  defp adjust_prompt(line, _line_no, _module),
    do: line

  defp skip_iex_number(string, line_no, module, line) do
    case :binary.split(string, ")>") do
      [_pre, post] ->
        post

      [_] ->
        message =
          "unknown IEx prompt: #{inspect(line)}.\nAccepted formats are: iex>, iex(1)>, ...>, ...(1)>}"

        raise Error, line: line_no, module: module, message: message
    end
  end

  defp chunk_tests(lines, acc, last_no) do
    case lines
         |> Enum.drop_while(&(not test_started?(&1)))
         |> Enum.split_while(&(not test_finished?(&1))) do
      {[], []} -> Enum.reverse(acc)
      {chunk, []} -> Enum.reverse([{chunk, last_no} | acc])
      {[], [_empty_line | lines]} -> chunk_tests(lines, acc, last_no)
      {chunk, [{_, line_no} | lines]} -> chunk_tests(lines, [{chunk, line_no} | acc], last_no)
    end
  end

  defp test_started?({"iex>" <> _, _}), do: true
  defp test_started?(_), do: false

  defp test_finished?({"", _}), do: true
  defp test_finished?(_), do: false

  defp extract_tests(line_no, doc, module, fun_arity) do
    {lines, last_line} =
      doc
      |> String.split(["\r\n", "\n"], trim: false)
      |> adjust_indent(line_no + 1, module)

    lines
    |> chunk_tests([], last_line)
    |> Enum.map(&build_test(&1, fun_arity))
  end

  defp build_test({[{"iex>" <> string = line, line_no} | lines], end_line_no}, fun_arity) do
    exprs = build_test(lines, [string], [], [line], [], line_no)
    %{line: line_no, end_line: end_line_no - 1, exprs: Enum.reverse(exprs), fun_arity: fun_arity}
  end

  # Started a new expression.
  defp build_test(
         [{"iex>" <> _, new_line_no} | _] = list,
         [_ | _] = expr,
         [_ | _] = expected,
         formatted,
         acc,
         line_no
       ) do
    acc = add_expr(acc, expr, expected, formatted, line_no)
    build_test(list, [], [], [], acc, new_line_no)
  end

  # Continuation of an expression.
  defp build_test(
         [{"iex>" <> string = line, _} | lines],
         expr,
         expected,
         formatted,
         acc,
         line_no
       ) do
    expr = add_line(expr, string)
    formatted = add_line(formatted, line)
    build_test(lines, expr, expected, formatted, acc, line_no)
  end

  # Continuation of an expression.
  defp build_test(
         [{"...>" <> string = line, _} | lines],
         expr,
         expected,
         formatted,
         acc,
         line_no
       ) do
    expr = add_line(expr, string)
    formatted = add_line(formatted, line)
    build_test(lines, expr, expected, formatted, acc, line_no)
  end

  # Expected lines.
  defp build_test([{line, _} | lines], expr, expected, formatted, acc, line_no) do
    build_test(lines, expr, add_line(expected, line), formatted, acc, line_no)
  end

  # We are done.
  defp build_test([], [_ | _] = expr, expected, formatted, acc, line_no) do
    add_expr(acc, expr, expected, formatted, line_no)
  end

  defp add_line([], line), do: [line]
  defp add_line(acc, line), do: [acc, [?\n, line]]

  defp add_expr(exprs, expr_lines, expected_lines, formatted_lines, line_no) do
    expected = IO.iodata_to_binary(expected_lines)
    doctest = IO.iodata_to_binary([?\n, formatted_lines, ?\n, expected])

    expr = %{
      expr: IO.iodata_to_binary(expr_lines),
      expr_line: line_no,
      expected: tag_expected(expected),
      expected_line: line_no + length(expr_lines),
      doctest: doctest
    }

    [expr | exprs]
  end

  defp tag_expected(expected) do
    case expected do
      "" ->
        :test

      "** (" <> error ->
        [mod, message] = :binary.split(error, ")")
        {:error, Module.concat([mod]), String.trim_leading(message)}

      _ ->
        if inspectable?(expected) do
          {:inspect, expected}
        else
          {:test, expected}
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

  defp last_expr({:__block__, _, [_ | _] = block}), do: block |> List.last() |> last_expr()
  defp last_expr(other), do: other

  defp insert_assertions({:__block__, meta, block}),
    do: {:__block__, meta, Enum.map(block, &insert_match_assertion/1)}

  defp insert_assertions(ast),
    do: insert_match_assertion(ast)

  defp insert_match_assertion({:=, _, [{var, _, context}, _]} = ast)
       when is_atom(var) and is_atom(context),
       do: ast

  defp insert_match_assertion({:=, meta, [left, right]}),
    do: {{:., meta, [__MODULE__, :__assert__]}, meta, [{:=, meta, [left, right]}]}

  defp insert_match_assertion(ast),
    do: ast

  @doc false
  defmacro __assert__({:=, _, [left, right]} = assertion) do
    code = Macro.escape(assertion, prune_metadata: true)
    ExUnit.Assertions.__match__(left, right, code, :ok, __CALLER__)
  end
end
