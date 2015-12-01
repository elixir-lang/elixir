Code.require_file "test_helper.exs", __DIR__

defmodule CodeTest do
  use ExUnit.Case, async: true

  doctest Code

  import PathHelpers

  def genmodule(name) do
    defmodule name do
      Kernel.LexicalTracker.remotes(__MODULE__)
    end
  end

  contents = quote do
    defmodule CodeTest.Sample do
      def eval_quoted_info, do: {__MODULE__, __ENV__.file, __ENV__.line}
    end
  end

  Code.eval_quoted contents, [], file: "sample.ex", line: 13

  test "eval string" do
    assert Code.eval_string("1 + 2") == {3, []}
    assert {3, _} = Code.eval_string("a + b", [a: 1, b: 2], Macro.Env.location(__ENV__))
  end

  test "eval string with other context" do
    assert Code.eval_string("var!(a, Sample) = 1") == {1, [{{:a, Sample}, 1}]}
  end

  test "eval binary errors" do
    msg = "nofile:2: a binary field without size is only allowed at the end of a binary pattern"
    assert_raise CompileError, msg, fn ->
      Code.eval_string("""
      foo = "foo"
      "\\"" <> bar <> "\\"" = foo
      """)
    end
  end

  test "eval with unnamed scopes" do
    assert {%RuntimeError{}, [a: %RuntimeError{}]} =
           Code.eval_string("a = (try do (raise \"hello\") rescue e -> e end)")
  end

  test "eval options" do
    assert Code.eval_string("is_atom(:foo) and K.is_list([])", [],
                            functions: [{Kernel, [is_atom: 1]}],
                            macros: [{Kernel, [..: 2, and: 2]}],
                            aliases: [{K, Kernel}],
                            requires: [Kernel]) == {true, []}
  end

  test "eval stacktrace" do
    try do
      Code.eval_string("<<a :: size(b)>>", a: :a, b: :b)
    rescue
      _ ->
        assert System.stacktrace |> Enum.any?(&(elem(&1, 0) == __MODULE__))
    end
  end

  test "eval with requires" do
    assert Code.eval_string("Kernel.if true, do: :ok", [], requires: [Z, Kernel]) == {:ok, []}
  end

  test "eval quoted" do
    assert Code.eval_quoted(quote(do: 1 + 2)) == {3, []}
    assert CodeTest.Sample.eval_quoted_info() == {CodeTest.Sample, "sample.ex", 13}
  end

  test "eval quoted with env" do
    alias :lists, as: MyList
    assert Code.eval_quoted(quote(do: MyList.flatten [[1, 2, 3]]), [], __ENV__) == {[1, 2, 3], []}
  end

  test "eval file" do
    assert Code.eval_file(fixture_path("code_sample.exs")) == {3, [var: 3]}

    assert_raise Code.LoadError, fn ->
      Code.eval_file("non_existent.exs")
    end
  end

  test "require" do
    Code.require_file fixture_path("code_sample.exs")
    assert fixture_path("code_sample.exs") in Code.loaded_files
    assert Code.require_file(fixture_path("code_sample.exs")) == nil

    Code.unload_files [fixture_path("code_sample.exs")]
    refute fixture_path("code_sample.exs") in Code.loaded_files
    assert Code.require_file(fixture_path("code_sample.exs")) != nil
  end

  test "string to quoted" do
    assert Code.string_to_quoted("1 + 2")  == {:ok, {:+, [line: 1], [1, 2]}}
    assert Code.string_to_quoted!("1 + 2") == {:+, [line: 1], [1, 2]}

    assert Code.string_to_quoted("a.1") ==
           {:error, {1, "syntax error before: ", "1"}}

    assert_raise SyntaxError, fn ->
      Code.string_to_quoted!("a.1")
    end
  end

  test "string to quoted existing atoms only" do
    assert :badarg = catch_error(Code.string_to_quoted!(":thereisnosuchatom", existing_atoms_only: true))
  end

  test "string_to_quoted!" do
    assert Code.string_to_quoted!("1 + 2") == {:+, [line: 1], [1, 2]}

    assert_raise SyntaxError, fn ->
      Code.string_to_quoted!("a.1")
    end

    assert_raise TokenMissingError, fn ->
      Code.string_to_quoted!("1 +")
    end
  end

  test "compile source" do
    assert __MODULE__.__info__(:compile)[:source] == String.to_char_list(__ENV__.file)
  end

  test "compile info returned with source accessible through keyword module" do
    compile = __MODULE__.__info__(:compile)
    assert Keyword.get(compile, :source) != nil
  end

  test "compile string works accross lexical scopes" do
    assert [{CompileCrossSample, _}] = Code.compile_string("CodeTest.genmodule CompileCrossSample")
  after
    :code.purge CompileCrossSample
    :code.delete CompileCrossSample
  end

  test "compile string" do
    assert [{CompileStringSample, _}] = Code.compile_string("defmodule CompileStringSample, do: :ok")
  after
    :code.purge CompileSimpleSample
    :code.delete CompileSimpleSample
  end

  test "compile quoted" do
    assert [{CompileQuotedSample, _}] = Code.compile_string("defmodule CompileQuotedSample, do: :ok")
  after
    :code.purge CompileQuotedSample
    :code.delete CompileQuotedSample
  end

  test "ensure_loaded?" do
    assert Code.ensure_loaded?(__MODULE__)
    refute Code.ensure_loaded?(Code.NoFile)
  end

  test "ensure_compiled?" do
    assert Code.ensure_compiled?(__MODULE__)
    refute Code.ensure_compiled?(Code.NoFile)
  end
end

defmodule Code.SyncTest do
  use ExUnit.Case

  test "path manipulation" do
    path = Path.join(__DIR__, "fixtures")
    Code.prepend_path path
    assert to_char_list(path) in :code.get_path

    Code.delete_path path
    refute to_char_list(path) in :code.get_path
  end
end
