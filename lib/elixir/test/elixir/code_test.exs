Code.require_file("test_helper.exs", __DIR__)

defmodule CodeTest do
  use ExUnit.Case, async: true

  doctest Code
  import PathHelpers

  def genmodule(name) do
    defmodule name do
      Kernel.LexicalTracker.references(__ENV__.lexical_tracker)
    end
  end

  contents =
    quote do
      defmodule CodeTest.Sample do
        def eval_quoted_info, do: {__MODULE__, __ENV__.file, __ENV__.line}
      end
    end

  Code.eval_quoted(contents, [], file: "sample.ex", line: 13)

  describe "cursor_context/2" do
    test "expressions" do
      assert Code.cursor_context([]) == :expr
      assert Code.cursor_context("++") == :expr
      assert Code.cursor_context(",") == :expr
      assert Code.cursor_context("[") == :expr
      assert Code.cursor_context("hello: ") == :expr
      assert Code.cursor_context("\n") == :expr
      assert Code.cursor_context('\n') == :expr
      assert Code.cursor_context("\n\n") == :expr
      assert Code.cursor_context('\n\n') == :expr
    end

    test "local_or_var" do
      assert Code.cursor_context("hello_wo") == {:local_or_var, 'hello_wo'}
      assert Code.cursor_context("hello_world?") == {:local_or_var, 'hello_world?'}
      assert Code.cursor_context("hello_world!") == {:local_or_var, 'hello_world!'}
      assert Code.cursor_context("hello/wor") == {:local_or_var, 'wor'}
      assert Code.cursor_context("hello..wor") == {:local_or_var, 'wor'}
      assert Code.cursor_context("hello::wor") == {:local_or_var, 'wor'}
      assert Code.cursor_context("[hello_wo") == {:local_or_var, 'hello_wo'}

      assert Code.cursor_context("hellò_wó") == {:local_or_var, 'hellò_wó'}
    end

    test "dot" do
      assert Code.cursor_context("hello.") == {:dot, {:var, 'hello'}, ''}
      assert Code.cursor_context(":hello.") == {:dot, {:unquoted_atom, 'hello'}, ''}
      assert Code.cursor_context("nested.map.") == {:dot, {:dot, {:var, 'nested'}, 'map'}, ''}

      assert Code.cursor_context("Hello.") == {:dot, {:alias, 'Hello'}, ''}
      assert Code.cursor_context("Hello.World.") == {:dot, {:alias, 'Hello.World'}, ''}
      assert Code.cursor_context("Hello.wor") == {:dot, {:alias, 'Hello'}, 'wor'}
      assert Code.cursor_context("hello.wor") == {:dot, {:var, 'hello'}, 'wor'}
      assert Code.cursor_context(":hello.wor") == {:dot, {:unquoted_atom, 'hello'}, 'wor'}
      assert Code.cursor_context("@hello.wor") == {:dot, {:module_attribute, 'hello'}, 'wor'}

      assert Code.cursor_context("nested.map.wor") ==
               {:dot, {:dot, {:var, 'nested'}, 'map'}, 'wor'}
    end

    test "local_arity" do
      assert Code.cursor_context("hello/") == {:local_arity, 'hello'}
    end

    test "local_call" do
      assert Code.cursor_context("hello\s") == {:local_call, 'hello'}
      assert Code.cursor_context("hello\t") == {:local_call, 'hello'}
      assert Code.cursor_context("hello(") == {:local_call, 'hello'}
      assert Code.cursor_context("hello(\s") == {:local_call, 'hello'}
      assert Code.cursor_context("hello(\t") == {:local_call, 'hello'}
    end

    test "dot_arity" do
      assert Code.cursor_context("Foo.hello/") == {:dot_arity, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello/") == {:dot_arity, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello/") == {:dot_arity, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("@f.hello/") == {:dot_arity, {:module_attribute, 'f'}, 'hello'}
    end

    test "dot_call" do
      assert Code.cursor_context("Foo.hello\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello(") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello(\s") == {:dot_call, {:alias, 'Foo'}, 'hello'}
      assert Code.cursor_context("Foo.hello(\t") == {:dot_call, {:alias, 'Foo'}, 'hello'}

      assert Code.cursor_context(":foo.hello\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello\t") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello(") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello(\s") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}
      assert Code.cursor_context(":foo.hello(\t") == {:dot_call, {:unquoted_atom, 'foo'}, 'hello'}

      assert Code.cursor_context("foo.hello\s") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello\t") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello(") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello(\s") == {:dot_call, {:var, 'foo'}, 'hello'}
      assert Code.cursor_context("foo.hello(\t") == {:dot_call, {:var, 'foo'}, 'hello'}

      assert Code.cursor_context("@f.hello\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello(") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello(\s") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
      assert Code.cursor_context("@f.hello(\t") == {:dot_call, {:module_attribute, 'f'}, 'hello'}
    end

    test "alias" do
      assert Code.cursor_context("HelloWor") == {:alias, 'HelloWor'}
      assert Code.cursor_context("Hello.Wor") == {:alias, 'Hello.Wor'}
      assert Code.cursor_context("Hello::Wor") == {:alias, 'Wor'}
      assert Code.cursor_context("Hello..Wor") == {:alias, 'Wor'}
      assert Code.cursor_context("%Hello.Wor") == {:alias, 'Hello.Wor'}
    end

    test "unquoted atom" do
      assert Code.cursor_context(":") == {:unquoted_atom, ''}
      assert Code.cursor_context(":HelloWor") == {:unquoted_atom, 'HelloWor'}
      assert Code.cursor_context(":HelloWór") == {:unquoted_atom, 'HelloWór'}
      assert Code.cursor_context(":hello_wor") == {:unquoted_atom, 'hello_wor'}
      assert Code.cursor_context(":Óla_mundo") == {:unquoted_atom, 'Óla_mundo'}
      assert Code.cursor_context("foo:hello_wor") == {:unquoted_atom, 'hello_wor'}
    end

    test "module attribute" do
      assert Code.cursor_context("@") == {:module_attribute, ''}
      assert Code.cursor_context("@hello_wo") == {:module_attribute, 'hello_wo'}
    end

    test "none" do
      # Containers
      assert Code.cursor_context(")") == :none
      assert Code.cursor_context("}") == :none

      # Numbers
      assert Code.cursor_context("123") == :none
      assert Code.cursor_context("123?") == :none
      assert Code.cursor_context("123!") == :none
      assert Code.cursor_context("123var?") == :none
      assert Code.cursor_context("0x") == :none

      # Dots
      assert Code.cursor_context("Mundo.Óla") == :none
      assert Code.cursor_context(":hello.World") == :none

      # Aliases
      assert Code.cursor_context("Hello::Wór") == :none
      assert Code.cursor_context("ÓlaMundo") == :none
      assert Code.cursor_context("HelloWór") == :none
      assert Code.cursor_context("@Hello") == :none
      assert Code.cursor_context("Hello(") == :none
      assert Code.cursor_context("Hello ") == :none
      assert Code.cursor_context("hello.World") == :none
    end

    test "newlines" do
      assert Code.cursor_context("this+does-not*matter\nHello.") == {:dot, {:alias, 'Hello'}, ''}
      assert Code.cursor_context('this+does-not*matter\nHello.') == {:dot, {:alias, 'Hello'}, ''}
    end
  end

  describe "eval_string/1,2,3" do
    test "correctly evaluates a string of code" do
      assert Code.eval_string("1 + 2") == {3, []}
      assert Code.eval_string("two = 1 + 1") == {2, [two: 2]}
    end

    test "supports a %Macro.Env{} struct as the third argument" do
      assert {3, _} = Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
    end

    test "returns bindings from a different context" do
      assert Code.eval_string("var!(a, Sample) = 1") == {1, [{{:a, Sample}, 1}]}
    end

    test "supports unnamed scopes" do
      assert {%RuntimeError{}, [a: %RuntimeError{}]} =
               Code.eval_string("a = (try do (raise \"hello\") rescue e -> e end)")
    end

    test "supports the :requires option" do
      assert Code.eval_string("Kernel.if true, do: :ok", [], requires: [Z, Kernel]) == {:ok, []}
    end

    test "with many options" do
      options = [
        functions: [{Kernel, [is_atom: 1]}],
        macros: [{Kernel, [and: 2]}],
        aliases: [{K, Kernel}],
        requires: [Kernel]
      ]

      code = "is_atom(:foo) and K.is_list([])"
      assert Code.eval_string(code, [], options) == {true, []}
    end

    test "yields the correct stacktrace" do
      try do
        Code.eval_string("<<a::size(b)>>", a: :a, b: :b)
      rescue
        _ ->
          assert Enum.any?(__STACKTRACE__, &(elem(&1, 0) == __MODULE__))
      end
    end

    test "raises streamlined argument errors" do
      assert_raise ArgumentError,
                   ~r"argument error while evaluating at line 1",
                   fn -> Code.eval_string("a <> b", a: :a, b: :b) end

      assert_raise ArgumentError,
                   ~r"argument error while evaluating example.ex at line 1",
                   fn -> Code.eval_string("a <> b", [a: :a, b: :b], file: "example.ex") end

      assert_raise ArgumentError,
                   ~r"argument error while evaluating example.ex between lines 1 and 2",
                   fn -> Code.eval_string("a <>\nb", [a: :a, b: :b], file: "example.ex") end
    end
  end

  test "eval_quoted/1" do
    assert Code.eval_quoted(quote(do: 1 + 2)) == {3, []}
    assert CodeTest.Sample.eval_quoted_info() == {CodeTest.Sample, "sample.ex", 13}
  end

  test "eval_quoted/2 with %Macro.Env{} at runtime" do
    alias :lists, as: MyList
    quoted = quote(do: MyList.flatten([[1, 2, 3]]))

    assert Code.eval_quoted(quoted, [], __ENV__) == {[1, 2, 3], []}

    # Let's check it discards tracers since the lexical tracker is explicitly nil
    assert Code.eval_quoted(quoted, [], %{__ENV__ | tracers: [:bad]}) == {[1, 2, 3], []}
  end

  test "eval_quoted/2 with %Macro.Env{} at compile time" do
    defmodule CompileTimeEnv do
      alias String.Chars
      {"foo", []} = Code.eval_string("Chars.to_string(:foo)", [], __ENV__)
    end
  end

  test "eval_file/1" do
    assert Code.eval_file(fixture_path("code_sample.exs")) == {3, [var: 3]}

    assert_raise Code.LoadError, fn ->
      Code.eval_file("non_existent.exs")
    end
  end

  test "compile_file/1" do
    assert Code.compile_file(fixture_path("code_sample.exs")) == []
    refute fixture_path("code_sample.exs") in Code.required_files()
  end

  test "compile_file/1 also emits checker warnings" do
    output =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        Code.compile_file(fixture_path("checker_warning.exs"))
      end)

    assert output =~ "incompatible types"
  end

  test "require_file/1" do
    assert Code.require_file(fixture_path("code_sample.exs")) == []
    assert fixture_path("code_sample.exs") in Code.required_files()
    assert Code.require_file(fixture_path("code_sample.exs")) == nil

    Code.unrequire_files([fixture_path("code_sample.exs")])
    refute fixture_path("code_sample.exs") in Code.required_files()
    assert Code.require_file(fixture_path("code_sample.exs")) != nil
  after
    Code.unrequire_files([fixture_path("code_sample.exs")])
  end

  test "compile source" do
    assert __MODULE__.__info__(:compile)[:source] == String.to_charlist(__ENV__.file)
  end

  test "compile info returned with source accessible through keyword module" do
    compile = __MODULE__.__info__(:compile)
    assert Keyword.get(compile, :source) != nil
  end

  describe "compile_string/1" do
    test "compiles the given string" do
      assert [{CompileStringSample, _}] =
               Code.compile_string("defmodule CompileStringSample, do: :ok")
    after
      :code.purge(CompileSimpleSample)
      :code.delete(CompileSimpleSample)
    end

    test "works across lexical scopes" do
      assert [{CompileCrossSample, _}] =
               Code.compile_string("CodeTest.genmodule CompileCrossSample")
    after
      :code.purge(CompileCrossSample)
      :code.delete(CompileCrossSample)
    end
  end

  test "ensure_loaded?/1" do
    assert Code.ensure_loaded?(__MODULE__)
    refute Code.ensure_loaded?(Code.NoFile)
  end

  test "ensure_loaded!/1" do
    assert Code.ensure_loaded!(__MODULE__) == __MODULE__

    assert_raise ArgumentError, "could not load module Code.NoFile due to reason :nofile", fn ->
      Code.ensure_loaded!(Code.NoFile)
    end
  end

  test "ensure_compiled/1" do
    assert Code.ensure_compiled(__MODULE__) == {:module, __MODULE__}
    assert Code.ensure_compiled(Code.NoFile) == {:error, :nofile}
  end

  test "ensure_compiled!/1" do
    assert Code.ensure_compiled!(__MODULE__) == __MODULE__

    assert_raise ArgumentError, "could not load module Code.NoFile due to reason :nofile", fn ->
      Code.ensure_compiled!(Code.NoFile)
    end
  end

  test "put_compiler_option/2 validates options" do
    message = "unknown compiler option: :not_a_valid_option"

    assert_raise RuntimeError, message, fn ->
      Code.put_compiler_option(:not_a_valid_option, :foo)
    end

    message = "compiler option :debug_info should be a boolean, got: :not_a_boolean"

    assert_raise RuntimeError, message, fn ->
      Code.put_compiler_option(:debug_info, :not_a_boolean)
    end
  end
end

defmodule Code.SyncTest do
  use ExUnit.Case

  import PathHelpers

  test "path manipulation" do
    path = Path.join(__DIR__, "fixtures")
    Code.prepend_path(path)
    assert to_charlist(path) in :code.get_path()

    Code.delete_path(path)
    refute to_charlist(path) in :code.get_path()
  end

  test "purges compiler modules" do
    quoted = quote(do: Agent.start_link(fn -> :ok end))
    Code.compile_quoted(quoted)

    {:ok, claimed} = Code.purge_compiler_modules()
    assert claimed > 0

    {:ok, claimed} = Code.purge_compiler_modules()
    assert claimed == 0
  end

  test "returns previous options when setting compiler options" do
    Code.compiler_options(debug_info: false)
    assert Code.compiler_options(debug_info: true) == %{debug_info: false}
  after
    Code.compiler_options(debug_info: true)
  end

  test "compile_file/1 return value" do
    assert [{CompileSample, binary}] = Code.compile_file(fixture_path("compile_sample.ex"))
    assert is_binary(binary)
  after
    :code.purge(CompileSample)
    :code.delete(CompileSample)
  end

  test "require_file/1 return value" do
    assert [{CompileSample, binary}] = Code.require_file(fixture_path("compile_sample.ex"))
    assert is_binary(binary)
  after
    Code.unrequire_files([fixture_path("compile_sample.ex")])
    :code.purge(CompileSample)
    :code.delete(CompileSample)
  end
end
