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

  describe "with_diagnostics/2" do
    test "captures warnings" do
      assert {:warn, [%{message: "hello"}]} =
               Code.with_diagnostics(fn ->
                 IO.warn("hello")
                 :warn
               end)
    end

    test "captures and logs warnings" do
      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert {:warn, [%{message: "hello"}]} =
                        Code.with_diagnostics([log: true], fn ->
                          IO.warn("hello")
                          :warn
                        end)
             end) =~ "hello"
    end

    test "can be nested" do
      assert {:warn, [%{message: "hello"}]} =
               Code.with_diagnostics(fn ->
                 IO.warn("hello")

                 assert {:nested, [%{message: "world"}]} =
                          Code.with_diagnostics(fn ->
                            IO.warn("world")
                            :nested
                          end)

                 :warn
               end)
    end

    test "includes column information on unused variables" do
      assert {_, [%{position: {1, 12}}]} =
               Code.with_diagnostics(fn ->
                 quoted = Code.string_to_quoted!("if true do var = :foo end", columns: true)
                 Code.eval_quoted(quoted, [])
               end)
    end

    test "includes column information on unused aliases" do
      sample = """
      defmodule CodeTest.UnusedAlias do
        alias String.Chars
      end
      """

      assert {_, [%{position: {2, 3}}]} =
               Code.with_diagnostics(fn ->
                 quoted = Code.string_to_quoted!(sample, columns: true)
                 Code.eval_quoted(quoted, [])
               end)
    end

    test "includes column information on unused imports" do
      sample = """
      defmodule CodeTest.UnusedImport do
        import URI
      end
      """

      assert {_, [%{position: {2, 3}}]} =
               Code.with_diagnostics(fn ->
                 quoted = Code.string_to_quoted!(sample, columns: true)
                 Code.eval_quoted(quoted, [])
               end)
    end

    test "includes column information on unknown remote function calls" do
      sample = """
      defmodule CodeTest.UnknownRemoteCall do
        def perform do
          UnkownModule.foo()
        end
      end
      """

      assert {_, [%{position: {3, 18}}]} =
               Code.with_diagnostics(fn ->
                 quoted = Code.string_to_quoted!(sample, columns: true)
                 Code.eval_quoted(quoted, [])
               end)
    end

    test "captures unknown local calls" do
      sample = """
      defmodule CodeTest.UnknownLocalCall do
        def perform do
          foo()
        end
      end
      """

      assert {:rescued, [%{message: message}]} =
               Code.with_diagnostics(fn ->
                 try do
                   quoted = Code.string_to_quoted!(sample, columns: true)
                   Code.eval_quoted(quoted, [])
                 rescue
                   _ -> :rescued
                 end
               end)

      assert message =~ "undefined function foo/0"
    end
  end

  describe "eval_string/1,2,3" do
    test "correctly evaluates a string of code" do
      assert Code.eval_string("1 + 2") == {3, []}
      assert Code.eval_string("two = 1 + 1") == {2, [two: 2]}
    end

    test "keeps bindings on optimized evals" do
      assert Code.eval_string("import Enum", x: 1) == {Enum, [x: 1]}
    end

    test "supports a %Macro.Env{} struct as the third argument" do
      assert {3, _} = Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
    end

    test "supports unnamed scopes" do
      assert {%RuntimeError{}, [a: %RuntimeError{}]} =
               Code.eval_string("a = (try do (raise \"hello\") rescue e -> e end)")
    end

    test "returns bindings from a different context" do
      assert Code.eval_string("var!(a, Sample) = 1") == {1, [{{:a, Sample}, 1}]}
    end

    defmacro hygiene_var do
      quote do
        a = 1
      end
    end

    test "does not return bindings from macro hygiene" do
      assert Code.eval_string("require CodeTest; CodeTest.hygiene_var()") == {1, []}
    end

    test "does not raise on duplicate bindings" do
      # The order of which values win is not guaranteed, but it should evaluate successfully.
      assert Code.eval_string("b = String.Chars.to_string(a)", a: 0, a: 1) ==
               {"1", [{:b, "1"}, {:a, 1}]}

      assert Code.eval_string("b = String.Chars.to_string(a)", a: 0, a: 1, c: 2) ==
               {"1", [{:c, 2}, {:b, "1"}, {:a, 1}]}
    end

    test "keeps caller in stacktrace" do
      try do
        Code.eval_string("<<a::size(b)>>", [a: :a, b: :b], file: "myfile")
      rescue
        _ ->
          assert Enum.any?(__STACKTRACE__, &(elem(&1, 0) == __MODULE__))
      end
    end

    test "includes eval file in stacktrace" do
      try do
        Code.eval_string("<<a::size(b)>>", [a: :a, b: :b], file: "myfile")
      rescue
        _ ->
          assert Exception.format_stacktrace(__STACKTRACE__) =~ "myfile:1"
      end

      try do
        Code.eval_string(
          "Enum.map([a: :a, b: :b], fn {a, b} -> <<a::size(b)>> end)",
          [],
          file: "myfile"
        )
      rescue
        _ ->
          assert Exception.format_stacktrace(__STACKTRACE__) =~ "myfile:1"
      end
    end

    test "warns when lexical tracker process is dead" do
      {pid, ref} = spawn_monitor(fn -> :ok end)
      assert_receive {:DOWN, ^ref, _, _, _}
      env = %{__ENV__ | lexical_tracker: pid}

      assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
               assert Code.eval_string("1 + 2", [], env) == {3, []}
             end) =~ "an __ENV__ with outdated compilation information was given to eval"
    end

    test "formats diagnostic file paths as relatives" do
      {_, diagnostics} =
        Code.with_diagnostics(fn ->
          try do
            Code.eval_string("x", [])
          rescue
            e -> e
          end
        end)

      assert [
               %{
                 message: "undefined variable \"x\"",
                 position: 1,
                 file: "nofile",
                 source: "nofile",
                 stacktrace: [],
                 severity: :error
               }
             ] = diagnostics
    end
  end

  describe "eval_quoted/1" do
    test "evaluates expression" do
      assert Code.eval_quoted(quote(do: 1 + 2)) == {3, []}
      assert CodeTest.Sample.eval_quoted_info() == {CodeTest.Sample, "sample.ex", 13}
    end

    test "with %Macro.Env{} at runtime" do
      alias :lists, as: MyList
      quoted = quote(do: MyList.flatten([[1, 2, 3]]))

      assert Code.eval_quoted(quoted, [], __ENV__) == {[1, 2, 3], []}

      # Let's check it discards tracers since the lexical tracker is explicitly nil
      assert Code.eval_quoted(quoted, [], %{__ENV__ | tracers: [:bad]}) == {[1, 2, 3], []}
    end

    test "with %Macro.Env{} at compile time" do
      defmodule CompileTimeEnv do
        alias String.Chars
        {"foo", []} = Code.eval_string("Chars.to_string(:foo)", [], __ENV__)
      end
    end
  end

  test "eval_file/1" do
    assert Code.eval_file(fixture_path("code_sample.exs")) == {3, [var: 3]}

    assert_raise Code.LoadError, fn ->
      Code.eval_file("non_existent.exs")
    end
  end

  describe "eval_quoted_with_env/3" do
    test "returns results, bindings, and env" do
      alias :lists, as: MyList
      quoted = quote(do: MyList.flatten([[1, 2, 3]]))
      env = Code.env_for_eval(__ENV__)
      assert Code.eval_quoted_with_env(quoted, [], env) == {[1, 2, 3], [], env}

      quoted = quote(do: alias(:dict, as: MyDict))
      {:dict, [], env} = Code.eval_quoted_with_env(quoted, [], env)
      assert Keyword.fetch(env.aliases, Elixir.MyDict) == {:ok, :dict}
    end

    test "manages env vars" do
      env = Code.env_for_eval(__ENV__)
      {1, [x: 1], env} = Code.eval_quoted_with_env(quote(do: var!(x) = 1), [], env)
      assert Macro.Env.vars(env) == [{:x, nil}]
    end

    test "prunes vars" do
      env = Code.env_for_eval(__ENV__)

      fun = fn quoted, binding ->
        {_, binding, env} = Code.eval_quoted_with_env(quoted, binding, env, prune_binding: true)
        {binding, Macro.Env.vars(env)}
      end

      assert fun.(quote(do: 123), []) == {[], []}
      assert fun.(quote(do: 123), x: 2, y: 3) == {[], []}

      assert fun.(quote(do: var!(x) = 1), []) == {[x: 1], [x: nil]}
      assert fun.(quote(do: var!(x) = 1), x: 2, y: 3) == {[x: 1], [x: nil]}

      assert fun.(quote(do: var!(x, :foo) = 1), []) == {[{{:x, :foo}, 1}], [x: :foo]}
      assert fun.(quote(do: var!(x, :foo) = 1), x: 2, y: 3) == {[{{:x, :foo}, 1}], [x: :foo]}

      assert fun.(quote(do: var!(x, :foo) = 1), [{{:x, :foo}, 2}, {{:y, :foo}, 3}]) ==
               {[{{:x, :foo}, 1}], [x: :foo]}

      assert fun.(quote(do: fn -> var!(x, :foo) = 1 end), []) == {[], []}
      assert fun.(quote(do: fn -> var!(x, :foo) = 1 end), x: 1, y: 2) == {[], []}

      assert fun.(quote(do: fn -> var!(x) end), x: 2, y: 3) == {[x: 2], [x: nil]}

      assert fun.(quote(do: fn -> var!(x, :foo) end), [{{:x, :foo}, 2}, {{:y, :foo}, 3}]) ==
               {[{{:x, :foo}, 2}], [x: :foo]}
    end

    test "undefined function" do
      env = Code.env_for_eval(__ENV__)
      quoted = quote do: foo()

      assert_exception(
        UndefinedFunctionError,
        ["** (UndefinedFunctionError) function foo/0 is undefined (there is no such import)"],
        fn ->
          Code.eval_quoted_with_env(quoted, [], env)
        end
      )
    end

    defmodule Tracer do
      def trace(event, env) do
        send(self(), {:trace, event, env})
        :ok
      end
    end

    test "with tracing and pruning" do
      env = %{Code.env_for_eval(__ENV__) | tracers: [Tracer], function: nil}
      binding = [x: 1, y: 2, z: 3]

      quoted =
        quote do
          defmodule Elixir.CodeTest.TracingPruning do
            var!(y) = :updated
            var!(y)
            var!(x)
          end
        end

      {_, binding, env} = Code.eval_quoted_with_env(quoted, binding, env, prune_binding: true)
      assert Enum.sort(binding) == []
      assert env.versioned_vars == %{}

      assert_receive {:trace, {:on_module, _, _}, %{module: CodeTest.TracingPruning} = trace_env}

      assert trace_env.versioned_vars == %{
               {:result, :elixir_compiler} => 5,
               {:x, nil} => 1,
               {:y, nil} => 4
             }
    end

    test "with defguard" do
      require Integer
      env = Code.env_for_eval(__ENV__)
      quoted = quote do: Integer.is_even(1)
      {false, binding, env} = Code.eval_quoted_with_env(quoted, [], env, prune_binding: true)
      assert binding == []
      assert Macro.Env.vars(env) == []
    end
  end

  describe "compile_file/1" do
    test "compiles the given path" do
      assert Code.compile_file(fixture_path("code_sample.exs")) == []
      refute fixture_path("code_sample.exs") in Code.required_files()
    end
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

  test "string_to_quoted!/2 errors take lines and columns into account" do
    assert_exception(
      SyntaxError,
      ["nofile:1:5:", "syntax error before:", "1 + * 3", "^"],
      fn ->
        Code.string_to_quoted!("1 + * 3")
      end
    )

    assert_exception(
      SyntaxError,
      ["nofile:10:5:", "syntax error before:", "1 + * 3", "^"],
      fn ->
        Code.string_to_quoted!("1 + * 3", line: 10)
      end
    )

    assert_exception(
      SyntaxError,
      ["nofile:10:7:", "syntax error before:", "1 + * 3", "^"],
      fn ->
        Code.string_to_quoted!("1 + * 3", line: 10, column: 3)
      end
    )

    assert_exception(
      SyntaxError,
      ["nofile:11:7:", "syntax error before:", "1 + * 3", "^"],
      fn ->
        Code.string_to_quoted!(":ok\n1 + * 3", line: 10, column: 3)
      end
    )
  end

  test "string_to_quoted only requires the List.Chars protocol implementation to work" do
    assert {:ok, 1.23} = Code.string_to_quoted(1.23)
    assert 1.23 = Code.string_to_quoted!(1.23)
    assert {:ok, 1.23, []} = Code.string_to_quoted_with_comments(1.23)
    assert {1.23, []} = Code.string_to_quoted_with_comments!(1.23)
  end

  test "string_to_quoted returns error on incomplete escaped string" do
    assert {:error, {meta, "missing terminator: \" (for string starting at line 1)", ""}} =
             Code.string_to_quoted("\"\\")

    assert meta[:line] == 1
    assert meta[:column] == 1
    assert meta[:end_line] == 1
    assert meta[:end_column] == 3
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

    test "disables tail call optimization at the root" do
      try do
        Code.compile_string("List.flatten(123)")
      rescue
        _ -> assert Enum.any?(__STACKTRACE__, &match?({_, :__FILE__, 1, _}, &1))
      end
    end
  end

  test "format_string/2 returns empty iodata for empty string" do
    assert Code.format_string!("") == []
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

  test "ensure_all_loaded/1" do
    assert Code.ensure_all_loaded([__MODULE__]) == :ok
    assert Code.ensure_all_loaded([__MODULE__, Kernel]) == :ok

    assert {:error, [error]} = Code.ensure_all_loaded([__MODULE__, Code.NoFile, __MODULE__])
    assert error == {Code.NoFile, :nofile}
  end

  test "ensure_all_loaded!/1" do
    assert Code.ensure_all_loaded!([__MODULE__]) == :ok
    assert Code.ensure_all_loaded!([__MODULE__, Kernel]) == :ok

    message = """
    could not load the following modules:

      * Code.NoFile due to reason :nofile
      * Code.OtherNoFile due to reason :nofile\
    """

    assert_raise ArgumentError, message, fn ->
      Code.ensure_all_loaded!([__MODULE__, Code.NoFile, Code.OtherNoFile])
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

  describe "fetch_docs/1" do
    test "is case sensitive" do
      assert {:docs_v1, _, :elixir, _, %{"en" => module_doc}, _, _} = Code.fetch_docs(IO)

      assert "Functions handling input/output (IO)." =
               module_doc |> String.split("\n") |> Enum.at(0)

      assert Code.fetch_docs(Io) == {:error, :module_not_found}
    end
  end

  defp assert_exception(ex, messages, callback) do
    e =
      assert_raise ex, fn ->
        callback.()
      end

    error_msg = Exception.format(:error, e, [])

    for msg <- messages do
      assert error_msg =~ msg
    end
  end
end

defmodule Code.SyncTest do
  use ExUnit.Case

  import PathHelpers

  if System.otp_release() >= "26" do
    defp assert_cached(path) do
      assert find_path(path) != :nocache
    end

    defp refute_cached(path) do
      assert find_path(path) == :nocache
    end

    defp find_path(path) do
      {:status, _, {:module, :code_server}, [_, :running, _, _, state]} =
        :sys.get_status(:code_server)

      [:state, _, _otp_root, paths | _] = Tuple.to_list(state)
      {_, value} = List.keyfind(paths, to_charlist(path), 0)
      value
    end
  else
    defp assert_cached(_path), do: :ok
    defp refute_cached(_path), do: :ok
  end

  test "prepend_path" do
    path = Path.join(__DIR__, "fixtures")
    true = Code.prepend_path(path)
    assert to_charlist(path) in :code.get_path()
    refute_cached(path)

    true = Code.prepend_path(path, cache: true)
    assert_cached(path)

    Code.delete_path(path)
    refute to_charlist(path) in :code.get_path()
  end

  test "append_path" do
    path = Path.join(__DIR__, "fixtures")
    true = Code.append_path(path)
    assert to_charlist(path) in :code.get_path()
    refute_cached(path)

    true = Code.append_path(path, cache: true)
    assert_cached(path)

    Code.delete_path(path)
    refute to_charlist(path) in :code.get_path()
  end

  test "prepend_paths" do
    path = Path.join(__DIR__, "fixtures")
    :ok = Code.prepend_paths([path])
    assert to_charlist(path) in :code.get_path()
    refute_cached(path)

    :ok = Code.prepend_paths([path], cache: true)
    assert_cached(path)

    Code.delete_paths([path])
    refute to_charlist(path) in :code.get_path()
  end

  test "append_paths" do
    path = Path.join(__DIR__, "fixtures")
    :ok = Code.append_paths([path])
    assert to_charlist(path) in :code.get_path()
    refute_cached(path)

    :ok = Code.append_paths([path], cache: true)
    assert_cached(path)

    Code.delete_paths([path])
    refute to_charlist(path) in :code.get_path()
  end

  test "purges compiler modules" do
    quoted = quote(do: :ok)
    Code.compile_quoted(quoted)

    {:ok, claimed} = Code.purge_compiler_modules()
    assert claimed > 0

    {:ok, claimed} = Code.purge_compiler_modules()
    assert claimed == 0

    quoted = quote(do: Agent.start_link(fn -> :ok end))
    Code.compile_quoted(quoted)

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
