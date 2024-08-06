Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.XrefTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  setup_all do
    previous = Application.get_env(:elixir, :ansi_enabled, false)
    Application.put_env(:elixir, :ansi_enabled, false)
    on_exit(fn -> Application.put_env(:elixir, :ansi_enabled, previous) end)
  end

  setup do
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  describe "calls/1" do
    test "returns all function calls" do
      files = %{
        "lib/a.ex" => """
        defmodule A do
          def a, do: A.a()
          def a(arg), do: A.a(arg)
          def c, do: B.a()
        end
        """,
        "lib/b.ex" => """
        defmodule B do
          def a, do: nil
        end
        """
      }

      output = [
        %{callee: {A, :a, 0}, caller_module: A, file: "lib/a.ex", line: 2},
        %{callee: {A, :a, 1}, caller_module: A, file: "lib/a.ex", line: 3},
        %{callee: {B, :a, 0}, caller_module: A, file: "lib/a.ex", line: 4}
      ]

      assert_all_calls(files, output)
    end

    test "returns function call inside expanded macro" do
      files = %{
        "lib/a.ex" => """
        defmodule A do
          defmacro a_macro(x) do
            quote do
              A.b(unquote(x))
            end
          end
          def b(x), do: x
        end
        """,
        "lib/b.ex" => """
        defmodule B do
          require A
          def a, do: A.a_macro(1)
        end
        """
      }

      output = [
        %{callee: {A, :b, 1}, caller_module: B, file: "lib/b.ex", line: 3}
      ]

      assert_all_calls(files, output)
    end

    defp assert_all_calls(files, expected, after_compile \\ fn -> :ok end) do
      in_fixture("no_mixfile", fn ->
        generate_files(files)

        Mix.Task.run("compile")
        after_compile.()
        xref = String.to_atom("Elixir.Mix.Tasks.Xref")
        assert Enum.sort(xref.calls()) == Enum.sort(expected)
      end)
    end
  end

  describe "mix xref callers MODULE" do
    @callers_files %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: :ok
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        def b, do: A.a()
      end
      """
    }

    @callers_output """
    Compiling 2 files (.ex)
    Generated sample app
    lib/b.ex (runtime)
    """

    test "prints callers of specified Module" do
      assert_callers("A", @callers_files, @callers_output)
    end

    test "filter by compile-connected label with fail-above" do
      message = "Too many references (found: 1, permitted: 0)"

      assert_raise Mix.Error, message, fn ->
        assert_callers(~w[--fail-above 0], "A", @callers_files, @callers_output)
      end
    end

    test "handles aliases" do
      files = %{
        "lib/a.ex" => """
        defmodule A do
          alias Enum, as: E

          def a(a, b), do: E.map(a, b)

          @file "lib/external_source.ex"
          def b() do
            alias Enum, as: EE
            EE.map([], &EE.flatten/1)
          end
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/a.ex (runtime)
      """

      assert_callers("Enum", files, output)
    end

    test "handles imports" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          import Integer
          &is_even/1
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          import Integer
          parse("1")
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/a.ex (compile)
      lib/b.ex (compile)
      """

      assert_callers("Integer", files, output)
    end

    test "no argument gives error" do
      in_fixture("no_mixfile", fn ->
        message = "xref doesn't support this command. For more information run \"mix help xref\""

        assert_raise Mix.Error, message, fn ->
          assert Mix.Task.run("xref", ["callers"]) == :error
        end
      end)
    end

    test "callers: gives nice error for quotable but invalid callers spec" do
      in_fixture("no_mixfile", fn ->
        message = "xref callers MODULE expects a MODULE, got: Module.func(arg)"

        assert_raise Mix.Error, message, fn ->
          Mix.Task.run("xref", ["callers", "Module.func(arg)"])
        end
      end)
    end

    test "gives nice error for unquotable callers spec" do
      in_fixture("no_mixfile", fn ->
        message = "xref callers MODULE expects a MODULE, got: %"

        assert_raise Mix.Error, message, fn ->
          Mix.Task.run("xref", ["callers", "%"])
        end
      end)
    end

    defp assert_callers(opts \\ [], module, files, expected) do
      in_fixture("no_mixfile", fn ->
        for {file, contents} <- files do
          File.write!(file, contents)
        end

        capture_io(:stderr, fn ->
          assert Mix.Task.run("xref", opts ++ ["callers", module]) == :ok
        end)

        assert ^expected = receive_until_no_messages([])
      end)
    end
  end

  describe "mix xref trace FILE" do
    test "shows labelled traces" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          defstruct [:foo, :bar]
          defmacro macro, do: :ok
          def fun, do: :ok
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          import A
          A.macro()
          macro()
          A.fun()
          fun()
          def calls_macro, do: A.macro()
          def calls_fun, do: A.fun()
          def calls_struct, do: %A{}
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/b.ex:2: require A (export)
      lib/b.ex:3: call A.macro/0 (compile)
      lib/b.ex:4: import call A.macro/0 (compile)
      lib/b.ex:5: call A.fun/0 (compile)
      lib/b.ex:6: import call A.fun/0 (compile)
      lib/b.ex:7: call A.macro/0 (compile)
      lib/b.ex:8: call A.fun/0 (runtime)
      lib/b.ex:9: struct A (export)
      """

      assert_trace("lib/b.ex", files, output)
    end

    test "ignores dependencies from patterns and guards" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          defstruct [:foo, :bar]
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          def pattern(A), do: true
          def guard(a) when is_struct(a, A), do: true
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      """

      assert_trace("lib/b.ex", files, output)
    end

    test "shows traces for module callbacks" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          @before_compile :"Elixir.B"
          @after_compile :"Elixir.B"
          @after_verify :"Elixir.B"
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          defmacro __before_compile__(_env), do: :ok
          defmacro __after_compile__(_env, _binary), do: :ok
          def __after_verify__(_module), do: :ok
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/a.ex:1: call B.__after_compile__/2 (compile)
      lib/a.ex:1: call B.__after_verify__/1 (compile)
      lib/a.ex:1: call B.__before_compile__/1 (compile)
      """

      assert_trace("lib/a.ex", files, output)
    end

    test "shows module with `@behaviour` calling `behaviour_info/1`" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          @callback fun() :: integer
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          @behaviour :"Elixir.A"
          def fun, do: 42
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/b.ex:1: call A.behaviour_info/1 (runtime)
      """

      assert_trace("lib/b.ex", files, output)
    end

    test "filters per label" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          defmacro macro, do: :ok
          def fun, do: :ok
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          require A
          def calls_macro, do: A.macro()
          def calls_fun, do: A.fun()
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/b.ex:3: call A.macro/0 (compile)
      """

      assert_trace(~w[--label compile], "lib/b.ex", files, output)
    end

    test "fails if above limit per label" do
      files = %{
        "lib/a.ex" => ~S"""
        defmodule A do
          defmacro macro, do: :ok
          def fun, do: :ok
        end
        """,
        "lib/b.ex" => ~S"""
        defmodule B do
          require A
          def calls_macro, do: A.macro()
          def calls_fun, do: A.fun()
        end
        """
      }

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/b.ex:3: call A.macro/0 (compile)
      """

      message = "Too many traces (found: 1, permitted: 0)"

      assert_raise Mix.Error, message, fn ->
        assert_trace(~w[--label compile --fail-above 0], "lib/b.ex", files, output)
      end
    end

    defp assert_trace(opts \\ [], file, files, expected) do
      in_fixture("no_mixfile", fn ->
        for {file, contents} <- files do
          File.write!(file, contents)
        end

        capture_io(:stderr, fn ->
          assert Mix.Task.run("xref", opts ++ ["trace", file]) == :ok
        end)

        assert ^expected = receive_until_no_messages([])
      end)
    end
  end

  describe "mix xref graph" do
    test "basic usage" do
      assert_graph("""
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      |-- lib/a.ex
      |-- lib/c.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      lib/d.ex
      `-- lib/e.ex
      lib/e.ex
      """)
    end

    test "stats" do
      assert_graph(["--format", "stats"], """
      Tracked files: 5 (nodes)
      Compile dependencies: 3 (edges)
      Exports dependencies: 0 (edges)
      Runtime dependencies: 3 (edges)
      Cycles: 1

      Top 5 files with most outgoing dependencies:
        * lib/b.ex (3)
        * lib/d.ex (1)
        * lib/c.ex (1)
        * lib/a.ex (1)
        * lib/e.ex (0)

      Top 5 files with most incoming dependencies:
        * lib/e.ex (2)
        * lib/d.ex (1)
        * lib/c.ex (1)
        * lib/b.ex (1)
        * lib/a.ex (1)
      """)
    end

    test "stats with compile label" do
      assert_graph(["--format", "stats", "--label", "compile"], """
      Tracked files: 5 (nodes)
      Compile dependencies: 3 (edges)
      Exports dependencies: 0 (edges)
      Runtime dependencies: 3 (edges)
      Cycles: 1

      Top 5 files with most outgoing dependencies:
        * lib/c.ex (1)
        * lib/b.ex (1)
        * lib/a.ex (1)
        * lib/e.ex (0)
        * lib/d.ex (0)

      Top 5 files with most incoming dependencies:
        * lib/e.ex (1)
        * lib/d.ex (1)
        * lib/b.ex (1)
        * lib/c.ex (0)
        * lib/a.ex (0)
      """)
    end

    test "stats with compile-connected label" do
      assert_graph(["--format", "stats", "--label", "compile-connected"], """
      Tracked files: 5 (nodes)
      Compile dependencies: 3 (edges)
      Exports dependencies: 0 (edges)
      Runtime dependencies: 3 (edges)
      Cycles: 1

      Top 5 files with most outgoing dependencies:
        * lib/c.ex (1)
        * lib/b.ex (1)
        * lib/a.ex (1)
        * lib/e.ex (0)
        * lib/d.ex (0)

      Top 5 files with most incoming dependencies:
        * lib/d.ex (1)
        * lib/b.ex (1)
        * lib/e.ex (0)
        * lib/c.ex (0)
        * lib/a.ex (0)
      """)
    end

    test "cycles" do
      assert_graph(["--format", "cycles"], """
      1 cycles found. Showing them in decreasing size:

      Cycle of length 2:

          lib/b.ex (compile)
          lib/a.ex

      """)
    end

    test "cycles with compile label require at least one of such type" do
      assert_graph(["--format", "cycles", "--label", "compile"], """
      1 cycles found. Showing them in decreasing size:

      Cycle of length 2:

          lib/b.ex (compile)
          lib/a.ex

      """)
    end

    test "cycles with compile-connected label is the same as compile" do
      assert_graph(["--format", "cycles", "--label", "compile-connected"], """
      1 cycles found. Showing them in decreasing size:

      Cycle of length 2:

          lib/b.ex (compile)
          lib/a.ex

      """)
    end

    test "cycles with `--fail-above`" do
      message = "Too many cycles (found: 1, permitted: 0)"

      assert_raise Mix.Error, message, fn ->
        assert_graph(["--format", "cycles", "--fail-above", "0"], """
        1 cycles found. Showing them in decreasing size:

        Cycle of length 3:

            lib/b.ex
            lib/a.ex
            lib/b.ex

        """)
      end
    end

    test "cycles with min cycle size" do
      assert_graph(["--format", "cycles", "--min-cycle-size", "3"], """
      No cycles found
      """)
    end

    test "unknown label" do
      assert_raise Mix.Error, "Unknown --label bad in mix xref graph", fn ->
        assert_graph(["--label", "bad"], "")
      end
    end

    test "unknown format" do
      assert_raise Mix.Error, "Unknown --format bad in mix xref graph", fn ->
        assert_graph(["--format", "bad"], "")
      end
    end

    test "exclude many" do
      assert_graph(~w[--exclude lib/c.ex --exclude lib/b.ex], """
      lib/a.ex
      lib/d.ex
      `-- lib/e.ex
      lib/e.ex
      """)
    end

    test "exclude one" do
      assert_graph(~w[--exclude lib/d.ex], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      |-- lib/a.ex
      |-- lib/c.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      lib/e.ex
      """)
    end

    @abc_linear_files %{
      "lib/a.ex" => "defmodule A, do: def(a, do: B.b())",
      "lib/b.ex" => "defmodule B, do: def(b, do: C.c())",
      "lib/c.ex" => "defmodule C, do: def(c, do: true)"
    }

    test "exclude one from linear case" do
      assert_graph(
        ~w[--exclude lib/b.ex],
        """
        lib/a.ex
        lib/c.ex
        """,
        files: @abc_linear_files
      )
    end

    test "exclude one with source from linear case" do
      assert_graph(
        ~w[--exclude lib/b.ex --source lib/a.ex],
        """
        lib/a.ex
        """,
        files: @abc_linear_files
      )
    end

    test "invalid exclude" do
      assert_raise Mix.Error, "Excluded files could not be found: lib/a2.ex, lib/a3.ex", fn ->
        assert_graph(~w[--exclude lib/a2.ex --exclude lib/a.ex --exclude lib/a3.ex], "")
      end
    end

    test "only nodes" do
      assert_graph(~w[--only-nodes], """
      lib/a.ex
      lib/b.ex
      lib/c.ex
      lib/d.ex
      lib/e.ex
      """)
    end

    test "only nodes with compile direct label" do
      assert_graph(~w[--label compile --only-direct --only-nodes], """
      lib/a.ex
      lib/b.ex
      lib/c.ex
      """)
    end

    test "filter by compile label" do
      assert_graph(~w[--label compile], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      """)
    end

    test "filter by compile-connected label" do
      assert_graph(~w[--label compile-connected], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      """)
    end

    test "filter by compile-connected label with exclusions" do
      assert_graph(~w[--label compile-connected --exclude lib/e.ex], """
      lib/a.ex
      `-- lib/b.ex (compile)
      """)
    end

    test "filter by compile-connected label with fail-above" do
      message = "Too many references (found: 2, permitted: 1)"

      assert_raise Mix.Error, message, fn ->
        assert_graph(~w[--label compile-connected --fail-above 1], """
        lib/a.ex
        `-- lib/b.ex (compile)
        lib/c.ex
        `-- lib/d.ex (compile)
        """)
      end
    end

    test "exclude many with fail-above" do
      message = "Too many references (found: 1, permitted: 0)"

      assert_raise Mix.Error, message, fn ->
        assert_graph(~w[--exclude lib/c.ex --exclude lib/b.ex --fail-above 0], """
        lib/a.ex
        lib/d.ex
        `-- lib/e.ex
        lib/e.ex
        """)
      end
    end

    test "filter by compile direct label" do
      assert_graph(~w[--label compile --only-direct], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      """)
    end

    test "filter by runtime label" do
      assert_graph(~w[--label runtime], """
      lib/b.ex
      |-- lib/a.ex
      `-- lib/c.ex
      lib/d.ex
      `-- lib/e.ex
      """)
    end

    test "sources" do
      assert_graph(~w[--source lib/a.ex --source lib/c.ex], """
      lib/a.ex
      `-- lib/b.ex (compile)
          |-- lib/a.ex
          |-- lib/c.ex
          `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
          `-- lib/e.ex
      """)
    end

    test "source with compile label" do
      assert_graph(~w[--source lib/a.ex --label compile], """
      lib/a.ex
      `-- lib/b.ex (compile)
          `-- lib/e.ex (compile)
      """)
    end

    test "source with compile-connected label" do
      assert_graph(~w[--source lib/a.ex --label compile-connected], """
      lib/a.ex
      `-- lib/b.ex (compile)
      """)
    end

    test "source with compile direct label" do
      assert_graph(~w[--source lib/a.ex --label compile --only-direct], """
      lib/a.ex
      `-- lib/b.ex (compile)
          `-- lib/e.ex (compile)
      """)
    end

    test "invalid sources" do
      assert_raise Mix.Error, "Sources could not be found: lib/a2.ex, lib/a3.ex", fn ->
        assert_graph(~w[--source lib/a2.ex --source lib/a.ex --source lib/a3.ex], "")
      end
    end

    test "sink" do
      assert_graph(~w[--sink lib/e.ex], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      |-- lib/a.ex
      |-- lib/c.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      lib/d.ex
      `-- lib/e.ex
      """)
    end

    test "sink with compile label" do
      assert_graph(~w[--sink lib/e.ex --label compile], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      """)
    end

    test "sink with compile-connected label" do
      assert_graph(~w[--sink lib/e.ex --label compile-connected], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      """)
    end

    test "sink with compile direct label" do
      assert_graph(~w[--sink lib/e.ex --label compile --only-direct], """
      lib/a.ex
      `-- lib/b.ex (compile)
      lib/b.ex
      `-- lib/e.ex (compile)
      """)
    end

    test "multiple sinks" do
      assert_graph(~w[--sink lib/a.ex --sink lib/c.ex], """
      lib/b.ex
      |-- lib/a.ex
      |   `-- lib/b.ex (compile)
      `-- lib/c.ex
      """)
    end

    test "multiple sinks with only nodes" do
      assert_graph(~w[--sink lib/a.ex --sink lib/c.ex --sink lib/e.ex --only-nodes], """
      lib/b.ex
      lib/d.ex
      """)
    end

    test "invalid sink" do
      assert_raise Mix.Error, "Sinks could not be found: lib/b2.ex, lib/b3.ex", fn ->
        assert_graph(~w[--sink lib/b2.ex --sink lib/b.ex --sink lib/b3.ex], "")
      end
    end

    test "sink and source" do
      assert_graph(~w[--source lib/a.ex --sink lib/b.ex], """
      lib/a.ex
      `-- lib/b.ex (compile)
          `-- lib/a.ex
      """)
    end

    test "with dynamic module" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        B.define()
        """)

        File.write!("lib/b.ex", """
        defmodule B do
          def define do
            defmodule A do
            end
          end
        end
        """)

        assert Mix.Task.run("xref", ["graph", "--format", "dot"]) == :ok

        assert File.read!("xref_graph.dot") === """
               digraph "xref graph" {
                 "lib/a.ex"
                 "lib/a.ex" -> "lib/b.ex" [label="(compile)"]
                 "lib/b.ex"
               }
               """
      end)
    end

    test "with export" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A do
          def fun do
            %B{}
          end
        end
        """)

        File.write!("lib/b.ex", """
        defmodule B do
          defstruct []
        end
        """)

        assert Mix.Task.run("xref", ["graph", "--format", "dot"]) == :ok

        assert File.read!("xref_graph.dot") === """
               digraph "xref graph" {
                 "lib/a.ex"
                 "lib/a.ex" -> "lib/b.ex" [label="(export)"]
                 "lib/b.ex"
               }
               """
      end)
    end

    test "with export to a custom file" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A do
          def fun, do: :ok
        end
        """)

        File.write!("lib/b.ex", """
        defmodule B do
          defstruct []
        end
        """)

        assert Mix.Task.run("xref", ["graph", "--format", "dot", "--output", "custom.dot"]) == :ok

        assert File.read!("custom.dot") === """
               digraph "xref graph" {
                 "lib/a.ex"
                 "lib/b.ex"
               }
               """
      end)
    end

    test "with export to stdout" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A do
          def fun, do: :ok
        end
        """)

        File.write!("lib/b.ex", """
        defmodule B do
          defstruct []
        end
        """)

        output =
          capture_io(fn ->
            assert Mix.Task.run("xref", ["graph", "--format", "dot", "--output", "-"]) == :ok
          end)

        assert output === """
               digraph "xref graph" {
                 "lib/a.ex"
                 "lib/b.ex"
               }
               """
      end)
    end

    test "with mixed cyclic dependencies" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A.Using do
          defmacro __using__(_), do: 42
        end

        defmodule A do
          B

          def foo do
            :foo
          end
        end
        """)

        File.write!("lib/b.ex", """
        defmodule B do
          # Let's also test that we track literal atom behaviours
          use :"Elixir.A.Using"

          def foo do
            A.foo()
          end
        end
        """)

        assert Mix.Task.run("xref", ["graph", "--format", "dot"]) == :ok

        assert File.read!("xref_graph.dot") === """
               digraph "xref graph" {
                 "lib/a.ex"
                 "lib/a.ex" -> "lib/b.ex" [label="(compile)"]
                 "lib/b.ex" -> "lib/a.ex" [label="(compile)"]
                 "lib/b.ex"
               }
               """
      end)
    end

    test "generates reports from the umbrella root" do
      Mix.Project.pop()

      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:umbrella, ".", fn _ ->
          File.write!("apps/bar/lib/bar.ex", """
          defmodule Bar do
            def bar do
              Foo.foo()
            end
          end
          """)

          Mix.Task.run("compile")
          Mix.shell().flush()

          Mix.Tasks.Xref.run(["graph", "--format", "stats", "--include-siblings"])

          assert receive_until_no_messages([]) == """
                 Tracked files: 2 (nodes)
                 Compile dependencies: 0 (edges)
                 Exports dependencies: 0 (edges)
                 Runtime dependencies: 1 (edges)
                 Cycles: 0

                 Top 2 files with most outgoing dependencies:
                   * lib/bar.ex (1)
                   * lib/foo.ex (0)

                 Top 2 files with most incoming dependencies:
                   * lib/foo.ex (1)
                   * lib/bar.ex (0)
                 """
        end)
      end)
    end

    test "generates reports considering siblings inside umbrellas" do
      Mix.Project.pop()

      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:bar, "apps/bar", fn _ ->
          File.write!("lib/bar.ex", """
          defmodule Bar do
            def bar do
              Foo.foo()
            end
          end
          """)

          Mix.Task.run("compile")
          Mix.shell().flush()

          Mix.Tasks.Xref.run(["graph", "--format", "stats", "--include-siblings"])

          assert receive_until_no_messages([]) == """
                 Tracked files: 2 (nodes)
                 Compile dependencies: 0 (edges)
                 Exports dependencies: 0 (edges)
                 Runtime dependencies: 1 (edges)
                 Cycles: 0

                 Top 2 files with most outgoing dependencies:
                   * lib/bar.ex (1)
                   * lib/foo.ex (0)

                 Top 2 files with most incoming dependencies:
                   * lib/foo.ex (1)
                   * lib/bar.ex (0)
                 """

          Mix.Tasks.Xref.run(["callers", "Foo"])

          assert receive_until_no_messages([]) == """
                 lib/bar.ex (runtime)
                 """
        end)
      end)
    end

    test "skip project compilation with --no-compile" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A do
          def a, do: :ok
        end
        """)

        Mix.Tasks.Xref.run(["graph", "--no-compile"])
        refute receive_until_no_messages([]) =~ "lib/a.ex"
      end)
    end

    test "group with multiple unconnected files" do
      assert_graph(~w[--group lib/a.ex,lib/c.ex,lib/e.ex], """
      lib/a.ex+
      |-- lib/b.ex (compile)
      `-- lib/d.ex (compile)
      lib/b.ex
      `-- lib/a.ex+ (compile)
      lib/d.ex
      `-- lib/a.ex+
      """)
    end

    test "group with directly dependent files and cycle" do
      assert_graph(["--group", "lib/a.ex,lib/b.ex,"], """
      lib/a.ex+
      |-- lib/c.ex
      `-- lib/e.ex (compile)
      lib/c.ex
      `-- lib/d.ex (compile)
      lib/d.ex
      `-- lib/e.ex
      lib/e.ex
      """)
    end

    test "multiple groups" do
      assert_graph(~w[--group lib/a.ex,lib/b.ex --group lib/c.ex,lib/e.ex], """
      lib/a.ex+
      `-- lib/c.ex+ (compile)
      lib/c.ex+
      `-- lib/d.ex (compile)
      lib/d.ex
      `-- lib/c.ex+
      """)
    end

    test "group with sink" do
      assert_graph(~w[--group lib/a.ex,lib/c.ex,lib/e.ex --sink lib/e.ex], """
      lib/b.ex
      `-- lib/a.ex+ (compile)
          |-- lib/b.ex (compile)
          `-- lib/d.ex (compile)
      lib/d.ex
      `-- lib/a.ex+
      """)
    end

    @default_files %{
      "lib/a.ex" => """
      defmodule A do
        def a, do: :ok
        B.b2()
      end
      """,
      "lib/b.ex" => """
      defmodule B do
        def b1, do: A.a() == C.c()
        def b2, do: :ok
        :e.e()
      end
      """,
      "lib/c.ex" => """
      defmodule C do
        def c, do: :ok
        :d.d()
      end
      """,
      "lib/d.ex" => """
      defmodule :d do
        def d, do: :ok
        def e, do: :e.e()
      end
      """,
      "lib/e.ex" => """
      defmodule :e do
        def e, do: :ok
      end
      """
    }

    defp assert_graph(opts \\ [], expected, params \\ []) do
      in_fixture("no_mixfile", fn ->
        nb_files =
          Enum.count(params[:files] || @default_files, fn {path, content} ->
            File.write!(path, content)
          end)

        assert Mix.Task.run("xref", opts ++ ["graph"]) == :ok
        first_line = "Compiling #{nb_files} files (.ex)"

        assert [
                 ^first_line | ["Generated sample app" | result]
               ] = receive_until_no_messages([]) |> String.split("\n")

        assert result |> Enum.join("\n") |> normalize_graph_output() == expected
      end)
    end

    defp normalize_graph_output(graph) do
      graph
      |> String.replace("├──", "|--")
      |> String.replace("└──", "`--")
      |> String.replace("│", "|")
    end
  end

  ## Helpers

  defp receive_until_no_messages(acc) do
    receive do
      {:mix_shell, :info, [line]} -> receive_until_no_messages([acc, line | "\n"])
    after
      0 -> IO.iodata_to_binary(acc)
    end
  end

  defp generate_files(files) do
    for {file, contents} <- files do
      File.write!(file, contents)
    end
  end
end
