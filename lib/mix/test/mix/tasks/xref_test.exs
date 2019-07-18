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

    defp assert_all_calls(files, expected) do
      in_fixture("no_mixfile", fn ->
        generate_files(files)

        Mix.Task.run("compile")
        assert Enum.sort(Mix.Tasks.Xref.calls()) == Enum.sort(expected)
      end)
    end
  end

  describe "mix xref callers MODULE" do
    test "prints callers of specified Module" do
      files = %{
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

      output = """
      Compiling 2 files (.ex)
      Generated sample app
      lib/b.ex (runtime)
      """

      assert_callers("A", files, output)
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

    defp assert_callers(module, files, expected) do
      in_fixture("no_mixfile", fn ->
        for {file, contents} <- files do
          File.write!(file, contents)
        end

        capture_io(:stderr, fn ->
          assert Mix.Task.run("xref", ["callers", module]) == :ok
        end)

        assert ^expected = receive_until_no_messages([])
      end)
    end
  end

  describe "mix xref graph" do
    test " basic usage" do
      assert_graph("""
      lib/a.ex
      └── lib/b.ex
          └── lib/a.ex
      lib/b.ex
      lib/c.ex
      lib/d.ex
      └── lib/a.ex (compile)
      """)
    end

    test "stats" do
      assert_graph(["--format", "stats"], """
      Tracked files: 4 (nodes)
      Compile dependencies: 1 (edges)
      Structs dependencies: 0 (edges)
      Runtime dependencies: 2 (edges)

      Top 4 files with most outgoing dependencies:
        * lib/d.ex (1)
        * lib/b.ex (1)
        * lib/a.ex (1)
        * lib/c.ex (0)

      Top 2 files with most incoming dependencies:
        * lib/a.ex (2)
        * lib/b.ex (1)
      """)
    end

    test "exclude many" do
      assert_graph(~w[--exclude lib/c.ex --exclude lib/b.ex], """
      lib/a.ex
      lib/d.ex
      └── lib/a.ex (compile)
      """)
    end

    test "exclude one" do
      assert_graph(~w[--exclude lib/d.ex], """
      lib/a.ex
      └── lib/b.ex
          └── lib/a.ex
      lib/b.ex
      lib/c.ex
      """)
    end

    test "source" do
      assert_graph(~w[--source lib/a.ex], """
      lib/b.ex
      └── lib/a.ex
          └── lib/b.ex
      """)
    end

    test "only nodes" do
      assert_graph(~w[--only-nodes], """
      lib/a.ex
      lib/b.ex
      lib/c.ex
      lib/d.ex
      """)
    end

    test "filter by compile label" do
      assert_graph(~w[--label compile], """
      lib/a.ex
      lib/b.ex
      lib/c.ex
      lib/d.ex
      └── lib/a.ex (compile)
      """)
    end

    test "filter by runtime label" do
      assert_graph(~w[--label runtime], """
      lib/a.ex
      └── lib/b.ex
          └── lib/a.ex
      lib/b.ex
      lib/c.ex
      lib/d.ex
      """)
    end

    test "invalid source" do
      assert_raise Mix.Error, "Source could not be found: lib/a2.ex", fn ->
        assert_graph(~w[--source lib/a2.ex], "")
      end
    end

    test "sink" do
      assert_graph(~w[--sink lib/b.ex], """
      lib/a.ex
      └── lib/b.ex
          └── lib/a.ex
      lib/d.ex
      └── lib/a.ex (compile)
      """)
    end

    test "invalid sink" do
      assert_raise Mix.Error, "Sink could not be found: lib/b2.ex", fn ->
        assert_graph(~w[--sink lib/b2.ex], "")
      end
    end

    test "sink and source is error" do
      assert_raise Mix.Error, "mix xref graph expects only one of --source and --sink", fn ->
        assert_graph(~w[--source lib/a.ex --sink lib/b.ex], "")
      end
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

    test "with struct" do
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
                 "lib/a.ex" -> "lib/b.ex" [label="(struct)"]
                 "lib/b.ex"
               }
               """
      end)
    end

    test "with mixed cyclic dependencies" do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A.Behaviour do
          @callback foo :: :foo
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
          @behaviour :"Elixir.A.Behaviour"

          def foo do
            A.foo
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

    defp assert_graph(opts \\ [], expected) do
      in_fixture("no_mixfile", fn ->
        File.write!("lib/a.ex", """
        defmodule A do
          def a do
            B.a
          end

          def b, do: :ok
        end
        """)

        File.write!("lib/b.ex", """
        defmodule B do
          def a do
            A.a
            B.a
          end
        end
        """)

        File.write!("lib/c.ex", """
        defmodule C do
        end
        """)

        File.write!("lib/d.ex", """
        defmodule :d do
          A.b
        end
        """)

        assert Mix.Task.run("xref", opts ++ ["graph"]) == :ok

        assert "Compiling 4 files (.ex)\nGenerated sample app\n" <> result =
                 receive_until_no_messages([])

        assert normalize_graph_output(result) == normalize_graph_output(expected)
      end)
    end

    defp normalize_graph_output(graph) do
      String.replace(graph, "└──", "`--")
    end

    test "generates reports considering siblings inside umbrellas" do
      Mix.Project.pop()

      in_fixture("umbrella_dep/deps/umbrella", fn ->
        Mix.Project.in_project(:bar, "apps/bar", fn _ ->
          File.write!("lib/bar.ex", """
          defmodule Bar do
            def bar do
              Foo.foo
            end
          end
          """)

          Mix.Task.run("compile")
          Mix.shell().flush()

          Mix.Tasks.Xref.run(["graph", "--format", "stats", "--include-siblings"])

          assert receive_until_no_messages([]) == """
                 Tracked files: 2 (nodes)
                 Compile dependencies: 0 (edges)
                 Structs dependencies: 0 (edges)
                 Runtime dependencies: 1 (edges)

                 Top 2 files with most outgoing dependencies:
                   * lib/bar.ex (1)
                   * lib/foo.ex (0)

                 Top 1 files with most incoming dependencies:
                   * lib/foo.ex (1)
                 """

          Mix.Tasks.Xref.run(["callers", "Foo"])

          assert receive_until_no_messages([]) == """
                 lib/bar.ex (runtime)
                 """
        end)
      end)
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
