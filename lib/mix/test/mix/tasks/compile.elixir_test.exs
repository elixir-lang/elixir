Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Compile.ElixirTest do
  import ExUnit.CaptureIO
  alias Mix.Task.Compiler.Diagnostic
  use MixTest.Case

  setup do
    Mix.Project.push(MixTest.Case.Sample)
    :ok
  end

  def trace(event, _e) do
    send(__MODULE__, event)
    :ok
  end

  @elixir_otp_version {System.version(), :erlang.system_info(:otp_release)}

  test "compiles a project without per environment build" do
    Mix.Project.pop()
    Mix.ProjectStack.post_config(build_per_environment: false)
    Mix.Project.push(MixTest.Case.Sample)

    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.B.beam")

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles a project with per environment build" do
    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.B.beam")

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles a project with custom tracer" do
    Process.register(self(), __MODULE__)

    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run(["--tracer", "Mix.Tasks.Compile.ElixirTest"])
      assert_received {:alias_reference, _meta, A}
      assert_received {:alias_reference, _meta, B}
    end)
  after
    Code.put_compiler_option(:tracers, [])
  end

  test "warns when Logger is used but not depended on" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
      require Logger
      def info, do: Logger.info("hello")
      end
      """)

      message =
        "Logger.info/1 defined in application :logger is used by the current application but the current application does not directly depend on :logger"

      assert capture_io(:stderr, fn ->
               Mix.Task.run("compile", [])
             end) =~ message

      Mix.Task.clear()

      assert capture_io(:stderr, fn ->
               assert catch_exit(Mix.Task.run("compile", ["--warnings-as-errors", "--force"]))
             end) =~ message
    end)
  end

  test "recompiles module-application manifest if manifest is outdated" do
    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run(["--force"])
      purge([A, B])

      mtime = File.stat!("_build/dev/lib/sample/.mix/compile.app_tracer").mtime
      ensure_touched("_build/dev/lib/sample/.mix/compile.lock", mtime)

      Mix.Tasks.Compile.Elixir.run(["--force"])
      assert File.stat!("_build/dev/lib/sample/.mix/compile.app_tracer").mtime > mtime
    end)
  end

  test "recompiles project if Elixir version changed" do
    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.run([])
      purge([A, B])

      assert File.exists?("_build/dev/lib/sample")
      assert File.exists?("_build/dev/lib/sample/consolidated")
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      Mix.Task.clear()
      File.write!("_build/dev/lib/sample/consolidated/.to_be_removed", "")
      manifest_data = :erlang.term_to_binary({:v1, "0.0.0", nil})
      File.write!("_build/dev/lib/sample/.mix/compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir_scm", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.run([])
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir_scm").mtime >
               {{2010, 1, 1}, {0, 0, 0}}

      refute File.exists?("_build/dev/lib/sample/consolidated/.to_be_removed")
    end)
  end

  test "recompiles project if scm changed" do
    in_fixture("no_mixfile", fn ->
      Mix.Tasks.Compile.run(["--verbose"])
      purge([A, B])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      Mix.Task.clear()
      manifest_data = :erlang.term_to_binary({1, @elixir_otp_version, :another})
      File.write!("_build/dev/lib/sample/.mix/compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.mix/compile.elixir_scm", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.run([])
      assert Mix.Dep.ElixirSCM.read() == {:ok, @elixir_otp_version, Mix.SCM.Path}

      assert File.stat!("_build/dev/lib/sample/.mix/compile.elixir_scm").mtime >
               {{2010, 1, 1}, {0, 0, 0}}
    end)
  end

  test "does not write BEAM files down on failures" do
    in_tmp("blank", fn ->
      File.mkdir_p!("lib")
      File.write!("lib/a.ex", "raise ~s(oops)")

      capture_io(fn ->
        assert {:error, [_]} = Mix.Tasks.Compile.Elixir.run([])
      end)

      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
    end)
  end

  test "removes, purges and deletes old artifacts" do
    in_fixture("no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert Code.ensure_loaded?(A)

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run([]) == {:ok, []}
      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      refute Code.ensure_loaded?(A)
      refute String.contains?(File.read!("_build/dev/lib/sample/.mix/compile.elixir"), "Elixir.A")
    end)
  end

  test "compiles mtime changed files" do
    in_fixture("no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush
      purge([A, B])

      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      message =
        "warning: mtime (modified time) for \"lib/a.ex\" was set to the future, resetting to now"

      assert_received {:mix_shell, :error, [^message]}

      message =
        "warning: mtime (modified time) for \"lib/b.ex\" was set to the future, resetting to now"

      refute_received {:mix_shell, :error, [^message]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      File.touch!("_build/dev/lib/sample/.mix/compile.elixir", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == {:noop, []}
    end)
  end

  test "compiles size changed files" do
    in_fixture("no_mixfile", fn ->
      past = {{2010, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", past)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush
      purge([A, B])

      File.write!("lib/a.ex", File.read!("lib/a.ex") <> "\n")
      File.touch!("lib/a.ex", past)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles dependent changed modules" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A, do: B.module_info()")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush
      purge([A, B])

      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/b.ex", future)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles dependent changed modules without beam files" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/b.ex", """
      defmodule B do
        def a, do: A.__info__(:module)
      end
      """)

      Mix.Tasks.Compile.Elixir.run(["--verbose"])
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.B.beam")

      Code.put_compiler_option(:ignore_module_conflict, true)
      Code.compile_file("lib/b.ex")
      File.touch!("lib/a.ex", {{2038, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.Elixir.run(["--verbose"])
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  after
    Code.put_compiler_option(:ignore_module_conflict, false)
  end

  test "compiles dependent changed modules even on removal" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A, do: B.module_info()")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush
      purge([A, B])

      File.rm("lib/b.ex")
      File.write!("lib/a.ex", "defmodule A, do: nil")
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "compiles dependent changed files" do
    in_fixture("no_mixfile", fn ->
      tmp = tmp_path("c.eex")
      File.touch!("lib/a.eex")

      File.write!("lib/a.ex", """
      defmodule A do
        @external_resource "lib/a.eex"
        @external_resource #{inspect(tmp)}
        def a, do: :ok
      end
      """)

      # Compiles with missing external resources
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      Mix.shell().flush
      purge([A, B])

      # Update local existing resource
      File.touch!("lib/a.eex", {{2038, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Does not update on old existing resource
      File.touch!("lib/a.eex", {{2000, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      Mix.shell().flush
      purge([A, B])

      # Update external existing resource
      File.touch!(tmp, {{2038, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  after
    File.rm(tmp_path("c.eex"))
  end

  test "recompiles modules with structs tracking" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        defstruct [:foo]
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def fun do
          %A{foo: 1}
        end
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        defstruct [:foo]
        def some_fun, do: :ok
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        defstruct [:foo, :bar]
        def some_fun, do: :ok
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      File.write!("lib/a.ex", """
      defmodule A do
        @enforce_keys [:foo]
        defstruct [:foo, :bar]
        def some_fun, do: :ok
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])
    end)
  end

  test "recompiles modules with async tracking" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      Kernel.ParallelCompiler.async(fn ->
        defmodule A do
          def fun, do: :ok
        end
      end) |> Task.await()
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        A.fun()
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])

      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      purge([A, B])
    end)
  end

  test "recompiles modules with multiple sources" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def one, do: 1
      end

      defmodule B do
        def two, do: 2
      end
      """)

      File.write!("lib/b.ex", """
      B.two()

      defmodule A do
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose", "--ignore-module-conflict"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute function_exported?(A, :one, 0)

      Mix.shell().flush
      purge([A])

      File.rm("lib/b.ex")
      Mix.Tasks.Compile.Elixir.run(["--verbose"])
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert function_exported?(A, :one, 0)
    end)
  end

  test "recompiles with --force" do
    in_fixture("no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      purge([A, B])

      # Now we have a noop
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}

      # --force
      assert Mix.Tasks.Compile.Elixir.run(["--force", "--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end)
  end

  test "compiles files with autoload disabled" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        @compile {:autoload, false}
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      purge([A, B])
    end)
  end

  test "does not recompile files that are empty or has no code" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", "")
      File.write!("lib/b.ex", "# Just a comment")
      File.write!("lib/c.ex", "\n\n")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:noop, []}
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/c.ex"]}
    end)
  end

  test "recompiles modules with __mix_recompile__ check" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def __mix_recompile__?(), do: true
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def __mix_recompile__?(), do: false
      end
      """)

      File.write!("lib/c.ex", """
      defmodule C do
        @compile {:autoload, false}

        def __mix_recompile__?(), do: true
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiling 3 files (.ex)"]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiling 1 file (.ex)"]}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      refute_received _
    end)
  end

  test "does not treat remote typespecs as compile time dependencies" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/b.ex", """
      defmodule B do
        @type t :: A.t
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == {:ok, []}
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell().flush
      purge([A, B])

      future = {{2038, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run(["--verbose"])

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end)
  end

  test "prints warnings from non-stale files with --all-warnings" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      # First compilation should print unused variable warning
      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run([]) == :ok
             end) =~ "variable \"unused\" is unused"

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
             end) =~ "variable \"unused\" is unused"

      # Should not print warning once fixed
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(_unused), do: :ok
      end
      """)

      assert capture_io(:stderr, fn ->
               Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
             end) == ""
    end)
  end

  test "returns warning diagnostics" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      diagnostic = %Diagnostic{
        file: Path.absname("lib/a.ex"),
        severity: :warning,
        position: 2,
        compiler_name: "Elixir",
        message:
          "variable \"unused\" is unused (if the variable is not meant to be used, prefix it with an underscore)"
      }

      capture_io(:stderr, fn ->
        assert {:ok, [^diagnostic]} = Mix.Tasks.Compile.Elixir.run([])
      end)

      # Recompiling should return :noop status because nothing is stale,
      # but also include previous warning diagnostics
      assert {:noop, [^diagnostic]} = Mix.Tasks.Compile.Elixir.run([])
    end)
  end

  test "returns warning diagnostics for external files" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      IO.warn "warning", [{nil, nil, 0, file: 'lib/foo.txt', line: 3}]
      """)

      diagnostic = %Diagnostic{
        file: Path.absname("lib/foo.txt"),
        severity: :warning,
        position: 3,
        compiler_name: "Elixir",
        message: "warning"
      }

      capture_io(:stderr, fn ->
        assert {:ok, [^diagnostic]} = Mix.Tasks.Compile.Elixir.run([])
      end)
    end)
  end

  test "returns warning diagnostics for unused imports" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        import B
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def foo, do: :ok
      end
      """)

      diagnostic = %Diagnostic{
        file: Path.absname("lib/a.ex"),
        severity: :warning,
        position: 2,
        compiler_name: "Elixir",
        message: "unused import B"
      }

      capture_io(:stderr, fn ->
        assert {:ok, [^diagnostic]} = Mix.Tasks.Compile.Elixir.run([])
      end)
    end)
  end

  test "returns error diagnostics", context do
    in_tmp(context.test, fn ->
      File.mkdir_p!("lib")

      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(), do: $$$
      end
      """)

      file = Path.absname("lib/a.ex")

      capture_io(fn ->
        assert {:error, [diagnostic]} = Mix.Tasks.Compile.Elixir.run([])

        assert %Diagnostic{
                 file: ^file,
                 severity: :error,
                 position: 2,
                 message: "** (SyntaxError) lib/a.ex:2:" <> _,
                 compiler_name: "Elixir"
               } = diagnostic
      end)
    end)
  end

  test "returns error diagnostics when deadlocked" do
    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        import B
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        import A
      end
      """)

      capture_io(fn ->
        assert {:error, errors} = Mix.Tasks.Compile.Elixir.run([])
        errors = Enum.sort_by(errors, &Map.get(&1, :file))

        file_a = Path.absname("lib/a.ex")
        file_b = Path.absname("lib/b.ex")

        assert [
                 %Diagnostic{file: ^file_a, message: "deadlocked waiting on module B"},
                 %Diagnostic{file: ^file_b, message: "deadlocked waiting on module A"}
               ] = errors
      end)
    end)
  end

  test "verify runtime dependent modules that haven't been compiled" do
    Mix.Project.pop()
    Mix.Project.push(MixTest.Case.Sample)

    in_fixture("no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def foo(), do: :ok
      end
      """)

      File.write!("lib/b.ex", """
      defmodule B do
        def foo(), do: A.foo()
      end
      """)

      File.write!("lib/c.ex", """
      defmodule C do
        def foo(), do: B.foo()
        def bar(), do: B.bar()
      end
      """)

      output =
        capture_io(:stderr, fn ->
          Mix.Tasks.Compile.Elixir.run(["--verbose"])
        end)

      refute output =~ "A.foo/0 is undefined or private"
      assert output =~ "B.bar/0 is undefined or private"

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/c.ex"]}

      File.write!("lib/a.ex", """
      defmodule A do
      end
      """)

      output =
        capture_io(:stderr, fn ->
          Mix.Tasks.Compile.Elixir.run(["--verbose"])
        end)

      # Check B due to direct dependency on A
      # Check C due to transient dependency on A
      assert output =~ "A.foo/0 is undefined or private"
      assert output =~ "B.bar/0 is undefined or private"

      # Ensure only A was recompiled
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/c.ex"]}
    end)
  end
end
