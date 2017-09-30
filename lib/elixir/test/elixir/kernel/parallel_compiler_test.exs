Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  describe "compile" do
    test "solves dependencies between modules" do
      fixtures = [fixture_path("parallel_compiler/bar.ex"), fixture_path("parallel_compiler/foo.ex")]
      assert capture_io(fn ->
        assert {:ok, [BarParallel, FooParallel], []} = Kernel.ParallelCompiler.compile(fixtures)
      end) =~ "message_from_foo"
    after
      Enum.map [FooParallel, BarParallel], fn mod ->
        :code.purge(mod)
        :code.delete(mod)
      end
    end

    test "solves dependencies between structs" do
      fixtures = [fixture_path("parallel_struct/bar.ex"), fixture_path("parallel_struct/foo.ex")]
      assert {:ok, modules, []} = Kernel.ParallelCompiler.compile(fixtures)
      assert [BarStruct, FooStruct] = Enum.sort(modules)
    after
      Enum.map [FooStruct, BarStruct], fn mod ->
        :code.purge(mod)
        :code.delete(mod)
      end
    end

    test "returns struct undefined error when local struct is undefined" do
      fixture = fixture_path("parallel_struct/undef.ex")
      expected_msg = "Undef.__struct__/1 is undefined, cannot expand struct Undef"
      assert capture_io(fn ->
        assert {:error, [{^fixture, 3, msg}], []} = Kernel.ParallelCompiler.compile([fixture])
        assert msg =~ expected_msg
      end) =~ expected_msg
    end

    test "does not hang on missing dependencies" do
      fixture = fixture_path("parallel_compiler/bat.ex")
      expected_msg = "ThisModuleWillNeverBeAvailable.__struct__/1 is undefined, cannot expand struct ThisModuleWillNeverBeAvailable"
      assert capture_io(fn ->
        assert {:error, [{^fixture, 7, msg}], []} = Kernel.ParallelCompiler.compile([fixture])
        assert msg =~ expected_msg
      end) =~ "== Compilation error"
    end

    test "handles possible deadlocks" do
      foo = fixture_path("parallel_deadlock/foo.ex")
      bar = fixture_path("parallel_deadlock/bar.ex")
      fixtures = [foo, bar]

      msg = capture_io(fn ->
        assert {:error,
                [{^bar, nil, "deadlocked waiting on module FooDeadlock"},
                 {^foo, nil, "deadlocked waiting on module BarDeadlock"}],
                []} = Kernel.ParallelCompiler.compile(fixtures)
      end)

      assert msg =~ "Compilation failed because of a deadlock between files."
      assert msg =~ "fixtures/parallel_deadlock/foo.ex => BarDeadlock"
      assert msg =~ "fixtures/parallel_deadlock/bar.ex => FooDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/foo\.ex =="
      assert msg =~ "** (CompileError)  deadlocked waiting on module BarDeadlock"
      assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/bar\.ex =="
      assert msg =~ "** (CompileError)  deadlocked waiting on module FooDeadlock"
    end

    test "supports warnings as errors" do
      warnings_as_errors = Code.compiler_options[:warnings_as_errors]
      fixture = fixture_path("warnings_sample.ex")

      try do
        Code.compiler_options(warnings_as_errors: true)

        msg = capture_io :stderr, fn ->
          assert {:error,
                  [{^fixture, 3,
                    "this clause cannot match because a previous clause at line 2 always matches"}],
                  []} = Kernel.ParallelCompiler.compile([fixture])
        end

        assert msg =~ "Compilation failed due to warnings while using the --warnings-as-errors option\n"
      after
        Code.compiler_options(warnings_as_errors: warnings_as_errors)
        :code.purge(WarningsSample)
        :code.delete(WarningsSample)
      end
    end

    test "does not use incorrect line number when error originates in another file" do
      file_a = tmp_path("error_line_a.ex")
      File.write!(file_a, """
      defmodule A do
        def fun(arg), do: arg / 2
      end
      """)

      file_b = tmp_path("error_line_b.ex")
      File.write!(file_b, """
      defmodule B do
        def fun(arg) do
          A.fun(arg)
          :ok
        end
      end
      B.fun(:not_a_number)
      """)

      capture_io(fn ->
        assert {:error, [{^file_b, nil, _}], _} =
          Kernel.ParallelCompiler.compile([file_a, file_b])
      end)
    end

    test "gets correct line number for UndefinedFunctionError" do
      file = tmp_path("undef_error.ex")
      File.write!(file, """
      defmodule UndefErrorLine do
        Bogus.fun()
      end
      """)

      capture_io(fn ->
        assert {:error, [{^file, 2, _}], _} = Kernel.ParallelCompiler.compile([file])
      end)
    end
  end

  describe "require" do
    test "returns struct undefined error when local struct is undefined" do
      fixture = fixture_path("parallel_struct/undef.ex")
      expected_msg = "Undef.__struct__/1 is undefined, cannot expand struct Undef"
      assert capture_io(fn ->
        assert {:error, [{^fixture, 3, msg}], []} = Kernel.ParallelCompiler.require([fixture])
        assert msg =~ expected_msg
      end) =~ expected_msg
    end

    test "does not hang on missing dependencies" do
      fixture = fixture_path("parallel_compiler/bat.ex")
      expected_msg = "ThisModuleWillNeverBeAvailable.__struct__/1 is undefined, cannot expand struct ThisModuleWillNeverBeAvailable"
      assert capture_io(fn ->
        assert {:error, [{^fixture, 7, msg}], []} = Kernel.ParallelCompiler.require([fixture])
        assert msg =~ expected_msg
      end) =~ "== Compilation error"
    end

    test "supports warnings as errors" do
      warnings_as_errors = Code.compiler_options[:warnings_as_errors]
      fixture = fixture_path("warnings_sample.ex")

      try do
        Code.compiler_options(warnings_as_errors: true)

        msg = capture_io :stderr, fn ->
          assert {:error,
                  [{^fixture, 3,
                    "this clause cannot match because a previous clause at line 2 always matches"}],
                  []} = Kernel.ParallelCompiler.require([fixture])
        end

        assert msg =~ "Compilation failed due to warnings while using the --warnings-as-errors option\n"
      after
        Code.compiler_options(warnings_as_errors: warnings_as_errors)
        :code.purge(WarningsSample)
        :code.delete(WarningsSample)
      end
    end
  end
end
