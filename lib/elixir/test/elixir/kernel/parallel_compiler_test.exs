Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "compiles files solving dependencies" do
    fixtures = [fixture_path("parallel_compiler/bar.ex"), fixture_path("parallel_compiler/foo.ex")]
    assert capture_io(fn ->
      assert {:ok, [BarParallel, FooParallel], []} = Kernel.ParallelCompiler.files(fixtures, return_errors: true)
    end) =~ "message_from_foo"
  after
    Enum.map [FooParallel, BarParallel], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "compiles files with structs solving dependencies" do
    fixtures = [fixture_path("parallel_struct/bar.ex"), fixture_path("parallel_struct/foo.ex")]
    assert {:ok, modules, []} = Kernel.ParallelCompiler.files(fixtures, return_errors: true)
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
      assert {:error, [{^fixture, 3, msg}], []} = Kernel.ParallelCompiler.files([fixture], return_errors: true)
      assert msg =~ expected_msg
    end) =~ expected_msg
  end

  test "exits on error if :return_errors is false" do
    fixture = fixture_path("parallel_struct/undef.ex")
    capture_io(fn ->
      assert {:shutdown, 1} = catch_exit(Kernel.ParallelCompiler.files([fixture]))
    end)
  end

  test "does not hang on missing dependencies" do
    fixture = fixture_path("parallel_compiler/bat.ex")
    expected_msg = "ThisModuleWillNeverBeAvailable.__struct__/1 is undefined, cannot expand struct ThisModuleWillNeverBeAvailable"
    assert capture_io(fn ->
      assert {:error, [{^fixture, 7, msg}], []} = Kernel.ParallelCompiler.files([fixture], return_errors: true)
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
              []
             } = Kernel.ParallelCompiler.files(fixtures, return_errors: true)
    end)

    assert msg =~ "Compilation failed because of a deadlock between files."
    assert msg =~ "fixtures/parallel_deadlock/foo.ex => BarDeadlock"
    assert msg =~ "fixtures/parallel_deadlock/bar.ex => FooDeadlock"
    assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/foo\.ex =="
    assert msg =~ "** (CompileError)  deadlocked waiting on module BarDeadlock"
    assert msg =~ ~r"== Compilation error in file .+parallel_deadlock/bar\.ex =="
    assert msg =~ "** (CompileError)  deadlocked waiting on module FooDeadlock"
  end

  test "warnings as errors" do
    warnings_as_errors = Code.compiler_options[:warnings_as_errors]
    fixture = fixture_path("warnings_sample.ex")

    try do
      Code.compiler_options(warnings_as_errors: true)

      msg = capture_io :stderr, fn ->
        assert {:error,
                [{^fixture, 3,
                  "this clause cannot match because a previous clause at line 2 always matches"}],
                []
               } = Kernel.ParallelCompiler.files([fixture], return_errors: true)
      end

      assert msg =~ "Compilation failed due to warnings while using the --warnings-as-errors option\n"
    after
      Code.compiler_options(warnings_as_errors: warnings_as_errors)
    end
  end
end
