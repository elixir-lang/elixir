Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.ParallelCompilerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "compiles files solving dependencies" do
    fixtures = [fixture_path("parallel_compiler/bar.ex"), fixture_path("parallel_compiler/foo.ex")]
    assert capture_io(fn ->
      assert {:ok, [BarParallel, FooParallel]} = Kernel.ParallelCompiler.files fixtures
    end) =~ "message_from_foo"
  after
    Enum.map [FooParallel, BarParallel], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "compiles files with structs solving dependencies" do
    fixtures = [fixture_path("parallel_struct/bar.ex"), fixture_path("parallel_struct/foo.ex")]
    assert {:ok, modules} = Kernel.ParallelCompiler.files(fixtures)
    assert [BarStruct, FooStruct] = Enum.sort(modules)
  after
    Enum.map [FooStruct, BarStruct], fn mod ->
      :code.purge(mod)
      :code.delete(mod)
    end
  end

  test "emits struct undefined error when local struct is undefined" do
    fixtures = [fixture_path("parallel_struct/undef.ex")]
    assert capture_io(fn ->
      assert {:error, _} = Kernel.ParallelCompiler.files(fixtures)
    end) =~ "Undef.__struct__/1 is undefined, cannot expand struct Undef"
  end

  test "does not hang on missing dependencies" do
    fixtures = [fixture_path("parallel_compiler/bat.ex")]
    assert capture_io(fn ->
      assert {:error, _} = Kernel.ParallelCompiler.files(fixtures)
    end) =~ "== Compilation error"
  end

  test "handles possible deadlocks" do
    fixtures = [fixture_path("parallel_deadlock/foo.ex"),
                fixture_path("parallel_deadlock/bar.ex")]

    msg = capture_io(fn ->
      assert {:error, _} = Kernel.ParallelCompiler.files(fixtures)
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
    fixtures = [fixture_path("warnings_sample.ex")]

    try do
      Code.compiler_options(warnings_as_errors: true)

      msg = capture_io :stderr, fn ->
        assert catch_exit(Kernel.ParallelCompiler.files fixtures) == {:shutdown, 1}
      end

      assert msg =~ "Compilation failed due to warnings while using the --warnings-as-errors option\n"
    after
      Code.compiler_options(warnings_as_errors: warnings_as_errors)
    end
  end
end
