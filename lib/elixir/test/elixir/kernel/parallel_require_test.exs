Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.ParallelRequireTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  setup do
    {:ok, %{}}
  end

  test "returns required modules and warnings" do
    fixture = fixture_path("warnings_sample.ex")
    :code.purge(WarningsSample)
    :code.delete(WarningsSample)
    Code.unload_files([fixture])

    try do
      capture_io :stderr, fn ->
        assert {:ok, [WarningsSample],
                [{^fixture, 3,
                  "this clause cannot match because a previous clause at line 2 always matches"}]
              } = Kernel.ParallelRequire.files([fixture], return_errors: true)
      end
    after
      Code.unload_files([fixture])
      :code.purge(WarningsSample)
      :code.delete(WarningsSample)
    end
  end


  test "warnings as errors" do
    fixture = fixture_path("warnings_sample.ex")
    :code.purge(WarningsSample)
    :code.delete(WarningsSample)
    Code.unload_files([fixture])
    compiler_options = Code.compiler_options()

    try do
      Code.compiler_options(warnings_as_errors: true)

      msg = capture_io :stderr, fn ->
        assert {:error,
                [{^fixture, 3,
                  "this clause cannot match because a previous clause at line 2 always matches"}]
               } = Kernel.ParallelRequire.files([fixture], return_errors: true)
      end

      assert msg =~ "Failed due to warnings while using the --warnings-as-errors option\n"
    after
      Code.compiler_options(compiler_options)
      Code.unload_files([fixture])
      :code.purge(WarningsSample)
      :code.delete(WarningsSample)
    end
  end

  test "returns compilation errors" do
    fixture = fixture_path("parallel_struct/undef.ex")
    expected_msg = "Undef.__struct__/1 is undefined, cannot expand struct Undef"
    assert capture_io(fn ->
      assert {:error, [{^fixture, 3, msg}]} = Kernel.ParallelRequire.files([fixture], return_errors: true)
      assert msg =~ expected_msg
    end) =~ expected_msg
  end

  test "returns error on invalid file" do
    fixture = fixture_path("does_not_exist.ex")
    expected_msg = "** (Code.LoadError) could not load #{fixture}"
    assert capture_io(fn ->
      assert {:error, [{^fixture, nil, msg}]} = Kernel.ParallelRequire.files([fixture], return_errors: true)
      assert msg =~ expected_msg
    end) =~ expected_msg
  end
end
