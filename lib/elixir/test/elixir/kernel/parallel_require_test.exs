Code.require_file "../test_helper.exs", __DIR__

import PathHelpers

defmodule Kernel.ParallelRequireTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "warnings as errors" do
    warnings_as_errors = Code.compiler_options[:warnings_as_errors]
    fixtures = [fixture_path("warnings_sample.ex")]

    try do
      Code.compiler_options(warnings_as_errors: true)

      msg = capture_io :stderr, fn ->
        assert catch_exit(Kernel.ParallelRequire.files fixtures) == {:shutdown, 1}
      end

      assert msg =~ "Execution failed due to warnings while using the --warnings-as-errors option\n"
    after
      Code.compiler_options(warnings_as_errors: warnings_as_errors)
      :code.purge(WarningsSample)
      :code.delete(WarningsSample)
    end
  end
end
