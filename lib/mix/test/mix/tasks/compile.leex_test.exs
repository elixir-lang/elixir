Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.LeexTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  test "tries to compile src/test_fail.xrl" do
    in_fixture "compile_leex", fn ->
      File.write!("src/test_fail.xrl", """)
      oops.
      """

      output = capture_io fn ->
        Mix.Tasks.Compile.Leex.run []
      end

      assert output =~ "src/test_fail.xrl:1: missing Definitions"
    end
  end

  test "compiles src/test_ok.xrl" do
    in_fixture "compile_leex", fn ->
      Mix.Tasks.Compile.Leex.run []

      assert_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }
      assert File.regular?("src/test_ok.erl")

      Mix.Tasks.Compile.Leex.run []
      refute_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }

      Mix.Tasks.Compile.Leex.run ["--force"]
      assert_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }
    end
  end
end
