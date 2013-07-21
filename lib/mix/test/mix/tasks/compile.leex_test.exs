Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.LeexTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  test "compilation continues if one file fails to compile" do
    in_fixture "compile_leex", fn ->
      File.write!("src/zzz.xrl", """)
      oops.
      """

      assert_raise CompileError, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Leex.run ["--force"]
        end
      end

      assert File.regular?("src/test_ok.erl")
    end
  end

  test "compiles src/test_ok.xrl" do
    in_fixture "compile_leex", fn ->
      assert Mix.Tasks.Compile.Leex.run([]) == :ok

      assert_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }
      assert File.regular?("src/test_ok.erl")

      assert Mix.Tasks.Compile.Leex.run([]) == :noop
      refute_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }

      assert Mix.Tasks.Compile.Leex.run(["--force"]) == :ok
      assert_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_leex", fn ->
      assert Mix.Tasks.Compile.Leex.run([]) == :ok
      assert File.regular?("src/test_ok.erl")

      File.rm!("src/test_ok.xrl")
      assert Mix.Tasks.Compile.Leex.run([]) == :ok
      refute File.regular?("src/test_ok.erl")
    end
  end
end
