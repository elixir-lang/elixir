Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.LeexTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  test "tries to compile src/test_fail.xrl" do
    in_fixture "compile_leex", fn ->
      File.write!("src/test_fail.xrl", """)
      oops.
      """

      assert_raise CompileError, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Leex.run []
        end
      end
    end
  end

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
      Mix.Tasks.Compile.Leex.run []

      assert_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }
      assert File.regular?("src/test_ok.erl")

      Mix.Tasks.Compile.Leex.run []
      refute_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }

      Mix.Tasks.Compile.Leex.run ["--force"]
      assert_received { :mix_shell, :info, ["Compiled src/test_ok.xrl"] }
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_leex", fn ->
      assert Mix.Tasks.Compile.Leex.run([]) == :ok
      assert File.regular?("src/test_ok.erl")

      # Now we have a noop
      File.rm!("src/test_ok.xrl")
      assert Mix.Tasks.Compile.Leex.run([]) == :noop

      # --force
      assert Mix.Tasks.Compile.Leex.run(["--force"]) == :ok
      refute File.regular?("src/test_ok.erl")
    end
  end
end
