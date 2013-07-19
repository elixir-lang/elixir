Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.YeccTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  test "tries to compile src/test_fail.yrl" do
    in_fixture "compile_yecc", fn ->
      File.write!("src/test_fail.yrl", """)
      oops.
      """

      assert_raise CompileError, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Yecc.run []
        end
      end
    end
  end

  test "compilation continues if one file fails to compile" do
    in_fixture "compile_yecc", fn ->
      File.write!("src/zzz.yrl", """)
      oops.
      """
      assert_raise CompileError, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Yecc.run ["--force"]
        end
      end

      assert File.regular?("src/test_ok.erl")
    end
  end

  test "compiles src/test_ok.yrl" do
    in_fixture "compile_yecc", fn ->
      Mix.Tasks.Compile.Yecc.run []

      assert_received { :mix_shell, :info, ["Compiled src/test_ok.yrl"] }
      assert File.regular?("src/test_ok.erl")

      Mix.Tasks.Compile.Yecc.run []
      refute_received { :mix_shell, :info, ["Compiled src/test_ok.yrl"] }

      Mix.Tasks.Compile.Yecc.run ["--force"]
      assert_received { :mix_shell, :info, ["Compiled src/test_ok.yrl"] }
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_yecc", fn ->
      assert Mix.Tasks.Compile.Yecc.run([]) == :ok
      assert File.regular?("src/test_ok.erl")

      # Now we have a noop
      File.rm!("src/test_ok.yrl")
      assert Mix.Tasks.Compile.Yecc.run([]) == :noop

      # --force
      assert Mix.Tasks.Compile.Yecc.run(["--force"]) == :ok
      refute File.regular?("src/test_ok.erl")
    end
  end
end
