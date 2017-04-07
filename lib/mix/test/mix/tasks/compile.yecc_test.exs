Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.YeccTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "compilation continues if one file fails to compile" do
    in_fixture "compile_yecc", fn ->
      File.write! "src/zzz.yrl", """
      oops.
      """

      assert_raise Mix.Error, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Yecc.run ["--force"]
        end
      end

      assert File.regular?("src/test_ok.erl")
    end
  end

  test "compiles src/test_ok.yrl" do
    in_fixture "compile_yecc", fn ->
      assert Mix.Tasks.Compile.Yecc.run(["--verbose"]) == :ok

      assert_received {:mix_shell, :info, ["Compiled src/test_ok.yrl"]}
      assert File.regular?("src/test_ok.erl")

      assert Mix.Tasks.Compile.Yecc.run(["--verbose"]) == :noop
      refute_received {:mix_shell, :info, ["Compiled src/test_ok.yrl"]}

      assert Mix.Tasks.Compile.Yecc.run(["--force", "--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled src/test_ok.yrl"]}
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_yecc", fn ->
      assert Mix.Tasks.Compile.Yecc.run([]) == :ok
      assert File.regular?("src/test_ok.erl")

      File.rm!("src/test_ok.yrl")
      assert Mix.Tasks.Compile.Yecc.run([]) == :ok
      refute File.regular?("src/test_ok.erl")
    end
  end
end
