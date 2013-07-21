Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ErlangTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  test "compilation continues if one file fails to compile" do
    in_fixture "compile_erlang", fn ->
      File.write!("src/zzz.erl", """)
      -module(zzz).
      def zzz(), do: b
      """

      assert_raise CompileError, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Erlang.run []
        end
      end

      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")
    end
  end

  test "compiles src/b.erl and src/c.erl" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok
      assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }

      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")

      assert Mix.Tasks.Compile.Erlang.run([]) == :noop
      refute_received { :mix_shell, :info, ["Compiled src/b.erl"] }

      assert Mix.Tasks.Compile.Erlang.run(["--force"]) == :ok
      assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok
      assert File.regular?("ebin/b.beam")

      File.rm!("src/b.erl")
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok
      refute File.regular?("ebin/b.beam")
    end
  end
end
