Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ErlangTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  test "tries to compile src/a.erl" do
    in_fixture "compile_erlang", fn ->
      File.write!("src/a.erl", """)
      -module(b).
      def b(), do: b
      """

      assert_raise CompileError, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Erlang.run []
        end
      end
    end
  end

  test "compiles src/b.erl and src/c.erl" do
    in_fixture "compile_erlang", fn ->
      Mix.Tasks.Compile.Erlang.run []
      assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }

      assert File.regular?("ebin/b.beam")
      assert File.regular?("ebin/c.beam")

      Mix.Tasks.Compile.Erlang.run []
      refute_received { :mix_shell, :info, ["Compiled src/b.erl"] }

      Mix.Tasks.Compile.Erlang.run ["--force"]
      assert_received { :mix_shell, :info, ["Compiled src/b.erl"] }
      assert_received { :mix_shell, :info, ["Compiled src/c.erl"] }
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok
      assert File.regular?("ebin/b.beam")

      # Now we have a noop
      File.rm!("src/b.erl")
      assert Mix.Tasks.Compile.Erlang.run([]) == :noop

      # --force
      assert Mix.Tasks.Compile.Erlang.run(["--force"]) == :ok
      refute File.regular?("ebin/b.beam")
    end
  end
end
