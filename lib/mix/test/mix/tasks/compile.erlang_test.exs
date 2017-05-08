Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ErlangTest do
  use MixTest.Case
  import ExUnit.CaptureIO

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "compilation continues if one file fails to compile" do
    in_fixture "compile_erlang", fn ->
      File.write! "src/zzz.erl", """
      -module(zzz).
      def zzz(), do: b
      """

      assert_raise Mix.Error, fn ->
        capture_io fn ->
          Mix.Tasks.Compile.Erlang.run []
        end
      end

      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/c.beam")
    end
  end

  test "compiles src/b.erl and src/c.erl" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled src/b.erl"]}
      assert_received {:mix_shell, :info, ["Compiled src/c.erl"]}

      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/c.beam")

      assert File.read!("_build/dev/lib/sample/.compile.erlang") ==
             "_build/dev/lib/sample/ebin/b.beam\n" <>
             "_build/dev/lib/sample/ebin/c.beam"

      assert Mix.Tasks.Compile.Erlang.run(["--verbose"]) == :noop
      refute_received {:mix_shell, :info, ["Compiled src/b.erl"]}

      assert Mix.Tasks.Compile.Erlang.run(["--force", "--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled src/b.erl"]}
      assert_received {:mix_shell, :info, ["Compiled src/c.erl"]}
    end
  end

  test "removes old artifact files" do
    in_fixture "compile_erlang", fn ->
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok
      assert File.regular?("_build/dev/lib/sample/ebin/b.beam")

      File.rm!("src/b.erl")
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok
      refute File.regular?("_build/dev/lib/sample/ebin/b.beam")
    end
  end

  test "compilation purges the module" do
    in_fixture "compile_erlang", fn ->
      # Create the first version of the module.
      defmodule :purge_test do
        def version, do: :v1
      end
      assert :v1 == :purge_test.version

      # Create the second version of the module (this time as Erlang source).
      File.write! "src/purge_test.erl", """
      -module(purge_test).
      -export([version/0]).
      version() -> v2.
      """
      assert Mix.Tasks.Compile.Erlang.run([]) == :ok

      # If the module was not purged on recompilation, this would fail.
      assert :v2 == :purge_test.version
    end
  end
end
