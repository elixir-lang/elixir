Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  defmodule CustomCompilers do
    def project do
      [compilers: [:elixir, :app, :custom]]
    end
  end

  defmodule Consolidation do
    def project do
      [app: :consolidation,
       version: "0.1.0",
       consolidate_protocols: true]
    end
  end

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "compile --list with mixfile" do
    Mix.Tasks.Compile.run ["--list"]
    assert_received {:mix_shell, :info, ["\nEnabled compilers: yecc, leex, erlang, elixir, app"]}
    assert_received {:mix_shell, :info, ["mix compile.elixir    # " <> _]}
  end

  test "compile --list with custom mixfile" do
    Mix.Project.push CustomCompilers
    Mix.Tasks.Compile.run ["--list"]
    assert_received {:mix_shell, :info, ["\nEnabled compilers: elixir, app, custom"]}
  end

  test "compile --list with consolidation" do
    Mix.Project.push Consolidation
    Mix.Tasks.Compile.run ["--list"]
    assert_received {:mix_shell, :info, ["\nEnabled compilers: yecc, leex, erlang, elixir, app, protocols"]}
  end

  test "compile a project with mixfile" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.run([]) == :ok
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/sample.app")
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Generated sample app"]}
    end
  end

  test "compile a project with protocol consolidation" do
    Mix.Project.push Consolidation
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.run([]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Consolidated Enumerable"]}
      assert File.regular? "_build/dev/consolidated/Elixir.Enumerable.beam"

      assert Mix.Tasks.Compile.run([]) == :noop
      refute_received {:mix_shell, :info, ["Consolidated Enumerable"]}

      assert Mix.Tasks.App.Start.run([]) == :ok
      assert Protocol.consolidated?(Enumerable)
    end
  after
    purge [Enumerable]
  end

  test "compile a project with multiple compilers and a syntax error in an erlang file" do
    in_fixture "no_mixfile", fn ->
      import ExUnit.CaptureIO

      File.mkdir! "src"
      File.write! "src/a.erl", """
      -module(b).
      def b(), do: b
      """
      assert File.regular?("src/a.erl")

      assert_raise Mix.Error, fn ->
        capture_io fn -> Mix.Tasks.Compile.run ["--force"] end
      end

      refute File.regular?("ebin/Elixir.A.beam")
      refute File.regular?("ebin/Elixir.B.beam")
      refute File.regular?("ebin/Elixir.C.beam")
    end
  end

  test "add logger application metadata" do
    import ExUnit.CaptureLog
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
      require Logger
      def info, do: Logger.info("hello")
      end
      """)

      assert Mix.Tasks.Compile.run([]) == :ok
      try do
        assert capture_log([metadata: [:application]], &A.info/0) =~ "application=sample"
      after
        purge [A]
      end
    end
  end
end
