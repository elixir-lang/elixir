Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ElixirTest do
  use MixTest.Case

  test "compile a project without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run []

      assert File.regular?("ebin/Elixir.A.beam")
      assert File.regular?("ebin/Elixir.B.beam")
      assert File.regular?("ebin/Elixir.C.beam")

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/c.ex"] }
    end
  end

  test "removes old artifact files" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert File.regular?("ebin/Elixir.A.beam")

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      refute File.regular?("ebin/Elixir.A.beam")
    end
  end

  defmodule SourcePathsProject do
    def project do
      [elixirc_paths: ["unknown"]]
    end
  end

  defmodule CompilePathProject do
    def project do
      [compile_path: "custom"]
    end
  end

  test "use custom source paths" do
    Mix.Project.push SourcePathsProject

    in_fixture "no_mixfile", fn ->
      # Nothing to compile with the custom source paths
      assert Mix.Tasks.Compile.Elixir.run([])
      refute_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  after
    Mix.Project.pop
  end

  test "use custom compile path" do
    Mix.Project.push CompilePathProject

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run([])
      assert File.regular?("custom/Elixir.A.beam")
    end
  after
    Mix.Project.pop
  end

  test "compiles only changed files" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      Mix.shell.flush
      purge [A, B, C]

      future = { { 2020, 1, 1 }, { 0, 0, 0 } }
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run []

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      refute_received { :mix_shell, :info, ["Compiled lib/b.ex"] }

      File.touch!("ebin/.compile.elixir", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == :noop
    end
  end

  test "compiles all when other watched exts change" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      Mix.shell.flush
      purge [A, B, C]

      future = { { 2020, 1, 1 }, { 0, 0, 0 } }
      File.touch!("lib/a.eex", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
    end
  end

  test "recompiles with --force" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      purge [A, B, C]

      # Now we have a noop
      assert Mix.Tasks.Compile.Elixir.run([]) == :noop

      # --force
      assert Mix.Tasks.Compile.Elixir.run(["--force"]) == :ok
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  end
end
