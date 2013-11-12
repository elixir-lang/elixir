Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ElixirTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  teardown do
    Mix.Project.pop
    :ok
  end

  test "compiles a project" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run []

      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.B.beam")
      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.C.beam")

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/c.ex"] }
    end
  end

  test "compiles a project with per environment build" do
    Mix.Project.pop
    Mix.ProjectStack.post_config [build_per_environment: true]
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run []

      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.B.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.C.beam")

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/c.ex"] }
    end
  end

  test "does not write beam down on failures" do
    import ExUnit.CaptureIO

    in_fixture "only_mixfile", fn ->
      File.mkdir_p!("lib")
      File.write!("lib/a.ex", "raise %s(oops)")

      capture_io fn ->
        assert_raise RuntimeError, fn ->
          Mix.Tasks.Compile.Elixir.run []
        end
      end

      refute File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")
    end
  end

  test "removes old artifact files" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      refute File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")
    end
  end

  test "compiles only changed files" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }

      Mix.shell.flush
      purge [A, B, C]

      future = { { 2020, 1, 1 }, { 0, 0, 0 } }
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run []

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      refute_received { :mix_shell, :info, ["Compiled lib/b.ex"] }

      File.touch!("_build/shared/lib/sample/.compile.elixir", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == :noop
    end
  end

  test "compiles dependent changed files" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A, do: B.module_info")

      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }

      Mix.shell.flush
      purge [A, B, C]

      future = { { 2020, 1, 1 }, { 0, 0, 0 } }
      File.touch!("lib/b.ex", future)
      Mix.Tasks.Compile.Elixir.run []

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
    end
  end

  test "compiles dependent changed files even on removal" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A, do: B.module_info")

      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      assert_received { :mix_shell, :info, ["Compiled lib/b.ex"] }

      Mix.shell.flush
      purge [A, B, C]

      File.rm("lib/b.ex")
      File.write!("lib/a.ex", "defmodule A, do: nil")
      Mix.Tasks.Compile.Elixir.run []

      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
      refute_received { :mix_shell, :info, ["Compiled lib/b.ex"] }
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

  defmodule SourcePathsProject do
    def project do
      [ app: :source_paths, elixirc_paths: ["unknown"]]
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
end
