Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ElixirTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  test "compiles a project without per environment build" do
    Mix.Project.pop
    Mix.ProjectStack.post_config [build_per_environment: false]
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/shared/lib/sample/ebin/Elixir.B.beam")

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  end

  test "compiles a project with per environment build" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.B.beam")

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  end

  test "recompiles project if elixir version changed" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      purge [A, B]

      assert File.exists?("_build/dev/lib/sample")
      assert File.exists?("_build/dev/consolidated")
      assert Mix.Dep.ElixirSCM.read == {:ok, System.version, Mix.SCM.Path}

      Mix.Task.clear
      File.write!("_build/dev/consolidated/.to_be_removed", "")
      manifest_data = :erlang.term_to_binary({:v1, "0.0.0", nil})
      File.write!("_build/dev/lib/sample/.compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.compile.elixir_scm", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.run []
      assert Mix.Dep.ElixirSCM.read == {:ok, System.version, Mix.SCM.Path}
      assert File.stat!("_build/dev/lib/sample/.compile.elixir_scm").mtime > {{2010, 1, 1}, {0, 0, 0}}
      refute File.exists?("_build/dev/consolidated/.to_be_removed")
    end
  end

  test "recompiles project if scm changed" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run ["--verbose"]
      purge [A, B]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert Mix.Dep.ElixirSCM.read == {:ok, System.version, Mix.SCM.Path}

      Mix.Task.clear
      manifest_data = :erlang.term_to_binary({:v1, "#{System.version}", :another})
      File.write!("_build/dev/lib/sample/.compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.compile.elixir_scm", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.run []
      assert Mix.Dep.ElixirSCM.read == {:ok, System.version, Mix.SCM.Path}
      assert File.stat!("_build/dev/lib/sample/.compile.elixir_scm").mtime > {{2010, 1, 1}, {0, 0, 0}}
    end
  end

  test "does not write beam down on failures" do
    import ExUnit.CaptureIO

    in_tmp "blank", fn ->
      File.mkdir_p!("lib")
      File.write!("lib/a.ex", "raise ~s(oops)")

      capture_io fn ->
        assert catch_exit(Mix.Tasks.Compile.Elixir.run []) == {:shutdown, 1}
      end

      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
    end
  end

  test "removes, purges and deletes old artifacts" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      assert File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      assert Code.ensure_loaded?(A)

      File.rm!("lib/a.ex")
      assert Mix.Tasks.Compile.Elixir.run([]) == :ok
      refute File.regular?("_build/dev/lib/sample/ebin/Elixir.A.beam")
      refute Code.ensure_loaded?(A)
      refute String.contains?(File.read!("_build/dev/lib/sample/.compile.elixir"), "Elixir.A")
    end
  end

  test "compiles only changed files" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell.flush
      purge [A, B]

      future = {{2020, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert_received {:mix_shell, :error, ["warning: mtime (modified time) for \"lib/a.ex\" was set to the future, resetting to now"]}
      refute_received {:mix_shell, :error, ["warning: mtime (modified time) for \"lib/b.ex\" was set to the future, resetting to now"]}

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      File.touch!("_build/dev/lib/sample/.compile.elixir", future)
      assert Mix.Tasks.Compile.Elixir.run([]) == :noop
    end
  end

  test "compiles dependent changed modules" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A, do: B.module_info")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell.flush
      purge [A, B]

      future = {{2020, 1, 1}, {0, 0, 0}}
      File.touch!("lib/b.ex", future)
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  end

  test "compiles dependent changed modules even on removal" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", "defmodule A, do: B.module_info")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell.flush
      purge [A, B]

      File.rm("lib/b.ex")
      File.write!("lib/a.ex", "defmodule A, do: nil")
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  end

  test "compiles dependent changed files" do
    in_fixture "no_mixfile", fn ->
      File.touch!("lib/a.eex")
      File.write!("lib/a.ex", """
      defmodule A do
        @external_resource "lib/b.eex"
        @external_resource "lib/a.eex"
        def a, do: :ok
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      Mix.shell.flush
      purge [A, B]

      File.touch!("lib/a.eex", {{2020, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  end

  test "does not recompile empty files" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", "")

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :noop
      refute_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end
  end

  test "compiles files with autoload disabled" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        @compile {:autoload, false}
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      purge [A, B]
    end
  end

  test "recompiles with --force" do
    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      purge [A, B]

      # Now we have a noop
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :noop

      # --force
      assert Mix.Tasks.Compile.Elixir.run(["--force", "--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
    end
  end
end
