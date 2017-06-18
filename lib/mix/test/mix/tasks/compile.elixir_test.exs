Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ElixirTest do
  use MixTest.Case

  setup do
    Mix.Project.push MixTest.Case.Sample
    :ok
  end

  @elixir_otp_version {System.version, :erlang.system_info(:otp_release)}

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

  test "recompiles project if Elixir version changed" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      purge [A, B]

      assert File.exists?("_build/dev/lib/sample")
      assert File.exists?("_build/dev/lib/sample/consolidated")
      assert Mix.Dep.ElixirSCM.read == {:ok, @elixir_otp_version, Mix.SCM.Path}

      Mix.Task.clear
      File.write!("_build/dev/lib/sample/consolidated/.to_be_removed", "")
      manifest_data = :erlang.term_to_binary({:v1, "0.0.0", nil})
      File.write!("_build/dev/lib/sample/.compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.compile.elixir_scm", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.run []
      assert Mix.Dep.ElixirSCM.read == {:ok, @elixir_otp_version, Mix.SCM.Path}
      assert File.stat!("_build/dev/lib/sample/.compile.elixir_scm").mtime > {{2010, 1, 1}, {0, 0, 0}}
      refute File.exists?("_build/dev/lib/sample/consolidated/.to_be_removed")
    end
  end

  test "recompiles project if scm changed" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run ["--verbose"]
      purge [A, B]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert Mix.Dep.ElixirSCM.read == {:ok, @elixir_otp_version, Mix.SCM.Path}

      Mix.Task.clear
      manifest_data = :erlang.term_to_binary({:v2, @elixir_otp_version, :another})
      File.write!("_build/dev/lib/sample/.compile.elixir_scm", manifest_data)
      File.touch!("_build/dev/lib/sample/.compile.elixir_scm", {{2010, 1, 1}, {0, 0, 0}})

      Mix.Tasks.Compile.run []
      assert Mix.Dep.ElixirSCM.read == {:ok, @elixir_otp_version, Mix.SCM.Path}
      assert File.stat!("_build/dev/lib/sample/.compile.elixir_scm").mtime > {{2010, 1, 1}, {0, 0, 0}}
    end
  end

  test "does not write BEAM files down on failures" do
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

  test "compiles mtime changed files" do
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

  test "compiles size changed files" do
    in_fixture "no_mixfile", fn ->
      past = {{2010, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", past)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell.flush
      purge [A, B]

      File.write!("lib/a.ex", File.read!("lib/a.ex") <> "\n")
      File.touch!("lib/a.ex", past)
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
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
      tmp = tmp_path("c.eex")
      File.touch!("lib/a.eex")

      File.write!("lib/a.ex", """
      defmodule A do
        @external_resource "lib/a.eex"
        @external_resource #{inspect tmp}
        def a, do: :ok
      end
      """)

      # Compiles with missing external resources
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :noop
      Mix.shell.flush
      purge [A, B]

      # Update local existing resource
      File.touch!("lib/a.eex", {{2020, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      # Does not update on old existing resource
      File.touch!("lib/a.eex", {{1970, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :noop
      Mix.shell.flush
      purge [A, B]

      # Update external existing resource
      File.touch!(tmp, {{2020, 1, 1}, {0, 0, 0}})
      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  after
    File.rm tmp_path("c.eex")
  end

  test "recompiles modules with multiple sources" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def one, do: 1
      end

      defmodule B do
        def two, do: 2
      end
      """)

      File.write!("lib/b.ex", """
      B.two()

      defmodule A do
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose", "--ignore-module-conflict"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      refute function_exported?(A, :one, 0)

      Mix.shell.flush
      purge [A]

      File.rm("lib/b.ex")
      Mix.Tasks.Compile.Elixir.run ["--verbose"]
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
      assert function_exported?(A, :one, 0)
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

  test "does not treat remote typespecs as compile time dependencies" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/b.ex", """
      defmodule B do
        @type t :: A.t
      end
      """)

      assert Mix.Tasks.Compile.Elixir.run(["--verbose"]) == :ok
      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      assert_received {:mix_shell, :info, ["Compiled lib/b.ex"]}

      Mix.shell.flush
      purge [A, B]

      future = {{2020, 1, 1}, {0, 0, 0}}
      File.touch!("lib/a.ex", future)
      Mix.Tasks.Compile.Elixir.run ["--verbose"]

      assert_received {:mix_shell, :info, ["Compiled lib/a.ex"]}
      refute_received {:mix_shell, :info, ["Compiled lib/b.ex"]}
    end
  end

  test "prints warnings from non-stale files with --all-warnings" do
    in_fixture "no_mixfile", fn ->
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(unused), do: :ok
      end
      """)

      # First compilation should print unused variable warning
      import ExUnit.CaptureIO
      output = capture_io(:standard_error, fn ->
        Mix.Tasks.Compile.Elixir.run([]) == :ok
      end)

      # Should also print warning
      assert capture_io(:standard_error, fn ->
        Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
      end) == output

      # Should not print warning once fixed
      File.write!("lib/a.ex", """
      defmodule A do
        def my_fn(_unused), do: :ok
      end
      """)
      assert capture_io(:standard_error, fn ->
        Mix.Tasks.Compile.Elixir.run(["--all-warnings"])
      end) == ""
    end
  end
end
