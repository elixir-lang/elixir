Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.ProjectTest do
  use MixTest.Case

  defmodule SampleProject do
    def project do
      [app: :sample, hello: "world"]
    end
  end

  test "consolidation_path/1" do
    config = [apps_path: "apps", build_per_environment: true]

    assert Mix.Project.consolidation_path(config) ==
             Path.join(File.cwd!(), "_build/dev/consolidated")

    config = [app: :sample, build_per_environment: true]

    assert Mix.Project.consolidation_path(config) ==
             Path.join(File.cwd!(), "_build/dev/lib/sample/consolidated")
  end

  describe "build_path/1" do
    test "considers the environment" do
      assert Mix.Project.build_path(build_per_environment: true) ==
               Path.join(File.cwd!(), "_build/dev")

      assert Mix.Project.build_path(build_per_environment: false) ==
               Path.join(File.cwd!(), "_build/shared")
    end

    test "considers the target" do
      Mix.target(:rpi3)

      assert Mix.Project.build_path(build_per_environment: true) ==
               Path.join(File.cwd!(), "_build/rpi3_dev")

      assert Mix.Project.build_path(build_per_environment: false) ==
               Path.join(File.cwd!(), "_build/rpi3_shared")
    end

    test "considers MIX_BUILD_PATH" do
      System.put_env("MIX_BUILD_PATH", "_build")
      assert Mix.Project.build_path() == "_build"
    after
      System.delete_env("MIX_BUILD_PATH")
    end

    test "considers MIX_BUILD_ROOT" do
      System.put_env("MIX_BUILD_ROOT", "_build_root")
      assert Mix.Project.build_path() == Path.join(File.cwd!(), "_build_root/dev")
    after
      System.delete_env("MIX_BUILD_ROOT")
    end

    test "considers MIX_DEPS_PATH" do
      System.put_env("MIX_DEPS_PATH", "test_deps_path")
      assert Mix.Project.deps_path() == Path.join(File.cwd!(), "test_deps_path")
    after
      System.delete_env("MIX_DEPS_PATH")
    end
  end

  test "returns mix.exs path" do
    assert Mix.Project.project_file() == nil
    Mix.Project.push(SampleProject, "sample")
    assert Mix.Project.project_file() == "sample"
  end

  test "push and pop projects" do
    refute Mix.Project.get()
    Mix.Project.push(SampleProject, "sample")
    assert Mix.Project.get() == SampleProject

    assert %{name: SampleProject, config: _, file: "sample"} = Mix.Project.pop()
    assert Mix.Project.pop() == nil
  end

  test "does not allow the same project to be pushed twice" do
    Mix.Project.push(SampleProject, "sample")

    assert_raise Mix.Error, ~r/Mix.ProjectTest.SampleProject from "another"/, fn ->
      Mix.Project.push(SampleProject, "another")
    end
  end

  test "allows nil projects to be pushed twice" do
    Mix.Project.push(nil)
    Mix.Project.push(nil)
    assert is_map(Mix.Project.pop())
    assert is_map(Mix.Project.pop())
    assert is_nil(Mix.Project.pop())
  end

  test "retrieves configuration from projects" do
    Mix.Project.push(SampleProject)
    assert Mix.Project.config()[:hello] == "world"
  end

  test "removes private configuration" do
    Mix.Project.push(SampleProject)
    assert is_nil(Mix.Project.config()[:deps_app_path])
  end

  test "raises an error when trying to retrieve the current project but none is set" do
    assert_raise Mix.NoProjectError, fn ->
      Mix.Project.get!()
    end
  end

  test "builds the project structure" do
    in_fixture("archive", fn ->
      config = [deps_app_path: Path.expand("_build/archive")]
      assert Mix.Project.build_structure(config) == :ok
      assert File.dir?("_build/archive/ebin")
      assert_proj_dir_linked_or_copied("_build/archive/priv", "priv", ~c"../../priv")
    end)
  end

  test "builds the project structure without symlinks" do
    in_fixture("archive", fn ->
      config = [deps_app_path: Path.expand("_build/archive"), build_embedded: true]
      assert Mix.Project.build_structure(config) == :ok
      assert File.dir?("_build/archive/ebin")
      assert {:error, _} = :file.read_link("_build/archive/ebin")
    end)
  end

  test "builds the project structure with symlinks" do
    in_fixture("archive", fn ->
      config = [deps_app_path: Path.expand("_build/archive")]
      File.mkdir_p!("include")

      assert Mix.Project.build_structure(config, symlink_ebin: true) == :ok
      assert_proj_dir_linked_or_copied("_build/archive/ebin", "ebin", ~c"../../ebin")
      assert_proj_dir_linked_or_copied("_build/archive/priv", "priv", ~c"../../priv")
      assert_proj_dir_linked_or_copied("_build/archive/include", "include", ~c"../../include")

      assert Mix.Project.build_structure(config) == :ok
      assert File.dir?("_build/archive/ebin")
      assert_proj_dir_linked_or_copied("_build/archive/priv", "priv", ~c"../../priv")
    end)
  end

  test "in_project pushes given configuration", context do
    in_tmp(context.test, fn ->
      result =
        Mix.Project.in_project(:foo, ".", [hello: :world], fn _ ->
          assert Mix.Project.config()[:app] == :foo
          assert Mix.Project.config()[:hello] == :world
          :result
        end)

      assert result == :result
    end)
  end

  test "in_project prints nice error message if fails to load file", context do
    in_tmp(context.test, fn ->
      File.write("mix.exs", """
      raise "oops"
      """)

      assert_raise RuntimeError, "oops", fn ->
        Mix.Project.in_project(:hello, ".", [], fn _ ->
          :ok
        end)
      end

      assert_receive {:mix_shell, :error, ["Error while loading project :hello at" <> _]}
    end)
  end

  defp assert_proj_dir_linked_or_copied(source, target, symlink_path) do
    case :file.read_link(source) do
      {:ok, path} ->
        case :os.type() do
          # relative symlink on Windows are broken, see symlink_or_copy/2
          {:win32, _} ->
            assert path ==
                     [source, ~c"..", symlink_path]
                     |> Path.join()
                     |> Path.expand()
                     |> String.to_charlist()

          _ ->
            assert path == symlink_path
        end

      {:error, _} ->
        assert File.ls!(source) == File.ls!(target)
    end
  end
end
