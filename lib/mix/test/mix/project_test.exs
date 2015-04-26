Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ProjectTest do
  use MixTest.Case

  defmodule SampleProject do
    def project do
      [app: :sample, hello: "world"]
    end
  end

  test "push and pop projects" do
    refute Mix.Project.get
    Mix.Project.push(SampleProject, "sample")
    assert Mix.Project.get == SampleProject

    assert %{name: SampleProject, config: _, file: "sample"} = Mix.Project.pop
    assert nil = Mix.Project.pop
  end

  test "does not allow the same project to be pushed twice" do
    Mix.Project.push(SampleProject, "sample")

    assert_raise Mix.Error, ~r/Mix.ProjectTest.SampleProject from "another"/, fn ->
      Mix.Project.push(SampleProject, "another")
    end
  end

  test "allows nil projects to be pushed twice" do
    Mix.Project.push nil
    Mix.Project.push nil
    assert is_map Mix.Project.pop
    assert is_map Mix.Project.pop
    assert is_nil Mix.Project.pop
  end

  test "retrieves configuration from projects" do
    Mix.Project.push(SampleProject)
    assert Mix.Project.config[:hello] == "world"
  end

  test "removes private configuration" do
    Mix.Project.push(SampleProject)
    assert is_nil Mix.Project.config[:app_path]
  end

  test "retrieves configuration even when a project is not set" do
    assert Mix.Project.config[:default_task] == "run"
  end

  test "raises an error when trying to retrieve the current project but none is set" do
    assert_raise Mix.NoProjectError, fn ->
      Mix.Project.get!
    end
  end

  test "builds the project structure" do
    in_fixture "archive", fn ->
      config = [app_path: Path.expand("_build/archive")]
      assert Mix.Project.build_structure(config) == :ok
      assert File.dir?("_build/archive/ebin")
      assert_proj_dir_linked_or_copied("_build/archive/priv", "priv", '../../priv')
    end
  end

  test "builds the project structure without symlinks" do
    in_fixture "archive", fn ->
      config = [app_path: Path.expand("_build/archive"), build_embedded: true]
      assert Mix.Project.build_structure(config) == :ok
      assert File.dir?("_build/archive/ebin")
      assert {:error, _} = :file.read_link("_build/archive/ebin")
    end
  end

  test "builds the project structure with symlinks" do
    in_fixture "archive", fn ->
      config = [app_path: Path.expand("_build/archive")]
      File.mkdir_p!("include")

      assert Mix.Project.build_structure(config, symlink_ebin: true) == :ok
      assert_proj_dir_linked_or_copied("_build/archive/ebin", "ebin", '../../ebin')
      assert_proj_dir_linked_or_copied("_build/archive/priv", "priv", '../../priv')
      assert_proj_dir_linked_or_copied("_build/archive/include", "include", '../../include')

      assert Mix.Project.build_structure(config) == :ok
      assert File.dir?("_build/archive/ebin")
      assert_proj_dir_linked_or_copied("_build/archive/priv", "priv", '../../priv')
    end
  end

  test "in_project pushes given configuration" do
    in_fixture "no_mixfile", fn ->
      Mix.Project.in_project :foo, ".", [hello: :world], fn _ ->
        assert Mix.Project.config[:app] == :foo
        assert Mix.Project.config[:hello] == :world
      end
    end
  end

  test "in_project prints nice error message if fails to load file" do
    in_fixture "no_mixfile", fn ->
      File.write "mix.exs", """
      raise "oops"
      """

      assert_raise RuntimeError, "oops", fn ->
        Mix.Project.in_project :hello, ".", [], fn _ ->
          :ok
        end
      end

      assert_receive {:mix_shell, :error, ["Error while loading project :hello at" <> _]}
    end
  end

  test "config_files" do
    Mix.Project.push(SampleProject)

    in_fixture "no_mixfile", fn ->
      File.mkdir_p!("config")
      File.write! "config/config.exs", "[]"
      File.write! "config/dev.exs", "[]"
      File.write! "config/.exs", "[]"

      files = Mix.Project.config_files
      assert __ENV__.file in files
      assert "config/config.exs" in files
      assert "config/dev.exs" in files
      refute "config/.exs" in files
    end
  end

  defp assert_proj_dir_linked_or_copied(source, target, symlink_path) do
    case :file.read_link(source) do
      {:ok, path} -> assert path == symlink_path
      {:error, _} -> assert File.ls!(source) == File.ls!(target)
    end
  end
end
