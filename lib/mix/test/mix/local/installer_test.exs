Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Local.InstallerTest do
  use MixTest.Case

  test "fetch" do
    dep_spec = {:"git repo", git: fixture_path("git_repo")}

    config =
      Mix.Local.Installer.fetch dep_spec, fn _mix_exs ->
        assert Mix.env() == :prod
        Mix.Project.config()
      end

    assert Mix.env() == :dev

    assert config[:app] == :git_repo
    assert config[:deps_path] =~ ~r/mix-local-installer-fetcher-.*\/deps/
    assert config[:lockfile] =~ ~r/mix-local-installer-fetcher-.*\/mix.lock/
  end

  test "parse_args Git" do
    assert Mix.Local.Installer.parse_args(["git", "https://example.com/user/repo.git"], []) ==
      {:fetcher, {:"new package", [branch: "master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "parse_args Git branch" do
    assert Mix.Local.Installer.parse_args(["git", "https://example.com/user/repo.git", "branch", "not_master"], []) ==
      {:fetcher, {:"new package", [branch: "not_master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "parse_args Git ref" do
    assert Mix.Local.Installer.parse_args(["git", "https://example.com/user/repo.git", "ref", "not_master"], []) ==
      {:fetcher, {:"new package", [ref: "not_master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "parse_args Git tag" do
    assert Mix.Local.Installer.parse_args(["git", "https://example.com/user/repo.git", "tag", "not_master"], []) ==
      {:fetcher, {:"new package", [tag: "not_master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "parse_args Git submodules" do
    assert Mix.Local.Installer.parse_args(["git", "https://example.com/user/repo.git"], [submodules: true]) ==
      {:fetcher, {:"new package", [branch: "master", git: "https://example.com/user/repo.git", submodules: true]}}
  end

  test "parse_args Git app" do
    assert Mix.Local.Installer.parse_args(["git", "https://example.com/user/repo.git"], [app: "my_app"]) ==
      {:fetcher, {:my_app, [branch: "master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "parse_args GitHub" do
    assert Mix.Local.Installer.parse_args(["github", "user/repo"], []) ==
      {:fetcher, {:"new package", [branch: "master", git: "https://github.com/user/repo.git", submodules: nil]}}
  end

  test "parse_args Hex" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package"], []) ==
      {:fetcher, {:a_package, ">= 0.0.0", [hex: :a_package]}}
  end

  test "parse_args Hex app" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package"], [app: "my_app"]) ==
      {:fetcher, {:my_app, ">= 0.0.0", [hex: :a_package]}}
  end

  test "parse_args Hex version spec" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package", "1.0.0"], []) ==
      {:fetcher, {:a_package, "1.0.0", [hex: :a_package]}}
  end
end
