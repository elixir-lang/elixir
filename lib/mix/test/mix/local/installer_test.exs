Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Local.InstallerTest do
  use MixTest.Case

  test "fetch" do
    dep_spec = {:git_repo, git: fixture_path("git_repo")}

    fetcher = fn mix_exs ->
      send(self(), mix_exs)
      Mix.Task.run("deps.get", [])
    end

    config =
      Mix.Local.Installer.fetch(dep_spec, fetcher, fn _mix_exs ->
        assert Mix.env() == :prod
        Mix.Project.config()
      end)

    # Fetcher should run twice
    assert_receive Mix.Local.Installer.MixProject
    assert_receive GitRepo.MixProject

    # In package runs once
    assert Mix.env() == :dev
    assert config[:app] == :git_repo
    assert config[:deps_path] =~ ~r/mix-local-installer-fetcher-.*\/deps/
    assert config[:lockfile] =~ ~r/mix-local-installer-fetcher-.*\/mix.lock/
  end

  test "parse_args Git" do
    args = ["git", "https://example.com/user/repo.git"]
    opts = [branch: "master", git: "https://example.com/user/repo.git", submodules: nil]

    assert Mix.Local.Installer.parse_args(args, []) == {:fetcher, {:"new package", opts}}
  end

  test "parse_args Git branch" do
    args = ["git", "https://example.com/user/repo.git", "branch", "not_master"]
    opts = [branch: "not_master", git: "https://example.com/user/repo.git", submodules: nil]

    assert Mix.Local.Installer.parse_args(args, []) == {:fetcher, {:"new package", opts}}
  end

  test "parse_args Git ref" do
    args = ["git", "https://example.com/user/repo.git", "ref", "not_master"]
    opts = [ref: "not_master", git: "https://example.com/user/repo.git", submodules: nil]

    assert Mix.Local.Installer.parse_args(args, []) == {:fetcher, {:"new package", opts}}
  end

  test "parse_args Git tag" do
    args = ["git", "https://example.com/user/repo.git", "tag", "not_master"]
    opts = [tag: "not_master", git: "https://example.com/user/repo.git", submodules: nil]

    assert Mix.Local.Installer.parse_args(args, []) == {:fetcher, {:"new package", opts}}
  end

  test "parse_args Git submodules" do
    args = ["git", "https://example.com/user/repo.git"]
    opts = [branch: "master", git: "https://example.com/user/repo.git", submodules: true]

    assert Mix.Local.Installer.parse_args(args, submodules: true) ==
             {:fetcher, {:"new package", opts}}
  end

  test "parse_args Git app" do
    args = ["git", "https://example.com/user/repo.git"]
    opts = [branch: "master", git: "https://example.com/user/repo.git", submodules: nil]

    assert Mix.Local.Installer.parse_args(args, app: "my_app") == {:fetcher, {:my_app, opts}}
  end

  test "parse_args GitHub" do
    args = ["github", "user/repo"]
    opts = [branch: "master", git: "https://github.com/user/repo.git", submodules: nil]

    assert Mix.Local.Installer.parse_args(args, []) == {:fetcher, {:"new package", opts}}
  end

  test "parse_args Hex" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package"], []) ==
             {:fetcher, {:a_package, ">= 0.0.0", [hex: :a_package]}}
  end

  test "parse_args Hex app" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package"], app: "my_app") ==
             {:fetcher, {:my_app, ">= 0.0.0", [hex: :a_package]}}
  end

  test "parse_args Hex version spec" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package", "1.0.0"], []) ==
             {:fetcher, {:a_package, "1.0.0", [hex: :a_package]}}
  end

  test "parse_args Hex with organization" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package"], organization: "my_org") ==
             {:fetcher, {:a_package, ">= 0.0.0", [hex: :a_package, organization: "my_org"]}}
  end

  test "parse_args Hex with repo" do
    assert Mix.Local.Installer.parse_args(["hex", "a_package"], repo: "my_repo") ==
             {:fetcher, {:a_package, ">= 0.0.0", [hex: :a_package, repo: "my_repo"]}}
  end
end
