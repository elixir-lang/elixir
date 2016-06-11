Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.EscriptTest do
  use MixTest.Case

  defmodule Escript do
    def project do
      [app: :escripttest,
       version: "0.0.1",
       escript: [
         main_module: Escripttest,
         name: "escriptest",
         embed_elixir: true
       ]]
    end
  end

  defmodule EscriptWithPath do
    def project do
      [app: :escripttestwithpath,
       version: "0.0.1",
       escript: [
         app: nil,
         embed_elixir: true,
         main_module: Escripttest,
         path: Path.join("ebin", "escripttestwithpath")
       ]]
    end
  end

  defmodule EscriptWithDeps do
    def project do
      [app: :escripttestwithdeps,
       version: "0.0.1",
       escript: [main_module: Escripttest],
       deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]]
    end
  end

  defmodule EscriptErlangWithDeps do
    def project do
      [app: :escripttesterlangwithdeps,
       version: "0.0.1",
       language: :erlang,
       escript: [main_module: :escripttest],
       deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]]
    end
  end

  defmodule EscriptWithUnknownMainModule do
    def project do
      [app: :escripttestwithunknownmainmodule,
       version: "0.0.1",
       escript: [
         main_module: BogusEscripttest
       ]]
    end
  end

  defmodule EscriptConsolidated do
    def project do
      [app: :escripttestconsolidated,
       build_embedded: true,
       version: "0.0.1",
       escript: [main_module: Escripttest]]
    end
  end

  test "generate escript" do
    Mix.Project.push Escript

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escriptest with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escriptest"]) == {"TEST\n", 0}

      Mix.Tasks.Escript.Build.run []
      refute_received {:mix_shell, :info, ["Generated escript escriptest with MIX_ENV=dev"]}
    end
  end

  test "generate escript with config" do
    Mix.Project.push Escript

    in_fixture "escripttest", fn ->
      File.mkdir_p! "config"
      File.write! "config/config.exs", """
      [foobar: [value: "FROM CONFIG", other: %{}]]
      """
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escriptest with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escriptest"]) == {"FROM CONFIG\n", 0}
    end
  end

  test "generate escript with path" do
    Mix.Project.push EscriptWithPath

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript ebin/escripttestwithpath with MIX_ENV=dev"]}
      assert System.cmd("escript", ["ebin/escripttestwithpath"]) == {"TEST\n", 0}
    end
  end

  test "generate escript with deps" do
    Mix.Project.push EscriptWithDeps

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escripttestwithdeps with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escripttestwithdeps"]) == {"TEST\n", 0}
    end
  after
    purge [Ok.Mixfile]
  end

  test "generate escript with Erlang and deps" do
    Mix.Project.push EscriptErlangWithDeps

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escripttesterlangwithdeps with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escripttesterlangwithdeps"]) == {"Erlang value", 0}
    end
  after
    purge [Ok.Mixfile]
  end

  test "generate escript with consolidated protocols" do
    Mix.Project.push EscriptConsolidated

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escripttestconsolidated with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escripttestconsolidated", "Enumerable"]) == {"true\n", 0}
    end
  end

  test "escript install and uninstall" do
    File.rm_rf! tmp_path(".mix/escripts")
    Mix.Project.push Escript

    in_fixture "escripttest", fn ->
      # build the escript
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escriptest with MIX_ENV=dev"]}

      # check that no escripts are installed
      Mix.Tasks.Escript.run []
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}

      # install our escript
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Escript.Install.run []

      # check that it shows in the list
      Mix.Tasks.Escript.run []
      assert_received {:mix_shell, :info, ["* escriptest"]}
      refute_received {:mix_shell, :info, ["* escriptest.bat"]}

      # check uninstall confirmation
      send self, {:mix_shell_input, :yes?, false}
      Mix.Tasks.Escript.Uninstall.run ["escriptest"]
      assert File.regular? tmp_path(".mix/escripts/escriptest")

      # uninstall the escript
      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Escript.Uninstall.run ["escriptest"]
      refute File.regular? tmp_path(".mix/escripts/escriptest")
      refute File.regular? tmp_path(".mix/escripts/escriptest.bat")

      # check that no escripts remain
      Mix.Tasks.Escript.run []
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}
    end
  end

  test "escript invalid install" do
    # Install our escript
    send self, {:mix_shell_input, :yes?, true}
    assert_raise Mix.Error,
                 "The given path does not point to an escript, installation aborted", fn ->
      Mix.Tasks.Escript.Install.run [__ENV__.file]
    end
  end

  test "escript invalid main module" do
    Mix.Project.push EscriptWithUnknownMainModule

    in_fixture "escripttest", fn ->
      assert_raise Mix.Error, "Could not generate escript, module Elixir.BogusEscripttest defined as " <>
                ":main_module could not be loaded", fn ->
        Mix.Tasks.Escript.Build.run []
      end
    end
  end

  test "escript.install git full" do
    in_fixture "git_repo", fn ->
      File.write! "lib/git_repo.ex", """
      defmodule GitRepo do
        def main(_argv) do
          IO.puts "TEST"
        end
      end
      """

      File.write! "mix.exs", """
      defmodule GitRepo.Mixfile do
        use Mix.Project

        def project do
          [app: :git_repo, version: "0.1.0", escript: [main_module: GitRepo]]
        end
      end
      """

      System.cmd("git", ~w[add .])
      System.cmd("git", ~w[commit -m "ok"])

      send self, {:mix_shell_input, :yes?, true}
      Mix.Tasks.Escript.Install.run ["git", File.cwd!()]
      assert_received {:mix_shell, :info, ["Generated escript git_repo with MIX_ENV=prod"]}

      escript_path = Path.join([tmp_path(".mix"), "escripts", "git_repo"])
      assert System.cmd("escript", [escript_path]) == {"TEST\n", 0}
    end
  after
    purge [GitRepo, GitRepo.Mixfile]
  end

  defmodule TestFetch do
    def fetch(package_name, dep_spec, _in_package) do
      Agent.update(__MODULE__, fn _ -> {package_name, dep_spec} end)
    end
  end

  test "escript.install git" do
    assert_installed ["git", "https://example.com/user/repo.git"],
      {"new escript", {:"new escript", [branch: "master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "escript.install git branch" do
    assert_installed ["git", "https://example.com/user/repo.git", "branch", "not_master"],
      {"new escript", {:"new escript", [branch: "not_master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "escript.install git ref" do
    assert_installed ["git", "https://example.com/user/repo.git", "ref", "not_master"],
      {"new escript", {:"new escript", [ref: "not_master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "escript.install git tag" do
    assert_installed ["git", "https://example.com/user/repo.git", "tag", "not_master"],
      {"new escript", {:"new escript", [tag: "not_master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "escript.install git submodules" do
    assert_installed ["--submodules", "git", "https://example.com/user/repo.git"],
      {"new escript", {:"new escript", [branch: "master", git: "https://example.com/user/repo.git", submodules: true]}}
  end

  test "escript.install git app" do
    assert_installed ["--app", "my_app", "git", "https://example.com/user/repo.git"],
      {"my_app", {:my_app, [branch: "master", git: "https://example.com/user/repo.git", submodules: nil]}}
  end

  test "escript.install github" do
    assert_installed ["github", "user/repo"],
      {"new escript", {:"new escript", [branch: "master", git: "https://github.com/user/repo.git", submodules: nil]}}
  end

  test "escript.install hex" do
    assert_installed ["hex", "a_package"],
      {"a_package", {:a_package, ">= 0.0.0", [hex: :a_package]}}
  end

  test "escript.install hex app" do
    assert_installed ["--app", "app_name", "hex", "a_package"],
      {"app_name", {:app_name, ">= 0.0.0", [hex: :a_package]}}
  end

  test "escript.install hex version spec" do
    assert_installed ["hex", "a_package", "1.0.0"],
      {"a_package", {:a_package, "1.0.0", [hex: :a_package]}}
  end

  defp assert_installed(argv, expected_agent_state) do
    Agent.start_link(fn -> {nil, nil} end, name: TestFetch)

    Mix.Tasks.Escript.Install.run(argv, TestFetch)

    agent_state = Agent.get(TestFetch, fn state -> state end)

    assert agent_state == expected_agent_state

    Agent.stop(TestFetch)
  end
end
