Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.EscriptTest do
  use MixTest.Case

  defmodule Escript do
    def project do
      [app: :escript_test,
       version: "0.0.1",
       escript: [
         main_module: EscriptTest,
         name: "escript_test",
         embed_elixir: true
       ]]
    end
  end

  defmodule EscriptWithPath do
    def project do
      [app: :escript_test_with_path,
       version: "0.0.1",
       escript: [
         app: nil,
         embed_elixir: true,
         main_module: EscriptTest,
         path: Path.join("ebin", "escript_test_with_path")
       ]]
    end
  end

  defmodule EscriptWithDeps do
    def project do
      [app: :escript_test_with_deps,
       version: "0.0.1",
       escript: [main_module: EscriptTest],
       deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]]
    end
  end

  defmodule EscriptErlangWithDeps do
    def project do
      [app: :escript_test_erlang_with_deps,
       version: "0.0.1",
       language: :erlang,
       escript: [main_module: :escript_test],
       deps: [{:ok, path: fixture_path("deps_status/deps/ok")}]]
    end
  end

  defmodule EscriptWithUnknownMainModule do
    def project do
      [app: :escript_test_with_unknown_main_module,
       version: "0.0.1",
       escript: [
         main_module: BogusEscriptTest
       ]]
    end
  end

  defmodule EscriptConsolidated do
    def project do
      [app: :escript_test_consolidated,
       build_embedded: true,
       version: "0.0.1",
       escript: [main_module: EscriptTest]]
    end
  end

  test "generate escript" do
    Mix.Project.push Escript

    in_fixture "escript_test", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test"]) == {"TEST\n", 0}

      Mix.Tasks.Escript.Build.run []
      refute_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}
    end
  end

  test "generate escript with config" do
    Mix.Project.push Escript

    in_fixture "escript_test", fn ->
      File.mkdir_p! "config"
      File.write! "config/config.exs", """
      [foobar: [value: "FROM CONFIG", other: %{}]]
      """
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test"]) == {"FROM CONFIG\n", 0}
    end
  end

  test "generate escript with path" do
    Mix.Project.push EscriptWithPath

    in_fixture "escript_test", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript ebin/escript_test_with_path with MIX_ENV=dev"]}
      assert System.cmd("escript", ["ebin/escript_test_with_path"]) == {"TEST\n", 0}
    end
  end

  test "generate escript with deps" do
    Mix.Project.push EscriptWithDeps

    in_fixture "escript_test", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escript_test_with_deps with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test_with_deps"]) == {"TEST\n", 0}
    end
  after
    purge [Ok.MixFile]
  end

  test "generate escript with Erlang and deps" do
    Mix.Project.push EscriptErlangWithDeps

    in_fixture "escript_test", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escript_test_erlang_with_deps with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test_erlang_with_deps"]) == {"Erlang value", 0}
    end
  after
    purge [Ok.MixFile]
  end

  test "generate escript with consolidated protocols" do
    Mix.Project.push EscriptConsolidated

    in_fixture "escript_test", fn ->
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escript_test_consolidated with MIX_ENV=dev"]}
      assert System.cmd("escript", ["escript_test_consolidated", "Enumerable"]) == {"true\n", 0}
    end
  end

  test "escript install and uninstall" do
    File.rm_rf! tmp_path(".mix/escripts")
    Mix.Project.push Escript

    in_fixture "escript_test", fn ->
      # build the escript
      Mix.Tasks.Escript.Build.run []
      assert_received {:mix_shell, :info, ["Generated escript escript_test with MIX_ENV=dev"]}

      # check that no escripts are installed
      Mix.Tasks.Escript.run []
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}

      # install our escript
      send self(), {:mix_shell_input, :yes?, true}
      Mix.Tasks.Escript.Install.run []

      # check that it shows in the list
      Mix.Tasks.Escript.run []
      assert_received {:mix_shell, :info, ["* escript_test"]}
      refute_received {:mix_shell, :info, ["* escript_test.bat"]}

      # check uninstall confirmation
      send self(), {:mix_shell_input, :yes?, false}
      Mix.Tasks.Escript.Uninstall.run ["escript_test"]
      assert File.regular? tmp_path(".mix/escripts/escript_test")

      # uninstall the escript
      send self(), {:mix_shell_input, :yes?, true}
      Mix.Tasks.Escript.Uninstall.run ["escript_test"]
      refute File.regular? tmp_path(".mix/escripts/escript_test")
      refute File.regular? tmp_path(".mix/escripts/escript_test.bat")

      # check that no escripts remain
      Mix.Tasks.Escript.run []
      assert_received {:mix_shell, :info, ["No escripts currently installed."]}
    end
  end

  test "escript invalid install" do
    # Install our escript
    send self(), {:mix_shell_input, :yes?, true}
    assert_raise Mix.Error,
                 "The given path does not point to an escript, installation aborted", fn ->
      Mix.Tasks.Escript.Install.run [__ENV__.file]
    end
  end

  test "escript invalid main module" do
    Mix.Project.push EscriptWithUnknownMainModule

    in_fixture "escript_test", fn ->
      assert_raise Mix.Error, "Could not generate escript, module Elixir.BogusEscriptTest defined as " <>
                ":main_module could not be loaded", fn ->
        Mix.Tasks.Escript.Build.run []
      end
    end
  end

  test "escript.install from git" do
    in_fixture "git_repo", fn ->
      File.write! "lib/git_repo.ex", """
      defmodule GitRepo do
        def main(_argv) do
          IO.puts "TEST"
        end
      end
      """

      File.write! "mix.exs", """
      defmodule GitRepo.MixFile do
        use Mix.Project

        def project do
          [app: :git_repo, version: "0.1.0", escript: [main_module: GitRepo]]
        end
      end
      """

      System.cmd("git", ~w[add .])
      System.cmd("git", ~w[commit -m "ok"])

      send self(), {:mix_shell_input, :yes?, true}
      Mix.Tasks.Escript.Install.run ["git", File.cwd!()]
      assert_received {:mix_shell, :info, ["Generated escript git_repo with MIX_ENV=prod"]}

      escript_path = Path.join([tmp_path(".mix"), "escripts", "git_repo"])
      assert System.cmd("escript", [escript_path]) == {"TEST\n", 0}
    end
  after
    purge [GitRepo, GitRepo.MixFile]
  end
end
