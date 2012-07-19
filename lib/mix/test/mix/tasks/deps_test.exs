Code.require_file "../../../test_helper", __FILE__

defmodule Mix.Tasks.DepsTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        deps: [
          { "ok", "0.1.0",         git: "https://github.com/elixir-lang/ok.git" },
          { "invalidvsn", "0.2.0", git: "https://github.com/elixir-lang/invalidvsn.git" },
          { "invalidapp", "0.1.0", git: "https://github.com/elixir-lang/invalidapp.git" },
          { "noappfile", "0.1.0",  git: "https://github.com/elixir-lang/noappfile.git" },
          { "uncloned", "0.1.0",   git: "https://github.com/elixir-lang/uncloned.git" }
        ]
      ]
    end
  end

  test "mix deps prints list of dependencies and their status" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Deps.run []

      assert_received { :mix_shell, :info, ["* ok (0.1.0) [git: \"https://github.com/elixir-lang/ok.git\"]"] }
      assert_received { :mix_shell, :info, ["  ok"] }
      assert_received { :mix_shell, :info, ["* invalidvsn [git: \"https://github.com/elixir-lang/invalidvsn.git\"]"] }
      assert_received { :mix_shell, :info, ["  the dependency does not match the specified version, got 0.1.0"] }
      assert_received { :mix_shell, :info, ["* invalidapp [git: \"https://github.com/elixir-lang/invalidapp.git\"]"] }
      assert_received { :mix_shell, :info, ["  the app file at deps/invalidapp/ebin/invalidapp.app is invalid"] }
      assert_received { :mix_shell, :info, ["* noappfile [git: \"https://github.com/elixir-lang/noappfile.git\"]"] }
      assert_received { :mix_shell, :info, ["  could not find app file at deps/noappfile/ebin/noappfile.app"] }
      assert_received { :mix_shell, :info, ["* uncloned [git: \"https://github.com/elixir-lang/uncloned.git\"]"] }
      assert_received { :mix_shell, :info, ["  the dependency is not checked out at: deps/uncloned"] }
    end
  after
    Mix.Project.pop
  end
end