Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  defmodule DepsApp do
    def project do
      [
        app: :sample,
        version: "0.1.0",
        deps: [
          { :tidy, "0.1.0", path: "elixir-lang/tidy" }
        ]
      ]
    end
  end

  test "compile a project without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/Elixir.A.beam")
      Mix.Tasks.Clean.run []
      refute File.regular?("ebin/Elixir.A.beam")
    end
  end

  test "cleans all repos" do
    Mix.Project.push DepsApp

    in_fixture "deps_status", fn ->
      Mix.Tasks.Clean.run ["--all"]
      assert_received { :mix_shell, :info, ["* Cleaning tidy [path: \"elixir-lang/tidy\"]"] }
    end
  after
    Mix.Project.pop
  end
end
