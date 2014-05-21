Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  defmodule GetApp do
    def project do
      [ app: :get_app,
        version: "0.1.0",
        deps: [
          {:git_repo, "0.1.0", git: MixTest.Case.fixture_path("git_repo")}
        ] ]
    end
  end

  setup do
    Mix.Project.push MixTest.Case.Sample
  end

  test "run requires files before evaling commands" do
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Run.run ["-r", git_repo, "-e", "send self, {:hello, GitRepo.hello}"]
      assert_received {:hello, "World"}

      Mix.Tasks.Run.run ["-pr", git_repo, "-e", "send self, {:hello, GitRepo.hello}"]
      assert_received {:hello, "World"}
    end
  after
    purge [GitRepo]
  end

  test "run rewrites System.argv" do
    in_fixture "no_mixfile", fn ->
      file = "argv.exs"

      File.write! file, "send self, {:argv, System.argv}"
      unload_file = fn ->
        Code.unload_files [Path.expand(file)]
      end

      Mix.Tasks.Run.run [file]
      assert_received {:argv, []}

      unload_file.()
      Mix.Tasks.Run.run [file, "foo", "-e", "bar"]
      assert_received {:argv, ["foo", "-e", "bar"]}

      unload_file.()
      Mix.Tasks.Run.run ["-e", "send self, {:argv, System.argv}", file, "foo", "-x", "bar"]
      assert_received {:argv, [^file, "foo", "-x", "bar"]}

      unload_file.()
      Mix.Tasks.Run.run [
        "-e", "send self, :evaled",
        "-e", "send self, {:argv, System.argv}",
        "--no-compile", file, "-x", "bar"
      ]
      assert_received :evaled
      assert_received {:argv, [^file, "-x", "bar"]}
    end
  end
end
