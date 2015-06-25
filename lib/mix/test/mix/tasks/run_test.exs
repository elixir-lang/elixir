Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  import ExUnit.CaptureIO

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

  test "loads configuration" do
    in_fixture "no_mixfile", fn ->
      assert capture_io(fn ->
        Mix.Task.run "run",
          ["--config", fixture_path("configs/good_config.exs"),
           "--eval", "IO.puts Application.get_env(:my_app, :key)"]
      end) == "value\n"
    end
  after
    Application.delete_env(:my_app, :key)
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

  test "run errors on missing files" do
    in_fixture "no_mixfile", fn ->
      assert_raise Mix.Error, "No files matched pattern \"non-existent\" given to --require", fn ->
        Mix.Tasks.Run.run ["-r", "non-existent"]
      end

      assert_raise Mix.Error, "No files matched pattern \"non-existent\" given to --parallel-require", fn ->
        Mix.Tasks.Run.run ["-pr", "non-existent"]
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        Mix.Tasks.Run.run ["non-existent"]
      end

      assert File.dir?("lib")
      assert_raise Mix.Error, "No such file: lib", fn ->
        Mix.Tasks.Run.run ["lib"]
      end
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
