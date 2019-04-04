Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  @moduletag apps: [:sample]

  setup do
    Mix.Project.push(MixTest.Case.Sample)
  end

  test "loads configuration", context do
    in_tmp(context.test, fn ->
      config = fixture_path("configs/good_config.exs")
      expr = "IO.puts Application.get_env(:my_app, :key)"

      output =
        capture_io(:stderr, fn ->
          assert capture_io(fn -> Mix.Task.run("run", ["--config", config, "--eval", expr]) end) ==
                   "value\n"
        end)

      assert output =~ "deprecated"
    end)
  after
    Application.delete_env(:my_app, :key)
  end

  test "run requires files before evaling commands", context do
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_tmp(context.test, fn ->
      Mix.Tasks.Run.run(["-r", git_repo, "-e", "send self(), {:hello, GitRepo.hello}"])
      assert_received {:hello, "World"}

      Mix.Tasks.Run.run(["-pr", git_repo, "-e", "send self(), {:hello, GitRepo.hello}"])
      assert_received {:hello, "World"}
    end)
  after
    purge([GitRepo])
  end

  test "does not start applications on --no-start", context do
    in_tmp(context.test, fn ->
      expr = "send self(), {:apps, Application.started_applications}"
      Mix.Tasks.Run.run(["--no-start", "-e", expr])

      assert_received {:apps, apps}
      refute List.keyfind(apps, :sample, 0)
      Mix.Task.clear()

      Mix.Tasks.Run.run(["-e", expr])
      assert_received {:apps, apps}
      assert List.keyfind(apps, :sample, 0)
    end)
  end

  test "run errors on missing files", context do
    in_tmp(context.test, fn ->
      message = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Run.run(["-r", "non-existent"])
      end

      assert_raise Mix.Error, message, fn ->
        Mix.Tasks.Run.run(["-pr", "non-existent"])
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        Mix.Tasks.Run.run(["non-existent"])
      end

      File.mkdir_p!("lib")

      assert_raise Mix.Error, "No such file: lib", fn ->
        Mix.Tasks.Run.run(["lib"])
      end
    end)
  after
    purge([GitRepo])
  end

  test "run rewrites System.argv", context do
    in_tmp(context.test, fn ->
      file = "argv.exs"
      expr = "send self(), {:argv, System.argv}"

      File.write!(file, expr)

      unload_file = fn ->
        Code.unrequire_files([Path.expand(file)])
      end

      Mix.Tasks.Run.run([file])
      assert_received {:argv, []}

      unload_file.()
      Mix.Tasks.Run.run([file, "foo", "-e", "bar"])
      assert_received {:argv, ["foo", "-e", "bar"]}

      unload_file.()
      Mix.Tasks.Run.run(["-e", expr, file, "foo", "-x", "bar"])
      assert_received {:argv, [^file, "foo", "-x", "bar"]}

      unload_file.()

      send_evaled = "send self(), :evaled"
      Mix.Tasks.Run.run(["-e", send_evaled, "-e", expr, "--no-compile", file, "-x", "bar"])

      assert_received :evaled
      assert_received {:argv, [^file, "-x", "bar"]}
    end)
  end
end
