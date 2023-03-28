Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.RunTest do
  use MixTest.Case

  setup do
    Mix.Project.push(MixTest.Case.Sample)
  end

  test "run requires files before evaling commands", context do
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_tmp(context.test, fn ->
      Mix.Tasks.Run.run(["-r", git_repo, "-e", "send self(), {:hello, GitRepo.hello()}"])
      assert_received {:hello, "World"}

      Mix.Tasks.Run.run(["-pr", git_repo, "-e", "send self(), {:hello, GitRepo.hello()}"])
      assert_received {:hello, "World"}
    end)
  after
    purge([GitRepo])
  end

  test "does not start applications on --no-start", context do
    in_tmp(context.test, fn ->
      expr = "send self(), {:apps, Application.started_applications()}"
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
      expr = "send self(), {:argv, System.argv()}"

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
      Mix.Tasks.Run.run([file, "foo", "--", "bar"])
      assert_received {:argv, ["foo", "--", "bar"]}

      unload_file.()
      Mix.Tasks.Run.run([file, "--custom-opt", "foo", "--", "bar"])
      assert_received {:argv, ["--custom-opt", "foo", "--", "bar"]}

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

  defmodule AppArgvSample do
    def project do
      [app: :app_argv_sample, version: "0.1.0"]
    end

    def application do
      send(self(), {:argv, System.argv()})
      [extra_applications: [:logger]]
    end
  end

  describe "rewrite System.argv without file arg" do
    setup do
      Mix.Project.pop()
      Mix.Project.push(AppArgvSample)
      :ok
    end

    test "no args" do
      in_fixture("no_mixfile", fn ->
        Mix.Tasks.Run.run(["--"])
        assert_received {:argv, []}
      end)
    end

    test "multiple args" do
      in_fixture("no_mixfile", fn ->
        Mix.Tasks.Run.run(["--", "foo", "bar"])
        assert_received {:argv, ["foo", "bar"]}
      end)
    end

    test "with opts" do
      in_fixture("no_mixfile", fn ->
        Mix.Tasks.Run.run(["--no-start", "--", "foo", "bar"])
        assert_received {:argv, ["foo", "bar"]}
      end)
    end

    test "with eval" do
      in_fixture("no_mixfile", fn ->
        expr = "send self(), {:test, :argv}"
        Mix.Tasks.Run.run(["-e", expr, "--", "foo"])
        assert_received {:argv, ["foo"]}
        assert_received {:test, :argv}
      end)
    end
  end
end
