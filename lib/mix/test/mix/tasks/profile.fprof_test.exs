Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Profile.FprofTest do
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

  test "profiles evaluated expression" do
    in_fixture "no_mixfile", fn ->
      assert capture_io(fn ->
        Mix.Tasks.Profile.Fprof.run(
          ["-e", "Enum.each(1..5, fn(_) -> HashSet.new end)"]
        )
      end) =~ ~r(HashSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end
  end

  test "profiles the script" do
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_fixture "no_mixfile", fn ->
      assert capture_io(fn ->
        Mix.Tasks.Profile.Fprof.run(
          [git_repo]
        )
      end) =~ ~r(:elixir_module\.compile/4 *\d+ *\d+\.\d{3} *\d+\.\d{3})
    end
  end

  test "expands callers" do
    in_fixture "no_mixfile", fn ->
      assert capture_io(fn ->
        Mix.Tasks.Profile.Fprof.run(
          ["-e", "Enum.each(1..5, fn(_) -> HashSet.new end)", "--callers"]
        )
      end) =~ ~r(HashSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3} +<--)
    end
  end

  test "expands processes" do
    in_fixture "no_mixfile", fn ->
      output = capture_io(fn ->
        Mix.Tasks.Profile.Fprof.run(
          ["-e", "spawn(fn -> :ok end); Enum.each(1..5, fn(_) -> HashSet.new end)", "--details"]
        )
      end)
      assert output =~ ~r(#{:erlang.pid_to_list(self)} +\d+ +\d+\.\d{3})
      assert output =~ ~r(spawned by #{:erlang.pid_to_list(self)})
      assert output =~ ~r(as :erlang.apply)
      assert output =~ ~r(initial calls:)
    end
  end

  test "sort options" do
    in_fixture "no_mixfile", fn ->
      assert capture_io(fn ->
        Mix.Tasks.Profile.Fprof.run(
          ["-e", "Enum.each(1..5, fn(_) -> HashSet.new end)", "--sort acc"]
        )
      end) =~ ~r(HashSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})

      assert capture_io(fn ->
        Mix.Tasks.Profile.Fprof.run(
          ["-e", "Enum.each(1..5, fn(_) -> HashSet.new end)", "--sort own"]
        )
      end) =~ ~r(HashSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end
  end

  test "errors on missing files" do
    in_fixture "no_mixfile", fn ->
      assert_raise Mix.Error, "No files matched pattern \"non-existent\" given to --require", fn ->
        capture_io(fn -> Mix.Tasks.Profile.Fprof.run ["-r", "non-existent"] end)
      end

      assert_raise Mix.Error, "No files matched pattern \"non-existent\" given to --parallel-require", fn ->
        capture_io(fn -> Mix.Tasks.Profile.Fprof.run ["-pr", "non-existent"] end)
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        capture_io(fn -> Mix.Tasks.Profile.Fprof.run ["non-existent"] end)
      end

      assert File.dir?("lib")
      assert_raise Mix.Error, "No such file: lib", fn ->
        capture_io(fn -> Mix.Tasks.Profile.Fprof.run ["lib"] end)
      end
    end
  end
end
