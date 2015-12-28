Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Profile.FprofTest do
  use MixTest.Case

  import ExUnit.CaptureIO

  alias Mix.Tasks.Profile.Fprof

  @moduletag apps: [:sample]

  setup do
    Mix.Project.push MixTest.Case.Sample
  end

  test "profiles evaluated expression", context do
    in_tmp context.test, fn ->
      assert capture_io(fn ->
        Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)"])
      end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end
  end

  test "profiles the script", context do
    git_repo = fixture_path("git_repo/lib/git_repo.ex")

    in_tmp context.test, fn ->
      assert capture_io(fn ->
        Fprof.run([git_repo])
      end) =~ ~r(:elixir_module\.compile/4 *\d+ *\d+\.\d{3} *\d+\.\d{3})
    end
  after
    purge [GitRepo]
  end

  test "expands callers", context do
    in_tmp context.test, fn ->
      assert capture_io(fn ->
        Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)", "--callers"])
      end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3} +<--)
    end
  end

  test "expands processes", context do
    in_tmp context.test, fn ->
      output = capture_io(fn ->
        Fprof.run(["-e", "spawn(fn -> :ok end); Enum.each(1..5, fn(_) -> MapSet.new end)", "--details"])
      end)
      assert output =~ ~r(#{:erlang.pid_to_list(self)} +\d+ +\d+\.\d{3})
      assert output =~ ~r(spawned by #{:erlang.pid_to_list(self)})
      assert output =~ ~r(as :erlang.apply)
      assert output =~ ~r(initial calls:)
    end
  end

  test "sort options", context do
    in_tmp context.test, fn ->
      assert capture_io(fn ->
        Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)", "--sort acc"])
      end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})

      assert capture_io(fn ->
        Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)", "--sort own"])
      end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end
  end

  test "errors on missing files", context do
    in_tmp context.test, fn ->
      assert_raise Mix.Error, "No files matched pattern \"non-existent\" given to --require", fn ->
        capture_io(fn -> Fprof.run ["-r", "non-existent"] end)
      end

      assert_raise Mix.Error, "No files matched pattern \"non-existent\" given to --parallel-require", fn ->
        capture_io(fn -> Fprof.run ["-pr", "non-existent"] end)
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        capture_io(fn -> Fprof.run ["non-existent"] end)
      end

      File.mkdir_p!("lib")
      assert_raise Mix.Error, "No such file: lib", fn ->
        capture_io(fn -> Fprof.run ["lib"] end)
      end
    end
  end
end
