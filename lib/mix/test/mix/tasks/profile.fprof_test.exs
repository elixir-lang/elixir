Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Profile.FprofTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  alias Mix.Tasks.Profile.Fprof

  test "profiles evaluated expression", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)"])
             end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end)
  end

  test "profiles the script", context do
    in_tmp(context.test, fn ->
      profile_script_name = "profile_script.ex"

      File.write!(profile_script_name, """
      Enum.each(1..5, fn(_) -> MapSet.new end)
      """)

      assert capture_io(fn ->
               Fprof.run([profile_script_name])
             end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end)
  end

  test "expands callers", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)", "--callers"])
             end) =~ ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3} +<--)
    end)
  end

  test "expands processes", context do
    in_tmp(context.test, fn ->
      expr = "spawn(fn -> Process.sleep(:infinity) end); Enum.each(1..5, fn(_) -> MapSet.new end)"
      output = capture_io(fn -> Fprof.run(["-e", expr, "--details"]) end)
      assert output =~ ~r(#{:erlang.pid_to_list(self())} +\d+ +\d+\.\d{3})
      assert output =~ ~r(spawned by #{:erlang.pid_to_list(self())})
      assert output =~ ~r(as :erlang.apply)
      assert output =~ ~r(initial calls:)
    end)
  end

  test "sort options", context do
    in_tmp(context.test, fn ->
      expr = "Enum.each(1..5, fn(_) -> MapSet.new end)"

      assert capture_io(fn -> Fprof.run(["-e", expr, "--sort", "acc"]) end) =~
               ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})

      expr = "Enum.each(1..5, fn(_) -> MapSet.new end)"

      assert capture_io(fn -> Fprof.run(["-e", expr, "--sort", "own"]) end) =~
               ~r(MapSet\.new/0 *5 *\d+\.\d{3} *\d+\.\d{3})
    end)
  end

  test "errors on missing files", context do
    in_tmp(context.test, fn ->
      message = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Fprof.run(["-r", "non-existent"]) end)
      end

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Fprof.run(["-pr", "non-existent"]) end)
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        capture_io(fn -> Fprof.run(["non-existent"]) end)
      end

      File.mkdir_p!("lib")

      assert_raise Mix.Error, "No such file: lib", fn ->
        capture_io(fn -> Fprof.run(["lib"]) end)
      end
    end)
  end

  test "warmup", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)"])
             end) =~ "Warmup..."

      refute capture_io(fn ->
               Fprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)", "--no-warmup"])
             end) =~ "Warmup..."
    end)
  end
end
