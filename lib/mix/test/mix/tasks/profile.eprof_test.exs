Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Profile.EprofTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  alias Mix.Tasks.Profile.Eprof

  @expr "Enum.each(1..5, &String.Chars.Integer.to_string/1)"

  test "profiles evaluated expression", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Eprof.run(["-e", @expr])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1\s+\d)
    end)
  end

  test "profiles evaluated expression in multiple processes", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Eprof.run(["-e", "spawn(fn -> #{@expr} end)"])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1\s+\d)
    end)
  end

  test "profiles the script", context do
    in_tmp(context.test, fn ->
      profile_script_name = "profile_script.ex"
      File.write!(profile_script_name, @expr)

      assert capture_io(fn ->
               Eprof.run([profile_script_name])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1\s+\d)
    end)
  end

  test "filters based on count", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Eprof.run(["--calls", "5", "-e", @expr])
             end) =~ ":erlang.apply/2"
    end)
  end

  test "sorts based on the calls count", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Eprof.run(["--sort", "calls", "-e", @expr])
             end) =~ ~r(erlang\.apply\/2.*String\.Chars\.Integer\.to_string\/1)s
    end)
  end

  test "Module matching", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Eprof.run(["--matching", "Enum", "-e", @expr])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1)
    end)
  end

  test "Module.function matching", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Eprof.run(["--matching", "Enum.each", "-e", @expr])
             end) =~ ~r(anonymous fn\/3 in Enum\.each\/2)
    end)
  end

  test "Module.function/arity matching", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Eprof.run(["--matching", "Enum.each/8", "-e", @expr])
             end) =~ ~r(Profile done over 0 matching functions)
    end)
  end

  test "errors on missing files", context do
    in_tmp(context.test, fn ->
      message = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Eprof.run(["-r", "non-existent"]) end)
      end

      message = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Eprof.run(["-pr", "non-existent"]) end)
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        capture_io(fn -> Eprof.run(["non-existent"]) end)
      end

      File.mkdir_p!("lib")

      assert_raise Mix.Error, "No such file: lib", fn ->
        capture_io(fn -> Eprof.run(["lib"]) end)
      end
    end)
  end

  test "warmup", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Eprof.run(["-e", @expr])
             end) =~ "Warmup..."

      refute capture_io(fn ->
               Eprof.run(["-e", @expr, "--no-warmup"])
             end) =~ "Warmup..."
    end)
  end
end
