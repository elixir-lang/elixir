Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Profile.CprofTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  alias Mix.Tasks.Profile.Cprof

  @expr "Enum.each(1..5, &String.Chars.Integer.to_string/1)"

  test "profiles evaluated expression", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Cprof.run(["-e", @expr])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1 *\d)
    end)
  end

  test "profiles the script", context do
    in_tmp(context.test, fn ->
      profile_script_name = "profile_script.ex"

      File.write!(profile_script_name, @expr)

      assert capture_io(fn ->
               Cprof.run([profile_script_name])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1 *\d)
    end)
  end

  test "filters based on limit", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Cprof.run(["--limit", "5", "-e", @expr])
             end) =~ ~r(:erlang\.trace_pattern\/3 *\d)
    end)
  end

  test "filters based on module", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Cprof.run(["--module", "Enum", "-e", @expr])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1 *\d)
    end)
  end

  test "Module matching", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Cprof.run(["--matching", "Enum", "-e", @expr])
             end) =~ ~r(String\.Chars\.Integer\.to_string\/1 *\d)
    end)
  end

  test "Module.function matching", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Cprof.run(["--matching", "Enum.each", "-e", @expr])
             end) =~ ~r(anonymous fn\/3 in Enum\.each\/2 *\d)
    end)
  end

  test "Module.function/arity matching", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Cprof.run(["--matching", "Enum.each/8", "-e", @expr])
             end) =~ ~r(Profile done over 0 matching functions)
    end)
  end

  test "errors on missing files", context do
    in_tmp(context.test, fn ->
      msg = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, msg, fn ->
        capture_io(fn -> Cprof.run(["-r", "non-existent"]) end)
      end

      assert_raise Mix.Error, msg, fn ->
        capture_io(fn -> Cprof.run(["-pr", "non-existent"]) end)
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        capture_io(fn -> Cprof.run(["non-existent"]) end)
      end

      File.mkdir_p!("lib")

      assert_raise Mix.Error, "No such file: lib", fn ->
        capture_io(fn -> Cprof.run(["lib"]) end)
      end
    end)
  end

  test "warmup", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Cprof.run(["-e", @expr])
             end) =~ "Warmup..."

      refute capture_io(fn ->
               Cprof.run(["-e", "Enum.each(1..5, fn(_) -> MapSet.new end)", "--no-warmup"])
             end) =~ "Warmup..."
    end)
  end
end
