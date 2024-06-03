Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Profile.TprofTest do
  use MixTest.Case

  import ExUnit.CaptureIO
  alias Mix.Tasks.Profile.Tprof

  # TODO remove once we require Erlang/OTP 27+
  @moduletag skip: System.otp_release() < "27"

  @expr "Enum.each(1..5, &String.Chars.Integer.to_string/1)"

  test "profiles evaluated expression", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["-e", @expr])
             end) =~ ~r/String\.Chars\.Integer\.to_string\/1\s+\d/
    end)
  end

  test "profiles evaluated expression in multiple processes", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["-e", "spawn(fn -> #{@expr} end)"])
             end) =~ ~r/String\.Chars\.Integer\.to_string\/1\s+\d/
    end)
  end

  test "profiles the script", context do
    in_tmp(context.test, fn ->
      profile_script_name = "profile_script.ex"
      File.write!(profile_script_name, @expr)

      assert capture_io(fn ->
               Tprof.run([profile_script_name])
             end) =~ ~r/String\.Chars\.Integer\.to_string\/1\s+\d/
    end)
  end

  test "filters based on calls count", context do
    in_tmp(context.test, fn ->
      result =
        capture_io(fn ->
          Tprof.run(["--calls", "5", "-e", @expr])
        end)

      assert result =~ "\nString.Chars.Integer.to_string/1"
      refute result =~ "\nEnum.each/2"
    end)
  end

  test "filters based on time", context do
    in_tmp(context.test, fn ->
      result =
        capture_io(fn ->
          Tprof.run(["--time", "50", "-e", @expr])
        end)

      refute result =~ "\nEnum.each/2"
    end)
  end

  test "filters based on memory", context do
    in_tmp(context.test, fn ->
      result =
        capture_io(fn ->
          Tprof.run(["--type", "memory", "--memory", "10", "-e", @expr])
        end)

      assert result =~ "\n:erlang.integer_to_binary/1"
      refute result =~ "\nEnum.each/2"
    end)
  end

  test "sorts based on calls count", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["--sort", "calls", "-e", @expr])
             end) =~ ~r/\nEnum\.each\/2.*\nString\.Chars\.Integer\.to_string\/1/s
    end)
  end

  test "sorts based on memory usage", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["--type", "memory", "--sort", "calls", "-e", @expr])
             end) =~ ~r/\nEnum\.each\/2.*\n:erlang\.integer_to_binary\/1/s
    end)
  end

  test "sorts based on memory per call", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["--type", "memory", "--sort", "per_call", "-e", @expr])
             end) =~ ~r/\n:erlang\.integer_to_binary\/1.*\nEnum\.each\/2/s
    end)
  end

  test "aggregates totals over all processes", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["--report", "total", "-e", @expr])
             end) =~ "Profile results over all processes"
    end)
  end

  test "Module matching", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Tprof.run(["--matching", "Enum", "-e", @expr])
             end) =~ ~r/String\.Chars\.Integer\.to_string\/1/
    end)
  end

  test "Module.function matching", context do
    in_tmp(context.test, fn ->
      refute capture_io(fn ->
               Tprof.run(["--matching", "Enum.each", "-e", @expr])
             end) =~ ~r/anonymous fn\/3 in Enum\.each\/2/
    end)
  end

  test "Module.function/arity matching", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["--matching", "Enum.each/8", "-e", @expr])
             end) =~ ~r/Profile done over 0 matching functions/
    end)
  end

  test "errors on missing files", context do
    in_tmp(context.test, fn ->
      message = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Tprof.run(["-r", "non-existent"]) end)
      end

      message = "No files matched pattern \"non-existent\" given to --require"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Tprof.run(["-pr", "non-existent"]) end)
      end

      assert_raise Mix.Error, "No such file: non-existent", fn ->
        capture_io(fn -> Tprof.run(["non-existent"]) end)
      end

      File.mkdir_p!("lib")

      assert_raise Mix.Error, "No such file: lib", fn ->
        capture_io(fn -> Tprof.run(["lib"]) end)
      end
    end)
  end

  test "warmup", context do
    in_tmp(context.test, fn ->
      assert capture_io(fn ->
               Tprof.run(["-e", @expr])
             end) =~ "Warmup..."

      refute capture_io(fn ->
               Tprof.run(["-e", @expr, "--no-warmup"])
             end) =~ "Warmup..."
    end)
  end

  test "errors on incompatible options", context do
    in_tmp(context.test, fn ->
      message = "Incompatible sort option :memory with type :time"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Tprof.run(["-e", @expr, "--sort", "memory"]) end)
      end

      message = "Incompatible sort option :time with type :calls"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Tprof.run(["-e", @expr, "--type", "calls", "--sort", "time"]) end)
      end

      message = "Incompatible use of memory option with type :time"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Tprof.run(["-e", @expr, "--time", "1", "--memory", "2"]) end)
      end

      message = "Incompatible use of time option with type :calls"

      assert_raise Mix.Error, message, fn ->
        capture_io(fn -> Tprof.run(["-e", @expr, "--type", "calls", "--time", "1"]) end)
      end
    end)
  end

  describe ".profile/2" do
    test "returns the return value of the function call" do
      capture_io(fn ->
        assert 42 == Tprof.profile(fn -> 42 end)
      end)
    end
  end
end
