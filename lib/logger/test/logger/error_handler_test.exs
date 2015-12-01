defmodule Logger.ErrorHandlerTest do
  use Logger.Case

  test "survives after crashes" do
    assert capture_log(fn ->
      :error_logger.info_msg("~p~n", [])
      wait_for_handler(:error_logger, Logger.ErrorHandler)
    end) =~ "[error] GenEvent handler Logger.ErrorHandler installed at :error_logger\n" <>
            "** (exit) an exception was raised:"
    assert error_log(:info_msg, "~p~n", [:hello]) =~ msg("[info]  :hello")
  end

  test "survives after Logger exit" do
    Process.whereis(Logger) |> Process.exit(:kill)
    wait_for_logger()
    wait_for_handler(:error_logger, Logger.ErrorHandler)
    assert error_log(:info_msg, "~p~n", [:hello]) =~ msg("[info]  :hello")
  end

  test "formats error_logger info message" do
    assert error_log(:info_msg, "hello", []) =~ msg("[info]  hello")
    assert error_log(:info_msg, "~p~n", [:hello]) =~ msg("[info]  :hello")
  end

  test "formats error_logger info report" do
    assert error_log(:info_report, "hello") =~ msg("[info]  \"hello\"")
    assert error_log(:info_report, :hello) =~ msg("[info]  :hello")
    assert error_log(:info_report, :special, :hello) == ""
  end

  test "formats error_logger error message" do
    assert error_log(:error_msg, "hello", []) =~ msg("[error] hello")
    assert error_log(:error_msg, "~p~n", [:hello]) =~ msg("[error] :hello")
  end

  test "formats error_logger error report" do
    assert error_log(:error_report, "hello") =~ msg("[error] \"hello\"")
    assert error_log(:error_report, :hello) =~ msg("[error] :hello")
    assert error_log(:error_report, :special, :hello) == ""
  end

  test "formats error_logger warning message" do
    assert error_log(:warning_msg, "hello", []) =~ msg("[warn]  hello")
    assert error_log(:warning_msg, "~p~n", [:hello]) =~ msg("[warn]  :hello")
  end

  test "formats error_logger warning report" do
    assert error_log(:warning_report, "hello") =~ msg("[warn]  \"hello\"")
    assert error_log(:warning_report, :hello) =~ msg("[warn]  :hello")
    assert error_log(:warning_report, :special, :hello) == ""
  end

  defp error_log(fun, format) do
    do_error_log(fun, [format])
  end

  defp error_log(fun, format, args) do
    do_error_log(fun, [format, args])
  end

  defp do_error_log(fun, args) do
    capture_log(fn -> apply(:error_logger, fun, args) end)
  end
end
