defmodule Logger.ErrorHandlerTest do
  use Logger.Case
  @moduletag :error_logger

  test "add_translator/1 and remove_translator/1" do
    defmodule CustomTranslator do
      def t(:debug, :info, :format, {'hello: ~p', [:ok]}) do
        :skip
      end

      def t(:debug, :info, :format, {'world: ~p', [:ok]}) do
        {:ok, "rewritten"}
      end

      def t(_, _, _, _) do
        :none
      end
    end

    assert Logger.add_translator({CustomTranslator, :t})

    assert capture_log(fn ->
             :error_logger.info_msg('hello: ~p', [:ok])
           end) == ""

    assert capture_log(fn ->
             :error_logger.info_msg('world: ~p', [:ok])
           end) =~ "\[info\]  rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "survives after crashes" do
    expected_result =
      "[error] :gen_event handler Logger.ErrorHandler installed in :error_logger terminating\n" <>
        "** (exit) an exception was raised:"

    assert capture_log(fn ->
             :error_logger.info_msg("~p~n", [])
             wait_for_handler(:error_logger, Logger.ErrorHandler)
           end) =~ expected_result

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
