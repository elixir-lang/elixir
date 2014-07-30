defmodule LoggerTest do
  use Logger.Case
  require Logger

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
    end) =~ "\[info\] rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "add_backend/1 and remove_backend/1" do
    assert :ok = Logger.remove_backend(:console)
    assert Logger.remove_backend(:console) ==
           {:error, :not_found}

    assert capture_log(fn ->
      assert Logger.debug("hello", []) == :ok
    end) == ""

    assert {:ok, pid} = Logger.add_backend(:console)
    assert Logger.add_backend(:console) ==
           {:error, {:already_started, pid}}
  end

  test "level/0" do
    assert Logger.level == :debug
  end

  test "compare_levels/2" do
    assert Logger.compare_levels(:debug, :debug) == :eq
    assert Logger.compare_levels(:debug, :info)  == :lt
    assert Logger.compare_levels(:debug, :warn)  == :lt
    assert Logger.compare_levels(:debug, :error) == :lt

    assert Logger.compare_levels(:info, :debug) == :gt
    assert Logger.compare_levels(:info, :info)  == :eq
    assert Logger.compare_levels(:info, :warn)  == :lt
    assert Logger.compare_levels(:info, :error) == :lt

    assert Logger.compare_levels(:warn, :debug) == :gt
    assert Logger.compare_levels(:warn, :info)  == :gt
    assert Logger.compare_levels(:warn, :warn)  == :eq
    assert Logger.compare_levels(:warn, :error) == :lt

    assert Logger.compare_levels(:error, :debug) == :gt
    assert Logger.compare_levels(:error, :info)  == :gt
    assert Logger.compare_levels(:error, :warn)  == :gt
    assert Logger.compare_levels(:error, :error) == :eq
  end

  test "debug/2" do
    assert capture_log(fn ->
      assert Logger.debug("hello", []) == :ok
    end) =~ msg("[debug] hello")

    assert capture_log(:info, fn ->
      assert Logger.debug("hello", []) == :ok
    end) == ""
  end

  test "info/2" do
    assert capture_log(fn ->
      assert Logger.info("hello", []) == :ok
    end) =~ msg("[info] hello")

    assert capture_log(:warn, fn ->
      assert Logger.info("hello", []) == :ok
    end) == ""
  end

  test "warn/2" do
    assert capture_log(fn ->
      assert Logger.warn("hello", []) == :ok
    end) =~ msg("[warn] hello")

    assert capture_log(:error, fn ->
      assert Logger.warn("hello", []) == :ok
    end) == ""
  end

  test "error/2" do
    assert capture_log(fn ->
      assert Logger.error("hello", []) == :ok
    end) =~ msg("[error] hello")
  end

  test "remove unused calls at compile time" do
    Logger.configure(compile_time_purge_level: :info)

    defmodule Sample do
      def debug do
        Logger.debug "hello"
      end

      def info do
        Logger.info "hello"
      end
    end

    assert capture_log(fn ->
      assert Sample.debug == :ok
    end) == ""

    assert capture_log(fn ->
      assert Sample.info == :ok
    end) =~ msg("[info] hello")
  after
    Logger.configure(compile_time_purge_level: :debug)
  end

  test "log/2 truncates messages" do
    Logger.configure(truncate: 4)
    assert capture_log(fn ->
      Logger.log(:debug, "hello")
    end) =~ "hell (truncated)"
  after
    Logger.configure(truncate: 8096)
  end

  test "log/2 fails when the application is off" do
    logger = Process.whereis(Logger)
    Process.unregister(Logger)

    try do
      assert_raise RuntimeError,
                   "Cannot log messages, the :logger application is not running", fn ->
        Logger.log(:debug, "hello")
      end
    after
      Process.register(logger, Logger)
    end
  end

  test "Logger.Config survives Logger exit" do
    Process.whereis(Logger)
      |> Process.exit(:kill)
    wait_for_logger()
    wait_for_handler(Logger, Logger.Config)
  end

  test "Logger.Config can restart the application" do
    Application.put_env(:logger, :backends, [])
    Logger.Config.restart()

    assert capture_log(fn ->
      assert Logger.debug("hello", []) == :ok
    end) == ""

    assert {:ok, pid} = Logger.add_backend(:console)
    assert Logger.add_backend(:console) ==
           {:error, {:already_started, pid}}
  end
end
