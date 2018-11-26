defmodule LoggerTest do
  use Logger.Case
  require Logger

  setup_all do
    Logger.configure_backend(:console, metadata: [:application, :module])
    on_exit(fn -> Logger.configure_backend(:console, metadata: []) end)
  end

  defmodule MyBackend do
    @behaviour :gen_event

    def init({MyBackend, :hello}) do
      {:ok, :hello}
    end

    def handle_event(_event, state) do
      {:ok, state}
    end

    def handle_call(:error, _) do
      raise "oops"
    end

    def handle_info(_msg, state) do
      {:ok, state}
    end

    def code_change(_old_vsn, state, _extra) do
      {:ok, state}
    end

    def terminate(_reason, _state) do
      :ok
    end
  end

  defp msg_with_meta(text) do
    msg("module=LoggerTest #{text}")
  end

  test "add_backend/1 and remove_backend/1" do
    assert :ok = Logger.remove_backend(:console)
    assert Logger.remove_backend(:console) == {:error, :not_found}

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) == ""

    assert {:ok, _pid} = Logger.add_backend(:console)
    assert Logger.add_backend(:console) == {:error, :already_present}
  end

  test "add_backend/1 with {module, id}" do
    assert {:ok, _} = Logger.add_backend({MyBackend, :hello})
    assert {:error, :already_present} = Logger.add_backend({MyBackend, :hello})
    assert :ok = Logger.remove_backend({MyBackend, :hello})
  end

  test "logs or writes to stderr on failed backends" do
    assert {:ok, _} = Logger.add_backend({MyBackend, :hello})

    assert capture_log(fn ->
             :gen_event.call(Logger, {MyBackend, :hello}, :error)
             wait_for_handler(Logger, {MyBackend, :hello})
           end) =~
             ":gen_event handler {LoggerTest.MyBackend, :hello} installed in Logger terminating"

    assert :ok = Logger.remove_backend(:console)

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             :gen_event.call(Logger, {MyBackend, :hello}, :error)
             wait_for_handler(Logger, {MyBackend, :hello})
           end) =~
             ":gen_event handler {LoggerTest.MyBackend, :hello} installed in Logger terminating"

    # Flush logs before reattaching to avoid OTP reports
    Logger.flush()
  after
    Logger.remove_backend({MyBackend, :hello})
    Logger.add_backend(:console)
  end

  test "level/0" do
    assert Logger.level() == :debug
  end

  test "process metadata" do
    assert Logger.metadata(data: true) == :ok
    assert Logger.metadata() == [data: true]
    assert Logger.metadata(data: true) == :ok
    assert Logger.metadata() == [data: true]
    assert Logger.metadata(meta: 1) == :ok
    metadata = Logger.metadata()
    assert Enum.sort(metadata) == [data: true, meta: 1]
    assert Logger.metadata(data: nil) == :ok
    assert Logger.metadata() == [meta: 1]

    assert Logger.reset_metadata(meta: 2) == :ok
    assert Logger.metadata() == [meta: 2]
    assert Logger.reset_metadata(data: true, app: nil) == :ok
    assert Logger.metadata() == [data: true]
    assert Logger.reset_metadata() == :ok
    assert Logger.metadata() == []
  end

  test "metadata merge" do
    assert Logger.metadata(module: Sample) == :ok

    assert capture_log(fn ->
             assert Logger.bare_log(:info, "ok", application: nil, module: LoggerTest) == :ok
           end) =~ msg("module=LoggerTest [info]  ok")
  end

  test "metadata compile-time merge" do
    assert Logger.metadata(module: Sample) == :ok

    assert capture_log(fn ->
             assert Logger.log(:info, "ok", application: nil, module: CustomTest) == :ok
           end) =~ msg("module=CustomTest [info]  ok")
  end

  test "metadata merge when the argument function returns metadata" do
    assert Logger.metadata(module: Sample) == :ok

    fun = fn -> {"ok", [module: "Function"]} end

    assert capture_log(fn ->
             assert Logger.bare_log(:info, fun, application: nil, module: LoggerTest) == :ok
           end) =~ msg("module=Function [info]  ok")
  end

  test "enable/1 and disable/1" do
    assert Logger.metadata([]) == :ok

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) =~ msg_with_meta("[debug] hello")

    assert Logger.disable(self()) == :ok

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) == ""

    assert Logger.metadata([]) == :ok

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) == ""

    assert Logger.enable(self()) == :ok

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) =~ msg_with_meta("[debug] hello")
  end

  test "compare_levels/2" do
    assert Logger.compare_levels(:debug, :debug) == :eq
    assert Logger.compare_levels(:debug, :info) == :lt
    assert Logger.compare_levels(:debug, :warn) == :lt
    assert Logger.compare_levels(:debug, :error) == :lt

    assert Logger.compare_levels(:info, :debug) == :gt
    assert Logger.compare_levels(:info, :info) == :eq
    assert Logger.compare_levels(:info, :warn) == :lt
    assert Logger.compare_levels(:info, :error) == :lt

    assert Logger.compare_levels(:warn, :debug) == :gt
    assert Logger.compare_levels(:warn, :info) == :gt
    assert Logger.compare_levels(:warn, :warn) == :eq
    assert Logger.compare_levels(:warn, :error) == :lt

    assert Logger.compare_levels(:error, :debug) == :gt
    assert Logger.compare_levels(:error, :info) == :gt
    assert Logger.compare_levels(:error, :warn) == :gt
    assert Logger.compare_levels(:error, :error) == :eq
  end

  test "debug/2" do
    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) =~ msg_with_meta("[debug] hello")

    assert capture_log(:info, fn ->
             assert Logger.debug("hello", []) == :ok
           end) == ""

    assert capture_log(:info, fn ->
             assert Logger.debug(raise("not invoked"), []) == :ok
           end) == ""
  end

  test "info/2" do
    assert capture_log(fn ->
             assert Logger.info("hello", []) == :ok
           end) =~ msg_with_meta("[info]  hello")

    assert capture_log(:warn, fn ->
             assert Logger.info("hello", []) == :ok
           end) == ""

    assert capture_log(:warn, fn ->
             assert Logger.info(raise("not invoked"), []) == :ok
           end) == ""
  end

  test "warn/2" do
    assert capture_log(fn ->
             assert Logger.warn("hello", []) == :ok
           end) =~ msg_with_meta("[warn]  hello")

    assert capture_log(:error, fn ->
             assert Logger.warn("hello", []) == :ok
           end) == ""

    assert capture_log(:error, fn ->
             assert Logger.warn(raise("not invoked"), []) == :ok
           end) == ""
  end

  test "error/2" do
    assert capture_log(fn ->
             assert Logger.error("hello", []) == :ok
           end) =~ msg_with_meta("[error] hello")
  end

  test "remove unused calls at compile time based on the level" do
    Logger.configure(compile_time_purge_level: :info)

    defmodule PurgeLevel do
      def debug do
        Logger.debug("hello")
      end

      def info do
        Logger.info("hello")
      end
    end

    assert capture_log(fn ->
             assert PurgeLevel.debug() == :ok
           end) == ""

    assert capture_log(fn ->
             assert PurgeLevel.info() == :ok
           end) =~ msg("module=LoggerTest.PurgeLevel [info]  hello")
  after
    Logger.configure(compile_time_purge_level: :debug)
  end

  test "remove unused calls at compile time based on matching metadata" do
    Logger.configure(
      compile_time_application: :sample_app,
      compile_time_purge_matching: [
        [module: LoggerTest.PurgeMatching, function: "two_filters/0"],
        [function: "one_filter/0"],
        [custom: true],
        [function: "level_filter/0", level_lower_than: :warn],
        [application: :sample_app, level_lower_than: :info]
      ]
    )

    defmodule PurgeMatching do
      def two_filters do
        Logger.debug("two_filters")
      end

      def one_filter do
        Logger.debug("one_filter")
      end

      def custom_filters do
        Logger.debug("custom_filters", custom: true)
      end

      def level_filter do
        Logger.info("info_filter")
        Logger.warn("warn_filter")
      end

      def works do
        Logger.info("works")
      end

      def log(level, metadata \\ []) do
        Logger.log(level, "ok", metadata)
      end
    end

    assert capture_log(fn -> assert PurgeMatching.works() == :ok end) =~ "works"
    assert capture_log(fn -> assert PurgeMatching.one_filter() == :ok end) == ""
    assert capture_log(fn -> assert PurgeMatching.two_filters() == :ok end) == ""
    assert capture_log(fn -> assert PurgeMatching.custom_filters() == :ok end) == ""
    assert capture_log(fn -> assert PurgeMatching.level_filter() == :ok end) =~ "warn_filter"
    refute capture_log(fn -> assert PurgeMatching.level_filter() == :ok end) =~ "info_filter"

    capture_log(fn -> assert PurgeMatching.log(:info) == :ok end)
    capture_log(fn -> assert PurgeMatching.log(:debug) == :ok end)
  after
    Logger.configure(compile_time_application: nil)
    Logger.configure(compile_time_purge_matching: [])
  end

  test "unused variable warnings suppressed when we remove macros from the AST" do
    Logger.configure(compile_time_purge_level: :info)

    # This should not warn, even if the Logger call is purged from the AST.
    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             defmodule Unused do
               require Logger

               def hello(a, b, c) do
                 Logger.debug(["a: ", inspect(a), ", b: ", inspect(b)], c: c)
               end
             end
           end) == ""

    assert LoggerTest.Unused.hello(1, 2, 3) == :ok
  after
    Logger.configure(compile_time_purge_level: :debug)
  end

  test "set application metadata at compile time" do
    Logger.configure(compile_time_application: nil)

    defmodule SampleNoApp do
      def info do
        Logger.info("hello")
      end
    end

    assert capture_log(fn ->
             assert SampleNoApp.info() == :ok
           end) =~ msg("module=LoggerTest.SampleNoApp [info]  hello")

    Logger.configure(compile_time_application: :sample_app)

    defmodule SampleApp do
      def info do
        Logger.info("hello")
      end
    end

    assert capture_log(fn ->
             assert SampleApp.info() == :ok
           end) =~ msg("application=sample_app module=LoggerTest.SampleApp [info]  hello")
  after
    Logger.configure(compile_time_application: nil)
  end

  test "log/2 truncates messages" do
    Logger.configure(truncate: 4)
    assert capture_log(fn -> Logger.log(:debug, "hello") end) =~ "hell (truncated)"
  after
    Logger.configure(truncate: 8096)
  end

  test "log/2 with to_string/1 conversion" do
    Logger.configure(truncate: 4)
    assert capture_log(fn -> Logger.log(:debug, :hello) end) =~ "hell (truncated)"
  after
    Logger.configure(truncate: 8096)
  end

  test "log/2 does not fails when the logger is off" do
    logger = Process.whereis(Logger)
    Process.unregister(Logger)

    try do
      assert Logger.log(:debug, "hello") == {:error, :noproc}
    after
      Process.register(logger, Logger)
    end
  end

  test "log/2 prunes bad unicode chars" do
    assert capture_log(fn ->
             assert Logger.log(:debug, "he" <> <<185>> <> "lo") == :ok
           end) =~ "he�lo"
  end

  test "logging something that is not a binary or chardata fails right away" do
    assert_raise Protocol.UndefinedError, "protocol String.Chars not implemented for %{}", fn ->
      Logger.log(:debug, %{})
    end

    message =
      "cannot truncate chardata because it contains something that is not valid chardata: %{}"

    # Something that looks like chardata but then inside isn't still raises an error, but a
    # different one.
    assert_raise ArgumentError, message, fn ->
      Logger.log(:debug, [%{}])
    end
  end

  test "stops the application silently" do
    Application.put_env(:logger, :backends, [])
    Logger.App.stop()
    Application.start(:logger)

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) == ""

    assert {:ok, _} = Logger.add_backend(:console)
    assert Logger.add_backend(:console) == {:error, :already_present}
  after
    Application.put_env(:logger, :backends, [:console])
    Logger.App.stop()
    Application.start(:logger)
  end

  test "configure/1 sets options" do
    Logger.configure(sync_threshold: 10)
    Logger.configure(truncate: 4048)
    Logger.configure(utc_log: true)
    Logger.configure(discard_threshold: 10_000)
    Logger.configure(translator_inspect_opts: [limit: 3])

    assert Application.get_env(:logger, :sync_threshold) == 10
    assert Application.get_env(:logger, :utc_log) == true
    assert Application.get_env(:logger, :truncate) == 4048
    assert Application.get_env(:logger, :discard_threshold) == 10_000
    assert Application.get_env(:logger, :translator_inspect_opts) == [limit: 3]

    logger_data = Logger.Config.__data__()
    assert logger_data.truncate == 4048
    assert logger_data.utc_log == true
  after
    Logger.configure(sync_threshold: 20)
    Logger.configure(truncate: 8096)
    Logger.configure(utc_log: false)
    Logger.configure(discard_threshold: 500)
    Logger.configure(translator_inspect_opts: [])
  end
end
