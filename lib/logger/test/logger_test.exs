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

  test "add_backend/1 with unknown backend" do
    assert {:error, {{:EXIT, {:undef, [_ | _]}}, _}} =
             Logger.add_backend({UnknownBackend, :hello})
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

    Logger.configure(level: :all)
    assert Logger.level() == :all

    Logger.configure(level: :info)
    assert Logger.level() == :info

    Logger.configure(level: :notice)
    assert Logger.level() == :notice

    Logger.configure(level: :warn)
    assert Logger.level() == :warning

    Logger.configure(level: :warning)
    assert Logger.level() == :warning

    Logger.configure(level: :error)
    assert Logger.level() == :error

    Logger.configure(level: :critical)
    assert Logger.level() == :critical

    Logger.configure(level: :alert)
    assert Logger.level() == :alert

    Logger.configure(level: :emergency)
    assert Logger.level() == :emergency

    Logger.configure(level: :none)
    assert Logger.level() == :none
  after
    Logger.configure(level: :debug)
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

  describe "log with function" do
    test "supports iolist" do
      fun = fn -> ["ok", ?:, 'example'] end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fun, application: nil, module: FunctionTest) == :ok
             end) =~ msg("module=FunctionTest [info]  ok:example")
    end

    test "supports binaries" do
      fun = fn -> "ok:example" end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fun, application: nil, module: FunctionTest) == :ok
             end) =~ msg("module=FunctionTest [info]  ok:example")
    end
  end

  describe "report logging" do
    test "supports maps" do
      assert capture_log(fn ->
               assert Logger.bare_log(:info, %{foo: 10}, application: nil, module: FunctionTest) ==
                        :ok
             end) =~ msg("module=FunctionTest [info]  [foo: 10]")
    end

    test "supports keyword" do
      assert capture_log(fn ->
               assert Logger.bare_log(:info, foo: 10) == :ok
             end) =~ msg("[info]  [foo: 10]")
    end

    test "supports custom report_cb" do
      report_cb = fn %{foo: foo} -> {"Foo is ~B", [foo]} end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, %{foo: 10}, report_cb: report_cb) == :ok
             end) =~ msg("[info]  Foo is 10")

      report_cb = fn %{foo: foo}, _opts -> "Foo is #{foo}" end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, %{foo: 20}, report_cb: report_cb) == :ok
             end) =~ msg("[info]  Foo is 20")
    end

    test "support function that return report" do
      assert capture_log(fn ->
               assert Logger.bare_log(:info, fn -> %{foo: 10} end) == :ok
             end) =~ msg("[info]  [foo: 10]")

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fn -> {%{foo: 10}, []} end) == :ok
             end) =~ msg("[info]  [foo: 10]")

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fn -> {[foo: 10], []} end) == :ok
             end) =~ msg("[info]  [foo: 10]")
    end
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
    assert Logger.compare_levels(:debug, :notice) == :lt
    assert Logger.compare_levels(:debug, :warn) == :lt
    assert Logger.compare_levels(:debug, :warning) == :lt
    assert Logger.compare_levels(:debug, :error) == :lt
    assert Logger.compare_levels(:debug, :critical) == :lt
    assert Logger.compare_levels(:debug, :alert) == :lt
    assert Logger.compare_levels(:debug, :emergency) == :lt

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

  describe "levels" do
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

      assert capture_log(:notice, fn ->
               assert Logger.info("hello", []) == :ok
             end) == ""

      assert capture_log(:notice, fn ->
               assert Logger.info(raise("not invoked"), []) == :ok
             end) == ""
    end

    test "warning/2" do
      assert capture_log(fn ->
               assert Logger.warning("hello", []) == :ok
             end) =~ msg_with_meta("[warn]  hello")

      assert capture_log(:error, fn ->
               assert Logger.warning("hello", []) == :ok
             end) == ""

      assert capture_log(:error, fn ->
               assert Logger.warning(raise("not invoked"), []) == :ok
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

      assert capture_log(:critical, fn ->
               assert Logger.error("hello", []) == :ok
             end) == ""

      assert capture_log(:critical, fn ->
               assert Logger.error(raise("not invoked"), []) == :ok
             end) == ""
    end

    test "critical/2" do
      assert capture_log(fn ->
               assert Logger.critical("hello", []) == :ok
             end) =~ msg_with_meta("[error] hello")

      assert capture_log(:alert, fn ->
               assert Logger.critical("hello", []) == :ok
             end) == ""

      assert capture_log(:alert, fn ->
               assert Logger.critical(raise("not invoked"), []) == :ok
             end) == ""
    end

    test "alert/2" do
      assert capture_log(fn ->
               assert Logger.alert("hello", []) == :ok
             end) =~ msg_with_meta("[error] hello")

      assert capture_log(:emergency, fn ->
               assert Logger.alert("hello", []) == :ok
             end) == ""

      assert capture_log(:emergency, fn ->
               assert Logger.alert(raise("not invoked"), []) == :ok
             end) == ""
    end

    test "emergency/2" do
      assert capture_log(fn ->
               assert Logger.emergency("hello", []) == :ok
             end) =~ msg_with_meta("[error] hello")

      assert capture_log(:none, fn ->
               assert Logger.emergency("hello", []) == :ok
             end) == ""

      assert capture_log(:none, fn ->
               assert Logger.emergency(raise("not invoked"), []) == :ok
             end) == ""
    end
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
        Logger.warning("warning_filter")
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

  test "log/2 does not fails when the logger is off" do
    logger = Process.whereis(Logger)
    Process.unregister(Logger)

    try do
      assert Logger.log(:debug, "hello") == :ok
    after
      Process.register(logger, Logger)
    end
  end

  test "log/2 prunes bad Unicode chars" do
    assert capture_log(fn ->
             assert Logger.log(:debug, "he" <> <<185>> <> "lo") == :ok
           end) =~ "he�lo"
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

  test "starts the application with custom options" do
    Logger.App.stop()
    Application.put_env(:logger, :start_options, spawn_opt: [priority: :high])
    Application.start(:logger)
    assert Process.info(Process.whereis(Logger), :priority) == {:priority, :high}
  after
    Application.put_env(:logger, :start_options, [])
    Logger.App.stop()
    Application.start(:logger)
  end

  test "starts the application with warn level" do
    Logger.App.stop()
    assert %{level: :notice} = :logger.get_primary_config()
    Application.put_env(:logger, :level, :warn)
    Application.start(:logger)
    assert %{level: :warning} = :logger.get_primary_config()
  after
    Application.delete_env(:logger, :level)
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

    assert {:ok, %{config: log_data}} = :logger.get_handler_config(Logger)
    assert log_data.utc_log == true
    assert log_data.truncate == 4048
  after
    Logger.configure(sync_threshold: 20)
    Logger.configure(truncate: 8096)
    Logger.configure(utc_log: false)
    Logger.configure(discard_threshold: 500)
    Logger.configure(translator_inspect_opts: [])
  end

  test "logs when discarding messages" do
    assert :ok = Logger.configure(discard_threshold: 5)

    log =
      capture_log(fn ->
        :sys.suspend(Logger)
        for _ <- 1..10, do: Logger.warning("warning!")
        :sys.resume(Logger)
        Logger.flush()

        # This is a private message but the simplest way to test this functionality
        send(Logger, {Logger.Config, :update_counter})

        # We need to flush twice. Once for the send and another for the log in send
        Logger.flush()
        Logger.flush()
      end)

    assert log =~ ~r"\[warn\]  Attempted to log \d+ messages, which is above :discard_threshold"
    assert log =~ ~r"\[warn\]  Attempted to log \d+ messages, which is below :discard_threshold"
  after
    :sys.resume(Logger)
    assert :ok = Logger.configure(discard_threshold: 500)
  end

  test "restarts Logger.Config on Logger exits" do
    Process.whereis(Logger) |> Process.exit(:kill)
    wait_for_logger()
    wait_for_handler(Logger, Logger.Config)
  end

  test "updates config on config_change/3" do
    :ok = Logger.configure(level: :debug)

    try do
      assert Logger.App.config_change([level: :error], [], []) === :ok
      assert Logger.level() === :error
    after
      Logger.configure(level: :debug)
    end
  end

  describe "OTP integration" do
    test "changes level in both" do
      assert :logger.get_primary_config().level == :debug
      Logger.configure(level: :error)
      assert :logger.get_primary_config().level == :error
    after
      Logger.configure(level: :debug)
    end

    test "supports module level" do
      :logger.set_module_level(__MODULE__, :none)
      assert capture_log(fn -> Logger.info("hello") end) == ""
      :logger.set_module_level(__MODULE__, :all)
      assert capture_log(fn -> Logger.info("hello") end) =~ "hello"
    after
      :logger.unset_module_level(__MODULE__)
    end

    test "maps Erlang levels" do
      :logger.set_primary_config(:level, :notice)
      assert capture_log(fn -> Logger.info("hello") end) =~ "hello"

      :logger.set_primary_config(:level, :notice)
      assert Logger.level() == :notice

      :logger.set_primary_config(:level, :emergency)
      assert Logger.level() == :emergency
    after
      Logger.configure(level: :debug)
    end

    test "metadata is synchronised" do
      Logger.metadata(foo: "bar")

      assert Map.new(Logger.metadata()) == :logger.get_process_metadata()
      :logger.set_process_metadata(%{bar: "foo"})

      assert Map.new(Logger.metadata()) == :logger.get_process_metadata()
    end
  end
end
