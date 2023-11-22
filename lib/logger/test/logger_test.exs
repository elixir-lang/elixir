defmodule LoggerTest do
  use Logger.Case
  require Logger

  @moduletag formatter: [metadata: [:application, :module]]

  setup tags do
    :logger.update_handler_config(:default, :formatter, Logger.default_formatter(tags.formatter))

    on_exit(fn ->
      :logger.update_handler_config(:default, :formatter, Logger.default_formatter())
    end)
  end

  defp msg_with_meta(text) do
    msg("module=LoggerTest #{text}")
  end

  test "levels/0" do
    assert [_ | _] = Logger.levels()
    assert :info in Logger.levels()
  end

  test "level/0" do
    assert Logger.level() == :debug

    Logger.configure(level: :all)
    assert Logger.level() == :all

    Logger.configure(level: :info)
    assert Logger.level() == :info

    Logger.configure(level: :notice)
    assert Logger.level() == :notice

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

  test "per-module levels" do
    defmodule PerModuleLevels do
      def debug, do: Logger.debug("debug_msg")

      def error, do: Logger.error("error_msg")
    end

    assert capture_log(fn -> assert PerModuleLevels.debug() == :ok end) =~ "debug_msg"
    assert capture_log(fn -> assert PerModuleLevels.error() == :ok end) =~ "error_msg"

    Logger.put_module_level(PerModuleLevels, :error)

    assert capture_log(fn -> assert PerModuleLevels.debug() == :ok end) == ""
    assert capture_log(fn -> Logger.debug("outer_debug_msg") end) =~ "outer_debug_msg"
    assert capture_log(fn -> assert PerModuleLevels.error() == :ok end) =~ "error_msg"

    Logger.put_module_level(PerModuleLevels, :debug)

    assert capture_log(:error, fn -> assert PerModuleLevels.debug() == :ok end) =~ "debug_msg"
    assert capture_log(:error, fn -> Logger.debug("outer_debug_msg") end) == ""
    assert capture_log(:error, fn -> assert PerModuleLevels.error() == :ok end) =~ "error_msg"

    Logger.delete_module_level(PerModuleLevels)

    assert capture_log(fn -> assert PerModuleLevels.debug() == :ok end) =~ "debug_msg"
    assert capture_log(fn -> assert PerModuleLevels.error() == :ok end) =~ "error_msg"

    Logger.put_module_level(PerModuleLevels, :error)
    assert capture_log(fn -> assert PerModuleLevels.debug() == :ok end) == ""
    Logger.delete_all_module_levels()
    assert capture_log(fn -> assert PerModuleLevels.debug() == :ok end) =~ "debug_msg"
  end

  test "per-modules levels" do
    modules =
      for n <- 1..2 do
        name = Module.concat([__MODULE__, PerModulesLevels, to_string(n)])

        defmodule name do
          def debug, do: Logger.debug("debug_msg")

          def error, do: Logger.error("error_msg")
        end

        name
      end

    for module <- modules do
      assert capture_log(fn -> assert module.debug() == :ok end) =~ "debug_msg"
      assert capture_log(fn -> assert module.error() == :ok end) =~ "error_msg"
    end

    Logger.put_module_level(modules, :error)

    for module <- modules do
      assert capture_log(fn -> assert module.debug() == :ok end) =~ ""
      assert capture_log(fn -> Logger.debug("outer_debug_msg") end) =~ "outer_debug_msg"
      assert capture_log(fn -> assert module.error() == :ok end) =~ "error_msg"
    end

    Logger.put_module_level(modules, :debug)

    for module <- modules do
      assert capture_log(:error, fn -> assert module.debug() == :ok end) =~ "debug_msg"
      assert capture_log(:error, fn -> Logger.debug("outer_debug_msg") end) == ""
      assert capture_log(fn -> assert module.error() == :ok end) =~ "error_msg"
    end

    Logger.delete_module_level(modules)

    for module <- modules do
      assert capture_log(fn -> assert module.debug() == :ok end) =~ "debug_msg"
      assert capture_log(fn -> assert module.error() == :ok end) =~ "error_msg"
    end

    Logger.put_module_level(modules, :error)

    for module <- modules do
      assert capture_log(fn -> assert module.debug() == :ok end) == ""
    end

    Logger.delete_all_module_levels()

    for module <- modules do
      assert capture_log(fn -> assert module.debug() == :ok end) =~ "debug_msg"
    end
  end

  test "per-application levels" do
    Application.load(:eex)
    assert Logger.get_module_level(EEx) == []
    Logger.put_application_level(:eex, :notice)
    assert Logger.get_module_level(EEx) == [{EEx, :notice}]
    Logger.put_module_level(EEx, :alert)
    assert Logger.get_module_level(EEx) == [{EEx, :alert}]
  after
    Logger.delete_all_module_levels()
  end

  test "per-process levels" do
    assert Logger.get_process_level(self()) == nil

    assert capture_log(fn -> Logger.debug("debug_msg") end) =~ "debug_msg"

    Logger.put_process_level(self(), :debug)
    assert Logger.get_process_level(self()) == :debug

    assert capture_log(fn -> Logger.debug("debug_msg") end) =~ "debug_msg"

    Logger.put_process_level(self(), :error)

    assert capture_log(fn -> Logger.debug("debug_msg") end) == ""
    assert capture_log(fn -> Logger.error("error_msg") end) =~ "error_msg"

    Logger.put_process_level(self(), :none)
    assert Logger.get_process_level(self()) == :none

    assert capture_log(fn -> Logger.debug("debug_msg") end) == ""
    assert capture_log(fn -> Logger.error("error_msg") end) == ""

    Logger.delete_process_level(self())
    assert Logger.get_process_level(self()) == nil

    assert capture_log(fn -> Logger.debug("debug_msg") end) =~ "debug_msg"
    assert capture_log(fn -> Logger.error("error_msg") end) =~ "error_msg"
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
           end) =~ msg("module=LoggerTest [info] ok")
  end

  test "metadata compile-time merge" do
    assert Logger.metadata(mfa: {Sample, :foo, 1}) == :ok

    assert capture_log(fn ->
             assert Logger.bare_log(:info, "ok", application: nil, mfa: {CustomTest, :bar, 2}) ==
                      :ok
           end) =~ msg("module=CustomTest [info] ok")
  end

  test "metadata merge when the argument function returns metadata" do
    assert Logger.metadata(module: Sample) == :ok

    fun = fn -> {"ok", [module: "Function"]} end

    assert capture_log(fn ->
             assert Logger.bare_log(:info, fun, application: nil, module: LoggerTest) == :ok
           end) =~ msg("module=Function [info] ok")
  end

  describe "log with function" do
    test "supports iolist" do
      fun = fn -> ["ok", ?:, ~c"example"] end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fun, application: nil, module: FunctionTest) == :ok
             end) =~ msg("module=FunctionTest [info] ok:example")
    end

    test "supports binaries" do
      fun = fn -> "ok:example" end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fun, application: nil, module: FunctionTest) == :ok
             end) =~ msg("module=FunctionTest [info] ok:example")
    end
  end

  describe "report logging" do
    test "supports maps" do
      assert capture_log(fn ->
               assert Logger.bare_log(:info, %{foo: 10}, application: nil, module: FunctionTest) ==
                        :ok
             end) =~ msg("module=FunctionTest [info] [foo: 10]")
    end

    test "supports keyword" do
      assert capture_log(fn ->
               assert Logger.bare_log(:info, foo: 10) == :ok
             end) =~ msg("[info] [foo: 10]")
    end

    test "supports custom report_cb" do
      report_cb = fn %{foo: foo} -> {"Foo is ~B", [foo]} end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, %{foo: 10}, report_cb: report_cb) == :ok
             end) =~ msg("[info] Foo is 10")

      report_cb = fn %{foo: foo}, _opts -> "Foo is #{foo}" end

      assert capture_log(fn ->
               assert Logger.bare_log(:info, %{foo: 20}, report_cb: report_cb) == :ok
             end) =~ msg("[info] Foo is 20")
    end

    test "supports translator_inspect_opts for reports" do
      Application.put_env(:logger, :translator_inspect_opts, printable_limit: 1)

      assert capture_log(fn -> :logger.error(%{foo: "bar"}) end) =~
               ~S([error] [foo: "b" <> ...])
    after
      Application.put_env(:logger, :translator_inspect_opts, [])
    end

    test "supports function that return report" do
      assert capture_log(fn ->
               assert Logger.bare_log(:info, fn -> %{foo: 10} end) == :ok
             end) =~ msg("[info] [foo: 10]")

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fn -> {%{foo: 10}, []} end) == :ok
             end) =~ msg("[info] [foo: 10]")

      assert capture_log(fn ->
               assert Logger.bare_log(:info, fn -> {[foo: 10], []} end) == :ok
             end) =~ msg("[info] [foo: 10]")
    end
  end

  test "compare_levels/2" do
    assert Logger.compare_levels(:debug, :debug) == :eq
    assert Logger.compare_levels(:debug, :info) == :lt
    assert Logger.compare_levels(:debug, :notice) == :lt
    assert Logger.compare_levels(:debug, :warning) == :lt
    assert Logger.compare_levels(:debug, :error) == :lt
    assert Logger.compare_levels(:debug, :critical) == :lt
    assert Logger.compare_levels(:debug, :alert) == :lt
    assert Logger.compare_levels(:debug, :emergency) == :lt

    assert Logger.compare_levels(:info, :debug) == :gt
    assert Logger.compare_levels(:info, :info) == :eq
    assert Logger.compare_levels(:info, :warning) == :lt
    assert Logger.compare_levels(:info, :error) == :lt

    assert Logger.compare_levels(:warning, :debug) == :gt
    assert Logger.compare_levels(:warning, :info) == :gt
    assert Logger.compare_levels(:warning, :warning) == :eq
    assert Logger.compare_levels(:warning, :error) == :lt

    assert Logger.compare_levels(:error, :debug) == :gt
    assert Logger.compare_levels(:error, :info) == :gt
    assert Logger.compare_levels(:error, :warning) == :gt
    assert Logger.compare_levels(:error, :error) == :eq
  end

  test "deprecated :warn" do
    ExUnit.CaptureIO.capture_io(:stderr, fn ->
      assert capture_log(fn ->
               Logger.log(:warn, "hello") == :ok
             end) =~ "[warning]"

      assert capture_log(fn ->
               Logger.bare_log(:warn, "hello") == :ok
             end) =~ "[warning]"
    end)
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
             end) =~ msg_with_meta("[info] hello")

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
             end) =~ msg_with_meta("[warning] hello")

      assert capture_log(:error, fn ->
               assert Logger.warning("hello", []) == :ok
             end) == ""

      assert capture_log(:error, fn ->
               assert Logger.warning(raise("not invoked"), []) == :ok
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
             end) =~ msg_with_meta("[critical] hello")

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
             end) =~ msg_with_meta("[alert] hello")

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
             end) =~ msg_with_meta("[emergency] hello")

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
        [function: "level_filter/0", level_lower_than: :warning],
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
    assert capture_log(fn -> assert PurgeMatching.level_filter() == :ok end) =~ "warning_filter"
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
           end) =~ msg("module=LoggerTest.SampleNoApp [info] hello")

    Logger.configure(compile_time_application: :sample_app)

    defmodule SampleApp do
      def info do
        Logger.info("hello")
      end
    end

    assert capture_log(fn ->
             assert SampleApp.info() == :ok
           end) =~ msg("application=sample_app module=LoggerTest.SampleApp [info] hello")
  after
    Logger.configure(compile_time_application: nil)
  end

  @tag formatter: [truncate: 4]
  test "log/2 truncates messages" do
    assert capture_log(fn -> Logger.log(:debug, "hello") end) =~ "hell (truncated)"
  end

  test "log/2 handles bad Unicode chars" do
    output =
      capture_log(fn ->
        assert Logger.log(:debug, "he" <> <<185>> <> "lo") == :ok
        Process.sleep(100)
      end)

    assert output =~ "FORMATTER ERROR: bad return value"
    assert output =~ "** (RuntimeError) bad return value from Logger formatter Logger.Formatter"
  end

  test "stops the application silently" do
    Logger.App.stop()
    Application.start(:logger)
  end

  test "starts the application with warning level" do
    Logger.App.stop()
    assert %{level: :notice} = :logger.get_primary_config()
    Application.put_env(:logger, :level, :warning)
    Application.start(:logger)
    assert %{level: :warning} = :logger.get_primary_config()
  after
    Application.delete_env(:logger, :level)
    Logger.App.stop()
    Application.start(:logger)
  end

  test "starts the application without a handler" do
    Application.put_env(:logger, :default_handler, false)
    Logger.App.stop()
    Application.start(:logger)
    assert :logger.get_handler_config() == []
  after
    Application.delete_env(:logger, :default_handler)
    Logger.App.stop()
    Application.start(:logger)
  end

  test "starts the application with custom formatter" do
    Application.put_env(:logger, :default_formatter, format: "yes--$message--yes")
    Logger.App.stop()
    Application.start(:logger)
    assert capture_log(fn -> Logger.log(:debug, "hello") end) =~ "yes--hello--yes"
  after
    Application.delete_env(:logger, :default_formatter)
    Logger.App.stop()
    Application.start(:logger)
  end

  test "starts the application with custom handler" do
    Application.put_env(:logger, :default_handler,
      level: :error,
      formatter: Logger.Formatter.new(format: "no--$message--no")
    )

    Logger.App.stop()
    Application.start(:logger)
    assert capture_log(fn -> Logger.log(:debug, "hello") end) == ""
    assert capture_log(fn -> Logger.log(:error, "hello") end) =~ "no--hello--no"
  after
    Application.delete_env(:logger, :default_handler)
    Logger.App.stop()
    Application.start(:logger)
  end

  test "starts the application with global metadata" do
    Application.put_env(:logger, :metadata, global_meta: :yes)
    Logger.App.stop()
    Application.start(:logger)
    assert :logger.get_primary_config()[:metadata] == %{global_meta: :yes}
  after
    Application.put_env(:logger, :metadata, [])
    Logger.App.stop()
    Application.start(:logger)
  end

  test "writes to stderr on bad default handler config" do
    Application.put_env(:logger, :default_handler, config: %{file: 123})
    Logger.App.stop()

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             Application.start(:logger)
           end) =~
             "Could not attach default Logger handler: {:handler_not_added, {:callback_crashed,"
  after
    Application.delete_env(:logger, :default_handler)
    Logger.App.stop()
    Application.start(:logger)
  end

  test "writes to stderr on bad default handler module" do
    Application.put_env(:logger, :default_handler, module: :who_knows)
    Logger.App.stop()

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             Application.start(:logger)
           end) =~
             "Could not attach default Logger handler: {:invalid_handler, {:function_not_exported"
  after
    Application.delete_env(:logger, :default_handler)
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

  test "always evaluate messages" do
    Logger.configure(
      always_evaluate_messages: true,
      compile_time_purge_matching: [[level_lower_than: :info]],
      level: :error
    )

    defmodule AlwaysEvaluate do
      def compile_purged do
        Logger.debug(send(self(), "compile purged"))
      end

      def runtime_purged do
        Logger.info(send(self(), "runtime purged"))
      end

      def not_purged do
        Logger.error(send(self(), "not purged"))
      end
    end

    assert capture_log(fn -> AlwaysEvaluate.not_purged() end) =~ "not purged"
    assert_received "not purged"

    assert capture_log(fn ->
             Logger.configure(level: :error)
             AlwaysEvaluate.runtime_purged()
           end) == ""

    assert_received "runtime purged"

    assert capture_log(fn ->
             Logger.configure(level: :debug)
             AlwaysEvaluate.runtime_purged()
           end) =~ "runtime purged"

    assert_received "runtime purged"

    assert capture_log(fn ->
             Logger.configure(level: :error)
             AlwaysEvaluate.compile_purged()
           end) == ""

    assert_received "compile purged"
  after
    Logger.configure(
      level: :debug,
      always_evaluate_messages: false,
      compile_time_purge_matching: []
    )
  end

  describe "colors" do
    @tag formatter: [format: "$message", colors: [enabled: true]]
    test "default" do
      assert capture_log(fn -> Logger.debug("hello", ansi_color: :yellow) end) ==
               IO.ANSI.yellow() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.debug("hello") end) ==
               IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.info("hello") end) ==
               IO.ANSI.normal() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.warning("hello") end) ==
               IO.ANSI.yellow() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.error("hello") end) ==
               IO.ANSI.red() <> "hello" <> IO.ANSI.reset()
    end

    @tag formatter: [
           format: "$message",
           colors: [enabled: true, debug: :magenta, info: :cyan, warning: :magenta, error: :cyan]
         ]
    test "custom" do
      assert capture_log(fn -> Logger.debug("hello") end) ==
               IO.ANSI.magenta() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.info("hello") end) ==
               IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.warning("hello") end) ==
               IO.ANSI.magenta() <> "hello" <> IO.ANSI.reset()

      assert capture_log(fn -> Logger.error("hello") end) ==
               IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()
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
