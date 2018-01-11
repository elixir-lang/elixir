defmodule LoggerTest do
  use Logger.Case
  require Logger

  setup_all do
    Logger.configure_backend(:console, metadata: [:application, :module])

    on_exit(fn ->
      Logger.configure_backend(:console, metadata: [])
    end)
  end

  defp msg_with_meta(text) do
    msg("module=LoggerTest #{text}")
  end

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

  test "add_backend/1 and remove_backend/1" do
    assert :ok = Logger.remove_backend(:console)
    assert Application.get_env(:logger, :backends) == []
    assert Logger.remove_backend(:console) == {:error, :not_found}

    assert capture_log(fn ->
             assert Logger.debug("hello", []) == :ok
           end) == ""

    assert {:ok, _pid} = Logger.add_backend(:console)
    assert Application.get_env(:logger, :backends) == [:console]
    assert Logger.add_backend(:console) == {:error, :already_present}
    assert Application.get_env(:logger, :backends) == [:console]
  end

  test "add_backend/1 with {module, id}" do
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

    assert {:ok, _} = Logger.add_backend({MyBackend, :hello})
    assert {:error, :already_present} = Logger.add_backend({MyBackend, :hello})
    assert :ok = Logger.remove_backend({MyBackend, :hello})
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
           end) =~ msg("application= module=LoggerTest [info]  ok")
  end

  test "metadata compile-time merge" do
    assert Logger.metadata(module: Sample) == :ok

    assert capture_log(fn ->
             assert Logger.log(:info, "ok", application: nil, module: CustomTest) == :ok
           end) =~ msg("application= module=CustomTest [info]  ok")
  end

  test "metadata merge when the argument function returns metadata" do
    assert Logger.metadata(module: Sample) == :ok

    fun = fn -> {"ok", [module: "Function"]} end

    assert capture_log(fn ->
             assert Logger.bare_log(:info, fun, application: nil, module: LoggerTest) == :ok
           end) =~ msg("application= module=Function [info]  ok")
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
  end

  test "info/2" do
    assert capture_log(fn ->
             assert Logger.info("hello", []) == :ok
           end) =~ msg_with_meta("[info]  hello")

    assert capture_log(:warn, fn ->
             assert Logger.info("hello", []) == :ok
           end) == ""
  end

  test "warn/2" do
    assert capture_log(fn ->
             assert Logger.warn("hello", []) == :ok
           end) =~ msg_with_meta("[warn]  hello")

    assert capture_log(:error, fn ->
             assert Logger.warn("hello", []) == :ok
           end) == ""
  end

  test "error/2" do
    assert capture_log(fn ->
             assert Logger.error("hello", []) == :ok
           end) =~ msg_with_meta("[error] hello")
  end

  test "remove unused calls at compile time" do
    Logger.configure(compile_time_purge_level: :info)

    defmodule Sample do
      def debug do
        Logger.debug("hello")
      end

      def info do
        Logger.info("hello")
      end
    end

    assert capture_log(fn ->
             assert Sample.debug() == :ok
           end) == ""

    assert capture_log(fn ->
             assert Sample.info() == :ok
           end) =~ msg("module=LoggerTest.Sample [info]  hello")
  after
    Logger.configure(compile_time_purge_level: :debug)
  end

  test "unused variable warnings suppressed when we remove macros from the AST" do
    Logger.configure(compile_time_purge_level: :info)

    # This should not warn, even if the Logger call is purged from the AST.
    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             defmodule Unused do
               require Logger

               def hello(a, b) do
                 Logger.debug(["a: ", inspect(a), ", b: ", inspect(b)])
               end
             end
           end) == ""

    assert LoggerTest.Unused.hello(1, 2) == :ok
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

    assert capture_log(fn ->
             Logger.log(:debug, "hello")
           end) =~ "hell (truncated)"
  after
    Logger.configure(truncate: 8096)
  end

  test "log/2 with to_string/1 conversion" do
    Logger.configure(truncate: 4)

    assert capture_log(fn ->
             Logger.log(:debug, :hello)
           end) =~ "hell (truncated)"
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
end
