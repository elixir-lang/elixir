defmodule Logger.Backends.ConsoleTest do
  use Logger.Case

  require Logger
  import ExUnit.CaptureIO

  setup do
    on_exit(fn ->
      :ok =
        Logger.configure_backend(
          :console,
          format: nil,
          device: :user,
          level: nil,
          metadata: [],
          colors: [enabled: false]
        )
    end)
  end

  test "does not start when there is no user" do
    :ok = Logger.remove_backend(:console)
    user = Process.whereis(:user)

    try do
      Process.unregister(:user)

      assert :gen_event.add_handler(Logger, Logger.Backends.Console, :console) ==
               {:error, :ignore}
    after
      Process.register(user, :user)
    end
  after
    {:ok, _} = Logger.add_backend(:console)
  end

  test "may use another device" do
    Logger.configure_backend(:console, device: :standard_error)

    assert capture_io(:standard_error, fn ->
             Logger.debug("hello")
             Logger.flush()
           end) =~ "hello"
  end

  test "configures format" do
    Logger.configure_backend(:console, format: "$message [$level]")

    assert capture_log(fn -> Logger.debug("hello") end) =~ "hello [debug]"
  end

  test "configures metadata" do
    Logger.configure_backend(:console, format: "$metadata$message", metadata: [:user_id])
    assert capture_log(fn -> Logger.debug("hello") end) =~ "hello"

    Logger.metadata(user_id: 11)
    Logger.metadata(user_id: 13)
    assert capture_log(fn -> Logger.debug("hello") end) =~ "user_id=13 hello"
  end

  test "ignores crash_reason metadata when configured with metadata: :all" do
    Logger.configure_backend(:console, format: "$metadata$message", metadata: :all)
    Logger.metadata(crash_reason: {%RuntimeError{message: "oops"}, []})
    assert capture_log(fn -> Logger.debug("hello") end) =~ "hello"
  end

  test "logs initial_call as metadata" do
    Logger.configure_backend(:console, format: "$metadata$message", metadata: [:initial_call])
    Logger.metadata(initial_call: {Foo, :bar, 3})
    assert capture_log(fn -> Logger.debug("hello") end) =~ "initial_call=Foo.bar/3 hello"
  end

  test "configures formatter to {module, function} tuple" do
    Logger.configure_backend(:console, format: {__MODULE__, :format})

    assert capture_log(fn -> Logger.debug("hello") end) =~ "my_format: hello"
  end

  def format(_level, message, _ts, _metadata) do
    "my_format: #{message}"
  end

  test "configures metadata to :all" do
    Logger.configure_backend(:console, format: "$metadata", metadata: :all)
    Logger.metadata(user_id: 11)
    Logger.metadata(dynamic_metadata: 5)

    %{module: mod, function: {name, arity}, file: file, line: line} = __ENV__
    log = capture_log(fn -> Logger.debug("hello") end)

    assert log =~ "file=#{file}"
    assert log =~ "line=#{line + 1}"
    assert log =~ "module=#{inspect(mod)}"
    assert log =~ "function=#{name}/#{arity}"
    assert log =~ "dynamic_metadata=5 user_id=11"
  end

  test "provides metadata defaults" do
    metadata = [:file, :line, :module, :function]
    Logger.configure_backend(:console, format: "$metadata", metadata: metadata)

    %{module: mod, function: {name, arity}, file: file, line: line} = __ENV__
    log = capture_log(fn -> Logger.debug("hello") end)

    assert log =~ "file=#{file} line=#{line + 1} module=#{inspect(mod)} function=#{name}/#{arity}"
  end

  test "configures level" do
    Logger.configure_backend(:console, level: :info)

    assert capture_log(fn -> Logger.debug("hello") end) == ""
  end

  test "configures colors" do
    Logger.configure_backend(:console, format: "$message", colors: [enabled: true])

    assert capture_log(fn -> Logger.debug("hello") end) ==
             IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, colors: [debug: :magenta])

    assert capture_log(fn -> Logger.debug("hello") end) ==
             IO.ANSI.magenta() <> "hello" <> IO.ANSI.reset()

    assert capture_log(fn -> Logger.info("hello") end) ==
             IO.ANSI.normal() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, colors: [info: :cyan])

    assert capture_log(fn -> Logger.info("hello") end) ==
             IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    assert capture_log(fn -> Logger.warn("hello") end) ==
             IO.ANSI.yellow() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, colors: [warn: :cyan])

    assert capture_log(fn -> Logger.warn("hello") end) ==
             IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    assert capture_log(fn -> Logger.error("hello") end) ==
             IO.ANSI.red() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, colors: [error: :cyan])

    assert capture_log(fn -> Logger.error("hello") end) ==
             IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, colors: [trace: :cyan])

    assert capture_log(:trace, fn -> Logger.error("hello") end) ==
             IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()
  end

  test "uses colors from metadata" do
    Logger.configure_backend(:console, format: "$message", colors: [enabled: true])

    assert capture_log(fn -> Logger.log(:error, "hello", ansi_color: :yellow) end) ==
             IO.ANSI.yellow() <> "hello" <> IO.ANSI.reset()
  end
end
