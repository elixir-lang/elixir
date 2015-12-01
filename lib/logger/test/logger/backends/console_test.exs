defmodule Logger.Backends.ConsoleTest do
  use Logger.Case
  require Logger

  setup do
    on_exit fn ->
      :ok = Logger.configure_backend(:console,
              [format: nil, level: nil, metadata: [], colors: [enabled: false]])
    end
  end

  test "does not start when there is no user" do
    :ok = Logger.remove_backend(:console)
    user = Process.whereis(:user)

    try do
      Process.unregister(:user)
      assert GenEvent.add_handler(Logger, Logger.Backends.Console, :console) ==
             {:error, :ignore}
    after
      Process.register(user, :user)
    end
  after
    {:ok, _} = Logger.add_backend(:console)
  end

  test "can configure format" do
    Logger.configure_backend(:console, format: "$message [$level]")

    assert capture_log(fn ->
      Logger.debug("hello")
    end) =~ "hello [debug]"
  end

  test "can configure metadata" do
    Logger.configure_backend(:console, format: "$metadata$message", metadata: [:user_id])

    assert capture_log(fn ->
      Logger.debug("hello")
    end) =~ "hello"

    Logger.metadata(user_id: 11)
    Logger.metadata(user_id: 13)

    assert capture_log(fn ->
      Logger.debug("hello")
    end) =~ "user_id=13 hello"
  end

  test "metadata defaults" do
    Logger.configure_backend(:console,
      format: "$metadata", metadata: [:file, :line, :module, :function])

    %{module: mod, function: {name, arity}, file: file, line: line} = __ENV__

    assert capture_log(fn ->
      Logger.debug("hello")
    end) =~ "file=#{file} line=#{line + 3} module=#{inspect(mod)} function=#{name}/#{arity}"
  end

  test "can configure level" do
    Logger.configure_backend(:console, level: :info)

    assert capture_log(fn ->
      Logger.debug("hello")
    end) == ""
  end

  test "can configure colors" do
    Logger.configure_backend(:console, [format: "$message", colors: [enabled: true]])

    assert capture_log(fn ->
      Logger.debug("hello")
    end) == IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, [colors: [debug: :magenta]])

    assert capture_log(fn ->
      Logger.debug("hello")
    end) == IO.ANSI.magenta() <> "hello" <> IO.ANSI.reset()

    assert capture_log(fn ->
      Logger.info("hello")
    end) == IO.ANSI.normal() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, [colors: [info: :cyan]])

    assert capture_log(fn ->
      Logger.info("hello")
    end) == IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    assert capture_log(fn ->
      Logger.warn("hello")
    end) == IO.ANSI.yellow() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, [colors: [warn: :cyan]])

    assert capture_log(fn ->
      Logger.warn("hello")
    end) == IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()

    assert capture_log(fn ->
      Logger.error("hello")
    end) == IO.ANSI.red() <> "hello" <> IO.ANSI.reset()

    Logger.configure_backend(:console, [colors: [error: :cyan]])

    assert capture_log(fn ->
      Logger.error("hello")
    end) == IO.ANSI.cyan() <> "hello" <> IO.ANSI.reset()
  end
end
