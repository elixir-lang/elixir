Code.require_file "test_helper.exs", __DIR__

defmodule PortTest do
  use ExUnit.Case, async: true
  import PathHelpers

  test "info/1,2 with registered name" do
    {:ok, port} = :gen_udp.open(0)

    assert Port.info(port, :links) == {:links, [self()]}
    assert Port.info(port, :registered_name) == {:registered_name, []}

    Process.register(port, __MODULE__)

    assert Port.info(port, :registered_name) == {:registered_name, __MODULE__}

    :ok = :gen_udp.close(port)

    assert Port.info(port, :registered_name) == nil
    assert Port.info(port) == nil
  end

  test "stream map" do
    stream = Port.stream!({:spawn, "echo"}, [:stream])

    assert %Port.Stream{} = stream
    assert stream.name == {:spawn, "echo"}
    assert stream.settings == [:exit_status, :stream]
    assert stream.raw == false

    stream = Port.stream!({:spawn, "echo"}, [:stream], true)
    assert stream.raw == true
  end

  test "stream simple" do
    {name, settings} = case windows?() do
      true  -> {{:spawn, "cmd /c echo hello"}, []}
      false -> {{:spawn, "echo hello"}, []}
    end

    messages = Port.stream!(name, settings) |> Enum.into([])
    assert messages == ['hello\n']
  end

  test "stream binary" do
    {name, settings} = case windows?() do
      true  -> {{:spawn, "cmd /c echo hello"}, [:binary]}
      false -> {{:spawn, "echo hello"}, [:binary]}
    end

    stream = Port.stream!(name, settings)

    transformed = Enum.map stream, fn(data) ->
      String.replace(data, "e", "a")
    end

    assert transformed == ["hallo\n"]
  end

  test "stream line" do
    {name, settings} = case windows?() do
      true  -> {{:spawn, "cmd /c echo 'hello\nworld"}, [:binary, {:line, 2048}]}
      false -> {{:spawn, "echo 'hello\nworld'"}, [:binary, {:line, 2048}]}
    end

    lines = Port.stream!(name, settings) |> Enum.into([])
    assert lines == ["hello", "world"]
  end

  test "stream raw and line" do
    {name, settings} = case windows?() do
      true  -> {{:spawn, "cmd /c echo hello"}, [{:line, 2}]}
      false -> {{:spawn, "echo hello"}, [{:line, 2}]}
    end

    messages = Port.stream!(name, settings, true) |> Enum.into([])
    assert messages == [{:noeol, 'he'}, {:noeol, 'll'}, {:eol, 'o'}]
  end
end
