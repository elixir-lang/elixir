defmodule BarTest do
  use ExUnit.Case

  test "greets the world" do
    assert Bar.hello() == :world
  end

  test "world the greets" do
    assert Bar.Ignore.world() == :hello
  end

  test "works with protocols" do
    assert Bar.Protocol.to_uppercase("foo") == "FOO"
  end

  test "protocols are consolidated" do
    assert Protocol.consolidated?(Bar.Protocol)
  end
end
