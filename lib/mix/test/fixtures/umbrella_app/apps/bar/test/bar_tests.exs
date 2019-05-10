defmodule BarTest do
  use ExUnit.Case

  test "greets the world" do
    assert Bar.hello() == :world
    assert Bar.Protocol.to_uppercase("foo") == "FOO"
  end
end
