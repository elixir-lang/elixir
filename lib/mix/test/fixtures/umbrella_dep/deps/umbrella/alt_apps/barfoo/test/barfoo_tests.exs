defmodule BarFooTest do
  use ExUnit.Case

  test "greets the world" do
    assert BarFoo.hello() == :world
    assert BarFoo.Protocol.to_uppercase("foobar") == "FOOBAR"
  end
end
