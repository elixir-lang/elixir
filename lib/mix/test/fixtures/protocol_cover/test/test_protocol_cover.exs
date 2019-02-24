defmodule ProtocolCoverTest do
  use ExUnit.Case

  test "test String" do
    string = "Test"
    assert ProtocolCover.to_uppercase(string) == "TEST"
  end
end
