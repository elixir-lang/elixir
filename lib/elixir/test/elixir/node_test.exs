Code.require_file("test_helper.exs", __DIR__)

defmodule NodeTest do
  use ExUnit.Case

  doctest Node

  @tag :epmd
  test "start/3 and stop/0" do
    assert Node.stop() == {:error, :not_found}
    assert {:ok, _} = Node.start(:hello, :shortnames, 15000)
    assert Node.stop() == :ok
  end
end
