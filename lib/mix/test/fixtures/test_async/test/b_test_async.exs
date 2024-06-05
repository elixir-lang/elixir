defmodule BTest do
  use ExUnit.Case, async: false

  test "f sync" do
    assert B.f() == :ok
  end
end
