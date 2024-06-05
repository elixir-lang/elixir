defmodule ATest do
  use ExUnit.Case, async: true

  test "f async" do
    assert A.f() == :ok
  end
end
