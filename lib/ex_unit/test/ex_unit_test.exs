Code.require_file "../test_helper.exs", __FILE__

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  test "it runs after_spawn hooks" do
    assert Process.get(:after_spawn) == :ex_unit
  end
end
