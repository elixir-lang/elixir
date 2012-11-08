Code.require_file "../test_helper.exs", __FILE__

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  test "it runs after_spawn hooks" do
    assert Process.get(:after_spawn) == :ex_unit
  end

  test "it can read user config" do
    File.write("ex_unit.test.config","[extra_option: true]")
    assert ExUnit.user_options("nosuchfile.config") == []
    assert ExUnit.user_options("ex_unit.test.config") == [extra_option: true]
    File.rm("ex_unit.test.config")
  end
end
