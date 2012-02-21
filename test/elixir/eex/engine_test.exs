Code.require_file "../../test_helper", __FILE__

defmodule EEx::EngineTest do
  use ExUnit::Case
  require EEx::Engine, as: E

  test "handle text" do
    assert_equal {:"<>", 0, ["", "foo"]}, E.handle_text("", "foo")
  end

  test "handle equal expression" do
    assert_equal {:"<>", 0, ["",{:to_binary,0,[:foo]}]}, E.handle_expr("", '=', :foo)
  end
end
