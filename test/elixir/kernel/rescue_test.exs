Code.require_file "../../test_helper", __FILE__

defmodule Kernel::RescueTest do
  use ExUnit::Case

  def test_rescue_with_underscore_no_exception do
    13 = try do
      RescueUndefinedModule.go
    rescue: _
      13
    end
  end

  def test_rescue_with_higher_precedence_than_catch do
    13 = try do
      RescueUndefinedModule.go
    catch: _
      11
    rescue: _
      13
    end
  end
end