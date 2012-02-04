Code.require_file "../../test_helper", __FILE__

defmodule Kernel::RescueTest do
  use ExUnit::Case

  def test_rescue_with_underscore_no_exception do
    true = try do
      RescueUndefinedModule.go
    rescue: _
      true
    end
  end

  def test_rescue_with_higher_precedence_than_catch do
    true = try do
      RescueUndefinedModule.go
    catch: _, _
      false
    rescue: _
      true
    end
  end

  def test_rescue_runtime_error do
    true = try do
      raise "an exception"
    rescue: RuntimeError
      true
    catch: :error, _
      false
    end

    false = try do
      raise "an exception"
    rescue: AnotherError
      true
    catch: :error, _
      false
    end
  end
end