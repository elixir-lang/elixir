Code.require_file "../../test_helper", __FILE__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case, sync: false

  def setup(test) do
    if Process.get(:ex_unit_callback) do
      raise "ex_unit_callback was not cleaned"
    else
      Process.put(:ex_unit_callback, test)
    end      
  end

  def teardown(_) do
    Process.delete(:ex_unit_callback)
  end

  test :callback do
    assert Process.get(:ex_unit_callback) == :test_callback
  end

  # Ensure we run at least two tests, so setup is run twice
  test :ok do
    assert :ok == :ok
  end
end
