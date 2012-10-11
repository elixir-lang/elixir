Code.require_file "../../test_helper.exs", __FILE__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case, sync: false

  def setup_all do
    [my_context: :setup_all]
  end

  def setup([my_context: context], test) do
    if Process.get(:ex_unit_callback) do
      raise "ex_unit_callback was not cleaned"
    else
      Process.put(:ex_unit_callback, test)
    end

    assert context == :setup_all

    [my_context: :setup]
  end

  def teardown([my_context: context], test) do
    assert context == :setup
    assert Process.get(:ex_unit_callback) == test
    Process.delete(:ex_unit_callback)
  end

  def teardown_all([my_context: context]) do
    assert context == :setup_all
  end

  test :callback do
    assert Process.get(:ex_unit_callback) == :test_callback
  end

  # Ensure we run at least two tests, so setup is run twice
  test :ok do
    assert :ok == :ok
  end
end
