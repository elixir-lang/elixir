Code.require_file "../../test_helper.exs", __FILE__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case, sync: false

  setup_all do
    [my_context: :setup_all]
  end

  setup do
    [first_setup: true]
  end

  setup context do
    if Process.get(:ex_unit_callback) do
      raise "ex_unit_callback was not cleaned"
    else
      Process.put(:ex_unit_callback, context[:test])
    end

    assert context[:first_setup]
    assert context[:my_context] == :setup_all

    [my_context: :setup]
  end

  teardown context do
    assert context[:my_context] == :setup
    assert Process.get(:ex_unit_callback) == context[:test]
    Process.delete(:ex_unit_callback)
    []
  end

  teardown_all context do
    assert context[:my_context] == :setup_all
    []
  end

  test :callback, context do
    assert context[:my_context] == :setup
    assert Process.get(:ex_unit_callback) == :test_callback
  end

  # Ensure we run at least two tests, so setup is run twice
  test :ok do
    assert :ok == :ok
  end
end
