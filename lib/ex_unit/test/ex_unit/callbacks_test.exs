Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.CallbacksTest do
  use ExUnit.Case

  setup_all do
    { :ok, [context: :setup_all] }
  end

  setup do
    { :ok, [initial_setup: true] }
  end

  setup context do
    assert context[:initial_setup]
    assert context[:context] == :setup_all
    { :ok, [context: :setup] }
  end

  setup context do
    if Process.get(:ex_unit_callback) do
      raise "ex_unit_callback was not cleaned"
    else
      Process.put(:ex_unit_callback, context[:test].name)
    end
    :ok
  end

  teardown context do
    assert context[:context] == :setup
    :ok
  end

  teardown context do
    assert Process.get(:ex_unit_callback) == context[:test].name
    Process.delete(:ex_unit_callback)
    :ok
  end

  teardown_all context do
    assert context[:context] == :setup_all
    :ok
  end

  test "callbacks can run custom code" do
    assert Process.get(:ex_unit_callback) == :"test callbacks can run custom code"
  end

  test "receives context from callback", context do
    assert context[:context] == :setup
  end
end
