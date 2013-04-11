Code.require_file "../../test_helper.exs", __FILE__

defmodule ExUnit.SampleCase do
  use ExUnit.CaseTemplate

  using _ do
    quote do
      import unquote(__MODULE__)
    end
  end

  def hello do
    "world"
  end

  setup_all do
    { :ok, [context: :setup_all] }
  end

  setup context do
    assert context[:context] == :setup_all
    { :ok, [context: :setup] }
  end

  teardown context do
    assert context[:context] == :setup
    :ok
  end

  teardown_all context do
    assert context[:context] == :setup_all
    :ok
  end
end

defmodule ExUnit.CaseTemplateTest do
  use ExUnit.SampleCase, async: true

  two = 2

  test "unquoting the value #{two}" do
    assert 2 == unquote(two)
  end

  test "receives context from parent case", context do
    assert context[:context] == :setup
  end

  test "using code is executed" do
    assert hello == "world"
  end
end
