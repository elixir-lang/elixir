Code.require_file("../test_helper.exs", __DIR__)

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
    {:ok, [context: :setup_all, setup_all: 1]}
  end

  setup context do
    assert context[:context] == :setup_all
    {:ok, [context: :setup, setup: 1]}
  end
end

defmodule ExUnit.NestedCase do
  use ExUnit.CaseTemplate

  setup_all context do
    {:ok, [setup_all: context[:setup_all] + 1]}
  end

  setup context do
    {:ok, [setup: context[:setup] + 1]}
  end
end

defmodule ExUnit.CaseTemplateTest do
  use ExUnit.SampleCase, async: true
  use ExUnit.NestedCase

  two = 2

  test "unquoting the value #{two}" do
    assert 2 == unquote(two)
  end

  test "receives context from parent case", %{context: context} do
    assert context == :setup
  end

  test "runs both templates setup", context do
    assert context[:setup] == 2
  end

  test "runs both templates setup_all", context do
    assert context[:setup_all] == 2
  end

  test "using code is executed" do
    assert hello() == "world"
  end
end
