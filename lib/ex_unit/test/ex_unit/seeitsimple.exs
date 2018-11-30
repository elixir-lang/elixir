Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.SeeItMatch do
  use ExUnit.Case

  alias ExUnit.Pattern

  test "#1" do
    assert %{a: 1, b: 4, m: 6} = %{a: 1, b: 1, d: 2, c: 3}
  end

  test "#2" do
    assert %{a: 1, b: 2} = %{a: 2, b: 1}
  end

  test "#3" do
    assert [1, 2, 3, 4] = [1, 2, 3]
  end

  test "#4" do
    assert [1, 2, 3] = [1, 2, 3, 4]
  end

  test "#5" do
    assert {1, 2, 3, 4} = {1, 2, 3}
  end

  test "#6" do
    assert {1, 2, 3} = {1, 2, 3, 4}
  end

  test "#7" do
    send(self(), %{a: 1, b: 1, d: 2, c: 3})
    send(self(), %{a: 3})
    send(self(), %{a: 4, b: 2})
    assert_receive %{a: 2}
  end

  test "#8" do
    send(
      self(),
      {:save_doc, %{:status => :created, :sync_history => %{"map" => true}, "other" => true}}
    )

    assert_receive {:save_doc, %{status: :creted, sync_history: []} = doc}
  end
end
