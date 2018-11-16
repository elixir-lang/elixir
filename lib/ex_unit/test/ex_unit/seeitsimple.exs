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
end
