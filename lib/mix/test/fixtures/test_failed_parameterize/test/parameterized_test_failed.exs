defmodule ParameterizedTest do
  use ExUnit.Case,
    parameterize: [%{value: :a}, %{value: :b}]

  test "checks value", %{value: value} do
    assert value == :b
  end
end
