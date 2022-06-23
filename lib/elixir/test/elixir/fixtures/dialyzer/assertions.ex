defmodule Dialyzer.Assertions do
  import ExUnit.Assertions

  def assert_with_truthy_match do
    assert :ok = known_type_truthy()
  end

  def assert_with_truthy_value do
    assert known_type_truthy()
  end

  def assert_with_unknown_type do
    assert unknown_type_truthy()
  end

  def refute_with_falsy_value do
    refute known_type_falsy()
  end

  def refute_with_unknown_type do
    refute unknown_type_falsy()
  end

  defp known_type_truthy, do: :ok
  defp known_type_falsy, do: nil

  @spec unknown_type_truthy :: any
  defp unknown_type_truthy, do: Enum.random([1, true, :ok])
  @spec unknown_type_falsy :: any
  defp unknown_type_falsy, do: Enum.random([false, nil])
end
