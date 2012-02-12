Code.require_file "../test_helper", __FILE__

defmodule OrddictTest do
  use ExUnit::Case

  def test_from_enum do
    assert_equal [first_key: 1, second_key: 2], Orddict.from_enum([{:second_key, 2}, {:first_key, 1}])
  end

  def test_fetch do
    assert_equal 1, Orddict.get(create_dict, :first_key)
    assert_equal 2, Orddict.get(create_dict, :second_key)
    assert_equal nil, Orddict.get(create_dict, :other_key)
    assert_equal "default", Orddict.get(create_empty_dict, :first_key, "default")
  end

  def test_keys do
    assert_equal [:first_key, :second_key], Orddict.keys(create_dict)
    assert_equal [], Orddict.keys(create_empty_dict)
  end

  def test_values do
    assert_equal [1, 2], Orddict.values(create_dict)
    assert_equal [], Orddict.values(create_empty_dict)
  end

  def test_delete do
    assert_equal [first_key: 1], Orddict.delete(create_dict, :second_key)
    assert_equal [first_key: 1, second_key: 2], Orddict.delete(create_dict, :other_key)
    assert_equal [], Orddict.delete(create_empty_dict, :other_key)
  end

  def test_store do
    assert_equal [first_key: 1], Orddict.set(create_empty_dict, :first_key, 1)
    assert_equal [first_key: 1, second_key: 2], Orddict.set(create_dict, :first_key, 1)
  end

  def test_merge do
    assert_equal [first_key: 1, second_key: 2], Orddict.merge(create_empty_dict, create_dict)
    assert_equal [first_key: 1, second_key: 2], Orddict.merge(create_dict, create_empty_dict)
    assert_equal [first_key: 1, second_key: 2], Orddict.merge(create_dict, create_dict)
    assert_equal [], Orddict.merge(create_empty_dict, create_empty_dict)
  end

  defp create_empty_dict, do: create_dict([])
  defp create_dict(list // [first_key: 1, second_key: 2]), do: Orddict.from_enum(list)
end
