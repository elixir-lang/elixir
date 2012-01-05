Code.require_file "../test_helper", __FILE__

defmodule OrddictTest do
  use ExUnit::Case

  def test_from_list do
    [first_key: 1, second_key: 2] = Orddict.from_list([{:second_key, 2}, {:first_key, 1}])
  end

  def test_fetch do
    1         = Orddict.fetch(create_dict, :first_key, nil)
    2         = Orddict.fetch(create_dict, :second_key, nil)
    nil       = Orddict.fetch(create_dict, :other_key, nil)
    "default" = Orddict.fetch(create_empty_dict, :first_key, "default")
  end

  def test_keys do
    [:first_key, :second_key] = Orddict.keys(create_dict)
    []                        = Orddict.keys(create_empty_dict)
  end

  def test_values do
    [1, 2] = Orddict.values(create_dict)
    []     = Orddict.values(create_empty_dict)
  end

  def test_delete do
    [first_key: 1]                = Orddict.delete(create_dict, :second_key)
    [first_key: 1, second_key: 2] = Orddict.delete(create_dict, :other_key)
    [] = Orddict.delete(create_empty_dict, :other_key)
  end

  def test_store do
    [first_key: 1]                = Orddict.store(create_empty_dict, :first_key, 1)
    [first_key: 1, second_key: 2] = Orddict.store(create_dict, :first_key, 1)
  end

  def test_merge do
    [first_key: 1, second_key: 2] = Orddict.merge(create_empty_dict, create_dict)
    [first_key: 1, second_key: 2] = Orddict.merge(create_dict, create_empty_dict)
    [first_key: 1, second_key: 2] = Orddict.merge(create_dict, create_dict)
    [] = Orddict.merge(create_empty_dict, create_empty_dict)
  end

  defp create_empty_dict, do: create_dict([])
  defp create_dict(list // [first_key: 1, second_key: 2]), do: Orddict.from_list(list)
end