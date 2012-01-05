Code.require_file "../../test_helper", __FILE__

defmodule Elixir::PartialApplicationTest do
  use ExUnit::Case

  def test_partial_with_simple_call_and_one_item do
    fun = minus(10, _)
    5 = fun.(5)
    7 = fun.(3)
  end

  def test_partial_with_simple_call_and_all_items do
    fun = minus(_, _)
    5 = fun.(10, 5)
    7 = fun.(13, 6)
  end

  def test_partial_with_atom_call_and_one_item do
    fun = :minus.(10, _)
    5 = fun.(5)
    7 = fun.(3)
  end

  def test_partial_with_funcall_and_one_item do
    fun = minus(_, _)
    fun = fun.(10, _)
    5 = fun.(5)
    7 = fun.(3)
  end

  def test_partial_with_funcall_and_all_items do
    fun = minus(_, _)
    fun = fun.(_, _)
    5 = fun.(10, 5)
    7 = fun.(13, 6)
  end

  def test_partial_with_internal_macro do
    fun = if(_, do: 1, else: 2)
    1 = fun.(20)
    2 = fun.(nil)
  end

  def test_partial_application_for_module_calls do
    fun = List.reverse(_)
    [3,2,1] = fun.([1,2,3])
  end

  def test_partial_application_for_operator do
    [2,4,6] = List.map [1,2,3], :*.(_, 2)
  end

  defp minus(x, y) do
    x - y
  end
end