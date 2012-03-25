Code.require_file "../../test_helper", __FILE__

defmodule Kernel.PartialApplicationTest do
  use ExUnit.Case

  test :partial_with_simple_call_and_one_item do
    fun = minus(10, &1)
    assert_equal 5, fun.(5)
    assert_equal 7, fun.(3)
  end

  test :partial_with_simple_call_and_all_items do
    fun = minus(&1, &2)
    assert_equal 5, fun.(10, 5)
    assert_equal 7, fun.(13, 6)
  end

  test :partial_with_atom_call_and_one_item do
    fun = :minus.(10, &1)
    assert_equal 5, fun.(5)
    assert_equal 7, fun.(3)
  end

  test :partial_with_funcall_and_one_item do
    fun = minus(&1, &2)
    fun = fun.(10, &1)
    assert_equal 5, fun.(5)
    assert_equal 7, fun.(3)
  end

  test :partial_with_funcall_and_all_items do
    fun = minus(&1, &2)
    fun = fun.(&1, &2)
    assert_equal 5, fun.(10, 5)
    assert_equal 7, fun.(13, 6)
  end

  test :partial_with_opposite_order do
    fun = minus(&2, &1)
    assert_equal -5, fun.(10, 5)
    assert_equal -7, fun.(13, 6)
  end

  test :partial_with_duplicated do
    fun = minus(&1, &1)
    assert_equal 0, fun.(10)
    assert_equal 0, fun.(13)
  end

  test :partial_with_internal_macro do
    fun = if(&1, do: 1, else: 2)
    assert_equal 1, fun.(20)
    assert_equal 2, fun.(nil)
  end

  test :partial_application_for_module_calls do
    fun = List.reverse(&1)
    assert_equal [3,2,1], fun.([1,2,3])
  end

  test :partial_application_for_operator do
    assert_equal [2,4,6], Enum.map [1,2,3], &1 * 2
  end

  test :partial_application_for_const_op do
    fun = Foo::&1
    assert_equal ::Foo.Bar, fun.(Bar)
  end

  defp minus(x, y) do
    x - y
  end
end
