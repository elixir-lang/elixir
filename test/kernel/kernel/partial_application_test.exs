Code.require_file "../../test_helper", __FILE__

defmodule Kernel.PartialApplicationTest do
  use ExUnit.Case

  test :partial_with_simple_call_and_one_item do
    fun = minus(10, &1)
    assert fun.(5) == 5
    assert fun.(3) == 7
  end

  test :partial_with_simple_call_and_all_items do
    fun = minus(&1, &2)
    assert fun.(10, 5) == 5
    assert fun.(13, 6) == 7
  end

  test :partial_with_atom_call_and_one_item do
    fun = :minus.(10, &1)
    assert fun.(5) == 5
    assert fun.(3) == 7
  end

  test :partial_with_funcall_and_one_item do
    fun = minus(&1, &2)
    fun = fun.(10, &1)
    assert fun.(5) == 5
    assert fun.(3) == 7
  end

  test :partial_with_funcall_and_all_items do
    fun = minus(&1, &2)
    fun = fun.(&1, &2)
    assert fun.(10, 5) == 5
    assert fun.(13, 6) == 7
  end

  test :partial_with_opposite_order do
    fun = minus(&2, &1)
    assert fun.(10, 5) == -5
    assert fun.(13, 6) == -7
  end

  test :partial_with_duplicated do
    fun = minus(&1, &1)
    assert fun.(10) == 0
    assert fun.(13) == 0
  end

  test :partial_with_internal_macro do
    fun = if(&1, do: 1, else: 2)
    assert fun.(20) == 1
    assert fun.(nil) == 2
  end

  test :partial_application_for_module_calls do
    fun = List.reverse(&1)
    assert fun.([1,2,3]) == [3,2,1]
  end

  test :partial_application_for_operator do
    assert Enum.map([1,2,3], &1 * 2) == [2,4,6]
  end

  defp minus(x, y) do
    x - y
  end
end
