Code.require_file "../../test_helper", __FILE__

defmodule Kernel::ComprehensionTest do
  use ExUnit::Case

  test :list_comprehensions do
    assert_equal [4], lc x in [1,2,3], rem(x, 2) == 0, do: x * 2
  end

  test :list_comprehensions_with_nil do
    assert_equal [], lc x in [1,2,3], nilly, do: x * 2
  end

  test :list_comprehensions_with_truthy_object do
    assert_equal [2,4,6], lc x in [1,2,3], 1, do: x * 2
  end

  test :list_comprehensions_with_inlist do
    assert_equal [2,4,6], lc inlist(x, [1,2,3]), do: x * 2
  end

  test :list_comprehensions_with_inlist_of_bins do
    assert_equal [2,4,6], lc inlist(<<x>>, [<<1>>,<<2>>,<<3>>]), do: x * 2
  end

  test :list_comprehensions_with_implicit_inbin do
    assert_equal [2,4,6], lc <<x>> in <<1,2,3>>, do: x * 2
  end

  test :list_comprehensions_with_explicit_inbin do
    assert_equal [2,4,6], lc inbin(<<x>>, <<1,2,3>>), do: x * 2
  end

  test :list_comprehensions_with_two_generators do
    assert_equal [4, 5, 6, 8, 10, 12, 12, 15, 18], lc x in [1,2,3], y in [4,5,6], do: x * y
  end

  test :list_comprehension_multiline do
    result = lc x in [1,2,3] do
      x * 2
    end

    assert_equal [2,4,6], result
  end

  test :bin_comprehensions do
    assert_equal <<4>>, bc x in [1,2,3], rem(x, 2) == 0, do: <<x * 2>>
  end

  test :bin_comprehensions_with_nil do
    assert_equal <<>>, bc x in [1,2,3], nilly, do: <<x * 2>>
  end

  test :bin_comprehensions_with_truthy_object do
    assert_equal <<2,4,6>>, bc x in [1,2,3], 1, do: <<x * 2>>
  end

  test :bin_comprehensions_with_inlist do
    assert_equal <<2,4,6>>, bc inlist(x, [1,2,3]), do: <<x * 2>>
  end

  test :bin_comprehensions_with_inlist_of_bins do
    assert_equal <<2,4,6>>, bc inlist(<<x>>, [<<1>>,<<2>>,<<3>>]), do: <<x * 2>>
  end

  test :bin_comprehensions_with_implicit_inbin do
    assert_equal <<2,4,6>>, bc <<x>> in <<1,2,3>>, do: <<x * 2>>
  end

  test :bin_comprehensions_with_explicit_inbin do
    assert_equal <<2,4,6>>, bc inbin(<<x>>, <<1,2,3>>), do: <<x * 2>>
  end

  test :bin_comprehensions_with_two_generators do
    assert_equal <<4, 5, 6, 8, 10, 12, 12, 15, 18>>, bc x in [1,2,3], y in [4,5,6], do: <<x*y>>
  end

  test :bin_comprehension_multiline do
    result = bc x in [1,2,3] do
      <<x * 2>>
    end

    assert_equal <<2,4,6>>, result
  end

  defp nilly, do: nil
end
