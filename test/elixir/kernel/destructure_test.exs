Code.require_file "../../test_helper", __FILE__

defmodule Kernel.DestructureTest do
  use ExUnit.Case

  test :less do
    destructure [x,y,z], [1,2,3,4,5]
    assert_equal 1, x
    assert_equal 2, y
    assert_equal 3, z
  end

  test :more do
    destructure [a,b,c,d,e], [1,2,3]
    assert_equal 1,   a
    assert_equal 2,   b
    assert_equal 3,   c
    assert_equal nil, d
    assert_equal nil, e
  end

  test :equal do
    destructure [a,b,c], [1,2,3]
    assert_equal 1, a
    assert_equal 2, b
    assert_equal 3, c
  end

  test :none do
    destructure [a,b,c], []
    assert_equal nil, a
    assert_equal nil, b
    assert_equal nil, c
  end

  test :match do
    destructure [1,b,_], [1,2,3]
    assert_equal 2, b
  end

  test :nil do
    destructure [a,b,c], a_nil
    assert_equal nil, a
    assert_equal nil, b
    assert_equal nil, c
  end

  test :invalid_match do
    a = 3
    assert_raises MatchError, fn ->
      destructure [^a,b,c], a_list
    end
  end

  defp a_list, do: [1,2,3]
  defp a_nil, do: nil
end
