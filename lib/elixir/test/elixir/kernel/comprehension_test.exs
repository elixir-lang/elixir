Code.require_file "../../test_helper", __FILE__

defmodule Kernel.ComprehensionTest do
  use ExUnit.Case

  test :list_comprehensions do
    assert [4] == lc x inlist [1,2,3], rem(x, 2) == 0, do: x * 2
  end

  test :list_comprehensions_with_nil do
    assert [] == lc x inlist [1,2,3], nilly, do: x * 2
  end

  test :list_comprehensions_with_truthy_object do
    assert [2,4,6] == lc x inlist [1,2,3], 1, do: x * 2
  end

  test :list_comprehensions_with_inlist_of_bins do
    assert [2,4,6] == lc <<x>> inlist [<<1>>,<<2>>,<<3>>], do: x * 2
  end

  test :list_comprehensions_with_implicit_inbits do
    assert [2,4,6] == lc <<x>> inbits <<1,2,3>>, do: x * 2
  end

  test :list_comprehensions_with_two_generators do
    assert [4, 5, 6, 8, 10, 12, 12, 15, 18] == lc x inlist [1,2,3], y inlist [4,5,6], do: x * y
  end

  test :list_comprehension_multiline do
    result = lc x inlist [1,2,3] do
      x * 2
    end

    assert [2,4,6] == result
  end

  test :generator_precedence do
    assert lc { _, _ } = x inlist [foo: :bar], do: x
  end

  test :bit_comprehensions do
    assert <<4>> == bc x inlist [1,2,3], rem(x, 2) == 0, do: <<x * 2>>
  end

  test :bit_comprehensions_with_nil do
    assert <<>> == bc x inlist [1,2,3], nilly, do: <<x * 2>>
  end

  test :bit_comprehensions_with_truthy_object do
    assert <<2,4,6>> == bc x inlist [1,2,3], 1, do: <<x * 2>>
  end

  test :bit_comprehensions_with_inlist_of_bins do
    assert <<2,4,6>> == bc <<x>> inlist [<<1>>,<<2>>,<<3>>], do: <<x * 2>>
  end

  test :bit_comprehensions_with_explicit_inbits do
    assert <<2,4,6>> == bc <<x>> inbits <<1,2,3>>, do: <<x * 2>>
  end

  test :bit_comprehensions_with_two_generators do
    assert <<4, 5, 6, 8, 10, 12, 12, 15, 18>> == bc x inlist [1,2,3], y inlist [4,5,6], do: <<x*y>>
  end

  test :bit_comprehension_multiline do
    result = bc x inlist [1,2,3] do
      <<x * 2>>
    end

    assert <<2,4,6>> == result
  end

  defp nilly, do: nil
end
