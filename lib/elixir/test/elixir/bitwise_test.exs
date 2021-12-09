Code.require_file("test_helper.exs", __DIR__)

defmodule BitwiseTest do
  use ExUnit.Case, async: true

  import Bitwise
  doctest Bitwise

  test "bnot/1" do
    assert bnot(1) == -2
  end

  test "band/2" do
    assert band(1, 1) == 1
  end

  test "bor/2" do
    assert bor(0, 1) == 1
  end

  test "bxor/2" do
    assert bxor(1, 1) == 0
  end

  test "bsl/2" do
    assert bsl(1, 1) == 2
  end

  test "bsr/2" do
    assert bsr(1, 1) == 0
  end

  test "band (&&&)" do
    assert (1 &&& 1) == 1
  end

  test "bor (|||)" do
    assert (0 ||| 1) == 1
  end

  test "bsl (<<<)" do
    assert 1 <<< 1 == 2
  end

  test "bsr (>>>)" do
    assert 1 >>> 1 == 0
  end
end
