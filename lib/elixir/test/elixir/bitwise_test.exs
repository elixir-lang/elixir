Code.require_file "test_helper.exs", __DIR__

defmodule Bitwise.FunctionsTest do
  use ExUnit.Case, async: true

  doctest Bitwise, import: true

  use Bitwise, skip_operators: true

  test "bnot" do
    assert bnot(1) == -2
  end

  test "band" do
    assert band(1, 1) == 1
  end

  test "bor" do
    assert bor(0, 1) == 1
  end

  test "bxor" do
    assert bxor(1, 1) == 0
  end

  test "bsl" do
    assert bsl(1, 1) == 2
  end

  test "bsr" do
    assert bsr(1, 1) == 0
  end
end

defmodule Bitwise.OperatorsTest do
  use ExUnit.Case, async: true
  use Bitwise, only_operators: true

  test "bnot" do
    assert ~~~1 == -2
  end

  test "band" do
    assert (1 &&& 1) == 1
  end

  test "bor" do
    assert (0 ||| 1) == 1
  end

  test "bxor" do
    assert 1 ^^^ 1 == 0
  end

  test "bsl" do
    assert (1 <<< 1) == 2
  end

  test "bsr" do
    assert (1 >>> 1) == 0
  end
end
