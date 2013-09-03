Code.require_file "test_helper.exs", __DIR__

defmodule IntegerTest do
  use ExUnit.Case, async: true

  test :odd? do
    assert Integer.odd?(1) == true
    assert Integer.odd?(2) == false
    assert Integer.odd?(3) == true
    assert Integer.odd?(-1) == true
    assert Integer.odd?(-2) == false
    assert Integer.odd?(-3) == true
  end

  test :even? do
    assert Integer.even?(1) == false
    assert Integer.even?(2) == true
    assert Integer.even?(3) == false
    assert Integer.even?(-1) == false
    assert Integer.even?(-2) == true
    assert Integer.even?(-3) == false
  end
end
