defmodule HelloTest do
  use ExUnit.Case

  test :thirteen do
    assert A.thirteen == 13
  end
end