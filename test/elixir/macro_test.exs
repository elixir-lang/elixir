Code.require_file "../test_helper", __FILE__

defmodule Macro.ExternalTest do
  defmacro external do
    17 = __CALLER__.line
    __FILE__ = __CALLER__.file
    [line: 17, file: __FILE__] = __CALLER__.location
  end
end

defmodule MacroTest do
  use ExUnit.Case

  # Changing the lines above will make compilation
  # fail since we are assertnig on the caller lines
  require Macro.ExternalTest
  Macro.ExternalTest.external()

  test :escapes_tuples_with_size_different_than_two do
    assert { :{}, 0, [:a] } == Macro.escape({ :a })
    assert { :{}, 0, [:a, :b, :c] } == Macro.escape({ :a, :b, :c })
  end

  test :simply_returns_macros_with_size_equal_to_two do
    assert { :a, :b } == Macro.escape({ :a, :b })
  end

  test :returns_any_other_structure do
    assert [1,2,3] == Macro.escape([1,2,3])
  end

  test :works_recursively do
    assert [1,{:{}, 0, [:a,:b,:c]},3] == Macro.escape([1, { :a, :b, :c },3])
  end
end