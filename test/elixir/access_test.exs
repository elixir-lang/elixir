Code.require_file "../test_helper", __FILE__

defmodule Access.TupleTest do
  use ExUnit.Case

  defrecord Config, other: { :a, :b, :c }

  test :literal do
    assert_equal :a, { :a, :b, :c }[1]
    assert_equal :a, Config.new.other[1]
  end

  test :positive_integer do
    tuple = { :a, :b, :c }
    assert_equal nil, tuple[0]
    assert_equal :a, tuple[1]
    assert_equal :b, tuple[2]
    assert_equal :c, tuple[3]
    assert_equal nil, tuple[4]
  end

  test :negative_integer do
    tuple = { :a, :b, :c }
    assert_equal nil, tuple[-4]
    assert_equal :a, tuple[-3]
    assert_equal :b, tuple[-2]
    assert_equal :c, tuple[-1]
  end

  test :access do
    assert_equal :c, Tuple.access({ :a, :b, :c }, -1)
  end
end

defmodule Access.ListTest do
  use ExUnit.Case

  test :literal do
    assert_equal 'a', 'abc'[%r(a)]
  end

  test :regex do
    list = 'abc'
    assert_equal 'b', list[%r(b)]
    assert_equal nil, list[%r(d)]
  end

  test :atom do
    list = [foo: "bar"]
    assert_equal "bar", list[:foo]
    assert_equal nil, list[:bar]
  end

  test :key do
    list = [{{:bar}, 2}, {'foo', 1}, {"key", 0}]
    assert_equal 0, list["key"]
    assert_equal 1, list['foo']
    assert_equal 2, list[{:bar}]
    assert_equal nil, list['baz']
  end

  test :access do
    assert_equal :bar, List.access([foo: :bar ], :foo)
  end
end

defmodule Access.BinaryTest do
  use ExUnit.Case

  test :literal do
    assert_equal "a", "abc"[%r(a)]
  end

  test :regex do
    binary = "abc"
    assert_equal "b", binary[%r(b)]
    assert_equal nil, binary[%r(d)]
  end

  test :access do
    assert_equal "a", Binary.access("abc", %r"a")
  end
end

defmodule Access.AtomTest do
  use ExUnit.Case

  defrecord Config, integer: 0

  test :keywords do
    assert_equal { Config, 0 }, Config[]
    assert_equal { Config, 1 }, Config[integer: 1]
  end

  test :in_guard_with_variable do
    assert_equal 0, get_var(Config.new)
    assert_equal 1, get_var(Config.new(integer: 1))
  end

  test :in_guard_with_record_match do
    assert_equal true, is_config(Config.new)
    assert_equal false, is_config({ Access.AtomTest, 1 })
    assert_equal false, is_config({ Config, 1, 2 })
  end

  test :in_guard_with_field_match do
    assert_equal true, is_zero(Config.new)
    assert_equal false, is_zero(Config.new(integer: 1))
  end

  test :match do
    assert_match Config[integer: 1], Config.new(integer: 1)
    refute_match Config[integer: 1], Config.new(integer: 0)
  end

  defp get_var(Config[integer: integer]) do
    integer
  end

  defp is_zero(Config[integer: 0]), do: true
  defp is_zero(Config[integer: _]), do: false

  defp is_config(Config[]), do: true
  defp is_config(_), do: false
end

defmodule Access.FunctionTest do
  use ExUnit.Case

  test :any do
    function = fn(x) -> x == :foo end
    assert_equal true, function[:foo]
    assert_equal false, function[:bar]
  end
end
