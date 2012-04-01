Code.require_file "../test_helper", __FILE__

defmodule Access.TupleTest do
  use ExUnit.Case

  test :literal do
    assert_equal :a, { :a, :b, :c }[1]
  end

  test :positive_integer do
    tuple = { :a, :b, :c }
    assert_equal nil, tuple[0]
    assert_equal :a,  tuple[1]
    assert_equal :b,  tuple[2]
    assert_equal :c,  tuple[3]
    assert_equal nil, tuple[4]
  end

  test :negative_integer do
    tuple = { :a, :b, :c }
    assert_equal nil, tuple[-4]
    assert_equal :a,  tuple[-3]
    assert_equal :b,  tuple[-2]
    assert_equal :c,  tuple[-1]
  end

  test :access do
    assert_equal :c, Tuple.access { :a, :b, :c }, -1
  end
end

defmodule Access.ListTest do
  use ExUnit.Case

  test :literal do
    assert_equal :a, [ :a, :b, :c ][1]
  end

  test :positive_integer do
    list = [ :a, :b, :c ]
    assert_equal nil, list[0]
    assert_equal :a,  list[1]
    assert_equal :b,  list[2]
    assert_equal :c,  list[3]
    assert_equal nil, list[4]
  end

  test :negative_integer do
    list = [ :a, :b, :c ]
    assert_equal nil, list[-4]
    assert_equal :a,  list[-3]
    assert_equal :b,  list[-2]
    assert_equal :c,  list[-1]
  end

  test :re_pattern do
    list = 'abc'
    assert_equal 'b', list[%r(b)]
    assert_equal nil, list[%r(d)]
  end

  test :atom do
    list = [foo: "bar"]
    assert_equal "bar", list[:foo]
    assert_equal nil,   list[:bar]
  end

  test :access do
    assert_equal :c, List.access [ :a, :b, :c ], -1
  end
end

defmodule Access.BinaryTest do
  use ExUnit.Case

  test :literal do
    assert_equal ?a, "abc"[1]
  end

  test :positive_integer do
    binary = "abc"
    assert_equal nil, binary[0]
    assert_equal ?a,  binary[1]
    assert_equal ?b,  binary[2]
    assert_equal ?c,  binary[3]
    assert_equal nil, binary[4]
  end

  test :negative_integer do
    binary = "abc"
    assert_equal nil, binary[-4]
    assert_equal ?a,  binary[-3]
    assert_equal ?b,  binary[-2]
    assert_equal ?c,  binary[-1]
  end

  test :re_pattern do
    binary = "abc"
    assert_equal "b", binary[%r(b)]
    assert_equal nil, binary[%r(d)]
  end

  test :access do
    assert_equal ?c, Binary.access "abc", -1
  end
end

defrecord Access.Config, integer: 0

defmodule Access.AtomTest do
  use ExUnit.Case

  test :keywords do
    assert_equal { Access.Config, 0 }, Access.Config[]
    assert_equal { Access.Config, 1 }, Access.Config[integer: 1]
  end

  test :in_guard_with_variable do
    assert_equal 0, get_var(Access.Config.new)
    assert_equal 1, get_var(Access.Config.new(integer: 1))
  end

  test :in_guard_with_record_match do
    assert_equal true,  is_config(Access.Config.new)
    assert_equal false, is_config({ Access.AtomTest, 1 })
    assert_equal false, is_config({ Access.Config, 1, 2 })
  end

  test :in_guard_with_field_match do
    assert_equal true,  is_zero(Access.Config.new)
    assert_equal false, is_zero(Access.Config.new(integer: 1))
  end

  defp get_var(Access.Config[integer: integer]) do
    integer
  end

  defp is_zero(Access.Config[integer: 0]), do: true
  defp is_zero(Access.Config[integer: _]),  do: false

  defp is_config(Access.Config[]), do: true
  defp is_config(_), do: false
end

defmodule Access.FunctionTest do
  use ExUnit.Case

  test :any do
    function = fn(x) -> x == :foo end
    assert_equal true,  function[:foo]
    assert_equal false, function[:bar]
  end
end