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