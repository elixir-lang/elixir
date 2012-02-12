Code.require_file "../../test_helper", __FILE__

defmodule String::Chars::AtomTest do
  use ExUnit::Case

  def test_basic do
    assert_equal "foo", to_binary(:foo)
  end

  def test_empty do
    assert_equal "", to_binary(:"")
  end

  def test_true_false_nil do
    assert_equal "false", to_binary(false)
    assert_equal "true", to_binary(true)
    assert_equal "nil", to_binary(nil)
  end

  def test_with_uppercase do
    assert_equal "fOO", to_binary(:fOO)
    assert_equal "FOO", to_binary(:FOO)
  end

  def test_reference_atom do
    assert_equal "::Foo::Bar", to_binary(::Foo::Bar)
  end
end

defmodule String::Chars::BitStringTest do
  use ExUnit::Case

  # TODO: Use flunk or assert_raises
  def test_bitstring do
    assert_equal "<<0,1|4>>", to_binary(<<1|12-integer-signed>>)
  rescue: FunctionClauseError
  end

  def test_binary do
    assert_equal "foo", to_binary("foo")
    assert_equal "abc", to_binary(<<?a, ?b, ?c>>)
    assert_equal "我今天要学习.", to_binary("我今天要学习.")
  end
end

defmodule String::Chars::NumberTest do
  use ExUnit::Case

  def test_integer do
    assert_equal "100", to_binary(100)
  end

  def test_float do
    assert_equal "1.00000000000000000000e+00", to_binary(1.0)
    assert_equal "1.00000000000000000000e+10", to_binary(1.0e10)
    assert_equal "1.00000000000000000000e+10", to_binary(1.0e+10)
  end
end

defmodule String::Chars::ListTest do
  use ExUnit::Case

  def test_basic do
    assert_equal <<1,98,3>>, to_binary([ 1, "b", 3 ])
  end

  def test_printable do
    assert_equal "abc"  , to_binary('abc')
  end

  def test_empty do
    assert_equal "", to_binary([])
  end
end