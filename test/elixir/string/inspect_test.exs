Code.require_file "../../test_helper", __FILE__

defmodule String::Inspect::AtomTest do
  use ExUnit::Case

  def test_basic do
    assert_equal ":foo", inspect(:foo)
    assert_equal "foo", to_binary(:foo)
  end

  def test_empty do
    assert_equal ":\"\"", inspect(:"")
    assert_equal "", to_binary(:"")
  end

  def test_true_false_nil do
    assert_equal "false", inspect(false)
    assert_equal "true", inspect(true)
    assert_equal "nil", inspect(nil)

    assert_equal "false", to_binary(false)
    assert_equal "true", to_binary(true)
    assert_equal "", to_binary(nil)
  end

  def test_with_uppercase do
    assert_equal ":fOO", inspect(:fOO)
    assert_equal "fOO", to_binary(:fOO)
    assert_equal ":FOO", inspect(:FOO)
    assert_equal "FOO", to_binary(:FOO)
  end

  def test_reference_atom do
    assert_equal "::Foo::Bar", inspect(::Foo::Bar)
    assert_equal "::Foo::Bar", to_binary(::Foo::Bar)
  end

  def test_impl do
    assert_equal String::Inspect, String::Inspect::Atom.__impl__
  end
end

defmodule String::Inspect::BitStringTest do
  use ExUnit::Case

  def test_bitstring do
    assert_equal "<<0,1|4>>", inspect(<<1|12-integer-signed>>)
    assert_equal "<<0,1|4>>", to_binary(<<1|12-integer-signed>>)
  end

  def test_binary do
    assert_equal "\"foo\"", inspect("foo")
    assert_equal "foo", to_binary("foo")

    assert_equal "\"abc\"", inspect(<<?a, ?b, ?c>>)
    assert_equal "abc", to_binary(<<?a, ?b, ?c>>)

    assert_equal "我今天要学习.", to_binary("我今天要学习.")
  end

  def test_escape do
    assert_equal "\"f\\no\"" , inspect("f\no")
    assert_equal "\"f\\\\o\"", inspect("f\\o")
  end

  def test_unprintable do
    assert_equal "<<1>>", inspect(<<1>>)
  end
end

defmodule String::Inspect::NumberTest do
  use ExUnit::Case

  def test_integer do
    assert_equal "100", inspect(100)
    assert_equal "100", to_binary(100)
  end

  def test_float do
    assert_equal "1.00000000000000000000e+00", inspect(1.0)
    assert_equal "1.00000000000000000000e+00", to_binary(1.0)

    assert_equal "1.00000000000000000000e+10", inspect(1.0e10)
    assert_equal "1.00000000000000000000e+10", to_binary(1.0e10)

    assert_equal "1.00000000000000000000e+10", inspect(1.0e+10)
    assert_equal "1.00000000000000000000e+10", to_binary(1.0e+10)
  end
end

defmodule String::Inspect::TupleTest do
  use ExUnit::Case

  def test_basic do
    assert_equal "{1,\"b\",3}", inspect({ 1, "b", 3 })
    assert_equal "{1,\"b\",3}", to_binary({ 1, "b", 3 })
  end

  def test_record_like do
    assert_equal "{:foo,:bar}", inspect({ :foo, :bar })
    assert_equal "{:foo,:bar}", to_binary({ :foo, :bar })
  end

  def test_exception do
    assert_equal "::RuntimeError{\"runtime error\"}", inspect(RuntimeError.new)
    assert_equal "::RuntimeError{\"runtime error\"}", to_binary(RuntimeError.new)
  end

  def test_empty do
    assert_equal "{}", inspect({})
    assert_equal "{}", to_binary({})
  end
end

defmodule String::Inspect::ListTest do
  use ExUnit::Case

  def test_basic do
    assert_equal "[1,\"b\",3]", inspect([ 1, "b", 3 ])
    assert_equal <<1,98,3>>, to_binary([ 1, "b", 3 ])
  end

  def test_printable do
    assert_equal "'abc'", inspect('abc')
    assert_equal "abc"  , to_binary('abc')
  end

  def test_non_printable do
    assert_equal "[{:a,1}]", inspect([{:a,1}])
    assert_equal "[{:a,1}]", to_binary([{:a,1}])
  end

  def test_unproper do
    assert_equal "[:foo|:bar]", inspect([:foo | :bar])
    assert_equal "[:foo|:bar]", to_binary([:foo | :bar])
  end

  def test_empty do
    assert_equal "[]", inspect([])
    assert_equal "", to_binary([])
  end
end

defmodule String::Inspect::AnyTest do
  use ExUnit::Case

  def test_funs do
    bin = inspect(fn(x, do: x + 1))
    '#Fun<' ++ _ = binary_to_list(bin)

    bin = to_binary(fn(x, do: x + 1))
    '#Fun<' ++ _ = binary_to_list(bin)
  end
end
