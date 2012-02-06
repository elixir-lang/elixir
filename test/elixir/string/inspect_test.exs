Code.require_file "../../test_helper", __FILE__

defmodule String::Inspect::AtomTest do
  use ExUnit::Case

  def test_basic do
    ":foo" = inspect(:foo)
    "foo"  = to_binary(:foo)
  end

  def test_empty do
    ":\"\"" = inspect(:"")
    ""     = to_binary(:"")
  end

  def test_true_false_nil do
    "false" = inspect(false)
    "true"  = inspect(true)
    "nil"   = inspect(nil)

    "false" = to_binary(false)
    "true"  = to_binary(true)
    ""      = to_binary(nil)
  end

  def test_with_uppercase do
    ":fOO" = inspect(:fOO)
    "fOO"  = to_binary(:fOO)
    ":FOO" = inspect(:FOO)
    "FOO"  = to_binary(:FOO)
  end

  def test_reference_atom do
    "::Foo::Bar" = inspect(::Foo::Bar)
    "::Foo::Bar" = to_binary(::Foo::Bar)
  end

  def test_impl do
    String::Inspect = String::Inspect::Atom.__impl__
  end
end

defmodule String::Inspect::BitStringTest do
  use ExUnit::Case

  def test_bitstring do
    "<<0,1|4>>" = inspect(<<1|12-integer-signed>>)
    "<<0,1|4>>" = to_binary(<<1|12-integer-signed>>)
  end

  def test_binary do
    "\"foo\"" = inspect("foo")
    "foo"     = to_binary("foo")

    "\"abc\"" = inspect(<<?a, ?b, ?c>>)
    "abc"     = to_binary(<<?a, ?b, ?c>>)

    "我今天要学习." = to_binary("我今天要学习.")
  end

  def test_escape do
    "\"f\\no\""  = inspect("f\no")
    "\"f\\\\o\"" = inspect("f\\o")
  end

  def test_unprintable do
    "<<1>>" = inspect(<<1>>)
  end
end

defmodule String::Inspect::NumberTest do
  use ExUnit::Case

  def test_integer do
    "100" = inspect(100)
    "100" = to_binary(100)
  end

  def test_float do
    "1.00000000000000000000e+00" = inspect(1.0)
    "1.00000000000000000000e+00" = to_binary(1.0)

    "1.00000000000000000000e+10" = inspect(1.0e10)
    "1.00000000000000000000e+10" = to_binary(1.0e10)

    "1.00000000000000000000e+10" = inspect(1.0e+10)
    "1.00000000000000000000e+10" = to_binary(1.0e+10)
  end
end

defmodule String::Inspect::TupleTest do
  use ExUnit::Case

  def test_basic do
    "{1,\"b\",3}" = inspect({ 1, "b", 3 })
    "{1,\"b\",3}" = to_binary({ 1, "b", 3 })
  end

  def test_record_like do
    "{:foo,:bar}" = inspect({ :foo, :bar })
    "{:foo,:bar}" = to_binary({ :foo, :bar })
  end

  def test_exception do
    "::RuntimeError{\"runtime error\"}" = inspect(RuntimeError.new)
    "::RuntimeError{\"runtime error\"}" = to_binary(RuntimeError.new)
  end

  def test_empty do
    "{}" = inspect({})
    "{}" = to_binary({})
  end
end

defmodule String::Inspect::ListTest do
  use ExUnit::Case

  def test_basic do
    "[1,\"b\",3]" = inspect([ 1, "b", 3 ])
    <<1,98,3>> = to_binary([ 1, "b", 3 ])
  end

  def test_printable do
    "'abc'" = inspect('abc')
    "abc"   = to_binary('abc')
  end

  def test_non_printable do
    "[{:a,1}]" = inspect([{:a,1}])
    "[{:a,1}]" = to_binary([{:a,1}])
  end

  def test_unproper do
    "[:foo|:bar]" = inspect([:foo | :bar])
    "[:foo|:bar]" = to_binary([:foo | :bar])
  end

  def test_empty do
    "[]" = inspect([])
    ""   = to_binary([])
  end
end

defmodule String::Inspect::AnyTest do
  use ExUnit::Case

  def test_funs do
    bin = inspect(fn(x){ x + 1 })
    '#Fun<' ++ _ = binary_to_list(bin)

    bin = to_binary(fn(x){ x + 1 })
    '#Fun<' ++ _ = binary_to_list(bin)
  end
end