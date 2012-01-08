Code.require_file "../test_helper", __FILE__

defmodule Inspect::AtomTest do
  use ExUnit::Case

  def test_basic do
    ":foo" = inspect(:foo)
    "foo"  = stringify(:foo)
  end

  def test_empty do
    ":\"\"" = inspect(:"")
    ""     = stringify(:"")
  end

  def test_true_false_nil do
    "false" = inspect(false)
    "true"  = inspect(true)
    "nil"   = inspect(nil)

    "false" = stringify(false)
    "true"  = stringify(true)
    ""      = stringify(nil)
  end

  def test_with_uppercase do
    ":fOO" = inspect(:fOO)
    "fOO"  = stringify(:fOO)
    ":FOO" = inspect(:FOO)
    "FOO"  = stringify(:FOO)
  end

  def test_reference_atom do
    "::Foo::Bar" = inspect(::Foo::Bar)
    "::Foo::Bar" = stringify(::Foo::Bar)
  end

  def test_impl do
    String::Inspect = String::Inspect::Atom.__impl__
  end
end

defmodule Inspect::BitStringTest do
  use ExUnit::Case

  def test_bitstring do
    "<<0,1|4>>" = inspect(<<1|12-integer-signed>>)
    "<<0,1|4>>" = stringify(<<1|12-integer-signed>>)
  end

  def test_binary do
    "\"foo\"" = inspect("foo")
    "foo"     = stringify("foo")

    "\"abc\"" = inspect(<<?a, ?b, ?c>>)
    "abc"     = stringify(<<?a, ?b, ?c>>)
  end

  def test_unprintable do
    "<<1>>" = inspect(<<1>>)
  end
end

defmodule Inspect::NumberTest do
  use ExUnit::Case

  def test_integer do
    "100" = inspect(100)
    "100" = stringify(100)
  end

  def test_float do
    "1.00000000000000000000e+00" = inspect(1.0)
    "1.00000000000000000000e+00" = stringify(1.0)

    "1.00000000000000000000e+10" = inspect(1.0e10)
    "1.00000000000000000000e+10" = stringify(1.0e10)

    "1.00000000000000000000e+10" = inspect(1.0e+10)
    "1.00000000000000000000e+10" = stringify(1.0e+10)
  end
end

defmodule Inspect::TupleTest do
  use ExUnit::Case

  def test_basic do
    "{1, \"b\", 3}" = inspect({ 1, "b", 3 })
    "{1, \"b\", 3}" = stringify({ 1, "b", 3 })
  end

  def test_record_like do
    "{:foo, :bar}" = inspect({ :foo, :bar })
    "{:foo, :bar}" = stringify({ :foo, :bar })
  end

  def test_empty do
    "{}" = inspect({})
    "{}" = stringify({})
  end
end

defmodule Inspect::ListTest do
  use ExUnit::Case

  def test_basic do
    "[1, \"b\", 3]" = inspect([ 1, "b", 3 ])
    "[1, \"b\", 3]" = stringify([ 1, "b", 3 ])
  end

  def test_printable do
    "'abc'" = inspect('abc')
    "'abc'" = stringify('abc')
  end

  def test_empty do
    "[]" = inspect([])
    "[]" = stringify([])
  end
end

defmodule Inspect::AnyTest do
  use ExUnit::Case

  def test_funs do
    bin = inspect(fn(x){ x + 1 })
    '#Fun<' ++ _ = binary_to_list(bin)

    bin = stringify(fn(x){ x + 1 })
    '#Fun<' ++ _ = binary_to_list(bin)
  end
end