module Inspect::AtomTest do
  use ExUnit::Case

  def test_basic do
    ":foo" = inspect(:foo)
    "foo"  = stringify(:foo)
  end

  def test_with_uppercase do
    ":fOO" = inspect(:fOO)
    "fOO"  = stringify(:fOO)
  end

  def test_reference_atom do
    "::Foo::Bar"  = inspect(::Foo::Bar)
    "::Foo::Bar"  = stringify(::Foo::Bar)
  end
end

module Inspect::AnyTest do
  use ExUnit::Case

  def test_basic do
    bin = inspect(fn(x){ x + 1 })
    '#Fun<' ++ _ = binary_to_list(bin)
  end
end