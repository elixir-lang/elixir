Code.require_file "../../test_helper", __FILE__

defmodule List::Inspect::AtomTest do
  use ExUnit::Case

  def test_basic do
    'foo' = to_char_list(:foo)
  end
end

defmodule List::Inspect::BitStringTest do
  use ExUnit::Case

  def test_basic do
    'foo' = to_char_list("foo")
  end
end

defmodule List::Inspect::TupleTest do
  use ExUnit::Case

  def test_basic do
    [1, "b", 3] = to_char_list(%{ 1, "b", 3 })
  end
end

defmodule List::Inspect::ListTest do
  use ExUnit::Case

  def test_basic do
    [1, "b", 3] = to_char_list([ 1, "b", 3 ])
  end
end
