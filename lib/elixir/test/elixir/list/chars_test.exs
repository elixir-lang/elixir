Code.require_file("../test_helper.exs", __DIR__)

defmodule List.Chars.AtomTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_charlist(:foo) == ~c"foo"
  end

  test "true false nil" do
    assert to_charlist(false) == ~c"false"
    assert to_charlist(true) == ~c"true"
    assert to_charlist(nil) == ~c""
  end

  test "atoms with special characters" do
    assert to_charlist(:'foo@bar') == ~c"foo@bar"
    assert to_charlist(:'foo-bar') == ~c"foo-bar"
  end

  test "uppercase atoms" do
    assert to_charlist(:FOO) == ~c"FOO"
  end

  test "long atoms" do
    long_atom = :erlang.list_to_atom(String.duplicate("a", 1000))
    assert to_charlist(long_atom) == String.duplicate("a", 1000) |> to_charlist()
  end
end

defmodule List.Chars.BitStringTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_charlist("foo") == ~c"foo"
  end
end

defmodule List.Chars.NumberTest do
  use ExUnit.Case, async: true

  test "integer" do
    assert to_charlist(1) == ~c"1"
  end

  test "float" do
    assert to_charlist(1.0) == ~c"1.0"
  end
end

defmodule List.Chars.ListTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_charlist([1, "b", 3]) == [1, "b", 3]
  end
end
