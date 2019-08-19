Code.require_file("../test_helper.exs", __DIR__)

defmodule List.Chars.AtomTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_charlist(:foo) == 'foo'
  end

  test "true false nil" do
    assert to_charlist(false) == 'false'
    assert to_charlist(true) == 'true'
    assert to_charlist(nil) == ''
  end
end

defmodule List.Chars.BitStringTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_charlist("foo") == 'foo'
  end
end

defmodule List.Chars.NumberTest do
  use ExUnit.Case, async: true

  test "integer" do
    assert to_charlist(1) == '1'
  end

  test "float" do
    assert to_charlist(1.0) == '1.0'
  end
end

defmodule List.Chars.ListTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_charlist([1, "b", 3]) == [1, "b", 3]
  end
end
