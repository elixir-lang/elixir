Code.require_file "test_helper.exs", __DIR__

defmodule AtomTest do
  use ExUnit.Case, async: true

  doctest Atom

  test "to_string/1" do
    assert Atom.to_string(:"héllo") == "héllo"
  end

  test "to_charlist/1" do
    assert Atom.to_charlist(:"héllo") == 'héllo'
  end
end
