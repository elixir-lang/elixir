Code.require_file "../../test_helper", __FILE__

defmodule Kernel.AtomToBinaryTest do
  use ExUnit.Case

  test :atom_to_binary_defaults_to_utf8 do
    expected  = atom_to_binary :some_binary, :utf8
    actual    = atom_to_binary :some_binary

    assert_equal expected, actual
    assert_equal "another_atom", atom_to_binary :another_atom
  end

  test :binary_to_atom_defaults_to_utf8 do
    expected  = binary_to_atom "some_binary", :utf8
    actual    = binary_to_atom "some_binary"

    assert_equal expected, actual
    assert_equal :another_binary, binary_to_atom "another_binary"
  end

  test :binary_to_existing_atom_defaults_to_utf8 do
    expected = binary_to_atom "existing_atom", :utf8
    actual   = binary_to_existing_atom "existing_atom"

    assert_equal expected, actual

    :existing_atom
    assert_equal :existing_atom, binary_to_existing_atom "existing_atom"

    assert_raises ArgumentError, fn ->
      binary_to_existing_atom "nonexisting_atom"
    end
  end
end
