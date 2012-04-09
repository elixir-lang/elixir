Code.require_file "../../test_helper", __FILE__

defmodule Kernel.AtomToBinaryTest do
  use ExUnit.Case

  test :default_to_utf8 do
    expected  = atom_to_binary :some_binary, :utf8
    actual    = atom_to_binary :some_binary

    assert_equal expected, actual
    assert_equal "another_atom", atom_to_binary :another_atom
  end

end
