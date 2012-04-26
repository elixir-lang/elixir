Code.require_file "../../test_helper", __FILE__

defmodule List.Chars.AtomTest do
  use ExUnit.Case

  test :basic do
    assert to_char_list(:foo) == 'foo'
  end
end

defmodule List.Chars.BitStringTest do
  use ExUnit.Case

  test :basic do
    assert to_char_list("foo") == 'foo'
  end
end

defmodule List.Chars.NumberTest do
  use ExUnit.Case

  test :integer do
    assert to_char_list(1) == '1'
  end

  test :float do
    assert to_char_list(1.0) == '1.00000000000000000000e+00'
  end
end

defmodule List.Chars.ListTest do
  use ExUnit.Case

  test :basic do
    assert to_char_list([ 1, "b", 3 ]) == [1, "b", 3]
  end
end
