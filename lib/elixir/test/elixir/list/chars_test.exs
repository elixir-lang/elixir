Code.require_file "../test_helper.exs", __DIR__

defmodule List.Chars.AtomTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_char_list(:foo) == 'foo'
  end
end

defmodule List.Chars.BitStringTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_char_list("foo") == 'foo'
  end
end

defmodule List.Chars.NumberTest do
  use ExUnit.Case, async: true

  test :integer do
    assert to_char_list(1) == '1'
  end

  unless :erlang.system_info(:otp_release) < 'R16' do
    test :float do
      assert to_char_list(1.0) == '1.0'
    end
  end
end

defmodule List.Chars.ListTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_char_list([ 1, "b", 3 ]) == [1, "b", 3]
  end
end
