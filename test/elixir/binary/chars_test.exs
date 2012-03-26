Code.require_file "../../test_helper", __FILE__

defmodule Binary.Chars.AtomTest do
  use ExUnit.Case

  test :basic do
    assert_equal "foo", to_binary(:foo)
  end

  test :empty do
    assert_equal "", to_binary(:"")
  end

  test :true_false_nil do
    assert_equal "false", to_binary(false)
    assert_equal "true", to_binary(true)
    assert_equal "", to_binary(nil)
  end

  test :with_uppercase do
    assert_equal "fOO", to_binary(:fOO)
    assert_equal "FOO", to_binary(:FOO)
  end

  test :reference_atom do
    assert_equal "__MAIN__.Foo.Bar", to_binary(Foo.Bar)
  end
end

defmodule Binary.Chars.BitStringTest do
  use ExUnit.Case

  test :bitstring do
    assert_raises FunctionClauseError, fn ->
      to_binary(<<1|12-integer-signed>>)
    end
  end

  test :binary do
    assert_equal "foo", to_binary("foo")
    assert_equal "abc", to_binary(<<?a, ?b, ?c>>)
    assert_equal "我今天要学习.", to_binary("我今天要学习.")
  end
end

defmodule Binary.Chars.NumberTest do
  use ExUnit.Case

  test :integer do
    assert_equal "100", to_binary(100)
  end

  test :float do
    assert_equal "1.00000000000000000000e+00", to_binary(1.0)
    assert_equal "1.00000000000000000000e+10", to_binary(1.0e10)
    assert_equal "1.00000000000000000000e+10", to_binary(1.0e+10)
  end
end

defmodule Binary.Chars.ListTest do
  use ExUnit.Case

  test :basic do
    assert_equal <<1,98,3>>, to_binary([ 1, "b", 3 ])
  end

  test :printable do
    assert_equal "abc"  , to_binary('abc')
  end

  test :empty do
    assert_equal "", to_binary([])
  end
end
