Code.require_file "../test_helper.exs", __DIR__

defmodule Binary.Chars.AtomTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_binary(:foo) == "foo"
  end

  test :empty do
    assert to_binary(:"") == ""
  end

  test :true_false_nil do
    assert to_binary(false) == "false"
    assert to_binary(true) == "true"
    assert to_binary(nil) == ""
  end

  test :with_uppercase do
    assert to_binary(:fOO) == "fOO"
    assert to_binary(:FOO) == "FOO"
  end

  test :alias_atom do
    assert to_binary(Foo.Bar) == "Elixir.Foo.Bar"
  end
end

defmodule Binary.Chars.BitStringTest do
  use ExUnit.Case, async: true

  test :bitstring do
    assert_raise FunctionClauseError, fn ->
      to_binary(<<1 :: [size(12), integer, signed]>>)
    end
  end

  test :binary do
    assert to_binary("foo") == "foo"
    assert to_binary(<<?a, ?b, ?c>>) == "abc"
    assert to_binary("我今天要学习.") == "我今天要学习."
  end
end

defmodule Binary.Chars.NumberTest do
  use ExUnit.Case, async: true

  test :integer do
    assert to_binary(100) == "100"
  end

  test :float do
    assert to_binary(1.0) == "1.0"
    assert to_binary(1.0e10) == "1.0e10"
  end
end

defmodule Binary.Chars.ListTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_binary([ 1, "b", 3 ]) == <<1, 98, 3>>
  end

  test :printable do
    assert to_binary('abc') == "abc"
  end

  test :empty do
    assert to_binary([]) == ""
  end
end

defmodule Binary.Chars.ErrorsTest do
  use ExUnit.Case, async: true

  test :list do
    assert to_binary([0, 1, 2, 3, 255]) == <<0, 1, 2, 3, 255>>
    assert to_binary([0, [1, "hello"], 2, [["bye"]]]) == <<0, 1, 104, 101, 108, 108, 111, 2, 98, 121, 101>>

    assert_raise Protocol.UndefinedError, "protocol Binary.Chars not implemented for [256], only iolists are supported", fn ->
      to_binary([256])
    end
    assert_raise Protocol.UndefinedError, "protocol Binary.Chars not implemented for [1001, 10001, 100001], only iolists are supported", fn ->
      to_binary([1001, 10001, 100001])
    end
    assert_raise Protocol.UndefinedError, "protocol Binary.Chars not implemented for [:atom, 13, \"hello\"], only iolists are supported", fn ->
      to_binary([:atom, 13, "hello"])
    end
  end

  test :tuple do
    assert_raise Protocol.UndefinedError, "protocol Binary.Chars not implemented for {1, 2, 3}", fn ->
      to_binary({1, 2, 3})
    end
  end

  test :nested_in_tuple do
    assert_raise Protocol.UndefinedError, "protocol Binary.Chars not implemented for {1, [2], :atom}", fn ->
      to_binary({1, [2], :atom})
    end
  end

  test :nested_tuple do
    assert_raise Protocol.UndefinedError, "protocol Binary.Chars not implemented for [1, {[2], :atom}], only iolists are supported", fn ->
      to_binary([1, {[2], :atom}])
    end
  end

  test :nested_pid do
    assert_raise Protocol.UndefinedError, %r"^protocol Binary\.Chars not implemented for \[#PID<.+?>, :atom\]\, only iolists are supported$", fn ->
      to_binary([self(), :atom])
    end
  end

  test :nested_fun do
    assert_raise Protocol.UndefinedError, %r"^protocol Binary\.Chars not implemented for \[:atom, #Function<.+?>\]\, only iolists are supported$", fn ->
      to_binary([:atom, fn -> end])
    end
  end

  test :record do
    # FIXME: is this expected error message?
    assert_raise UndefinedFunctionError, "undefined function: Binary.Chars.ArgumentError.to_binary/1", fn ->
      to_binary(ArgumentError[])
    end

    # FIXME: is this expected error message?
    assert_raise UndefinedFunctionError, "undefined function: Binary.Chars.File.Stat.to_binary/1", fn ->
      to_binary(File.Stat[])
    end
  end

  test :pid do
    assert_raise Protocol.UndefinedError, %r"^protocol Binary\.Chars not implemented for #PID<.+?>$", fn ->
      to_binary(self())
    end
  end

  test :ref do
    assert_raise Protocol.UndefinedError, %r"^protocol Binary\.Chars not implemented for #Reference<.+?>$", fn ->
      to_binary(make_ref()) == ""
    end
  end

  test :function do
    assert_raise Protocol.UndefinedError, %r"^protocol Binary\.Chars not implemented for #Function<.+?>$", fn ->
      to_binary(fn -> end)
    end
  end

  test :port do
    [port|_] = Port.list
    assert_raise Protocol.UndefinedError, %r"^protocol Binary\.Chars not implemented for #Port<.+?>$", fn ->
      to_binary(port)
    end
  end
end
