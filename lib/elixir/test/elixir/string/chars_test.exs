Code.require_file "../test_helper.exs", __DIR__

defmodule String.Chars.AtomTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_string(:foo) == "foo"
  end

  test :empty do
    assert to_string(:"") == ""
  end

  test :true_false_nil do
    assert to_string(false) == "false"
    assert to_string(true) == "true"
    assert to_string(nil) == ""
  end

  test :with_uppercase do
    assert to_string(:fOO) == "fOO"
    assert to_string(:FOO) == "FOO"
  end

  test :alias_atom do
    assert to_string(Foo.Bar) == "Elixir.Foo.Bar"
  end
end

defmodule String.Chars.BitStringTest do
  use ExUnit.Case, async: true

  test :binary do
    assert to_string("foo") == "foo"
    assert to_string(<<?a, ?b, ?c>>) == "abc"
    assert to_string("我今天要学习.") == "我今天要学习."
  end
end

defmodule String.Chars.NumberTest do
  use ExUnit.Case, async: true

  test :integer do
    assert to_string(100) == "100"
  end

  test :float do
    assert to_string(1.0) == "1.0"
    assert to_string(1.0e10) == "1.0e10"
  end
end

defmodule String.Chars.ListTest do
  use ExUnit.Case, async: true

  test :basic do
    assert to_string([ 1, "b", 3 ]) == <<1, 98, 3>>
  end

  test :printable do
    assert to_string('abc') == "abc"
  end

  test :char_list do
    assert to_string([0, 1, 2, 3, 255]) ==
           <<0, 1, 2, 3, 195, 191>>

    assert to_string([0, [1, "hello"], 2, [["bye"]]]) ==
           <<0, 1, 104, 101, 108, 108, 111, 2, 98, 121, 101>>
  end

  test :empty do
    assert to_string([]) == ""
  end
end

defmodule String.Chars.ErrorsTest do
  use ExUnit.Case, async: true

  test :bitstring do
    assert_raise Protocol.UndefinedError,
                 "protocol String.Chars not implemented for <<0, 1::size(4)>>, " <>
                 "cannot convert a bitstring to a string", fn ->
      to_string(<<1 :: [size(12), integer, signed]>>)
    end
  end

  test :tuple do
    assert_raise Protocol.UndefinedError, "protocol String.Chars not implemented for {1, 2, 3}", fn ->
      to_string({1, 2, 3})
    end
  end

  test :record do
    assert_raise UndefinedFunctionError, "undefined function: String.Chars.ArgumentError.to_string/1", fn ->
      to_string(ArgumentError[])
    end

    assert_raise UndefinedFunctionError, "undefined function: String.Chars.File.Stat.to_string/1", fn ->
      to_string(File.Stat[])
    end
  end

  test :pid do
    assert_raise Protocol.UndefinedError, %r"^protocol String\.Chars not implemented for #PID<.+?>$", fn ->
      to_string(self())
    end
  end

  test :ref do
    assert_raise Protocol.UndefinedError, %r"^protocol String\.Chars not implemented for #Reference<.+?>$", fn ->
      to_string(make_ref()) == ""
    end
  end

  test :function do
    assert_raise Protocol.UndefinedError, %r"^protocol String\.Chars not implemented for #Function<.+?>$", fn ->
      to_string(fn -> end)
    end
  end

  test :port do
    [port|_] = Port.list
    assert_raise Protocol.UndefinedError, %r"^protocol String\.Chars not implemented for #Port<.+?>$", fn ->
      to_string(port)
    end
  end
end
