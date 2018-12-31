Code.require_file("../test_helper.exs", __DIR__)

defmodule String.Chars.AtomTest do
  use ExUnit.Case, async: true

  doctest String.Chars

  test "basic" do
    assert to_string(:foo) == "foo"
  end

  test "empty" do
    assert to_string(:"") == ""
  end

  test "true false nil" do
    assert to_string(false) == "false"
    assert to_string(true) == "true"
    assert to_string(nil) == ""
  end

  test "with uppercase" do
    assert to_string(:fOO) == "fOO"
    assert to_string(:FOO) == "FOO"
  end

  test "alias atom" do
    assert to_string(Foo.Bar) == "Elixir.Foo.Bar"
  end
end

defmodule String.Chars.BitStringTest do
  use ExUnit.Case, async: true

  test "binary" do
    assert to_string("foo") == "foo"
    assert to_string(<<?a, ?b, ?c>>) == "abc"
    assert to_string("我今天要学习.") == "我今天要学习."
  end
end

defmodule String.Chars.NumberTest do
  use ExUnit.Case, async: true

  test "integer" do
    assert to_string(100) == "100"
  end

  test "float" do
    assert to_string(1.0) == "1.0"
    assert to_string(1.0e10) == "1.0e10"
  end
end

defmodule String.Chars.ListTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert to_string([1, "b", 3]) == <<1, 98, 3>>
  end

  test "printable" do
    assert to_string('abc') == "abc"
  end

  test "charlist" do
    assert to_string([0, 1, 2, 3, 255]) == <<0, 1, 2, 3, 195, 191>>

    assert to_string([0, [1, "hello"], 2, [["bye"]]]) ==
             <<0, 1, 104, 101, 108, 108, 111, 2, 98, 121, 101>>
  end

  test "empty" do
    assert to_string([]) == ""
  end
end

defmodule String.Chars.Version.RequirementTest do
  use ExUnit.Case, async: true

  test "version requirement" do
    {:ok, requirement} = Version.parse_requirement("== 2.0.1")
    assert String.Chars.to_string(requirement) == "== 2.0.1"
  end
end

defmodule String.Chars.URITest do
  use ExUnit.Case, async: true

  test "uri" do
    uri = URI.parse("http://google.com")
    assert String.Chars.to_string(uri) == "http://google.com"

    uri_no_host = URI.parse("/foo/bar")
    assert String.Chars.to_string(uri_no_host) == "/foo/bar"
  end
end

defmodule String.Chars.ErrorsTest do
  use ExUnit.Case, async: true

  defmodule Foo do
    defstruct foo: "bar"
  end

  test "bitstring" do
    message =
      "protocol String.Chars not implemented for <<0, 1::size(4)>> of type BitString, cannot convert a bitstring to a string"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string(<<1::size(12)-integer-signed>>)
    end
  end

  test "tuple" do
    message = "protocol String.Chars not implemented for {1, 2, 3} of type Tuple"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string({1, 2, 3})
    end
  end

  test "PID" do
    message = ~r"^protocol String\.Chars not implemented for #PID<.+?> of type PID$"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string(self())
    end
  end

  test "ref" do
    message = ~r"^protocol String\.Chars not implemented for #Reference<.+?> of type Reference$"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string(make_ref()) == ""
    end
  end

  test "function" do
    message = ~r"^protocol String\.Chars not implemented for #Function<.+?> of type Function$"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string(fn -> nil end)
    end
  end

  test "port" do
    [port | _] = Port.list()
    message = ~r"^protocol String\.Chars not implemented for #Port<.+?> of type Port$"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string(port)
    end
  end

  test "user-defined struct" do
    message =
      "protocol String\.Chars not implemented for %String.Chars.ErrorsTest.Foo{foo: \"bar\"} of type String.Chars.ErrorsTest.Foo (a struct)"

    assert_raise Protocol.UndefinedError, message, fn ->
      to_string(%Foo{})
    end
  end
end
