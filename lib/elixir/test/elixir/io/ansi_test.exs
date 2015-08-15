Code.require_file "../test_helper.exs", __DIR__

defmodule IO.ANSITest do
  use ExUnit.Case, async: true

  test "format ansicode" do
    assert IO.chardata_to_string(IO.ANSI.format(:green, true)) ==
           "#{IO.ANSI.green}#{IO.ANSI.reset}"
    assert IO.chardata_to_string(IO.ANSI.format(:green, false)) ==
           ""
  end

  test "format binary" do
    assert IO.chardata_to_string(IO.ANSI.format("Hello, world!", true)) ==
           "Hello, world!"
    assert IO.chardata_to_string(IO.ANSI.format("A map: %{foo: :bar}", false)) ==
           "A map: %{foo: :bar}"
  end

  test "format empty list" do
    assert IO.chardata_to_string(IO.ANSI.format([], true)) ==
           ""
    assert IO.chardata_to_string(IO.ANSI.format([], false)) ==
           ""
  end

  test "format ansicode list" do
    assert IO.chardata_to_string(IO.ANSI.format([:red, :bright], true)) ==
           "#{IO.ANSI.red}#{IO.ANSI.bright}#{IO.ANSI.reset}"
    assert IO.chardata_to_string(IO.ANSI.format([:red, :bright], false)) ==
           ""
  end

  test "format binary list" do
    assert IO.chardata_to_string(IO.ANSI.format(["Hello, ", "world!"], true)) ==
           "Hello, world!"
    assert IO.chardata_to_string(IO.ANSI.format(["Hello, ", "world!"], false)) ==
           "Hello, world!"
  end

  test "format char list" do
    assert IO.chardata_to_string(IO.ANSI.format('Hello, world!', true)) ==
           "Hello, world!"
    assert IO.chardata_to_string(IO.ANSI.format('Hello, world!', false)) ==
           "Hello, world!"
  end

  test "format mixed list" do
    data = ["Hello", ?,, 32, :red, "world!"]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.chardata_to_string(IO.ANSI.format(data, false)) ==
           "Hello, world!"
  end

  test "format nested list" do
    data = ["Hello, ", ["nested", 32, :red, "world!"]]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
           "Hello, nested #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.chardata_to_string(IO.ANSI.format(data, false)) ==
           "Hello, nested world!"
  end

  test "format improper list" do
    data = ["Hello, ", :red, "world" | "!"]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.chardata_to_string(IO.ANSI.format(data, false)) ==
           "Hello, world!"
  end

  test "format nested improper list" do
    data = [["Hello, " | :red], "world!" | :green]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.green}#{IO.ANSI.reset}"
    assert IO.chardata_to_string(IO.ANSI.format(data, false)) ==
           "Hello, world!"
  end

  test "format fragment" do
    assert IO.chardata_to_string(IO.ANSI.format_fragment([:red, "Hello!"], true)) ==
           "#{IO.ANSI.red}Hello!"
  end

  test "format invalid sequence" do
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.format([:brigh, "Hello!"], true)
    end
  end
end
