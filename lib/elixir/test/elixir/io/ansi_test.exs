Code.require_file "../test_helper.exs", __DIR__

defmodule IO.ANSITest do
  use ExUnit.Case, async: true

  test :escape_ansicode do
    assert IO.ANSI.escape(:green, true) ==
           "#{IO.ANSI.green}#{IO.ANSI.reset}"
    assert IO.ANSI.escape(:green, false) ==
           ""
  end

  test :escape_binary do
    assert IO.ANSI.escape("Hello, world!", true) ==
           "Hello, world!"
    assert IO.ANSI.escape("This is a map: %{foo: :bar}", false) ==
           "This is a map: %{foo: :bar}"
  end

  test :escape_empty_list do
    assert IO.ANSI.escape([], true) ==
           ""
    assert IO.ANSI.escape([], false) ==
           ""
  end

  test :escape_ansicode_list do
    assert IO.ANSI.escape([:red, :bright], true) ==
           "#{IO.ANSI.red}#{IO.ANSI.bright}#{IO.ANSI.reset}"
    assert IO.ANSI.escape([:red, :bright], false) ==
           ""
  end

  test :escape_binary_list do
    assert IO.ANSI.escape(["Hello, ", "world!"], true) ==
           "Hello, world!"
    assert IO.ANSI.escape(["Hello, ", "world!"], false) ==
           "Hello, world!"
  end

  test :escape_char_list do
    assert IO.ANSI.escape('Hello, world!', true) ==
           "Hello, world!"
    assert IO.ANSI.escape('Hello, world!', false) ==
           "Hello, world!"
  end

  test :escape_mixed_list do
    assert IO.ANSI.escape(["Hello", ?,, 32, :red, "world!"], true) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape(["Hello", ?,, 32, :red, "world!"], false) ==
           "Hello, world!"
  end

  test :escape_nested_list do
    assert IO.ANSI.escape(["Hello, ", ["nested", 32, :red, "world!"]], true) ==
           "Hello, nested #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape(["Hello, ", ["nested", 32, :red, "world!"]], false) ==
           "Hello, nested world!"
  end

  test :escape_fragment do
    assert IO.ANSI.escape_fragment([:red, "Hello!"], true) ==
           "#{IO.ANSI.red}Hello!"
  end

  test :invalid_sequence do
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.escape([:brigh, "Hello!"], true)
    end
  end
end
