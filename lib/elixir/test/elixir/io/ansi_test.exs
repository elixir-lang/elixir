Code.require_file "../test_helper.exs", __DIR__

defmodule IO.ANSITest do
  use ExUnit.Case, async: true

  test :format_ansicode do
    assert IO.ANSI.format(:green, true) ==
           "#{IO.ANSI.green}#{IO.ANSI.reset}"
    assert IO.ANSI.format(:green, false) ==
           ""
  end

  test :format_binary do
    assert IO.ANSI.format("Hello, world!", true) ==
           "Hello, world!"
    assert IO.ANSI.format("This is a map: %{foo: :bar}", false) ==
           "This is a map: %{foo: :bar}"
  end

  test :format_empty_list do
    assert IO.ANSI.format([], true) ==
           ""
    assert IO.ANSI.format([], false) ==
           ""
  end

  test :format_ansicode_list do
    assert IO.ANSI.format([:red, :bright], true) ==
           "#{IO.ANSI.red}#{IO.ANSI.bright}#{IO.ANSI.reset}"
    assert IO.ANSI.format([:red, :bright], false) ==
           ""
  end

  test :format_binary_list do
    assert IO.ANSI.format(["Hello, ", "world!"], true) ==
           "Hello, world!"
    assert IO.ANSI.format(["Hello, ", "world!"], false) ==
           "Hello, world!"
  end

  test :format_char_list do
    assert IO.ANSI.format('Hello, world!', true) ==
           "Hello, world!"
    assert IO.ANSI.format('Hello, world!', false) ==
           "Hello, world!"
  end

  test :format_mixed_list do
    assert IO.ANSI.format(["Hello", ?,, 32, :red, "world!"], true) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.ANSI.format(["Hello", ?,, 32, :red, "world!"], false) ==
           "Hello, world!"
  end

  test :format_nested_list do
    assert IO.ANSI.format(["Hello, ", ["nested", 32, :red, "world!"]], true) ==
           "Hello, nested #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.ANSI.format(["Hello, ", ["nested", 32, :red, "world!"]], false) ==
           "Hello, nested world!"
  end

  test :format_fragment do
    assert IO.ANSI.format_fragment([:red, "Hello!"], true) ==
           "#{IO.ANSI.red}Hello!"
  end

  test :format_invalid_sequence do
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.format([:brigh, "Hello!"], true)
    end
  end

  test :escape_single do
    assert IO.ANSI.escape("Hello, %{red}world!", true) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello, %{red}world!", true) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
  end

  test :escape_non_attribute do
    assert IO.ANSI.escape("Hello %{clear}world!", true) ==
           "Hello #{IO.ANSI.clear}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello %{home}world!", true) ==
           "Hello #{IO.ANSI.home}world!#{IO.ANSI.reset}"
  end

  test :escape_multiple do
    assert IO.ANSI.escape("Hello, %{red,bright}world!", true) ==
           "Hello, #{IO.ANSI.red}#{IO.ANSI.bright}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello, %{red, bright}world!", true) ==
           "Hello, #{IO.ANSI.red}#{IO.ANSI.bright}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello, %{red , bright}world!", true) ==
           "Hello, #{IO.ANSI.red}#{IO.ANSI.bright}world!#{IO.ANSI.reset}"
  end

  test :escape_no_emit do
    assert IO.ANSI.escape("Hello, %{}world!", false) ==
           "Hello, world!"

    assert IO.ANSI.escape("Hello, %{red,bright}world!", false) ==
           "Hello, world!"
  end

  test :escape_fragment do
    assert IO.ANSI.escape("%{red}", true) == "#{IO.ANSI.red}#{IO.ANSI.reset}"
    assert IO.ANSI.escape_fragment("", true) == ""
  end

  test :escape_noop do
    assert IO.ANSI.escape("") == ""
  end

  test :escape_invalid do
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.escape("%{brigh}, yes")
    end
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.escape("%{brigh,red}, yes")
    end
  end
end
