Code.require_file "../test_helper.exs", __DIR__

defmodule IO.ANSITest do
  use ExUnit.Case, async: true

  test :escape_single do
    assert IO.ANSI.escape("Hello, %{red}world!", true) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello, %{ red }world!", true) ==
           "Hello, #{IO.ANSI.red}world!#{IO.ANSI.reset}"
  end

  test :escape_multiple do
    assert IO.ANSI.escape("Hello, %{red,bright}world!", true) ==
           "Hello, #{IO.ANSI.red}#{IO.ANSI.bright}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello, %{red, bright}world!", true) ==
           "Hello, #{IO.ANSI.red}#{IO.ANSI.bright}world!#{IO.ANSI.reset}"
    assert IO.ANSI.escape("Hello, %{red , bright}world!", true) ==
           "Hello, #{IO.ANSI.red}#{IO.ANSI.bright}world!#{IO.ANSI.reset}"
  end

  test :no_emit do
    assert IO.ANSI.escape("Hello, %{red,bright}world!", false) ==
           "Hello, world!"
  end

  test :fragment do
    assert IO.ANSI.escape("%{red}", true) == "#{IO.ANSI.red}#{IO.ANSI.reset}"
    assert IO.ANSI.escape_fragment("", true) == ""
  end

  test :noop do
    assert IO.ANSI.escape("") == ""
  end

  test :invalid do
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.escape("%{brigh}, yes")
    end
    assert_raise ArgumentError, "invalid ANSI sequence specification: brigh", fn ->
      IO.ANSI.escape("%{brigh,red}, yes")
    end
  end
end
