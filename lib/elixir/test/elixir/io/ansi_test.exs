Code.require_file "../../test_helper.exs", __FILE__

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
    assert IO.ANSI.escape("") == "#{IO.ANSI.reset}"
    assert IO.ANSI.escape_fragment("") == ""
  end
end
