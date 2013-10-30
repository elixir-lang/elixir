Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.AnsiDocsTest do
  use IEx.Case

  @colors [ enabled: true,
            doc_code: "cyan,bright",
            doc_inline_code: "cyan",
            doc_bold: "bright",
            doc_underline: "underline",
            doc_headings: "yellow,bright",
            doc_title: "reverse,yellow,bright" ]

  @opts [colors: @colors]

  def format(str, use_ansi // true) do
    cmd = "IEx.ANSIDocs.print(#{inspect str}, #{inspect use_ansi})"
    capture_iex(cmd, @opts)
  end

  test "non-ansi heading just uses an asterisk" do
    assert capture_iex("IEx.ANSIDocs.print_heading(\"wibble\", false)", []) == "* wibble"
  end

  test "ansi heading is formatted" do
    result = capture_iex("IEx.ANSIDocs.print_heading(\"wibble\", true)", @opts)
    assert String.starts_with?(result, "\e[0m\n\e[7m\e[33m\e[1m")
    assert String.ends_with?(result, "\e[0m\n\e[0m")
    assert String.contains?(result, " wibble ")
  end

  test "first level heading is converted" do
    result = format("# wibble\n\ntext\n")
    assert result == "\e[33m\e[1mWIBBLE\e[0m\n\e[0m\ntext\n\e[0m"
  end

  test "second level heading is converted" do
    result = format("## wibble\n\ntext\n")
    assert result == "\e[33m\e[1mwibble\e[0m\n\e[0m\ntext\n\e[0m"
  end

  test "third level heading is converted" do
    result = format("## wibble\n\ntext\n")
    assert result == "\e[33m\e[1mwibble\e[0m\n\e[0m\ntext\n\e[0m"
  end

  test "code block is converted" do
    result = format("line\n\n    code\n    code2\n\nline2\n")
    assert result == "line\n\e[0m\n\e[36m\e[1m┃ code\n┃ code2\e[0m\n\e[0m\nline2\n\e[0m"
  end

  test "list is converted" do
    result = format("* one\n* two\n* three\n")
    assert result == "• one\n• two\n• three\n\e[0m"
  end

  test "list surrounded by text is converted" do
    result = format("Count:\n\n* one\n* two\n* three\n\nDone")
    assert result == "Count:\n\e[0m\n• one\n• two\n• three\n\e[0m\nDone\n\e[0m"
  end

  test "list with continuation is converted" do
    result = format("* one\n  two\n  three\n* four")
    assert result == "• one two three\n• four"
  end

  test "nested lists are converted" do
    result = format("* one\n  * one.one\n  * one.two\n* two")
    assert result == "• one\n  • one.one\n  • one.two\n• two"
  end

  test "lists with spaces are converted" do
    result = format("  * one\n  * two\n  * three")
    assert result == "• one\n• two\n• three"
  end

  test "paragraphs are split" do
    result = format("para1\n\npara2")
    assert result == "para1\n\e[0m\npara2\n\e[0m"
  end

  test "extra whitespace is ignored between paras" do
    result = format("para1\n   \npara2")
    assert result == "para1\n\e[0m\npara2\n\e[0m"
  end

  test "extra whitespace doesn't mess up a following list" do
    result = format("para1\n   \n* one\n* two")
    assert result == "para1\n\e[0m\n• one\n• two"
  end

  test "star/underscore/backtick works" do
    result = format("*world*")
    assert result == "\e[1mworld\e[0m\n\e[0m"

    result = format("**world**")
    assert result == "\e[1mworld\e[0m\n\e[0m"

    result = format("_world_")
    assert result == "\e[4mworld\e[0m\n\e[0m"

    result = format("`world`")
    assert result == "\e[36mworld\e[0m\n\e[0m"
  end

  test "star/underscore/backtick works accross words" do
    result = format("*hello world*")
    assert result == "\e[1mhello world\e[0m\n\e[0m"

    result = format("**hello world**")
    assert result == "\e[1mhello world\e[0m\n\e[0m"

    result = format("_hello world_")
    assert result == "\e[4mhello world\e[0m\n\e[0m"

    result = format("`hello world`")
    assert result == "\e[36mhello world\e[0m\n\e[0m"
  end

  test "star/underscore preceeded by space doesn't get interpreted" do
    result = format("_unit _size")
    assert result == "_unit _size\n\e[0m"

    result = format("**unit **size")
    assert result == "**unit **size\n\e[0m"

    result = format("*unit *size")
    assert result == "*unit *size\n\e[0m"
  end

  test "backtick preceeded by space gets interpreted" do
    result = format("`unit `size")
    assert result == "\e[36munit \e[0msize\n\e[0m"
  end

  test "star/underscore/backtick with leading escape" do
    result = format("\\_unit_")
    assert result == "_unit_\n\e[0m"

    result = format("\\*unit*")
    assert result == "*unit*\n\e[0m"

    result = format("\\`unit`")
    assert result == "`unit`\n\e[0m"
  end

  test "star/underscore/backtick with closing escape" do
    result = format("_unit\\_")
    assert result == "_unit_\n\e[0m"

    result = format("*unit\\*")
    assert result == "*unit*\n\e[0m"

    result = format("`unit\\`")
    assert result == "\e[36munit\\\e[0m\n\e[0m"
  end

  test "star/underscore/backtick with double escape" do
    result = format("\\\\*world*")
    assert result == "\\\e[1mworld\e[0m\n\e[0m"

    result = format("\\\\_world_")
    assert result == "\\\e[4mworld\e[0m\n\e[0m"

    result = format("\\\\`world`")
    assert result == "\\\e[36mworld\e[0m\n\e[0m"
  end

  test "star/underscore/backtick when incomplete" do
    result = format("unit_size")
    assert result == "unit_size\n\e[0m"

    result = format("unit`size")
    assert result == "unit`size\n\e[0m"

    result = format("unit*size")
    assert result == "unit*size\n\e[0m"

    result = format("unit**size")
    assert result == "unit**size\n\e[0m"
  end

  test "backtick with escape" do
    result = format("`\\`")
    assert result == "\e[36m\\\e[0m\n\e[0m"
  end

  test "backtick close to underscores gets interpreted as code" do
    result = format("`__world__`")
    assert result == "\e[36m__world__\e[0m\n\e[0m"
  end

  test "backtick works inside parenthesis" do
    result = format("(`hello world`)")
    assert result == "(\e[36mhello world\e[0m)\n\e[0m"
  end

  test "escaping of underlines within links" do
    result = format("(http://en.wikipedia.org/wiki/ANSI_escape_code)")
    assert result == "(http://en.wikipedia.org/wiki/ANSI_escape_code)\n\e[0m"
    result = format("[ANSI escape code](http://en.wikipedia.org/wiki/ANSI_escape_code)")
    assert result == "ANSI escape code (http://en.wikipedia.org/wiki/ANSI_escape_code)\n\e[0m"
  end
end
