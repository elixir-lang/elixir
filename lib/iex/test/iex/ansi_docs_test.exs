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

  def format(str, opts // [], use_ansi // true) do
    cmd = "IEx.ANSIDocs.format(#{inspect str}, #{inspect use_ansi})"
    capture_iex(cmd, opts)
  end

  test "non-ansi heading just uses an asterisk" do
    assert capture_iex("IEx.ANSIDocs.doc_heading(\"wibble\", false)", []) == "* wibble"
  end

  test "ansi heading is formatted" do
    result = capture_iex("IEx.ANSIDocs.doc_heading(\"wibble\", true)", @opts)
    assert String.starts_with?(result, "\e[0m\n\e[7m\e[33m\e[1m")
    assert String.ends_with?(result, "\e[0m\n\e[0m")
    assert String.contains?(result, " wibble ")
  end

  test "first level heading is converted" do
    result = format("# wibble\n\ntext\n", @opts)
    assert result == "\e[33m\e[1mWIBBLE\e[0m\n\e[0m\ntext"
  end

  test "second level heading is converted" do
    result = format("## wibble\n\ntext\n", @opts)
    assert result == "\e[33m\e[1mwibble\e[0m\n\e[0m\ntext"
  end

  test "third level heading is converted" do
    result = format("## wibble\n\ntext\n", @opts)
    assert result == "\e[33m\e[1mwibble\e[0m\n\e[0m\ntext"
  end

  test "code block is converted" do
    result = format("line\n\n    code\n    code2\n\nline2\n", @opts)
    assert result == "line\n\n\e[36m\e[1m┃ code\n┃ code2\e[0m\n\e[0m\nline2"
  end

  test "list is converted" do
    result = format("* one\n* two\n* three\n", @opts)
    assert result == "• one\n• two\n• three"
  end

  test "list with continuation is converted" do
    result = format("* one\n  two\n  three\n* four", @opts)
    assert result == "• one two three\n• four"
  end

  test "nested lists are converted" do
    result = format("* one\n  * one.one\n  * one.two\n* two")
    assert result == "• one\n  • one.one\n  • one.two\n• two"
  end

  test "paragraphs are split" do
    result = format("para1\n\npara2")
    assert result == "para1\n\npara2"
  end

  test "extra whitespace is ignored between paras" do
    result = format("para1\n   \npara2")
    assert result == "para1\n\npara2"
  end

  test "extra whitespace doesn't mess up a following list" do
    result = format("para1\n   \n* one\n* two")
    assert result == "para1\n\n• one\n• two"
  end


end