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

  test "non-ansi heading just uses an asterisk" do
    assert capture_iex("IEx.ANSIDocs.doc_heading(\"wibble\", false)", []) == "* wibble\n\n:ok"
  end

  test "ansi heading is formatted" do
    result = capture_iex("IEx.ANSIDocs.doc_heading(\"wibble\", true)", @opts)
    assert String.starts_with?(result, "\e[0m\n\e[7m\e[33m\e[1m")
    assert String.ends_with?(result, "\e[0m\n\e[0m\n\e[33m:ok\e[0m")
    assert String.contains?(result, " wibble ")
  end

  test "first level heading is converted" do
    result = capture_iex("IEx.ANSIDocs.format(\"# wibble\n\ntext\n\", true)", @opts)
    assert result == "\e[33m\e[1mWIBBLE\e[0m\n\e[0m\ntext \n\n\e[33mnil\e[0m"
  end

  test "second level heading is converted" do
    result = capture_iex("IEx.ANSIDocs.format(\"## wibble\n\ntext\n\", true)", @opts)
    assert result == "\e[33m\e[1mwibble\e[0m\n\e[0m\ntext \n\n\e[33mnil\e[0m"
  end

  test "third level heading is converted" do
    result = capture_iex("IEx.ANSIDocs.format(\"## wibble\n\ntext\n\", true)", @opts)
    assert result == "\e[33m\e[1mwibble\e[0m\n\e[0m\ntext \n\n\e[33mnil\e[0m"
  end

  test "code block is converted" do
    result = capture_iex("IEx.ANSIDocs.format(\"line\n\n    code\n    code2\n\nline2\n\", true)", @opts)
    assert result == "line \n\n\e[36m\e[1m┃ code\n┃ code2\e[0m\n\e[0m\nline2 \n\n\e[33mnil\e[0m"
  end

  test "list is converted" do
    result = capture_iex("IEx.ANSIDocs.format(\"* one\n* two\n* three\n\", true)", @opts)
    assert result == "• one \n• two \n• three \n\n\e[33mnil\e[0m"
  end
end