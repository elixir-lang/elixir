Code.require_file("../../test_helper.exs", __DIR__)

defmodule IO.ANSI.DocsTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  def format_heading(str) do
    capture_io(fn -> IO.ANSI.Docs.print_heading(str, []) end) |> String.trim_trailing()
  end

  def format_metadata(map) do
    capture_io(fn -> IO.ANSI.Docs.print_metadata(map, []) end)
  end

  def format(str) do
    capture_io(fn -> IO.ANSI.Docs.print(str, []) end) |> String.trim_trailing()
  end

  test "heading is formatted" do
    result = format_heading("wibble")
    assert String.starts_with?(result, "\e[0m\n\e[7m\e[33m")
    assert String.ends_with?(result, "\e[0m\n\e[0m")
    assert String.contains?(result, " wibble ")
  end

  test "metadata is formatted" do
    result =
      format_metadata(%{
        since: "1.2.3",
        deprecated: "Use that other one",
        author: "Alice",
        delegate_to: {Foo, :bar, 3}
      })

    assert result ==
             "\e[33mdelegate_to:\e[0m Foo.bar/3\n\e[33mdeprecated:\e[0m Use that other one\n\e[33msince:\e[0m 1.2.3\n\n"

    assert format_metadata(%{author: "Alice"}) == ""
  end

  test "first level heading is converted" do
    result = format("# wibble\n\ntext\n")
    assert result == "\e[33m# wibble\e[0m\n\e[0m\ntext\n\e[0m"
  end

  test "second level heading is converted" do
    result = format("## wibble\n\ntext\n")
    assert result == "\e[33m## wibble\e[0m\n\e[0m\ntext\n\e[0m"
  end

  test "third level heading is converted" do
    result = format("### wibble\n\ntext\n")
    assert result == "\e[33m### wibble\e[0m\n\e[0m\ntext\n\e[0m"
  end

  test "code block is converted" do
    result = format("line\n\n    code\n    code2\n\nline2\n")
    assert result == "line\n\e[0m\n\e[36m    code\n    code2\e[0m\n\e[0m\nline2\n\e[0m"
  end

  test "fenced code block is converted" do
    result = format("line\n```\ncode\ncode2\n```\nline2\n")
    assert result == "line\n\e[0m\n\e[36m    code\n    code2\e[0m\n\e[0m\nline2\n\e[0m"
    result = format("line\n```elixir\ncode\ncode2\n```\nline2\n")
    assert result == "line\n\e[0m\n\e[36m    code\n    code2\e[0m\n\e[0m\nline2\n\e[0m"
    result = format("line\n~~~elixir\ncode\n```\n~~~\nline2\n")
    assert result == "line\n\e[0m\n\e[36m    code\n    ```\e[0m\n\e[0m\nline2\n\e[0m"
  end

  test "* list is converted" do
    result = format("* one\n* two\n* three\n")
    assert result == "  • one\n  • two\n  • three\n\e[0m"
  end

  test "* list surrounded by text is converted" do
    result = format("Count:\n\n* one\n* two\n* three\n\nDone")
    assert result == "Count:\n\e[0m\n  • one\n  • two\n  • three\n\e[0m\nDone\n\e[0m"
  end

  test "* list with continuation is converted" do
    result = format("* one\ntwo\n\n    three\nfour\n* five")
    assert result == "  • one two\n    three four\n\e[0m\n  • five\n\e[0m"
  end

  test "* nested lists are converted" do
    result = format("* one\n  * one.one\n  * one.two\n* two")
    assert result == "  • one\n    • one.one\n    • one.two\n\e[0m\n  • two\n\e[0m"
  end

  test "* lists with spaces are converted" do
    result = format("  * one\n  * two\n  * three")
    assert result == "  • one\n  • two\n  • three\n\e[0m"
  end

  test "* lists with code" do
    result = format("  * one\n        two three")
    assert result == "  • one\n\e[36m        two three\e[0m\n\e[0m\n\e[0m"
  end

  test "- list is converted" do
    result = format("- one\n- two\n- three\n")
    assert result == "  • one\n  • two\n  • three\n\e[0m"
  end

  test "+ list is converted" do
    result = format("+ one\n+ two\n+ three\n")
    assert result == "  • one\n  • two\n  • three\n\e[0m"
  end

  test "+ and - nested lists are converted" do
    result = format("- one\n  + one.one\n  + one.two\n- two")
    assert result == "  • one\n    • one.one\n    • one.two\n\e[0m\n  • two\n\e[0m"
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
    assert result == "para1\n\e[0m\n  • one\n  • two\n\e[0m"
  end

  test "star/underscore/backtick works" do
    result = format("*world*")
    assert result == "\e[1mworld\e[0m\n\e[0m"

    result = format("*world*.")
    assert result == "\e[1mworld\e[0m.\n\e[0m"

    result = format("**world**")
    assert result == "\e[1mworld\e[0m\n\e[0m"

    result = format("_world_")
    assert result == "\e[4mworld\e[0m\n\e[0m"

    result = format("`world`")
    assert result == "\e[36mworld\e[0m\n\e[0m"
  end

  test "star/underscore/backtick works across words" do
    result = format("*hello world*")
    assert result == "\e[1mhello world\e[0m\n\e[0m"

    result = format("**hello world**")
    assert result == "\e[1mhello world\e[0m\n\e[0m"

    result = format("_hello world_")
    assert result == "\e[4mhello world\e[0m\n\e[0m"

    result = format("`hello world`")
    assert result == "\e[36mhello world\e[0m\n\e[0m"
  end

  test "multiple stars/underscores/backticks work" do
    result = format("*hello world* *hello world*")
    assert result == "\e[1mhello world\e[0m \e[1mhello world\e[0m\n\e[0m"

    result = format("_hello world_ _hello world_")
    assert result == "\e[4mhello world\e[0m \e[4mhello world\e[0m\n\e[0m"

    result = format("`hello world` `hello world`")
    assert result == "\e[36mhello world\e[0m \e[36mhello world\e[0m\n\e[0m"
  end

  test "multiple stars/underscores/backticks work when separated by other words" do
    result = format("*hello world* unit test *hello world*")
    assert result == "\e[1mhello world\e[0m unit test \e[1mhello world\e[0m\n\e[0m"

    result = format("_hello world_ unit test _hello world_")
    assert result == "\e[4mhello world\e[0m unit test \e[4mhello world\e[0m\n\e[0m"

    result = format("`hello world` unit test `hello world`")
    assert result == "\e[36mhello world\e[0m unit test \e[36mhello world\e[0m\n\e[0m"
  end

  test "star/underscore preceded by space doesn't get interpreted" do
    result = format("_unit _size")
    assert result == "_unit _size\n\e[0m"

    result = format("**unit **size")
    assert result == "**unit **size\n\e[0m"

    result = format("*unit *size")
    assert result == "*unit *size\n\e[0m"
  end

  test "star/underscore/backtick preceded by non-space delimiters gets interpreted" do
    result = format("(`hello world`)")
    assert result == "(\e[36mhello world\e[0m)\n\e[0m"
    result = format("<`hello world`>")
    assert result == "<\e[36mhello world\e[0m>\n\e[0m"

    result = format("(*hello world*)")
    assert result == "(\e[1mhello world\e[0m)\n\e[0m"
    result = format("@*hello world*@")
    assert result == "@\e[1mhello world\e[0m@\n\e[0m"

    result = format("(_hello world_)")
    assert result == "(\e[4mhello world\e[0m)\n\e[0m"
    result = format("'_hello world_'")
    assert result == "'\e[4mhello world\e[0m'\n\e[0m"
  end

  test "star/underscore/backtick starts/ends within a word doesn't get interpreted" do
    result = format("foo_bar, foo_bar_baz!")
    assert result == "foo_bar, foo_bar_baz!\n\e[0m"

    result = format("_foo_bar")
    assert result == "_foo_bar\n\e[0m"

    result = format("foo_bar_")
    assert result == "foo_bar_\n\e[0m"

    result = format("foo*bar, foo*bar*baz!")
    assert result == "foo*bar, foo*bar*baz!\n\e[0m"

    result = format("*foo*bar")
    assert result == "*foo*bar\n\e[0m"

    result = format("foo*bar*")
    assert result == "foo*bar*\n\e[0m"
  end

  test "backtick preceded by space gets interpreted" do
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

  test "escaping of underlines within links" do
    result = format("(https://en.wikipedia.org/wiki/ANSI_escape_code)")
    assert result == "(https://en.wikipedia.org/wiki/ANSI_escape_code)\n\e[0m"
    result = format("[ANSI escape code](https://en.wikipedia.org/wiki/ANSI_escape_code)")
    assert result == "ANSI escape code (https://en.wikipedia.org/wiki/ANSI_escape_code)\n\e[0m"
  end

  test "escaping of underlines within links does not escape surrounding text" do
    result = format("_emphasis_ (https://en.wikipedia.org/wiki/ANSI_escape_code) more _emphasis_")

    assert result ==
             "\e[4memphasis\e[0m (https://en.wikipedia.org/wiki/ANSI_escape_code) more \e[4memphasis\e[0m\n\e[0m"
  end

  test "lone thing that looks like a table line isn't" do
    assert format("one\n2 | 3\ntwo\n") == "one 2 | 3 two\n\e[0m"
  end

  test "lone table line at end of input isn't" do
    assert format("one\n2 | 3") == "one 2 | 3\n\e[0m"
  end

  test "two successive table lines are a table" do
    # note spacing
    assert format("a | b\none | two\n") == "a   | b  \none | two\n\e[0m"
  end

  test "table with heading" do
    assert format("column 1 | and 2\n-- | --\na | b\none | two\n") ==
             "\e[7mcolumn 1 | and 2\e[0m\na        | b    \none      | two  \n\e[0m"
  end

  test "table with heading alignment" do
    table = """
    column 1 | 2        | and three
    -------: | :------: | :-----
        a    |  even    | c\none | odd | three
    """

    expected =
      """
      \e[7mcolumn 1 |   2   | and three\e[0m
             a | even  | c\s\s\s\s\s\s\s\s
           one |  odd  | three\s\s\s\s
      \e[0m
      """
      |> String.trim_trailing()

    assert format(table) == expected
  end

  test "table with formatting in cells" do
    assert format("`a` | _b_\nc | d") == "\e[36ma\e[0m | \e[4mb\e[0m\nc | d\n\e[0m"
    assert format("`abc` | d \n`e` | f") == "\e[36mabc\e[0m | d\n\e[36me\e[0m   | f\n\e[0m"
  end

  test "table with variable number of columns" do
    assert format("a | b | c\nd | e") == "a | b | c\nd | e |  \n\e[0m"
  end

  test "one reference link label per line" do
    assert format("  [id]: //example.com\n  [Elixir]:  http://elixir-lang.org") ==
             "  [id]: //example.com\n  [Elixir]:  http://elixir-lang.org"
  end
end
