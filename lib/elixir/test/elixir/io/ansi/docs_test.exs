Code.require_file("../../test_helper.exs", __DIR__)

defmodule IO.ANSI.DocsTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  def format_headings(list) do
    capture_io(fn -> IO.ANSI.Docs.print_headings(list, []) end) |> String.trim_trailing()
  end

  def format_metadata(map) do
    capture_io(fn -> IO.ANSI.Docs.print_metadata(map, []) end)
  end

  def format_markdown(str, opts \\ []) do
    capture_io(fn -> IO.ANSI.Docs.print(str, "text/markdown", opts) end)
    |> String.trim_trailing()
  end

  def format_erlang(str, opts \\ []) do
    capture_io(fn -> IO.ANSI.Docs.print(str, "application/erlang+html", opts) end)
  end

  describe "heading" do
    test "is formatted" do
      result = format_headings(["foo"])
      assert String.starts_with?(result, "\e[0m\n\e[7m\e[33m")
      assert String.ends_with?(result, "\e[0m\n\e[0m")
      assert String.contains?(result, " foo ")
    end

    test "multiple entries formatted" do
      result = format_headings(["foo", "bar"])
      assert :binary.matches(result, "\e[0m\n\e[7m\e[33m") |> length == 2
      assert String.starts_with?(result, "\e[0m\n\e[7m\e[33m")
      assert String.ends_with?(result, "\e[0m\n\e[0m")
      assert String.contains?(result, " foo ")
      assert String.contains?(result, " bar ")
    end
  end

  describe "metadata" do
    test "is formatted" do
      result =
        format_metadata(%{
          since: "1.2.3",
          deprecated: "Use that other one",
          author: "Alice",
          delegate_to: {Foo, :bar, 3}
        })

      assert result == """
             \e[33mdelegate_to:\e[0m Foo.bar/3
             \e[33mdeprecated:\e[0m Use that other one
             \e[33msince:\e[0m 1.2.3

             """

      assert format_metadata(%{author: "Alice"}) == ""
    end
  end

  describe "markdown" do
    test "first level heading is converted" do
      result = format_markdown("# wibble\n\ntext\n")
      assert result == "\e[33m# wibble\e[0m\n\e[0m\ntext\n\e[0m"
    end

    test "second level heading is converted" do
      result = format_markdown("## wibble\n\ntext\n")
      assert result == "\e[33m## wibble\e[0m\n\e[0m\ntext\n\e[0m"
    end

    test "third level heading is converted" do
      result = format_markdown("### wibble\n\ntext\n")
      assert result == "\e[33m### wibble\e[0m\n\e[0m\ntext\n\e[0m"
    end

    test "short single-line quote block is converted into single-line quote" do
      result =
        format_markdown("""
        line

        > normal *italics* `code`

        line2
        """)

      assert result ==
               """
               line
               \e[0m
               \e[90m> \e[0mnormal \e[1mitalics\e[0m \e[36mcode\e[0m
               \e[0m
               line2
               \e[0m\
               """
    end

    test "short multi-line quote block is converted into single-line quote" do
      result =
        format_markdown("""
        line

        > normal
        > *italics*
        > `code`

        line2
        """)

      assert result ==
               """
               line
               \e[0m
               \e[90m> \e[0mnormal \e[1mitalics\e[0m \e[36mcode\e[0m
               \e[0m
               line2
               \e[0m\
               """
    end

    test "long multi-line quote block is converted into wrapped multi-line quote" do
      result =
        format_markdown("""
        line

        > normal
        > *italics*
        > `code`
        > some-extremely-long-word-which-can-not-possibly-fit-into-the-previous-line

        line2
        """)

      assert result ==
               """
               line
               \e[0m
               \e[90m> \e[0mnormal \e[1mitalics\e[0m \e[36mcode\e[0m
               \e[90m> \e[0msome-extremely-long-word-which-can-not-possibly-fit-into-the-previous-line
               \e[0m
               line2
               \e[0m\
               """
    end

    test "multi-line quote block containing empty lines is converted into wrapped multi-line quote" do
      result =
        format_markdown("""
        line

        > normal
        > *italics*
        >
        > `code`
        > some-extremely-long-word-which-can-not-possibly-fit-into-the-previous-line

        line2
        """)

      assert result ==
               """
               line
               \e[0m
               \e[90m> \e[0mnormal \e[1mitalics\e[0m
               \e[90m> \e[0m
               \e[90m> \e[0m\e[36mcode\e[0m
               \e[90m> \e[0msome-extremely-long-word-which-can-not-possibly-fit-into-the-previous-line
               \e[0m
               line2
               \e[0m\
               """
    end

    test "code block is converted" do
      result = format_markdown("line\n\n    code\n    code2\n\nline2\n")
      assert result == "line\n\e[0m\n\e[36m    code\n    code2\e[0m\n\e[0m\nline2\n\e[0m"
    end

    test "fenced code block is converted" do
      result = format_markdown("line\n```\ncode\ncode2\n```\nline2\n")
      assert result == "line\n\e[0m\n\e[36m    code\n    code2\e[0m\n\e[0m\nline2\n\e[0m"
      result = format_markdown("line\n```elixir\ncode\ncode2\n```\nline2\n")
      assert result == "line\n\e[0m\n\e[36m    code\n    code2\e[0m\n\e[0m\nline2\n\e[0m"
      result = format_markdown("line\n~~~elixir\ncode\n```\n~~~\nline2\n")
      assert result == "line\n\e[0m\n\e[36m    code\n    ```\e[0m\n\e[0m\nline2\n\e[0m"
    end

    test "* list is converted" do
      result = format_markdown("* one\n* two\n* three\n")
      assert result == "  • one\n  • two\n  • three\n\e[0m"
    end

    test "* list is converted without ansi" do
      result = format_markdown("* one\n* two\n* three\n", enabled: false)
      assert result == "  * one\n  * two\n  * three"
    end

    test "* list surrounded by text is converted" do
      result = format_markdown("Count:\n\n* one\n* two\n* three\n\nDone")
      assert result == "Count:\n\e[0m\n  • one\n  • two\n  • three\n\e[0m\nDone\n\e[0m"
    end

    test "* list with continuation is converted" do
      result = format_markdown("* one\ntwo\n\n    three\nfour\n* five")
      assert result == "  • one two\n    three four\n\e[0m\n  • five\n\e[0m"
    end

    test "* nested lists are converted" do
      result = format_markdown("* one\n  * one.one\n  * one.two\n* two")
      assert result == "  • one\n    • one.one\n    • one.two\n\e[0m\n  • two\n\e[0m"
    end

    test "* deep nested lists are converted" do
      result =
        format_markdown("""
          * level 1
            * level 2a
            * level 2b
              * level 3
                * level 4a
                * level 4b
                  * level 5
                    * level 6
        """)

      assert result ==
               "  • level 1\n    • level 2a\n    • level 2b\n      • level 3\n        • level 4a\n        • level 4b\n          • level 5\n            • level 6\n\e[0m\n\e[0m\n\e[0m\n\e[0m\n\e[0m\n\e[0m"
    end

    test "* lists with spaces are converted" do
      result = format_markdown("  * one\n  * two\n  * three")
      assert result == "  • one\n  • two\n  • three\n\e[0m"
    end

    test "* lists with code" do
      result = format_markdown("  * one\n        two three")
      assert result == "  • one\n\e[36m        two three\e[0m\n\e[0m\n\e[0m"
    end

    test "- list is converted" do
      result = format_markdown("- one\n- two\n- three\n")
      assert result == "  • one\n  • two\n  • three\n\e[0m"
    end

    test "+ list is converted" do
      result = format_markdown("+ one\n+ two\n+ three\n")
      assert result == "  • one\n  • two\n  • three\n\e[0m"
    end

    test "+ and - nested lists are converted" do
      result = format_markdown("- one\n  + one.one\n  + one.two\n- two")
      assert result == "  • one\n    • one.one\n    • one.two\n\e[0m\n  • two\n\e[0m"
    end

    test "paragraphs are split" do
      result = format_markdown("para1\n\npara2")
      assert result == "para1\n\e[0m\npara2\n\e[0m"
    end

    test "extra whitespace is ignored between paras" do
      result = format_markdown("para1\n   \npara2")
      assert result == "para1\n\e[0m\npara2\n\e[0m"
    end

    test "extra whitespace doesn't mess up a following list" do
      result = format_markdown("para1\n   \n* one\n* two")
      assert result == "para1\n\e[0m\n  • one\n  • two\n\e[0m"
    end

    test "star/underscore/backtick works" do
      result = format_markdown("*world*")
      assert result == "\e[1mworld\e[0m\n\e[0m"

      result = format_markdown("*world*.")
      assert result == "\e[1mworld\e[0m.\n\e[0m"

      result = format_markdown("**world**")
      assert result == "\e[1mworld\e[0m\n\e[0m"

      result = format_markdown("_world_")
      assert result == "\e[4mworld\e[0m\n\e[0m"

      result = format_markdown("`world`")
      assert result == "\e[36mworld\e[0m\n\e[0m"
    end

    test "star/underscore/backtick works across words" do
      result = format_markdown("*hello world*")
      assert result == "\e[1mhello world\e[0m\n\e[0m"

      result = format_markdown("**hello world**")
      assert result == "\e[1mhello world\e[0m\n\e[0m"

      result = format_markdown("_hello world_")
      assert result == "\e[4mhello world\e[0m\n\e[0m"

      result = format_markdown("`hello world`")
      assert result == "\e[36mhello world\e[0m\n\e[0m"
    end

    test "star/underscore/backtick works across words with ansi disabled" do
      result = format_markdown("*hello world*", enabled: false)
      assert result == "*hello world*"

      result = format_markdown("**hello world**", enabled: false)
      assert result == "**hello world**"

      result = format_markdown("_hello world_", enabled: false)
      assert result == "_hello world_"

      result = format_markdown("`hello world`", enabled: false)
      assert result == "`hello world`"
    end

    test "multiple stars/underscores/backticks work" do
      result = format_markdown("*hello world* *hello world*")
      assert result == "\e[1mhello world\e[0m \e[1mhello world\e[0m\n\e[0m"

      result = format_markdown("_hello world_ _hello world_")
      assert result == "\e[4mhello world\e[0m \e[4mhello world\e[0m\n\e[0m"

      result = format_markdown("`hello world` `hello world`")
      assert result == "\e[36mhello world\e[0m \e[36mhello world\e[0m\n\e[0m"
    end

    test "multiple stars/underscores/backticks work when separated by other words" do
      result = format_markdown("*hello world* unit test *hello world*")
      assert result == "\e[1mhello world\e[0m unit test \e[1mhello world\e[0m\n\e[0m"

      result = format_markdown("_hello world_ unit test _hello world_")
      assert result == "\e[4mhello world\e[0m unit test \e[4mhello world\e[0m\n\e[0m"

      result = format_markdown("`hello world` unit test `hello world`")
      assert result == "\e[36mhello world\e[0m unit test \e[36mhello world\e[0m\n\e[0m"
    end

    test "star/underscore preceded by space doesn't get interpreted" do
      result = format_markdown("_unit _size")
      assert result == "_unit _size\n\e[0m"

      result = format_markdown("**unit **size")
      assert result == "**unit **size\n\e[0m"

      result = format_markdown("*unit *size")
      assert result == "*unit *size\n\e[0m"
    end

    test "star/underscore/backtick preceded by non-space delimiters gets interpreted" do
      result = format_markdown("(`hello world`)")
      assert result == "(\e[36mhello world\e[0m)\n\e[0m"
      result = format_markdown("<`hello world`>")
      assert result == "<\e[36mhello world\e[0m>\n\e[0m"

      result = format_markdown("(*hello world*)")
      assert result == "(\e[1mhello world\e[0m)\n\e[0m"
      result = format_markdown("@*hello world*@")
      assert result == "@\e[1mhello world\e[0m@\n\e[0m"

      result = format_markdown("(_hello world_)")
      assert result == "(\e[4mhello world\e[0m)\n\e[0m"
      result = format_markdown("'_hello world_'")
      assert result == "'\e[4mhello world\e[0m'\n\e[0m"
    end

    test "star/underscore/backtick starts/ends within a word doesn't get interpreted" do
      result = format_markdown("foo_bar, foo_bar_baz!")
      assert result == "foo_bar, foo_bar_baz!\n\e[0m"

      result = format_markdown("_foo_bar")
      assert result == "_foo_bar\n\e[0m"

      result = format_markdown("foo_bar_")
      assert result == "foo_bar_\n\e[0m"

      result = format_markdown("foo*bar, foo*bar*baz!")
      assert result == "foo*bar, foo*bar*baz!\n\e[0m"

      result = format_markdown("*foo*bar")
      assert result == "*foo*bar\n\e[0m"

      result = format_markdown("foo*bar*")
      assert result == "foo*bar*\n\e[0m"
    end

    test "backtick preceded by space gets interpreted" do
      result = format_markdown("`unit `size")
      assert result == "\e[36munit \e[0msize\n\e[0m"
    end

    test "backtick does not escape characters" do
      result = format_markdown("`Ctrl+\\ `")
      assert result == "\e[36mCtrl+\\ \e[0m\n\e[0m"
    end

    test "star/underscore/backtick with leading escape" do
      result = format_markdown("\\_unit_")
      assert result == "_unit_\n\e[0m"

      result = format_markdown("\\*unit*")
      assert result == "*unit*\n\e[0m"

      result = format_markdown("\\`unit`")
      assert result == "`unit`\n\e[0m"
    end

    test "star/underscore/backtick with closing escape" do
      result = format_markdown("_unit\\_")
      assert result == "_unit_\n\e[0m"

      result = format_markdown("*unit\\*")
      assert result == "*unit*\n\e[0m"

      result = format_markdown("`unit\\`")
      assert result == "\e[36munit\\\e[0m\n\e[0m"
    end

    test "star/underscore/backtick with double escape" do
      result = format_markdown("\\\\*world*")
      assert result == "\\\e[1mworld\e[0m\n\e[0m"

      result = format_markdown("\\\\_world_")
      assert result == "\\\e[4mworld\e[0m\n\e[0m"

      result = format_markdown("\\\\`world`")
      assert result == "\\\e[36mworld\e[0m\n\e[0m"
    end

    test "star/underscore/backtick when incomplete" do
      result = format_markdown("unit_size")
      assert result == "unit_size\n\e[0m"

      result = format_markdown("unit`size")
      assert result == "unit`size\n\e[0m"

      result = format_markdown("unit*size")
      assert result == "unit*size\n\e[0m"

      result = format_markdown("unit**size")
      assert result == "unit**size\n\e[0m"
    end

    test "backtick with escape" do
      result = format_markdown("`\\`")
      assert result == "\e[36m\\\e[0m\n\e[0m"
    end

    test "backtick close to underscores gets interpreted as code" do
      result = format_markdown("`__world__`")
      assert result == "\e[36m__world__\e[0m\n\e[0m"
    end

    test "escaping of underlines within links" do
      result = format_markdown("(https://en.wikipedia.org/wiki/ANSI_escape_code)")
      assert result == "(https://en.wikipedia.org/wiki/ANSI_escape_code)\n\e[0m"

      result =
        format_markdown("[ANSI escape code](https://en.wikipedia.org/wiki/ANSI_escape_code)")

      assert result == "ANSI escape code (https://en.wikipedia.org/wiki/ANSI_escape_code)\n\e[0m"

      result = format_markdown("(ftp://example.com/ANSI_escape_code.zip)")
      assert result == "(ftp://example.com/ANSI_escape_code.zip)\n\e[0m"
    end

    test "escaping of underlines within links does not escape surrounding text" do
      result =
        format_markdown(
          "_emphasis_ (https://en.wikipedia.org/wiki/ANSI_escape_code) more _emphasis_"
        )

      assert result ==
               "\e[4memphasis\e[0m (https://en.wikipedia.org/wiki/ANSI_escape_code) more \e[4memphasis\e[0m\n\e[0m"
    end

    test "escaping of underlines within links avoids false positives" do
      assert format_markdown("`https_proxy`") == "\e[36mhttps_proxy\e[0m\n\e[0m"
    end

    test "escaping of several Markdown links in one line" do
      assert format_markdown("[List](`List`) (`[1, 2, 3]`), [Map](`Map`)") ==
               "List (\e[36mList\e[0m) (\e[36m[1, 2, 3]\e[0m), Map (\e[36mMap\e[0m)\n\e[0m"
    end

    test "one reference link label per line" do
      assert format_markdown("  [id]: //example.com\n  [Elixir]:  https://elixir-lang.org") ==
               "  [id]: //example.com\n  [Elixir]:  https://elixir-lang.org"
    end
  end

  describe "markdown tables" do
    test "lone thing that looks like a table line isn't" do
      assert format_markdown("one\n2 | 3\ntwo\n") == "one 2 | 3 two\n\e[0m"
    end

    test "lone table line at end of input isn't" do
      assert format_markdown("one\n2 | 3") == "one 2 | 3\n\e[0m"
    end

    test "two successive table lines are a table" do
      # note spacing
      assert format_markdown("a | b\none | two\n") == "a   | b  \none | two\n\e[0m"
    end

    test "table with heading" do
      assert format_markdown("column 1 | and 2\n-- | --\na | b\none | two\n") ==
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

      assert format_markdown(table) == expected
    end

    test "table with heading alignment and no space around \"|\"" do
      table = """
      | Value | Encoding | Value | Encoding |
      |------:|:---------|------:|:---------|
      |     0 | A        |    17 | R        |
      |     1 | B        |    18 | S        |
      """

      expected =
        "\e[7m" <>
          "Value | Encoding | Value | Encoding\e[0m\n" <>
          "    0 | A        |    17 | R       \n" <>
          "    1 | B        |    18 | S       \n\e[0m"

      assert format_markdown(table) == expected
    end

    test "table with formatting in cells" do
      assert format_markdown("`a` | _b_\nc | d") == "\e[36ma\e[0m | \e[4mb\e[0m\nc | d\n\e[0m"

      assert format_markdown("`abc` | d \n`e` | f") ==
               "\e[36mabc\e[0m | d\n\e[36me\e[0m   | f\n\e[0m"
    end

    test "table with variable number of columns" do
      assert format_markdown("a | b | c\nd | e") == "a | b | c\nd | e |  \n\e[0m"
    end

    test "table with escaped \"|\" inside cell" do
      table = "a | smth\\|smth_else | c\nd | e | f"

      expected =
        """
        a | smth|smth_else | c
        d | e              | f
        \e[0m
        """
        |> String.trim_trailing()

      assert format_markdown(table) == expected
    end

    test "table with last two columns empty" do
      table = """
      AAA |     |     |
      BBB | CCC |     |
      GGG | HHH | III |
      JJJ | KKK | LLL | MMM
      """

      expected =
        """
        AAA |     |     |\s\s\s\s
        BBB | CCC |     |\s\s\s\s
        GGG | HHH | III |\s\s\s\s
        JJJ | KKK | LLL | MMM
        \e[0m
        """
        |> String.trim_trailing()

      assert format_markdown(table) == expected
    end
  end

  describe "erlang" do
    @hello_world [{:p, [], ["Hello"]}, {:p, [], ["World"]}]

    test "text" do
      assert format_erlang("Hello world") == "Hello world"
    end

    test "skips line breaks" do
      assert format_erlang([{:p, [], ["Hello"]}, {:br, [], []}, {:p, [], ["World"]}]) ==
               "Hello\n\nWorld\n\n"
    end

    test "paragraphs" do
      assert format_erlang(@hello_world) == "Hello\n\nWorld\n\n"
    end

    test "code chunks" do
      code = """
      def foo do
        :bar
      end\
      """

      assert format_erlang({:pre, [], [{:code, [], [code]}]}) == """
                 def foo do
                   :bar
                 end

             """
    end

    test "unordered lists" do
      assert format_erlang([{:ul, [], [{:li, [], ["Hello"]}, {:li, [], ["World"]}]}]) ==
               "  • Hello\n\n  • World\n\n"

      assert format_erlang([{:ul, [], [{:li, [], [@hello_world]}]}]) ==
               "  • Hello\n\n    World\n\n"

      assert format_erlang([
               {:ul, [], [{:li, [], [{:p, [], ["Hello"]}]}, {:li, [], [{:p, [], ["World"]}]}]}
             ]) ==
               "  • Hello\n\n  • World\n\n"
    end

    test "ordered lists" do
      assert format_erlang([{:ol, [], [{:li, [], ["Hello"]}, {:li, [], ["World"]}]}]) ==
               "  1. Hello\n\n  2. World\n\n"

      assert format_erlang([
               {:ol, [], [{:li, [], [{:p, [], ["Hello"]}]}, {:li, [], [{:p, [], ["World"]}]}]}
             ]) ==
               "  1. Hello\n\n  2. World\n\n"
    end

    test "admonition blocks" do
      assert format_erlang([{:div, [class: "warning"], @hello_world}]) == """
             \e[90m> \e[0mWARNING
             \e[90m> \e[0m
             \e[90m> \e[0mHello
             \e[90m> \e[0m
             \e[90m> \e[0mWorld

             """
    end

    test "headers" do
      assert format_erlang([{:h1, [], ["Hello"]}]) ==
               "\e[33m# Hello\e[0m\n\n"

      assert format_erlang([{:h2, [], ["Hello"]}]) ==
               "\e[33m## Hello\e[0m\n\n"

      assert format_erlang([{:h3, [], ["Hello"]}]) ==
               "\e[33m### Hello\e[0m\n\n"

      assert format_erlang([{:h4, [], ["Hello"]}]) ==
               "\e[33m#### Hello\e[0m\n\n"

      assert format_erlang([{:h5, [], ["Hello"]}]) ==
               "\e[33m##### Hello\e[0m\n\n"

      assert format_erlang([{:h6, [], ["Hello"]}]) ==
               "\e[33m###### Hello\e[0m\n\n"

      assert format_erlang([{:h1, [], [{:code, [], ["Hello"]}]}]) ==
               "\e[33m# \e[36mHello\e[0m\e[0m\n\n"

      assert format_erlang([{:h2, [], [{:code, [], ["Hello"]}]}]) ==
               "\e[33m## \e[36mHello\e[0m\e[0m\n\n"

      assert format_erlang([{:h3, [], [{:code, [], ["Hello"]}]}]) ==
               "\e[33m### \e[36mHello\e[0m\e[0m\n\n"

      assert format_erlang([{:h4, [], [{:code, [], ["Hello"]}]}]) ==
               "\e[33m#### \e[36mHello\e[0m\e[0m\n\n"

      assert format_erlang([{:h5, [], [{:code, [], ["Hello"]}]}]) ==
               "\e[33m##### \e[36mHello\e[0m\e[0m\n\n"

      assert format_erlang([{:h6, [], [{:code, [], ["Hello"]}]}]) ==
               "\e[33m###### \e[36mHello\e[0m\e[0m\n\n"
    end

    test "inline tags" do
      assert format_erlang([{:i, [], ["Hello"]}]) == "\e[4mHello\e[0m"
      assert format_erlang([{:i, [], ["Hello"]}], enabled: false) == "_Hello_"

      assert format_erlang([{:em, [], ["Hello"]}]) == "\e[1mHello\e[0m"
      assert format_erlang([{:em, [], ["Hello"]}], enabled: false) == "*Hello*"

      assert format_erlang([{:code, [], ["Hello"]}]) == "\e[36mHello\e[0m"
      assert format_erlang([{:code, [], ["Hello"]}], enabled: false) == "`Hello`"
    end

    test "inline tags within paragraphs" do
      assert format_erlang([{:p, [], [[{:em, [], ["Hello"]}, {:code, [], ["World"]}]]}]) ==
               "\e[1mHello\e[0m\e[36mWorld\e[0m"
    end

    test "inline tags within list item" do
      assert format_erlang([
               {:ul, [], [{:li, [], [{:em, [], ["Hello"]}, {:code, [], ["World"]}]}]}
             ]) ==
               "  • \e[1mHello\e[0m\e[36mWorld\e[0m\n\n"
    end

    test "links" do
      assert format_erlang([{:a, [], ["Hello"]}]) == "Hello"
      assert format_erlang([{:a, [href: "foo/bar"], ["Hello"]}]) == "Hello (foo/bar)"
    end

    test "definition lists" do
      assert format_erlang([{:dl, [], [[{:dt, [], ["Hello"]}, {:dd, [], ["World"]}]]}]) == """
               • Hello

                 World

             """
    end

    test "typespecs" do
      assert format_erlang([{:ul, [class: "types"], [{:li, [], []}]}]) == ""

      assert format_erlang([{:ul, [class: "types"], [{:li, [], ["Hello"]}, {:li, [], ["World"]}]}]) ==
               """
               Typespecs:

                   Hello
                   World

               """

      assert format_erlang([
               {:ul, [class: "types"], [{:li, [], ["Hello", {:code, [], ["World"]}]}]}
             ]) ==
               """
               Typespecs:

                   Hello
                   \e[36mWorld\e[0m

               """
    end

    test "extra markup" do
      assert format_erlang([{:p, [], ["Hello"]}, {:unknown, [], ["Unknown"]}, {:p, [], ["World"]}]) ==
               """
               Hello

               <unknown>
               Unknown
               </unknown>

               World

               """

      assert format_erlang([
               {:p, [], ["Hello"]},
               {:unknown, [], [{:p, [], ["Unknown"]}]},
               {:p, [], ["World"]}
             ]) == """
             Hello

             <unknown>
                 Unknown
             </unknown>

             World

             """
    end
  end

  describe "invalid format" do
    test "prints message" do
      assert capture_io(fn -> IO.ANSI.Docs.print("hello", "text/unknown", []) end) ==
               "\nUnknown documentation format \"text/unknown\"\n\n"
    end
  end
end
