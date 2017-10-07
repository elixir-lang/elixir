Code.require_file "../test_helper.exs", __DIR__

defmodule Code.Formatter.ContainersTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @short_length [line_length: 10]

  describe "tuples" do
    test "without arguments" do
      assert_format "{ }", "{}"
    end

    test "with arguments" do
      assert_format "{1,2}", "{1, 2}"
      assert_format "{1,2,3}", "{1, 2, 3}"
    end

    test "is strict on line limits" do
      bad = "{1, 2, 3, 4}"
      good = """
      {
        1,
        2,
        3,
        4
      }
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "{1,}", "{1}"
      assert_format "{1, 2, 3,}", "{1, 2, 3}"
    end

    test "with keyword lists" do
      # The one below is not valid syntax
      # assert_same "{foo: 1, bar: 2}"

      assert_same "{:hello, foo: 1, bar: 2}"

      assert_same """
      {
        :hello,
        foo: 1,
        bar: 2
      }
      """, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      {
        :hello,
        :foo,
        :bar
      }
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "{\nfoo, bar, baz}", "{foo, bar, baz}"
    end
  end

  describe "lists" do
    test "empty" do
      assert_format "[ ]", "[]"
      assert_format "[\n]", "[]"
    end

    test "with elements" do
      assert_format "[ 1 , 2,3, 4 ]", "[1, 2, 3, 4]"
    end

    test "with tail" do
      assert_format "[1,2,3|4]", "[1, 2, 3 | 4]"
    end

    test "are strict on line limit" do
      bad = """
      [11, 22, 33, 44]
      """
      good = """
      [
        11,
        22,
        33,
        44
      ]
      """
      assert_format bad, good, @short_length

      bad = """
      [11, 22, 33 | 44]
      """
      good = """
      [
        11,
        22,
        33 | 44
      ]
      """
      assert_format bad, good, @short_length

      bad = """
      [1, 2, 3 | 4]
      """
      good = """
      [
        1,
        2,
        3 | 4
      ]
      """
      assert_format bad, good, @short_length

      bad = """
      [1, 2, 3 | really_long_expression()]
      """
      good = """
      [
        1,
        2,
        3
        | really_long_expression()
      ]
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "[1,]", "[1]"
      assert_format "[1, 2, 3,]", "[1, 2, 3]"
    end

    test "with keyword lists" do
      assert_same "[foo: 1, bar: 2]"
      assert_same "[:hello, foo: 1, bar: 2]"

      # Pseudo keyword lists are kept as is
      assert_same "[{:foo, 1}, {:bar, 2}]"

      assert_same """
      [
        foo: 1,
        bar: 2
      ]
      """, @short_length
    end

    test "with quoted keyword lists" do
      assert_same ~S(["with spaces": 1])
      assert_same ~S(["one #{two} three": 1])
      assert_format ~S(["Foo": 1, "Bar": 2]), ~S([Foo: 1, Bar: 2])
    end

    test "preserves user choice even when it fits" do
      assert_same """
      [
        :hello,
        :foo,
        :bar
      ]
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "[\nfoo, bar, baz]", "[foo, bar, baz]"
    end
  end

  describe "bitstrings" do
    test "without arguments" do
      assert_format "<< >>", "<<>>"
      assert_format "<<\n>>", "<<>>"
    end

    test "with arguments" do
      assert_format "<<1,2,3>>", "<<1, 2, 3>>"
    end

    test "add parens on first and last in case of ambiguity" do
      assert_format "<< <<>> >>", "<<(<<>>)>>"
      assert_format "<< <<>> + <<>> >>", "<<(<<>> + <<>>)>>"
      assert_format "<< 1 + <<>> >>", "<<(1 + <<>>)>>"
      assert_format "<< <<>> + 1 >>", "<<(<<>> + 1)>>"
      assert_format "<< <<>>, <<>>, <<>> >>", "<<(<<>>), <<>>, (<<>>)>>"
      assert_format "<< <<>>::1, <<>>::2, <<>>::3 >>", "<<(<<>>::1), <<>>::2, <<>>::3>>"
    end

    test "with modifiers" do
      assert_format "<< 1 :: 1 >>", "<<1::1>>"
      assert_format "<< 1 :: 2 + 3 >>", "<<1::(2 + 3)>>"
      assert_format "<< 1 :: 2 - integer >>", "<<1::2-integer>>"
      assert_format "<< 1 :: 2 - unit(3) >>", "<<1::2-unit(3)>>"
      assert_format "<< 1 :: 2 * 3 - unit(4) >>", "<<1::2*3-unit(4)>>"
      assert_format "<< 1 :: 2 - unit(3) - 4 / 5 >>", "<<1::2-unit(3)-(4 / 5)>>"
    end

    test "is strict on line limits" do
      bad = "<<1, 2, 3, 4>>"
      good = """
      <<
        1,
        2,
        3,
        4
      >>
      """
      assert_format bad, good, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      <<
        :hello,
        :foo,
        :bar
      >>
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "<<\nfoo, bar, baz>>", "<<foo, bar, baz>>"
    end
  end

  describe "maps" do
    test "without arguments" do
      assert_format "%{ }", "%{}"
    end

    test "with arguments" do
      assert_format "%{1 => 2,3 => 4}", "%{1 => 2, 3 => 4}"
    end

    test "is strict on line limits" do
      bad = "%{1 => 2, 3 => 4}"
      good = """
      %{
        1 => 2,
        3 => 4
      }
      """
      assert_format bad, good, @short_length

      assert_same """
      %{
        a(1, 2) => b,
        c(3, 4) => d
      }
      """, @short_length

      assert_same """
      %{
        a => fn x ->
          y
        end,
        b => fn y ->
          z
        end
      }
      """, @short_length

      assert_same """
      %{
        a => for(
          y <- x,
          z <- y,
          do: 123
        )
      }
      """, @short_length

      assert_same """
      %{
        a => for do
          :ok
        end
      }
      """, @short_length
    end

    test "removes trailing comma" do
      assert_format "%{1 => 2,}", "%{1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%{:foo => :bar, baz: :bat}"

      assert_same """
      %{
        :foo => :bar,
        baz: :bat
      }
      """, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      %{
        :hello => 1,
        :foo => 2,
        :bar => 3
      }
      """

      # Doesn't preserve this because only the beginning has a newline
      assert_format "%{\nfoo: 1, bar: 2}", "%{foo: 1, bar: 2}"
    end
  end

  describe "maps with update" do
    test "with arguments" do
      assert_format "%{foo | 1 => 2,3 => 4}", "%{foo | 1 => 2, 3 => 4}"
    end

    test "is strict on line limits" do
      bad = "%{foo | 1 => 2, 3 => 4}"
      good = """
      %{
        foo
        | 1 => 2,
          3 => 4
      }
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "%{foo | 1 => 2,}", "%{foo | 1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%{foo | :foo => :bar, baz: :bat}"

      assert_same """
      %{
        foo
        | :foo => :bar,
          baz: :bat
      }
      """, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      %{
        foo
        | :hello => 1,
          :foo => 2,
          :bar => 3
      }
      """
    end
  end


  describe "structs" do
    test "without arguments" do
      assert_format "%struct{ }", "%struct{}"
    end

    test "with arguments" do
      assert_format "%struct{1 => 2,3 => 4}", "%struct{1 => 2, 3 => 4}"
    end

    test "is strict on line limits" do
      bad = "%struct{1 => 2, 3 => 4}"
      good = """
      %struct{
        1 => 2,
        3 => 4
      }
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "%struct{1 => 2,}", "%struct{1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%struct{:foo => :bar, baz: :bat}"

      assert_same """
      %struct{
        :foo => :bar,
        baz: :bat
      }
      """, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      %Foo{
        :hello => 1,
        :foo => 2,
        :bar => 3
      }
      """
    end
  end

  describe "struct with update" do
    test "with arguments" do
      assert_format "%struct{foo | 1 => 2,3 => 4}", "%struct{foo | 1 => 2, 3 => 4}"
    end

    test "is strict on line limits" do
      bad = "%struct{foo | 1 => 2, 3 => 4}"
      good = """
      %struct{
        foo
        | 1 => 2,
          3 => 4
      }
      """
      assert_format bad, good, @short_length
    end

    test "removes trailing comma" do
      assert_format "%struct{foo | 1 => 2,}", "%struct{foo | 1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%struct{foo | :foo => :bar, baz: :bat}"

      assert_same """
      %struct{
        foo
        | :foo => :bar,
          baz: :bat
      }
      """, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      %Foo{
        foo
        | :hello => 1,
          :foo => 2,
          :bar => 3
      }
      """
    end
  end
end
