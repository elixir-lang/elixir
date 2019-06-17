Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.Formatter.ContainersTest do
  use ExUnit.Case, async: true

  import CodeFormatterHelpers

  @short_length [line_length: 10]
  @medium_length [line_length: 20]

  describe "tuples" do
    test "without arguments" do
      assert_format "{ }", "{}"
    end

    test "with arguments" do
      assert_format "{1,2}", "{1, 2}"
      assert_format "{1,2,3}", "{1, 2, 3}"
    end

    test "is flex on line limits" do
      bad = "{1, 2, 3, 4}"

      good = """
      {1, 2, 3,
       4}
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

      tuple = """
      {
        :hello,
        foo: 1,
        bar: 2
      }
      """

      assert_same tuple, @short_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      {
        :hello,
        :foo,
        :bar
      }
      """

      # Doesn't preserve this because only the ending has a newline
      assert_format "{foo, bar, baz\n}", "{foo, bar, baz}"
    end

    test "preserves user choice even when it fits with trailing comma" do
      bad = """
      {
        :hello,
        :foo,
        :bar,
      }
      """

      assert_format bad, """
      {
        :hello,
        :foo,
        :bar
      }
      """
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

      keyword = """
      [
        foo: 1,
        bar: 2
      ]
      """

      assert_same keyword, @short_length
    end

    test "with keyword lists on comma line limit" do
      bad = """
      [
        foooo: 1,
        barrr: 2
      ]
      """

      good = """
      [
        foooo:
          1,
        barrr: 2
      ]
      """

      assert_format bad, good, @short_length
    end

    test "with quoted keyword lists" do
      assert_same ~S(["with spaces": 1])
      assert_same ~S(["one #{two} three": 1])
      assert_same ~S(["\w": 1, "\\w": 2])
      assert_same ~S(["Elixir.Foo": 1, "Elixir.Bar": 2])
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

      # Doesn't preserve this because only the ending has a newline
      assert_format "[foo, bar, baz\n]", "[foo, bar, baz]"
    end

    test "preserves user choice even when it fits with trailing comma" do
      bad = """
      [
        :hello,
        :foo,
        :bar,
      ]
      """

      assert_format bad, """
      [
        :hello,
        :foo,
        :bar
      ]
      """
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

    test "add parens on first and last in case of binary ambiguity" do
      assert_format "<< <<>> >>", "<<(<<>>)>>"
      assert_format "<< <<>> + <<>> >>", "<<(<<>> + <<>>)>>"
      assert_format "<< 1 + <<>> >>", "<<(1 + <<>>)>>"
      assert_format "<< <<>> + 1 >>", "<<(<<>> + 1)>>"
      assert_format "<< <<>>, <<>>, <<>> >>", "<<(<<>>), <<>>, (<<>>)>>"
      assert_format "<< <<>>::1, <<>>::2, <<>>::3 >>", "<<(<<>>)::1, <<>>::2, <<>>::3>>"
      assert_format "<< <<>>::<<>> >>", "<<(<<>>)::(<<>>)>>"
    end

    test "add parens on first in case of operator ambiguity" do
      assert_format "<< ~~~1::8 >>", "<<(~~~1)::8>>"
      assert_format "<< ~s[foo]::binary >>", "<<(~s[foo])::binary>>"
    end

    test "with modifiers" do
      assert_format "<< 1 :: 1 >>", "<<1::1>>"
      assert_format "<< 1 :: 2 + 3 >>", "<<1::(2 + 3)>>"
      assert_format "<< 1 :: 2 - integer >>", "<<1::2-integer>>"
      assert_format "<< 1 :: 2 - unit(3) >>", "<<1::2-unit(3)>>"
      assert_format "<< 1 :: 2 * 3 - unit(4) >>", "<<1::2*3-unit(4)>>"
      assert_format "<< 1 :: 2 - unit(3) - 4 / 5 >>", "<<1::2-unit(3)-(4 / 5)>>"
    end

    test "in comprehensions" do
      assert_format "<< 0, 1 :: 1 <- x >>", "<<0, 1::1 <- x>>"
      assert_format "<< 0, 1 :: 2 + 3 <- x >>", "<<0, 1::(2 + 3) <- x>>"
      assert_format "<< 0, 1 :: 2 - integer <- x >>", "<<0, 1::2-integer <- x>>"
      assert_format "<< 0, 1 :: 2 - unit(3) <- x >>", "<<0, 1::2-unit(3) <- x>>"
      assert_format "<< 0, 1 :: 2 * 3 - unit(4) <- x >>", "<<0, 1::2*3-unit(4) <- x>>"
      assert_format "<< 0, 1 :: 2 - unit(3) - 4 / 5 <- x >>", "<<0, 1::2-unit(3)-(4 / 5) <- x>>"

      assert_same "<<(<<y>> <- <<x>>)>>"
      assert_same "<<(y <- <<x>>)>>"
      assert_same "<<(<<y>> <- x)>>"
    end

    test "is flex on line limits" do
      bad = "<<1, 2, 3, 4>>"

      good = """
      <<1, 2, 3,
        4>>
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

      # Doesn't preserve this because only the ending has a newline
      assert_format "<<foo, bar, baz\n>>", "<<foo, bar, baz>>"
    end

    test "preserves user choice even when it fits with trailing comma" do
      bad = """
      <<
        :hello,
        :foo,
        :bar,
      >>
      """

      assert_format bad, """
      <<
        :hello,
        :foo,
        :bar
      >>
      """
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

      map = """
      %{
        a(1, 2) => b,
        c(3, 4) => d
      }
      """

      assert_same map, @medium_length

      map = """
      %{
        a => fn x ->
          y
        end,
        b => fn y ->
          z
        end
      }
      """

      assert_same map, @medium_length

      map = """
      %{
        a =>
          for(
            y <- x,
            z <- y,
            do: 123
          )
      }
      """

      assert_same map, @medium_length

      map = """
      %{
        a =>
          for do
            :ok
          end
      }
      """

      assert_same map, @short_length
    end

    test "removes trailing comma" do
      assert_format "%{1 => 2,}", "%{1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%{:foo => :bar, baz: :bat}"

      map = """
      %{
        :foo => :bar,
        baz: :bat
      }
      """

      assert_same map, @medium_length
    end

    test "preserves user choice even when it fits" do
      assert_same """
      %{
        :hello => 1,
        :foo => 2,
        :bar => 3
      }
      """

      # Doesn't preserve this because only the ending has a newline
      assert_format "%{foo: 1, bar: 2\n}", "%{foo: 1, bar: 2}"
    end

    test "preserves user choice even when it fits with trailing comma" do
      bad = """
      %{
        hello,
        foo,
        bar,
      }
      """

      assert_format bad, """
      %{
        hello,
        foo,
        bar
      }
      """
    end

    test "preserves user choice when a newline is used after keyword" do
      good = """
      %{
        hello:
          {:ok, :world}
      }
      """

      assert_same good, @medium_length
    end

    test "preserves user choice when a newline is used after assoc" do
      good = """
      %{
        hello =>
          {:ok, :world}
      }
      """

      assert_same good, @medium_length
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

      assert_format bad, good, line_length: 11
    end

    test "removes trailing comma" do
      assert_format "%{foo | 1 => 2,}", "%{foo | 1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%{foo | :foo => :bar, baz: :bat}"

      map = """
      %{
        foo
        | :foo => :bar,
          baz: :bat
      }
      """

      assert_same map, @medium_length
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

    test "wraps operators in parens" do
      assert_format "%{foo && bar | baz: :bat}", "%{(foo && bar) | baz: :bat}"
      assert_same "%{@foo | baz: :bat}"
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

      struct = """
      %struct{
        :foo => :bar,
        baz: :bat
      }
      """

      assert_same struct, @medium_length
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

      assert_format bad, good, line_length: 11
    end

    test "removes trailing comma" do
      assert_format "%struct{foo | 1 => 2,}", "%struct{foo | 1 => 2}"
    end

    test "with keyword lists" do
      assert_same "%struct{foo | :foo => :bar, baz: :bat}"

      struct = """
      %struct{
        foo
        | :foo => :bar,
          baz: :bat
      }
      """

      assert_same struct, @medium_length
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

    test "converges" do
      bad = "hello_world(%struct{foo | 1 => 2, 3 => 4})"

      good = """
      hello_world(%struct{
        foo
        | 1 => 2,
          3 => 4
      })
      """

      assert_format bad, good, line_length: 30
    end
  end
end
