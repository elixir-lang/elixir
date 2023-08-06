Code.require_file("../test_helper.exs", __DIR__)

defmodule IO.ANSITest do
  use ExUnit.Case, async: true

  doctest IO.ANSI

  test "format ansicode" do
    assert IO.chardata_to_string(IO.ANSI.format(:green, true)) ==
             "#{IO.ANSI.green()}#{IO.ANSI.reset()}"

    assert IO.chardata_to_string(IO.ANSI.format(:green, false)) == ""
  end

  test "format binary" do
    assert IO.chardata_to_string(IO.ANSI.format("Hello, world!", true)) == "Hello, world!"

    assert IO.chardata_to_string(IO.ANSI.format("A map: %{foo: :bar}", false)) ==
             "A map: %{foo: :bar}"
  end

  test "format empty list" do
    assert IO.chardata_to_string(IO.ANSI.format([], true)) == ""
    assert IO.chardata_to_string(IO.ANSI.format([], false)) == ""
  end

  test "format ansicode list" do
    assert IO.chardata_to_string(IO.ANSI.format([:red, :bright], true)) ==
             "#{IO.ANSI.red()}#{IO.ANSI.bright()}#{IO.ANSI.reset()}"

    assert IO.chardata_to_string(IO.ANSI.format([:red, :bright], false)) == ""
  end

  test "format binary list" do
    assert IO.chardata_to_string(IO.ANSI.format(["Hello, ", "world!"], true)) == "Hello, world!"
    assert IO.chardata_to_string(IO.ANSI.format(["Hello, ", "world!"], false)) == "Hello, world!"
  end

  test "format charlist" do
    assert IO.chardata_to_string(IO.ANSI.format(~c"Hello, world!", true)) == "Hello, world!"
    assert IO.chardata_to_string(IO.ANSI.format(~c"Hello, world!", false)) == "Hello, world!"
  end

  test "format mixed list" do
    data = ["Hello", ?,, 32, :red, "world!"]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
             "Hello, #{IO.ANSI.red()}world!#{IO.ANSI.reset()}"

    assert IO.chardata_to_string(IO.ANSI.format(data, false)) == "Hello, world!"
  end

  test "format nested list" do
    data = ["Hello, ", ["nested", 32, :red, "world!"]]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
             "Hello, nested #{IO.ANSI.red()}world!#{IO.ANSI.reset()}"

    assert IO.chardata_to_string(IO.ANSI.format(data, false)) == "Hello, nested world!"
  end

  test "format improper list" do
    data = ["Hello, ", :red, "world" | "!"]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
             "Hello, #{IO.ANSI.red()}world!#{IO.ANSI.reset()}"

    assert IO.chardata_to_string(IO.ANSI.format(data, false)) == "Hello, world!"
  end

  test "format nested improper list" do
    data = [["Hello, " | :red], "world!" | :green]

    assert IO.chardata_to_string(IO.ANSI.format(data, true)) ==
             "Hello, #{IO.ANSI.red()}world!#{IO.ANSI.green()}#{IO.ANSI.reset()}"

    assert IO.chardata_to_string(IO.ANSI.format(data, false)) == "Hello, world!"
  end

  test "format fragment" do
    assert IO.chardata_to_string(IO.ANSI.format_fragment([:red, "Hello!"], true)) ==
             "#{IO.ANSI.red()}Hello!"
  end

  test "format invalid sequence" do
    assert_raise ArgumentError, "invalid ANSI sequence specification: :brigh", fn ->
      IO.ANSI.format([:brigh, "Hello!"], true)
    end

    assert_raise ArgumentError, "invalid ANSI sequence specification: nil", fn ->
      IO.ANSI.format(["Hello!", nil], true)
    end

    assert_raise ArgumentError, "invalid ANSI sequence specification: :brigh", fn ->
      IO.ANSI.format([:brigh, "Hello!"], false)
    end

    assert_raise ArgumentError, "invalid ANSI sequence specification: nil", fn ->
      IO.ANSI.format(["Hello!", nil], false)
    end

    assert_raise ArgumentError, "invalid ANSI sequence specification: :invalid", fn ->
      IO.ANSI.format(:invalid, false)
    end
  end

  test "colors" do
    assert IO.ANSI.red() == "\e[31m"
    assert IO.ANSI.light_red() == "\e[91m"

    assert IO.ANSI.red_background() == "\e[41m"
    assert IO.ANSI.light_red_background() == "\e[101m"
  end

  test "color/1" do
    assert IO.ANSI.color(0) == "\e[38;5;0m"
    assert IO.ANSI.color(42) == "\e[38;5;42m"
    assert IO.ANSI.color(255) == "\e[38;5;255m"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color(-1)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color(256)
    end
  end

  test "color/3" do
    assert IO.ANSI.color(0, 4, 2) == "\e[38;5;42m"
    assert IO.ANSI.color(1, 1, 1) == "\e[38;5;59m"
    assert IO.ANSI.color(5, 5, 5) == "\e[38;5;231m"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color(0, 6, 1)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color(5, -1, 1)
    end
  end

  test "color_background/1" do
    assert IO.ANSI.color_background(0) == "\e[48;5;0m"
    assert IO.ANSI.color_background(42) == "\e[48;5;42m"
    assert IO.ANSI.color_background(255) == "\e[48;5;255m"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color_background(-1)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color_background(256)
    end
  end

  test "color_background/3" do
    assert IO.ANSI.color_background(0, 4, 2) == "\e[48;5;42m"
    assert IO.ANSI.color_background(1, 1, 1) == "\e[48;5;59m"
    assert IO.ANSI.color_background(5, 5, 5) == "\e[48;5;231m"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color_background(0, 6, 1)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.color_background(5, -1, 1)
    end
  end

  test "cursor/2" do
    assert IO.ANSI.cursor(0, 0) == "\e[0;0H"
    assert IO.ANSI.cursor(11, 12) == "\e[11;12H"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor(-1, 5)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor(5, -1)
    end
  end

  test "cursor_up/1" do
    assert IO.ANSI.cursor_up() == "\e[1A"
    assert IO.ANSI.cursor_up(12) == "\e[12A"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_up(0)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_up(-1)
    end
  end

  test "cursor_down/1" do
    assert IO.ANSI.cursor_down() == "\e[1B"
    assert IO.ANSI.cursor_down(2) == "\e[2B"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_right(0)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_down(-1)
    end
  end

  test "cursor_left/1" do
    assert IO.ANSI.cursor_left() == "\e[1D"
    assert IO.ANSI.cursor_left(3) == "\e[3D"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_left(0)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_left(-1)
    end
  end

  test "cursor_right/1" do
    assert IO.ANSI.cursor_right() == "\e[1C"
    assert IO.ANSI.cursor_right(4) == "\e[4C"

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_right(0)
    end

    assert_raise FunctionClauseError, fn ->
      IO.ANSI.cursor_right(-1)
    end
  end
end
