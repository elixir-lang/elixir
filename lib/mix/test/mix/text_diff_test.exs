Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.TextDiffTest do
  use ExUnit.Case

  alias Mix.TextDiff

  doctest Mix.TextDiff, tags: :doctests

  describe "format/3" do
    test "with unchanged texts" do
      assert TextDiff.format("abc", "abc") == []
      assert to_binary("abc", "abc") == ""
    end

    test "with one deleted line" do
      old = "del"
      new = ""

      assert TextDiff.format(old, new) == [
               [
                 [["1", " ", " "], [[[[] | "\e[31m"], " - "] | "\e[0m"], "|"],
                 [[], [[[[] | "\e[31m"], "del"] | "\e[0m"]],
                 "\n"
               ],
               [[[" ", " ", "1"], [[[[] | "\e[32m"], " + "] | "\e[0m"], "|"], [], "\n"]
             ]

      assert to_binary(old, new, color: false) == """
             1   - |del
               1 + |
             """
    end

    test "with one changed line" do
      old = "one three two"
      new = "one two three"

      assert TextDiff.format(old, new) == [
               [
                 [["1", " ", " "], [[[[] | "\e[31m"], " - "] | "\e[0m"], "|"],
                 [
                   [[[] | "one t"] | "hree"],
                   [[[[] | "\e[31m"], ""] | "\e[0m"],
                   [[[[] | "\e[41m"], " "] | "\e[0m"],
                   [[[[] | "\e[31m"], "two"] | "\e[0m"]
                 ],
                 "\n"
               ],
               [
                 [[" ", " ", "1"], [[[[] | "\e[32m"], " + "] | "\e[0m"], "|"],
                 [
                   [
                     [[] | "one t"],
                     [[[[] | "\e[32m"], "wo"] | "\e[0m"],
                     [[[[] | "\e[42m"], " "] | "\e[0m"],
                     [[[[] | "\e[32m"], "t"] | "\e[0m"]
                   ]
                   | "hree"
                 ],
                 "\n"
               ]
             ]

      assert to_binary(old, new, color: false) == """
             1   - |one three two
               1 + |one two three
             """
    end

    test "with one deleted line in the middle" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg
      """

      new = """
      aaa
      bbb
      ccc
      eee
      fff
      ggg
      """

      exp = """
         ...|
      2 2   |bbb
      3 3   |ccc
      4   - |ddd
      5 4   |eee
      6 5   |fff
         ...|
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false) == exp
    end

    test "with multiple deleted lines" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg\
      """

      new = """
      aaa
      ggg\
      """

      exp = """
      1 1   |aaa
      2   - |bbb
      3   - |ccc
      4   - |ddd
      5   - |eee
      6   - |fff
      7 2   |ggg
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false) == exp
    end

    test "with one added line in the middle" do
      old = """
      aaa
      bbb
      ccc
      eee
      fff
      ggg
      """

      new = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg
      """

      exp = """
         ...|
      2 2   |bbb
      3 3   |ccc
        4 + |ddd
      4 5   |eee
      5 6   |fff
         ...|
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false) == exp
    end

    test "with changed first line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      """

      new = """
      axa
      bbb
      ccc
      ddd
      """

      exp = """
      1   - |aaa
        1 + |axa
      2 2   |bbb
      3 3   |ccc
         ...|
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false) == exp
    end

    test "with changed last line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      """

      new = """
      aaa
      bbb
      ccc
      dxd
      """

      exp = """
         ...|
      2 2   |bbb
      3 3   |ccc
      4   - |ddd
        4 + |dxd
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false) == exp
    end

    test "with changed first and last line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      """

      new = """
      axa
      bbb
      ccc
      ddd
      exe
      """

      exp = """
      1   - |aaa
        1 + |axa
      2 2   |bbb
         ...|
      4 4   |ddd
      5   - |eee
        5 + |exe
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false, before: 1, after: 1) == exp
    end

    test "with changed second and second last line" do
      old = """
      aaa
      bbb
      ccc
      ddd
      eee
      fff
      ggg
      hhh
      iii\
      """

      new = """
      aaa
      bXb
      ccc
      ddd
      eee
      fff
      ggg
      hXh
      iii\
      """

      exp = """
      1 1   |aaa
      2   - |bbb
        2 + |bXb
      3 3   |ccc
         ...|
      7 7   |ggg
      8   - |hhh
        8 + |hXh
      9 9   |iii
      """

      assert TextDiff.format(old, new)

      assert to_binary(old, new, color: false, before: 1, after: 1) == exp
    end
  end

  defp to_binary(old, new, opts \\ []) do
    old
    |> TextDiff.format(new, opts)
    |> IO.iodata_to_binary()
  end
end
