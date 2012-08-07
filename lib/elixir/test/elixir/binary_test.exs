Code.require_file "../test_helper", __FILE__

defmodule Binary.LiteralTest do
  use ExUnit.Case, async: true

  test :heredoc do
    assert 7 == __ENV__.line
    assert "foo\nbar\n" == """
foo
bar
"""

    assert 13 == __ENV__.line
    assert "foo\nbar \"\"\"\n" == """
foo
bar """
"""
  end

  test :heredoc_with_extra do
    assert 21 == __ENV__.line
    assert "foo\nbar\nbar\n" == """ <> "bar\n"
foo
bar
"""
  end

  test :aligned_heredoc do
    assert "foo\nbar\nbar\n" == """ <> "bar\n"
    foo
    bar
    """
  end

  test :utf8 do
    assert size(" ゆんゆん") == 13
  end

  test :utf8_char do
    assert ?ゆ == 12422
    assert ?\ゆ == 12422
  end

  test :string_concatenation_as_match do
    "foo" <> x = "foobar"
    assert x == "bar"
  end

  test :__B__ do
    assert %B(foo) == "foo"
    assert %B[foo] == "foo"
    assert %B{foo} == "foo"
    assert %B'foo' == "foo"
    assert %B"foo" == "foo"
    assert %B|foo| == "foo"
    assert %B(f#{o}o) == "f\#{o}o"
    assert %B(f\no) == "f\\no"
  end

  test :__b__ do
    assert %b(foo) == "foo"
    assert %b(f#{:o}o) == "foo"
    assert %b(f\no) == "f\no"
  end

  test :__B__with_heredoc do
    assert "  f\#{o}o\\n\n" == %B"""
      f#{o}o\n
    """
  end

  test :__b__with_heredoc do
    assert "  foo\n\n" == %b"""
      f#{:o}o\n
    """
  end

  test :octals do
    assert "\123" == "S"
    assert "\128" == "\n8"
    assert "\18"  == <<1,?8>>
  end

  test :match do
    assert is_match?("ab", ?a)
    assert not is_match?("cd", ?a)
  end

  test :pattern_match do
    s = 16
    assert <<a, b|s>> = "foo"
  end

  test :bitsyntax_translation do
    refb = "sample"
    sec_data = "another"
    << size(refb) | 1 - :big - :signed - :integer - {:unit, 8},
       refb | :binary,
       size(sec_data) | 1 - :big - :signed - :integer - {:unit, 16},
       sec_data|:binary >>
  end

  defp is_match?(<<char, _|:binary>>, char) do
    true
  end

  defp is_match?(_, _) do
    false
  end
end

defmodule BinaryTest do
  use ExUnit.Case, async: true

  test :part do
    assert Binary.part("foo", 1, 2) == "oo"
    assert Binary.part("foo", 0, 3) == "foo"
    assert Binary.part("foobar", 6, 0) == ""
    assert Binary.part("foobar", 9, 3) == nil
    assert Binary.part("foobar", 3, 10) == "bar"
    assert Binary.part("foobar", 3, -3) == "foo"
    assert Binary.part("foobar", 3, -6) == "foo"
    assert Binary.part("foobar", 0, -3) == ""
  end

  test :part_with_range do
    assert Binary.part("foo", 1..2) == "oo"
    assert Binary.part("foobar", 2..4) == "oba"
    assert Binary.part("foobar", 0..0) == "f"
    assert Binary.part("foobar", 0..-1) == "foobar"
    assert Binary.part("foobar", 1..-2) == "ooba"
    assert Binary.part("foobar", -3..-1) == "bar"
  end

  test :split do
    assert Binary.split("a,b", ",") == ["a", "b"]
    assert Binary.split("a,b,c", ",") == ["a", "b,c"]
    assert Binary.split("foo bar") == ["foo", "bar"]
    assert Binary.split("1,2 3,4", [" ", ","]) == ["1", "2 3,4"]
    assert Binary.split("1,2 3,4", [" ", ","], global: true) == ["1", "2", "3", "4"]
  end

  test :split_with_regex do
    assert Binary.split("a,b", %r{,}) == ["a", "b"]
    assert Binary.split("a,b,c", %r{,}) == ["a", "b,c"]
    assert Binary.split("a,b,c", %r{,}, global: true) == ["a", "b", "c"]
  end
end
