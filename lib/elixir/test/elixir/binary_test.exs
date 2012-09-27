Code.require_file "../test_helper.exs", __FILE__

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
    assert <<a, b :: size(s)>> = "foo"
  end

  test :bitsyntax_translation do
    refb = "sample"
    sec_data = "another"
    << size(refb) :: [size(1), big, signed, integer, unit(8)],
       refb :: binary,
       size(sec_data) :: [size(1), big, signed, integer, unit(16)],
       sec_data :: binary >>
  end

  defmacrop signed_16 do
    quote do
      [big, signed, integer, unit(16)]
    end
  end

  defmacrop refb_spec do
    quote do
      [size(1), big, signed, integer, unit(8)]
    end
  end

  test :bitsyntax_macro do
    refb = "sample"
    sec_data = "another"
    << size(refb) :: refb_spec,
       refb :: binary,
       size(sec_data) :: [size(1) | signed_16],
       sec_data :: binary >>
  end

  defp is_match?(<<char, _ :: binary>>, char) do
    true
  end

  defp is_match?(_, _) do
    false
  end
end