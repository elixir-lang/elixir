Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.BinaryTest do
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

  test :heredoc_with_interpolation do
    assert "37\n" == """
    #{__ENV__.line}
    """

    assert "\n42\n" == """

    #{__ENV__.line}
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

  test :octals do
    assert "\1" == <<1>>
    assert "\12" == "\n"
    assert "\123" == "S"
    assert "\123" == "S"
    assert "\377" == "ÿ"
    assert "\128" == "\n8"
    assert "\18"  == <<1, ?8>>
  end

  test :hex do
    assert "\xa" == "\n"
    assert "\xE9" == "é"
    assert "\xFF" == "ÿ"
    assert "\x{A}"== "\n"
    assert "\x{E9}"== "é"
    assert "\x{10F}" == <<196, 143>>
    assert "\x{10FF}" == <<225, 131, 191>>
    assert "\x{10FFF}" == <<240, 144, 191, 191>>
    assert "\x{10FFFF}" == <<244, 143, 191, 191>>
  end

  test :match do
    assert match?(<< ?a, _ :: binary >>, "ab")
    refute match?(<< ?a, _ :: binary >>, "cd")
    assert match?(<< _ :: utf8 >> <> _, "éf")
  end

  test :interpolation do
    res = "hello \\abc"
    assert "hello #{"\\abc"}" == res
    assert "hello #{"\\abc" <> ""}" == res
  end

  test :pattern_match do
    s = 16
    assert <<_a, _b :: size(s)>> = "foo"
  end

  test :pattern_match_with_splice do
    assert << 1, <<2, 3, 4>>, 5 >> = <<1, 2, 3, 4, 5>>
  end

  test :partial_application do
    assert (&<< &1, 2 >>).(1) == << 1, 2 >>
    assert (&<< &1, &2 >>).(1, 2) == << 1, 2 >>
    assert (&<< &2, &1 >>).(2, 1) == << 1, 2 >>
  end

  test :literal do
    assert <<106, 111, 115, 101>> == << "jose" :: binary >>
    assert <<106, 111, 115, 101>> == << "jose" :: bits >>
    assert <<106, 111, 115, 101>> == << "jose" :: bitstring >>
    assert <<106, 111, 115, 101>> == << "jose" :: bytes >>

    assert <<106, 111, 115, 101>> == << "jose" :: utf8 >>
    assert <<106, 111, 115, 101>> == << 'jose' :: utf8 >>

    assert <<0, 106, 0, 111, 0, 115, 0, 101>> == << "jose" :: utf16 >>
    assert <<0, 106, 0, 111, 0, 115, 0, 101>> == << 'jose' :: utf16 >>

    assert <<106, 0, 111, 0, 115, 0, 101, 0>> == << "jose" :: [utf16, little] >>
    assert <<106, 0, 111, 0, 115, 0, 101, 0>> == << 'jose' :: [utf16, little] >>

    assert <<0, 0, 0, 106, 0, 0, 0, 111, 0, 0, 0, 115, 0, 0, 0, 101>> == << "jose" :: utf32 >>
    assert <<0, 0, 0, 106, 0, 0, 0, 111, 0, 0, 0, 115, 0, 0, 0, 101>> == << 'jose' :: utf32 >>
  end

  test :literal_errors do
    assert_raise ArgumentError, fn ->
      Code.eval_string(%s[<< "foo" :: integer >>])
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string(%s[<< "foo" :: float >>])
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string(%s[<< 'foo' :: binary >>])
    end
  end

  @binary   "new "
  @charlist 'old '

  test :bitsyntax_with_expansion do
    assert <<@binary, "world">> == "new world"
    assert <<@charlist, "world">> == "old world"
  end

  test :bitsyntax_translation do
    refb = "sample"
    sec_data = "another"
    << size(refb) :: [size(1), big, signed, integer, unit(8)],
       refb :: binary,
       size(sec_data) :: [size(1), big, signed, integer, unit(16)],
       sec_data :: binary >>
  end

  test :bitsyntax_size_shorcut do
    assert << 1 :: 3 >> == << 1 :: size(3) >>
    assert << 1 :: [unit(8), 3] >> == << 1 :: [unit(8), size(3)] >>
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
end
