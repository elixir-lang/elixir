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

  test :aligned_heredoc do
    assert "foo\nbar\n" == """
    foo
    bar
    """
  end

  test :heredoc_with_interpolation do
    assert "29\n" == """
    #{__ENV__.line}
    """

    assert "\n34\n" == """

    #{__ENV__.line}
    """
  end

  test :heredoc_in_call do
    assert "foo\nbar" == Kernel.<>("""
    foo
    """, "bar")
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

    <<"foo">> <> x = "foobar"
    assert x == "bar"

    <<"f", "oo">> <> x = "foobar"
    assert x == "bar"

    <<x :: [binary, size(3)]>> <> _ = "foobar"
    assert x == "foo"

    size = 3
    <<x :: [binary, size(size)]>> <> _ = "foobar"
    assert x == "foo"

    <<x :: [binary, size(6), unit(4)]>> <> _ = "foobar"
    assert x == "foo"

    assert_raise ErlangError, fn ->
      Code.eval_string(~s(<<x :: [binary, size(3), unit(4)]>> <> _ = "foobar"))
    end

    assert_raise ErlangError, fn ->
      Code.eval_string(~s(<<x :: [integer, size(4)]>> <> _ = "foobar"))
    end
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
    assert <<106,111,115,195,169>> == << "josé" :: binary >>
    assert <<106,111,115,195,169>> == << "josé" :: bits >>
    assert <<106,111,115,195,169>> == << "josé" :: bitstring >>
    assert <<106,111,115,195,169>> == << "josé" :: bytes >>

    assert <<106,111,115,195,169>> == << "josé" :: utf8 >>
    assert <<0,106,0,111,0,115,0,233>> == << "josé" :: utf16 >>
    assert <<106,0,111,0,115,0,233,0>> == << "josé" :: [utf16, little] >>
    assert <<0,0,0,106,0,0,0,111,0,0,0,115,0,0,0,233>> == << "josé" :: utf32 >>
  end

  test :literal_errors do
    assert_raise CompileError, fn ->
      Code.eval_string(~s[<< "foo" :: integer >>])
    end

    assert_raise CompileError, fn ->
      Code.eval_string(~s[<< "foo" :: float >>])
    end

    assert_raise CompileError, fn ->
      Code.eval_string(~s[<< 'foo' :: binary >>])
    end

    assert_raise ArgumentError, fn ->
      Code.eval_string(~s[<<1::size(4)>> <> "foo"])
    end
  end

  @binary "new "

  test :bitsyntax_with_expansion do
    assert <<@binary, "world">> == "new world"
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
       size(sec_data) :: [size(1), signed_16],
       sec_data :: binary >>
  end
end
