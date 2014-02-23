Code.require_file "test_helper.exs", __DIR__

defmodule CharListTest do
  use ExUnit.Case, async: true

  test :heredoc do
    assert __ENV__.line == 7
    assert 'foo\nbar\n' == '''
foo
bar
'''

    assert __ENV__.line == 13
    assert 'foo\nbar \'\'\'\n' == '''
foo
bar '''
'''
  end

  test :utf8 do
    assert length(' ゆんゆん') == 5
  end

  test :octals do
    assert '\1' == [1]
    assert '\12' == '\n'
    assert '\123' == 'S'
    assert '\123' == 'S'
    assert '\377' == 'ÿ'
    assert '\128' == '\n8'
    assert '\18' == [1, ?8]
  end

  test :hex do
    assert '\xa' == '\n'
    assert '\xE9' == 'é'
    assert '\xfF' == 'ÿ'
    assert '\x{A}' == '\n'
    assert '\x{e9}' == 'é'
    assert '\x{10F}' == [271]
    assert '\x{10FF}' == [4351]
    assert '\x{10FFF}' == [69631]
    assert '\x{10FFFF}' == [1114111]
  end

  test :from_char_data do
    assert List.from_char_data("æß")  == { :ok, [?æ, ?ß] }
    assert List.from_char_data("abc") == { :ok, [?a, ?b, ?c] }

    assert List.from_char_data(<< 0xDF, 0xFF >>) == { :error, [], << 223, 255 >> }
    assert List.from_char_data(<< 106, 111, 115, 195 >>) == { :incomplete, 'jos', << 195 >> }
  end

  test :from_char_data! do
    assert List.from_char_data!("æß")  == [?æ, ?ß]
    assert List.from_char_data!("abc") == [?a, ?b, ?c]

    assert_raise UnicodeConversionError,
                 "invalid encoding starting at <<223, 255>>", fn ->
      List.from_char_data!(<< 0xDF, 0xFF >>)
    end

    assert_raise UnicodeConversionError,
                 "incomplete encoding starting at <<195>>", fn ->
      List.from_char_data!(<< 106, 111, 115, 195 >>)
    end
  end

end