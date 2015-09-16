Code.require_file "../test_helper.exs", __DIR__

defmodule CharListTest do
  use ExUnit.Case, async: true

  test "heredoc" do
    assert __ENV__.line == 7
    assert 'foo\nbar\n' == '''
foo
bar
'''

    assert __ENV__.line == 13
    assert 'foo\nbar \'\'\'\n' == '''
foo
bar \'\'\'
'''
  end

  test "utf8" do
    assert length(' ゆんゆん') == 5
  end

  test "hex" do
    assert '\x76' == 'v'
    assert '\u00fF' == 'ÿ'
    assert '\u{A}' == '\n'
    assert '\u{e9}' == 'é'
    assert '\u{10F}' == [271]
    assert '\u{10FF}' == [4351]
    assert '\u{10FFF}' == [69631]
    assert '\u{10FFFF}' == [1114111]
  end
end
