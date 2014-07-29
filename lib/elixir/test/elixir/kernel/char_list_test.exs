Code.require_file "../test_helper.exs", __DIR__

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
end
