Code.require_file "../test_helper.exs", __FILE__

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
    assert length(' ゆんゆん') == 13
  end

  test :octals do
    assert '\o1' == [1]
    assert '\o12' == '\n'
    assert '\o123' == 'S'
    assert '\O123' == 'S'
    assert '\o377' == 'ÿ'
    assert '\o128' == '\n8'
    assert '\o18' == [1, ?8]
  end

  test :hex do
    assert '\xa' == '\n'
    assert '\xE9' == 'é'
    assert '\xfF' == 'ÿ'
    assert '\x{A}' == '\n'
    assert '\x{e9}' == 'é'
    assert '\x{10F}' == [196,143]
    assert '\x{10FF}' == [225,131,191]
    assert '\x{10FFF}' == [240,144,191,191]
    assert '\x{10FFFF}' == [244,143,191,191]
  end

end