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
    assert '\123' == 'S'
    assert '\128' == '\n8'
    assert '\18' == [1, ?8]
  end
end