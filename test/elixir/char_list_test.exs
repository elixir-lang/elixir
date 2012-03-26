Code.require_file "../test_helper", __FILE__

defmodule CharListTest do
  use ExUnit.Case

  test :heredoc do
    assert_equal 7, __LINE__
    assert_equal 'foo\nbar\n', '''
foo
bar
'''

    assert_equal 13, __LINE__
    assert_equal 'foo\nbar \'\'\'\n', '''
foo
bar '''
'''
  end

  test :utf8 do
    assert_equal 13, length(' ゆんゆん')
  end

  test :octals do
    assert_equal 'S', '\123'
    assert_equal '\n8', '\128'
    assert_equal [1, ?8], '\18'
  end
end