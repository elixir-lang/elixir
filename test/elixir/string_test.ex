object StringTest
  proto ExUnit::Case

  % Please leave this test at the top as we are asserting
  % the result of __LINE__ at some point.
  def heredoc_test
    "abc\n" = ~~
abc
~~

    "(~a~)\n" = ~~STRING
(~a~)
~~

    "abc ~~\n" = ~~STRING
abc ~~
~~

    "~~ abc\n" = ~~STRING
~~ abc
~~

    "abc\n123" = ~~STRING + "123"
abc
~~

    list = [~~FOO, ~~BAR, ~~BAZ]
one
~~
#{'two}
~~
three
~~

    % Ensure backtrace line is still the same
    36 = __LINE__

    ["one\n", "two\n", "three\n"] = list
  end

  def concatenate_test
    "elixir" = "eli" + "xir"
  end

  def length_test
    0 = "".length
    6 = "elixir".length
    4 = "josé".length
    3 = "講中文".length
    5 = "こんにちは".length
  end

  def to_list_test
    [115, 116, 114, 105, 110, 103] = "string".to_list
    [115, 116, 114, 105, 110, 103] = "string".to_char_list
  end

  def to_bin_test
    <<115, 116, 114, 105, 110, 103>> = "string".to_bin
  end

  def to_s_test
    "Hello" = "Hello".to_s
  end

  def to_atom_test
    'hello = "hello".to_atom
  end
end
