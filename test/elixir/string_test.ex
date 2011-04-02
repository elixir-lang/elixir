Code.require File.expand_path("../test_helper", __FILE__)

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
    38 = __LINE__

    ["one\n", "two\n", "three\n"] = list
  end

  def concatenate_test
    "elixir" = "eli" + "xir"
  end

  def brackets_test
    $e = "elixir"[0]
    $x = "elixir"[-3]
    $x = "elixir"[3]
    $é = "josé"[3]
    $é = "josé"[-1]
  end

  def slice_test
    "[1,"   = "[1,2,3]"[0,3]
    ",2,"   = "[1,2,3]"[2,3]
    "1,2,3" = "[1,2,3]"[1,-2]
    ""      = "[1,2,3]"[1,0]
    "é"     = "josé"[3,1]

    self.assert_error 'badarg, -> "[1,2,3]"[10,0]
    self.assert_error 'badarg, -> "[1,2,3]"[1,10]
    self.assert_error 'badarg, -> "[1,2,3]"[1,-10]
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

  def include_test
    true  = "abc".include?("a")
    true  = "abc".include?("abc")
    true  = "ab cd".include?("b c")
    false = "abc".include?("d")
  end

  def split_test
    ["foo", "baz"] = "foobarbaz".split(~r"bar")
    ["foo", "baz", "bat"] = "foobarbazbarbat".split(~r"bar")
    ["foo", "bazbarbat"] = "foobarbazbarbat".split(~r"bar", 2)
    ["foobaz"] = "foobaz".split(~r"bar")
  end

  def sub_test
    "abc"   = "abc".sub(~r(d), "d")
    "adc"   = "abc".sub(~r(b), "d")
    "a[b]c" = "abc".sub(~r(b), "[&]")
    "a[&]c" = "abc".sub(~r(b), "[\\&]")
    "a[b]c" = "abc".sub(~r[(b)], "[\\1]")
  end

  def empty_test
    false = "abc".empty?
    true  = "".empty?
  end

  def gsub_test
    "abcbe"     = "abcbe".gsub(~r(d), "d")
    "adcde"     = "abcbe".gsub(~r(b), "d")
    "a[b]c[b]e" = "abcbe".gsub(~r(b), "[&]")
    "a[&]c[&]e" = "abcbe".gsub(~r(b), "[\\&]")
    "a[b]c[b]e" = "abcbe".gsub(~r[(b)], "[\\1]")
  end

  def conversion_test
    "HELLO"  = "hello".upcase
    "HELLO"  = "HeLlO".upcase
    "hello"  = "HELLO".downcase
    "hello"  = "HeLlO".downcase
    "olleh"  = "hello".reverse
  end

end
