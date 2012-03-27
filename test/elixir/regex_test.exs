Code.require_file "../test_helper", __FILE__

defmodule Regex.BinaryTest do
  use ExUnit.Case

  test :compile do
    assert is_record(Regex.compile("foo"), :re_pattern)
  end

  test :match? do
    assert Regex.match?(%r(foo), "foo")
    assert !Regex.match?(%r(foo), "FOO")
    assert Regex.match?(%r(foo)i, "FOO")

    assert Regex.match?(%r(foo),   "afooa")
    assert !Regex.match?(%r(^foo), "afooa")
    assert Regex.match?(%r(^foo),  "fooa")
    assert !Regex.match?(%r(foo$), "afooa")
    assert Regex.match?(%r(foo$),  "afoo")
  end

  test :run do
    assert_equal ["cd", "d"], Regex.run %r"c(d)", "abcd"
    assert_equal nil, Regex.run %r"e", "abcd"
  end

  test :indexes do
    assert_equal [{2,2},{3,1}], Regex.indexes %r"c(d)", "abcd"
    assert_equal nil, Regex.indexes %r"e", "abcd"
  end

  test :scan do
    assert_equal [["d"], ["e"]], Regex.scan %r"c(d|e)", "abcd abce"
    assert_equal ["cd", "ce"], Regex.scan %r"c(?:d|e)", "abcd abce"
    assert_equal [], Regex.scan %r"e", "abcd"
  end

  test :split do
    assert_equal ["foo", "bar", "baz"], Regex.split %r" ", "foo bar baz"
    assert_equal ["foo", "bar baz"], Regex.split %r" ", "foo bar baz", 2
    assert_equal ["foobar"], Regex.split %r"\s", "foobar"
  end

  test :replace do
    assert_equal "abc",   Regex.replace(%r(d), "abc", "d")
    assert_equal "adc",   Regex.replace(%r(b), "abc", "d")
    assert_equal "a[b]c", Regex.replace(%r(b), "abc", "[&]")
    assert_equal "a[&]c", Regex.replace(%r(b), "abc", "[\\&]")
    assert_equal "a[b]c", Regex.replace(%r[(b)], "abc", "[\\1]")
  end

  test :replace_all do
    assert_equal "abcbe", Regex.replace_all(%r(d), "abcbe", "d")
    assert_equal "adcde", Regex.replace_all(%r(b), "abcbe", "d")
    assert_equal "a[b]c[b]e", Regex.replace_all(%r(b), "abcbe", "[&]")
    assert_equal "a[&]c[&]e", Regex.replace_all(%r(b), "abcbe", "[\\&]")
    assert_equal "a[b]c[b]e", Regex.replace_all(%r[(b)], "abcbe", "[\\1]")
  end
end

defmodule Regex.ListTest do
  use ExUnit.Case

  test :compile do
    assert is_record(Regex.compile("foo"), :re_pattern)
  end


  test :match? do
    assert Regex.match?(%r(foo), 'foo')
    assert !Regex.match?(%r(foo), 'FOO')
    assert Regex.match?(%r(foo)i, 'FOO')

    assert Regex.match?(%r(foo),   'afooa')
    assert !Regex.match?(%r(^foo), 'afooa')
    assert Regex.match?(%r(^foo),  'fooa')
    assert !Regex.match?(%r(foo$), 'afooa')
    assert Regex.match?(%r(foo$),  'afoo')
  end

  test :run do
    assert_equal ['cd', 'd'], Regex.run %r'c(d)', 'abcd'
    assert_equal nil, Regex.run %r'e', 'abcd'
  end

  test :indexes do
    assert_equal [{2,2},{3,1}], Regex.indexes %r'c(d)', 'abcd'
    assert_equal nil, Regex.indexes %r'e', 'abcd'
  end

  test :scan do
    assert_equal [['d'], ['e']], Regex.scan %r'c(d|e)', 'abcd abce'
    assert_equal ['cd', 'ce'], Regex.scan %r'c(?:d|e)', 'abcd abce'
    assert_equal [], Regex.scan %r'e', 'abcd'
  end

  test :split do
    assert_equal ['foo', 'bar', 'baz'], Regex.split %r' ', 'foo bar baz'
    assert_equal ['foo', 'bar baz'], Regex.split %r' ', 'foo bar baz', 2
    assert_equal ['foobar'], Regex.split %r'\s', 'foobar'
  end

  test :replace do
    assert_equal 'abc',   Regex.replace(%r(d), 'abc', 'd')
    assert_equal 'adc',   Regex.replace(%r(b), 'abc', 'd')
    assert_equal 'a[b]c', Regex.replace(%r(b), 'abc', '[&]')
    assert_equal 'a[&]c', Regex.replace(%r(b), 'abc', '[\\&]')
    assert_equal 'a[b]c', Regex.replace(%r[(b)], 'abc', '[\\1]')
  end

  test :replace_all do
    assert_equal 'abcbe', Regex.replace_all(%r(d), 'abcbe', 'd')
    assert_equal 'adcde', Regex.replace_all(%r(b), 'abcbe', 'd')
    assert_equal 'a[b]c[b]e', Regex.replace_all(%r(b), 'abcbe', '[&]')
    assert_equal 'a[&]c[&]e', Regex.replace_all(%r(b), 'abcbe', '[\\&]')
    assert_equal 'a[b]c[b]e', Regex.replace_all(%r[(b)], 'abcbe', '[\\1]')
  end
end