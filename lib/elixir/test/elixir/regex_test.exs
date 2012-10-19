Code.require_file "../test_helper.exs", __FILE__

defmodule Regex.BinaryTest do
  use ExUnit.Case, async: true

  test :multiline do
    refute Regex.match?(%r/^b$/, "a\nb\nc")
    assert Regex.match?(%r/^b$/m, "a\nb\nc")
  end

  test :backreference do
    assert "aa" =~ %r/(a)\1/
  end

  test :compile! do
    assert is_record(Regex.compile!("foo"), Regex)
    assert is_regex(Regex.compile!("foo"))

    assert_raise Regex.CompileError, "nothing to repeat at position 0", fn ->
      Regex.compile!("*foo")
    end
  end

  test :compile do
    { :ok, regex } = Regex.compile("foo")
    assert is_regex(regex)
    assert { :error, _ } = Regex.compile("*foo")
  end

  test :source do
    assert Regex.source(Regex.compile!("foo")) == "foo"
  end

  test :opts do
    assert Regex.opts(Regex.compile!("foo", "u")) == "u"
  end

  test :groups do
    assert Regex.groups(%r/(?<FOO>foo)/g) == [:FOO]
    assert Regex.groups(Regex.compile!("foo")) == nil
    assert Regex.groups(Regex.compile!("(?<FOO>foo)", "g")) == [:FOO]
  end

  test :match? do
    assert Regex.match?(%r/foo/, "foo")
    refute Regex.match?(%r/foo/, "FOO")
    assert Regex.match?(%r/foo/i, "FOO")
    assert Regex.match?(%r/\d{1,3}/i, "123")

    assert Regex.match?(%r/foo/,   "afooa")
    refute Regex.match?(%r/^foo/, "afooa")
    assert Regex.match?(%r/^foo/,  "fooa")
    refute Regex.match?(%r/foo$/, "afooa")
    assert Regex.match?(%r/foo$/,  "afoo")
  end

  test :captures do
    assert Keyword.equal? Regex.captures(%r/(?<foo>c)(?<bar>d)/g, 'abcd'), [bar: 'd', foo: 'c']
    assert Regex.captures(%r/c(?<foo>d)/g, 'abcd') == [foo: 'd']
    assert Regex.captures(%r/c(?<foo>d)/g, 'no_match') == nil
    assert Regex.captures(%r/c(?<foo>d|e)/g, 'abcd abce') == [foo: 'd']
    assert Regex.captures(%r/c(?<foo>d)/g, 'abcd', return: :binary) == [foo: "d"]
  end

  test :__R__ do
    assert Regex.match?(%R/f#{1,3}o/, "f#o")
  end

  test :run do
    assert Regex.run(%r"c(d)", "abcd") == ["cd", "d"]
    assert Regex.run(%r"e", "abcd") == nil
    assert Regex.run(%r"c(d)", "abcd", return: :list) == ['cd', 'd']
  end

  test :run_with_groups do
    assert Regex.run(%r/c(?<foo>d)/g, 'abcd', capture: :groups) == ['d']
    assert Regex.run(%r/c(?<foo>d)/g, 'no_match', capture: :groups) == nil
    assert Regex.run(%r/c(?<foo>d|e)/g, 'abcd abce', capture: :groups) == ['d']
    assert Regex.run(%r/c(?<foo>d)/g, 'abcd', return: :binary, capture: :groups) == ["d"]
  end

  test :run_with_indexes do
    assert Regex.run(%r"c(d)", "abcd", return: :index) == [{2,2},{3,1}]
    assert Regex.run(%r"e", "abcd", return: :index) == nil
  end

  test :index do
    assert Regex.index(%r"c(d)", "abcd") == 2
    assert Regex.index(%r"e", "abcd") == nil
  end

  test :scan do
    assert Regex.scan(%r"c(d|e)", "abcd abce") == [["d"], ["e"]]
    assert Regex.scan(%r"c(?:d|e)", "abcd abce") == ["cd", "ce"]
    assert Regex.scan(%r"e", "abcd") == []
    assert Regex.scan(%r"c(d|e)", "abcd abce", return: :list) == [['d'], ['e']]
  end

  test :split do
    assert Regex.split(%r" ", "foo bar baz") == ["foo", "bar", "baz"]
    assert Regex.split(%r" ", "foo bar baz", parts: 2) == ["foo", "bar baz"]
    assert Regex.split(%r"\s", "foobar") == ["foobar"]
    assert Regex.split(%r" ", "foo bar baz") == ["foo", "bar", "baz"]
    assert Regex.split(%r"=", "key=") == ["key", ""]
    assert Regex.split(%r"=", "=value") == ["", "value"]
  end

  test :replace do
    assert Regex.replace(%r(d), "abc", "d") == "abc"
    assert Regex.replace(%r(b), "abc", "d") == "adc"
    assert Regex.replace(%r(b), "abc", "[&]") == "a[b]c"
    assert Regex.replace(%r(b), "abc", "[\\&]") == "a[&]c"
    assert Regex.replace(%r[(b)], "abc", "[\\1]") == "a[b]c"

    assert Regex.replace(%r(d), "abcbe", "d") == "abcbe"
    assert Regex.replace(%r(b), "abcbe", "d") == "adcde"
    assert Regex.replace(%r(b), "abcbe", "[&]") == "a[b]c[b]e"
    assert Regex.replace(%r(b), "abcbe", "[\\&]") == "a[&]c[&]e"
    assert Regex.replace(%r[(b)], "abcbe", "[\\1]") == "a[b]c[b]e"
  end
end

defmodule Regex.ListTest do
  use ExUnit.Case, async: true

  test :compile do
    assert is_record(Regex.compile!('foo'), Regex)
    assert is_regex(Regex.compile!('foo'))
  end

  test :source do
    assert Regex.source(Regex.compile!('foo')) == "foo"
  end

  test :opts do
    assert Regex.opts(Regex.compile!('foo', 'u')) == "u"
  end

  test :match? do
    assert Regex.match?(%r(foo), 'foo')
    refute Regex.match?(%r(foo), 'FOO')
    assert Regex.match?(%r(foo)i, 'FOO')
    assert Regex.match?(%r/\d{1,3}/i, '123')

    assert Regex.match?(%r(foo),   'afooa')
    refute Regex.match?(%r(^foo), 'afooa')
    assert Regex.match?(%r(^foo),  'fooa')
    refute Regex.match?(%r(foo$), 'afooa')
    assert Regex.match?(%r(foo$),  'afoo')
  end

  test :run do
    assert Regex.run(%r'c(d)', 'abcd') == ['cd', 'd']
    assert Regex.run(%r'e', 'abcd') == nil
    assert Regex.run(%r"c(d)", "abcd", return: :binary) == ["cd", "d"]
  end

  test :index do
    assert Regex.index(%r'c(d)', 'abcd') == 2
    assert Regex.index(%r'e', 'abcd') == nil
  end

  test :indexes do
    assert Regex.run(%r'c(d)', 'abcd', return: :index) == [{2,2},{3,1}]
    assert Regex.run(%r'e', 'abcd', return: :index) == nil
  end

  test :scan do
    assert Regex.scan(%r'c(d|e)', 'abcd abce') == [['d'], ['e']]
    assert Regex.scan(%r'c(?:d|e)', 'abcd abce') == ['cd', 'ce']
    assert Regex.scan(%r'e', 'abcd') == []
    assert Regex.scan(%r'c(d|e)', 'abcd abce', return: :binary) == [["d"], ["e"]]
  end

  test :split do
    assert Regex.split(%r' ', 'foo bar baz') == ['foo', 'bar', 'baz']
    assert Regex.split(%r' ', 'foo bar baz', parts: 2) == ['foo', 'bar baz']
    assert Regex.split(%r'\s', 'foobar') == ['foobar']
  end

  test :replace do
    assert Regex.replace(%r(d), 'abc', 'd') == 'abc'
    assert Regex.replace(%r(b), 'abc', 'd') == 'adc'
    assert Regex.replace(%r(b), 'abc', '[&]') == 'a[b]c'
    assert Regex.replace(%r(b), 'abc', '[\\&]') == 'a[&]c'
    assert Regex.replace(%r[(b)], 'abc', '[\\1]') == 'a[b]c'

    assert Regex.replace(%r(d), 'abcbe', 'd') == 'abcbe'
    assert Regex.replace(%r(b), 'abcbe', 'd') == 'adcde'
    assert Regex.replace(%r(b), 'abcbe', '[&]') == 'a[b]c[b]e'
    assert Regex.replace(%r(b), 'abcbe', '[\\&]') == 'a[&]c[&]e'
    assert Regex.replace(%r[(b)], 'abcbe', '[\\1]') == 'a[b]c[b]e'
  end
end
