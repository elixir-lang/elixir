Code.require_file "test_helper.exs", __DIR__

defmodule RegexTest do
  use ExUnit.Case, async: true

  test :multiline do
    refute Regex.match?(~r/^b$/, "a\nb\nc")
    assert Regex.match?(~r/^b$/m, "a\nb\nc")
  end

  test :precedence do
    assert { "aa", :unknown } |> elem(0) =~ ~r/(a)\1/
  end

  test :backreference do
    assert "aa" =~ ~r/(a)\1/
  end

  test :compile! do
    assert Regex.regex?(Regex.compile!("foo"))

    assert_raise Regex.CompileError, ~r/position 0$/, fn ->
      Regex.compile!("*foo")
    end
  end

  test :compile do
    { :ok, regex } = Regex.compile("foo")
    assert Regex.regex?(regex)
    assert { :error, _ } = Regex.compile("*foo")
    assert { :error, _ } = Regex.compile("foo", "y")
  end

  test :compile_with_erl_opts do
    { :ok, regex } = Regex.compile("foo\\sbar", [:dotall, {:newline, :anycrlf}])
    assert "foo\nbar" =~ regex
  end

  test :regex? do
    assert Regex.regex?(~r/foo/)
    refute Regex.regex?(0)
  end

  test :source do
    assert Regex.source(Regex.compile!("foo")) == "foo"
  end

  test :opts do
    assert Regex.opts(Regex.compile!("foo", "i")) == "i"
  end

  test :unicode_by_default do
    assert ("josé" =~ ~r"\p{Latin}$")
    refute ("£" =~ ~r/\p{Lu}/)
  end

  test :groups do
    assert Regex.groups(~r/(?<FOO>foo)/) == [:FOO]
  end

  test :match? do
    assert Regex.match?(~r/foo/, "foo")
    refute Regex.match?(~r/foo/, "FOO")
    assert Regex.match?(~r/foo/i, "FOO")
    assert Regex.match?(~r/\d{1,3}/i, "123")

    assert Regex.match?(~r/foo/,   "afooa")
    refute Regex.match?(~r/^foo/, "afooa")
    assert Regex.match?(~r/^foo/,  "fooa")
    refute Regex.match?(~r/foo$/, "afooa")
    assert Regex.match?(~r/foo$/,  "afoo")
  end

  test :named_captures do
    assert Keyword.equal? Regex.named_captures(~r/(?<foo>c)(?<bar>d)/, "abcd"), [bar: "d", foo: "c"]
    assert Regex.named_captures(~r/c(?<foo>d)/, "abcd") == [foo: "d"]
    assert Regex.named_captures(~r/c(?<foo>d)/, "no_match") == nil
    assert Regex.named_captures(~r/c(?<foo>d|e)/, "abcd abce") == [foo: "d"]
    assert Regex.named_captures(~r/c(.)/, "cat") == []
  end

  test :sigil_R do
    assert Regex.match?(~R/f#{1,3}o/, "f#o")
  end

  test :run do
    assert Regex.run(~r"c(d)", "abcd") == ["cd", "d"]
    assert Regex.run(~r"e", "abcd") == nil
  end

  test :run_with_groups do
    assert Regex.run(~r/c(?<foo>d)/, "abcd", capture: :groups) == ["d"]
    assert Regex.run(~r/c(?<foo>d)/, "no_match", capture: :groups) == nil
    assert Regex.run(~r/c(?<foo>d|e)/, "abcd abce", capture: :groups) == ["d"]
  end

  test :run_with_indexes do
    assert Regex.run(~r"c(d)", "abcd", return: :index) == [{2, 2}, {3, 1}]
    assert Regex.run(~r"e", "abcd", return: :index) == nil
  end

  test :scan do
    assert Regex.scan(~r"c(d|e)", "abcd abce") == [["cd", "d"], ["ce", "e"]]
    assert Regex.scan(~r"c(?:d|e)", "abcd abce") == [["cd"], ["ce"]]
    assert Regex.scan(~r"e", "abcd") == []
  end

  test :scan_with_groups do
    assert Regex.scan(~r/cd/, "abcd", capture: :groups) == []
    assert Regex.scan(~r/c(?<foo>d)/, "abcd", capture: :groups) == [["d"]]
    assert Regex.scan(~r/c(?<foo>d)/, "no_match", capture: :groups) == []
    assert Regex.scan(~r/c(?<foo>d|e)/, "abcd abce", capture: :groups) == [["d"], ["e"]]
  end

  test :split do
    assert Regex.split(~r",", "") == [""]
    assert Regex.split(~r" ", "foo bar baz") == ["foo", "bar", "baz"]
    assert Regex.split(~r" ", "foo bar baz", parts: 2) == ["foo", "bar baz"]
    assert Regex.split(~r"\s", "foobar") == ["foobar"]
    assert Regex.split(~r" ", " foo bar baz ") == ["", "foo", "bar", "baz", ""]
    assert Regex.split(~r" ", " foo bar baz ", trim: true) == ["foo", "bar", "baz"]
    assert Regex.split(~r"=", "key=") == ["key", ""]
    assert Regex.split(~r"=", "=value") == ["", "value"]
  end

  test :replace do
    assert Regex.replace(~r(d), "abc", "d") == "abc"
    assert Regex.replace(~r(b), "abc", "d") == "adc"
    assert Regex.replace(~r(b), "abc", "[&]") == "a[b]c"
    assert Regex.replace(~r(b), "abc", "[\\&]") == "a[&]c"
    assert Regex.replace(~r[(b)], "abc", "[\\1]") == "a[b]c"

    assert Regex.replace(~r(d), "abcbe", "d") == "abcbe"
    assert Regex.replace(~r(b), "abcbe", "d") == "adcde"
    assert Regex.replace(~r(b), "abcbe", "[&]") == "a[b]c[b]e"
    assert Regex.replace(~r(b), "abcbe", "[\\&]") == "a[&]c[&]e"
    assert Regex.replace(~r[(b)], "abcbe", "[\\1]") == "a[b]c[b]e"
  end

  test :escape do
    assert matches_escaped?(".")
    refute matches_escaped?(".", "x")

    assert matches_escaped?("[\w]")
    refute matches_escaped?("[\w]", "x")

    assert matches_escaped?("\\")

    assert matches_escaped?("\\xff", "\\xff")
    refute matches_escaped?("\\xff", "\xff")

    assert matches_escaped?("(")
    assert matches_escaped?("()")
    assert matches_escaped?("(?:foo)")

    assert matches_escaped?("\\A  \\z")
    assert matches_escaped?("  x  ")
    assert matches_escaped?("  x    x ") # unicode spaces here
    assert matches_escaped?("# lol")

    assert matches_escaped?("\\A.^$*+?()[{\\| \t\n\xff\\z #hello\x{202F}\x{205F}")
  end

  defp matches_escaped?(string) do
    matches_escaped?(string, string)
  end

  defp matches_escaped?(string, match) do
    Regex.match? ~r/#{Regex.escape(string)}/simx, match
  end
end
