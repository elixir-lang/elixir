Code.require_file "../test_helper.exs", __FILE__

defmodule StringTest do
  use ExUnit.Case, async: true

  test :split do
    assert String.split("a,b,c", ",") == ["a", "b,c"]
    assert String.split("a,b,c", ",", global: true) == ["a", "b", "c"]
    assert String.split("foo bar") == ["foo", "bar"]
    assert String.split("1,2 3,4", [" ", ","]) == ["1", "2 3,4"]
    assert String.split("1,2 3,4", [" ", ","], global: true) == ["1", "2", "3", "4"]
    assert String.split("a,b", ".") == ["a,b"]
  end

  test :split_with_regex do
    assert String.split("a,b", %r{,}) == ["a", "b"]
    assert String.split("a,b,c", %r{,}) == ["a", "b,c"]
    assert String.split("a,b,c", %r{,}, global: true) == ["a", "b", "c"]
    assert String.split("a,b", %r{\.}) == ["a,b"]
  end

  test :upcase do
    assert String.upcase("123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz") == "123 ABCD 456 EFG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ"
    assert String.upcase("") == ""
  end

  test :upcase_utf8 do
    assert String.upcase("& % # àáâ ãäå 1 2 ç æ") == "& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ"
    assert String.upcase("àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ") == "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"
  end

  test :downcase do
    assert String.downcase("123 ABcD 456 EfG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ") == "123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz"
    assert String.downcase("") == ""
  end

  test :downcase_utf8 do
    assert String.downcase("& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ") == "& % # àáâ ãäå 1 2 ç æ"
    assert String.downcase("ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ") == "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ"
  end
end
