# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("test_helper.exs", __DIR__)

defmodule StringTest do
  use ExUnit.Case, async: true

  doctest String

  test "next_codepoint/1" do
    assert String.next_codepoint("ésoj") == {"é", "soj"}
    assert String.next_codepoint(<<255>>) == {<<255>>, ""}
    assert String.next_codepoint("") == nil
  end

  # test cases described in https://mortoray.com/2013/11/27/the-string-type-is-broken/
  test "Unicode" do
    assert String.reverse("noël") == "lëon"
    assert String.slice("noël", 0..2) == "noë"
    assert String.length("noël") == 4

    assert String.length("") == 2
    assert String.slice("", 1..1) == ""
    assert String.reverse("") == ""

    assert String.upcase("baﬄe") == "BAFFLE"

    assert String.equivalent?("noël", "noël")
  end

  test "split/1,2,3" do
    assert String.split("") == []
    assert String.split("foo bar") == ["foo", "bar"]
    assert String.split(" foo bar") == ["foo", "bar"]
    assert String.split("foo bar ") == ["foo", "bar"]
    assert String.split(" foo bar ") == ["foo", "bar"]
    assert String.split("foo\t\n\v\f\r\sbar\n") == ["foo", "bar"]
    assert String.split("foo" <> <<194, 133>> <> "bar") == ["foo", "bar"]
    # information separators are not considered whitespace
    assert String.split("foo\u001Fbar") == ["foo\u001Fbar"]
    # no-break space is excluded
    assert String.split("foo\00A0bar") == ["foo\00A0bar"]
    assert String.split("foo\u202Fbar") == ["foo\u202Fbar"]

    assert String.split("a,b,c", ",") == ["a", "b", "c"]
    assert String.split("a,b", ".") == ["a,b"]
    assert String.split("1,2 3,4", [" ", ","]) == ["1", "2", "3", "4"]

    assert String.split("", ",") == [""]
    assert String.split(" a b c ", " ") == ["", "a", "b", "c", ""]
    assert String.split(" a b c ", " ", parts: :infinity) == ["", "a", "b", "c", ""]
    assert String.split(" a b c ", " ", parts: 1) == [" a b c "]
    assert String.split(" a b c ", " ", parts: 2) == ["", "a b c "]

    assert String.split("", ",", trim: true) == []
    assert String.split(" a b c ", " ", trim: true) == ["a", "b", "c"]
    assert String.split(" a b c ", " ", trim: true, parts: :infinity) == ["a", "b", "c"]
    assert String.split(" a b c ", " ", trim: true, parts: 1) == [" a b c "]
    assert String.split(" a b c ", " ", trim: true, parts: 2) == ["a", "b c "]

    assert String.split("abé", "") == ["", "a", "b", "é", ""]
    assert String.split("abé", "", parts: :infinity) == ["", "a", "b", "é", ""]
    assert String.split("abé", "", parts: 1) == ["abé"]
    assert String.split("abé", "", parts: 2) == ["", "abé"]
    assert String.split("abé", "", parts: 3) == ["", "a", "bé"]
    assert String.split("abé", "", parts: 4) == ["", "a", "b", "é"]
    assert String.split("abé", "", parts: 5) == ["", "a", "b", "é", ""]
    assert String.split("abé", "", parts: 10) == ["", "a", "b", "é", ""]
    assert String.split("abé", "", trim: true) == ["a", "b", "é"]
    assert String.split("abé", "", trim: true, parts: :infinity) == ["a", "b", "é"]
    assert String.split("abé", "", trim: true, parts: 2) == ["a", "bé"]
    assert String.split("abé", "", trim: true, parts: 3) == ["a", "b", "é"]
    assert String.split("abé", "", trim: true, parts: 4) == ["a", "b", "é"]

    assert String.split("noël", "") == ["", "n", "o", "ë", "l", ""]
    assert String.split("x-", "-", parts: 2, trim: true) == ["x"]
    assert String.split("x-x-", "-", parts: 3, trim: true) == ["x", "x"]

    assert String.split("hello", []) == ["hello"]
    assert String.split("hello", [], trim: true) == ["hello"]
    assert String.split("", []) == [""]
    assert String.split("", [], trim: true) == []

    assert_raise ArgumentError, fn ->
      String.split("a,b,c", [""])
    end

    assert_raise ArgumentError, fn ->
      String.split("a,b,c", [""])
    end
  end

  test "split/2,3 with regex" do
    assert String.split("", ~r{,}) == [""]
    assert String.split("", ~r{,}, trim: true) == []
    assert String.split("a,b", ~r{,}) == ["a", "b"]
    assert String.split("a,b,c", ~r{,}) == ["a", "b", "c"]
    assert String.split("a,b,c", ~r{,}, parts: 2) == ["a", "b,c"]
    assert String.split("a,b.c ", ~r{\W}) == ["a", "b", "c", ""]
    assert String.split("a,b.c ", ~r{\W}, trim: false) == ["a", "b", "c", ""]
    assert String.split("a,b", ~r{\.}) == ["a,b"]
  end

  test "split/2,3 with compiled pattern" do
    pattern = :binary.compile_pattern("-")

    assert String.split("x-", pattern) == ["x", ""]
    assert String.split("x-", pattern, parts: 2, trim: true) == ["x"]
    assert String.split("x-x-", pattern, parts: 3, trim: true) == ["x", "x"]
  end

  test "split/2,3 with malformed" do
    assert String.split(<<225, 158, 128, 225, 158, 185, 225>>, "", parts: 1) ==
             [<<225, 158, 128, 225, 158, 185, 225>>]

    assert String.split(<<225, 158, 128, 225, 158, 185, 225>>, "", parts: 2) ==
             ["", <<225, 158, 128, 225, 158, 185, 225>>]

    assert String.split(<<225, 158, 128, 225, 158, 185, 225>>, "", parts: 3) ==
             ["", "កឹ", <<225>>]

    assert String.split(<<225, 158, 128, 225, 158, 185, 225>>, "", parts: 4) ==
             ["", "កឹ", <<225>>, ""]
  end

  test "splitter/2,3" do
    assert String.splitter("a,b,c", ",") |> Enum.to_list() == ["a", "b", "c"]
    assert String.splitter("a,b", ".") |> Enum.to_list() == ["a,b"]
    assert String.splitter("1,2 3,4", [" ", ","]) |> Enum.to_list() == ["1", "2", "3", "4"]
    assert String.splitter("", ",") |> Enum.to_list() == [""]

    assert String.splitter("", ",", trim: true) |> Enum.to_list() == []
    assert String.splitter(" a b c ", " ", trim: true) |> Enum.to_list() == ["a", "b", "c"]
    assert String.splitter(" a b c ", " ", trim: true) |> Enum.take(1) == ["a"]
    assert String.splitter(" a b c ", " ", trim: true) |> Enum.take(2) == ["a", "b"]

    assert String.splitter("hello", []) |> Enum.to_list() == ["hello"]
    assert String.splitter("hello", [], trim: true) |> Enum.to_list() == ["hello"]
    assert String.splitter("", []) |> Enum.to_list() == [""]
    assert String.splitter("", [], trim: true) |> Enum.to_list() == []

    assert String.splitter("1,2 3,4 5", "") |> Enum.take(4) == ["", "1", ",", "2"]

    assert_raise ArgumentError, fn ->
      String.splitter("a", [""])
    end
  end

  test "split_at/2" do
    assert String.split_at("", 0) == {"", ""}
    assert String.split_at("", -1) == {"", ""}
    assert String.split_at("", 1) == {"", ""}

    assert String.split_at("abc", 0) == {"", "abc"}
    assert String.split_at("abc", 2) == {"ab", "c"}
    assert String.split_at("abc", 3) == {"abc", ""}
    assert String.split_at("abc", 4) == {"abc", ""}
    assert String.split_at("abc", 1000) == {"abc", ""}

    assert String.split_at("abc", -1) == {"ab", "c"}
    assert String.split_at("abc", -3) == {"", "abc"}
    assert String.split_at("abc", -4) == {"", "abc"}
    assert String.split_at("abc", -1000) == {"", "abc"}
  end

  test "split_at/2 with malformed" do
    assert String.split_at(<<?a, 195, 10, ?a>>, 2) == {<<?a, 195>>, <<10, ?a>>}
    assert String.split_at(<<107, 205, 135, 184>>, 1) == {<<107, 205, 135>>, <<184>>}

    assert String.split_at(<<225, 158, 128, 225, 158, 185, 225>>, 0) ==
             {"", <<225, 158, 128, 225, 158, 185, 225>>}

    assert String.split_at(<<225, 158, 128, 225, 158, 185, 225>>, 1) ==
             {"កឹ", <<225>>}

    assert String.split_at(<<225, 158, 128, 225, 158, 185, 225>>, 2) ==
             {<<225, 158, 128, 225, 158, 185, 225>>, ""}
  end

  test "upcase/1" do
    assert String.upcase("123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz") ==
             "123 ABCD 456 EFG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ"

    assert String.upcase("") == ""
    assert String.upcase("abcD") == "ABCD"
  end

  test "upcase/1 with UTF-8" do
    assert String.upcase("& % # àáâ ãäå 1 2 ç æ") == "& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ"
    assert String.upcase("àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ") == "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"
  end

  test "upcase/1 with UTF-8 multibyte" do
    assert String.upcase("straße") == "STRASSE"
    assert String.upcase("áüÈß") == "ÁÜÈSS"
  end

  test "upcase/1 with ascii" do
    assert String.upcase("olá", :ascii) == "OLá"
  end

  test "upcase/1 with turkic" do
    assert String.upcase("ıi", :turkic) == "Iİ"
    assert String.upcase("Iİ", :turkic) == "Iİ"
  end

  test "downcase/1" do
    assert String.downcase("123 ABcD 456 EfG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ") ==
             "123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz"

    assert String.downcase("abcD") == "abcd"
    assert String.downcase("") == ""
  end

  test "downcase/1 with UTF-8" do
    assert String.downcase("& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ") == "& % # àáâ ãäå 1 2 ç æ"
    assert String.downcase("ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ") == "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ"
    assert String.downcase("áüÈß") == "áüèß"
  end

  test "downcase/1 with greek final sigma" do
    assert String.downcase("Σ") == "σ"
    assert String.downcase("ΣΣ") == "σσ"
    assert String.downcase("Σ ΣΣ") == "σ σσ"
    assert String.downcase("ΜΕΣ'ΑΠΟ") == "μεσ'απο"
    assert String.downcase("ΑΣ'ΤΟΥΣ") == "ασ'τουσ"

    assert String.downcase("Σ", :greek) == "σ"
    assert String.downcase("Σ ΣΣ", :greek) == "σ σς"
    assert String.downcase("Σ ΣΑΣ Σ", :greek) == "σ σας σ"
    assert String.downcase("ΜΕΣ'ΑΠΟ", :greek) == "μεσ'απο"
    assert String.downcase("ΑΣ'ΤΟΥΣ", :greek) == "ασ'τους"
  end

  test "downcase/1 with ascii" do
    assert String.downcase("OLÁ", :ascii) == "olÁ"
  end

  test "downcase/1 with turkic" do
    assert String.downcase("Iİ", :turkic) == "ıi"
    assert String.downcase("İ", :turkic) == "i"

    assert String.downcase("ıi", :turkic) == "ıi"
    assert String.downcase("i", :turkic) == "i"

    assert String.downcase("İ") == "i̇"
  end

  test "capitalize/1" do
    assert String.capitalize("") == ""
    assert String.capitalize("1") == "1"
    assert String.capitalize("abc") == "Abc"
    assert String.capitalize("ABC") == "Abc"
    assert String.capitalize("c b a") == "C b a"
    assert String.capitalize("1ABC") == "1abc"
    assert String.capitalize("_aBc1") == "_abc1"
    assert String.capitalize(" aBc1") == " abc1"
    assert String.capitalize("àáâ") == "Àáâ"
    assert String.capitalize("ÀÁÂ") == "Àáâ"
    assert String.capitalize("âáà") == "Âáà"
    assert String.capitalize("ÂÁÀ") == "Âáà"
    assert String.capitalize("òóôõö") == "Òóôõö"
    assert String.capitalize("ÒÓÔÕÖ") == "Òóôõö"
    assert String.capitalize("ﬁn") == "Fin"

    assert String.capitalize("ABC", :ascii) == "Abc"
    assert String.capitalize("àáâ", :ascii) == "àáâ"
    assert String.capitalize("aáA", :ascii) == "Aáa"

    assert String.capitalize("iii", :turkic) == "İii"
    assert String.capitalize("ııı", :turkic) == "Iıı"
    assert String.capitalize("İii", :turkic) == "İii"
    assert String.capitalize("Iıı", :turkic) == "Iıı"

    assert String.capitalize(<<138, ?B, ?C>>) == <<138, ?b, ?c>>

    assert String.capitalize(<<225, 158, 128, 225, 158, 185, 225>>) ==
             <<225, 158, 128, 225, 158, 185, 225>>
  end

  test "replace_leading/3" do
    assert String.replace_leading("aa abc   ", "a", "b") == "bb abc   "
    assert String.replace_leading("__ abc   ", "_", "b") == "bb abc   "
    assert String.replace_leading("aaaaaaaa ", "a", "b") == "bbbbbbbb "
    assert String.replace_leading("aaaaaaaa ", "aaa", "b") == "bbaa "
    assert String.replace_leading("aaaaaaaaa", "a", "b") == "bbbbbbbbb"
    assert String.replace_leading("]]]]]]", "]", "[]") == "[][][][][][]"
    assert String.replace_leading("]]]]]]]]", "]", "") == ""
    assert String.replace_leading("]]]]]] ]", "]", "") == " ]"
    assert String.replace_leading("猫猫 cat  ", "猫", "й") == "йй cat  "
    assert String.replace_leading("test", "t", "T") == "Test"
    assert String.replace_leading("t", "t", "T") == "T"
    assert String.replace_leading("aaa", "b", "c") == "aaa"

    message = ~r/cannot use an empty string/

    assert_raise ArgumentError, message, fn ->
      String.replace_leading("foo", "", "bar")
    end

    assert_raise ArgumentError, message, fn ->
      String.replace_leading("", "", "bar")
    end
  end

  test "replace_trailing/3" do
    assert String.replace_trailing("   abc aa", "a", "b") == "   abc bb"
    assert String.replace_trailing("   abc __", "_", "b") == "   abc bb"
    assert String.replace_trailing(" aaaaaaaa", "a", "b") == " bbbbbbbb"
    assert String.replace_trailing(" aaaaaaaa", "aaa", "b") == " aabb"
    assert String.replace_trailing("aaaaaaaaa", "a", "b") == "bbbbbbbbb"
    assert String.replace_trailing("]]]]]]", "]", "[]") == "[][][][][][]"
    assert String.replace_trailing("]]]]]]]]", "]", "") == ""
    assert String.replace_trailing("] ]]]]]]", "]", "") == "] "
    assert String.replace_trailing("  cat 猫猫", "猫", "й") == "  cat йй"
    assert String.replace_trailing("test", "t", "T") == "tesT"
    assert String.replace_trailing("t", "t", "T") == "T"
    assert String.replace_trailing("aaa", "b", "c") == "aaa"

    message = ~r/cannot use an empty string/

    assert_raise ArgumentError, message, fn ->
      String.replace_trailing("foo", "", "bar")
    end

    assert_raise ArgumentError, message, fn ->
      String.replace_trailing("", "", "bar")
    end
  end

  test "trim/1,2" do
    assert String.trim("") == ""
    assert String.trim("  abc ") == "abc"
    assert String.trim("a  abc  a\n\n") == "a  abc  a"
    assert String.trim("a  abc  a\t\n\v\f\r\s") == "a  abc  a"

    assert String.trim("___  abc  ___", "_") == "  abc  "
    assert String.trim("猫猫猫cat猫猫猫", "猫猫") == "猫cat猫"
    # no-break space
    assert String.trim("\u00A0a  abc  a\u00A0") == "a  abc  a"
    # whitespace defined as a range
    assert String.trim("\u2008a  abc  a\u2005") == "a  abc  a"
  end

  test "trim_leading/1,2" do
    assert String.trim_leading("") == ""
    assert String.trim_leading("   abc  ") == "abc  "
    assert String.trim_leading("a  abc  a") == "a  abc  a"
    assert String.trim_leading("\n\na  abc  a") == "a  abc  a"
    assert String.trim_leading("\t\n\v\f\r\sa  abc  a") == "a  abc  a"
    assert String.trim_leading(<<194, 133, "a  abc  a">>) == "a  abc  a"
    # information separators are not whitespace
    assert String.trim_leading("\u001F a  abc  a") == "\u001F a  abc  a"
    # no-break space
    assert String.trim_leading("\u00A0 a  abc  a") == "a  abc  a"

    assert String.trim_leading("aa aaa", "aaa") == "aa aaa"
    assert String.trim_leading("aaa aaa", "aa") == "a aaa"
    assert String.trim_leading("aa abc   ", "a") == " abc   "
    assert String.trim_leading("__ abc   ", "_") == " abc   "
    assert String.trim_leading("aaaaaaaaa ", "a") == " "
    assert String.trim_leading("aaaaaaaaaa", "a") == ""
    assert String.trim_leading("]]]]]] ]", "]") == " ]"
    assert String.trim_leading("猫猫 cat   ", "猫") == " cat   "
    assert String.trim_leading("test", "t") == "est"
    assert String.trim_leading("t", "t") == ""
    assert String.trim_leading("", "t") == ""
  end

  test "trim_trailing/1,2" do
    assert String.trim_trailing("") == ""
    assert String.trim_trailing("1\n") == "1"
    assert String.trim_trailing("\r\n") == ""
    assert String.trim_trailing("   abc  ") == "   abc"
    assert String.trim_trailing("   abc a") == "   abc a"
    assert String.trim_trailing("a  abc  a\n\n") == "a  abc  a"
    assert String.trim_trailing("a  abc  a\t\n\v\f\r\s") == "a  abc  a"
    assert String.trim_trailing(<<"a  abc  a", 194, 133>>) == "a  abc  a"
    # information separators are not whitespace
    assert String.trim_trailing("a  abc  a \u001F") == "a  abc  a \u001F"
    # no-break space
    assert String.trim_trailing("a  abc  a \u00A0") == "a  abc  a"

    assert String.trim_trailing("aaa aa", "aaa") == "aaa aa"
    assert String.trim_trailing("aaa aaa", "aa") == "aaa a"
    assert String.trim_trailing("   abc aa", "a") == "   abc "
    assert String.trim_trailing("   abc __", "_") == "   abc "
    assert String.trim_trailing(" aaaaaaaaa", "a") == " "
    assert String.trim_trailing("aaaaaaaaaa", "a") == ""
    assert String.trim_trailing("] ]]]]]]", "]") == "] "
    assert String.trim_trailing("   cat 猫猫", "猫") == "   cat "
    assert String.trim_trailing("test", "t") == "tes"
    assert String.trim_trailing("t", "t") == ""
    assert String.trim_trailing("", "t") == ""
  end

  test "pad_leading/2,3" do
    assert String.pad_leading("", 5) == "     "
    assert String.pad_leading("abc", 5) == "  abc"
    assert String.pad_leading("  abc  ", 9) == "    abc  "
    assert String.pad_leading("猫", 5) == "    猫"
    assert String.pad_leading("-", 0) == "-"
    assert String.pad_leading("-", 1) == "-"

    assert String.pad_leading("---", 5, "abc") == "ab---"
    assert String.pad_leading("---", 9, "abc") == "abcabc---"

    assert String.pad_leading("---", 5, ["abc"]) == "abcabc---"
    assert String.pad_leading("--", 6, ["a", "bc"]) == "abcabc--"

    message = "expected a string padding element, got: 10"

    assert_raise ArgumentError, message, fn ->
      String.pad_leading("-", 3, ["-", 10])
    end
  end

  test "pad_trailing/2,3" do
    assert String.pad_trailing("", 5) == "     "
    assert String.pad_trailing("abc", 5) == "abc  "
    assert String.pad_trailing("  abc  ", 9) == "  abc    "
    assert String.pad_trailing("猫", 5) == "猫    "
    assert String.pad_trailing("-", 0) == "-"
    assert String.pad_trailing("-", 1) == "-"

    assert String.pad_trailing("---", 5, "abc") == "---ab"
    assert String.pad_trailing("---", 9, "abc") == "---abcabc"

    assert String.pad_trailing("---", 5, ["abc"]) == "---abcabc"
    assert String.pad_trailing("--", 6, ["a", "bc"]) == "--abcabc"

    message = "expected a string padding element, got: 10"

    assert_raise ArgumentError, message, fn ->
      String.pad_trailing("-", 3, ["-", 10])
    end
  end

  test "reverse/1" do
    assert String.reverse("") == ""
    assert String.reverse("abc") == "cba"
    assert String.reverse("Hello World") == "dlroW olleH"
    assert String.reverse("Hello ∂og") == "go∂ olleH"
    assert String.reverse("Ā̀stute") == "etutsĀ̀"
    assert String.reverse(String.reverse("Hello World")) == "Hello World"
    assert String.reverse(String.reverse("Hello \r\n World")) == "Hello \r\n World"
  end

  describe "replace/3" do
    test "with empty string and string replacement" do
      assert String.replace("elixir", "", "") == "elixir"
      assert String.replace("ELIXIR", "", ".") == ".E.L.I.X.I.R."
      assert String.replace("ELIXIR", "", ".", global: true) == ".E.L.I.X.I.R."
      assert String.replace("ELIXIR", "", ".", global: false) == ".ELIXIR"

      assert_raise ArgumentError, fn ->
        String.replace("elixir", [""], "")
      end
    end

    test "with empty string and string replacement with malformed" do
      assert String.replace(<<225, 158, 128, 225, 158, 185, 225>>, "", ".") == ".កឹ.\xE1."
    end

    test "with empty pattern list" do
      assert String.replace("elixir", [], "anything") == "elixir"
    end

    test "with match pattern and string replacement" do
      assert String.replace("a,b,c", ",", "-") == "a-b-c"
      assert String.replace("a,b,c", [",", "b"], "-") == "a---c"

      assert String.replace("a,b,c", ",", "-", global: false) == "a-b,c"
      assert String.replace("a,b,c", [",", "b"], "-", global: false) == "a-b,c"
      assert String.replace("ãéã", "é", "e", global: false) == "ãeã"
    end

    test "with regex and string replacement" do
      assert String.replace("a,b,c", ~r/,(.)/, ",\\1\\1") == "a,bb,cc"
      assert String.replace("a,b,c", ~r/,(.)/, ",\\1\\1", global: false) == "a,bb,c"
    end

    test "with empty string and function replacement" do
      assert String.replace("elixir", "", fn "" -> "" end) == "elixir"
      assert String.replace("ELIXIR", "", fn "" -> "." end) == ".E.L.I.X.I.R."
      assert String.replace("ELIXIR", "", fn "" -> "." end, global: true) == ".E.L.I.X.I.R."
      assert String.replace("ELIXIR", "", fn "" -> "." end, global: false) == ".ELIXIR"

      assert String.replace("elixir", "", fn "" -> [""] end) == "elixir"
      assert String.replace("ELIXIR", "", fn "" -> ["."] end) == ".E.L.I.X.I.R."
      assert String.replace("ELIXIR", "", fn "" -> ["."] end, global: true) == ".E.L.I.X.I.R."
      assert String.replace("ELIXIR", "", fn "" -> ["."] end, global: false) == ".ELIXIR"
    end

    test "with match pattern and function replacement" do
      assert String.replace("a,b,c", ",", fn "," -> "-" end) == "a-b-c"
      assert String.replace("a,b,c", [",", "b"], fn x -> "[#{x}]" end) == "a[,][b][,]c"
      assert String.replace("a,b,c", [",", "b"], fn x -> [?[, x, ?]] end) == "a[,][b][,]c"

      assert String.replace("a,b,c", ",", fn "," -> "-" end, global: false) == "a-b,c"
      assert String.replace("a,b,c", [",", "b"], fn x -> "[#{x}]" end, global: false) == "a[,]b,c"
      assert String.replace("ãéã", "é", fn "é" -> "e" end, global: false) == "ãeã"
    end

    test "with regex and function replacement" do
      assert String.replace("a,b,c", ~r/,(.)/, fn x -> "#{x}#{x}" end) == "a,b,b,c,c"
      assert String.replace("a,b,c", ~r/,(.)/, fn x -> [x, x] end) == "a,b,b,c,c"
      assert String.replace("a,b,c", ~r/,(.)/, fn x -> "#{x}#{x}" end, global: false) == "a,b,b,c"
      assert String.replace("a,b,c", ~r/,(.)/, fn x -> [x, x] end, global: false) == "a,b,b,c"
    end
  end

  test "duplicate/2" do
    assert String.duplicate("abc", 0) == ""
    assert String.duplicate("abc", 1) == "abc"
    assert String.duplicate("abc", 2) == "abcabc"
    assert String.duplicate("&ã$", 2) == "&ã$&ã$"

    assert_raise ArgumentError, fn ->
      String.duplicate("abc", -1)
    end
  end

  test "codepoints/1" do
    assert String.codepoints("elixir") == ["e", "l", "i", "x", "i", "r"]
    # slovak
    assert String.codepoints("elixír") == ["e", "l", "i", "x", "í", "r"]
    # armenian
    assert String.codepoints("ոգելից ըմպելիք") ==
             ["ո", "գ", "ե", "լ", "ի", "ց", " ", "ը", "մ", "պ", "ե", "լ", "ի", "ք"]

    # belarussian
    assert String.codepoints("эліксір") == ["э", "л", "і", "к", "с", "і", "р"]
    # greek
    assert String.codepoints("ελιξήριο") == ["ε", "λ", "ι", "ξ", "ή", "ρ", "ι", "ο"]
    # hebraic
    assert String.codepoints("סם חיים") == ["ס", "ם", " ", "ח", "י", "י", "ם"]
    # hindi
    assert String.codepoints("अमृत") == ["अ", "म", "ृ", "त"]
    # bengali
    assert String.codepoints("স্পর্শমণি") == ["স", "্", "প", "র", "্", "শ", "ম", "ণ", "ি"]
    # gujarati
    assert String.codepoints("સર્વશ્રેષ્ઠ ઇલાજ") ==
             ["સ", "ર", "્", "વ", "શ", "્", "ર", "ે", "ષ", "્", "ઠ", " ", "ઇ", "લ", "ા", "જ"]

    # japanese
    assert String.codepoints("世界中の一番") == ["世", "界", "中", "の", "一", "番"]
    assert String.codepoints("がガちゃ") == ["が", "ガ", "ち", "ゃ"]
    assert String.codepoints("") == []

    assert String.codepoints("ϖͲϥЫݎߟΈټϘለДШव׆ש؇؊صلټܗݎޥޘ߉ऌ૫ሏᶆ℆ℙℱ ⅚Ⅷ↠∈⌘①ﬃ") ==
             ["ϖ", "Ͳ", "ϥ", "Ы", "ݎ", "ߟ", "Έ"] ++
               ["ټ", "Ϙ", "ለ", "Д", "Ш", "व"] ++
               ["׆", "ש", "؇", "؊", "ص", "ل", "ټ"] ++
               ["ܗ", "ݎ", "ޥ", "ޘ", "߉", "ऌ", "૫"] ++
               ["ሏ", "ᶆ", "℆", "ℙ", "ℱ", " ", "⅚"] ++ ["Ⅷ", "↠", "∈", "⌘", "①", "ﬃ"]
  end

  test "equivalent?/2" do
    assert String.equivalent?("", "")
    assert String.equivalent?("elixir", "elixir")
    assert String.equivalent?("뢴", "뢴")
    assert String.equivalent?("ṩ", "ṩ")
    refute String.equivalent?("ELIXIR", "elixir")
    refute String.equivalent?("døge", "dóge")
  end

  test "graphemes/1" do
    # Extended
    assert String.graphemes("Ā̀stute") == ["Ā̀", "s", "t", "u", "t", "e"]
    # CLRF
    assert String.graphemes("\r\n\f") == ["\r\n", "\f"]
    # Regional indicator
    assert String.graphemes("\u{1F1E6}\u{1F1E7}") == ["\u{1F1E6}\u{1F1E7}"]
    assert String.graphemes("\u{1F1E6}\u{1F1E7}\u{1F1E8}") == ["\u{1F1E6}\u{1F1E7}", "\u{1F1E8}"]
    # Hangul
    assert String.graphemes("\u1100\u115D\uB4A4") == ["ᄀᅝ뒤"]
    # Special Marking with Extended
    assert String.graphemes("a\u0300\u0903") == ["a\u0300\u0903"]
  end

  test "next_grapheme/1" do
    assert String.next_grapheme("Ā̀stute") == {"Ā̀", "stute"}
    assert String.next_grapheme(<<225, 158, 128, 225, 158, 185, 225>>) == {"កឹ", <<225>>}
    assert String.next_grapheme("") == nil
  end

  describe "randomized" do
    test "next_grapheme" do
      for _ <- 1..10 do
        bin = :crypto.strong_rand_bytes(20)

        try do
          bin |> Stream.unfold(&String.next_grapheme/1) |> Enum.to_list()
        rescue
          # Ignore malformed pictographic sequences
          _ -> :ok
        else
          list ->
            assert Enum.all?(list, &is_binary/1), "cannot build graphemes for #{inspect(bin)}"
        end
      end
    end

    test "split empty" do
      for _ <- 1..10 do
        bin = :crypto.strong_rand_bytes(20)

        try do
          String.split(bin, "")
        rescue
          # Ignore malformed pictographic sequences
          _ -> :ok
        else
          split ->
            assert Enum.all?(split, &is_binary/1), "cannot split #{inspect(bin)}"
            assert IO.iodata_to_binary(split) == bin
        end
      end
    end

    test "graphemes" do
      for _ <- 1..10 do
        bin = :crypto.strong_rand_bytes(20)

        try do
          String.graphemes(bin)
        rescue
          # Ignore malformed pictographic sequences
          _ -> :ok
        else
          graphemes ->
            assert Enum.all?(graphemes, &is_binary/1),
                   "cannot build graphemes for #{inspect(bin)}"

            assert IO.iodata_to_binary(graphemes) == bin
        end
      end
    end
  end

  test "first/1" do
    assert String.first("elixir") == "e"
    assert String.first("íelixr") == "í"
    assert String.first("եոգլից ըմպելիք") == "ե"
    assert String.first("лэіксір") == "л"
    assert String.first("ελιξήριο") == "ε"
    assert String.first("סם חיים") == "ס"
    assert String.first("がガちゃ") == "が"
    assert String.first("Ā̀stute") == "Ā̀"
    assert String.first("") == nil
  end

  test "last/1" do
    assert String.last("elixir") == "r"
    assert String.last("elixrí") == "í"
    assert String.last("եոգլից ըմպելիքե") == "ե"
    assert String.last("ліксірэ") == "э"
    assert String.last("ειξήριολ") == "λ"
    assert String.last("סם ייםח") == "ח"
    assert String.last("がガちゃ") == "ゃ"
    assert String.last("Ā̀") == "Ā̀"
    assert String.last("") == nil
  end

  test "length/1" do
    assert String.length("elixir") == 6
    assert String.length("elixrí") == 6
    assert String.length("եոգլից") == 6
    assert String.length("ліксрэ") == 6
    assert String.length("ειξήριολ") == 8
    assert String.length("סם ייםח") == 7
    assert String.length("がガちゃ") == 4
    assert String.length("Ā̀stute") == 6
    assert String.length("👨‍👩‍👧‍👦") == 1
    assert String.length("") == 0
  end

  test "at/2" do
    assert String.at("л", 0) == "л"
    assert String.at("elixir", 1) == "l"
    assert String.at("がガちゃ", 2) == "ち"
    assert String.at("л", 10) == nil
    assert String.at("elixir", -1) == "r"
    assert String.at("がガちゃ", -2) == "ち"
    assert String.at("л", -3) == nil
    assert String.at("Ā̀stute", 1) == "s"
    assert String.at("elixir", 6) == nil
  end

  test "slice/3" do
    assert String.slice("elixir", 1, 3) == "lix"
    assert String.slice("あいうえお", 2, 2) == "うえ"
    assert String.slice("ειξήριολ", 2, 3) == "ξήρ"
    assert String.slice("elixir", 3, 4) == "xir"
    assert String.slice("あいうえお", 3, 5) == "えお"
    assert String.slice("ειξήριολ", 5, 4) == "ιολ"
    assert String.slice("elixir", -3, 2) == "xi"
    assert String.slice("あいうえお", -4, 3) == "いうえ"
    assert String.slice("ειξήριολ", -5, 3) == "ήρι"
    assert String.slice("elixir", -10, 1) == "e"
    assert String.slice("あいうえお", -10, 2) == "あい"
    assert String.slice("ειξήριολ", -10, 3) == "ειξ"
    assert String.slice("elixir", 8, 2) == ""
    assert String.slice("あいうえお", 6, 2) == ""
    assert String.slice("ειξήριολ", 8, 1) == ""
    assert String.slice("ειξήριολ", 9, 1) == ""
    assert String.slice("elixir", 0, 0) == ""
    assert String.slice("elixir", 5, 0) == ""
    assert String.slice("elixir", -5, 0) == ""
    assert String.slice("elixir", -10, 10) == "elixir"
    assert String.slice("", 0, 1) == ""
    assert String.slice("", 1, 1) == ""
  end

  test "slice/2" do
    assert String.slice("elixir", 0..-2//1) == "elixi"
    assert String.slice("elixir", 1..3) == "lix"
    assert String.slice("elixir", -5..-3) == "lix"
    assert String.slice("elixir", -5..3) == "lix"
    assert String.slice("elixir", -10..10) == "elixir"
    assert String.slice("あいうえお", 2..3) == "うえ"
    assert String.slice("ειξήριολ", 2..4) == "ξήρ"
    assert String.slice("elixir", 3..6) == "xir"
    assert String.slice("あいうえお", 3..7) == "えお"
    assert String.slice("ειξήριολ", 5..8) == "ιολ"
    assert String.slice("elixir", -3..-2) == "xi"
    assert String.slice("あいうえお", -4..-2) == "いうえ"
    assert String.slice("ειξήριολ", -5..-3) == "ήρι"
    assert String.slice("elixir", 8..9) == ""
    assert String.slice("あいうえお", 6..7) == ""
    assert String.slice("ειξήριολ", 8..8) == ""
    assert String.slice("ειξήριολ", 9..9) == ""
    assert String.slice("", 0..0) == ""
    assert String.slice("", 1..1) == ""
    assert String.slice("あいうえお", -2..-4//1) == ""
    assert String.slice("あいうえお", -10..-15//1) == ""
    assert String.slice("hello あいうえお Unicode", 8..-1//1) == "うえお Unicode"
    assert String.slice("abc", -1..14) == "c"
    assert String.slice("a·̀ͯ‿.⁀:", 0..-2//1) == "a·̀ͯ‿.⁀"

    assert ExUnit.CaptureIO.capture_io(:stderr, fn ->
             assert String.slice("elixir", 0..-2//-1) == "elixi"
           end) =~ "negative steps are not supported in String.slice/2, pass 0..-2//1 instead"
  end

  test "slice/2 with steps" do
    assert String.slice("elixir", 0..-2//2) == "eii"
    assert String.slice("elixir", 1..3//2) == "lx"
    assert String.slice("elixir", -5..-3//2) == "lx"
    assert String.slice("elixir", -5..3//2) == "lx"
    assert String.slice("あいうえお", 2..3//2) == "う"
    assert String.slice("ειξήριολ", 2..4//2) == "ξρ"
    assert String.slice("elixir", 3..6//2) == "xr"
    assert String.slice("あいうえお", 3..7//2) == "え"
    assert String.slice("ειξήριολ", 5..8//2) == "ιλ"
    assert String.slice("elixir", -3..-2//2) == "x"
    assert String.slice("あいうえお", -4..-2//2) == "いえ"
    assert String.slice("ειξήριολ", -5..-3//2) == "ήι"
    assert String.slice("elixir", 8..9//2) == ""
    assert String.slice("", 0..0//2) == ""
    assert String.slice("", 1..1//2) == ""
    assert String.slice("あいうえお", -2..-4//2) == ""
    assert String.slice("あいうえお", -10..-15//2) == ""
    assert String.slice("hello あいうえお Unicode", 8..-1//2) == "うおUioe"
    assert String.slice("abc", -1..14//2) == "c"
    assert String.slice("a·̀ͯ‿.⁀:", 0..-2//2) == "a‿⁀"
  end

  test "byte_slice/2" do
    # ASCII
    assert String.byte_slice("elixir", 0, 6) == "elixir"
    assert String.byte_slice("elixir", 0, 5) == "elixi"
    assert String.byte_slice("elixir", 1, 4) == "lixi"
    assert String.byte_slice("elixir", 0, 10) == "elixir"
    assert String.byte_slice("elixir", -3, 10) == "xir"
    assert String.byte_slice("elixir", -10, 10) == "elixir"
    assert String.byte_slice("elixir", 1, 0) == ""
    assert String.byte_slice("elixir", 10, 10) == ""

    # 2 byte
    assert String.byte_slice("héllò", 1, 4) == "éll"
    assert String.byte_slice("héllò", 1, 5) == "éll"
    assert String.byte_slice("héllò", 1, 6) == "éllò"
    assert String.byte_slice("héllò", 2, 4) == "llò"

    # 3 byte
    assert String.byte_slice("hかllか", 1, 4) == "かl"
    assert String.byte_slice("hかllか", 1, 5) == "かll"
    assert String.byte_slice("hかllか", 1, 6) == "かll"
    assert String.byte_slice("hかllか", 1, 7) == "かll"
    assert String.byte_slice("hかllか", 1, 8) == "かllか"
    assert String.byte_slice("hかllか", 2, 4) == "ll"
    assert String.byte_slice("hかllか", 2, 5) == "llか"

    # 4 byte
    assert String.byte_slice("h😍ll😍", 1, 4) == "😍"
    assert String.byte_slice("h😍ll😍", 1, 5) == "😍l"
    assert String.byte_slice("h😍ll😍", 1, 6) == "😍ll"
    assert String.byte_slice("h😍ll😍", 1, 7) == "😍ll"
    assert String.byte_slice("h😍ll😍", 1, 8) == "😍ll"
    assert String.byte_slice("h😍ll😍", 1, 9) == "😍ll"
    assert String.byte_slice("h😍ll😍", 1, 10) == "😍ll😍"
    assert String.byte_slice("h😍ll😍", 2, 5) == "ll"
    assert String.byte_slice("h😍ll😍", 2, 6) == "ll😍"

    # Already truncated
    assert String.byte_slice(<<178, "ll", 178>>, 0, 10) == "ll"

    # Already invalid
    assert String.byte_slice(<<255, "ll", 255>>, 0, 10) == <<255, "ll", 255>>
  end

  test "valid?/1" do
    assert String.valid?("afds")
    assert String.valid?("øsdfh")
    assert String.valid?("dskfjあska")
    assert String.valid?(<<0xEF, 0xB7, 0x90>>)

    refute String.valid?(<<0xFFFF::16>>)
    refute String.valid?("asd" <> <<0xFFFF::16>>)

    assert String.valid?("afdsafdsafds", :fast_ascii)
    assert String.valid?("øsdfhøsdfh", :fast_ascii)
    assert String.valid?("dskfjあskadskfjあska", :fast_ascii)
    assert String.valid?(<<0xEF, 0xB7, 0x90, 0xEF, 0xB7, 0x90, 0xEF, 0xB7, 0x90>>, :fast_ascii)

    refute String.valid?(<<0xFFFF::16>>, :fast_ascii)
    refute String.valid?("asdasdasd" <> <<0xFFFF::16>>, :fast_ascii)
  end

  test "replace_invalid" do
    assert String.replace_invalid("") === ""
    assert String.replace_invalid(<<0xFF>>) === "�"
    assert String.replace_invalid(<<0xFF, 0xFF, 0xFF>>) === "���"

    # Valid ASCII
    assert String.replace_invalid("hello") === "hello"

    # Valid UTF-8
    assert String.replace_invalid("こんにちは") === "こんにちは"

    # 2/3 byte truncated "ề"
    assert String.replace_invalid(<<225, 187>>) === "�"
    assert String.replace_invalid("nem rán b" <> <<225, 187>> <> " bề") === "nem rán b� bề"

    # 2/4 byte truncated "😔"
    assert String.replace_invalid(<<240, 159>>) === "�"
    assert String.replace_invalid("It's so over " <> <<240, 159>>) === "It's so over �"

    # 3/4 byte truncated "😃"
    assert String.replace_invalid(<<240, 159, 152>>) === "�"
    assert String.replace_invalid("We're so back " <> <<240, 159, 152>>) === "We're so back �"

    # 3 byte overlong "e"
    assert String.replace_invalid(<<0b11100000, 0b10000001, 0b10100101>>) === "���"
  end

  test "chunk/2 with :valid trait" do
    assert String.chunk("", :valid) == []

    assert String.chunk("ødskfjあ\x11ska", :valid) == ["ødskfjあ\x11ska"]
  end

  test "chunk/2 with :printable trait" do
    assert String.chunk("", :printable) == []

    assert String.chunk("ødskfjあska", :printable) == ["ødskfjあska"]
    assert String.chunk("abc\u{0FFFF}def", :printable) == ["abc", <<0x0FFFF::utf8>>, "def"]

    assert String.chunk("\x06ab\x05cdef\x03\0", :printable) ==
             [<<6>>, "ab", <<5>>, "cdef", <<3, 0>>]
  end

  test "starts_with?/2" do
    assert String.starts_with?("hello", "he")
    assert String.starts_with?("hello", "hello")
    refute String.starts_with?("hello", [])
    assert String.starts_with?("hello", "")
    assert String.starts_with?("hello", [""])
    assert String.starts_with?("hello", ["hellö", "hell"])
    assert String.starts_with?("エリクシア", "エリ")
    refute String.starts_with?("hello", "lo")
    refute String.starts_with?("hello", "hellö")
    refute String.starts_with?("hello", ["hellö", "goodbye"])
    refute String.starts_with?("エリクシア", "仙丹")
  end

  test "ends_with?/2" do
    assert String.ends_with?("hello", "lo")
    assert String.ends_with?("hello", "hello")
    refute String.ends_with?("hello", [])
    assert String.ends_with?("hello", ["hell", "lo", "xx"])
    assert String.ends_with?("hello", ["hellö", "lo"])
    assert String.ends_with?("エリクシア", "シア")
    refute String.ends_with?("hello", "he")
    refute String.ends_with?("hello", "hellö")
    refute String.ends_with?("hello", ["hel", "goodbye"])
    refute String.ends_with?("エリクシア", "仙丹")
  end

  test "contains?/2" do
    assert String.contains?("elixir of life", "of")
    assert String.contains?("エリクシア", "シ")
    refute String.contains?("elixir of life", [])
    assert String.contains?("elixir of life", "")
    assert String.contains?("elixir of life", [""])
    assert String.contains?("elixir of life", ["mercury", "life"])
    refute String.contains?("elixir of life", "death")
    refute String.contains?("エリクシア", "仙")
    refute String.contains?("elixir of life", ["death", "mercury", "eternal life"])
  end

  test "to_charlist/1" do
    assert String.to_charlist("æß") == [?æ, ?ß]
    assert String.to_charlist("abc") == [?a, ?b, ?c]

    assert_raise UnicodeConversionError, "invalid encoding starting at <<223, 255>>", fn ->
      String.to_charlist(<<0xDF, 0xFF>>)
    end

    assert_raise UnicodeConversionError, "incomplete encoding starting at <<195>>", fn ->
      String.to_charlist(<<106, 111, 115, 195>>)
    end
  end

  test "to_float/1" do
    assert String.to_float("3.0") == 3.0

    three = fn -> "3" end
    assert_raise ArgumentError, fn -> String.to_float(three.()) end

    dot_three = fn -> ".3" end
    assert_raise ArgumentError, fn -> String.to_float(dot_three.()) end
  end

  test "jaro_distance/2" do
    assert String.jaro_distance("same", "same") == 1.0
    assert String.jaro_distance("any", "") == 0.0
    assert String.jaro_distance("", "any") == 0.0
    assert String.jaro_distance("martha", "marhta") == 0.9444444444444445
    assert String.jaro_distance("martha", "marhha") == 0.888888888888889
    assert String.jaro_distance("marhha", "martha") == 0.888888888888889
    assert String.jaro_distance("dwayne", "duane") == 0.8222222222222223
    assert String.jaro_distance("dixon", "dicksonx") == 0.7666666666666666
    assert String.jaro_distance("xdicksonx", "dixon") == 0.7518518518518519
    assert String.jaro_distance("shackleford", "shackelford") == 0.9696969696969697
    assert String.jaro_distance("dunningham", "cunnigham") == 0.8962962962962964
    assert String.jaro_distance("nichleson", "nichulson") == 0.9259259259259259
    assert String.jaro_distance("jones", "johnson") == 0.7904761904761904
    assert String.jaro_distance("massey", "massie") == 0.888888888888889
    assert String.jaro_distance("abroms", "abrams") == 0.888888888888889
    assert String.jaro_distance("hardin", "martinez") == 0.7222222222222222
    assert String.jaro_distance("itman", "smith") == 0.4666666666666666
    assert String.jaro_distance("jeraldine", "geraldine") == 0.9259259259259259
    assert String.jaro_distance("michelle", "michael") == 0.8690476190476191
    assert String.jaro_distance("julies", "julius") == 0.888888888888889
    assert String.jaro_distance("tanya", "tonya") == 0.8666666666666667
    assert String.jaro_distance("sean", "susan") == 0.7833333333333333
    assert String.jaro_distance("jon", "john") == 0.9166666666666666
    assert String.jaro_distance("jon", "jan") == 0.7777777777777777
    assert String.jaro_distance("семена", "стремя") == 0.6666666666666666
    assert String.jaro_distance("Sunday", "Saturday") == 0.7194444444444444
  end

  test "myers_difference/2" do
    assert String.myers_difference("", "abc") == [ins: "abc"]
    assert String.myers_difference("abc", "") == [del: "abc"]
    assert String.myers_difference("", "") == []
    assert String.myers_difference("abc", "abc") == [eq: "abc"]
    assert String.myers_difference("abc", "aйbc") == [eq: "a", ins: "й", eq: "bc"]
    assert String.myers_difference("aйbc", "abc") == [eq: "a", del: "й", eq: "bc"]
  end

  test "normalize/2" do
    assert String.normalize("ŝ", :nfd) == "ŝ"
    assert String.normalize("ḇravô", :nfd) == "ḇravô"
    assert String.normalize("ṩierra", :nfd) == "ṩierra"
    assert String.normalize("뢴", :nfd) == "뢴"
    assert String.normalize("êchǭ", :nfc) == "êchǭ"
    assert String.normalize("거̄", :nfc) == "거̄"
    assert String.normalize("뢴", :nfc) == "뢴"

    ## Error cases
    assert String.normalize(<<15, 216>>, :nfc) == <<15, 216>>
    assert String.normalize(<<15, 216>>, :nfd) == <<15, 216>>
    assert String.normalize(<<216, 15>>, :nfc) == <<216, 15>>
    assert String.normalize(<<216, 15>>, :nfd) == <<216, 15>>

    assert String.normalize(<<15, 216>>, :nfkc) == <<15, 216>>
    assert String.normalize(<<15, 216>>, :nfkd) == <<15, 216>>
    assert String.normalize(<<216, 15>>, :nfkc) == <<216, 15>>
    assert String.normalize(<<216, 15>>, :nfkd) == <<216, 15>>

    ## Cases from NormalizationTest.txt

    # 05B8 05B9 05B1 0591 05C3 05B0 05AC 059F
    # 05B1 05B8 05B9 0591 05C3 05B0 05AC 059F
    # HEBREW POINT QAMATS, HEBREW POINT HOLAM, HEBREW POINT HATAF SEGOL,
    # HEBREW ACCENT ETNAHTA, HEBREW PUNCTUATION SOF PASUQ, HEBREW POINT SHEVA,
    # HEBREW ACCENT ILUY, HEBREW ACCENT QARNEY PARA
    assert String.normalize("ֱָֹ֑׃ְ֬֟", :nfc) == "ֱָֹ֑׃ְ֬֟"

    # 095D (exclusion list)
    # 0922 093C
    # DEVANAGARI LETTER RHA
    assert String.normalize("ढ़", :nfc) == "ढ़"

    # 0061 0315 0300 05AE 0340 0062
    # 00E0 05AE 0300 0315 0062
    # LATIN SMALL LETTER A, COMBINING COMMA ABOVE RIGHT, COMBINING GRAVE ACCENT,
    # HEBREW ACCENT ZINOR, COMBINING GRAVE TONE MARK, LATIN SMALL LETTER B
    assert String.normalize("à֮̀̕b", :nfc) == "à֮̀̕b"

    # 0344
    # 0308 0301
    # COMBINING GREEK DIALYTIKA TONOS
    assert String.normalize("\u0344", :nfc) == "\u0308\u0301"

    # 115B9 0334 115AF
    # 115B9 0334 115AF
    # SIDDHAM VOWEL SIGN AI, COMBINING TILDE OVERLAY, SIDDHAM VOWEL SIGN AA
    assert String.normalize("𑖹̴𑖯", :nfc) == "𑖹̴𑖯"

    # HEBREW ACCENT ETNAHTA, HEBREW PUNCTUATION SOF PASUQ, HEBREW POINT SHEVA,
    # HEBREW ACCENT ILUY, HEBREW ACCENT QARNEY PARA
    assert String.normalize("ֱָֹ֑׃ְ֬֟", :nfc) == "ֱָֹ֑׃ְ֬֟"

    # 095D (exclusion list)
    # HEBREW ACCENT ETNAHTA, HEBREW PUNCTUATION SOF PASUQ, HEBREW POINT SHEVA,
    # HEBREW ACCENT ILUY, HEBREW ACCENT QARNEY PARA
    assert String.normalize("ֱָֹ֑׃ְ֬֟", :nfc) == "ֱָֹ֑׃ְ֬֟"

    # 095D (exclusion list)
    # 0922 093C
    # DEVANAGARI LETTER RHA
    assert String.normalize("ढ़", :nfc) == "ढ़"

    # 0061 0315 0300 05AE 0340 0062
    # 00E0 05AE 0300 0315 0062
    # LATIN SMALL LETTER A, COMBINING COMMA ABOVE RIGHT, COMBINING GRAVE ACCENT,
    # HEBREW ACCENT ZINOR, COMBINING GRAVE TONE MARK, LATIN SMALL LETTER B
    assert String.normalize("à֮̀̕b", :nfc) == "à֮̀̕b"

    # 0344
    # 0308 0301
    # COMBINING GREEK DIALYTIKA TONOS
    assert String.normalize("\u0344", :nfc) == "\u0308\u0301"

    # 115B9 0334 115AF
    # 115B9 0334 115AF
    # SIDDHAM VOWEL SIGN AI, COMBINING TILDE OVERLAY, SIDDHAM VOWEL SIGN AA
    assert String.normalize("𑖹̴𑖯", :nfc) == "𑖹̴𑖯"

    # (ﬀ; ﬀ; ﬀ; ff; ff; ) LATIN SMALL LIGATURE FF
    # FB00;FB00;FB00;0066 0066;0066 0066;
    assert String.normalize("ﬀ", :nfkd) == "\u0066\u0066"

    # (ﬂ; ﬂ; ﬂ; fl; fl; ) LATIN SMALL LIGATURE FL
    # FB02;FB02;FB02;0066 006C;0066 006C;
    assert String.normalize("ﬂ", :nfkd) == "\u0066\u006C"

    # (ﬅ; ﬅ; ﬅ; st; st; ) LATIN SMALL LIGATURE LONG S T
    # FB05;FB05;FB05;0073 0074;0073 0074;
    assert String.normalize("ﬅ", :nfkd) == "\u0073\u0074"

    # (ﬆ; ﬆ; ﬆ; st; st; ) LATIN SMALL LIGATURE ST
    # FB06;FB06;FB06;0073 0074;0073 0074;
    assert String.normalize("\u0073\u0074", :nfkc) == "\u0073\u0074"

    # (ﬓ; ﬓ; ﬓ; մն; մն; ) ARMENIAN SMALL LIGATURE MEN NOW
    # FB13;FB13;FB13;0574 0576;0574 0576;
    assert String.normalize("\u0574\u0576", :nfkc) == "\u0574\u0576"
  end

  # Carriage return can be a grapheme cluster if followed by
  # newline so we test some corner cases here.
  test "carriage return" do
    assert String.at("\r\t\v", 0) == "\r"
    assert String.at("\r\t\v", 1) == "\t"
    assert String.at("\r\t\v", 2) == "\v"
    assert String.at("\xFF\r\t\v", 1) == "\r"
    assert String.at("\r\xFF\t\v", 2) == "\t"
    assert String.at("\r\t\xFF\v", 3) == "\v"

    assert String.last("\r\t\v") == "\v"
    assert String.last("\r\xFF\t\xFF\v") == "\v"

    assert String.next_grapheme("\r\t\v") == {"\r", "\t\v"}
    assert String.next_grapheme("\t\v") == {"\t", "\v"}
    assert String.next_grapheme("\v") == {"\v", ""}

    assert String.length("\r\t\v") == 3
    assert String.length("\r\xFF\t\v") == 4
    assert String.length("\r\t\xFF\v") == 4

    assert String.bag_distance("\r\t\xFF\v", "\xFF\r\n\xFF") == 0.25
    assert String.split("\r\t\v", "") == ["", "\r", "\t", "\v", ""]
  end

  test "to_existing_atom/2" do
    # constant
    assert String.to_existing_atom("foo", [:foo, :bar]) == :foo
    assert String.to_existing_atom("bar", [:foo, :bar]) == :bar

    assert_raise ArgumentError, fn ->
      String.to_existing_atom("baz", [:foo, :bar])
    end

    # variable
    values = [:foo, :bar]
    assert String.to_existing_atom("foo", values) == :foo
    assert String.to_existing_atom("bar", values) == :bar

    assert_raise ArgumentError, fn ->
      String.to_existing_atom("baz", values)
    end
  end
end
