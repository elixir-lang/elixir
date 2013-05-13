Code.require_file "test_helper.exs", __DIR__

defmodule StringTest do
  use ExUnit.Case, async: true

  test :integer_codepoints do
    assert ?é == 233
    assert ?\xE9 == 233
    assert ?\351 == 233
  end

  test :next_codepoint do
    assert String.next_codepoint("ésoj") == { "é", "soj" }
    assert String.next_codepoint(<<255>>) == { <<255>>, "" }
    assert String.next_codepoint("") == :no_codepoint
  end

  test :split do
    assert String.split("foo bar") == ["foo", "bar"]
    assert String.split(" foo bar") == ["foo", "bar"]
    assert String.split("foo bar ") == ["foo", "bar"]
    assert String.split(" foo bar ") == ["foo", "bar"]
    assert String.split("foo\t\n\v\f\r\sbar\n") == ["foo", "bar"]
    assert String.split("foo" <> <<31>> <> "bar") == ["foo", "bar"]
    assert String.split("foo" <> <<194,133>> <> "bar") == ["foo", "bar"]

    assert String.split("a,b,c", ",") == ["a", "b", "c"]
    assert String.split("a,b", ".") == ["a,b"]
    assert String.split("1,2 3,4", [" ", ","]) == ["1", "2", "3", "4"]

    assert String.split("a,b,c", ",", global: false) == ["a", "b,c"]
    assert String.split("1,2 3,4", [" ", ","], global: false) == ["1", "2 3,4"]
  end

  test :split_with_regex do
    assert String.split("a,b", %r{,}) == ["a", "b"]
    assert String.split("a,b,c", %r{,}) == ["a", "b", "c"]
    assert String.split("a,b,c", %r{,}, global: false) == ["a", "b,c"]
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

  test :upcase_utf8_multibyte do
    assert String.upcase("straße") == "STRASSE"
  end

  test :downcase do
    assert String.downcase("123 ABcD 456 EfG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ") == "123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz"
    assert String.downcase("") == ""
  end

  test :downcase_utf8 do
    assert String.downcase("& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ") == "& % # àáâ ãäå 1 2 ç æ"
    assert String.downcase("ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ") == "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ"
  end

  test :capitalize do
    assert String.capitalize("") == ""
    assert String.capitalize("abc") == "Abc"
    assert String.capitalize("ABC") == "Abc"
    assert String.capitalize("c b a") == "C b a"
    assert String.capitalize("1ABC") == "1abc"
    assert String.capitalize("_aBc1") == "_abc1"
    assert String.capitalize(" aBc1") == " abc1"
  end

  test :capitalize_utf8 do
    assert String.capitalize("àáâ") == "Àáâ"
    assert String.capitalize("ÀÁÂ") == "Àáâ"
    assert String.capitalize("âáà") == "Âáà"
    assert String.capitalize("ÂÁÀ") == "Âáà"
    assert String.capitalize("òóôõö") == "Òóôõö"
    assert String.capitalize("ÒÓÔÕÖ") == "Òóôõö"
    assert String.capitalize("ﬁn") == "Fin"
  end

  test :rstrip do
    assert String.rstrip("") == ""
    assert String.rstrip("   abc  ") == "   abc"
    assert String.rstrip("   abc a") == "   abc a"
    assert String.rstrip("a  abc  a\n\n") == "a  abc  a"
    assert String.rstrip("a  abc  a\t\n\v\f\r\s") == "a  abc  a"
    assert String.rstrip("a  abc  a " <> <<31>>) == "a  abc  a"
    assert String.rstrip("a  abc  a" <> <<194,133>>) == "a  abc  a"
    assert String.rstrip("   abc aa", ?a) == "   abc "
    assert String.rstrip("   abc __", ?_) == "   abc "
  end

  test :lstrip do
    assert String.lstrip("") == ""
    assert String.lstrip("   abc  ") == "abc  "
    assert String.lstrip("a  abc  a") == "a  abc  a"
    assert String.lstrip("\n\na  abc  a") == "a  abc  a"
    assert String.lstrip("\t\n\v\f\r\sa  abc  a") == "a  abc  a"
    assert String.lstrip(<<31>> <> " a  abc  a") == "a  abc  a"
    assert String.lstrip(<<194,133>> <> "a  abc  a") == "a  abc  a"
    assert String.lstrip("__  abc  _", ?_) == "  abc  _"
  end

  test :strip do
    assert String.strip("") == ""
    assert String.strip("   abc  ") == "abc"
    assert String.strip("a  abc  a\n\n") == "a  abc  a"
    assert String.strip("a  abc  a\t\n\v\f\r\s") == "a  abc  a"
    assert String.strip("___  abc  ___", ?_) == "  abc  "
  end

  test :replace do
    assert String.replace("a,b,c", ",", "-") == "a-b-c"
    assert String.replace("a,b,c", [",","b"], "-") == "a---c"

    assert String.replace("a,b,c", ",", "-", global: false) == "a-b,c"
    assert String.replace("a,b,c", [",", "b"], "-", global: false) == "a-b,c"
    assert String.replace("ãéã", "é", "e", global: false) == "ãeã"

    assert String.replace("a,b,c", ",", "[]", insert_replaced: 2) == "a[],b[],c"
    assert String.replace("a,b,c", ",", "[]", insert_replaced: [1,1]) == "a[,,]b[,,]c"
    assert String.replace("a,b,c", "b", "[]", insert_replaced: 1, global: false) == "a,[b],c"
  end

  test :duplicate do
    assert String.duplicate("abc", 1) == "abc"
    assert String.duplicate("abc", 2) == "abcabc"
    assert String.duplicate("&ã$", 2) == "&ã$&ã$"
  end

  test :codepoints do
    assert String.codepoints("elixir") == ["e","l","i","x","i","r"]
    assert String.codepoints("elixír") == ["e","l","i","x","í","r"] # slovak
    assert String.codepoints("ոգելից ըմպելիք") == ["ո","գ","ե","լ","ի","ց"," ","ը","մ","պ","ե","լ","ի","ք"] # armenian
    assert String.codepoints("эліксір") == ["э","л","і","к","с","і","р"] # belarussian
    assert String.codepoints("ελιξήριο") == ["ε","λ","ι","ξ","ή","ρ","ι","ο"] # greek
    assert String.codepoints("סם חיים") == ["ס","ם"," ","ח","י","י","ם"] # hebraic
    assert String.codepoints("अमृत") == ["अ","म","ृ","त"] # hindi
    assert String.codepoints("স্পর্শমণি") == ["স","্","প","র","্","শ","ম","ণ","ি"] # bengali
    assert String.codepoints("સર્વશ્રેષ્ઠ ઇલાજ") == ["સ","ર","્","વ","શ","્","ર","ે","ષ","્","ઠ"," ","ઇ","લ","ા","જ"] # gujarati
    assert String.codepoints("世界中の一番") == ["世","界","中", "の", "一", "番"] # japanese
    assert String.codepoints("がガちゃ") == ["が", "ガ", "ち", "ゃ"]
    assert String.codepoints("") == []
  end

  test :mixed_codepoints do
    assert String.codepoints("ϖͲϥЫݎߟΈټϘለДШव׆ש؇؊صلټܗݎޥޘ߉ऌ૫ሏᶆ℆ℙℱ ⅚Ⅷ↠∈⌘①ﬃ") == ["ϖ","Ͳ","ϥ","Ы","ݎ","ߟ","Έ","ټ","Ϙ","ለ","Д","Ш","व","׆","ש","؇","؊","ص","ل","ټ","ܗ","ݎ","ޥ","ޘ","߉","ऌ","૫","ሏ","ᶆ","℆","ℙ","ℱ"," ","⅚","Ⅷ","↠","∈","⌘","①","ﬃ"]
  end

  test :graphemes do
    assert String.graphemes("Ā̀stute") == ["Ā̀","s","t","u","t","e"]
  end

  test :next_grapheme do
    assert String.next_grapheme("Ā̀stute") == {"Ā̀","stute"}
    assert String.next_grapheme("") == :no_grapheme
  end

  test :first do
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

  test :last do
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

  test :length do
    assert String.length("elixir") == 6
    assert String.length("elixrí") == 6
    assert String.length("եոգլից") == 6
    assert String.length("ліксрэ") == 6
    assert String.length("ειξήριολ") == 8
    assert String.length("סם ייםח") == 7
    assert String.length("がガちゃ") == 4
    assert String.length("Ā̀stute") == 6    
    assert String.length("") == 0
  end

  test :at do
    assert String.at("л", 0) == "л"
    assert String.at("elixir", 1) == "l"
    assert String.at("がガちゃ", 2) == "ち"
    assert String.at("л", 10) == nil
    assert String.at("elixir", -1) == "r"
    assert String.at("がガちゃ", -2) == "ち"
    assert String.at("л", -3) == nil
    assert String.at("Ā̀stute", 1) == "s"
    assert String.at("elixir",6) == nil
  end

  test :slice do
    assert String.slice("elixir", 1, 3) == "lix"
    assert String.slice("あいうえお", 2, 2) == "うえ"
    assert String.slice("ειξήριολ", 2, 3) == "ξήρ"
    assert String.slice("elixir", 3, 4) == "xir"
    assert String.slice("あいうえお", 3, 5) == "えお"
    assert String.slice("ειξήριολ", 5, 4) == "ιολ"
    assert String.slice("elixir", -3, 2) == "xi"
    assert String.slice("あいうえお", -4, 3) == "いうえ"
    assert String.slice("ειξήριολ", -5, 3) == "ήρι"
    assert String.slice("elixir", -10, 1) == nil
    assert String.slice("あいうえお", -10, 2) == nil
    assert String.slice("ειξήριολ", -10, 3) == nil
    assert String.slice("elixir", 8, 2) == nil
    assert String.slice("あいうえお", 6, 2) == nil
    assert String.slice("ειξήριολ", 8, 1) == nil
    assert String.slice("", 0, 1) == nil
  end

  test :valid? do
    assert String.valid?("afds")
    assert String.valid?("øsdfh")
    assert String.valid?("dskfjあska")

    refute String.valid?(<<0xffff :: 16>>)
    refute String.valid?("asd" <> <<0xffff :: 16>>)
  end

  test :valid_codepoint? do
    assert String.valid_codepoint?("a")
    assert String.valid_codepoint?("ø")
    assert String.valid_codepoint?("あ")

    refute String.valid_codepoint?(<<0xffff :: 16>>)
    refute String.valid_codepoint?("ab")
  end

end
