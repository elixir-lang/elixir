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

  test :rstrip do
    assert String.rstrip("   abc  ") == "   abc"
    assert String.rstrip("   abc a") == "   abc a"
    assert String.rstrip("   abc aa", ?a) == "   abc "
    assert String.rstrip("   abc __", ?_) == "   abc "
  end

  test :lstrip do
    assert String.lstrip("   abc  ") == "abc  "
    assert String.lstrip("a  abc  a") == "a  abc  a"
    assert String.lstrip("__  abc  _", ?_) == "  abc  _"
  end

  test :strip do
    assert String.strip("   abc  ") == "abc"
    assert String.strip("___  abc  ___", ?_) == "  abc  "
  end

  test :replace do
    assert String.replace("a,b,c", ",", "-") == "a-b,c"
    assert String.replace("a,b,c", [",", "b"], "-") == "a-b,c"
	assert String.replace("ãéã", "é", "e") == "ãeã"
  end

  test :replace_with_options do
    assert String.replace("a,b,c", ",", "-", global: true) == "a-b-c"
    assert String.replace("a,b,c", [",","b"], "-", global: true) == "a---c"
    assert String.replace("a,b,c", "b", "[]", insert_replaced: 1) == "a,[b],c"
    assert String.replace("a,b,c", ",", "[]", global: true, insert_replaced: 2) == "a[],b[],c"
    assert String.replace("a,b,c", ",", "[]", global: true, insert_replaced: [1,1]) == "a[,,]b[,,]c"
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

  test :first do
    assert String.first("elixir") == "e"
    assert String.first("íelixr") == "í"
    assert String.first("եոգլից ըմպելիք") == "ե"
    assert String.first("лэіксір") == "л"
    assert String.first("ελιξήριο") == "ε"
    assert String.first("סם חיים") == "ס"
    assert String.first("がガちゃ") == "が"
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
  end

end
