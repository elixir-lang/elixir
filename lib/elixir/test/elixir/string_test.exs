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

  %% test cases described in http://mortoray.com/2013/11/27/the-string-type-is-broken/
  test :unicode do
    assert String.reverse("noël") == "lëon"
    assert String.slice("noël", 0..2) == "noë"
    assert String.length("noël") == 4

    assert String.length("😸😾") == 2
    assert String.slice("😸😾", 1..1) == "😾"
    assert String.reverse("😸😾") == "😾😸"

    assert String.upcase("baﬄe") == "BAFFLE"
  end

  test :split do
    assert String.split("") == [""]
    assert String.split("foo bar") == ["foo", "bar"]
    assert String.split(" foo bar") == ["foo", "bar"]
    assert String.split("foo bar ") == ["foo", "bar"]
    assert String.split(" foo bar ") == ["foo", "bar"]
    assert String.split("foo\t\n\v\f\r\sbar\n") == ["foo", "bar"]
    assert String.split("foo" <> <<31>> <> "bar") == ["foo", "bar"]
    assert String.split("foo" <> <<194, 133>> <> "bar") == ["foo", "bar"]

    assert String.split("", ",") == [""]
    assert String.split("a,b,c", ",") == ["a", "b", "c"]
    assert String.split("a,b", ".") == ["a,b"]
    assert String.split("1,2 3,4", [" ", ","]) == ["1", "2", "3", "4"]
    assert String.split(" a b c ", " ") == ["", "a", "b", "c", ""]

    assert String.split("a,b,c", ",", global: false) == ["a", "b,c"]
    assert String.split("1,2 3,4", [" ", ","], global: false) == ["1", "2 3,4"]

    assert String.split(" a b c ", " ", trim: true) == ["a", "b", "c"]
    assert String.split(" a b c ", " ", trim: true, global: false) == ["a b c "]

    assert String.split("abé", "") == ["a", "b", "é", ""]
    assert String.split("abé", "", global: false) == ["a", "bé"]
    assert String.split("abé", "", trim: true) == ["a", "b", "é"]
  end

  test :split_with_regex do
    assert String.split("", %r{,}) == [""]
    assert String.split("a,b", %r{,}) == ["a", "b"]
    assert String.split("a,b,c", %r{,}) == ["a", "b", "c"]
    assert String.split("a,b,c", %r{,}, global: false) == ["a", "b,c"]
    assert String.split("a,b.c ", %r{\W}) == ["a", "b", "c", ""]
    assert String.split("a,b.c ", %r{\W}, trim: false) == ["a", "b", "c", ""]
    assert String.split("a,b", %r{\.}) == ["a,b"]
  end

  test :upcase do
    assert String.upcase("123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz") == "123 ABCD 456 EFG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ"
    assert String.upcase("") == ""
    assert String.upcase("abcD") == "ABCD"
  end

  test :upcase_utf8 do
    assert String.upcase("& % # àáâ ãäå 1 2 ç æ") == "& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ"
    assert String.upcase("àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ") == "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"
  end

  test :upcase_utf8_multibyte do
    assert String.upcase("straße") == "STRASSE"
    assert String.upcase("áüÈß") == "ÁÜÈSS"
  end

  test :downcase do
    assert String.downcase("123 ABcD 456 EfG HIJ ( %$#) KL MNOP @ QRST = -_ UVWXYZ") == "123 abcd 456 efg hij ( %$#) kl mnop @ qrst = -_ uvwxyz"
    assert String.downcase("abcD") == "abcd"
    assert String.downcase("") == ""
  end

  test :downcase_utf8 do
    assert String.downcase("& % # ÀÁÂ ÃÄÅ 1 2 Ç Æ") == "& % # àáâ ãäå 1 2 ç æ"
    assert String.downcase("ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ") == "àáâãäåæçèéêëìíîïðñòóôõöøùúûüýþ"
    assert String.downcase("áüÈß") == "áüèß"
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
    assert String.rstrip("a  abc  a" <> <<194, 133>>) == "a  abc  a"
    assert String.rstrip("   abc aa", ?a) == "   abc "
    assert String.rstrip("   abc __", ?_) == "   abc "
    assert String.rstrip("   cat 猫猫", ?猫) == "   cat "
  end

  test :lstrip do
    assert String.lstrip("") == ""
    assert String.lstrip("   abc  ") == "abc  "
    assert String.lstrip("a  abc  a") == "a  abc  a"
    assert String.lstrip("\n\na  abc  a") == "a  abc  a"
    assert String.lstrip("\t\n\v\f\r\sa  abc  a") == "a  abc  a"
    assert String.lstrip(<<31>> <> " a  abc  a") == "a  abc  a"
    assert String.lstrip(<<194, 133>> <> "a  abc  a") == "a  abc  a"
    assert String.lstrip("__  abc  _", ?_) == "  abc  _"
    assert String.lstrip("猫猫 cat   ", ?猫) == " cat   "
  end

  test :strip do
    assert String.strip("") == ""
    assert String.strip("   abc  ") == "abc"
    assert String.strip("a  abc  a\n\n") == "a  abc  a"
    assert String.strip("a  abc  a\t\n\v\f\r\s") == "a  abc  a"
    assert String.strip("___  abc  ___", ?_) == "  abc  "
    assert String.strip("猫猫猫  cat  猫猫猫", ?猫) == "  cat  "
  end

  test :rjust do
    assert String.rjust("", 5) == "     "
    assert String.rjust("abc", 5) == "  abc"
    assert String.rjust("  abc  ", 9) == "    abc  "
    assert String.rjust("猫", 5) == "    猫"
    assert String.rjust("abc", 5, ?-) == "--abc"
    assert String.rjust("abc", 5, ?猫) == "猫猫abc"
  end

  test :ljust do
    assert String.ljust("", 5) == "     "
    assert String.ljust("abc", 5) == "abc  "
    assert String.ljust("  abc  ", 9) == "  abc    "
    assert String.ljust("猫", 5) == "猫    "
    assert String.ljust("abc", 5, ?-) == "abc--"
    assert String.ljust("abc", 5, ?猫) == "abc猫猫"
  end

  test :reverse do
    assert String.reverse("") == ""
    assert String.reverse("abc") == "cba"
    assert String.reverse("Hello World") == "dlroW olleH"
    assert String.reverse("Hello ∂og") == "go∂ olleH"
    assert String.reverse("Ā̀stute") == "etutsĀ̀"
    assert String.reverse(String.reverse("Hello World")) == "Hello World"
  end

  test :replace do
    assert String.replace("a,b,c", ",", "-") == "a-b-c"
    assert String.replace("a,b,c", [",", "b"], "-") == "a---c"

    assert String.replace("a,b,c", ",", "-", global: false) == "a-b,c"
    assert String.replace("a,b,c", [",", "b"], "-", global: false) == "a-b,c"
    assert String.replace("ãéã", "é", "e", global: false) == "ãeã"

    assert String.replace("a,b,c", ",", "[]", insert_replaced: 2) == "a[],b[],c"
    assert String.replace("a,b,c", ",", "[]", insert_replaced: [1, 1]) == "a[,,]b[,,]c"
    assert String.replace("a,b,c", "b", "[]", insert_replaced: 1, global: false) == "a,[b],c"

    assert String.replace("a,b,c", %r/,(.)/, ",\\1\\1") == "a,bb,cc"
    assert String.replace("a,b,c", %r/,(.)/, ",\\1\\1", global: false) == "a,bb,c"
  end

  test :duplicate do
    assert String.duplicate("abc", 0) == ""
    assert String.duplicate("abc", 1) == "abc"
    assert String.duplicate("abc", 2) == "abcabc"
    assert String.duplicate("&ã$", 2) == "&ã$&ã$"
  end

  test :codepoints do
    assert String.codepoints("elixir") == ["e", "l", "i", "x", "i", "r"]
    assert String.codepoints("elixír") == ["e", "l", "i", "x", "í", "r"] # slovak
    assert String.codepoints("ոգելից ըմպելիք") == ["ո", "գ", "ե", "լ", "ի", "ց", " ", "ը", "մ", "պ", "ե", "լ", "ի", "ք"] # armenian
    assert String.codepoints("эліксір") == ["э", "л", "і", "к", "с", "і", "р"] # belarussian
    assert String.codepoints("ελιξήριο") == ["ε", "λ", "ι", "ξ", "ή", "ρ", "ι", "ο"] # greek
    assert String.codepoints("סם חיים") == ["ס", "ם", " ", "ח", "י", "י", "ם"] # hebraic
    assert String.codepoints("अमृत") == ["अ", "म", "ृ", "त"] # hindi
    assert String.codepoints("স্পর্শমণি") == ["স", "্", "প", "র", "্", "শ", "ম", "ণ", "ি"] # bengali
    assert String.codepoints("સર્વશ્રેષ્ઠ ઇલાજ") == ["સ", "ર", "્", "વ", "શ", "્", "ર", "ે", "ષ", "્", "ઠ", " ", "ઇ", "લ", "ા", "જ"] # gujarati
    assert String.codepoints("世界中の一番") == ["世", "界", "中", "の", "一", "番"] # japanese
    assert String.codepoints("がガちゃ") == ["が", "ガ", "ち", "ゃ"]
    assert String.codepoints("") == []
    assert String.codepoints("ϖͲϥЫݎߟΈټϘለДШव׆ש؇؊صلټܗݎޥޘ߉ऌ૫ሏᶆ℆ℙℱ ⅚Ⅷ↠∈⌘①ﬃ") ==
           ["ϖ", "Ͳ", "ϥ", "Ы", "ݎ", "ߟ", "Έ", "ټ", "Ϙ", "ለ", "Д", "Ш", "व", "׆", "ש", "؇", "؊", "ص", "ل", "ټ", "ܗ", "ݎ", "ޥ", "ޘ", "߉", "ऌ", "૫", "ሏ", "ᶆ", "℆", "ℙ", "ℱ", " ", "⅚", "Ⅷ", "↠", "∈", "⌘", "①", "ﬃ"]
  end

  test :graphemes do
    # Extended
    assert String.graphemes("Ā̀stute") == ["Ā̀", "s", "t", "u", "t", "e"]
    # CLRF
    assert String.graphemes("\n\r\f") == ["\n\r", "\f"]
    # Regional indicator
    assert String.graphemes("\x{1F1E6}\x{1F1E7}\x{1F1E8}") == ["\x{1F1E6}\x{1F1E7}\x{1F1E8}"]
    # Hangul
    assert String.graphemes("\x{1100}\x{115D}\x{B4A4}") == ["ᄀᅝ뒤"]
    # Special Marking with Extended
    assert String.graphemes("a\x{0300}\x{0903}") == ["a\x{0300}\x{0903}"]
  end

  test :next_grapheme do
    assert String.next_grapheme("Ā̀stute") == {"Ā̀", "stute"}
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
    assert String.at("elixir", 6) == nil
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
    assert String.slice("ειξήριολ", 8, 1) == ""
    assert String.slice("ειξήριολ", 9, 1) == nil
    assert String.slice("elixir", 0, 0) == ""
    assert String.slice("elixir", 5, 0) == ""
    assert String.slice("elixir", -5, 0) == ""
    assert String.slice("", 0, 1) == ""
    assert String.slice("", 1, 1) == nil

    assert String.slice("elixir", 1..3) == "lix"
    assert String.slice("elixir", -5..-3) == "lix"
    assert String.slice("elixir", -5..3) == "lix"
    assert String.slice("あいうえお", 2..3) == "うえ"
    assert String.slice("ειξήριολ", 2..4) == "ξήρ"
    assert String.slice("elixir", 3..6) == "xir"
    assert String.slice("あいうえお", 3..7) == "えお"
    assert String.slice("ειξήριολ", 5..8) == "ιολ"
    assert String.slice("elixir", -3..-2) == "xi"
    assert String.slice("あいうえお", -4..-2) == "いうえ"
    assert String.slice("ειξήριολ", -5..-3) == "ήρι"
    assert String.slice("elixir", 8..9) == nil
    assert String.slice("あいうえお", 6..7) == nil
    assert String.slice("ειξήριολ", 8..8) == ""
    assert String.slice("ειξήριολ", 9..9) == nil
    assert String.slice("", 0..0) == ""
    assert String.slice("", 1..1) == nil
    assert String.slice("あいうえお", -2..-4) == nil
    assert String.slice("あいうえお", -10..-15) == nil
  end

  test :valid? do
    assert String.valid?("afds")
    assert String.valid?("øsdfh")
    assert String.valid?("dskfjあska")

    refute String.valid?(<<0xffff :: 16>>)
    refute String.valid?("asd" <> <<0xffff :: 16>>)
  end

  test :valid_character? do
    assert String.valid_character?("a")
    assert String.valid_character?("ø")
    assert String.valid_character?("あ")

    refute String.valid_character?("\x{ffff}")
    refute String.valid_character?("ab")
  end

  test :starts_with? do
    ## Normal cases ##
    assert String.starts_with? "hello", "he"
    assert String.starts_with? "hello", "hello"
    assert String.starts_with? "hello", ["hellö", "hell"]
    assert String.starts_with? "エリクシア", "エリ"
    refute String.starts_with? "hello", "lo"
    refute String.starts_with? "hello", "hellö"
    refute String.starts_with? "hello", ["hellö", "goodbye"]
    refute String.starts_with? "エリクシア", "仙丹"

    ## Edge cases ##
    assert String.starts_with? "", ""
    assert String.starts_with? "", ["", "a"]
    assert String.starts_with? "b", ["", "a"]

    assert String.starts_with? "abc", ""
    assert String.starts_with? "abc", [""]

    refute String.starts_with? "", "abc"
    refute String.starts_with? "", [" "]

    ## Sanity checks ##
    assert String.starts_with? "", ["", ""]
    assert String.starts_with? "abc", ["", ""]
    assert_raise ArgumentError, fn ->
      String.starts_with? "abc", [["a"], "a"]
    end
  end

  test :ends_with? do
    ## Normal cases ##
    assert String.ends_with? "hello", "lo"
    assert String.ends_with? "hello", "hello"
    assert String.ends_with? "hello", ["hell", "lo", "xx"]
    assert String.ends_with? "hello", ["hellö", "lo"]
    assert String.ends_with? "エリクシア", "シア"
    refute String.ends_with? "hello", "he"
    refute String.ends_with? "hello", "hellö"
    refute String.ends_with? "hello", ["hel", "goodbye"]
    refute String.ends_with? "エリクシア", "仙丹"

    ## Edge cases ##
    assert String.ends_with? "", ""
    assert String.ends_with? "", ["", "a"]
    refute String.ends_with? "", ["a", "b"]

    assert String.ends_with? "abc", ""
    assert String.ends_with? "abc", ["", "x"]

    refute String.ends_with? "", "abc"
    refute String.ends_with? "", [" "]

    ## Sanity checks ##
    assert String.ends_with? "", ["", ""]
    assert String.ends_with? "abc", ["", ""]
    assert_raise ArgumentError, fn ->
      String.ends_with? "abc", [["c"], "c"]
    end
  end

  test :contains? do
    ## Normal cases ##
    assert String.contains? "elixir of life", "of"
    assert String.contains? "エリクシア", "シ"
    assert String.contains? "elixir of life", ["mercury", "life"]
    refute String.contains? "exlixir of life", "death"
    refute String.contains? "エリクシア", "仙"
    refute String.contains? "elixir of life", ["death", "mercury", "eternal life"]

    ## Edge cases ##
    assert String.contains? "", ""
    assert String.contains? "abc", ""
    assert String.contains? "abc", ["", "x"]

    refute String.contains? "", " "
    refute String.contains? "", "a"

    ## Sanity checks ##
    assert String.contains? "", ["", ""]
    assert String.contains? "abc", ["", ""]
    assert_raise ArgumentError, fn ->
      String.contains? "abc", [["b"], "b"]
    end
  end

  test :to_char_list do
    assert String.to_char_list("æß")  == { :ok, [?æ, ?ß] }
    assert String.to_char_list("abc") == { :ok, [?a, ?b, ?c] }

    assert String.to_char_list(<< 0xDF, 0xFF >>) == { :error, [], << 223, 255 >> }
    assert String.to_char_list(<< 106, 111, 115, 195 >>) == { :incomplete, 'jos', << 195 >> }
  end

  test :to_char_list! do
    assert String.to_char_list!("æß")  == [?æ, ?ß]
    assert String.to_char_list!("abc") == [?a, ?b, ?c]

    assert_raise String.UnicodeConversionError,
                 "invalid encoding starting at <<223, 255>>", fn ->
      String.to_char_list!(<< 0xDF, 0xFF >>)
    end

    assert_raise String.UnicodeConversionError,
                 "incomplete encoding starting at <<195>>", fn ->
      String.to_char_list!(<< 106, 111, 115, 195 >>)
    end
  end

  test :from_char_list do
    assert String.from_char_list([?æ, ?ß]) == { :ok, "æß" }
    assert String.from_char_list([?a, ?b, ?c]) == { :ok, "abc" }

    assert String.from_char_list([0xDFFF]) == { :error, "", [0xDFFF] }
  end

  test :from_char_list! do
    assert String.from_char_list!([?æ, ?ß]) == "æß"
    assert String.from_char_list!([?a, ?b, ?c]) == "abc"

    assert_raise String.UnicodeConversionError,
                 "invalid code point 57343", fn ->
      String.from_char_list!([0xDFFF])
    end
  end
end
