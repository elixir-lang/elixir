Code.require_file "../test_helper.exs", __DIR__

defmodule String.NormalizerTest do
  use ExUnit.Case, async: true

  # String.normalize

  test "String.normalize 015C;LATIN CAPITAL LETTER S WITH CIRCUMFLEX" do
    assert String.normalize("\u015C", :nfd) == "\u0053\u0302"
    assert String.normalize("\u0053\u0302", :nfc) == "\u015C"
  end

  test "String.normalize 015D;LATIN SMALL LETTER S WITH CIRCUMFLEX" do
    assert String.normalize("\u015D", :nfd) == "\u0073\u0302"
    assert String.normalize("\u0073\u0302", :nfc) == "\u015D"
  end

  test "String.normalize 1E64;LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE" do
    assert String.normalize("\u1E64", :nfd) == "\u0053\u0301\u0307"
    assert String.normalize("\u0053\u0301\u0307", :nfc) == "\u1E64"
  end

  test "String.normalize 1E65;LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE" do
    assert String.normalize("\u1E65", :nfd) == "\u0073\u0301\u0307"
    assert String.normalize("\u0073\u0301\u0307", :nfc) == "\u1E65"
  end

  # String.equivalent?

  test "String.equivalent? 015C;LATIN CAPITAL LETTER S WITH CIRCUMFLEX" do
    assert String.equivalent? "\u015C", "\u0053\u0302"
    assert String.equivalent? "\u0053\u0302", "\u015C"
  end

  test "String.equivalent? 015D;LATIN SMALL LETTER S WITH CIRCUMFLEX" do
    assert String.equivalent? "\u015D", "\u0073\u0302"
    assert String.equivalent? "\u0073\u0302", "\u015D"
  end

  test "String.equivalent? 1E64;LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE" do
    assert String.equivalent? "\u1E64", "\u0053\u0301\u0307"
    assert String.equivalent? "\u0053\u0301\u0307", "\u1E64"
  end

  test "String.equivalent? 1E65;LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE" do
    assert String.equivalent? "\u1E65", "\u0073\u0301\u0307"
    assert String.equivalent? "\u0073\u0301\u0307", "\u1E65"
  end
end
