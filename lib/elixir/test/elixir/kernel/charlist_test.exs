Code.require_file("../test_helper.exs", __DIR__)

defmodule CharlistTest do
  use ExUnit.Case, async: true

  test "heredoc" do
    assert __ENV__.line == 7

    assert ~c"foo\nbar\n" == ~c"""
           foo
           bar
           """

    assert __ENV__.line == 14

    assert ~c"foo\nbar '''\n" == ~c"""
           foo
           bar \'\'\'
           """
  end

  test "UTF-8" do
    assert length(~c" ゆんゆん") == 5
  end

  test "hex" do
    assert ~c"\x76" == ~c"v"
    assert ~c"\u00fF" == ~c"ÿ"
    assert ~c"\u{A}" == ~c"\n"
    assert ~c"\u{e9}" == ~c"é"
    assert ~c"\u{10F}" == [271]
    assert ~c"\u{10FF}" == [4351]
    assert ~c"\u{10FFF}" == [69631]
    assert ~c"\u{10FFFF}" == [1_114_111]
  end
end
