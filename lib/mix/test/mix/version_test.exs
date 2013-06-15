Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.VersionTest do
  use   ExUnit.Case, async: true
  alias Mix.Version.Requirement, as: R
  alias Mix.Version, as: V

  test "lexes specifications properly" do
    assert R.lexer("== != > >= < <= ~>", []) == [:'==', :'!=', :'>', :'>=', :'<', :'<=', :'~>']
    assert R.lexer("2.3", []) == [:'==', "2.3"]
    assert R.lexer("!2.3", []) == [:'!=', "2.3"]
    assert R.lexer(">>=", []) == [:'>', :'>=']
    assert R.lexer(">2.4", []) == [:'>', "2.4"]
    assert R.lexer("    >     2.4", []) == [:'>', "2.4"]
  end

  test "lexer gets verified properly" do
    assert R.valid?(R.lexer("2.3", []))
    refute R.valid?(R.lexer("> >= 2.3", []))
    refute R.valid?(R.lexer("> 2.3 and", []))
    refute R.valid?(R.lexer("> 2.3 or and 4.3", []))
    assert R.valid?(R.lexer("> 2.4 and 4.5", []))
    refute R.valid?(R.lexer("& 1.0.0", []))
  end

  test "matches properly" do
    assert V.match?("2.3", "2.3")
    refute V.match?("2.3", "2.4")

    assert V.match?("!2.3", "2.4")
    refute V.match?("!2.3", "2.3")

    assert V.match?("> 2.3", "2.4")
    refute V.match?("> 2.3", "2.2")
    refute V.match?("> 2.3", "2.3")

    assert V.match?(">= 2.3", "2.4")
    refute V.match?(">= 2.3", "2.2")
    assert V.match?(">= 2.3", "2.3")

    assert V.match?("< 2.3", "2.2")
    refute V.match?("< 2.3", "2.4")
    refute V.match?("< 2.3", "2.3")

    assert V.match?("<= 2.3", "2.2")
    refute V.match?("<= 2.3", "2.4")
    assert V.match?("<= 2.3", "2.3")

    assert V.match?("~> 3.0", "3.0")
    assert V.match?("~> 3.0", "3.2")
    refute V.match?("~> 3.0", "4.0")
    refute V.match?("~> 3.0", "4.4")

    assert V.match?("~> 3.0.0", "3.0.2")
    assert V.match?("~> 3.0.0", "3.0.0")
    refute V.match?("~> 3.0.0", "3.1")
    refute V.match?("~> 3.0.0", "3.4")

    assert V.match?("~> 3.5", "3.6")
    assert V.match?("~> 3.5", "3.5")
    refute V.match?("~> 3.5", "4.0")
    refute V.match?("~> 3.5", "5.0")

    assert V.match?("~> 3.5.0", "3.5.2")
    assert V.match?("~> 3.5.0", "3.5.4")
    refute V.match?("~> 3.5.0", "3.6")
    refute V.match?("~> 3.5.0", "3.6.3")

    assert V.match?("1.0.0", "1.0.0")
    assert V.match?("1.0", "1.0.0")
    assert V.match?(">= 1.0", "2.0")
    assert V.match?(">= 1.0", "1.0.0")

    assert V.match?("1.2.3-alpha", "1.2.3-alpha")
    refute V.match?("> 1.2.3-alpha", "1.2.3")
    assert V.match?("> 1.2.3-alpha", "1.2.3-alpha1")
    assert V.match?("> 1.2.3-alpha1", "1.2.3-alpha10")

    assert V.match?("iliketrains", "iliketrains")
    assert V.match?("1.2.3.4", "1.2.3.4")
  end
end
