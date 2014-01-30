Code.require_file "test_helper.exs", __DIR__

defmodule VersionTest do
  use   ExUnit.Case, async: true
  alias Version.Parser, as: P
  alias Version, as: V

  test "compare" do
    assert :gt == V.compare("1.0.1",     "1.0.0")
    assert :gt == V.compare("1.1.0",     "1.0.1")
    assert :gt == V.compare("2.1.1",     "1.2.2")
    assert :gt == V.compare("1.0.0",     "1.0.0-dev")
    assert :gt == V.compare("1.2.3-dev", "0.1.2")
    assert :gt == V.compare("1.0.0-a.b", "1.0.0-a")
    assert :gt == V.compare("1.0.0-b",   "1.0.0-a.b")
    assert :gt == V.compare("1.0.0-a",   "1.0.0-0")
    assert :gt == V.compare("1.0.0-a.b", "1.0.0-a.a")

    assert :lt == V.compare("1.0.0",     "1.0.1")
    assert :lt == V.compare("1.0.1",     "1.1.0")
    assert :lt == V.compare("1.2.2",     "2.1.1")
    assert :lt == V.compare("1.0.0-dev", "1.0.0")
    assert :lt == V.compare("0.1.2",     "1.2.3-dev")
    assert :lt == V.compare("1.0.0-a",   "1.0.0-a.b")
    assert :lt == V.compare("1.0.0-a.b", "1.0.0-b")
    assert :lt == V.compare("1.0.0-0",   "1.0.0-a")
    assert :lt == V.compare("1.0.0-a.a", "1.0.0-a.b")

    assert :eq == V.compare("1.0.0", "1.0.0")
    assert :eq == V.compare("1.0.0-dev", "1.0.0-dev")
    assert :eq == V.compare("1.0.0-a", "1.0.0-a")
  end

  test "lexes specifications properly" do
    assert P.lexer("== != > >= < <= ~>", []) == [:'==', :'!=', :'>', :'>=', :'<', :'<=', :'~>']
    assert P.lexer("2.3", []) == [:'==', "2.3"]
    assert P.lexer("!2.3", []) == [:'!=', "2.3"]
    assert P.lexer(">>=", []) == [:'>', :'>=']
    assert P.lexer(">2.4", []) == [:'>', "2.4"]
    assert P.lexer("    >     2.4", []) == [:'>', "2.4"]
  end

  test "valid requirement" do
    assert V.valid_requirement?("2.3")
    refute V.valid_requirement?("> >= 2.3")
    refute V.valid_requirement?("> 2.3 and")
    refute V.valid_requirement?("> 2.3 or and 4.3")
    assert V.valid_requirement?("> 2.4 and 4.5")
    refute V.valid_requirement?("& 1.0.0")
  end

  test "valid version" do
    assert V.valid?("1.0.0")
    assert V.valid?("1.0.0-beep+boop")
    assert V.valid?("1.0.0+boop")
    refute V.valid?("1.0.")
    refute V.valid("1.2.b")
    refute V.valid("abc")
    refute V.valid("-beep")
  end

  test "parse" do
    assert V.Schema[major: 1, minor: 0, patch: 0] = V.parse("1")
    assert V.Schema[major: 1, minor: 2, patch: 0] = V.parse("1.2")
    assert V.Schema[major: 1, minor: 2, patch: 3] = V.parse("1.2.3")
    assert V.Schema[major: 1, minor: 4, patch: 0, pre: "5-g3318bd5"] = V.parse("1.4-5-g3318bd5")
  end

  test "==" do
    assert V.match?("2.3", "2.3")
    refute V.match?("2.4", "2.3")

    assert V.match?("2.3", "== 2.3")
    refute V.match?("2.4", "== 2.3")

    assert V.match?("1.0.0", "1.0.0")
    assert V.match?("1.0.0", "1.0")

    assert V.match?("1.2.3-alpha", "1.2.3-alpha")

    assert V.match?("iliketrains", "iliketrains")
    assert V.match?("1.2.3.4", "1.2.3.4")

    assert V.match?("0.9.3", "== 0.9.3+dev")
  end

  test "!=" do
    assert V.match?("2.4", "!2.3")
    refute V.match?("2.3", "!2.3")

    assert V.match?("2.4", "!= 2.3")
    refute V.match?("2.3", "!= 2.3")
  end

  test ">" do
    assert V.match?("2.4", "> 2.3")
    refute V.match?("2.2", "> 2.3")
    refute V.match?("2.3", "> 2.3")

    assert V.match?("1.2.3", "> 1.2.3-alpha")
    assert V.match?("1.2.3-alpha.1", "> 1.2.3-alpha")
    assert V.match?("1.2.3-alpha.beta.sigma", "> 1.2.3-alpha.beta")
    refute V.match?("1.2.3-alpha.10", "< 1.2.3-alpha.1")
    refute V.match?("0.10.2-dev", "> 0.10.2")

    refute V.match?("1.0.0-dev", "> 1.0.0")
    refute V.match?("0.1.2", "> 1.2.3-dev")
  end

  test ">=" do
    assert V.match?("2.4", ">= 2.3")
    refute V.match?("2.2", ">= 2.3")
    assert V.match?("2.3", ">= 2.3")

    assert V.match?("2.0", ">= 1.0")
    assert V.match?("1.0.0", ">= 1.0")
  end

  test "<" do
    assert V.match?("2.2", "< 2.3")
    refute V.match?("2.4", "< 2.3")
    refute V.match?("2.3", "< 2.3")

    assert V.match?("0.10.2-dev", "< 0.10.2")

    refute V.match?("1.0.0", "< 1.0.0-dev")
    refute V.match?("1.2.3-dev", "< 0.1.2")
  end

  test "<=" do
    assert V.match?("2.2", "<= 2.3")
    refute V.match?("2.4", "<= 2.3")
    assert V.match?("2.3", "<= 2.3")
  end

  test "~>" do
    assert V.match?("3.0", "~> 3.0")
    assert V.match?("3.2", "~> 3.0")
    refute V.match?("4.0", "~> 3.0")
    refute V.match?("4.4", "~> 3.0")

    assert V.match?("3.0.2", "~> 3.0.0")
    assert V.match?("3.0.0", "~> 3.0.0")
    refute V.match?("3.1", "~> 3.0.0")
    refute V.match?("3.4", "~> 3.0.0")

    assert V.match?("3.6", "~> 3.5")
    assert V.match?("3.5", "~> 3.5")
    refute V.match?("4.0", "~> 3.5")
    refute V.match?("5.0", "~> 3.5")

    assert V.match?("3.5.2", "~> 3.5.0")
    assert V.match?("3.5.4", "~> 3.5.0")
    refute V.match?("3.6", "~> 3.5.0")
    refute V.match?("3.6.3", "~> 3.5.0")

    assert V.match?("0.9.3", "~> 0.9.3-dev")
    refute V.match?("0.10.0", "~> 0.9.3-dev")

    refute V.match?("0.3.0-dev", "~> 0.2.0")
  end

  test "and" do
    assert V.match?("0.9.3", "> 0.9 and < 0.10")
    refute V.match?("0.10.2", "> 0.9 and < 0.10")
  end

  test "or" do
    assert V.match?("0.9.1", "0.9.1 or 0.9.3 or 0.9.5")
    assert V.match?("0.9.3", "0.9.1 or 0.9.3 or 0.9.5")
    assert V.match?("0.9.5", "0.9.1 or 0.9.3 or 0.9.5")
  end
end
