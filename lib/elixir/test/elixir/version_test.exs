Code.require_file "test_helper.exs", __DIR__

defmodule VersionTest do
  use   ExUnit.Case, async: true
  alias Version.Parser, as: P
  alias Version, as: V

  test "compare" do
    assert { :ok, :gt } == V.compare("1.0.1",     "1.0.0")
    assert { :ok, :gt } == V.compare("1.1.0",     "1.0.1")
    assert { :ok, :gt } == V.compare("2.1.1",     "1.2.2")
    assert { :ok, :gt } == V.compare("1.0.0",     "1.0.0-dev")
    assert { :ok, :gt } == V.compare("1.2.3-dev", "0.1.2")
    assert { :ok, :gt } == V.compare("1.0.0-a.b", "1.0.0-a")
    assert { :ok, :gt } == V.compare("1.0.0-b",   "1.0.0-a.b")
    assert { :ok, :gt } == V.compare("1.0.0-a",   "1.0.0-0")
    assert { :ok, :gt } == V.compare("1.0.0-a.b", "1.0.0-a.a")

    assert { :ok, :lt } == V.compare("1.0.0",     "1.0.1")
    assert { :ok, :lt } == V.compare("1.0.1",     "1.1.0")
    assert { :ok, :lt } == V.compare("1.2.2",     "2.1.1")
    assert { :ok, :lt } == V.compare("1.0.0-dev", "1.0.0")
    assert { :ok, :lt } == V.compare("0.1.2",     "1.2.3-dev")
    assert { :ok, :lt } == V.compare("1.0.0-a",   "1.0.0-a.b")
    assert { :ok, :lt } == V.compare("1.0.0-a.b", "1.0.0-b")
    assert { :ok, :lt } == V.compare("1.0.0-0",   "1.0.0-a")
    assert { :ok, :lt } == V.compare("1.0.0-a.a", "1.0.0-a.b")

    assert { :ok, :eq } == V.compare("1.0.0",     "1.0.0")
    assert { :ok, :eq } == V.compare("1.0.0-dev", "1.0.0-dev")
    assert { :ok, :eq } == V.compare("1.0.0-a",   "1.0.0-a")
  end

  test "invalid compare" do
    assert { :error, _ } = V.compare("1.0", "1.0.0")
    assert { :error, _ } = V.compare("1.0.0-dev", "1.0")
    assert { :error, _ } = V.compare("foo", "1.0.0-a")
  end

  test "lexes specifications properly" do
    assert P.lexer("== != > >= < <= ~>", []) == [:'==', :'!=', :'>', :'>=', :'<', :'<=', :'~>']
    assert P.lexer("2.3.0", [])              == [:'==', "2.3.0"]
    assert P.lexer("!2.3.0", [])             == [:'!=', "2.3.0"]
    assert P.lexer(">>=", [])                == [:'>', :'>=']
    assert P.lexer(">2.4.0", [])             == [:'>', "2.4.0"]
    assert P.lexer("    >     2.4.0", [])    == [:'>', "2.4.0"]
  end

  test "valid requirement" do
    assert V.valid_requirement?("2.3.0")
    refute V.valid_requirement?("> >= 2.3.0")
    refute V.valid_requirement?("> 2.3.0 and")
    refute V.valid_requirement?("> 2.3.0 or and 4.3.0")
    assert V.valid_requirement?("> 2.4.0 and 4.5.0")
    refute V.valid_requirement?("& 1.0.0")
  end

  test "valid version" do
    assert V.valid?("1.0.0")
    assert V.valid?("1.0.0-beep+boop")
    assert V.valid?("1.0.0+boop")
    refute V.valid?("1.0.")
    refute V.valid?("1.2.b")
    refute V.valid?("abc")
    refute V.valid?("-beep")
  end

  test "parse" do
    assert { :ok, V.Schema[major: 1, minor: 2, patch: 3] } = V.parse("1.2.3")
    assert { :ok, V.Schema[major: 1, minor: 4, patch: 5] } = V.parse("1.4.5+ignore")
    assert { :ok, V.Schema[major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]] } = V.parse("1.4.5-6-g3318bd5")
    assert { :ok, V.Schema[major: 1, minor: 4, patch: 5, pre: [6, 7, "eight"]] } = V.parse("1.4.5-6.7.eight")
    assert { :ok, V.Schema[major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]] } = V.parse("1.4.5-6-g3318bd5+ignore")

    assert { :error, :invalid_version } = V.parse("foobar")
    assert { :error, :inexact_version } = V.parse("2.3")
    assert { :error, :inexact_version } = V.parse("2")
    assert { :error, :leading_zeros_in_prerelease } = V.parse("2.3.0-01")
  end

  test "invalid match" do
    assert { :error, :invalid_version } == V.match?("foo", "2.3.0")
    assert { :error, :invalid_version } == V.match?("2.3.0", "foo")
    assert { :error, :inexact_version } == V.match?("2.3.0", "2.3")
    assert { :error, :inexact_version } == V.match?("2.3", "2.3.0")
  end

  test "==" do
    assert { :ok, true } == V.match?("2.3.0", "2.3.0")
    assert { :ok, false } == V.match?("2.4.0", "2.3.0")

    assert { :ok, true } == V.match?("2.3.0", "== 2.3.0")
    assert { :ok, false } == V.match?("2.4.0", "== 2.3.0")

    assert { :ok, true } == V.match?("1.0.0", "1.0.0")
    assert { :ok, true } == V.match?("1.0.0", "1.0.0")

    assert { :ok, true } == V.match?("1.2.3-alpha", "1.2.3-alpha")

    assert { :ok, true } == V.match?("0.9.3", "== 0.9.3+dev")
  end

  test "!=" do
    assert { :ok, true } == V.match?("2.4.0", "!2.3.0")
    assert { :ok, false } == V.match?("2.3.0", "!2.3.0")

    assert { :ok, true } == V.match?("2.4.0", "!= 2.3.0")
    assert { :ok, false } == V.match?("2.3.0", "!= 2.3.0")

    assert { :error, :inexact_version } == V.match?("2.3.0", "! 2.3")
    assert { :error, :inexact_version } == V.match?("2.3.0", "!= 2.3")
  end

  test ">" do
    assert { :ok, true } == V.match?("2.4.0", "> 2.3.0")
    assert { :ok, false } == V.match?("2.2.0", "> 2.3.0")
    assert { :ok, false } == V.match?("2.3.0", "> 2.3.0")

    assert { :ok, true } == V.match?("1.2.3", "> 1.2.3-alpha")
    assert { :ok, true } == V.match?("1.2.3-alpha.1", "> 1.2.3-alpha")
    assert { :ok, true } == V.match?("1.2.3-alpha.beta.sigma", "> 1.2.3-alpha.beta")
    assert { :ok, false } == V.match?("1.2.3-alpha.10", "< 1.2.3-alpha.1")
    assert { :ok, false } == V.match?("0.10.2-dev", "> 0.10.2")
  end

  test ">=" do
    assert { :ok, true } == V.match?("2.4.0", ">= 2.3.0")
    assert { :ok, false } == V.match?("2.2.0", ">= 2.3.0")
    assert { :ok, true } == V.match?("2.3.0", ">= 2.3.0")

    assert { :ok, true } == V.match?("2.0.0", ">= 1.0.0")
    assert { :ok, true } == V.match?("1.0.0", ">= 1.0.0")
  end

  test "<" do
    assert { :ok, true } == V.match?("2.2.0", "< 2.3.0")
    assert { :ok, false } == V.match?("2.4.0", "< 2.3.0")
    assert { :ok, false } == V.match?("2.3.0", "< 2.3.0")

    assert { :ok, true } == V.match?("0.10.2-dev", "< 0.10.2")

    assert { :ok, false } == V.match?("1.0.0", "< 1.0.0-dev")
    assert { :ok, false } == V.match?("1.2.3-dev", "< 0.1.2")
  end

  test "<=" do
    assert { :ok, true } == V.match?("2.2.0", "<= 2.3.0")
    assert { :ok, false } == V.match?("2.4.0", "<= 2.3.0")
    assert { :ok, true } == V.match?("2.3.0", "<= 2.3.0")
  end

  test "~>" do
    assert { :ok, true } == V.match?("3.0.0", "~> 3.0")
    assert { :ok, true } == V.match?("3.2.0", "~> 3.0")
    assert { :ok, false } == V.match?("4.0.0", "~> 3.0")
    assert { :ok, false } == V.match?("4.4.0", "~> 3.0")

    assert { :ok, true } == V.match?("3.0.2", "~> 3.0.0")
    assert { :ok, true } == V.match?("3.0.0", "~> 3.0.0")
    assert { :ok, false } == V.match?("3.1.0", "~> 3.0.0")
    assert { :ok, false } == V.match?("3.4.0", "~> 3.0.0")

    assert { :ok, true } == V.match?("3.6.0", "~> 3.5")
    assert { :ok, true } == V.match?("3.5.0", "~> 3.5")
    assert { :ok, false } == V.match?("4.0.0", "~> 3.5")
    assert { :ok, false } == V.match?("5.0.0", "~> 3.5")

    assert { :ok, true } == V.match?("3.5.2", "~> 3.5.0")
    assert { :ok, true } == V.match?("3.5.4", "~> 3.5.0")
    assert { :ok, false } == V.match?("3.6.0", "~> 3.5.0")
    assert { :ok, false } == V.match?("3.6.3", "~> 3.5.0")

    assert { :ok, true } == V.match?("0.9.3", "~> 0.9.3-dev")
    assert { :ok, false } == V.match?("0.10.0", "~> 0.9.3-dev")

    assert { :ok, false } == V.match?("0.3.0-dev", "~> 0.2.0")

    assert { :error, :inexact_version } == V.match?("3.0.0", "~> 3")
  end

  test "and" do
    assert { :ok, true } == V.match?("0.9.3", "> 0.9.0 and < 0.10.0")
    assert { :ok, false } == V.match?("0.10.2", "> 0.9.0 and < 0.10.0")
  end

  test "or" do
    assert { :ok, true } == V.match?("0.9.1", "0.9.1 or 0.9.3 or 0.9.5")
    assert { :ok, true } == V.match?("0.9.3", "0.9.1 or 0.9.3 or 0.9.5")
    assert { :ok, true } == V.match?("0.9.5", "0.9.1 or 0.9.3 or 0.9.5")

    assert { :ok, false } == V.match?("0.9.6", "0.9.1 or 0.9.3 or 0.9.5")
  end
end
