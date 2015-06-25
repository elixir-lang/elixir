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

    assert :eq == V.compare("1.0.0",     "1.0.0")
    assert :eq == V.compare("1.0.0-dev", "1.0.0-dev")
    assert :eq == V.compare("1.0.0-a",   "1.0.0-a")
  end

  test "invalid compare" do
    assert_raise V.InvalidVersionError, fn ->
      V.compare("1.0", "1.0.0")
    end

    assert_raise V.InvalidVersionError, fn ->
      V.compare("1.0.0-dev", "1.0")
    end

    assert_raise V.InvalidVersionError, fn ->
      V.compare("foo", "1.0.0-a")
    end
  end

  test "lexes specifications properly" do
    assert P.lexer("== != > >= < <= ~>", []) == [:'==', :'!=', :'>', :'>=', :'<', :'<=', :'~>']
    assert P.lexer("2.3.0", [])              == [:'==', "2.3.0"]
    assert P.lexer("!2.3.0", [])             == [:'!=', "2.3.0"]
    assert P.lexer(">>=", [])                == [:'>', :'>=']
    assert P.lexer(">2.4.0", [])             == [:'>', "2.4.0"]
    assert P.lexer("    >     2.4.0", [])    == [:'>', "2.4.0"]
  end

  test "parse" do
    assert {:ok, %V{major: 1, minor: 2, patch: 3}} = V.parse("1.2.3")
    assert {:ok, %V{major: 1, minor: 4, patch: 5}} = V.parse("1.4.5+ignore")
    assert {:ok, %V{major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]}} = V.parse("1.4.5-6-g3318bd5")
    assert {:ok, %V{major: 1, minor: 4, patch: 5, pre: [6, 7, "eight"]}} = V.parse("1.4.5-6.7.eight")
    assert {:ok, %V{major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]}} = V.parse("1.4.5-6-g3318bd5+ignore")

    assert :error = V.parse("foobar")
    assert :error = V.parse("2.3")
    assert :error = V.parse("2")
    assert :error = V.parse("2.3.0-01")
  end

  test "to_string" do
    assert V.parse("1.0.0") |> elem(1) |> to_string == "1.0.0"
    assert V.parse("1.0.0-dev") |> elem(1) |> to_string == "1.0.0-dev"
    assert V.parse("1.0.0+lol") |> elem(1) |> to_string == "1.0.0+lol"
    assert V.parse("1.0.0-dev+lol") |> elem(1) |> to_string == "1.0.0-dev+lol"
    assert V.parse("1.0.0-0") |> elem(1) |> to_string == "1.0.0-0"
    assert V.parse("1.0.0-rc.0") |> elem(1) |> to_string == "1.0.0-rc.0"
  end

  test "invalid match" do
    assert_raise V.InvalidVersionError, fn ->
      V.match?("foo", "2.3.0")
    end

    assert_raise V.InvalidVersionError, fn ->
      V.match?("2.3", "2.3.0")
    end

    assert_raise V.InvalidRequirementError, fn ->
      V.match?("2.3.0", "foo")
    end

    assert_raise V.InvalidRequirementError, fn ->
      V.match?("2.3.0", "2.3")
    end
  end

  test "==" do
    assert V.match?("2.3.0", "2.3.0")
    refute V.match?("2.4.0", "2.3.0")

    assert V.match?("2.3.0", "== 2.3.0")
    refute V.match?("2.4.0", "== 2.3.0")

    assert V.match?("1.0.0", "1.0.0")
    assert V.match?("1.0.0", "1.0.0")

    assert V.match?("1.2.3-alpha", "1.2.3-alpha")

    assert V.match?("0.9.3", "== 0.9.3+dev")
  end

  test "!=" do
    assert V.match?("2.4.0", "!2.3.0")
    refute V.match?("2.3.0", "!2.3.0")

    assert V.match?("2.4.0", "!= 2.3.0")
    refute V.match?("2.3.0", "!= 2.3.0")
  end

  test ">" do
    assert V.match?("2.4.0", "> 2.3.0")
    refute V.match?("2.2.0", "> 2.3.0")
    refute V.match?("2.3.0", "> 2.3.0")

    assert V.match?("1.2.3", "> 1.2.3-alpha")
    assert V.match?("1.2.3-alpha.1", "> 1.2.3-alpha")
    assert V.match?("1.2.3-alpha.beta.sigma", "> 1.2.3-alpha.beta")
    refute V.match?("1.2.3-alpha.10", "< 1.2.3-alpha.1")
    refute V.match?("0.10.2-dev", "> 0.10.2")
  end

  test ">=" do
    assert V.match?("2.4.0", ">= 2.3.0")
    refute V.match?("2.2.0", ">= 2.3.0")
    assert V.match?("2.3.0", ">= 2.3.0")

    assert V.match?("2.0.0", ">= 1.0.0")
    assert V.match?("1.0.0", ">= 1.0.0")
  end

  test "<" do
    assert V.match?("2.2.0", "< 2.3.0")
    refute V.match?("2.4.0", "< 2.3.0")
    refute V.match?("2.3.0", "< 2.3.0")

    assert V.match?("0.10.2-dev", "< 0.10.2")

    refute V.match?("1.0.0", "< 1.0.0-dev")
    refute V.match?("1.2.3-dev", "< 0.1.2")
  end

  test "<=" do
    assert V.match?("2.2.0", "<= 2.3.0")
    refute V.match?("2.4.0", "<= 2.3.0")
    assert V.match?("2.3.0", "<= 2.3.0")
  end

  test "~>" do
    assert V.match?("3.0.0", "~> 3.0")
    assert V.match?("3.2.0", "~> 3.0")
    refute V.match?("4.0.0", "~> 3.0")
    refute V.match?("4.4.0", "~> 3.0")

    assert V.match?("3.0.2", "~> 3.0.0")
    assert V.match?("3.0.0", "~> 3.0.0")
    refute V.match?("3.1.0", "~> 3.0.0")
    refute V.match?("3.4.0", "~> 3.0.0")

    assert V.match?("3.6.0", "~> 3.5")
    assert V.match?("3.5.0", "~> 3.5")
    refute V.match?("4.0.0", "~> 3.5")
    refute V.match?("5.0.0", "~> 3.5")

    assert V.match?("3.5.2", "~> 3.5.0")
    assert V.match?("3.5.4", "~> 3.5.0")
    refute V.match?("3.6.0", "~> 3.5.0")
    refute V.match?("3.6.3", "~> 3.5.0")

    assert V.match?("0.9.3", "~> 0.9.3-dev")
    refute V.match?("0.10.0", "~> 0.9.3-dev")

    refute V.match?("0.3.0-dev", "~> 0.2.0")

    assert_raise V.InvalidRequirementError, fn ->
      V.match?("3.0.0", "~> 3")
    end
  end

  test "and" do
    assert V.match?("0.9.3", "> 0.9.0 and < 0.10.0")
    refute V.match?("0.10.2", "> 0.9.0 and < 0.10.0")
  end

  test "or" do
    assert V.match?("0.9.1", "0.9.1 or 0.9.3 or 0.9.5")
    assert V.match?("0.9.3", "0.9.1 or 0.9.3 or 0.9.5")
    assert V.match?("0.9.5", "0.9.1 or 0.9.3 or 0.9.5")

    refute V.match?("0.9.6", "0.9.1 or 0.9.3 or 0.9.5")
  end
end
