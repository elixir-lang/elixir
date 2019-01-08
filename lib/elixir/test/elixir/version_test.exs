Code.require_file("test_helper.exs", __DIR__)

defmodule VersionTest do
  use ExUnit.Case, async: true

  doctest Version

  alias Version.Parser

  test "compare/2 with valid versions" do
    assert Version.compare("1.0.1", "1.0.0") == :gt
    assert Version.compare("1.1.0", "1.0.1") == :gt
    assert Version.compare("2.1.1", "1.2.2") == :gt
    assert Version.compare("1.0.0", "1.0.0-dev") == :gt
    assert Version.compare("1.2.3-dev", "0.1.2") == :gt
    assert Version.compare("1.0.0-a.b", "1.0.0-a") == :gt
    assert Version.compare("1.0.0-b", "1.0.0-a.b") == :gt
    assert Version.compare("1.0.0-a", "1.0.0-0") == :gt
    assert Version.compare("1.0.0-a.b", "1.0.0-a.a") == :gt

    assert Version.compare("1.0.0", "1.0.1") == :lt
    assert Version.compare("1.0.1", "1.1.0") == :lt
    assert Version.compare("1.2.2", "2.1.1") == :lt
    assert Version.compare("1.0.0-dev", "1.0.0") == :lt
    assert Version.compare("0.1.2", "1.2.3-dev") == :lt
    assert Version.compare("1.0.0-a", "1.0.0-a.b") == :lt
    assert Version.compare("1.0.0-a.b", "1.0.0-b") == :lt
    assert Version.compare("1.0.0-0", "1.0.0-a") == :lt
    assert Version.compare("1.0.0-a.a", "1.0.0-a.b") == :lt

    assert Version.compare("1.0.0", "1.0.0") == :eq
    assert Version.compare("1.0.0-dev", "1.0.0-dev") == :eq
    assert Version.compare("1.0.0-a", "1.0.0-a") == :eq
  end

  test "compare/2 with invalid versions" do
    assert_raise Version.InvalidVersionError, fn ->
      Version.compare("1.0", "1.0.0")
    end

    assert_raise Version.InvalidVersionError, fn ->
      Version.compare("1.0.0-dev", "1.0")
    end

    assert_raise Version.InvalidVersionError, fn ->
      Version.compare("foo", "1.0.0-a")
    end
  end

  test "lexes specifications properly" do
    assert Parser.lexer("== != > >= < <= ~>", []) == [:==, :!=, :>, :>=, :<, :<=, :~>]
    assert Parser.lexer("2.3.0", []) == [:==, "2.3.0"]
    assert Parser.lexer("!2.3.0", []) == [:!=, "2.3.0"]
    assert Parser.lexer(">>=", []) == [:>, :>=]
    assert Parser.lexer(">2.4.0", []) == [:>, "2.4.0"]
    assert Parser.lexer("> 2.4.0", []) == [:>, "2.4.0"]
    assert Parser.lexer("    >     2.4.0", []) == [:>, "2.4.0"]
    assert Parser.lexer(" or 2.1.0", []) == [:||, :==, "2.1.0"]
    assert Parser.lexer(" and 2.1.0", []) == [:&&, :==, "2.1.0"]
    assert Parser.lexer(">= 2.0.0 and < 2.1.0", []) == [:>=, "2.0.0", :&&, :<, "2.1.0"]
    assert Parser.lexer(">= 2.0.0 or < 2.1.0", []) == [:>=, "2.0.0", :||, :<, "2.1.0"]
  end

  test "parse/1" do
    assert {:ok, %Version{major: 1, minor: 2, patch: 3}} = Version.parse("1.2.3")
    assert {:ok, %Version{major: 1, minor: 4, patch: 5}} = Version.parse("1.4.5+ignore")
    assert {:ok, %Version{major: 0, minor: 0, patch: 1}} = Version.parse("0.0.1+sha.0702245")

    assert {:ok, %Version{major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]}} =
             Version.parse("1.4.5-6-g3318bd5")

    assert {:ok, %Version{major: 1, minor: 4, patch: 5, pre: [6, 7, "eight"]}} =
             Version.parse("1.4.5-6.7.eight")

    assert {:ok, %Version{major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]}} =
             Version.parse("1.4.5-6-g3318bd5+ignore")

    assert Version.parse("foobar") == :error
    assert Version.parse("2") == :error
    assert Version.parse("2.") == :error
    assert Version.parse("2.3") == :error
    assert Version.parse("2.3.") == :error
    assert Version.parse("2.3.0-") == :error
    assert Version.parse("2.3.0+") == :error
    assert Version.parse("2.3.0.") == :error
    assert Version.parse("2.3.0.4") == :error
    assert Version.parse("2.3.-rc.1") == :error
    assert Version.parse("2.3.+rc.1") == :error
    assert Version.parse("2.3.0-01") == :error
    assert Version.parse("2.3.00-1") == :error
    assert Version.parse("2.3.00") == :error
    assert Version.parse("2.03.0") == :error
    assert Version.parse("02.3.0") == :error
    assert Version.parse("0. 0.0") == :error
    assert Version.parse("0.1.0-&&pre") == :error
  end

  test "Kernel.to_string/1" do
    assert Version.parse!("1.0.0") |> to_string == "1.0.0"
    assert Version.parse!("1.0.0-dev") |> to_string == "1.0.0-dev"
    assert Version.parse!("1.0.0+lol") |> to_string == "1.0.0+lol"
    assert Version.parse!("1.0.0-dev+lol") |> to_string == "1.0.0-dev+lol"
    assert Version.parse!("1.0.0-0") |> to_string == "1.0.0-0"
    assert Version.parse!("1.0.0-rc.0") |> to_string == "1.0.0-rc.0"
  end

  test "match?/2 with invalid versions" do
    assert_raise Version.InvalidVersionError, fn ->
      Version.match?("foo", "2.3.0")
    end

    assert_raise Version.InvalidVersionError, fn ->
      Version.match?("2.3", "2.3.0")
    end

    assert_raise Version.InvalidRequirementError, fn ->
      Version.match?("2.3.0", "foo")
    end

    assert_raise Version.InvalidRequirementError, fn ->
      Version.match?("2.3.0", "2.3")
    end
  end

  test "==" do
    assert Version.match?("2.3.0", "2.3.0")
    refute Version.match?("2.4.0", "2.3.0")

    assert Version.match?("2.3.0", "== 2.3.0")
    refute Version.match?("2.4.0", "== 2.3.0")

    assert Version.match?("1.0.0", "1.0.0")
    assert Version.match?("1.0.0", "1.0.0")

    assert Version.match?("1.2.3-alpha", "1.2.3-alpha")

    assert Version.match?("0.9.3", "== 0.9.3+dev")

    {:ok, vsn} = Version.parse("2.3.0")
    assert Version.match?(vsn, "2.3.0")
  end

  test "!=" do
    assert Version.match?("2.4.0", "!2.3.0")
    refute Version.match?("2.3.0", "!2.3.0")

    assert Version.match?("2.4.0", "!= 2.3.0")
    refute Version.match?("2.3.0", "!= 2.3.0")
  end

  test ">" do
    assert Version.match?("2.4.0", "> 2.3.0")
    refute Version.match?("2.2.0", "> 2.3.0")
    refute Version.match?("2.3.0", "> 2.3.0")

    assert Version.match?("1.2.3", "> 1.2.3-alpha")
    assert Version.match?("1.2.3-alpha.1", "> 1.2.3-alpha")
    assert Version.match?("1.2.3-alpha.beta.sigma", "> 1.2.3-alpha.beta")
    refute Version.match?("1.2.3-alpha.10", "< 1.2.3-alpha.1")
    refute Version.match?("0.10.2-dev", "> 0.10.2")
  end

  test ">=" do
    assert Version.match?("2.4.0", ">= 2.3.0")
    refute Version.match?("2.2.0", ">= 2.3.0")
    assert Version.match?("2.3.0", ">= 2.3.0")

    assert Version.match?("2.0.0", ">= 1.0.0")
    assert Version.match?("1.0.0", ">= 1.0.0")
  end

  test "<" do
    assert Version.match?("2.2.0", "< 2.3.0")
    refute Version.match?("2.4.0", "< 2.3.0")
    refute Version.match?("2.3.0", "< 2.3.0")

    assert Version.match?("0.10.2-dev", "< 0.10.2")

    refute Version.match?("1.0.0", "< 1.0.0-dev")
    refute Version.match?("1.2.3-dev", "< 0.1.2")
  end

  test "<=" do
    assert Version.match?("2.2.0", "<= 2.3.0")
    refute Version.match?("2.4.0", "<= 2.3.0")
    assert Version.match?("2.3.0", "<= 2.3.0")
  end

  describe "~>" do
    test "regular cases" do
      assert Version.match?("3.0.0", "~> 3.0")
      assert Version.match?("3.2.0", "~> 3.0")
      refute Version.match?("4.0.0", "~> 3.0")
      refute Version.match?("4.4.0", "~> 3.0")

      assert Version.match?("3.0.2", "~> 3.0.0")
      assert Version.match?("3.0.0", "~> 3.0.0")
      refute Version.match?("3.1.0", "~> 3.0.0")
      refute Version.match?("3.4.0", "~> 3.0.0")

      assert Version.match?("3.6.0", "~> 3.5")
      assert Version.match?("3.5.0", "~> 3.5")
      refute Version.match?("4.0.0", "~> 3.5")
      refute Version.match?("5.0.0", "~> 3.5")

      assert Version.match?("3.5.2", "~> 3.5.0")
      assert Version.match?("3.5.4", "~> 3.5.0")
      refute Version.match?("3.6.0", "~> 3.5.0")
      refute Version.match?("3.6.3", "~> 3.5.0")

      assert Version.match?("0.9.3", "~> 0.9.3-dev")
      refute Version.match?("0.10.0", "~> 0.9.3-dev")

      refute Version.match?("0.3.0-dev", "~> 0.2.0")

      assert_raise Version.InvalidRequirementError, fn ->
        Version.match?("3.0.0", "~> 3")
      end
    end

    test "~> will never include pre-release versions of its upper bound" do
      refute Version.match?("2.2.0-dev", "~> 2.1.0")
      refute Version.match?("2.2.0-dev", "~> 2.1.0", allow_pre: false)
      refute Version.match?("2.2.0-dev", "~> 2.1.0-dev")
      refute Version.match?("2.2.0-dev", "~> 2.1.0-dev", allow_pre: false)
    end
  end

  test "allow_pre" do
    assert Version.match?("1.1.0", "~> 1.0", allow_pre: true)
    assert Version.match?("1.1.0", "~> 1.0", allow_pre: false)
    assert Version.match?("1.1.0-beta", "~> 1.0", allow_pre: true)
    refute Version.match?("1.1.0-beta", "~> 1.0", allow_pre: false)
    assert Version.match?("1.0.1-beta", "~> 1.0.0-beta", allow_pre: false)

    assert Version.match?("1.1.0", ">= 1.0.0", allow_pre: true)
    assert Version.match?("1.1.0", ">= 1.0.0", allow_pre: false)
    assert Version.match?("1.1.0-beta", ">= 1.0.0", allow_pre: true)
    refute Version.match?("1.1.0-beta", ">= 1.0.0", allow_pre: false)
    assert Version.match?("1.1.0-beta", ">= 1.0.0-beta", allow_pre: false)
  end

  test "and" do
    assert Version.match?("0.9.3", "> 0.9.0 and < 0.10.0")
    refute Version.match?("0.10.2", "> 0.9.0 and < 0.10.0")
  end

  test "or" do
    assert Version.match?("0.9.1", "0.9.1 or 0.9.3 or 0.9.5")
    assert Version.match?("0.9.3", "0.9.1 or 0.9.3 or 0.9.5")
    assert Version.match?("0.9.5", "0.9.1 or 0.9.3 or 0.9.5")

    refute Version.match?("0.9.6", "0.9.1 or 0.9.3 or 0.9.5")
  end

  test "compile requirement" do
    {:ok, req} = Version.parse_requirement("1.2.3")
    req = Version.compile_requirement(req)

    assert Version.match?("1.2.3", req)
    refute Version.match?("1.2.4", req)
  end
end
