# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../../test_helper.exs", __DIR__)

defmodule ExUnit.Filters.WhereTest do
  use ExUnit.Case, async: true

  alias ExUnit.Filters.Where

  describe "parse/1" do
    test "parses simple tag" do
      assert Where.parse("slow") == {:ok, {:tag, :slow}}
    end

    test "parses tag with value" do
      assert Where.parse("interface:ui") == {:ok, {:tag, {:interface, "ui"}}}
    end

    test "parses 'and' expression" do
      assert Where.parse("slow and fast") ==
               {:ok, {:and, {:tag, :slow}, {:tag, :fast}}}
    end

    test "parses 'or' expression" do
      assert Where.parse("slow or fast") ==
               {:ok, {:or, {:tag, :slow}, {:tag, :fast}}}
    end

    test "parses 'not' expression" do
      assert Where.parse("not slow") == {:ok, {:not, {:tag, :slow}}}
    end

    test "parses nested 'not' expression" do
      assert Where.parse("not not slow") == {:ok, {:not, {:not, {:tag, :slow}}}}
    end

    test "parses parenthesized expression" do
      assert Where.parse("(slow)") == {:ok, {:tag, :slow}}
    end

    test "parses complex expression with parentheses" do
      assert Where.parse("(slow or fast) and integration") ==
               {:ok, {:and, {:or, {:tag, :slow}, {:tag, :fast}}, {:tag, :integration}}}
    end

    test "parses expression with correct precedence (and binds tighter than or)" do
      # "slow or fast and integration" should parse as "slow or (fast and integration)"
      assert Where.parse("slow or fast and integration") ==
               {:ok, {:or, {:tag, :slow}, {:and, {:tag, :fast}, {:tag, :integration}}}}
    end

    test "parses expression with correct precedence (not binds tighter than and)" do
      # "not slow and fast" should parse as "(not slow) and fast"
      assert Where.parse("not slow and fast") ==
               {:ok, {:and, {:not, {:tag, :slow}}, {:tag, :fast}}}
    end

    test "parses chained 'and' expressions (left associative)" do
      assert Where.parse("a and b and c") ==
               {:ok, {:and, {:and, {:tag, :a}, {:tag, :b}}, {:tag, :c}}}
    end

    test "parses chained 'or' expressions (left associative)" do
      assert Where.parse("a or b or c") ==
               {:ok, {:or, {:or, {:tag, :a}, {:tag, :b}}, {:tag, :c}}}
    end

    test "handles whitespace" do
      assert Where.parse("  slow  and  fast  ") ==
               {:ok, {:and, {:tag, :slow}, {:tag, :fast}}}
    end

    test "parses tag with colon-separated value" do
      assert Where.parse("status:pending") == {:ok, {:tag, {:status, "pending"}}}
    end
  end

  describe "eval/2" do
    test "evaluates simple tag presence" do
      assert Where.eval({:tag, :slow}, %{slow: true}) == true
      assert Where.eval({:tag, :slow}, %{slow: false}) == false
      assert Where.eval({:tag, :slow}, %{fast: true}) == false
    end

    test "evaluates tag with value" do
      assert Where.eval({:tag, {:interface, "ui"}}, %{interface: "ui"}) == true
      assert Where.eval({:tag, {:interface, "ui"}}, %{interface: "api"}) == false
      assert Where.eval({:tag, {:interface, "ui"}}, %{}) == false
    end

    test "evaluates 'and' expression" do
      expr = {:and, {:tag, :slow}, {:tag, :integration}}
      assert Where.eval(expr, %{slow: true, integration: true}) == true
      assert Where.eval(expr, %{slow: true, integration: false}) == false
      assert Where.eval(expr, %{slow: true}) == false
    end

    test "evaluates 'or' expression" do
      expr = {:or, {:tag, :slow}, {:tag, :integration}}
      assert Where.eval(expr, %{slow: true}) == true
      assert Where.eval(expr, %{integration: true}) == true
      assert Where.eval(expr, %{slow: true, integration: true}) == true
      assert Where.eval(expr, %{}) == false
    end

    test "evaluates 'not' expression" do
      expr = {:not, {:tag, :slow}}
      assert Where.eval(expr, %{slow: true}) == false
      assert Where.eval(expr, %{slow: false}) == true
      assert Where.eval(expr, %{}) == true
    end

    test "evaluates complex expression" do
      # (slow or fast) and not flaky
      expr = {:and, {:or, {:tag, :slow}, {:tag, :fast}}, {:not, {:tag, :flaky}}}

      assert Where.eval(expr, %{slow: true}) == true
      assert Where.eval(expr, %{fast: true}) == true
      assert Where.eval(expr, %{slow: true, flaky: true}) == false
      assert Where.eval(expr, %{fast: true, flaky: true}) == false
      assert Where.eval(expr, %{}) == false
    end

    test "evaluates tag value as atom comparison" do
      assert Where.eval({:tag, {:status, "pending"}}, %{status: :pending}) == true
    end
  end
end
