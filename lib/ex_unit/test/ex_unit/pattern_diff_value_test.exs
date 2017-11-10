Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffValueTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  describe "value matching" do
    test "integer pattern match" do
      simple =
        quote do
          1
        end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %PatternDiff{
        type: :value,
        lh: pattern,
        rh: 1,
        diff_result: :eq
      }

      actual = PatternDiff.cmp(pattern, 1)

      assert actual == expected_match

      expected_no_match = %{expected_match | rh: 2, diff_result: :neq}

      actual = PatternDiff.cmp(pattern, 2)

      assert actual == expected_no_match
    end

    test "string pattern match" do
      simple =
        quote do
          "hello"
        end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %PatternDiff{
        type: :value,
        lh: pattern,
        rh: "hello",
        diff_result: :eq
      }

      actual = PatternDiff.cmp(pattern, "hello")

      assert actual == expected_match

      expected_no_match = %{
        expected_match
        | rh: "good bye",
          diff_result: :neq
      }

      actual = PatternDiff.cmp(pattern, "good bye")

      assert actual == expected_no_match
    end

    test "atom match" do
      simple =
        quote do
          :a
        end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %PatternDiff{
        type: :value,
        lh: pattern,
        rh: :a,
        diff_result: :eq
      }

      actual = PatternDiff.cmp(pattern, :a)

      assert actual == expected_match

      expected_no_match = %{expected_match | rh: :b, diff_result: :neq}

      actual = PatternDiff.cmp(pattern, :b)

      assert actual == expected_no_match
    end

    test "float pattern match" do
      simple =
        quote do
          1.0
        end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %PatternDiff{
        type: :value,
        lh: pattern,
        rh: 1.0,
        diff_result: :eq
      }

      actual = PatternDiff.cmp(pattern, 1.0)

      assert actual == expected_match

      expected_no_match = %{expected_match | rh: 2.0, diff_result: :neq}

      actual = PatternDiff.cmp(pattern, 2.0)

      assert actual == expected_no_match
    end
  end

  describe "that dissimilar things are labeled as different" do
    test "that map is not anything else" do
      map =
        quote do
          %{a: 1}
        end

      pattern = ExUnit.Pattern.new(map, [], [])
      actual = PatternDiff.cmp(pattern, 1)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, "string")
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, 2.0)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, [1, 2, 3])
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, {1, 2})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, {1, 2, 3})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, :a)
      assert actual.type == :different
    end

    test "that a list is not anything else" do
      map =
        quote do
          [1, 2, 3]
        end

      pattern = ExUnit.Pattern.new(map, [], [])
      actual = PatternDiff.cmp(pattern, 1)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, "string")
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, 2.0)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, %{a: 1, b: 2})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, {1, 2})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, {1, 2, 3})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, :a)
      assert actual.type == :different
    end

    test "that a tuple is not anything else" do
      tuple =
        quote do
        {1, 2, 3}
      end

      pattern = ExUnit.Pattern.new(tuple, [], [])
      actual = PatternDiff.cmp(pattern, 1)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, "string")
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, 2.0)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, %{a: 1, b: 2})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, [1, 2])
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, [1, 2, 3])
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, :a)
      assert actual.type == :different

      tuple =
        quote do
        {1, 2}
      end

      pattern = ExUnit.Pattern.new(tuple, [], [])
      actual = PatternDiff.cmp(pattern, 1)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, "string")
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, 2.0)
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, %{a: 1, b: 2})
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, [1, 2])
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, [1, 2, 3])
      assert actual.type == :different

      actual = PatternDiff.cmp(pattern, :a)
      assert actual.type == :different
    end
  end
end
