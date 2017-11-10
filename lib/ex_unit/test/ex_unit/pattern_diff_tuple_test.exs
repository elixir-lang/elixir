Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffTupleTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  describe "Comparing Tuples" do
    test "one element tuple" do
      simple = quote do
        {1}
      end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, {1})

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 2,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, {2})

      assert actual == expected_no_match
    end

    test "two element tuple" do
      simple = quote do
        {1, 2}
      end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          },
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 2, meta: nil, pins: [], vars: []},
            rh: 2,
            diff_result: :eq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, {1, 2})

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 2,
            diff_result: :neq
          },
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 2, meta: nil, pins: [], vars: []},
            rh: 3,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, {2, 3})

      assert actual == expected_no_match
    end

  end
end
