Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffTupleTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  describe "Comparing Tuples" do
    test "one element tuple" do
      simple =
        quote do
          {1}
        end

      pattern = Pattern.new(simple, [], %{})

      expected_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %{ast: 1},
            rh: 1,
            diff_result: :eq
          }
        ]
      }

      actual = PatternDiff.compare(pattern, {1})

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %{ast: 1},
            rh: 2,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.compare(pattern, {2})

      assert actual == expected_no_match
    end

    test "two element tuple" do
      simple =
        quote do
          {1, 2}
        end

      pattern = Pattern.new(simple, [], %{})

      expected_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %{ast: 1},
            rh: 1,
            diff_result: :eq
          },
          %PatternDiff{
            type: :value,
            lh: %{ast: 2},
            rh: 2,
            diff_result: :eq
          }
        ]
      }

      actual = PatternDiff.compare(pattern, {1, 2})

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :tuple,
        items: [
          %PatternDiff{
            type: :value,
            lh: %{ast: 1},
            rh: 2,
            diff_result: :neq
          },
          %PatternDiff{
            type: :value,
            lh: %{ast: 2},
            rh: 3,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.compare(pattern, {2, 3})

      assert actual == expected_no_match
    end
  end
end
