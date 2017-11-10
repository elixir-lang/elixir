Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffListTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  describe "compare lists" do
    test "that a list can compare" do
      simple =
        quote do
          [1]
        end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %ContainerDiff{
        type: :list,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [1])

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :list,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 2,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [2])

      assert actual == expected_no_match
    end

    test "That a two element list can compare" do

      simple =
        quote do
        [1, 2]
      end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %ContainerDiff{
        type: :list,
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

      actual = PatternDiff.cmp(pattern, [1, 2])

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :list,
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
            rh: 1,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [1, 1])

      assert actual == expected_no_match
    end

    test "That list can compare to [h | t]" do
      cons_pattern =
        quote do
          [1 | [2, 3]]
        end

      pattern = ExUnit.Pattern.new(cons_pattern, [], [])

      expected = %ContainerDiff{
        type: :list,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: :cons_l, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          },
          %ContainerDiff{
            type: :list,
            items: [
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: 2, meta: nil, pins: [], vars: []},
                rh: 2,
                diff_result: :eq
              },
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: 3, meta: nil, pins: [], vars: []},
                rh: 3,
                diff_result: :eq
              }
            ]
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [1, 2, 3])
      assert actual == expected
    end

    test "That list can compare to [h1 | h2 | t]" do
      cons_pattern =
        quote do
          [1 | [2 | [3]]]
        end

      pattern = ExUnit.Pattern.new(cons_pattern, [], [])

      expected = %ContainerDiff{
        type: :list,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: :cons_l, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          },
          %ContainerDiff{
            type: :list,
            items: [
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: 2, meta: :cons_l, pins: [], vars: []},
                rh: 2,
                diff_result: :eq
              },
              %ContainerDiff{
                type: :list,
                items: [
                  %PatternDiff{
                    type: :value,
                    lh: %Pattern{val: 3, meta: nil, pins: [], vars: []},
                    rh: 3,
                    diff_result: :eq
                  }
                ]
              }
            ]
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [1, 2, 3])
      assert actual == expected
    end

    test "That an uneven list can compare" do

      simple =
        quote do
        [1, 2]
      end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected = %ContainerDiff{
        type: :list,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          },
          %PatternDiff{
            type: :different,
            lh: %Pattern{val: 2, meta: nil, pins: [], vars: []},
            rh: :ex_unit_no_meaningful_value,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [1])

      assert actual == expected

      simple =
        quote do
        [1]
      end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected = %ContainerDiff{
        type: :list,
        items: [
          %PatternDiff{
            type: :value,
            lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
            rh: 1,
            diff_result: :eq
          },
          %PatternDiff{
            type: :different,
            lh: :ex_unit_no_meaningful_value,
            rh: 2,
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, [1, 2])

      assert actual == expected
    end
  end
end
