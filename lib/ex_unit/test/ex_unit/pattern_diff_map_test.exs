Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffMapTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  describe "compare maps" do
    test "that a map can compare" do
      simple =
        quote do
          %{a: 1}
        end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_match = %ContainerDiff{
        type: :map,
        items: [
          %ContainerDiff {
            type: :tuple,
            items: [
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: :a, meta: nil, pins: [], vars: []},
                rh: :a,
                diff_result: :eq
              },
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
                rh: 1,
                diff_result: :eq
              }
            ]
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, %{a: 1})

      assert actual == expected_match

      expected_no_match = %ContainerDiff{
        type: :map,
        items: [
          %ContainerDiff {
            type: :tuple,
            items: [
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: :a, meta: nil, pins: [], vars: []},
                rh: :a,
                diff_result: :eq
              },
              %PatternDiff{
                type: :value,
                lh: %Pattern{val: 1, meta: nil, pins: [], vars: []},
                rh: 2,
                diff_result: :neq
              }
            ]
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, %{a: 2})

      assert actual == expected_no_match
    end

    test "that a map with nothing in common can compare" do
      simple =
        quote do
        %{a: 1}
      end

      pattern = ExUnit.Pattern.new(simple, [], [])

      expected_no_match = %ContainerDiff{
        type: :map,
        items: [
              %PatternDiff{
                type: :different,
                lh: %Pattern{val: {:a, 1}, meta: nil, pins: [], vars: []},
                rh: :ex_unit_no_meaningful_value,
                diff_result: :neq
              },
              %PatternDiff{
                type: :different,
                lh: :ex_unit_no_meaningful_value,
                rh: {:b, 2},
                diff_result: :neq
              }
        ]
      }

      actual = PatternDiff.cmp(pattern, %{b: 2})

      assert actual == expected_no_match
    end

    test "map compare with empty map" do
      simple = quote do
        %{a: 1}
      end
      pattern = ExUnit.Pattern.new(simple, [], [])
      expected_no_match = %ContainerDiff{
        type: :map,
        items: [
          %PatternDiff{
            type: :different,
            lh: %Pattern{val: {:a, 1}, meta: nil, pins: [], vars: []},
            rh: :ex_unit_no_meaningful_value,
            diff_result: :neq
          },
        ]
      }

      actual = PatternDiff.cmp(pattern, %{})

      assert actual == expected_no_match

      simple = quote do
        %{}
      end
      pattern = ExUnit.Pattern.new(simple, [], [])
      expected_no_match = %ContainerDiff{
        type: :map,
        items: [
          %PatternDiff{
            type: :different,
            lh: :ex_unit_no_meaningful_value,
            rh: {:a, 1},
            diff_result: :neq
          }
        ]
      }

      actual = PatternDiff.cmp(pattern, %{a: 1})

      assert actual == expected_no_match
    end

  end
end
