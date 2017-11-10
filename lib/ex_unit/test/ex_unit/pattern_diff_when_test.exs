Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffWhenTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  test "simple unbound var" do
    simple =
      quote do
       a
      end
    pattern = ExUnit.Pattern.new(simple, [], [a: :ex_unit_unbound_var])
    expected_match = %PatternDiff{
      type: :value,
      lh: pattern,
      rh: 1,
      diff_result: :eq
    }
    actual = PatternDiff.cmp(pattern, 1)
    assert actual == expected_match
  end

  test "simple bound var" do
    simple =
      quote do
      {a, a}
    end
    pattern = ExUnit.Pattern.new(simple, [], [a: :ex_unit_unbound_var])
    expected_var_pattern = %Pattern{val: {:a, [], ExUnit.PatternDiffWhenTest}, pins: [], vars: [a: :ex_unit_unbound_var]}
    expected_match = %ContainerDiff{
      type: :tuple,
      items: [
        %PatternDiff{
          type: :value,
          lh: expected_var_pattern,
          rh: 1,
          diff_result: :eq
        },
        %PatternDiff{
          type: :value,
          lh: expected_var_pattern,
          rh: 1,
          diff_result: :eq
        }
      ]
    }
    actual = PatternDiff.cmp(pattern, {1, 1})
    assert actual == expected_match

    expected_no_match = %ContainerDiff{
      type: :tuple,
      items: [
        %PatternDiff{
          type: :value,
          lh: expected_var_pattern,
          rh: 1,
          diff_result: :eq
        },
        %PatternDiff{
          type: :value,
          lh: expected_var_pattern,
          rh: 2,
          diff_result: :neq
        }
      ]
    }
    actual = PatternDiff.cmp(pattern, {1, 2})
    assert actual == expected_no_match
  end

  test "use a when" do


  end

  #### Handle Variables/Pins ####
  #### Consider how to replace pattern for internal workings ####
end
