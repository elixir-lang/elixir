Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.PatternDiffMapTest do
  use ExUnit.Case

  alias ExUnit.{ContainerDiff, Pattern, PatternDiff}

  test "that a map can compare" do
    simple =
      quote do
        %{a: 1}
      end

    pattern = Pattern.new(simple, [], %{})

    expected_match = %ContainerDiff{
      type: :map,
      items: [
        %ContainerDiff{
          type: :tuple,
          items: [
            %PatternDiff{
              type: :value,
              lh: %{ast: :a},
              rh: :a,
              diff_result: :eq
            },
            %PatternDiff{
              type: :value,
              lh: %{ast: 1},
              rh: 1,
              diff_result: :eq
            }
          ]
        }
      ]
    }

    actual = PatternDiff.compare(pattern, %{a: 1})

    assert actual == expected_match

    expected_no_match = %ContainerDiff{
      type: :map,
      items: [
        %ContainerDiff{
          type: :tuple,
          items: [
            %PatternDiff{
              type: :value,
              lh: %{ast: :a},
              rh: :a,
              diff_result: :eq
            },
            %PatternDiff{
              type: :value,
              lh: %{ast: 1},
              rh: 2,
              diff_result: :neq
            }
          ]
        }
      ]
    }

    actual = PatternDiff.compare(pattern, %{a: 2})

    assert actual == expected_no_match
  end

  test "that a map with nothing in common can compare" do
    simple =
      quote do
        %{a: 1}
      end

    pattern = Pattern.new(simple, [], %{})

    expected_no_match = %ContainerDiff{
      type: :map,
      items: [
        %PatternDiff{
          type: :different,
          lh: %{ast: {:a, 1}},
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

    actual = PatternDiff.compare(pattern, %{b: 2})

    assert actual == expected_no_match
  end

  test "map compare with empty map" do
    simple =
      quote do
        %{a: 1}
      end

    pattern = Pattern.new(simple, [], %{})

    expected_no_match = %ContainerDiff{
      type: :map,
      items: [
        %PatternDiff{
          type: :different,
          lh: %{ast: {:a, 1}},
          rh: :ex_unit_no_meaningful_value,
          diff_result: :neq
        }
      ]
    }

    actual = PatternDiff.compare(pattern, %{})

    assert actual == expected_no_match

    simple =
      quote do
        %{}
      end

    pattern = Pattern.new(simple, [], %{})

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

    actual = PatternDiff.compare(pattern, %{a: 1})

    assert actual == expected_no_match
  end

  test "map with a pinned key" do
    simple =
      quote do
        %{^a => 1}
      end

    pattern = Pattern.new(simple, [a: :a], %{})
    actual = PatternDiff.compare(pattern, %{a: 1})

    expected_match = %ContainerDiff{
      type: :map,
      items: [
        %ContainerDiff{
          type: :tuple,
          items: [
            %PatternDiff{
              type: :value,
              lh: %{ast: {:^, [], [{:a, [], ExUnit.PatternDiffMapTest}]}},
              rh: :a,
              diff_result: :eq
            },
            %PatternDiff{
              type: :value,
              lh: %{ast: 1},
              rh: 1,
              diff_result: :eq
            }
          ]
        }
      ]
    }

    assert actual == expected_match
  end

  test "map with nil keys" do
    simple =
      quote do
        %{a: nil, b: nil}
      end

    pattern = Pattern.new(simple, [], %{})

    actual = PatternDiff.compare(pattern, %{a: nil, b: 1})

    expected_match = %ExUnit.ContainerDiff{
      items: [
        %ExUnit.ContainerDiff{
          items: [
            %ExUnit.PatternDiff{
              diff_result: :eq,
              lh: %{ast: :a},
              rh: :a,
              type: :value
            },
            %ExUnit.PatternDiff{
              diff_result: :eq,
              lh: %{ast: nil},
              rh: nil,
              type: :value
            }
          ],
          type: :tuple
        },
        %ExUnit.ContainerDiff{
          items: [
            %ExUnit.PatternDiff{
              diff_result: :eq,
              lh: %{ast: :b},
              rh: :b,
              type: :value
            },
            %ExUnit.PatternDiff{
              diff_result: :neq,
              lh: %{ast: nil},
              rh: 1,
              type: :different
            }
          ],
          type: :tuple
        }
      ],
      type: :map
    }

    assert actual == expected_match
  end
end
