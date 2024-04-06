Code.require_file("../test_helper.exs", __DIR__)

defmodule DurationTest do
  use ExUnit.Case, async: true
  doctest Duration

  test "new!/1" do
    assert Duration.new!(year: 2, month: 1, week: 3) == %Duration{year: 2, month: 1, week: 3}
    assert Duration.new!(microsecond: {20000, 2}) == %Duration{microsecond: {20000, 2}}

    duration = %Duration{year: 1}
    assert ^duration = Duration.new!(duration)

    assert_raise ArgumentError,
                 "unsupported value nil for :month. Expected an integer",
                 fn -> Duration.new!(month: nil) end

    assert_raise ArgumentError,
                 "unsupported unit :years. Expected :year, :month, :week, :day, :hour, :minute, :second, :microsecond",
                 fn -> Duration.new!(years: 1) end

    assert_raise ArgumentError,
                 "unsupported value {1, 2, 3} for :microsecond. Expected a tuple {ms, precision} where precision is an integer from 0 to 6",
                 fn -> Duration.new!(microsecond: {1, 2, 3}) end

    assert_raise ArgumentError,
                 "unsupported value {100, 7} for :microsecond. Expected a tuple {ms, precision} where precision is an integer from 0 to 6",
                 fn -> Duration.new!(microsecond: {100, 7}) end
  end

  test "add/2" do
    d1 = %Duration{
      year: 1,
      month: 2,
      week: 3,
      day: 4,
      hour: 5,
      minute: 6,
      second: 7,
      microsecond: {8, 6}
    }

    d2 = %Duration{
      year: 8,
      month: 7,
      week: 6,
      day: 5,
      hour: 4,
      minute: 3,
      second: 2,
      microsecond: {1, 6}
    }

    assert Duration.add(d1, d2) == %Duration{
             year: 9,
             month: 9,
             week: 9,
             day: 9,
             hour: 9,
             minute: 9,
             second: 9,
             microsecond: {9, 6}
           }

    assert Duration.add(d1, d2) == Duration.add(d2, d1)

    d1 = %Duration{month: 2, week: 3, day: 4}
    d2 = %Duration{year: 8, day: 2, second: 2}

    assert Duration.add(d1, d2) == %Duration{
             year: 8,
             month: 2,
             week: 3,
             day: 6,
             hour: 0,
             minute: 0,
             second: 2,
             microsecond: {0, 0}
           }

    d1 = %Duration{microsecond: {1000, 4}}
    d2 = %Duration{microsecond: {5, 6}}
    assert Duration.add(d1, d2) == %Duration{microsecond: {1005, 6}}
  end

  test "subtract/2" do
    d1 = %Duration{
      year: 1,
      month: 2,
      week: 3,
      day: 4,
      hour: 5,
      minute: 6,
      second: 7,
      microsecond: {8, 6}
    }

    d2 = %Duration{
      year: 8,
      month: 7,
      week: 6,
      day: 5,
      hour: 4,
      minute: 3,
      second: 2,
      microsecond: {1, 6}
    }

    assert Duration.subtract(d1, d2) == %Duration{
             year: -7,
             month: -5,
             week: -3,
             day: -1,
             hour: 1,
             minute: 3,
             second: 5,
             microsecond: {7, 6}
           }

    assert Duration.subtract(d2, d1) == %Duration{
             year: 7,
             month: 5,
             week: 3,
             day: 1,
             hour: -1,
             minute: -3,
             second: -5,
             microsecond: {-7, 6}
           }

    assert Duration.subtract(d1, d2) != Duration.subtract(d2, d1)

    d1 = %Duration{year: 10, month: 2, week: 3, day: 4}
    d2 = %Duration{year: 8, day: 2, second: 2}

    assert Duration.subtract(d1, d2) == %Duration{
             year: 2,
             month: 2,
             week: 3,
             day: 2,
             hour: 0,
             minute: 0,
             second: -2,
             microsecond: {0, 0}
           }

    d1 = %Duration{microsecond: {1000, 4}}
    d2 = %Duration{microsecond: {5, 6}}
    assert Duration.subtract(d1, d2) == %Duration{microsecond: {995, 6}}
  end

  test "multiply/2" do
    duration = %Duration{
      year: 1,
      month: 2,
      week: 3,
      day: 4,
      hour: 5,
      minute: 6,
      second: 7,
      microsecond: {8, 6}
    }

    assert Duration.multiply(duration, 3) == %Duration{
             year: 3,
             month: 6,
             week: 9,
             day: 12,
             hour: 15,
             minute: 18,
             second: 21,
             microsecond: {24, 6}
           }

    assert Duration.multiply(%Duration{year: 2, day: 4, minute: 5}, 4) ==
             %Duration{
               year: 8,
               month: 0,
               week: 0,
               day: 16,
               hour: 0,
               minute: 20,
               second: 0,
               microsecond: {0, 0}
             }
  end

  test "negate/1" do
    duration = %Duration{
      year: 1,
      month: 2,
      week: 3,
      day: 4,
      hour: 5,
      minute: 6,
      second: 7,
      microsecond: {8, 6}
    }

    assert Duration.negate(duration) == %Duration{
             year: -1,
             month: -2,
             week: -3,
             day: -4,
             hour: -5,
             minute: -6,
             second: -7,
             microsecond: {-8, 6}
           }

    assert Duration.negate(%Duration{year: 2, day: 4, minute: 5}) ==
             %Duration{
               year: -2,
               month: 0,
               week: 0,
               day: -4,
               hour: 0,
               minute: -5,
               second: 0,
               microsecond: {0, 0}
             }
  end
end
