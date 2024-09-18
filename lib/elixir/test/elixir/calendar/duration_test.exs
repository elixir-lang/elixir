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
                 "unknown unit :years. Expected :year, :month, :week, :day, :hour, :minute, :second, :microsecond",
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

  test "from_iso8601/1" do
    assert Duration.from_iso8601("P1Y2M3DT4H5M6S") ==
             {:ok, %Duration{year: 1, month: 2, day: 3, hour: 4, minute: 5, second: 6}}

    assert Duration.from_iso8601("P3WT5H3M") == {:ok, %Duration{week: 3, hour: 5, minute: 3}}
    assert Duration.from_iso8601("PT5H3M") == {:ok, %Duration{hour: 5, minute: 3}}
    assert Duration.from_iso8601("P1Y2M3D") == {:ok, %Duration{year: 1, month: 2, day: 3}}
    assert Duration.from_iso8601("PT4H5M6S") == {:ok, %Duration{hour: 4, minute: 5, second: 6}}
    assert Duration.from_iso8601("P1Y2M") == {:ok, %Duration{year: 1, month: 2}}
    assert Duration.from_iso8601("P3D") == {:ok, %Duration{day: 3}}
    assert Duration.from_iso8601("PT4H5M") == {:ok, %Duration{hour: 4, minute: 5}}
    assert Duration.from_iso8601("PT6S") == {:ok, %Duration{second: 6}}
    assert Duration.from_iso8601("P2M4Y") == {:error, :invalid_date_component}
    assert Duration.from_iso8601("P4Y2W3Y") == {:error, :invalid_date_component}
    assert Duration.from_iso8601("P5HT4MT3S") == {:error, :invalid_date_component}
    assert Duration.from_iso8601("P5H3HT4M") == {:error, :invalid_date_component}
    assert Duration.from_iso8601("P0.5Y") == {:error, :invalid_date_component}
    assert Duration.from_iso8601("PT1D") == {:error, :invalid_time_component}
    assert Duration.from_iso8601("PT.6S") == {:error, :invalid_time_component}
    assert Duration.from_iso8601("PT0.5H") == {:error, :invalid_time_component}
    assert Duration.from_iso8601("invalid") == {:error, :invalid_duration}
  end

  test "from_iso8601!/1" do
    assert Duration.from_iso8601!("P1Y2M3DT4H5M6S") == %Duration{
             year: 1,
             month: 2,
             day: 3,
             hour: 4,
             minute: 5,
             second: 6
           }

    assert Duration.from_iso8601!("P3WT5H3M") == %Duration{week: 3, hour: 5, minute: 3}
    assert Duration.from_iso8601!("PT5H3M") == %Duration{hour: 5, minute: 3}
    assert Duration.from_iso8601!("P1Y2M3D") == %Duration{year: 1, month: 2, day: 3}
    assert Duration.from_iso8601!("PT4H5M6S") == %Duration{hour: 4, minute: 5, second: 6}
    assert Duration.from_iso8601!("P1Y2M") == %Duration{year: 1, month: 2}
    assert Duration.from_iso8601!("P3D") == %Duration{day: 3}
    assert Duration.from_iso8601!("PT4H5M") == %Duration{hour: 4, minute: 5}
    assert Duration.from_iso8601!("PT6S") == %Duration{second: 6}
    assert Duration.from_iso8601!("PT1,6S") == %Duration{second: 1, microsecond: {600_000, 1}}
    assert Duration.from_iso8601!("PT-1.6S") == %Duration{second: -1, microsecond: {-600_000, 1}}
    assert Duration.from_iso8601!("PT0,6S") == %Duration{second: 0, microsecond: {600_000, 1}}
    assert Duration.from_iso8601!("PT-0,6S") == %Duration{second: 0, microsecond: {-600_000, 1}}
    assert Duration.from_iso8601!("-PT-0,6S") == %Duration{second: 0, microsecond: {600_000, 1}}
    assert Duration.from_iso8601!("-P10DT4H") == %Duration{day: -10, hour: -4}
    assert Duration.from_iso8601!("-P10DT-4H") == %Duration{day: -10, hour: 4}
    assert Duration.from_iso8601!("P-10D") == %Duration{day: -10}
    assert Duration.from_iso8601!("+P10DT-4H") == %Duration{day: 10, hour: -4}
    assert Duration.from_iso8601!("P+10D") == %Duration{day: 10}
    assert Duration.from_iso8601!("-P+10D") == %Duration{day: -10}

    assert Duration.from_iso8601!("PT-1.234567S") == %Duration{
             second: -1,
             microsecond: {-234_567, 6}
           }

    assert Duration.from_iso8601!("PT1.12345678S") == %Duration{
             second: 1,
             microsecond: {123_456, 6}
           }

    assert Duration.from_iso8601!("P3Y4W-3DT-6S") == %Duration{
             year: 3,
             week: 4,
             day: -3,
             second: -6
           }

    assert Duration.from_iso8601!("PT-4.23S") == %Duration{second: -4, microsecond: {-230_000, 2}}

    assert_raise ArgumentError,
                 ~s/failed to parse duration "P5H3HT4M". reason: :invalid_date_component/,
                 fn ->
                   Duration.from_iso8601!("P5H3HT4M")
                 end

    assert_raise ArgumentError,
                 ~s/failed to parse duration "P4Y2W3Y". reason: :invalid_date_component/,
                 fn ->
                   Duration.from_iso8601!("P4Y2W3Y")
                 end

    assert_raise ArgumentError,
                 ~s/failed to parse duration "invalid". reason: :invalid_duration/,
                 fn ->
                   Duration.from_iso8601!("invalid")
                 end

    assert_raise ArgumentError,
                 ~s/failed to parse duration "P4.5YT6S". reason: :invalid_date_component/,
                 fn ->
                   Duration.from_iso8601!("P4.5YT6S")
                 end
  end

  test "to_iso8601/1" do
    assert %Duration{year: 1, month: 2, day: 3, hour: 4, minute: 5, second: 6}
           |> Duration.to_iso8601() == "P1Y2M3DT4H5M6S"

    assert %Duration{week: 3, hour: 5, minute: 3} |> Duration.to_iso8601() == "P3WT5H3M"
    assert %Duration{hour: 5, minute: 3} |> Duration.to_iso8601() == "PT5H3M"
    assert %Duration{year: 1, month: 2, day: 3} |> Duration.to_iso8601() == "P1Y2M3D"
    assert %Duration{hour: 4, minute: 5, second: 6} |> Duration.to_iso8601() == "PT4H5M6S"
    assert %Duration{year: 1, month: 2} |> Duration.to_iso8601() == "P1Y2M"
    assert %Duration{day: 3} |> Duration.to_iso8601() == "P3D"
    assert %Duration{hour: 4, minute: 5} |> Duration.to_iso8601() == "PT4H5M"
    assert %Duration{second: 6} |> Duration.to_iso8601() == "PT6S"
    assert %Duration{second: 1, microsecond: {600_000, 1}} |> Duration.to_iso8601() == "PT1.6S"
    assert %Duration{second: -1, microsecond: {-600_000, 1}} |> Duration.to_iso8601() == "PT-1.6S"

    assert %Duration{second: -1, microsecond: {-234_567, 6}} |> Duration.to_iso8601() ==
             "PT-1.234567S"

    assert %Duration{second: 1, microsecond: {123_456, 6}} |> Duration.to_iso8601() ==
             "PT1.123456S"

    assert %Duration{year: 3, week: 4, day: -3, second: -6} |> Duration.to_iso8601() ==
             "P3Y4W-3DT-6S"

    assert %Duration{second: -4, microsecond: {-230_000, 2}} |> Duration.to_iso8601() ==
             "PT-4.23S"

    assert %Duration{second: -4, microsecond: {230_000, 2}} |> Duration.to_iso8601() ==
             "PT-3.77S"

    assert %Duration{second: 2, microsecond: {-1_200_000, 4}} |> Duration.to_iso8601() ==
             "PT0.8000S"

    assert %Duration{second: 1, microsecond: {-1_200_000, 3}} |> Duration.to_iso8601() ==
             "PT-0.200S"

    assert %Duration{microsecond: {-800_000, 2}} |> Duration.to_iso8601() == "PT-0.80S"
    assert %Duration{microsecond: {-800_000, 0}} |> Duration.to_iso8601() == "PT0S"
    assert %Duration{microsecond: {-1_200_000, 2}} |> Duration.to_iso8601() == "PT-1.20S"
  end
end
