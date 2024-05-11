Code.require_file("../test_helper.exs", __DIR__)

defmodule Calendar.ISOTest do
  use ExUnit.Case, async: true
  doctest Calendar.ISO

  describe "date_from_iso_days" do
    test "with positive dates" do
      assert {0, 1, 1} == iso_day_roundtrip(0, 1, 1)
      assert {0, 12, 31} == iso_day_roundtrip(0, 12, 31)
      assert {1, 12, 31} == iso_day_roundtrip(1, 12, 31)
      assert {4, 1, 1} == iso_day_roundtrip(4, 1, 1)
      assert {4, 12, 31} == iso_day_roundtrip(4, 12, 31)
      assert {9999, 12, 31} == iso_day_roundtrip(9999, 12, 31)
      assert {9999, 1, 1} == iso_day_roundtrip(9999, 1, 1)
      assert {9996, 12, 31} == iso_day_roundtrip(9996, 12, 31)
      assert {9996, 1, 1} == iso_day_roundtrip(9996, 1, 1)
    end

    test "with negative dates" do
      assert {-1, 1, 1} == iso_day_roundtrip(-1, 1, 1)
      assert {-1, 12, 31} == iso_day_roundtrip(-1, 12, 31)
      assert {-1, 12, 31} == iso_day_roundtrip(-1, 12, 31)
      assert {-2, 1, 1} == iso_day_roundtrip(-2, 1, 1)
      assert {-5, 12, 31} == iso_day_roundtrip(-5, 12, 31)

      assert {-4, 1, 1} == iso_day_roundtrip(-4, 1, 1)
      assert {-4, 12, 31} == iso_day_roundtrip(-4, 12, 31)

      assert {-9999, 12, 31} == iso_day_roundtrip(-9999, 12, 31)
      assert {-9996, 12, 31} == iso_day_roundtrip(-9996, 12, 31)

      assert {-9996, 12, 31} == iso_day_roundtrip(-9996, 12, 31)
      assert {-9996, 1, 1} == iso_day_roundtrip(-9996, 1, 1)
    end
  end

  describe "date_to_string/4" do
    test "regular use" do
      assert Calendar.ISO.date_to_string(1000, 1, 1, :basic) == "10000101"
      assert Calendar.ISO.date_to_string(1000, 1, 1, :extended) == "1000-01-01"

      assert Calendar.ISO.date_to_string(-123, 1, 1, :basic) == "-01230101"
      assert Calendar.ISO.date_to_string(-123, 1, 1, :extended) == "-0123-01-01"
    end

    test "handles years > 9999" do
      assert Calendar.ISO.date_to_string(10000, 1, 1, :basic) == "100000101"
      assert Calendar.ISO.date_to_string(10000, 1, 1, :extended) == "10000-01-01"
    end
  end

  describe "naive_datetime_to_iso_days/7" do
    test "raises with invalid dates" do
      assert_raise ArgumentError, "invalid date: 2018-02-30", fn ->
        Calendar.ISO.naive_datetime_to_iso_days(2018, 2, 30, 0, 0, 0, 0)
      end

      assert_raise ArgumentError, "invalid date: 2017-11--03", fn ->
        Calendar.ISO.naive_datetime_to_iso_days(2017, 11, -3, 0, 0, 0, 0)
      end
    end
  end

  describe "day_of_week/4" do
    test "raises with invalid dates" do
      assert_raise ArgumentError, "invalid date: 2018-02-30", fn ->
        Calendar.ISO.day_of_week(2018, 2, 30, :default)
      end

      assert_raise ArgumentError, "invalid date: 2017-11-00", fn ->
        Calendar.ISO.day_of_week(2017, 11, 0, :default)
      end
    end
  end

  describe "day_of_era/3" do
    test "raises with invalid dates" do
      assert_raise ArgumentError, "invalid date: 2018-02-30", fn ->
        Calendar.ISO.day_of_era(2018, 2, 30)
      end

      assert_raise ArgumentError, "invalid date: 2017-11-00", fn ->
        Calendar.ISO.day_of_era(2017, 11, 0)
      end
    end
  end

  describe "day_of_year/3" do
    test "raises with invalid dates" do
      assert_raise ArgumentError, "invalid date: 2018-02-30", fn ->
        Calendar.ISO.day_of_year(2018, 2, 30)
      end

      assert_raise ArgumentError, "invalid date: 2017-11-00", fn ->
        Calendar.ISO.day_of_year(2017, 11, 0)
      end
    end
  end

  test "year_of_era/3" do
    # Compatibility tests for year_of_era/1
    assert Calendar.ISO.year_of_era(-9999) == {10000, 0}
    assert Calendar.ISO.year_of_era(-1) == {2, 0}
    assert Calendar.ISO.year_of_era(0) == {1, 0}
    assert Calendar.ISO.year_of_era(1) == {1, 1}
    assert Calendar.ISO.year_of_era(1984) == {1984, 1}

    assert Calendar.ISO.year_of_era(-9999, 1, 1) == {10000, 0}
    assert Calendar.ISO.year_of_era(-1, 1, 1) == {2, 0}
    assert Calendar.ISO.year_of_era(0, 12, 1) == {1, 0}
    assert Calendar.ISO.year_of_era(1, 12, 1) == {1, 1}
    assert Calendar.ISO.year_of_era(1984, 12, 1) == {1984, 1}

    random_positive_year = Enum.random(1..9999)
    assert Calendar.ISO.year_of_era(random_positive_year, 1, 1) == {random_positive_year, 1}
  end

  defp iso_day_roundtrip(year, month, day) do
    iso_days = Calendar.ISO.date_to_iso_days(year, month, day)
    Calendar.ISO.date_from_iso_days(iso_days)
  end

  describe "parse_date/1" do
    test "supports both only extended format by default" do
      assert Calendar.ISO.parse_date("20150123") == {:error, :invalid_format}
      assert Calendar.ISO.parse_date("2015-01-23") == {:ok, {2015, 1, 23}}
    end
  end

  describe "parse_date/2" do
    test "allows enforcing basic formats" do
      assert Calendar.ISO.parse_date("20150123", :basic) == {:ok, {2015, 1, 23}}
      assert Calendar.ISO.parse_date("2015-01-23", :basic) == {:error, :invalid_format}
    end

    test "allows enforcing extended formats" do
      assert Calendar.ISO.parse_date("20150123", :extended) == {:error, :invalid_format}
      assert Calendar.ISO.parse_date("2015-01-23", :extended) == {:ok, {2015, 1, 23}}
    end

    test "errors on other format names" do
      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_date("20150123", :other)
      end

      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_date("2015-01-23", :other)
      end
    end
  end

  describe "parse_time/1" do
    test "supports only extended format by default" do
      assert Calendar.ISO.parse_time("235007") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("23:50:07") == {:ok, {23, 50, 7, {0, 0}}}
    end

    test "ignores offset data but requires valid ones" do
      assert Calendar.ISO.parse_time("23:50:07Z") == {:ok, {23, 50, 7, {0, 0}}}
      assert Calendar.ISO.parse_time("23:50:07+01:00") == {:ok, {23, 50, 7, {0, 0}}}

      assert Calendar.ISO.parse_time("2015-01-23 23:50-00:00") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015-01-23 23:50-00:60") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015-01-23 23:50-24:00") == {:error, :invalid_format}
    end

    test "supports either comma or period millisecond delimiters" do
      assert Calendar.ISO.parse_time("23:50:07,012345") == {:ok, {23, 50, 7, {12345, 6}}}
      assert Calendar.ISO.parse_time("23:50:07.012345") == {:ok, {23, 50, 7, {12345, 6}}}
    end

    test "only supports reduced precision for milliseconds" do
      assert Calendar.ISO.parse_time("23:50:07.012345") == {:ok, {23, 50, 7, {12345, 6}}}
      assert Calendar.ISO.parse_time("23:50:07") == {:ok, {23, 50, 7, {0, 0}}}
      assert Calendar.ISO.parse_time("23:50") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("23") == {:error, :invalid_format}
    end

    test "supports various millisecond precisions" do
      assert Calendar.ISO.parse_time("23:50:07.012345") == {:ok, {23, 50, 7, {12345, 6}}}
      assert Calendar.ISO.parse_time("23:50:07.0123") == {:ok, {23, 50, 7, {12300, 4}}}
      assert Calendar.ISO.parse_time("23:50:07.01") == {:ok, {23, 50, 7, {10000, 2}}}
      assert Calendar.ISO.parse_time("23:50:07.0") == {:ok, {23, 50, 7, {0, 1}}}
      assert Calendar.ISO.parse_time("23:50:07") == {:ok, {23, 50, 7, {0, 0}}}
    end

    test "truncates extra millisecond precision" do
      assert Calendar.ISO.parse_time("23:50:07.012345") == {:ok, {23, 50, 7, {12345, 6}}}
      assert Calendar.ISO.parse_time("23:50:07.0123456") == {:ok, {23, 50, 7, {12345, 6}}}
    end

    test "rejects strings with formatting errors" do
      assert Calendar.ISO.parse_time("23:50:07A") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("23:50:07.") == {:error, :invalid_format}
    end

    test "refuses to parse the wrong thing" do
      assert Calendar.ISO.parse_time("2015:01:23 23-50-07") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015-01-23 23:50:07") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015:01:23T23-50-07") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015-01-23T23:50:07") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015:01:23") == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("2015-01-23") == {:error, :invalid_format}
    end

    test "recognizes invalid times" do
      assert Calendar.ISO.parse_time("23:59:61") == {:error, :invalid_time}
      assert Calendar.ISO.parse_time("23:61:59") == {:error, :invalid_time}
      assert Calendar.ISO.parse_time("25:59:59") == {:error, :invalid_time}
    end
  end

  describe "parse_time/2" do
    test "allows enforcing basic formats" do
      assert Calendar.ISO.parse_time("235007", :basic) == {:ok, {23, 50, 7, {0, 0}}}
      assert Calendar.ISO.parse_time("23:50:07", :basic) == {:error, :invalid_format}
    end

    test "allows enforcing extended formats" do
      assert Calendar.ISO.parse_time("235007", :extended) == {:error, :invalid_format}
      assert Calendar.ISO.parse_time("23:50:07", :extended) == {:ok, {23, 50, 7, {0, 0}}}
    end

    test "errors on other format names" do
      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_time("235007", :other)
      end

      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_time("23:50:07", :other)
      end
    end
  end

  describe "parse_naive_datetime/1" do
    test "rejects strings with formatting errors" do
      assert Calendar.ISO.parse_naive_datetime("2015:01:23 23-50-07") == {:error, :invalid_format}
      assert Calendar.ISO.parse_naive_datetime("2015-01-23P23:50:07") == {:error, :invalid_format}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07A") ==
               {:error, :invalid_format}
    end

    test "recognizes invalid dates and times" do
      assert Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:61") == {:error, :invalid_time}
      assert Calendar.ISO.parse_naive_datetime("2015-01-32 23:50:07") == {:error, :invalid_date}
    end

    test "ignores offset data but requires valid ones" do
      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.123+02:30") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.123+00:00") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.123-02:30") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.123-00:00") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.123-00:60") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07.123-24:00") ==
               {:error, :invalid_format}
    end

    test "supports both spaces and 'T' as datetime separators" do
      assert Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23T23:50:07") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}}
    end

    test "supports only extended format by default" do
      assert Calendar.ISO.parse_naive_datetime("20150123 235007.123") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.123") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}}
    end

    test "errors on mixed basic and extended formats" do
      assert Calendar.ISO.parse_naive_datetime("20150123 23:50:07.123") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23 235007.123") ==
               {:error, :invalid_format}
    end
  end

  describe "parse_naive_datetime/2" do
    test "allows enforcing basic formats" do
      assert Calendar.ISO.parse_naive_datetime("20150123 235007.123", :basic) ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.123", :basic) ==
               {:error, :invalid_format}
    end

    test "allows enforcing extended formats" do
      assert Calendar.ISO.parse_naive_datetime("20150123 235007.123", :extended) ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.123", :extended) ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}}
    end

    test "errors on other format names" do
      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_naive_datetime("20150123 235007.123", :other)
      end

      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.123", :other)
      end
    end
  end

  describe "parse_utc_datetime/1" do
    test "rejects strings with formatting errors" do
      assert Calendar.ISO.parse_utc_datetime("2015:01:23 23-50-07Z") == {:error, :invalid_format}
      assert Calendar.ISO.parse_utc_datetime("2015-01-23P23:50:07Z") == {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07A") == {:error, :invalid_format}
    end

    test "recognizes invalid dates, times, and offsets" do
      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07") == {:error, :missing_offset}
      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:61Z") == {:error, :invalid_time}
      assert Calendar.ISO.parse_utc_datetime("2015-01-32 23:50:07Z") == {:error, :invalid_date}
    end

    test "interprets offset data" do
      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.123+02:30") ==
               {:ok, {2015, 1, 23, 21, 20, 7, {123_000, 3}}, 9000}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.123+00:00") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}, 0}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.123-02:30") ==
               {:ok, {2015, 1, 24, 2, 20, 7, {123_000, 3}}, -9000}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.123-00:00") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.123-00:60") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07.123-24:00") ==
               {:error, :invalid_format}
    end

    test "supports both spaces and 'T' as datetime separators" do
      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07Z") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}, 0}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23T23:50:07Z") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {0, 0}}, 0}
    end

    test "supports only extended format by default" do
      assert Calendar.ISO.parse_utc_datetime("20150123 235007.123Z") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07.123Z") ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}, 0}
    end

    test "errors on mixed basic and extended formats" do
      assert Calendar.ISO.parse_utc_datetime("20150123 23:50:07.123Z") ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 235007.123Z") ==
               {:error, :invalid_format}
    end
  end

  describe "parse_utc_datetime/2" do
    test "allows enforcing basic formats" do
      assert Calendar.ISO.parse_utc_datetime("20150123 235007.123Z", :basic) ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}, 0}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07.123Z", :basic) ==
               {:error, :invalid_format}
    end

    test "allows enforcing extended formats" do
      assert Calendar.ISO.parse_utc_datetime("20150123 235007.123Z", :extended) ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 23:50:07.123Z", :extended) ==
               {:ok, {2015, 1, 23, 23, 50, 7, {123_000, 3}}, 0}
    end

    test "errors on other format names" do
      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_naive_datetime("20150123 235007.123Z", :other)
      end

      assert_raise FunctionClauseError, fn ->
        Calendar.ISO.parse_naive_datetime("2015-01-23 23:50:07.123Z", :other)
      end
    end

    test "errors on mixed basic and extended formats" do
      assert Calendar.ISO.parse_utc_datetime("20150123 23:50:07.123Z", :basic) ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("20150123 23:50:07.123Z", :extended) ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 235007.123Z", :basic) ==
               {:error, :invalid_format}

      assert Calendar.ISO.parse_utc_datetime("2015-01-23 235007.123Z", :extended) ==
               {:error, :invalid_format}
    end
  end

  test "shift_date/2" do
    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!([])) == {2024, 3, 2}
    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!(year: 1)) == {2025, 3, 2}
    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!(month: 2)) == {2024, 5, 2}
    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!(week: 3)) == {2024, 3, 23}
    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!(day: 5)) == {2024, 3, 7}

    assert Calendar.ISO.shift_date(0, 1, 1, Duration.new!(month: 1)) == {0, 2, 1}
    assert Calendar.ISO.shift_date(0, 1, 1, Duration.new!(year: 1)) == {1, 1, 1}
    assert Calendar.ISO.shift_date(0, 1, 1, Duration.new!(year: -2, month: 2)) == {-2, 3, 1}
    assert Calendar.ISO.shift_date(-4, 1, 1, Duration.new!(year: -1)) == {-5, 1, 1}

    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!(year: 1, month: 2, week: 3, day: 5)) ==
             {2025, 5, 28}

    assert Calendar.ISO.shift_date(2024, 3, 2, Duration.new!(year: -1, month: -2, week: -3)) ==
             {2022, 12, 12}

    assert Calendar.ISO.shift_date(2020, 2, 28, Duration.new!(day: 1)) == {2020, 2, 29}
    assert Calendar.ISO.shift_date(2020, 2, 29, Duration.new!(year: 1)) == {2021, 2, 28}
    assert Calendar.ISO.shift_date(2024, 3, 31, Duration.new!(month: -1)) == {2024, 2, 29}
    assert Calendar.ISO.shift_date(2024, 3, 31, Duration.new!(month: -2)) == {2024, 1, 31}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 1)) == {2024, 2, 29}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 2)) == {2024, 3, 31}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 3)) == {2024, 4, 30}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 4)) == {2024, 5, 31}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 5)) == {2024, 6, 30}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 6)) == {2024, 7, 31}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 7)) == {2024, 8, 31}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 8)) == {2024, 9, 30}
    assert Calendar.ISO.shift_date(2024, 1, 31, Duration.new!(month: 9)) == {2024, 10, 31}
  end

  test "shift_naive_datetime/2" do
    assert Calendar.ISO.shift_naive_datetime(
             2024,
             3,
             2,
             0,
             0,
             0,
             {0, 0},
             Duration.new!([])
           ) == {2024, 3, 2, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(year: 1)
           ) == {2001, 1, 1, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: 1)
           ) == {2000, 2, 1, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: 1, day: 28)
           ) == {2000, 2, 29, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: 1, day: 30)
           ) == {2000, 3, 2, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: 2, day: 29)
           ) == {2000, 3, 30, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             2,
             29,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(year: -1)
           ) == {1999, 2, 28, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             2,
             29,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: -1)
           ) == {2000, 1, 29, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             2,
             29,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: -1, day: -28)
           ) == {2000, 1, 1, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             2,
             29,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: -1, day: -30)
           ) == {1999, 12, 30, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             2,
             29,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(month: -1, day: -29)
           ) == {1999, 12, 31, 0, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(hour: 12)
           ) == {2000, 1, 1, 12, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_naive_datetime(
             2000,
             1,
             1,
             0,
             0,
             0,
             {0, 0},
             Duration.new!(minute: -65)
           ) == {1999, 12, 31, 22, 55, 0, {0, 0}}
  end

  test "shift_time/2" do
    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(hour: 1)) == {1, 0, 0, {0, 0}}
    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(hour: -1)) == {23, 0, 0, {0, 0}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(minute: 30)) ==
             {0, 30, 0, {0, 0}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(minute: -30)) ==
             {23, 30, 0, {0, 0}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(second: 30)) ==
             {0, 0, 30, {0, 0}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(second: -30)) ==
             {23, 59, 30, {0, 0}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(microsecond: {100, 6})) ==
             {0, 0, 0, {100, 6}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(microsecond: {-100, 6})) ==
             {23, 59, 59, {999_900, 6}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(microsecond: {2000, 4})) ==
             {0, 0, 0, {2000, 4}}

    assert Calendar.ISO.shift_time(0, 0, 0, {0, 0}, Duration.new!(microsecond: {-2000, 4})) ==
             {23, 59, 59, {998_000, 4}}

    assert Calendar.ISO.shift_time(0, 0, 0, {3500, 6}, Duration.new!(microsecond: {-2000, 4})) ==
             {0, 0, 0, {1500, 4}}

    assert Calendar.ISO.shift_time(0, 0, 0, {3500, 4}, Duration.new!(minute: 5)) ==
             {0, 5, 0, {3500, 4}}

    assert Calendar.ISO.shift_time(0, 0, 0, {3500, 6}, Duration.new!(hour: 4)) ==
             {4, 0, 0, {3500, 6}}

    assert Calendar.ISO.shift_time(
             23,
             59,
             59,
             {999_900, 6},
             Duration.new!(hour: 4, microsecond: {100, 6})
           ) == {4, 0, 0, {0, 6}}
  end
end
