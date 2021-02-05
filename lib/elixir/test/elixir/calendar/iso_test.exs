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

  describe "day_of_week/3" do
    test "raises with invalid dates" do
      assert_raise ArgumentError, "invalid date: 2018-02-30", fn ->
        Calendar.ISO.day_of_week(2018, 2, 30)
      end

      assert_raise ArgumentError, "invalid date: 2017-11-00", fn ->
        Calendar.ISO.day_of_week(2017, 11, 0)
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

  test "year_of_era/1" do
    assert Calendar.ISO.year_of_era(-9999) == {10000, 0}
    assert Calendar.ISO.year_of_era(-1) == {2, 0}
    assert Calendar.ISO.year_of_era(0) == {1, 0}
    assert Calendar.ISO.year_of_era(1) == {1, 1}
    assert Calendar.ISO.year_of_era(1984) == {1984, 1}

    random_positive_year = Enum.random(1..9999)
    assert Calendar.ISO.year_of_era(random_positive_year) == {random_positive_year, 1}

    assert_raise FunctionClauseError, fn ->
      Calendar.ISO.year_of_era(10000)
    end

    assert_raise FunctionClauseError, fn ->
      Calendar.ISO.year_of_era(-10000)
    end
  end

  defp iso_day_roundtrip(year, month, day) do
    iso_days = Calendar.ISO.date_to_iso_days(year, month, day)
    Calendar.ISO.date_from_iso_days(iso_days)
  end
end
