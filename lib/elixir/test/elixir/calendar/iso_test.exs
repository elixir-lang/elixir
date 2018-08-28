Code.require_file("../test_helper.exs", __DIR__)

defmodule Calendar.ISOTest do
  use ExUnit.Case, async: true
  doctest Calendar.ISO

  describe "date_from_iso_days" do
    test "positive dates" do
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

    test "negative dates" do
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

  describe "date_from_iso_days symptoms" do
    test "date add" do
      assert ~D[-0004-01-01] == Date.add(~D[-0004-01-01], 0)
    end

    test "naive_datetime_from_iso_days" do
      {iso_days, parts} = Calendar.ISO.naive_datetime_to_iso_days(-4, 1, 1, 0, 0, 0, {0, 6})
      naive_dt = Calendar.ISO.naive_datetime_from_iso_days({iso_days, parts})
      assert {-4, 1, 1, 0, 0, 0, {0, 6}} == naive_dt
    end
  end

  defp iso_day_roundtrip(year, month, day) do
    iso_days = Calendar.ISO.date_to_iso_days(year, month, day)
    Calendar.ISO.date_from_iso_days(iso_days)
  end
end
