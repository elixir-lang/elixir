Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)

defmodule Date.RangeTest do
  use ExUnit.Case, async: true

  @asc_range Date.range(~D[2000-01-01], ~D[2001-01-01])
  @asc_range_2 Date.range(~D[2000-01-01], ~D[2001-01-01], 2)
  @desc_range Date.range(~D[2001-01-01], ~D[2000-01-01], -1)
  @desc_range_2 Date.range(~D[2001-01-01], ~D[2000-01-01], -2)
  @empty_range Date.range(~D[2001-01-01], ~D[2000-01-01], 1)

  describe "Enum.member?/2" do
    test "for ascending range" do
      assert Enum.member?(@asc_range, ~D[2000-02-22])
      assert Enum.member?(@asc_range, ~D[2000-01-01])
      assert Enum.member?(@asc_range, ~D[2001-01-01])
      refute Enum.member?(@asc_range, ~D[2002-01-01])
      refute Enum.member?(@asc_range, Calendar.Holocene.date(12000, 1, 1))

      assert Enum.member?(@asc_range_2, ~D[2000-01-03])
      refute Enum.member?(@asc_range_2, ~D[2000-01-02])
    end

    test "for descending range" do
      assert Enum.member?(@desc_range, ~D[2000-02-22])
      assert Enum.member?(@desc_range, ~D[2000-01-01])
      assert Enum.member?(@desc_range, ~D[2001-01-01])
      refute Enum.member?(@desc_range, ~D[1999-01-01])
      refute Enum.member?(@desc_range, Calendar.Holocene.date(12000, 1, 1))

      assert Enum.member?(@desc_range_2, ~D[2000-12-30])
      refute Enum.member?(@desc_range_2, ~D[2000-12-29])
    end

    test "empty range" do
      refute Enum.member?(@empty_range, @empty_range.first)
    end
  end

  describe "Enum.count/1" do
    test "for ascending range" do
      assert Enum.count(@asc_range) == 367
      assert Enum.count(@asc_range_2) == 184
    end

    test "for descending range" do
      assert Enum.count(@desc_range) == 367
      assert Enum.count(@desc_range_2) == 184
    end

    test "for empty range" do
      assert Enum.count(@empty_range) == 0
    end
  end

  describe "Enum.slice/3" do
    test "for ascending range" do
      assert Enum.slice(@asc_range, 3, 3) == [~D[2000-01-04], ~D[2000-01-05], ~D[2000-01-06]]
      assert Enum.slice(@asc_range, -3, 3) == [~D[2000-12-30], ~D[2000-12-31], ~D[2001-01-01]]

      assert Enum.slice(@asc_range_2, 3, 3) == [~D[2000-01-07], ~D[2000-01-09], ~D[2000-01-11]]
      assert Enum.slice(@asc_range_2, -3, 3) == [~D[2000-12-28], ~D[2000-12-30], ~D[2001-01-01]]
    end

    test "for descending range" do
      assert Enum.slice(@desc_range, 3, 3) == [~D[2000-12-29], ~D[2000-12-28], ~D[2000-12-27]]
      assert Enum.slice(@desc_range, -3, 3) == [~D[2000-01-03], ~D[2000-01-02], ~D[2000-01-01]]

      assert Enum.slice(@desc_range_2, 3, 3) == [~D[2000-12-26], ~D[2000-12-24], ~D[2000-12-22]]
      assert Enum.slice(@desc_range_2, -3, 3) == [~D[2000-01-05], ~D[2000-01-03], ~D[2000-01-01]]
    end

    test "for empty range" do
      assert Enum.slice(@empty_range, 1, 3) == []
      assert Enum.slice(@empty_range, 3, 3) == []
      assert Enum.slice(@empty_range, -1, 3) == []
      assert Enum.slice(@empty_range, -3, 3) == []
    end
  end

  describe "Enum.reduce/3" do
    test "for ascending range" do
      assert Enum.take(@asc_range, 3) == [~D[2000-01-01], ~D[2000-01-02], ~D[2000-01-03]]

      assert Enum.take(@asc_range_2, 3) == [~D[2000-01-01], ~D[2000-01-03], ~D[2000-01-05]]
    end

    test "for descending range" do
      assert Enum.take(@desc_range, 3) == [~D[2001-01-01], ~D[2000-12-31], ~D[2000-12-30]]

      assert Enum.take(@desc_range_2, 3) == [~D[2001-01-01], ~D[2000-12-30], ~D[2000-12-28]]
    end

    test "for empty range" do
      assert Enum.take(@empty_range, 3) == []
    end
  end

  test "works with date-like structs" do
    range = Date.range(~N[2000-01-01 09:00:00], ~U[2000-01-02 09:00:00Z])
    assert range.first == ~D[2000-01-01]
    assert range.last == ~D[2000-01-02]
    assert Enum.to_list(range) == [~D[2000-01-01], ~D[2000-01-02]]

    range = Date.range(~N[2000-01-01 09:00:00], ~U[2000-01-03 09:00:00Z], 2)
    assert range.first == ~D[2000-01-01]
    assert range.last == ~D[2000-01-03]
    assert Enum.to_list(range) == [~D[2000-01-01], ~D[2000-01-03]]
  end

  test "both dates must have matching calendars" do
    first = ~D[2000-01-01]
    last = Calendar.Holocene.date(12001, 1, 1)

    assert_raise ArgumentError, "both dates must have matching calendars", fn ->
      Date.range(first, last)
    end
  end

  test "accepts equal but non Calendar.ISO calendars" do
    first = Calendar.Holocene.date(12000, 1, 1)
    last = Calendar.Holocene.date(12001, 1, 1)
    range = Date.range(first, last)
    assert range
    assert first in range
    assert last in range
    assert Enum.count(range) == 367
  end

  test "step is a non-zero integer" do
    step = 1.0
    message = ~r"the step must be a non-zero integer"

    assert_raise ArgumentError, message, fn ->
      Date.range(~D[2000-01-01], ~D[2000-01-31], step)
    end

    step = 0
    message = ~r"the step must be a non-zero integer"

    assert_raise ArgumentError, message, fn ->
      Date.range(~D[2000-01-01], ~D[2000-01-31], step)
    end
  end

  describe "old date ranges" do
    test "inspect" do
      asc = %{
        __struct__: Date.Range,
        first: ~D[2021-07-14],
        first_in_iso_days: 738_350,
        last: ~D[2021-07-17],
        last_in_iso_days: 738_353
      }

      desc = %{
        __struct__: Date.Range,
        first: ~D[2021-07-17],
        first_in_iso_days: 738_353,
        last: ~D[2021-07-14],
        last_in_iso_days: 738_350
      }

      assert inspect(asc) == "Date.range(~D[2021-07-14], ~D[2021-07-17])"
      assert inspect(desc) == "Date.range(~D[2021-07-17], ~D[2021-07-14], -1)"
    end

    test "enumerable" do
      asc = %{
        __struct__: Date.Range,
        first: ~D[2021-07-14],
        first_in_iso_days: 738_350,
        last: ~D[2021-07-17],
        last_in_iso_days: 738_353
      }

      desc = %{
        __struct__: Date.Range,
        first: ~D[2021-07-17],
        first_in_iso_days: 738_353,
        last: ~D[2021-07-14],
        last_in_iso_days: 738_350
      }

      # member? implementations tests also empty?
      assert Enumerable.member?(asc, ~D[2021-07-15])
      assert {:ok, 4, _} = Enumerable.slice(asc)

      assert Enum.reduce(asc, [], fn x, acc -> [x | acc] end) == [
               ~D[2021-07-17],
               ~D[2021-07-16],
               ~D[2021-07-15],
               ~D[2021-07-14]
             ]

      assert Enum.count(asc) == 4

      # member? implementations tests also empty?
      assert Enumerable.member?(desc, ~D[2021-07-15])
      assert {:ok, 4, _} = Enumerable.slice(desc)

      assert Enum.reduce(desc, [], fn x, acc -> [x | acc] end) == [
               ~D[2021-07-14],
               ~D[2021-07-15],
               ~D[2021-07-16],
               ~D[2021-07-17]
             ]

      assert Enum.count(desc) == 4
    end
  end
end
