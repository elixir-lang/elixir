Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)
Code.require_file("fakes.exs", __DIR__)

defmodule DateTest do
  use ExUnit.Case, async: true
  doctest Date

  test "sigil_D" do
    assert ~D[2000-01-01] ==
             %Date{calendar: Calendar.ISO, year: 2000, month: 1, day: 1}

    assert ~D[20001-01-01 Calendar.Holocene] ==
             %Date{calendar: Calendar.Holocene, year: 20001, month: 1, day: 1}

    assert_raise ArgumentError,
                 ~s/cannot parse "2000-50-50" as Date for Calendar.ISO, reason: :invalid_date/,
                 fn -> Code.eval_string("~D[2000-50-50]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2000-04-15 notalias" as Date for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~D[2000-04-15 notalias]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "20010415" as Date for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string(~s{~D[20010415]}) end

    assert_raise ArgumentError,
                 ~s/cannot parse "20001-50-50" as Date for Calendar.Holocene, reason: :invalid_date/,
                 fn -> Code.eval_string("~D[20001-50-50 Calendar.Holocene]") end

    assert_raise UndefinedFunctionError, fn ->
      Code.eval_string("~D[2000-01-01 UnknownCalendar]")
    end
  end

  test "to_string/1" do
    date = ~D[2000-01-01]
    assert to_string(date) == "2000-01-01"
    assert Date.to_string(date) == "2000-01-01"
    assert Date.to_string(Map.from_struct(date)) == "2000-01-01"

    assert to_string(%{date | calendar: FakeCalendar}) == "1/1/2000"
    assert Date.to_string(%{date | calendar: FakeCalendar}) == "1/1/2000"

    date2 = Date.new!(5_874_897, 12, 31)
    assert to_string(date2) == "5874897-12-31"
    assert Date.to_string(date2) == "5874897-12-31"
    assert Date.to_string(Map.from_struct(date2)) == "5874897-12-31"

    assert to_string(%{date2 | calendar: FakeCalendar}) == "31/12/5874897"
    assert Date.to_string(%{date2 | calendar: FakeCalendar}) == "31/12/5874897"
  end

  test "inspect/1" do
    assert inspect(~D[2000-01-01]) == "~D[2000-01-01]"
    assert inspect(~D[-0100-12-31]) == "~D[-0100-12-31]"

    date = %{~D[2000-01-01] | calendar: FakeCalendar}
    assert inspect(date) == "~D[1/1/2000 FakeCalendar]"

    assert inspect(Date.new!(99999, 12, 31)) == "Date.new!(99999, 12, 31)"
    assert inspect(Date.new!(-99999, 1, 1)) == "Date.new!(-99999, 1, 1)"

    date2 = %{Date.new!(99999, 12, 31) | calendar: FakeCalendar}

    assert inspect(%{date2 | calendar: FakeCalendar}) ==
             "~D[31/12/99999 FakeCalendar]"
  end

  test "compare/2" do
    date1 = ~D[-0001-12-30]
    date2 = ~D[-0001-12-31]
    date3 = ~D[0001-01-01]
    date4 = Date.new!(5_874_897, 12, 31)
    date5 = Date.new!(-4713, 1, 1)

    assert Date.compare(date1, date1) == :eq
    assert Date.compare(date1, date2) == :lt
    assert Date.compare(date2, date1) == :gt
    assert Date.compare(date3, date3) == :eq
    assert Date.compare(date2, date3) == :lt
    assert Date.compare(date3, date2) == :gt
    assert Date.compare(date4, date1) == :gt
    assert Date.compare(date1, date4) == :lt
    assert Date.compare(date4, date4) == :eq
    assert Date.compare(date4, date5) == :gt
    assert Date.compare(date5, date4) == :lt
    assert Date.compare(date5, date5) == :eq
  end

  test "before?/2 and after?/2" do
    date1 = ~D[2022-11-01]
    date2 = ~D[2022-11-02]
    date3 = Date.new!(5_874_897, 12, 31)
    date4 = Date.new!(-4713, 1, 1)

    assert Date.before?(date1, date2)
    assert Date.before?(date1, date3)
    assert Date.before?(date4, date1)
    assert not Date.before?(date2, date1)
    assert not Date.before?(date3, date1)
    assert not Date.before?(date1, date4)

    assert Date.after?(date2, date1)
    assert Date.after?(date3, date2)
    assert Date.after?(date2, date4)
    assert not Date.after?(date1, date2)
    assert not Date.after?(date2, date3)
    assert not Date.after?(date4, date2)
  end

  test "compare/2 across calendars" do
    date1 = ~D[2000-01-01]
    date2 = Calendar.Holocene.date(12000, 01, 01)
    assert Date.compare(date1, date2) == :eq

    date2 = Calendar.Holocene.date(12001, 01, 01)
    assert Date.compare(date1, date2) == :lt
    assert Date.compare(date2, date1) == :gt
  end

  test "day_of_week/1" do
    assert Date.day_of_week(Calendar.Holocene.date(2016, 10, 31)) == 1
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 01)) == 2
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 02)) == 3
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 03)) == 4
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 04)) == 5
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 05)) == 6
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 06)) == 7
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 07)) == 1

    assert Date.day_of_week(Calendar.Holocene.date(2016, 10, 30), :sunday) == 1
    assert Date.day_of_week(Calendar.Holocene.date(2016, 10, 31), :sunday) == 2
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 01), :sunday) == 3
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 02), :sunday) == 4
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 03), :sunday) == 5
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 04), :sunday) == 6
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 05), :sunday) == 7
    assert Date.day_of_week(Calendar.Holocene.date(2016, 11, 06), :sunday) == 1
  end

  test "beginning_of_week" do
    assert Date.beginning_of_week(Calendar.Holocene.date(2020, 07, 11)) ==
             Calendar.Holocene.date(2020, 07, 06)

    assert Date.beginning_of_week(Calendar.Holocene.date(2020, 07, 06)) ==
             Calendar.Holocene.date(2020, 07, 06)

    assert Date.beginning_of_week(Calendar.Holocene.date(2020, 07, 11), :sunday) ==
             Calendar.Holocene.date(2020, 07, 05)

    assert Date.beginning_of_week(Calendar.Holocene.date(2020, 07, 11), :saturday) ==
             Calendar.Holocene.date(2020, 07, 11)
  end

  test "end_of_week" do
    assert Date.end_of_week(Calendar.Holocene.date(2020, 07, 11)) ==
             Calendar.Holocene.date(2020, 07, 12)

    assert Date.end_of_week(Calendar.Holocene.date(2020, 07, 05)) ==
             Calendar.Holocene.date(2020, 07, 05)

    assert Date.end_of_week(Calendar.Holocene.date(2020, 07, 05), :sunday) ==
             Calendar.Holocene.date(2020, 07, 11)

    assert Date.end_of_week(Calendar.Holocene.date(2020, 07, 05), :saturday) ==
             Calendar.Holocene.date(2020, 07, 10)
  end

  test "convert/2" do
    assert Date.convert(~D[2000-01-01], Calendar.Holocene) ==
             {:ok, Calendar.Holocene.date(12000, 01, 01)}

    assert ~D[2000-01-01]
           |> Date.convert!(Calendar.Holocene)
           |> Date.convert!(Calendar.ISO) == ~D[2000-01-01]

    assert Date.convert(~D[2016-02-03], FakeCalendar) == {:error, :incompatible_calendars}

    assert Date.convert(~N[2000-01-01 00:00:00], Calendar.Holocene) ==
             {:ok, Calendar.Holocene.date(12000, 01, 01)}
  end

  test "add/2" do
    assert Date.add(~D[0000-01-01], 3_652_424) == ~D[9999-12-31]
    assert Date.add(~D[0000-01-01], 3_652_425) == Date.new!(10000, 1, 1)
    assert Date.add(~D[0000-01-01], -1) == ~D[-0001-12-31]
    assert Date.add(~D[0000-01-01], -365) == ~D[-0001-01-01]
    assert Date.add(~D[0000-01-01], -366) == ~D[-0002-12-31]
    assert Date.add(~D[0000-01-01], -(365 * 4)) == ~D[-0004-01-02]
    assert Date.add(~D[0000-01-01], -(365 * 5)) == ~D[-0005-01-02]
    assert Date.add(~D[0000-01-01], -(365 * 100)) == ~D[-0100-01-25]
    assert Date.add(~D[0000-01-01], -3_652_059) == ~D[-9999-01-01]
    assert Date.add(~D[0000-01-01], -3_652_060) == Date.new!(-10000, 12, 31)
    assert Date.add(Date.new!(5_874_897, 12, 31), 1) == Date.new!(5_874_898, 1, 1)
  end

  test "diff/2" do
    assert Date.diff(~D[2000-01-31], ~D[2000-01-01]) == 30
    assert Date.diff(~D[2000-01-01], ~D[2000-01-31]) == -30

    assert Date.diff(~D[0000-01-01], ~D[-0001-01-01]) == 365
    assert Date.diff(~D[-0003-01-01], ~D[-0004-01-01]) == 366

    assert Date.diff(Date.new!(5_874_898, 1, 1), Date.new!(5_874_897, 1, 1)) == 365
    assert Date.diff(Date.new!(5_874_905, 1, 1), Date.new!(5_874_904, 1, 1)) == 366

    date1 = ~D[2000-01-01]
    date2 = Calendar.Holocene.date(12000, 01, 14)
    assert Date.diff(date1, date2) == -13
    assert Date.diff(date2, date1) == 13
  end

  test "shift/2" do
    assert Date.shift(~D[2012-02-29], day: -1) == ~D[2012-02-28]
    assert Date.shift(~D[2012-02-29], month: -1) == ~D[2012-01-29]
    assert Date.shift(~D[2012-02-29], week: -9) == ~D[2011-12-28]
    assert Date.shift(~D[2012-02-29], month: 1) == ~D[2012-03-29]
    assert Date.shift(~D[2012-02-29], year: -1) == ~D[2011-02-28]
    assert Date.shift(~D[2012-02-29], year: 4) == ~D[2016-02-29]
    assert Date.shift(~D[0000-01-01], day: -1) == ~D[-0001-12-31]
    assert Date.shift(~D[0000-01-01], month: -1) == ~D[-0001-12-01]
    assert Date.shift(~D[0000-01-01], year: -1) == ~D[-0001-01-01]
    assert Date.shift(~D[0000-01-01], year: -1) == ~D[-0001-01-01]
    assert Date.shift(~D[2000-01-01], month: 12) == ~D[2001-01-01]
    assert Date.shift(~D[0000-01-01], day: 2, year: 1, month: 37) == ~D[0004-02-03]

    assert Date.shift(Date.new!(5_874_904, 2, 29), day: -1) == Date.new!(5_874_904, 2, 28)
    assert Date.shift(Date.new!(5_874_904, 2, 29), month: -2) == Date.new!(5_874_903, 12, 29)
    assert Date.shift(Date.new!(5_874_904, 2, 29), week: -9) == Date.new!(5_874_903, 12, 28)
    assert Date.shift(Date.new!(5_874_904, 2, 29), month: 1) == Date.new!(5_874_904, 3, 29)
    assert Date.shift(Date.new!(5_874_904, 2, 29), year: -1) == Date.new!(5_874_903, 2, 28)
    assert Date.shift(Date.new!(5_874_904, 2, 29), year: -4) == Date.new!(5_874_900, 2, 28)
    assert Date.shift(Date.new!(5_874_904, 2, 29), year: 4) == Date.new!(5_874_908, 2, 29)

    assert Date.shift(Date.new!(5_874_904, 2, 29), day: 1, year: 4, month: 2) ==
             Date.new!(5_874_908, 4, 30)

    assert_raise ArgumentError,
                 "unsupported unit :second. Expected :year, :month, :week, :day",
                 fn -> Date.shift(~D[2012-02-29], second: 86400) end

    assert_raise ArgumentError,
                 "unknown unit :months. Expected :year, :month, :week, :day",
                 fn -> Date.shift(~D[2012-01-01], months: 12) end

    assert_raise ArgumentError,
                 "cannot shift date by time scale unit. Expected :year, :month, :week, :day",
                 fn -> Date.shift(~D[2012-02-29], %Duration{second: 86400}) end

    # Implements calendar callback
    assert_raise RuntimeError, "shift_date/4 not implemented", fn ->
      date = Calendar.Holocene.date(10000, 01, 01)
      Date.shift(date, month: 1)
    end
  end
end
