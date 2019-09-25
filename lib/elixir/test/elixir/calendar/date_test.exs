Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)
Code.require_file("fakes.exs", __DIR__)

defmodule DateTest do
  use ExUnit.Case, async: true
  doctest Date

  test "sigil_D" do
    assert ~D[2000-01-01] ==
             %Date{calendar: Calendar.ISO, year: 2000, month: 1, day: 1}

    assert ~D[2000-01-01 Calendar.Holocene] ==
             %Date{calendar: Calendar.Holocene, year: 2000, month: 1, day: 1}

    assert ~D[10001-01-01 Calendar.Holocene] ==
             %Date{calendar: Calendar.Holocene, year: 10001, month: 1, day: 1}

    assert_raise ArgumentError,
                 ~s/cannot parse "2000-50-50" as Date for Calendar.ISO, reason: :invalid_date/,
                 fn -> Code.eval_string("~D[2000-50-50]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2000-50-50 notalias" as Date for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~D[2000-50-50 notalias]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2000-50-50" as Date for Calendar.Holocene, reason: :invalid_date/,
                 fn -> Code.eval_string("~D[2000-50-50 Calendar.Holocene]") end

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
  end

  test "inspect/1" do
    assert inspect(~D[2000-01-01]) == "~D[2000-01-01]"
    assert inspect(~D[-0100-12-31]) == "~D[-0100-12-31]"

    date = %{~D[2000-01-01] | calendar: FakeCalendar}
    assert inspect(date) == "~D[1/1/2000 FakeCalendar]"
  end

  test "inspect/1 roundtrip to to_string/1" do
    test_string = "~D[2000-01-01 Calendar.Holocene]"
    {date, []} = Code.eval_string(test_string)
    assert inspect(date) == test_string
  end

  test "compare/2" do
    date1 = ~D[-0001-12-30]
    date2 = ~D[-0001-12-31]
    date3 = ~D[0001-01-01]
    assert Date.compare(date1, date1) == :eq
    assert Date.compare(date1, date2) == :lt
    assert Date.compare(date2, date1) == :gt
    assert Date.compare(date3, date3) == :eq
    assert Date.compare(date2, date3) == :lt
    assert Date.compare(date3, date2) == :gt
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
    assert Date.day_of_week(~D[2016-10-31]) == 1
    assert Date.day_of_week(~D[2016-11-01]) == 2
    assert Date.day_of_week(~D[2016-11-02]) == 3
    assert Date.day_of_week(~D[2016-11-03]) == 4
    assert Date.day_of_week(~D[2016-11-04]) == 5
    assert Date.day_of_week(~D[2016-11-05]) == 6
    assert Date.day_of_week(~D[2016-11-06]) == 7
    assert Date.day_of_week(~D[2016-11-07]) == 1
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

    assert_raise FunctionClauseError, fn ->
      Date.add(~D[0000-01-01], 3_652_425)
    end

    assert Date.add(~D[0000-01-01], -1) == ~D[-0001-12-31]
    assert Date.add(~D[0000-01-01], -365) == ~D[-0001-01-01]
    assert Date.add(~D[0000-01-01], -366) == ~D[-0002-12-31]
    assert Date.add(~D[0000-01-01], -(365 * 4)) == ~D[-0004-01-02]
    assert Date.add(~D[0000-01-01], -(365 * 5)) == ~D[-0005-01-02]
    assert Date.add(~D[0000-01-01], -(365 * 100)) == ~D[-0100-01-25]
    assert Date.add(~D[0000-01-01], -3_652_059) == ~D[-9999-01-01]

    assert_raise FunctionClauseError, fn ->
      Date.add(~D[0000-01-01], -3_652_060)
    end
  end

  test "diff/2" do
    assert Date.diff(~D[2000-01-31], ~D[2000-01-01]) == 30
    assert Date.diff(~D[2000-01-01], ~D[2000-01-31]) == -30

    assert Date.diff(~D[0000-01-01], ~D[-0001-01-01]) == 365
    assert Date.diff(~D[-0003-01-01], ~D[-0004-01-01]) == 366

    date1 = ~D[2000-01-01]
    date2 = Calendar.Holocene.date(12000, 01, 14)
    assert Date.diff(date1, date2) == -13
    assert Date.diff(date2, date1) == 13
  end
end
