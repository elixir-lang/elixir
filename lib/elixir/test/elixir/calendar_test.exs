Code.require_file("test_helper.exs", __DIR__)
Code.require_file("fixtures/calendar/holocene.exs", __DIR__)

defmodule FakeCalendar do
  def date_to_string(_, _, _), do: "boom"
  def time_to_string(_, _, _, _), do: "boom"
  def naive_datetime_to_string(_, _, _, _, _, _, _), do: "boom"
  def datetime_to_string(_, _, _, _, _, _, _, _, _, _), do: "boom"
  def day_rollover_relative_to_midnight_utc, do: {123_456, 123_457}
end

defmodule DateTest do
  use ExUnit.Case, async: true
  doctest Date

  test "to_string/1" do
    assert to_string(~D[2000-01-01]) == "2000-01-01"

    date = %{~D[2000-01-01] | calendar: FakeCalendar}
    assert to_string(date) == "boom"
  end

  test "inspect/1" do
    assert inspect(~D[2000-01-01]) == "~D[2000-01-01]"

    date = %{~D[2000-01-01] | calendar: FakeCalendar}
    assert inspect(date) == "%Date{calendar: FakeCalendar, day: 1, month: 1, year: 2000}"
  end

  test "compare/2" do
    date1 = ~D[2000-01-01]
    date2 = ~D[2000-01-02]
    assert Date.compare(date1, date1) == :eq
    assert Date.compare(date1, date2) == :lt
    assert Date.compare(date2, date1) == :gt
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
    assert_raise FunctionClauseError, fn ->
      Date.add(~D[0000-01-01], 3_652_425)
    end

    assert_raise FunctionClauseError, fn ->
      Date.add(~D[0000-01-01], -1)
    end
  end

  test "diff/2" do
    assert Date.diff(~D[2000-01-31], ~D[2000-01-01]) == 30
    assert Date.diff(~D[2000-01-01], ~D[2000-01-31]) == -30

    date1 = ~D[2000-01-01]
    date2 = Calendar.Holocene.date(12000, 01, 14)
    assert Date.diff(date1, date2) == -13
    assert Date.diff(date2, date1) == 13
  end
end

defmodule TimeTest do
  use ExUnit.Case, async: true
  doctest Time

  test "to_string/1" do
    assert to_string(~T[23:00:07.005]) == "23:00:07.005"
  end

  test "inspect/1" do
    assert inspect(~T[23:00:07.005]) == "~T[23:00:07.005]"
  end

  test "compare/2" do
    time0 = ~T[01:01:01.0]
    time1 = ~T[01:01:01.005]
    time2 = ~T[01:01:01.0050]
    time3 = ~T[23:01:01.0050]
    assert Time.compare(time0, time1) == :lt
    assert Time.compare(time1, time1) == :eq
    assert Time.compare(time1, time2) == :eq
    assert Time.compare(time1, time3) == :lt
    assert Time.compare(time3, time2) == :gt
  end

  test "truncate/2" do
    assert Time.truncate(~T[01:01:01.123456], :microsecond) == ~T[01:01:01.123456]

    assert Time.truncate(~T[01:01:01.0], :millisecond) == ~T[01:01:01.0]
    assert Time.truncate(~T[01:01:01.00], :millisecond) == ~T[01:01:01.00]
    assert Time.truncate(~T[01:01:01.1], :millisecond) == ~T[01:01:01.1]
    assert Time.truncate(~T[01:01:01.100], :millisecond) == ~T[01:01:01.100]
    assert Time.truncate(~T[01:01:01.999], :millisecond) == ~T[01:01:01.999]
    assert Time.truncate(~T[01:01:01.1000], :millisecond) == ~T[01:01:01.100]
    assert Time.truncate(~T[01:01:01.1001], :millisecond) == ~T[01:01:01.100]
    assert Time.truncate(~T[01:01:01.123456], :millisecond) == ~T[01:01:01.123]
    assert Time.truncate(~T[01:01:01.000123], :millisecond) == ~T[01:01:01.000]
    assert Time.truncate(~T[01:01:01.00012], :millisecond) == ~T[01:01:01.000]

    assert Time.truncate(~T[01:01:01.123456], :second) == ~T[01:01:01]
  end
end

defmodule NaiveDateTimeTest do
  use ExUnit.Case, async: true
  doctest NaiveDateTime

  test "to_string/1" do
    assert to_string(~N[2000-01-01 23:00:07.005]) == "2000-01-01 23:00:07.005"

    ndt = %{~N[2000-01-01 23:00:07.005] | calendar: FakeCalendar}
    assert to_string(ndt) == "boom"
  end

  test "inspect/1" do
    assert inspect(~N[2000-01-01 23:00:07.005]) == "~N[2000-01-01 23:00:07.005]"

    ndt = %{~N[2000-01-01 23:00:07.005] | calendar: FakeCalendar}

    assert inspect(ndt) ==
             "%NaiveDateTime{calendar: FakeCalendar, day: 1, hour: 23, " <>
               "microsecond: {5000, 3}, minute: 0, month: 1, second: 7, year: 2000}"
  end

  test "compare/2" do
    ndt1 = ~N[2000-04-16 13:30:15.0049]
    ndt2 = ~N[2000-04-16 13:30:15.0050]
    ndt3 = ~N[2001-04-16 13:30:15.0050]
    assert NaiveDateTime.compare(ndt1, ndt1) == :eq
    assert NaiveDateTime.compare(ndt1, ndt2) == :lt
    assert NaiveDateTime.compare(ndt2, ndt1) == :gt
    assert NaiveDateTime.compare(ndt3, ndt1) == :gt
    assert NaiveDateTime.compare(ndt3, ndt2) == :gt
  end

  test "to_iso8601/1" do
    ndt = ~N[2000-04-16 12:34:15.1234]
    ndt = put_in(ndt.calendar, FakeCalendar)

    message =
      "cannot convert #{inspect(ndt)} to target calendar Calendar.ISO, " <>
        "reason: #{inspect(ndt.calendar)} and Calendar.ISO have different day rollover moments, " <>
        "making this conversion ambiguous"

    assert_raise ArgumentError, message, fn ->
      NaiveDateTime.to_iso8601(ndt)
    end
  end

  test "add/2 with other calendars" do
    assert ~N[2000-01-01 12:34:15.123456]
           |> NaiveDateTime.convert!(Calendar.Holocene)
           |> NaiveDateTime.add(10, :second) ==
             %NaiveDateTime{
               calendar: Calendar.Holocene,
               year: 12000,
               month: 1,
               day: 1,
               hour: 12,
               minute: 34,
               second: 25,
               microsecond: {123_456, 6}
             }
  end

  test "diff/2 with other calendars" do
    assert ~N[2000-01-01 12:34:15.123456]
           |> NaiveDateTime.convert!(Calendar.Holocene)
           |> NaiveDateTime.add(10, :second)
           |> NaiveDateTime.diff(~N[2000-01-01 12:34:15.123456]) == 10
  end

  test "convert/2" do
    assert NaiveDateTime.convert(~N[2000-01-01 12:34:15.123400], Calendar.Holocene) ==
             {:ok, Calendar.Holocene.naive_datetime(12000, 1, 1, 12, 34, 15, {123_400, 6})}

    assert ~N[2000-01-01 12:34:15]
           |> NaiveDateTime.convert!(Calendar.Holocene)
           |> NaiveDateTime.convert!(Calendar.ISO) == ~N[2000-01-01 12:34:15]

    assert ~N[2000-01-01 12:34:15.123456]
           |> NaiveDateTime.convert!(Calendar.Holocene)
           |> NaiveDateTime.convert!(Calendar.ISO) == ~N[2000-01-01 12:34:15.123456]

    assert NaiveDateTime.convert(~N[2016-02-03 00:00:01], FakeCalendar) ==
             {:error, :incompatible_calendars}

    assert NaiveDateTime.convert(~N[1970-01-01 00:00:00], Calendar.Holocene) ==
             {:ok, Calendar.Holocene.naive_datetime(11970, 1, 1, 0, 0, 0, {0, 0})}

    assert NaiveDateTime.convert(DateTime.from_unix!(0, :seconds), Calendar.Holocene) ==
             {:ok, Calendar.Holocene.naive_datetime(11970, 1, 1, 0, 0, 0, {0, 0})}
  end

  test "truncate/2" do
    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.123456], :microsecond) ==
             ~N[2017-11-06 00:23:51.123456]

    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.0], :millisecond) ==
             ~N[2017-11-06 00:23:51.0]

    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.999], :millisecond) ==
             ~N[2017-11-06 00:23:51.999]

    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.1009], :millisecond) ==
             ~N[2017-11-06 00:23:51.100]

    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.123456], :millisecond) ==
             ~N[2017-11-06 00:23:51.123]

    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.000456], :millisecond) ==
             ~N[2017-11-06 00:23:51.000]

    assert NaiveDateTime.truncate(~N[2017-11-06 00:23:51.123456], :second) ==
             ~N[2017-11-06 00:23:51]
  end
end

defmodule DateTimeTest do
  use ExUnit.Case, async: true
  doctest DateTime

  test "to_string/1" do
    datetime = %DateTime{
      year: 2000,
      month: 2,
      day: 29,
      zone_abbr: "BRM",
      hour: 23,
      minute: 0,
      second: 7,
      microsecond: {0, 0},
      utc_offset: -12600,
      std_offset: 3600,
      time_zone: "Brazil/Manaus"
    }

    assert to_string(datetime) == "2000-02-29 23:00:07-02:30 BRM Brazil/Manaus"
  end

  test "from_iso8601/1 handles positive and negative offsets" do
    assert DateTime.from_iso8601("2015-01-24T09:50:07-10:00")
           |> elem(1) ==
             %DateTime{
               microsecond: {0, 0},
               month: 1,
               std_offset: 0,
               time_zone: "Etc/UTC",
               utc_offset: 0,
               year: 2015,
               zone_abbr: "UTC",
               day: 24,
               hour: 19,
               minute: 50,
               second: 7
             }

    assert DateTime.from_iso8601("2015-01-24T09:50:07+10:00")
           |> elem(1) ==
             %DateTime{
               microsecond: {0, 0},
               month: 1,
               std_offset: 0,
               time_zone: "Etc/UTC",
               utc_offset: 0,
               year: 2015,
               zone_abbr: "UTC",
               day: 23,
               hour: 23,
               minute: 50,
               second: 7
             }
  end

  test "from_unix/2" do
    # with Unix times back to 0 Gregorian seconds
    min_datetime = %DateTime{
      calendar: Calendar.ISO,
      day: 1,
      hour: 0,
      microsecond: {0, 0},
      minute: 0,
      month: 1,
      second: 0,
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 0,
      zone_abbr: "UTC"
    }

    assert DateTime.from_unix(-62_167_219_200) == {:ok, min_datetime}
    assert DateTime.from_unix(-62_167_219_201) == {:error, :invalid_unix_time}

    max_datetime = %DateTime{
      calendar: Calendar.ISO,
      day: 31,
      hour: 23,
      microsecond: {999_999, 6},
      minute: 59,
      month: 12,
      second: 59,
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 9999,
      zone_abbr: "UTC"
    }

    assert DateTime.from_unix(253_402_300_799_999_999, :microsecond) == {:ok, max_datetime}
    assert DateTime.from_unix(253_402_300_800) == {:error, :invalid_unix_time}

    minus_datetime = %DateTime{
      calendar: Calendar.ISO,
      day: 31,
      hour: 23,
      microsecond: {999_999, 6},
      minute: 59,
      month: 12,
      second: 59,
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 1969,
      zone_abbr: "UTC"
    }

    assert DateTime.from_unix(-1, :microsecond) == {:ok, minus_datetime}
  end

  test "from_unix!/2" do
    # with Unix times back to 0 Gregorian seconds
    datetime = %DateTime{
      calendar: Calendar.ISO,
      day: 1,
      hour: 0,
      microsecond: {0, 0},
      minute: 0,
      month: 1,
      second: 0,
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 0,
      zone_abbr: "UTC"
    }

    assert DateTime.from_unix!(-62_167_219_200) == datetime

    assert_raise ArgumentError, fn ->
      DateTime.from_unix!(-62_167_219_201)
    end
  end

  test "to_unix/2 works with Unix times back to 0 Gregorian seconds" do
    # with Unix times back to 0 Gregorian seconds
    gregorian_0 = %DateTime{
      calendar: Calendar.ISO,
      day: 1,
      hour: 0,
      microsecond: {0, 0},
      minute: 0,
      month: 1,
      second: 0,
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 0,
      zone_abbr: "UTC"
    }

    assert DateTime.to_unix(gregorian_0) == -62_167_219_200

    before_gregorian_0 = %DateTime{gregorian_0 | year: -1}

    assert_raise FunctionClauseError, fn ->
      DateTime.to_unix(before_gregorian_0)
    end
  end

  test "compare/2" do
    datetime1 = %DateTime{
      year: 2000,
      month: 2,
      day: 29,
      zone_abbr: "CET",
      hour: 23,
      minute: 0,
      second: 7,
      microsecond: {0, 0},
      utc_offset: 3600,
      std_offset: 0,
      time_zone: "Europe/Warsaw"
    }

    datetime2 = %DateTime{
      year: 2000,
      month: 2,
      day: 29,
      zone_abbr: "AMT",
      hour: 23,
      minute: 0,
      second: 7,
      microsecond: {0, 0},
      utc_offset: -14400,
      std_offset: 0,
      time_zone: "America/Manaus"
    }

    assert DateTime.compare(datetime1, datetime1) == :eq
    assert DateTime.compare(datetime1, datetime2) == :lt
    assert DateTime.compare(datetime2, datetime1) == :gt
  end

  test "convert/2" do
    datetime_iso = %DateTime{
      year: 2000,
      month: 2,
      day: 29,
      zone_abbr: "CET",
      hour: 23,
      minute: 0,
      second: 7,
      microsecond: {0, 0},
      utc_offset: 3600,
      std_offset: 0,
      time_zone: "Europe/Warsaw"
    }

    datetime_hol = %DateTime{
      year: 12000,
      month: 2,
      day: 29,
      zone_abbr: "CET",
      hour: 23,
      minute: 0,
      second: 7,
      microsecond: {0, 0},
      utc_offset: 3600,
      std_offset: 0,
      time_zone: "Europe/Warsaw",
      calendar: Calendar.Holocene
    }

    assert DateTime.convert(datetime_iso, Calendar.Holocene) == {:ok, datetime_hol}

    assert datetime_iso
           |> DateTime.convert!(Calendar.Holocene)
           |> DateTime.convert!(Calendar.ISO) == datetime_iso

    assert %{datetime_iso | microsecond: {123, 6}}
           |> DateTime.convert!(Calendar.Holocene)
           |> DateTime.convert!(Calendar.ISO) == %{datetime_iso | microsecond: {123, 6}}

    assert DateTime.convert(datetime_iso, FakeCalendar) == {:error, :incompatible_calendars}
  end

  test "from_iso8601/1 with tz offsets" do
    assert DateTime.from_iso8601("2017-06-02T14:00:00+01:00")
           |> elem(1) ==
             %DateTime{
               year: 2017,
               month: 6,
               day: 2,
               zone_abbr: "UTC",
               hour: 13,
               minute: 0,
               second: 0,
               microsecond: {0, 0},
               utc_offset: 0,
               std_offset: 0,
               time_zone: "Etc/UTC"
             }

    assert DateTime.from_iso8601("2017-06-02T14:00:00-04:00")
           |> elem(1) ==
             %DateTime{
               year: 2017,
               month: 6,
               day: 2,
               zone_abbr: "UTC",
               hour: 18,
               minute: 0,
               second: 0,
               microsecond: {0, 0},
               utc_offset: 0,
               std_offset: 0,
               time_zone: "Etc/UTC"
             }

    assert DateTime.from_iso8601("2017-06-02T14:00:00+0100")
           |> elem(1) ==
             %DateTime{
               year: 2017,
               month: 6,
               day: 2,
               zone_abbr: "UTC",
               hour: 13,
               minute: 0,
               second: 0,
               microsecond: {0, 0},
               utc_offset: 0,
               std_offset: 0,
               time_zone: "Etc/UTC"
             }

    assert DateTime.from_iso8601("2017-06-02T14:00:00-0400")
           |> elem(1) ==
             %DateTime{
               year: 2017,
               month: 6,
               day: 2,
               zone_abbr: "UTC",
               hour: 18,
               minute: 0,
               second: 0,
               microsecond: {0, 0},
               utc_offset: 0,
               std_offset: 0,
               time_zone: "Etc/UTC"
             }

    assert DateTime.from_iso8601("2017-06-02T14:00:00+01")
           |> elem(1) ==
             %DateTime{
               year: 2017,
               month: 6,
               day: 2,
               zone_abbr: "UTC",
               hour: 13,
               minute: 0,
               second: 0,
               microsecond: {0, 0},
               utc_offset: 0,
               std_offset: 0,
               time_zone: "Etc/UTC"
             }

    assert DateTime.from_iso8601("2017-06-02T14:00:00-04")
           |> elem(1) ==
             %DateTime{
               year: 2017,
               month: 6,
               day: 2,
               zone_abbr: "UTC",
               hour: 18,
               minute: 0,
               second: 0,
               microsecond: {0, 0},
               utc_offset: 0,
               std_offset: 0,
               time_zone: "Etc/UTC"
             }
  end

  test "truncate/2" do
    datetime = %DateTime{
      year: 2017,
      month: 11,
      day: 6,
      zone_abbr: "CET",
      hour: 0,
      minute: 6,
      second: 23,
      microsecond: {0, 0},
      utc_offset: 3600,
      std_offset: 0,
      time_zone: "Europe/Paris"
    }

    assert DateTime.truncate(%{datetime | microsecond: {123_456, 6}}, :microsecond) ==
             %{datetime | microsecond: {123_456, 6}}

    assert DateTime.truncate(%{datetime | microsecond: {0, 0}}, :millisecond) ==
             %{datetime | microsecond: {0, 0}}

    assert DateTime.truncate(%{datetime | microsecond: {000_100, 6}}, :millisecond) ==
             %{datetime | microsecond: {0, 3}}

    assert DateTime.truncate(%{datetime | microsecond: {000_999, 6}}, :millisecond) ==
             %{datetime | microsecond: {0, 3}}

    assert DateTime.truncate(%{datetime | microsecond: {001_000, 6}}, :millisecond) ==
             %{datetime | microsecond: {1000, 3}}

    assert DateTime.truncate(%{datetime | microsecond: {001_200, 6}}, :millisecond) ==
             %{datetime | microsecond: {1000, 3}}

    assert DateTime.truncate(%{datetime | microsecond: {123_456, 6}}, :millisecond) ==
             %{datetime | microsecond: {123_000, 3}}

    assert DateTime.truncate(%{datetime | microsecond: {123_456, 6}}, :second) ==
             %{datetime | microsecond: {0, 0}}
  end
end
