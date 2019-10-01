Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)
Code.require_file("fakes.exs", __DIR__)

defmodule NaiveDateTimeTest do
  use ExUnit.Case, async: true
  doctest NaiveDateTime

  test "sigil_N" do
    assert ~N[2000-01-01T12:34:56] ==
             %NaiveDateTime{
               calendar: Calendar.ISO,
               year: 2000,
               month: 1,
               day: 1,
               hour: 12,
               minute: 34,
               second: 56
             }

    assert ~N[2000-01-01T12:34:56 Calendar.Holocene] ==
             %NaiveDateTime{
               calendar: Calendar.Holocene,
               year: 2000,
               month: 1,
               day: 1,
               hour: 12,
               minute: 34,
               second: 56
             }

    assert ~N[2000-01-01 12:34:56] ==
             %NaiveDateTime{
               calendar: Calendar.ISO,
               year: 2000,
               month: 1,
               day: 1,
               hour: 12,
               minute: 34,
               second: 56
             }

    assert ~N[2000-01-01 12:34:56 Calendar.Holocene] ==
             %NaiveDateTime{
               calendar: Calendar.Holocene,
               year: 2000,
               month: 1,
               day: 1,
               hour: 12,
               minute: 34,
               second: 56
             }

    assert_raise ArgumentError,
                 ~s/cannot parse "2001-50-50T12:34:56" as NaiveDateTime for Calendar.ISO, reason: :invalid_date/,
                 fn -> Code.eval_string("~N[2001-50-50T12:34:56]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2001-01-01T12:34:65" as NaiveDateTime for Calendar.ISO, reason: :invalid_time/,
                 fn -> Code.eval_string("~N[2001-01-01T12:34:65]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2001-01-01 12:34:56 notalias" as NaiveDateTime for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~N[2001-01-01 12:34:56 notalias]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2001-01-01T12:34:56 notalias" as NaiveDateTime for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~N[2001-01-01T12:34:56 notalias]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2001-50-50T12:34:56" as NaiveDateTime for Calendar.Holocene, reason: :invalid_date/,
                 fn -> Code.eval_string("~N[2001-50-50T12:34:56 Calendar.Holocene]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "2001-01-01T12:34:65" as NaiveDateTime for Calendar.Holocene, reason: :invalid_time/,
                 fn -> Code.eval_string("~N[2001-01-01T12:34:65 Calendar.Holocene]") end

    assert_raise UndefinedFunctionError, fn ->
      Code.eval_string("~N[2001-01-01 12:34:56 UnknownCalendar]")
    end

    assert_raise UndefinedFunctionError, fn ->
      Code.eval_string("~N[2001-01-01T12:34:56 UnknownCalendar]")
    end
  end

  test "to_string/1" do
    assert to_string(~N[2000-01-01 23:00:07.005]) == "2000-01-01 23:00:07.005"
    assert NaiveDateTime.to_string(~N[2000-01-01 23:00:07.005]) == "2000-01-01 23:00:07.005"

    ndt = %{~N[2000-01-01 23:00:07.005] | calendar: FakeCalendar}
    assert to_string(ndt) == "1/1/2000F23::0::7"
  end

  test "inspect/1" do
    assert inspect(~N[2000-01-01 23:00:07.005]) == "~N[2000-01-01 23:00:07.005]"
    assert inspect(~N[-0100-12-31 23:00:07.005]) == "~N[-0100-12-31 23:00:07.005]"

    ndt = %{~N[2000-01-01 23:00:07.005] | calendar: FakeCalendar}
    assert inspect(ndt) == "~N[1/1/2000F23::0::7 FakeCalendar]"
  end

  test "compare/2" do
    ndt1 = ~N[2000-04-16 13:30:15.0049]
    ndt2 = ~N[2000-04-16 13:30:15.0050]
    ndt3 = ~N[2001-04-16 13:30:15.0050]
    ndt4 = ~N[-0001-04-16 13:30:15.004]
    assert NaiveDateTime.compare(ndt1, ndt1) == :eq
    assert NaiveDateTime.compare(ndt1, ndt2) == :lt
    assert NaiveDateTime.compare(ndt2, ndt1) == :gt
    assert NaiveDateTime.compare(ndt3, ndt1) == :gt
    assert NaiveDateTime.compare(ndt3, ndt2) == :gt
    assert NaiveDateTime.compare(ndt4, ndt4) == :eq
    assert NaiveDateTime.compare(ndt1, ndt4) == :gt
    assert NaiveDateTime.compare(ndt4, ndt3) == :lt
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

  test "add/2 with datetime" do
    dt = %DateTime{
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

    assert NaiveDateTime.add(dt, 21, :second) == ~N[2000-02-29 23:00:28]
  end

  test "diff/2 with other calendars" do
    assert ~N[2000-01-01 12:34:15.123456]
           |> NaiveDateTime.convert!(Calendar.Holocene)
           |> NaiveDateTime.add(10, :second)
           |> NaiveDateTime.diff(~N[2000-01-01 12:34:15.123456]) == 10
  end

  test "diff/2 with datetime" do
    dt = %DateTime{
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

    assert NaiveDateTime.diff(%{dt | second: 57}, dt, :second) == 50
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

    assert NaiveDateTime.convert(DateTime.from_unix!(0, :second), Calendar.Holocene) ==
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

  test "truncate/2 with datetime" do
    dt = %DateTime{
      year: 2000,
      month: 2,
      day: 29,
      zone_abbr: "CET",
      hour: 23,
      minute: 0,
      second: 7,
      microsecond: {3000, 6},
      utc_offset: 3600,
      std_offset: 0,
      time_zone: "Europe/Warsaw"
    }

    assert NaiveDateTime.truncate(dt, :millisecond) == ~N[2000-02-29 23:00:07.003]
    assert catch_error(NaiveDateTime.truncate(~T[00:00:00.000000], :millisecond))
  end

  describe "to_date/2" do
    test "downcasting" do
      dt = %DateTime{
        year: 2000,
        month: 2,
        day: 29,
        zone_abbr: "CET",
        hour: 23,
        minute: 0,
        second: 7,
        microsecond: {3000, 6},
        utc_offset: 3600,
        std_offset: 0,
        time_zone: "Europe/Warsaw"
      }

      assert NaiveDateTime.to_date(dt) == ~D[2000-02-29]
    end

    test "upcasting" do
      assert catch_error(NaiveDateTime.to_date(~D[2000-02-29]))
    end
  end

  describe "to_time/2" do
    test "downcasting" do
      dt = %DateTime{
        year: 2000,
        month: 2,
        day: 29,
        zone_abbr: "CET",
        hour: 23,
        minute: 0,
        second: 7,
        microsecond: {3000, 6},
        utc_offset: 3600,
        std_offset: 0,
        time_zone: "Europe/Warsaw"
      }

      assert NaiveDateTime.to_time(dt) == ~T[23:00:07.003000]
    end

    test "upcasting" do
      assert catch_error(NaiveDateTime.to_time(~T[00:00:00.000000]))
    end
  end
end
