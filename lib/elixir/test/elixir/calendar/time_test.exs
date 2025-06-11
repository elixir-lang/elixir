# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)
Code.require_file("holocene.exs", __DIR__)
Code.require_file("fakes.exs", __DIR__)

defmodule TimeTest do
  use ExUnit.Case, async: true
  doctest Time

  test "sigil_T" do
    assert ~T[12:34:56] ==
             %Time{calendar: Calendar.ISO, hour: 12, minute: 34, second: 56}

    assert ~T[12:34:56 Calendar.Holocene] ==
             %Time{calendar: Calendar.Holocene, hour: 12, minute: 34, second: 56}

    assert_raise ArgumentError,
                 ~s/cannot parse "12:34:65" as Time for Calendar.ISO, reason: :invalid_time/,
                 fn -> Code.eval_string("~T[12:34:65]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "123456" as Time for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~T[123456]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "12:34:56 notalias" as Time for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~T[12:34:56 notalias]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "12:34:65" as Time for Calendar.Holocene, reason: :invalid_time/,
                 fn -> Code.eval_string("~T[12:34:65 Calendar.Holocene]") end

    assert_raise UndefinedFunctionError, fn ->
      Code.eval_string("~T[12:34:56 UnknownCalendar]")
    end
  end

  test "to_iso8601/2" do
    time1 = ~T[23:00:07.005]
    assert Time.to_iso8601(time1) == "23:00:07.005"
    assert Time.to_iso8601(Map.from_struct(time1)) == "23:00:07.005"
    assert Time.to_iso8601(time1, :basic) == "230007.005"

    time2 = ~T[23:00:07.005 Calendar.Holocene]
    assert Time.to_iso8601(time2) == "23:00:07.005"
    assert Time.to_iso8601(Map.from_struct(time2)) == "23:00:07.005"
    assert Time.to_iso8601(time2, :basic) == "230007.005"
  end

  test "to_string/1" do
    time = ~T[23:00:07.005]
    assert to_string(time) == "23:00:07.005"
    assert Time.to_string(time) == "23:00:07.005"
    assert Time.to_string(Map.from_struct(time)) == "23:00:07.005"

    assert to_string(%{time | calendar: FakeCalendar}) == "23::0::7"
    assert Time.to_string(%{time | calendar: FakeCalendar}) == "23::0::7"
  end

  test "inspect/1" do
    assert inspect(~T[23:00:07.005]) == "~T[23:00:07.005]"

    time = %{~T[23:00:07.005] | calendar: FakeCalendar}
    assert inspect(time) == "~T[23::0::7 FakeCalendar]"
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

    time1_holocene = ~T[01:01:01.005 Calendar.Holocene]
    assert Time.compare(time1_holocene, time1) == :eq
    assert Time.compare(time1_holocene, time2) == :eq
    assert Time.compare(time1_holocene, time3) == :lt
  end

  test "before?/2 and after?/2" do
    time1 = ~T[05:02:01.234]
    time2 = ~T[10:00:04.123]

    assert Time.before?(time1, time2)
    assert not Time.before?(time2, time1)

    assert Time.after?(time2, time1)
    assert not Time.after?(time1, time2)
  end

  test "diff/3" do
    time1 = ~T[05:02:01.234]
    time2 = ~T[10:00:04.123]
    time1_holocene = ~T[05:02:01.234 Calendar.Holocene]

    assert Time.diff(time1, time2) == -17883
    assert Time.diff(time1, time2, :hour) == -4
    assert Time.diff(time1, time2, :minute) == -298
    assert Time.diff(time1, time2, :second) == -17883
    assert Time.diff(time1, time2, :millisecond) == -17_882_889
    assert Time.diff(time1, time2, :microsecond) == -17_882_889_000

    assert Time.diff(time1_holocene, time2) == -17883
    assert Time.diff(time1_holocene, time2, :hour) == -4
    assert Time.diff(time1_holocene, time2, :minute) == -298
    assert Time.diff(time1_holocene, time2, :second) == -17883
    assert Time.diff(time1_holocene, time2, :millisecond) == -17_882_889
    assert Time.diff(time1_holocene, time2, :microsecond) == -17_882_889_000
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

  test "add/3" do
    time = ~T[00:00:00.0]

    assert Time.add(time, 1, :hour) == ~T[01:00:00.0]

    assert Time.add(time, 1, 10) == ~T[00:00:00.100000]

    assert_raise ArgumentError, ~r/Expected :hour, :minute, :second/, fn ->
      Time.add(time, 1, 0)
    end
  end

  test "shift/2" do
    time = ~T[00:00:00.0]
    assert Time.shift(time, hour: 1) == ~T[01:00:00.0]
    assert Time.shift(time, hour: 25) == ~T[01:00:00.0]
    assert Time.shift(time, minute: 25) == ~T[00:25:00.0]
    assert Time.shift(time, second: 50) == ~T[00:00:50.0]
    assert Time.shift(time, microsecond: {150, 6}) == ~T[00:00:00.000150]
    assert Time.shift(time, microsecond: {1000, 4}) == ~T[00:00:00.0010]
    assert Time.shift(time, hour: 2, minute: 65, second: 5) == ~T[03:05:05.0]

    assert_raise ArgumentError,
                 "unsupported unit :day. Expected :hour, :minute, :second, :microsecond",
                 fn -> Time.shift(time, day: 1) end

    assert_raise ArgumentError,
                 "unknown unit :hours. Expected :hour, :minute, :second, :microsecond",
                 fn -> Time.shift(time, hours: 12) end

    assert_raise ArgumentError,
                 "cannot shift time by date scale unit. Expected :hour, :minute, :second, :microsecond",
                 fn -> Time.shift(time, %Duration{day: 1}) end

    assert_raise ArgumentError,
                 "unsupported value nil for :minute. Expected an integer",
                 fn -> Time.shift(time, minute: nil) end

    assert_raise ArgumentError,
                 ~r/unsupported value 1 for :microsecond. Expected a tuple \{ms, precision\}/,
                 fn -> Time.shift(time, microsecond: 1) end
  end
end
