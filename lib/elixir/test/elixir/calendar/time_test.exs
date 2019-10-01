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
                 ~s/cannot parse "12:34:56 notalias" as Time for Calendar.ISO, reason: :invalid_format/,
                 fn -> Code.eval_string("~T[12:34:56 notalias]") end

    assert_raise ArgumentError,
                 ~s/cannot parse "12:34:65" as Time for Calendar.Holocene, reason: :invalid_time/,
                 fn -> Code.eval_string("~T[12:34:65 Calendar.Holocene]") end

    assert_raise UndefinedFunctionError, fn ->
      Code.eval_string("~T[12:34:56 UnknownCalendar]")
    end
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
