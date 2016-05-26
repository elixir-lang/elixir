Code.require_file "test_helper.exs", __DIR__

defmodule FakeCalendar do
  def to_string(_), do: "boom"
end

defmodule DateTest do
  use ExUnit.Case, async: true
  doctest Date

  test "to_string/1" do
    assert to_string(~D[2000-01-01]) == "2000-01-01"

    date = Map.put(~D[2000-01-01], :calendar, FakeCalendar)
    assert to_string(date) == "boom"
  end

  test "inspect/1" do
    assert inspect(~D[2000-01-01]) == "~D[2000-01-01]"

    date = Map.put(~D[2000-01-01], :calendar, FakeCalendar)
    assert inspect(date) == "%Date{calendar: FakeCalendar, day: 1, month: 1, year: 2000}"
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
end

defmodule NaiveDateTimeTest do
  use ExUnit.Case, async: true
  doctest NaiveDateTime

  test "to_string/1" do
    assert to_string(~N[2000-01-01 23:00:07.005]) == "2000-01-01 23:00:07.005"

    date = Map.put(~N[2000-01-01 23:00:07.005], :calendar, FakeCalendar)
    assert to_string(date) == "boom"
  end

  test "inspect/1" do
    assert inspect(~N[2000-01-01 23:00:07.005]) == "~N[2000-01-01 23:00:07.005]"

    date = Map.put(~N[2000-01-01 23:00:07.005], :calendar, FakeCalendar)
    assert inspect(date) == "%NaiveDateTime{calendar: FakeCalendar, day: 1, hour: 23, " <>
                            "microsecond: {5000, 3}, minute: 0, month: 1, second: 7, year: 2000}"
  end
end

defmodule DateTimeTest do
  use ExUnit.Case, async: true
  doctest DateTime

  test "to_string/1" do
    dt = %DateTime{year: 2000, month: 2, day: 29,
                   hour: 23, minute: 0, second: 7, microsecond: {0, 0},
                   utc_offset: -12600, std_offset: 3600, time_zone: "Brazil/Manaus"}
    assert to_string(dt) == "2000-02-29 23:00:07-02:30 Brazil/Manaus"
  end
end
