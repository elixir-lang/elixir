Code.require_file "../test_helper.exs", __FILE__

defmodule DateTimeTest do
  use ExUnit.Case, async: true

  test "can create a new DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert time.date == { 2012, 9, 29 }
  end

  test "can convert a DateTime to a tuple" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.to_tuple(time) == { { 2012, 9, 29 }, { 19, 21, 0 } }
  end

  test "can make a tuple into a DateTime" do
    time = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    expected = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert time == expected
  end

  test "can retrieve the universal time" do
    DateTime[date: date, time: time] = Calendar.universal_time
    assert { _, _, _ } = date
    assert { _, _, _ } = time
  end

  test "can retrieve the local time" do
    DateTime[date: date, time: time] = Calendar.local_time
    assert { _, _, _ } = date
    assert { _, _, _ } = time
  end

  test "can get the day of the week from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.weekday(time) == 6

    time = DateTime.new(date: { 2012, 9, 30 }, time: { 19, 21, 0 })
    assert Calendar.weekday(time) == 7

    time = DateTime.new(date: { 2012, 10, 1 }, time: { 19, 21, 0 })
    assert Calendar.weekday(time) == 1
  end

  test "can get the day of the month from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.day(time) == 29

    time = DateTime.new(date: { 2012, 9, 30 }, time: { 19, 21, 0 })
    assert Calendar.day(time) == 30

    time = DateTime.new(date: { 2012, 10, 1 }, time: { 19, 21, 0 })
    assert Calendar.day(time) == 1 
  end

  test "can get the month from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.month(time) == 9
  end

  test "can get the year from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.year(time) == 2012
  end

  test "can determine if the year is a leap year" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.leap?(time)

    time = DateTime.new(date: { 2013, 9, 29 }, time: { 19, 21, 0 })
    refute Calendar.leap?(time)

    assert Calendar.leap?(2012)
    refute Calendar.leap?(2013)
  end

  test "can get an abbreviated weekday from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.weekday_abbr(time) == "Sat"
  end

  test "can get the full weekday from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.weekday_name(time) == "Saturday"
  end

  test "can get an abbreviate month name from the DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.month_abbr(time) == "Sep"
  end

  test "can get a difference between two DateTimes" do
    time1 = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    time2 = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 5, 0 })
    assert Calendar.difference(time2, time1) == 300
  end

  test "can add seconds to a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.add(time, seconds: 75) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 1, 15 })
  end

  test "can add minutes to a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.add(time, minutes: 3) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 3, 0 })
  end

  test "can add hours to a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.add(time, hours: 2) == DateTime.new(date: { 2012, 9, 29 }, time: { 2, 0, 0 })
  end

  test "can add days to a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.add(time, days: 2) == DateTime.new(date: { 2012, 10, 1 }, time: { 0, 0, 0 })
  end

  test "can add combined options" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.add(time, days: 2, hours: 3, minutes: 5) == DateTime.new(date: { 2012, 10, 1 }, time: { 3, 5, 0 })
  end

  test "can subtract seconds from a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 1, 15 })
    assert Calendar.subtract(time, seconds: 75) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
  end

  test "can subtract minutes from a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 3, 0 })
    assert Calendar.subtract(time, minutes: 3) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
  end

  test "can subtract hours from a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 2, 0, 0 })
    assert Calendar.subtract(time, hours: 2) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
  end

  test "can subtract days from a DateTime" do
    time = DateTime.new(date: { 2012, 10, 1 }, time: { 0, 0, 0 })
    assert Calendar.subtract(time, days: 2) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
  end

  test "can subtract combined options" do
    time = DateTime.new(date: { 2012, 10, 1 }, time: { 3, 5, 0 })
    assert Calendar.subtract(time, days: 2, hours: 3, minutes: 5) == DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
  end
end

defmodule DateTime.FormatTest do
  use ExUnit.Case, async: true

  test "handles dd" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "d") == "1"

    datetime = DateTime[date: { 2012, 10, 13 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "d") == "13"
  end

  test "handles d" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "dd") == "01"
    datetime = DateTime[date: { 2012, 10, 13 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "dd") == "13"
  end

  test "handles MM" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "MM") == "Oct"
  end

  test "handles MMMM" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "MMMM") == "October"
  end

  test "handles YY" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "MM YY") == "Oct 12"

    datetime = DateTime[date: { 2008, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "MM YY") == "Oct 08"
  end

  test "handles YYYY" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "MM YYYY") == "Oct 2012"
  end

  test "supports other non-alphabetic characters" do
    datetime = DateTime[date: { 2012, 10, 1 }, time: { 3, 5, 0 }]
    assert Calendar.format(datetime, "MM?") == "Oct?"
  end
end