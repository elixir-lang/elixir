Code.require_file "../test_helper.exs", __FILE__

defmodule DateTimeTest do
  use ExUnit.Case, async: true

  test "can create a new DateTime from a tuple" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 19, 21, 0 } })
    expected = DateTime.new(date: Date.new(year: 2012, month: 9, day: 29),
                            time: Time.new(hour: 19, minute: 21, second: 0))
    assert datetime == expected
  end

  test "can convert a Date to a tuple" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.to_tuple(date) == { 2012, 9, 29 }
  end

  test "can convert a Time to a tuple" do
    time = Time.new(hour: 19, minute: 21, second: 0)
    assert Calendar.to_tuple(time) == { 19, 21, 0 }
  end

  test "can convert a DateTime to a tuple" do
    date = Date.new(year: 2012, month: 9, day: 29)
    time = Time.new(hour: 19, minute: 21, second: 0)
    datetime = DateTime.new(date: date, time: time)
    assert Calendar.to_tuple(datetime) == { { 2012, 9, 29 }, { 19, 21, 0 } }
  end

  test "can get the day of the week from a Date" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.weekday(date) == 6

    date = date.update(day: 30)
    assert Calendar.weekday(date) == 7

    date = date.update(month: 10, day: 1)
    assert Calendar.weekday(date) == 1
  end

  test "can get the day of the week from a DateTime" do
    date = Date.new(year: 2012, month: 9, day: 29)
    time = Time.new(hour: 19, minute: 21, second: 0)
    datetime = DateTime.new(date: date, time: time)
    assert Calendar.weekday(datetime) == 6

    datetime = datetime.update(date: date.update(day: 30))
    assert Calendar.weekday(datetime) == 7

    datetime = datetime.update(date: date.update(month: 10, day: 1))
    assert Calendar.weekday(datetime) == 1
  end

  test "can retrieve the universal time" do
    DateTime[date: date, time: time] = Calendar.universal_time
    assert Date[] = date
    assert Time[] = time
  end

  test "can retrieve the local time" do
    DateTime[date: date, time: time] = Calendar.local_time
    assert Date[] = date
    assert Time[] = time
  end

  test "can get the day of the month from a Date" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.day(date) == 29

    date = date.update(day: 30)
    assert Calendar.day(date) == 30

    date = date.update(month: 10, day: 1)
    assert Calendar.day(date) == 1
  end

  test "can get the day of the month from a DateTime" do
    date = Date.new(year: 2012, month: 9, day: 29)
    time = Time.new(hour: 19, minute: 21, second: 0)
    datetime = DateTime.new(date: date, time: time)
    assert Calendar.day(datetime) == 29

    datetime = datetime.update(date: date.update(day: 30))
    assert Calendar.day(datetime) == 30

    datetime = datetime.update(date: date.update(month: 10, day: 1))
    assert Calendar.day(datetime) == 1
  end

  test "can get the month from a Date" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.month(date) == 9
  end

  test "can get the month from a DateTime" do
    date = Date.new(year: 2012, month: 9, day: 29)
    datetime = DateTime.new(date: date)
    assert Calendar.month(datetime) == 9
  end

  test "can get the year from a Date" do
    date = Date.new(year: 2012, month: 10, day: 1)
    assert Calendar.year(date) == 2012
  end

  test "can get the year from a DateTime" do
    date = Date.new(year: 2012, month: 10, day: 1)
    time = Time.new(hour: 19, minute: 21, second: 0)
    datetime = DateTime.new(date: date, time: time)
    assert Calendar.year(datetime) == 2012
  end

  test "can determine if the year is a leap year from a Date" do
    date = Date.new(year: 2012, month: 10, day: 1)
    assert Calendar.leap?(date) == true

    date = date.update(year: 2013)
    assert Calendar.leap?(date) == false
  end

  test "can determine if the year is a leap year from a DateTime" do
    date = Date.new(year: 2012, month: 10, day: 1)
    datetime = DateTime.new(date: date)
    assert Calendar.leap?(datetime) == true

    datetime = datetime.update(date: date.update(year: 2013))
    assert Calendar.leap?(datetime) == false
  end

  test "can determine if the year is a leap year from an integer" do
    assert Calendar.leap?(2012)
    refute Calendar.leap?(2013)
  end

  test "can get an abbreviated weekday from a Date" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.weekday_abbr(date) == "Sat"
  end

  test "can get the full weekday from a Date" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.weekday_name(date) == "Saturday"
  end

  test "can get an abbreviate month name from a Date" do
    date = Date.new(year: 2012, month: 9, day: 29)
    assert Calendar.month_abbr(date) == "Sep"
  end

  test "can get an abbreviated weekday from a DateTime" do
    datetime = DateTime.new(date: Date.new(year: 2012, month: 9, day: 29))
    assert Calendar.weekday_abbr(datetime) == "Sat"
  end

  test "can get the full weekday from a DateTime" do
    datetime = DateTime.new(date: Date.new(year: 2012, month: 9, day: 29))
    assert Calendar.weekday_name(datetime) == "Saturday"
  end

  test "can get an abbreviate month name from a DateTime" do
    datetime = DateTime.new(date: Date.new(year: 2012, month: 9, day: 29))
    assert Calendar.month_abbr(datetime) == "Sep"
  end

  test "can get a difference between two DateTimes" do
    datetime1 = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    datetime2 = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 5, 0 } })
    assert Calendar.difference(datetime2, datetime1) == 300
  end

  test "can add seconds to a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 1, 15 } })
    assert Calendar.add(datetime, seconds: 75) == expected
  end

  test "can add minutes to a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    expected =Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 3, 0 } })
    assert Calendar.add(datetime, minutes: 3) == expected
  end

  test "can add hours to a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 2, 0, 0 } })
    assert Calendar.add(datetime, hours: 2) == expected
  end

  test "can add days to a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    expected = Calendar.from_tuple({ { 2012, 10, 1 }, { 0, 0, 0 } })
    assert Calendar.add(datetime, days: 2) == expected
  end

  test "can add combined options" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    expected = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.add(datetime, days: 2, hours: 3, minutes: 5) == expected
  end

  test "can subtract seconds from a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 1, 15 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    assert Calendar.subtract(datetime, seconds: 75) == expected
  end

  test "can subtract minutes from a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 3, 0 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    assert Calendar.subtract(datetime, minutes: 3) == expected
  end

  test "can subtract hours from a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 9, 29 }, { 2, 0, 0 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    assert Calendar.subtract(datetime, hours: 2) == expected
  end

  test "can subtract days from a DateTime" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 0, 0, 0 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    assert Calendar.subtract(datetime, days: 2) == expected
  end

  test "can subtract combined options" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    expected = Calendar.from_tuple({ { 2012, 9, 29 }, { 0, 0, 0 } })
    assert Calendar.subtract(datetime, days: 2, hours: 3, minutes: 5) == expected
  end
end

defmodule DateTime.FormatTest do
  use ExUnit.Case, async: true

  test "handles dd" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "d") == "1"

    datetime = Calendar.from_tuple({ { 2012, 10, 13 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "d") == "13"
  end

  test "handles d" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "dd") == "01"
    datetime = Calendar.from_tuple({ { 2012, 10, 13 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "dd") == "13"
  end

  test "handles MM" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "MM") == "Oct"
  end

  test "handles MMMM" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "MMMM") == "October"
  end

  test "handles YY" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "MM YY") == "Oct 12"

    datetime = Calendar.from_tuple({ { 2008, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "MM YY") == "Oct 08"
  end

  test "handles YYYY" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "MM YYYY") == "Oct 2012"
  end

  test "supports other non-alphabetic characters" do
    datetime = Calendar.from_tuple({ { 2012, 10, 1 }, { 3, 5, 0 } })
    assert Calendar.format(datetime, "MM?") == "Oct?"
  end
end
