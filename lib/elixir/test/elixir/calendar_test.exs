Code.require_file "../test_helper.exs", __FILE__

defmodule DateTimeTest do
  use ExUnit.Case, async: true

  test "can create a new DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert time.date == { 2012, 9, 29 }
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
    assert Calendar.leap?(time) == true

    time = DateTime.new(date: { 2013, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.leap?(time) == false
  end

  test "can add a second to a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 0 })
    assert Calendar.add_seconds(time, 1).time == { 19, 21, 1 }

    time = DateTime.new(date: { 2012, 9, 29 }, time: { 23, 59, 59 })
    assert Calendar.add_seconds(time, 1).time == { 0, 0, 0 }
    assert Calendar.add_seconds(time, 1).date == { 2012, 9, 30 }

    time = DateTime.new(date: { 2012, 9, 30 }, time: { 23, 59, 59 })
    assert Calendar.add_seconds(time, 1).time == { 0, 0, 0 }
    assert Calendar.add_seconds(time, 1).date == { 2012, 10, 1 }

    time = DateTime.new(date: { 2012, 12, 31 }, time: { 23, 59, 59 })
    assert Calendar.add_seconds(time, 1).time == { 0, 0, 0 }
    assert Calendar.add_seconds(time, 1).date == { 2013, 1, 1 }
  end

  test "can subtract a second from a DateTime" do
    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 21, 1 })
    assert Calendar.subtract_seconds(time, 1).time == { 19, 21, 0 }

    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 20, 0 })
    assert Calendar.subtract_seconds(time, 1).time == { 19, 19, 59 }

    time = DateTime.new(date: { 2012, 9, 29 }, time: { 19, 0, 0 })
    assert Calendar.subtract_seconds(time, 1).time == { 18, 59, 59 }

    time = DateTime.new(date: { 2012, 9, 29 }, time: { 0, 0, 0 })
    assert Calendar.subtract_seconds(time, 1).time == { 23, 59, 59 }
    assert Calendar.subtract_seconds(time, 1).date == { 2012, 9, 28 }

    time = DateTime.new(date: { 2012, 10, 1 }, time: { 0, 0, 0 })
    assert Calendar.subtract_seconds(time, 1).time == { 23, 59, 59 }
    assert Calendar.subtract_seconds(time, 1).date == { 2012, 9, 30 }

    time = DateTime.new(date: { 2013, 1, 1 }, time: { 0, 0, 0 })
    assert Calendar.subtract_seconds(time, 1).time == { 23, 59, 59 }
    assert Calendar.subtract_seconds(time, 1).date == { 2012, 12, 31 }    
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
end