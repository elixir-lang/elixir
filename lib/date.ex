% elixir: cache

object Date
  attr_reader ['day, 'month, 'year]

  module Mixin
    def today
      Date.new(Erlang.date)
    end

    def tomorrow
      Date.today.tomorrow
    end

    def yesterday
      Date.today.yesterday
    end

    def days_in_month(date)
      if [1,3,5,7,8,10,12].member?(date.month)
        31
      elsif [4,6,9,1,11].member?(date.month)
        30
      elsif date.month == 2 && date.leap_year?
        29
      else
        28
      end
    end
  end

  def initialize(date_tuple)
    {year, month, day} = date_tuple
    initialize(year, month, day)
  end

  def initialize(year, month, day)
    @('year: year, 'month: month, 'day: day)
  end

  def inspect
    "#{@year}-#{convert_to_double_digit(@month)}-#{convert_to_double_digit(@day)}"
  end

  def to_s
    inspect
  end

  def leap_year?
    Erlang.calendar.is_leap_year(@year)
  end

  def tomorrow
    gregorian_addition(1)
  end

  def yesterday
    gregorian_addition(-1)
  end

  def -(days)
    gregorian_addition(-days)
  end

  def +(days)
    gregorian_addition(days)
  end

  def to_tuple
   {@year, @month, @day}
  end

  private

  def gregorian_addition(days)
    time = {0,0,0}
    seconds = Erlang.calendar.datetime_to_gregorian_seconds({to_tuple,time})
    { date, _time } = Erlang.calendar.gregorian_seconds_to_datetime(seconds + (86400 * days))
    Date.new(date)
  end

  def convert_to_double_digit(unit)
    if unit < 10
      "0" + unit.to_s
    else
      unit.to_s
    end
  end
end
