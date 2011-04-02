% elixir: cache

% Date represents dates, with years, months, and days. Date has no concept of timezones, while DateTime does(but not currently).
% 
%
% This implementation is based on Erlang's date BIF: http://www.erlang.org/doc/man/erlang.html#date-0
object Date

  attr_reader ['day, 'month, 'year]

  module Mixin
    % Return the current date according to the operating system.
    def today
      Date.new(Erlang.date)
    end

    % Return the tomorrow's date according to the operating system.
    def tomorrow
      Date.today.tomorrow
    end

    % Return the yesterday's date according to the operating system.
    def yesterday
      Date.today.yesterday
    end

    % Return the number of days in the month in *date*.
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

  % Return a string representation of the Date object.
  %
  % ## Example
  %
  %     date = Date.new(2012, 12, 21)
  %     date.inspect % => 2012-12-21
  %
  def inspect
    "#{@year}-#{convert_to_double_digit(@month)}-#{convert_to_double_digit(@day)}"
  end

  def to_s
    inspect
  end

  % Determine whether or the not the Date object occurs within a leap year
  def leap_year?
    Erlang.calendar.is_leap_year(@year)
  end

  % Return tomorrow's date, relative to the Date object
  def tomorrow
    gregorian_addition(1)
  end

  % Return yesterdays's date, relative to the Date object
  def yesterday
    gregorian_addition(-1)
  end

  % Subtract the number of *days* from the Date object.
  def -(days)
    gregorian_addition(-days)
  end

  % Add the number of *days* from the Date object.
  def +(days)
    gregorian_addition(days)
  end

  % Converts the Date object to a tuple, with year, month, and day
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
