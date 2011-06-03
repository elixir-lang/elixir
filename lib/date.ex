% Date represents dates, with years, months, and days. Date has no concept of timezones,
% while DateTime does(but not currently). This implementation is based on Erlang's date BIF:
% http://www.erlang.org/doc/man/erlang.html#date-0
module Date
  def new([date_tuple])
    {year, month, day} = date_tuple
    #Date::Instance(year, month, day)
  end

  def new([year, month, day])
    #Date::Instance(year, month, day)
  end

  % Return the current date according to the operating system,
  % but in UTC. This means that, if you are on 17th April but
  % in UTC it still is 16th April, it will return 16th April.
  def today
    {date,_} = Erlang.calendar.universal_time
    Date.new(date)
  end

  % Return the tomorrow's date according to the operating system,
  % but in UTC.
  def tomorrow
    Date.today.tomorrow
  end

  % Return the yesterday's date according to the operating system,
  % but in UTC.
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

  module Instance
    attr_reader ['day, 'month, 'year]

    def __bound__(year, month, day)
      @('year: year, 'month: month, 'day: day)
    end

    def weekday
      Erlang.calendar.day_of_the_week(to_tuple)
    end

    def weekday_name
      day_name(weekday)
    end

    def month_name
      month_name(@month)
    end

    % Return a string representation of the Date.
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

    % Determine whether or the not the Date occurs within a leap year
    def leap_year?
      Erlang.calendar.is_leap_year(@year)
    end

    % Return tomorrow's date, relative to the current date
    def tomorrow
      gregorian_addition(1)
    end

    % Return yesterdays's date, relative to the current date
    def yesterday
      gregorian_addition(-1)
    end

    % Subtract the number of *days* from the current date
    def -(days)
      gregorian_addition(-days)
    end

    % Add the number of *days* from the current date
    def +(days)
      gregorian_addition(days)
    end

    % Converts the Date object to a tuple, with year, month, and day
    def to_tuple
     {@year, @month, @day}
    end

    private

    def day_name(1) "Mon"; end
    def day_name(2) "Tue"; end
    def day_name(3) "Wed"; end
    def day_name(4) "Thu"; end
    def day_name(5) "Fri"; end
    def day_name(6) "Sat"; end
    def day_name(7) "Sun"; end

    def month_name(1) "Jan"; end
    def month_name(2) "Feb"; end
    def month_name(3) "Mar"; end
    def month_name(4) "Apr"; end
    def month_name(5) "May"; end
    def month_name(6) "Jun"; end
    def month_name(7) "Jul"; end
    def month_name(8) "Aug"; end
    def month_name(9) "Sep"; end
    def month_name(10) "Oct"; end
    def month_name(11) "Nov"; end
    def month_name(12) "Dec"; end

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
end
