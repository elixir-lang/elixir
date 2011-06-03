% DateTime represents both Date and Time. Currently it is not aware of
% timezones, but that should be added in the future, while Date and Time
% objects should always ignore timezones.
%
% This implementation is based on Erlang's calendar module: http://erlang.org/doc/man/calendar.html
module DateTime
  def new([date_time])
    #DateTime::Instance(date_time)
  end

  % Return the current time in UTC according to the value
  % returned by the operating system.
  def utc
    #DateTime::Instance(Erlang.calendar.universal_time)
  end

  module Instance
    def __bound__({date, time})
      @('date: date.to_tuple, 'time: time.to_tuple)
    end

    % Add the number of *seconds* to the DateTime object.
    %
    % ## Example
    %
    %     datetime = DateTime.utc
    %     datetime.to_s  % => "2010-04-17 14:00:00"
    %
    %     % Add two days
    %     datetime + (2 * 86400)  % => "2010-04-19 14:00:00"
    %
    def +(seconds)
      DateTime.new Erlang.calendar.gregorian_seconds_to_datetime(to_i + seconds)
    end

    % Subtract the number of *seconds* from the DateTime object.
    def -(seconds)
      DateTime.new Erlang.calendar.gregorian_seconds_to_datetime(to_i - seconds)
    end

    % Returns a string representation of the DateTime object.
    def inspect
      "#{formatted_date} #{formatted_time}"
    end

    def to_s
      inspect
    end

    % Converts the given time to a string according to the gregorian calendar (i.e. starting with year 0).
    % You can find more information about it on Erlang's calendar module: http://erlang.org/doc/man/calendar.html
    def to_i
      Erlang.calendar.datetime_to_gregorian_seconds({@date,@time})
    end

    % Returns the year
    def year
      @date[0]
    end

    % Returns the month
    def month
      @date[1]
    end

    % Returns the day
    def day
      @date[2]
    end

    % Returns the hours
    def hours
      @time[0]
    end

    % Returns the minutes
    def minutes
      @time[1]
    end

    % Returns the seconds
    def seconds
      @time[2]
    end

    % Returns a tuple where the first element is a tuple containing { year, month, day }
    % and the second is a tuple containing { hours, minutes, seconds }.
    def to_tuple
      { @date, @time }
    end

    % Returns time according to RFC1123.
    def rfc1123
      date = Date.new(@date)
      "#{date.weekday_name}, #{convert_to_double_digit(@date[2])}-#{date.month_name}-#{@date[0]} #{formatted_time} GMT"
    end

    private

    def formatted_date
      "#{@date[0]}-#{convert_to_double_digit(@date[1])}-#{convert_to_double_digit(@date[2])}"
    end

    def formatted_time
      "#{convert_to_double_digit(@time[0])}:#{convert_to_double_digit(@time[1])}:#{convert_to_double_digit(@time[2])}"
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