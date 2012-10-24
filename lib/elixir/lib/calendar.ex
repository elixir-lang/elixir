defrecord Date, year: nil, month: nil, day: nil
defrecord Time, hour: nil, minute: nil, second: nil
defrecord DateTime, date: nil, time: nil

defmodule Calendar do
  @minute_in_seconds 60
  @hour_in_seconds   60 * 60
  @day_in_seconds    24 * 60 * 60

  def from_tuple({ { year, month, day }, { hour, minute, second } }) do
    DateTime[date: Date[year: year, month: month, day: day],
             time: Time[hour: hour, minute: minute, second: second]]
  end

  def to_tuple(Date[year: year, month: month, day: day]) do
    { year, month, day }
  end

  def to_tuple(Time[hour: hour, minute: minute, second: second]) do
    { hour, minute, second }
  end

  def to_tuple(DateTime[date: date, time: time]) do
    { to_tuple(date), to_tuple(time) }
  end

  def universal_time do
    from_tuple(:calendar.universal_time)
  end

  def local_time do
    from_tuple(:calendar.local_time)
  end

  def difference(DateTime[] = datetime1, DateTime[] = datetime2) do
    tuple1 = to_tuple(datetime1)
    tuple2 = to_tuple(datetime2)
    seconds1 = :calendar.datetime_to_gregorian_seconds(tuple1)
    seconds2 = :calendar.datetime_to_gregorian_seconds(tuple2)
    seconds1 - seconds2
  end

  def format(DateTime[date: date], fmt) do
    format(date, fmt)
  end

  def format(Date[day: day] = date, "dd" <> t) do
    padded_two_digits(day) <> format(date, t)
  end

  def format(Date[day: day] = date, "d" <> t) do
    integer_to_binary(day) <> format(date, t)
  end

  def format(Date[year: year] = date, "YYYY" <> t) do
    integer_to_binary(year) <> format(date, t)
  end

  def format(Date[year: year] = date, "YY" <> t) do
    year = rem(year, 100)
    padded_two_digits(year) <> format(date, t)
  end

  def format(date, "MMMM" <> t) do
    month_name(date) <> format(date, t)
  end

  def format(date, "MM" <> t) do
    month_abbr(date) <> format(date, t)
  end

  def format(date, << h, t :: binary >>) when not (h in ?a..?z or h in ?A..?Z) do
    << h, format(date, t) :: binary >>
  end

  def format(_date, <<>>) do
    <<>>
  end

  def add(DateTime[] = datetime, options // []) do
    update(datetime, options, &1 + &2)
  end

  def subtract(DateTime[] = datetime, options // []) do
    update(datetime, options, &1 - &2)
  end

  defp update(DateTime[] = datetime, options, function) do
    seconds = 0
    if seconds_option = options[:seconds] do
      seconds = seconds + seconds_option
    end

    if minutes_option = options[:minutes] do
      seconds = seconds + (minutes_option * @minute_in_seconds)
    end

    if hours_option = options[:hours] do
      seconds = seconds + (hours_option * @hour_in_seconds)
    end

    if days_option = options[:days] do
      seconds = seconds + (days_option * @day_in_seconds)
    end

    time_in_seconds = :calendar.datetime_to_gregorian_seconds(to_tuple(datetime))
    time_in_seconds = function.(time_in_seconds, seconds)
    datetime = :calendar.gregorian_seconds_to_datetime(time_in_seconds)
    from_tuple(datetime)
  end

  def weekday(Date[] = date) do
    :calendar.day_of_the_week(to_tuple(date))
  end

  def weekday(DateTime[date: date]) do
    weekday(date)
  end

  def day(Date[day: day]),       do: day
  def month(Date[month: month]), do: month
  def year(Date[year: year]),    do: year

  def day(DateTime[date: date]),   do: day(date)
  def month(DateTime[date: date]), do: month(date)
  def year(DateTime[date: date]),  do: year(date)

  def leap?(Date[year: year]) do
    leap?(year)
  end

  def leap?(DateTime[date: date]) do
    leap?(date)
  end

  def leap?(year) when is_integer(year) do
    :calendar.is_leap_year(year)
  end

  def weekday_abbr(Date[] = date) do
    weekday = weekday(date)
    weekday_abbr(weekday)
  end

  def weekday_abbr(DateTime[date: date]) do
    weekday_abbr(date)
  end

  def weekday_abbr(1), do: "Mon"
  def weekday_abbr(2), do: "Tue"
  def weekday_abbr(3), do: "Wed"
  def weekday_abbr(4), do: "Thu"
  def weekday_abbr(5), do: "Fri"
  def weekday_abbr(6), do: "Sat"
  def weekday_abbr(7), do: "Sun"

  def weekday_name(Date[] = date) do
    weekday = weekday(date)
    weekday_name(weekday)
  end

  def weekday_name(DateTime[date: date]) do
    weekday_name(date)
  end

  def weekday_name(1), do: "Monday"
  def weekday_name(2), do: "Tuesday"
  def weekday_name(3), do: "Wednesday"
  def weekday_name(4), do: "Thursday"
  def weekday_name(5), do: "Friday"
  def weekday_name(6), do: "Saturday"
  def weekday_name(7), do: "Sunday"

  def month_abbr(Date[month: month]) do
    month_abbr(month)
  end

  def month_abbr(DateTime[date: date]) do
    month_abbr(date)
  end

  def month_abbr(1),  do: "Jan"
  def month_abbr(2),  do: "Feb"
  def month_abbr(3),  do: "Mar"
  def month_abbr(4),  do: "Apr"
  def month_abbr(5),  do: "May"
  def month_abbr(6),  do: "Jun"
  def month_abbr(7),  do: "Jul"
  def month_abbr(8),  do: "Aug"
  def month_abbr(9),  do: "Sep"
  def month_abbr(10), do: "Oct"
  def month_abbr(11), do: "Nov"
  def month_abbr(12), do: "Dec"

  def month_name(Date[month: month]) do
    month_name(month)
  end

  def month_name(DateTime[date: date]) do
    month_name(date)
  end

  def month_name(1),  do: "January"
  def month_name(2),  do: "February"
  def month_name(3),  do: "March"
  def month_name(4),  do: "April"
  def month_name(5),  do: "May"
  def month_name(6),  do: "June"
  def month_name(7),  do: "July"
  def month_name(8),  do: "August"
  def month_name(9),  do: "September"
  def month_name(10), do: "October"
  def month_name(11), do: "November"
  def month_name(12), do: "December"

  ## Helpers

  defp padded_two_digits(x) when x < 10 do
    << ?0, ?0 + x >>
  end

  defp padded_two_digits(x) do
    integer_to_binary(x)
  end
end
