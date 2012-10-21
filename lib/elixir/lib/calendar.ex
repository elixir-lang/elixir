defrecord DateTime, date: nil, time: nil

defmodule Calendar do
  @minute_in_seconds 60
  @hour_in_seconds   60 * 60
  @day_in_seconds    24 * 60 * 60

  def from_tuple({ date, time }) do
    DateTime[date: date, time: time]
  end

  def to_tuple(DateTime[date: date, time: time]) do
    { date, time }
  end

  def universal_time do
    { date, time } = :calendar.universal_time
    DateTime[date: date, time: time]
  end

  def local_time do
    { date, time } = :calendar.local_time
    DateTime[date: date, time: time]
  end

  def difference(DateTime[date: date1, time: time1], DateTime[date: date2, time: time2]) do
    seconds1 = :calendar.datetime_to_gregorian_seconds({ date1, time1 })
    seconds2 = :calendar.datetime_to_gregorian_seconds({ date2, time2 })
    seconds1 - seconds2
  end

  def format(DateTime[date: { _, _, days }] = datetime, "dd" <> t) do
    padded_two_digits(days) <> format(datetime, t)
  end

  def format(DateTime[date: { _, _, days }] = datetime, "d" <> t) do
    integer_to_binary(days) <> format(datetime, t)
  end

  def format(DateTime[date: { year, _, _ }] = datetime, "YYYY" <> t) do
    integer_to_binary(year) <> format(datetime, t)
  end

  def format(DateTime[date: { year, _, _ }] = datetime, "YY" <> t) do
    year = rem(year, 100)
    padded_two_digits(year) <> format(datetime, t)
  end

  def format(datetime, "MMMM" <> t) do
    month_name(datetime) <> format(datetime, t)
  end

  def format(datetime, "MM" <> t) do
    month_abbr(datetime) <> format(datetime, t)
  end

  def format(datetime, << h, t :: binary >>) when not (h in ?a..?z or h in ?A..?Z) do
    << h, format(datetime, t) :: binary >>
  end

  def format(_datetime, <<>>) do
    <<>>
  end

  def add(DateTime[] = datetime, options // []) do
    update(datetime, options, &1 + &2)
  end

  def subtract(DateTime[] = datetime, options // []) do
    update(datetime, options, &1 - &2)
  end

  defp update(DateTime[date: date, time: time], options, function) do
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

    time_in_seconds = :calendar.datetime_to_gregorian_seconds({ date, time })
    time_in_seconds = function.(time_in_seconds, seconds)
    { date, time } = :calendar.gregorian_seconds_to_datetime(time_in_seconds)
    DateTime[date: date, time: time]
  end

  def weekday(DateTime[date: date]) do
    :calendar.day_of_the_week(date)
  end

  def day(DateTime[date: { _, _, day }]), do: day
  def month(DateTime[date: { _, month, _ }]), do: month
  def year(DateTime[date: { year, _, _ }]), do: year

  def leap?(DateTime[date: { year, _, _ }]) do
    leap?(year)
  end

  def leap?(year) when is_integer(year) do
    :calendar.is_leap_year(year)
  end

  def weekday_abbr(DateTime[] = datetime) do
    weekday = weekday(datetime)
    weekday_abbr(weekday)
  end

  def weekday_abbr(1), do: "Mon"
  def weekday_abbr(2), do: "Tue"
  def weekday_abbr(3), do: "Wed"
  def weekday_abbr(4), do: "Thu"
  def weekday_abbr(5), do: "Fri"
  def weekday_abbr(6), do: "Sat"
  def weekday_abbr(7), do: "Sun"

  def weekday_name(DateTime[] = datetime) do
    weekday = weekday(datetime)
    weekday_name(weekday)
  end

  def weekday_name(1), do: "Monday"
  def weekday_name(2), do: "Tuesday"
  def weekday_name(3), do: "Wednesday"
  def weekday_name(4), do: "Thursday"
  def weekday_name(5), do: "Friday"
  def weekday_name(6), do: "Saturday"
  def weekday_name(7), do: "Sunday"

  def month_abbr(DateTime[date: { _, month, _ }]) do
    month_abbr(month)
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

  def month_name(DateTime[date: { _, month, _ }]) do
    month_name(month)
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
