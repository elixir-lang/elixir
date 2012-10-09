defrecord DateTime, date: nil, time: nil

defmodule Calendar do
  def from_tuple({ date, time }) do
    DateTime[date: date, time: time]
  end

  def to_tuple(DateTime[date: date, time: time]) do
    { date, time }
  end

  def difference(DateTime[] = time1, DateTime[] = time2) do
    tuple1 = to_tuple(time1)
    tuple2 = to_tuple(time2)
    seconds1 = :calendar.datetime_to_gregorian_seconds(tuple1)
    seconds2 = :calendar.datetime_to_gregorian_seconds(tuple2)
    seconds1 - seconds2
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
      seconds = seconds + (minutes_option * 60)
    end

    if hours_option = options[:hours] do
      seconds = seconds + (hours_option * 60 * 60)
    end

    if days_option = options[:days] do
      seconds = seconds + (days_option * 60 * 60 * 24)
    end

    time_in_seconds = :calendar.datetime_to_gregorian_seconds({ date, time })
    time_in_seconds = function.(time_in_seconds, seconds)
    { date, time } = :calendar.gregorian_seconds_to_datetime(time_in_seconds)
    DateTime[date: date, time: time]
  end

  def weekday(DateTime[date:  date]) do
    :calendar.day_of_the_week(date)
  end

  def day(DateTime[date: { _, _, day }]), do: day
  def month(DateTime[date: { _, month, _ }]), do: month
  def year(DateTime[date: { year, _, _ }]), do: year

  def leap?(DateTime[date: { year, _, _ }]) do
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

end
