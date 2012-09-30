defrecord DateTime, date: nil, time: nil

defmodule Calendar do
  def weekday(DateTime[date:  date]) do
    :calendar.day_of_the_week(date)
  end

  def day(DateTime[date: { _, _, day }]), do: day
  def month(DateTime[date: { _, month, _ }]), do: month
  def year(DateTime[date: { year, _, _ }]), do: year

  def leap?(DateTime[date: { year, _, _ }]) do
    :calendar.is_leap_year(year)
  end

  def add_seconds(datetime, seconds) do
    update_seconds(datetime, &1 + seconds)
  end

  def subtract_seconds(datetime, seconds) do
    update_seconds(datetime, &1 - seconds)
  end

  defp update_seconds(DateTime[date: date, time: time], function) do
    time_in_seconds = :calendar.datetime_to_gregorian_seconds({ date, time })
    time_in_seconds = function.(time_in_seconds)
    { date, time } = :calendar.gregorian_seconds_to_datetime(time_in_seconds)
    DateTime[date: date, time: time]
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
