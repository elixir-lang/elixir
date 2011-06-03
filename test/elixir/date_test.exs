Code.require_file "../test_helper", __FILE__

module DateTest
  mixin ExUnit::Case

  def leap_year_test
    true  = Date.new({2012, 12, 21}).leap_year?
    false = Date.new({2009, 12, 21}).leap_year?
  end

  def to_tuple_test
    {2012, 12, 21} = Date.new({2012, 12, 21}).to_tuple
  end

  def to_s_test
    "2012-12-21" = Date.new({2012, 12, 21}).to_s
  end

  def days_in_month_test
    31 = Date.days_in_month(Date.new({2012, 12, 21}))
    30 = Date.days_in_month(Date.new({2011, 4, 21}))
    29 = Date.days_in_month(Date.new({2012, 2, 21}))
    28 = Date.days_in_month(Date.new({2011, 2, 21}))
  end

  def subtraction_test
    {2011, 2, 28} = (Date.new({2011, 3, 25}) - 25).to_tuple
    {2011, 1, 31} = (Date.new({2011, 3, 25}) - 53).to_tuple
    {2010, 12, 31} = (Date.new({2011, 3, 25}) - 84).to_tuple
    {2010, 12, 11} = (Date.new({2011, 3, 25}) - 104).to_tuple
    {2010, 11, 30} = (Date.new({2011, 3, 25}) - 115).to_tuple
    {2008, 6, 28} = (Date.new({2011, 3, 25}) - 1000).to_tuple
  end

  def addition_test
    {2011, 3, 27} = (Date.new({2011, 3, 25}) + 2).to_tuple
    {2011, 4, 1} = (Date.new({2011, 3, 25}) + 7).to_tuple
    {2011, 5, 1} = (Date.new({2011, 3, 25}) + 37).to_tuple
    {2012, 3, 24} = (Date.new({2011, 3, 25}) + 365).to_tuple
  end

  def weekday_test
    6 = Date.new(2010,4,17).weekday
  end

  def weekday_name_test
    "Sat" = Date.new(2010,4,17).weekday_name
  end

  def month_name_test
    "Apr" = Date.new(2010,4,17).month_name
  end
end