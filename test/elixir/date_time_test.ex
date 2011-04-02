Code.require File.expand_path("../test_helper", __FILE__)
Code.require "date_time"

object DateTimeTest
  proto ExUnit::Case

  def utc_test
    date = Date.today.to_tuple
    { ~date, _ } = DateTime.utc.to_tuple
  end

  def addition_test
    {{2011, 3, 25}, {0,0,25}} = (date_time(2011, 3, 25) + 25).to_tuple
    {{2011, 3, 25}, {0,1,25}} = (date_time(2011, 3, 25) + 85).to_tuple
    {{2011, 3, 25}, {1,1,25}} = (date_time(2011, 3, 25) + 3685).to_tuple
    {{2011, 3, 26}, {1,1,25}} = (date_time(2011, 3, 25) + 90085).to_tuple
  end

  def subtraction_test
    {{2011, 3, 24}, {23,59,35}} = (date_time(2011, 3, 25) - 25).to_tuple
    {{2011, 3, 24}, {23,58,35}} = (date_time(2011, 3, 25) - 85).to_tuple
    {{2011, 3, 24}, {22,58,35}} = (date_time(2011, 3, 25) - 3685).to_tuple
    {{2011, 3, 23}, {22,58,35}} = (date_time(2011, 3, 25) - 90085).to_tuple
  end

  def to_i_test
    current = DateTime.utc
    future  = current.to_i + 85
    ~future = (current + 85).to_i
  end

  private

  def date_time(year, month, day)
    DateTime.new({{year, month, day}, {0,0,0}})
  end
end
