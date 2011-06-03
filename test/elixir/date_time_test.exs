Code.require_file "../test_helper", __FILE__

module DateTimeTest
  mixin ExUnit::Case

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

  def accessors_test
    datetime = DateTime.new({{2011, 3, 24}, {23,59,35}})
    2011 = datetime.year
    3    = datetime.month
    24   = datetime.day
    23   = datetime.hours
    59   = datetime.minutes
    35   = datetime.seconds
  end

  def inspect_test
    datetime = DateTime.new({{2011, 3, 4}, {3,9,5}})
    "2011-03-04 03:09:05" = datetime.inspect
  end

  def to_i_test
    current = DateTime.utc
    future  = current.to_i + 85
    ~future = (current + 85).to_i
  end

  def rfc1123_test
    "Thu, 01-Jan-1970 00:00:01 GMT" = DateTime.new({{1970,1,1},{0,0,1}}).rfc1123
  end

  private

  def date_time(year, month, day)
    DateTime.new({{year, month, day}, {0,0,0}})
  end
end
