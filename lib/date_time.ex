object DateTime
  module Mixin
    def utc
      self.new(Erlang.calendar.universal_time)
    end
  end

  def initialize({date, time})
    @('date: date.to_tuple, 'time: time.to_tuple)
  end

  def +(value)
    DateTime.new Erlang.calendar.gregorian_seconds_to_datetime(to_i + value)
  end

  def -(value)
    DateTime.new Erlang.calendar.gregorian_seconds_to_datetime(to_i - value)
  end

  def to_i
    Erlang.calendar.datetime_to_gregorian_seconds({@date,@time})
  end

  def to_tuple
    { @date, @time }
  end
end