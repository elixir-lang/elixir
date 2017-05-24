% elixir: cache
object Date

  attr_reader ['day, 'month, 'year]

  module Mixin

    def today
      Date.new(Erlang.date)
    end
 
    def tomorrow      
      Date.today.tomorrow
    end

    def yesterday
      Date.today.yesterday
    end
    
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

  end
 
  def initialize(date_tuple)
    {year, month, day} = date_tuple
    @('year: year, 'month: month, 'day: day)
  end

  def to_s
    "#{@year}-#{convert_to_double_digit(@month)}-#{convert_to_double_digit(@day)}"
  end
  
  def leap_year?
    Erlang.calendar.is_leap_year(self.year)  
  end

  def tomorrow
    self + 1
  end

  def yesterday
    self - 1
  end

  def -(days)
    {year, month, day} = self.to_dict
    if days < day
      Date.new({year, month, day - days})
    else
      complex_subtraction(days, {year, month, day})
    end
  end

  def +(days)
    {year, month, day} = self.to_dict
    if (day + days) < Date.days_in_month(self)
      Date.new({year, month, day + days})
    else
      complex_addition(days, {year, month, day})
    end
  end

  def to_dict
   {self.year, self.month, self.day}
  end

  private
 
  def complex_subtraction(0, date)
    Date.new(date)
  end

  def complex_addition(0, date)
    Date.new(date)
  end

  def complex_subtraction(days, date)

    {year, month, day} = date
    days_left_to_subtract = Erlang.abs(day - days)
    
    if month == 1 
      complex_subtraction( days_left_to_subtract, {year - 1, 12, 31})
    elsif (day - days) > 0 
      complex_subtraction(0, {year, month, days_left_to_subtract })
    else 
      new_month = month - 1
      complex_subtraction(days_left_to_subtract, {year, new_month, Date.days_in_month(Date.new({year, new_month, 1}) )})
    end

  end

  def complex_addition(days, date)

    {year, month, day} = date
    days_left_to_add = Erlang.abs(days - (Date.days_in_month(Date.new(date)) - day) )
   
    if month == 12
      Date.new({ year + 1, 1, 0}) + days_left_to_add
    else
      Date.new({year, month + 1, 0}) + days_left_to_add
    end

  end

  def convert_to_double_digit(unit)
    if unit < 10
      "0" + unit.to_s 
    else
      unit.to_s
    end
  end

end
