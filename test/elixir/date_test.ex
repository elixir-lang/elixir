Code.require File.expand_path("../test_helper", __FILE__)
Code.require "date"

object DateTest
  proto ExUnit::Case

  def general_test
    'Date = Date.today.__parent_name__ 
     true = Date.new({2012, 12, 21}).leap_year?
     {2012, 12, 21} = Date.new({2012, 12, 21}).to_dict
     "2012-12-21" = Date.new({2012, 12, 21}).to_s
     31 = Date.days_in_month(Date.new({2012, 12, 21}))
     30 = Date.days_in_month(Date.new({2011, 4, 21}))
     29 = Date.days_in_month(Date.new({2012, 2, 21}))
     28 = Date.days_in_month(Date.new({2011, 2, 21}))
  end

  def subtraction_test
   {2011, 2, 28} = (Date.new({2011, 3, 25}) - 25).to_dict
   {2011, 1, 31} = (Date.new({2011, 3, 25}) - 53).to_dict
   {2010, 12, 31} = (Date.new({2011, 3, 25}) - 84).to_dict
   {2010, 12, 11} = (Date.new({2011, 3, 25}) - 104).to_dict
   {2010, 11, 30} = (Date.new({2011, 3, 25}) - 115).to_dict
   {2008, 6, 28} = (Date.new({2011, 3, 25}) - 1000).to_dict
  end

  def addition_test
   {2011, 3, 27} = (Date.new({2011, 3, 25}) + 2).to_dict
   {2011, 4, 1} = (Date.new({2011, 3, 25}) + 7).to_dict
   {2011, 5, 1} = (Date.new({2011, 3, 25}) + 37).to_dict
   {2012, 3, 24} = (Date.new({2011, 3, 25}) + 365).to_dict
  end

end
