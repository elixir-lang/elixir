Code.require File.expand_path("../test_helper", __FILE__)

object Code::FormatterTest
  proto ExUnit::Case

  def format_object_test
    "[]" = Code::Formatter.format_object([])
    "{1,2,3}" = Code::Formatter.format_object({1,2,3})
    "[1,2,3]" = Code::Formatter.format_object([1,2,3])
    "abc" = Code::Formatter.format_object([$a,$b,$c])
  end
end
