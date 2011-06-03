Code.require_file "../../test_helper", __FILE__

module Code::FormatterTest
  mixin ExUnit::Case

  def format_object_test
    "[]" = Code::Formatter.format_object([])
    "{1,2,3}" = Code::Formatter.format_object({1,2,3})
    "[1,2,3]" = Code::Formatter.format_object([1,2,3])
    "abc" = Code::Formatter.format_object([$a,$b,$c])
  end
end
