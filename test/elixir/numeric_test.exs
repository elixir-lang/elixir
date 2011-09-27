Code.require_file "../test_helper", __FILE__

module NumericTest
  mixin ExUnit::Case

  def abs_test
    1 = -1.abs
    1 = 1.abs
    1.0 = -1.0.abs
    1.0 = 1.0.abs
    0 = 0.abs
    -1 = -(1).abs
  end

  def abs2_test
    1 = -1.abs2
    1 = 1.abs2
    4 = 2.abs2
    16.0 = -4.0.abs2
  end

  def round_test
    3 = 3.round
    3 = 3.4.round
    4 = 3.5.round
    4 = 4.0.round
    -3 = -3.4.round
  end

  def round_with_positive_precision_test
    3.0 = 3.round(2)
    3 = 3.1.round(0)
    3.123 = 3.1234.round(3)
    3.124 = 3.1235.round(3)
  end

  def round_with_negative_precision_test
    100   = 123.4.round(-2)
    0     = 123.4.round(-5)
  end

  def truncate_test
    % 3 = 3.truncate
    3 = 3.4.truncate
    3 = 3.5.truncate
    4 = 4.0.truncate
    -3 = -3.4.truncate
  end

  def floor_test
    3 = 3.floor
    3 = 3.4.floor
    3 = 3.5.floor
    4 = 4.0.floor
    -4 = -3.4.floor
  end

  def ceil_test
    4 = 3.ceil
    4 = 3.4.ceil
    4 = 3.5.ceil
    5 = 4.0.ceil
    -3 = -3.4.ceil
  end

  def integer_test
    true = 0.integer?
    false = 0.0.integer?
    true = 1.integer?
    false = 1.0.integer?
    true = -1.integer?
    false = -1.0.integer?
  end

  def zero_test
    true = 0.zero?
    true = 0.0.zero?
    false = 1.zero?
    false = 1.0.zero?
    false = -1.zero?
    false = -1.0.zero?
  end

  def nonzero_test
    false = 0.nonzero?
    false = 0.0.nonzero?
    true = 1.nonzero?
    true = 1.0.nonzero?
    true = -1.nonzero?
    true = -1.0.nonzero?
  end

end
