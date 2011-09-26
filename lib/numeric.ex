module Numeric

  % Returns the absolute value of self.
  def abs
    Erlang.abs(self)
  end

  % Returns square of self.
  def abs2
    self * self
  end

  def truncate
    Erlang.trunc(self)
  end

  def round
    Erlang.round(self)
  end

  def round(precision) when precision > 0
    _round_with_precision(precision)
  end

  def round(precision) when precision < 0
    _round_with_precision(precision).truncate
  end

  def round(_)
    round
  end

  % Returns the largest Integer less than or equal to self.
  def floor
    case self
    match x when x < 0 then (x - 1).truncate
    match x then x.truncate
    end
  end

  % Returns the smallest Integer greater than or equal to self.
  def ceil
    case self
    match x when x < 0 then x.truncate
    match x then (x + 1).truncate
    end
  end

  % Returns true if integer
  def integer?
    Erlang.is_integer self
  end

  % Returns true if zero
  def zero?
    self == 0
  end

  % Returns true if not zero
  def nonzero?
    !zero?
  end

  ['+, '-, '*, '/].each do (op)
    define_erlang_method __FILE__, __LINE__, op, 1, [
      {
        'clause, __LINE__, [{'var, __LINE__, 'other}], [], [
          { 'op, __LINE__, op, {'var, __LINE__, 'self}, {'var, __LINE__, 'other} }
        ]
      }
    ]
  end

  private

    def _round_with_precision(precision)
      magnitude = Math.pow(10, precision)
      (self * magnitude).round / magnitude
    end

end