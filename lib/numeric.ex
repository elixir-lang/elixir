module Numeric

  def abs
    Erlang.abs(self)
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

  def floor
    case self
    match x when x < 0 then (x - 1).truncate
    match x then x.truncate
    end
  end

  def ceil
    case self
    match x when x < 0 then x.truncate
    match x then (x + 1).truncate
    end
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