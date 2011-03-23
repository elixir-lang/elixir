% elixir: cache

object Integer
  proto Numeric

  module Mixin
  end

  def inspect
    to_s
  end

  def times(function)
    if function.arity == 0
      times_0(self, 1, function)
    else
      times_1(self, 1, function)
    end
  end

  def times(acc, function)
    times_2(self, 1, function, acc)
  end

  ['div, 'rem].each do (op)
    define_erlang_method __FILE__, __LINE__, op, 1, [
      {
        'clause, __LINE__, [{'var, __LINE__, 'other}], [], [
          { 'op, __LINE__, op, {'var, __LINE__, 'self}, {'var, __LINE__, 'other} }
        ]
      }
    ]
  end

  def chr
    String.new <<self>>
  end

  def to_s
    String.new Erlang.integer_to_list(self)
  end

  private

  def times_0(limit, counter, function)
    if counter <= limit
      function()
      times_0(limit, counter + 1, function)
    end
  end

  def times_1(limit, counter, function)
    if counter <= limit
      function(counter)
      times_1(limit, counter + 1, function)
    end
  end

  def times_2(limit, counter, function, acc)
    if counter <= limit
      new_acc = function(counter, acc)
      times_2(limit, counter + 1, function, new_acc)
    else
      acc
    end
  end
end