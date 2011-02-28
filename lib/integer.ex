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

  def div(other)
    Erlang.elixir_numeric_methods.integer_div(self, other)
  end

  def rem(other)
    Erlang.elixir_numeric_methods.integer_rem(self, other)
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