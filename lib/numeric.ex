module Numeric
  def +(other)
    erl.elixir_methods.add(self, other)
  end

  def -(other)
    erl.elixir_methods.subtract(self, other)
  end

  def *(other)
    erl.elixir_methods.multiply(self, other)
  end

  def /(other)
    erl.elixir_methods.divide(self, other)
  end

  def @+
    erl.elixir_methods.unary_plus(self)
  end

  def @-
    erl.elixir_methods.unary_minus(self)
  end
end