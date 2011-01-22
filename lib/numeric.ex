module Numeric
  def +(other)
    Erlang.elixir_numeric_methods.add(self, other)
  end

  def -(other)
    Erlang.elixir_numeric_methods.subtract(self, other)
  end

  def *(other)
    Erlang.elixir_numeric_methods.multiply(self, other)
  end

  def /(other)
    Erlang.elixir_numeric_methods.divide(self, other)
  end
end