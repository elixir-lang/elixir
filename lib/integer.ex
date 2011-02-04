object Integer
  proto Numeric

  module Mixin
  end

  def inspect
    to_s
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
end