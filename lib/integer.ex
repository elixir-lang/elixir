object Integer
  proto Numeric

  module Mixin
  end

  def inspect
    to_s
  end

  def to_s
    String.new Erlang.integer_to_list(self)
  end
end