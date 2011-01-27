object Float
  proto Numeric

  module Mixin
  end

  def inspect
    to_s
  end

  def to_s
    String.new Erlang.float_to_list(self)
  end
end