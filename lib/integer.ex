object Integer
  proto Numeric

  module Mixin
  end

  def to_s
    String.new Erlang.integer_to_list(self)
  end
end