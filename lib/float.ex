object Float
  proto Numeric

  module Mixin
  end

  def inspect
    to_s
  end

  def to_s
    Erlang.float_to_list(self).to_bin
  end
end