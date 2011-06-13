module Float
  module Behavior
    mixin Numeric

    def inspect
      Erlang.float_to_list(self).to_bin
    end
  end
end