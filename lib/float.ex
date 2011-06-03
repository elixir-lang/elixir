module Float
  module Instance
    mixin Numeric

    def inspect
      to_s
    end

    def to_s
      Erlang.float_to_list(self).to_bin
    end
  end
end