Code.require_file "../test_helper", __FILE__

module MathTest
  mixin ExUnit::Case

  ['sin, 'cos, 'tan, 'atan, 'sinh, 'cosh, 'tanh, 'asinh, 'acosh, 'exp, 'log, 'log10, 'sqrt, 'erf, 'erfc].each do (method)
    module_eval __FILE__, __LINE__, ~~METHOD
    def #{method}_test
      Math.#{method}(3) == Erlang.math.#{method}(3)
    end
  ~~
  end

  def asin_test
    Math.asin(0.4) == Erlang.math.asin(0.4)
  end

  def acos_test
    Math.acos(0.4) == Erlang.math.acos(0.4)
  end

  def atanh_test
    Math.atanh(0.4) == Erlang.math.atanh(0.4)
  end

  def atan2_test
    Math.atan2(3,5) == Erlang.math.atan2(3,5)
  end

  def pow_test
    Math.pow(3,7) == Erlang.math.pow(3,7)
  end
end
