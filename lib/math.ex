module Math
  ['sin, 'cos, 'tan, 'asin, 'acos, 'atan, 'sinh, 'cosh, 'tanh, 'asinh, 'acosh, 'atanh, 'exp, 'log, 'log10, 'sqrt, 'erf, 'erfc].each do (method)
  module_eval __FILE__, __LINE__+1, ~~METHOD
    def #{method}(x)
      Erlang.math.#{method}(x)
    end
~~
  end

  def atan2(x,y)
    Erlang.math.atan2(x,y)
  end

  def pow(x,y)
    Erlang.math.pow(x,y)
  end
end
