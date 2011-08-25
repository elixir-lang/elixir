module Math
  delegate ['sin/1, 'cos/1, 'tan/1, 'asin/1, 'acos/1, 'atan/1, 'sinh/1,
            'cosh/1, 'tanh/1, 'asinh/1, 'acosh/1, 'atanh/1, 'exp/1,
            'log/1, 'log10/1, 'sqrt/1, 'erf/1, 'erfc/1, 'atan2/2,
            'pow/2], 'to: "Erlang.math"
end
