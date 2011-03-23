% elixir: cache

module Numeric
  def abs
    Erlang.abs(self)
  end

  ['+, '-, '*, '/].each do (op)
    define_erlang_method __FILE__, __LINE__, op, 1, [
      {
        'clause, __LINE__, [{'var, __LINE__, 'other}], [], [
          { 'op, __LINE__, op, {'var, __LINE__, 'self}, {'var, __LINE__, 'other} }
        ]
      }
    ]
  end
end