% A tail call optimized fibonacci.
module Math
  def fibonacci(n)
    fibonacci(n, 1, 0)
  end

  def fibonacci(0, _, result)
    result
  end

  def fibonacci(n, next, result)
    fibonacci(n - 1, next + result, next)
  end
end

Math.fibonacci(0)   % => 0
Math.fibonacci(1)   % => 1
Math.fibonacci(3)   % => 2
Math.fibonacci(10)  % => 55