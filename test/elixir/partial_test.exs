Code.require_file "../test_helper", __FILE__

module PartialTest
  mixin ExUnit::Case

  module Car
    def __bound__(color)
      @('color, color)
    end

    attr_reader ['color]
  end

  def partially_apply_module_bound_test
    1 = #PartialTest::Car(_).arity
    'green = #PartialTest::Car(_).('green).color
  end

  def partially_apply_anonymous_function_test
    2 = (-> (x, y) x + y).(_, 1).(1)
  end

  def partially_apply_function_test
    add = -> (x, y) x + y
    addOne = add.(1, _)
    3 = addOne.(2)
  end

  def partially_apply_function_with_many_argumentes_test
    sum = -> (a, b, c, d) a + b + c + d
    4 = (sum.(_, 1, 1, 1).apply [1])
    3 = (sum.(_, _, 1, 1).apply [1, 0])
    2 = (sum.(_, _, _, 1).apply [1, 0, 0])
    1 = (sum.(_, _, _, _).apply [1, 0, 0, 0])
  end
end
