Code.require_file "../test_helper", __FILE__

module FunctionTest
  mixin ExUnit::Case

  def arity_test
    0 = (-> 'a).arity
    1 = (-> (a) a).arity
    2 = (-> (a,b) a + b).arity
  end

  def syntax_call_test
    0 = (-> 0).()
    1 = (-> (a) a).(1)
    2 = (-> (a,b) a + b).(1,1)
  end

  def call_test
    0 = (-> 0).call
    1 = (-> (a) a).call 1
    2 = (-> (a,b) a + b).call 1,1
  end

  def brackets_test
    0 = (-> 0)[]
    1 = (-> (a) a)[1]
    2 = (-> (a,b) a + b)[1,1]
  end

  def apply_test
    0 = (-> 0).apply []
    1 = (-> (a) a).apply [1]
    2 = (-> (a,b) a + b).apply [1,1]
  end

  def anonymous_receiver_function_test
    fun = _.arity
    'Function = fun.__parent_name__
    1 = fun.(-> (a) a)

    sum = _.+(2)
    5 = sum.(3)
  end

  def match_arg_test
    [1,2,3] = match_arg1([])
    [1,2,3] = match_arg2([])

    self.assert_error 'function_clause, do
      match_arg1({})
    end
  end

  private

  def match_arg1(x = [])
    x + [1,2,3]
  end

  def match_arg2([] = x)
    x + [1,2,3]
  end

end
