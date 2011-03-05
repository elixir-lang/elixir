Code.require File.expand_path("../test_helper", __FILE__)

object FunctionTest
  proto ExUnit::Case

  def arity_test
    0 = (-> 'a).arity
    1 = (-> (a) a).arity
    2 = (-> (a,b) a + b).arity
  end
end
