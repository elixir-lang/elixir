Code.require_file "../test_helper", __FILE__

object MethodTest
  proto ExUnit::Case

  object Sample
    set_ivar 'foo, def foo(x) 
      x + 1
    end

    bar = def(x, y)
      x + y
    end

    @('bar, bar)

    module Mixin
      attr_reader ['foo, 'bar]
    end
  end

  def def_returns_method_object_test
    method = MethodTest::Sample.foo
    'foo = method.name
    1 = method.arity
    'MethodTest::Sample = method.owner.__name__
  end

  def anonymous_methods_test
    method = MethodTest::Sample.bar
    '__anonymous_method_MethodTest::Sample_4 = method.name
    2 = method.arity
    'MethodTest::Sample = method.owner.__name__
  end

end
