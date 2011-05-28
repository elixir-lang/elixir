Code.require_file "../test_helper", __FILE__

object UnboundMethodTest
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
    method = UnboundMethodTest::Sample.foo
    'foo = method.name
    1 = method.arity
    'UnboundMethodTest::Sample = method.owner.__name__
  end

  def anonymous_methods_test
    method = UnboundMethodTest::Sample.bar
    '__anonymous_method_UnboundMethodTest::Sample_4 = method.name
    2 = method.arity
    'UnboundMethodTest::Sample = method.owner.__name__
  end

end
