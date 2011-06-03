Code.require_file "../test_helper", __FILE__

module UnboundMethodTest
  mixin ExUnit::Case

  object Sample
    set_ivar 'foo, def foo(x) 
      x + 1
    end

    bar = def(x, y)
      x + y
    end

    @('bar, bar)

    @('baz, def(x)
      x + self.y
    end)

    module Mixin
      attr_reader ['foo, 'bar, 'baz]
    end
  end

  % Hook used for the binding
  def y
    13
  end

  def def_returns_method_object_test
    method = UnboundMethodTest::Sample.foo
    'foo = method.name
    1 = method.arity
    'UnboundMethodTest::Sample = method.owner
  end

  def anonymous_methods_test
    method = UnboundMethodTest::Sample.bar
    '__anonymous_method_UnboundMethodTest::Sample_4 = method.name
    2 = method.arity
    'UnboundMethodTest::Sample = method.owner
  end

  def binding_and_call_test
    method = UnboundMethodTest::Sample.bar.bind(Object.new)
    3 = method.apply [1,2]
    3 = method.call(1,2)
    3 = method[1,2]
  end

  def binding_and_call_with_self_test
    method = UnboundMethodTest::Sample.baz.bind(Object.new).bind(self)
    23 = method.apply [10]
    23 = method.call(10)
    23 = method[10]
  end
end
