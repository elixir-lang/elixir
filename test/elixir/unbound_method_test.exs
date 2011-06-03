Code.require_file "../test_helper", __FILE__

module UnboundMethodTest
  mixin ExUnit::Case

  module Sample
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

    attr_reader ['foo, 'bar, 'baz]
  end

  % Hook used for the binding
  def y
    13
  end

  def def_returns_method_object_test
    method = UnboundMethodTest::Sample.foo
    'foo = method.name
    1 = method.arity
    'exUnboundMethodTest::Sample = method.owner
  end

  def anonymous_methods_test
    method = UnboundMethodTest::Sample.bar
    '__anonymous_method_UnboundMethodTest::Sample_4 = method.name
    2 = method.arity
    'exUnboundMethodTest::Sample = method.owner
  end

  def apply_to_test
    3 = UnboundMethodTest::Sample.bar.apply_to(Module.blank_slate, [1,2])
  end

  def binding_and_call_test
    method = UnboundMethodTest::Sample.bar.bind(Module.blank_slate)
    3 = method.apply [1,2]
    3 = method.call(1,2)
    3 = method[1,2]
  end

  def binding_and_call_with_self_test
    method = UnboundMethodTest::Sample.baz.bind(Module.blank_slate).bind(self)
    23 = method.apply [10]
    23 = method.call(10)
    23 = method[10]
  end
end
