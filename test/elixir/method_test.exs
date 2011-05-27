Code.require_file "../test_helper", __FILE__

object MethodTest
  proto ExUnit::Case

  object Sample
    set_ivar 'foo, def foo(x) 
      x + 1
    end

    bar = def bar(x, y)
      x + y
    end

    module Mixin
      def foo
        @foo
      end
    end
  end

  def def_returns_method_object_test
    {'foo,1} = MethodTest::Sample.foo
  end
end
