Code.require_file "../test_helper", __FILE__

module BindTest
  mixin ExUnit::Case

  module Math
    def one
      1
    end
  end

  module CrazyMath
    def one
      11
    end
  end

  module ModuleBound
    def __bound__
      BindTest::ModuleBound
    end
  end

  module BuiltinBound
    def __bound__
      1
    end
  end

  % TODO: Make parens optional

  def bind_test
    1 = Module.blank_slate#BindTest::Math().one
  end

  def slate_bind_test
    1 = #BindTest::Math().one
  end

  def module_question_mark_test
    true  = BindTest::Math.__module__?
    false = #BindTest::Math().__module__?
  end

  def precedence_test
    thing = mirror #BindTest::Math()
    1 = thing.one
  end

  def rebind_test
    sample = #BindTest::Math()
    1  = sample.one
    11 = sample#BindTest::CrazyMath().one
  end

  def bind_method_test
    sample = #BindTest::Math()
    1  = sample.one
    11 = sample.__bind__(BindTest::CrazyMath).one
  end

  def builtin_binding_test
    assert_error { 'builtin_not_allowed, { '__bind__, 1 } }, do
      1#BindTest::Math()
    end
  end

  def module_only_test
    assert_error { 'not_a_module, 1 }, do
      var = 1
      Module.blank_slate#var()
    end
  end

  def assert_same_test
    assert_error { 'bad_binding, { 'BindTest::ModuleBound, BindTest::ModuleBound } }, do
      #BindTest::ModuleBound()
    end

    assert_error { 'bad_binding, { 'BindTest::BuiltinBound, 1 } }, do
      #BindTest::BuiltinBound()
    end
  end

  private

  def mirror(thing)
    thing
  end
end