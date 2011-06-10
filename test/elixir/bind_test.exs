Code.require_file "../test_helper", __FILE__

module BindMath
  def one
    1
  end
end

module BindCrazyMath
  def one
    11
  end
end

module BindTest
  mixin ExUnit::Case

  % TODO: Test exceptions
  % TODO: Make parens optional

  def bind_test
    1 = Module.blank_slate#BindMath().one
  end

  def slate_bind_test
    1 = #BindMath().one
  end

  def module_question_mark_test
    true  = BindMath.__module__?
    false = #BindMath().__module__?
  end

  def precedence_test
    thing = mirror #BindMath()
    1 = thing.one
  end

  def rebind_test
    sample = #BindMath()
    1  = sample.one
    11 = sample#BindCrazyMath().one
  end

  def bind_method_test
    sample = #BindMath()
    1  = sample.one
    11 = sample.__bind__(BindCrazyMath).one
  end

  private

  def mirror(thing)
    thing
  end
end