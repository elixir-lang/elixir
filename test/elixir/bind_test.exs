Code.require_file "../test_helper", __FILE__

module BindMath
  def one
    1
  end
end

module BindTest
  mixin ExUnit::Case

  % TODO: Test exceptions
  % TODO: Make parens optional
  % TODO: Test blank slate

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

  private

  def mirror(thing)
    thing
  end
end