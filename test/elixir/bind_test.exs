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
  % TODO: Test inspect

  def bind_test
    1 = Module.blank_slate#BindMath().one
  end

  def slate_bind_test
    1 = #BindMath().one
  end
end