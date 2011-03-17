Code.require File.expand_path("../test_helper", __FILE__)

object ExUnitTest
  proto ExUnit::Case

  def setup(_)
    @('foo, 1)
  end

  def setup_test
    1 = @foo
  end

  def teardown('setup_test)
    1 = @foo
  end

  def teardown(_)
  end
end
