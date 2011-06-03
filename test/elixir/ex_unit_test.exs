Code.require_file "../test_helper", __FILE__

module ExUnitTest
  mixin ExUnit::Case

  def setup(_)
    @('foo, 1)
  end

  def setup_test
    1 = @foo
  end

  def teardown_test
    fixture = OS.cmd("bin/exunit test/elixir/fixtures/ex_unit_failure.exs")
    self.assert_include "{'badmatch,1}", fixture
    self.assert_include "{'badmatch,3}", fixture
    self.assert_include "EVEN ON FAILURES", fixture
  end
end
