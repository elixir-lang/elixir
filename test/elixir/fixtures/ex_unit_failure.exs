ExUnit.configure {}

module ExUnitFailureTest
  mixin ExUnit::Case

  def setup('always_pass_test)
    @('foo, 1)
  end

  def setup(_)
    self
  end

  def always_pass_test
    true = true
  end

  def always_fail_test
    0 = 1 + 2
  end

  def teardown('always_pass_test)
    2 = @foo
  end
  
  def teardown('always_fail_test)
    IO.puts "EVEN ON FAILURES"
  end
end