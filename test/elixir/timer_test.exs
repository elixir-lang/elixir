Code.require_file "../test_helper", __FILE__

module TimerTest
  mixin ExUnit::Case

  def ms_test
    { ms, 3 } = Timer.ms -> 1 + 2
    true = Erlang.is_number(ms)
  end

  def ms_times_test
    { ms, 10 } = Timer.ms 10, -> 1 + 2
    true = Erlang.is_number(ms)
  end
end