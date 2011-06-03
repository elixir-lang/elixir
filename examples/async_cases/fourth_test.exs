module FourthTest
  mixin ExUnit::Case

  def sleep_test
    Erlang.timer.sleep(2000)
  end
end