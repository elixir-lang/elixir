module Timer
  % Executes the given *function* and return a tuple with
  % the function result and the time spent in mileseconds.
  def ms(function)
    Erlang.timer.tc(function, [])
  end

  % Executes the given *function* the number of *times* given
  % and returns a tuple with the function result and *times*.
  def ms(times, function)
    Erlang.timer.tc(-> times.times(function), [])
  end
end