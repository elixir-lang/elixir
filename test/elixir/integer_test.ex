Code.require File.expand_path("../test_helper", __FILE__)

object IntegerTest
  proto ExUnit::Case

  def abs_test
    1 = -1.abs
    1 = 1.abs
    0 = 0.abs
    -1 = -(1).abs
  end

  def times_with_arity_0_test
    reset_counter

    3.times do
      increment_counter
    end

    3 = get_counter
  end

  def times_with_arity_1_test
    reset_counter

    3.times do (i)
      increment_counter
      i = get_counter
    end

    3 = get_counter
  end

  def times_with_arity_2_test
    reset_counter

    6 = 3.times 0, do (i, acc)
      increment_counter
      i = get_counter
      acc + i
    end

    3 = get_counter
  end

  private

  def reset_counter
    Erlang.put('times_counter, 0)
  end

  def get_counter
    Erlang.get('times_counter)
  end

  def increment_counter
    Erlang.put('times_counter, get_counter + 1)
  end
end
