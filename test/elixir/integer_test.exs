Code.require_file "../test_helper", __FILE__

module IntegerTest
  mixin ExUnit::Case

  def abs_test
    1 = -1.abs
    1 = 1.abs
    0 = 0.abs
    -1 = -(1).abs
  end

  def separator_test
    1234 = 1_234
    -1_3_4 = -134
    12.34 = 1_2.34
    -1.3_4 = -1.34
  end

  def times_with_arity_0_test
    reset_counter

    3 = 3.times do
      increment_counter
    end

    3 = get_counter
  end

  def times_with_arity_1_test
    reset_counter

    3 = 3.times do (i)
      ~i = get_counter
      increment_counter
    end

    3 = get_counter
  end

  def times_with_arity_2_test
    reset_counter

    13 = 3.times 10, do (i, acc)
      ~i = get_counter
      increment_counter
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
