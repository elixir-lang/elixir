Code.require_file "test_helper.exs", __DIR__

defmodule RandomTest do
  use ExUnit.Case, async: true

  test :seed do
    timestamp = {a1, a2, a3} = :erlang.now

    assert is_tuple(Random.seed)
    assert is_tuple(Random.seed(a1, a2, a3))
    assert is_tuple(Random.seed(timestamp))
  end

  test :initial_seed do
    seed = Random.initial_seed
    assert is_tuple(seed)
  end

  test :uniform do
    random = Random.uniform
    other = Random.uniform
    assert is_float(random)
    assert random != other

    random = Random.uniform(10)
    assert random <= 10
    assert is_integer(random)
  end
end
