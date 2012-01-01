module Elixir::LoopTest do
  use ExUnit::Case

  def test_loop do
    list = [1,2,3]

    [6,4,2] = loop { list, [] } do
    match: { [h|t], acc }
      recur { t, [h*2|acc] }
    match: { [], acc }
      acc
    end
  end
end