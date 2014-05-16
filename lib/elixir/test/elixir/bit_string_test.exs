Code.require_file "test_helper.exs", __DIR__

defmodule BitStringTest do
  use ExUnit.Case, async: true

  test "to_list/1" do
    assert BitString.to_list(<<"a", 1 :: 4>>) ==
           [97, <<1::size(4)>>]
  end
end
