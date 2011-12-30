module Elixir::ImportTest do
  use ExUnit::Case

  import Erlang.lists, only: [flatten: 1]

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end