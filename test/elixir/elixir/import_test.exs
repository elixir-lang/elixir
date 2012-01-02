defmodule Elixir::ImportOnlyTest do
  use ExUnit::Case

  import Erlang.lists, only: [flatten: 1]
  import Erlang.other, only: []

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end

defmodule Elixir::ImportAllTest do
  use ExUnit::Case

  import Erlang.lists

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end

defmodule Elixir::ImportExceptTest do
  use ExUnit::Case

  import Erlang.lists, except: [each: 1]

  def test_import_erlang do
    [1,2,3] = flatten [1,[2],3]
  end
end