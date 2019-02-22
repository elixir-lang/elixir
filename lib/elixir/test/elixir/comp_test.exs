Code.require_file("test_helper.exs", __DIR__)

defmodule CompTest do
  use ExUnit.Case, async: true
  doctest Comp
end
