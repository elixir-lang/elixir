Code.require_file "test_helper.exs", __DIR__

defmodule CollectableTest do
  use ExUnit.Case, async: true

  doctest Collectable
end
