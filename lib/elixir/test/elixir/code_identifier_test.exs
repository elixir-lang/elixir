Code.require_file("test_helper.exs", __DIR__)

defmodule Code.IdentifierTest do
  use ExUnit.Case, async: true
  doctest Code.Identifier
end
