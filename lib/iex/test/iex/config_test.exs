Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.ConfigTest do
  use ExUnit.Case, async: true

  test "configuration sets a default for width" do
    assert IEx.Config.configuration[:width] == 80
  end
end
