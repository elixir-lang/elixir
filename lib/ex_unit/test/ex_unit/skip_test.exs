Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.SkipTest do
  use ExUnit.Case, async: true

  skip_test "this test is skipped" do
    raise "This should never be executed."
  end

end
