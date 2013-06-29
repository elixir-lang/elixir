Code.require_file "test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case, async: true

  test :self do
    assert is_pid(Process.self)
  end
end
