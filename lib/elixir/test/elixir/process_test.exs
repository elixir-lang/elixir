Code.require_file "test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case, async: true

  test :self do
    assert is_pid(Process.self)
  end

  test :group_leader do
    another = spawn_link(fn -> :timer.sleep(1000) end)
    assert Process.group_leader(self, another)
    assert Process.group_leader == another
  end
end
