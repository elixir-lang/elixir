Code.require_file "test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case, async: true

  test "self/0" do
    assert is_pid(Process.self)
  end

  test "group_leader/2 and group_leader/0" do
    another = spawn_link(fn -> :timer.sleep(1000) end)
    assert Process.group_leader(self, another)
    assert Process.group_leader == another
  end

  test "monitoring functions are inlined by the compiler" do
    assert expand(quote(do: Process.monitor(pid())), __ENV__) ==
           quote(do: :erlang.monitor(:process, pid()))
  end

  defp expand(expr, env) do
    {expr, _env} = :elixir_exp.expand(expr, env)
    expr
  end
end
