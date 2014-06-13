Code.require_file "test_helper.exs", __DIR__

defmodule ProcessTest do
  use ExUnit.Case, async: true

  test "group_leader/2 and group_leader/0" do
    another = spawn_link(fn -> :timer.sleep(1000) end)
    assert Process.group_leader(self, another)
    assert Process.group_leader == another
  end

  test "monitoring functions are inlined by the compiler" do
    assert expand(quote(do: Process.monitor(pid())), __ENV__) ==
           quote(do: :erlang.monitor(:process, pid()))
  end

  test "info/2" do
    pid = spawn fn -> end
    Process.exit(pid, :kill)
    assert Process.info(pid, :backtrace) == nil
  end

  test "info/2 with registered name" do
    pid = spawn fn -> end
    Process.exit(pid, :kill)
    assert Process.info(pid, :registered_name) ==
           nil

    assert Process.info(self, :registered_name) ==
           {:registered_name, nil}

    Process.register(self, __MODULE__)
    assert Process.info(self, :registered_name) ==
           {:registered_name, __MODULE__}
  end

  defp expand(expr, env) do
    {expr, _env} = :elixir_exp.expand(expr, env)
    expr
  end
end
