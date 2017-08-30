Code.require_file "test_helper.exs", __DIR__

defmodule PortTest do
  use ExUnit.Case, async: true

  test "info/1,2 with registered name" do
    {:ok, port} = :gen_udp.open(0)

    assert Port.info(port, :links) == {:links, [self()]}
    assert Port.info(port, :registered_name) == {:registered_name, []}

    Process.register(port, __MODULE__)

    assert Port.info(port, :registered_name) == {:registered_name, __MODULE__}

    :ok = :gen_udp.close(port)

    assert Port.info(port, :registered_name) == nil
    assert Port.info(port) == nil
  end

  test "monitoring functions are inlined by the compiler" do
    assert expand(quote(do: Port.monitor(port())), __ENV__) ==
           quote(do: :erlang.monitor(:port, port()))

    assert expand(quote(do: Port.demonitor(port())), __ENV__) ==
           quote(do: :erlang.demonitor(port()))
  end

  defp expand(expr, env) do
    {expr, _env} = :elixir_expand.expand(expr, env)
    expr
  end
end
