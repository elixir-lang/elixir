Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.ProxyIOTest do
  use ExUnit.Case, async: true

  alias ExUnit.ProxyIO

  test "open and close" do
    {:ok, pid} = ProxyIO.open(:stdio)
    assert ProxyIO.close(pid) == :ok
  end

  test "proxying" do
    {:ok, pid} = StringIO.open("one")
    {:ok, proxy} = ProxyIO.open(pid)

    assert IO.gets(proxy, ">") == "one"
    assert IO.getn(proxy, ">") == :eof
    assert IO.write(proxy, "two") == :ok
    assert IO.puts(proxy, "three") == :ok

    assert StringIO.contents(pid) == {"", "twothree\n"}
  end
end
