Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.ServerTest do
  use IEx.Case

  # Options

  test "prefix option" do
    assert capture_io(fn ->
      boot([prefix: "pry"])
    end) =~ "pry(1)> "
  end

  test "env option" do
    assert capture_io("__ENV__.file", fn ->
      boot([env: __ENV__])
    end) =~ "server_test.exs"
  end

  # Take over

  test "allows take over of the shell during boot" do
    assert capture_io("Y\na+b", fn ->
      server = self()
      boot([], fn ->
        opts = [prefix: "dbg", binding: [a: 1, b: 2]]
        IEx.Server.take_over("iex:13", opts, 1000, server)
      end)
    end) =~ "dbg(1)> "
  end

  test "does not operate if callback during boot fails" do
    assert capture_io(fn ->
      boot([], fn -> exit(0) end)
    end) == ""
  end

  test "take over fails when there is no shell" do
    assert IEx.Server.take_over("iex:13", [], 10) == {:error, :no_iex}
  end

  test "pry wraps around take over" do
    require IEx
    assert capture_io(fn ->
      assert IEx.pry == {:error, :no_iex}
    end) =~ "Is an IEx shell running?"
  end

  # Helpers

  defp boot(opts, callback \\ fn -> nil end) do
    IEx.Server.start(Keyword.merge([dot_iex_path: ""], opts),
                     {:erlang, :apply, [callback, []]})
  end
end
