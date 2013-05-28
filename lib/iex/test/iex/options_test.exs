Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.Options.Test do
  use IEx.Case

  test "color" do
    opts = [colors: [enabled: true, eval_result: "red"]]
    assert capture_iex("1 + 2", opts) == "\e[31m3\e[0m"
  end

  test "inspect opts" do
    opts = [inspect: [limit: 3, raw: true]]
    assert capture_iex("[1,2,3,4,5]\nArgumentError[]", opts) ==
              "[1,2,3,...]\n{ArgumentError,:__exception__,\"argument error\"}"

    opts = [inspect: [raw: false]]
    assert capture_iex("ArgumentError[]", opts) == "ArgumentError[message: \"argument error\"]"
  end

  test "history size" do
    opts = [history_size: 3]
    assert capture_iex("1\n2\n3\nv(1)", opts) == "1\n2\n3\n1"
    assert "1\n2\n3\n4\n** (RuntimeError) Out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(1)", opts)
    assert "1\n2\n3\n4\n** (RuntimeError) Out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(-4)", opts)
    assert "1\n2\n3\n4\n2\n** (RuntimeError) Out of bounds" <> _ = capture_iex("1\n2\n3\n4\nv(2)\nv(2)", opts)
  end
end

