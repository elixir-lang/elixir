Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.EvaluatorTest do
  use ExUnit.Case, async: true

  alias IEx.Evaluator, as: E

  test "format_stacktrace returns formatted result in columns" do
    frames = [
      {List,   :one,    1, [file: "loc",    line: 1]},
      {String, :second, 2, [file: "loc2",   line: 102]},
      {IEx,    :three,  3, [file: "longer", line: 1234]},
      {List,   :four,   4, [file: "loc",    line: 1]},
    ]

    expected = """
        (elixir) loc:1: List.one/1
        (elixir) loc2:102: String.second/2
           (iex) longer:1234: IEx.three/3
        (elixir) loc:1: List.four/4
    """

    assert E.format_stacktrace(frames) <> "\n" == expected
  end
end
