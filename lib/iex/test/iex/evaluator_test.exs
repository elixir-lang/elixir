Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.EvaluatorTest do
  use ExUnit.Case, async: true

  alias IEx.Evaluator, as: E

  test "calculate_width selects correct column" do
    assert E.calculate_width([{"a", "bbb", "ccccc"}], 0) == 1
    assert E.calculate_width([{"a", "bbb", "ccccc"}], 1) == 3
    assert E.calculate_width([{"a", "bbb", "ccccc"}], 2) == 5
  end

  test "calculate_width finds a maximum" do
    assert E.calculate_width([{"a",    "b", "c"},
                              {"aaa",  "b", "c"},
                              {"a",    "b", "c"}], 0) == 3
  end

  test "calculate_width returns 0 if the different in widths is > 8" do
    assert E.calculate_width([{"a",    "b", "c"},
                              {"aaaaaaaaa",  "b", "c"},
                              {"a",    "b", "c"}], 0) == 0
  end

  test "pretty_stacktrace returns formatted result in columns" do
    frames = [
      { List,   :one,    1, [file: "loc",    line: 1] },
      { String, :second, 2, [file: "loc2",   line: 102] },
      { IEx,    :three,  3, [file: "longer", line: 1234] },
      { List,   :four,   4, [file: "loc",    line: 1] },

    ]
    IEx.Options.set :colors, enabled: false

    expected = """
      (elixir) loc:1:       List.one/1
      (elixir) loc2:102:    String.second/2
         (iex) longer:1234: IEx.three/3
      (elixir) loc:1:       List.four/4
    """

    assert E.pretty_stacktrace(frames) <> "\n" == expected
  end

  test "pretty_stacktrace doesn't use columns if the widths are disparate" do
    frames = [
      { List,   :one,    1, [file: "loc",    line: 1] },
      { String, :second, 2, [file: "loc2",   line: 102] },
      { IEx,    :three,  3, [file: "longerandlonger", line: 1234] },
      { List,   :four,   4, [file: "loc",    line: 1] },

    ]
    IEx.Options.set :colors, enabled: false

    expected = """
      (elixir) loc:1: List.one/1
      (elixir) loc2:102: String.second/2
         (iex) longerandlonger:1234: IEx.three/3
      (elixir) loc:1: List.four/4
    """
    IO.puts  E.pretty_stacktrace(frames)

    assert E.pretty_stacktrace(frames) <> "\n" == expected
  end

end
