Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.Helpers.Test do
  use IEx.Case

  test "h helper" do
    assert "# IEx.Helpers\n\nWelcome to Interactive Elixir" <> _ = capture_iex("h")
  end

  test "h helper module" do
    assert "# Enumerable\n\nThis is the protocol used by the `Enum` module" <> _ = capture_iex("h Enumerable")
    assert capture_iex("h :whatever") == "Could not load module :whatever: nofile\n:ok"
  end

  test "h helper function" do
    expand_1_re = %r/\* def expand\(path\)\n\nConverts the path to an absolute one and expands/
    expand_2_re = %r/\* def expand\(path, relative_to\)\n\nExpands the path relative to the path given as the second argument/

    assert capture_iex("h Path.expand/1") =~ expand_1_re
    assert capture_iex("h Path.expand/2") =~ expand_2_re

    output = capture_iex("h Path.expand")
    assert output =~ expand_1_re
    assert output =~ expand_2_re

    assert capture_iex("h pwd") == "* def pwd()\n\nPrints the current working directory.\n\n:ok"
  end
end
