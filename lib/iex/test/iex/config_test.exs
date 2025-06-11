Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.ConfigTest do
  use ExUnit.Case, async: true

  import IEx.Config

  describe "prompt" do
    test "converts everything before opening parens to dots" do
      assert prompt(~c"iex(321)>") == ~c"...(321)>"
      assert prompt(~c"foo-bar(321)>") == ~c".......(321)>"
    end

    test "falls back to Erlang wit no parens around" do
      assert prompt(~c"foo-bar>") == ~c"     .. "
    end

    test "ignores ansi escapes" do
      assert prompt(~c"#{IO.ANSI.red()}iex(foo)>") == ~c"...(foo)>"
      assert prompt(~c"#{IO.ANSI.red()}foo-bar>") == ~c"     .. "
    end
  end
end
