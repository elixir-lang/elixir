Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.EscriptizeTest do
  use MixTest.Case

  test "generate simple escript" do
    in_fixture "escripttest", fn ->
      output = mix "escriptize"
      assert output =~ %r/Generated escript escripttest/
      assert System.cmd("escript escripttest") == "TEST\n"

      output = mix "escriptize"
      assert output == ""
    end
  end

  test "generate simple escript with path" do
    in_fixture "escripttestwithpath", fn ->
      output = mix "escriptize"
      assert output =~ %r/Generated escript ebin(\/|\\)escripttestwithpath/
      assert System.cmd("escript " <> Path.join("ebin", "escripttestwithpath")) == "TEST_WITH_PATH\n"
    end
  end
end
