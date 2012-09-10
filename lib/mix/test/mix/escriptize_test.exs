Code.require_file "../../test_helper.exs", __FILE__

defmodule Mix.EscriptizeTest do
  use MixTest.Case

  test "generate simple escript" do
    in_fixture "escripttest", fn ->
      output = mix "escriptize"
      assert output =~ %r/Generated escript escripttest/
      assert System.cmd("escript escripttest") == "TEST\n"
    end
  end
end
