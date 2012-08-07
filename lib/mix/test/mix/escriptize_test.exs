Code.require_file "../../test_helper", __FILE__

defmodule Mix.EscriptizeTest do
  use MixTest.Case

  test "generate simple escript" do
    in_tmp "escripttest", fn ->
      mix "new ."
      main_module = "defmodule Escripttest do\ndef start do\n:ok = :application.start(:escripttest)\nend\ndef main(args) do\nIO.puts \"TEST\"\nend\nend\n"
      File.write("lib/escripttest.ex", main_module, [:write])
      mix "escriptize"
      assert System.cmd("escript escripttest") == "TEST\n"
    end
  end
end
