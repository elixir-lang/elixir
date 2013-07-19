Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.EscriptizeTest do
  use MixTest.Case

  defmodule Escript do
    def project do
      [ app: :escripttest,
        version: "0.0.1",
        escript_embed_elixir: true ]
    end
  end

  defmodule EscriptWithPath do
    def project do
      [ app: :escripttestwithpath,
        version: "0.0.1",
        escript_app: nil,
        escript_embed_elixir: true,
        escript_main_module: Escripttest,
        escript_name: :escripttestwithpath,
        escript_path: Path.join("ebin", "escripttestwithpath") ]
    end
  end

  test "generate simple escript" do
    Mix.Project.push Escript

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escriptize.run []
      assert_received { :mix_shell, :info, ["Generated escript escripttest"] }
      assert System.cmd("escript escripttest") == "TEST\n"

      Mix.Tasks.Escriptize.run []
      refute_received { :mix_shell, :info, ["Generated escript escripttest"] }
    end
  after
    Mix.Project.pop
  end

  test "generate simple escript with path" do
    Mix.Project.push EscriptWithPath

    in_fixture "escripttest", fn ->
      Mix.Tasks.Escriptize.run []
      assert_received { :mix_shell, :info, ["Generated escript ebin/escripttestwithpath"] }
      assert System.cmd("escript ebin/escripttestwithpath") == "TEST\n"
    end
  after
    Mix.Project.pop
  end
end
