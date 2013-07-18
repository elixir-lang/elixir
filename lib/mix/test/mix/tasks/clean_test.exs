Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.CleanTest do
  use MixTest.Case

  test "compile a project without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/Elixir.A.beam")
      Mix.Tasks.Clean.run []
      refute File.regular?("ebin/Elixir.A.beam")
    end
  end
end