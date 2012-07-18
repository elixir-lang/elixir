Code.require_file "../../../test_helper", __FILE__

defmodule Mix.Tasks.CompileTest do
  use MixTest.Case

  test "mix compile --list without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run ["--list"]
      assert_received { :mix_shell, :info, ["Enabled compilers: elixir"] }
    end
  end

  test "compile a project without mixfile" do
    in_fixture "no_mixfile", fn ->
      Mix.Tasks.Compile.run []
      assert File.regular?("ebin/__MAIN__-A.beam")
      assert_received { :mix_shell, :info, ["Compiled lib/a.ex"] }
    end
  after
    purge [A, B, C]
  end
end