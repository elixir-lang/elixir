Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.Compile.ProtocolsTest do
  use MixTest.Case

  test "compiles and consolidates protocols" do
    Mix.Project.push MixTest.Case.Sample

    in_fixture "no_mixfile", fn ->
      assert Mix.Tasks.Compile.Protocols.run([]) == :ok
      assert_received {:mix_shell, :info, ["Consolidated Enumerable"]}

      assert File.regular? "_build/dev/consolidated/Elixir.Enumerable.beam"
      purge [Enumerable]

      Code.prepend_path("_build/dev/consolidated")
      assert Protocol.consolidated?(Enumerable)
    end
  after
    purge [Enumerable]
  end
end
