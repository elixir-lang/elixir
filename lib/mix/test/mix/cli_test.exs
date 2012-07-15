Code.require_file "../../test_helper", __FILE__

defmodule Mix.CLITest do
  use MixTest.Case, sync: true

  test "invoke simple task from CLI" do
    in_fixture "only_mixfile", fn ->
      assert mix("hello") == "Hello from MyProject!\n"
    end
  end

  test "compile smoke test" do
    in_fixture "no_mixfile", fn ->
      mix "compile"
      assert File.regular?("ebin/__MAIN__-A.beam")
      assert File.regular?("ebin/__MAIN__-B.beam")
      assert File.regular?("ebin/__MAIN__-C.beam")
    end
  end
end