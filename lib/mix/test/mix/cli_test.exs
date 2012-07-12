Code.require_file "../../test_helper", __FILE__

defmodule Mix.CLITest do
  use MixTest.Case

  test "invoke simple task from CLI" do
    in_fixture "only_mixfile", fn ->
      assert mix("hello") == "Hello from MyProject!\n"
    end
  end
end