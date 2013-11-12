Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Tasks.IexTest do
  use MixTest.Case

  test "iex raises error message about correct usage" do
    in_fixture "only_mixfile", fn ->
      error_message = "Cannot start IEx after the VM was booted. " <>
                      "To use IEx with Mix, please run: iex -S mix"
      assert_raise Mix.Error, error_message, fn ->
        Mix.Tasks.Iex.run []
      end
    end
  end
end
