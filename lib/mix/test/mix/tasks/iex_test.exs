Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.IExTest do
  use MixTest.Case, async: true

  test "raises error message about correct usage", context do
    in_tmp(context.test, fn ->
      msg = "To use IEx with Mix, please run \"iex -S mix\""

      assert_raise Mix.Error, msg, fn ->
        Mix.Tasks.Iex.run([])
      end
    end)
  end
end
