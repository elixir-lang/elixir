Code.require_file "../../test_helper", __FILE__

defmodule Mix.TaskTest do
  use MixTest.Case

  test :run do
    assert Mix.Task.run("hello") == :ok

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.run("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not respond to run/1", fn ->
      Mix.Task.run("invalid")
    end
  end
end