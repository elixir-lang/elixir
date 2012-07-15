Code.require_file "../../test_helper", __FILE__

defmodule Mix.TaskTest do
  use MixTest.Case

  test :run do
    assert Mix.Task.run("hello") == :ok
    assert Mix.Task.run("hello") == :noop

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.run("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not respond to run/1", fn ->
      Mix.Task.run("invalid")
    end
  end

  test :run! do
    assert Mix.Task.run!("hello") == :ok
    assert Mix.Task.run!("hello") == :ok
  end

  test :clear do
    Mix.Task.run("hello")
    assert Mix.Task.clear == ["hello"]
  end

  test :reenable do
    assert Mix.Task.run("hello") == :ok
    Mix.Task.reenable("hello")
    assert Mix.Task.run("hello") == :ok
  end
end