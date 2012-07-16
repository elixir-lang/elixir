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

  test :shortdoc do
    tuple = List.keyfind Mix.Tasks.Hello.__info__(:attributes), :shortdoc, 1
    assert tuple == { :shortdoc, ["This is short documentation, see"] }
  end

  test :hidden do
    tuple = List.keyfind Mix.Tasks.Hello.__info__(:attributes), :hidden, 1
    assert tuple == { :hidden, [true] }
  end

  test :moduledoc do
    assert { _, "A test task.\n" } = Mix.Tasks.Hello.__info__(:moduledoc)
  end

end