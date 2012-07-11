Code.require_file "../../test_helper", __FILE__

defmodule Mix.TasksTest do
  use MixTest.Case

  test :get_module do
    assert Mix.Tasks.get_module("hello")   == { :module, Mix.Tasks.Hello }
    assert Mix.Tasks.get_module("unknown") == { :error, :nofile }
  end

  test :run do
    assert Mix.Tasks.run("hello")   == { :ok, "Hello, World!" }
    assert Mix.Tasks.run("unknown") == { :error, :no_task }
    assert Mix.Tasks.run("invalid") == { :error, :invalid_task }
  end

  test :run! do
    assert Mix.Tasks.run!("hello") == "Hello, World!"

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Tasks.run!("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not respond to run/1", fn ->
      Mix.Tasks.run!("invalid")
    end
  end

  test :module_to_task do
    assert Mix.Tasks.module_to_task("Mix.Tasks.Foo")     == "foo"
    assert Mix.Tasks.module_to_task("Mix.Tasks.Foo.Bar") == "foo.bar"
  end

  test :task_to_module do
    assert Mix.Tasks.task_to_module("foo")     == "Foo"
    assert Mix.Tasks.task_to_module("foo.bar") == "Foo.Bar"
  end
end