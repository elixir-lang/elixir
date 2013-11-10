Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.TaskTest do
  use MixTest.Case

  test :run do
    assert Mix.Task.run("hello") == "Hello, World!"
    assert Mix.Task.run("hello") == :noop

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.run("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not respond to run/1", fn ->
      Mix.Task.run("invalid")
    end
  end

  test :clear do
    Mix.Task.run("hello")
    assert { "hello", nil } in Mix.Task.clear
  end

  test :reenable do
    assert Mix.Task.run("hello") == "Hello, World!"
    Mix.Task.reenable("hello")
    assert Mix.Task.run("hello") == "Hello, World!"
  end

  test :get! do
    assert Mix.Task.get!("hello") == Mix.Tasks.Hello

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.get!("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not respond to run/1", fn ->
      Mix.Task.get!("invalid")
    end
  end

  test :all_modules do
    Mix.Task.load_all
    modules = Mix.Task.all_modules
    assert Mix.Tasks.Hello in modules
    assert Mix.Tasks.Compile in modules
  end

  test :moduledoc do
    assert Mix.Task.moduledoc(Mix.Tasks.Hello) == "A test task.\n"
  end

  test :shortdoc do
    assert Mix.Task.shortdoc(Mix.Tasks.Hello) == "This is short documentation, see"
  end

  test :hidden do
    assert Mix.Task.hidden?(Mix.Tasks.Loadpaths)
    refute Mix.Task.hidden?(Mix.Tasks.Compile)
  end
end
