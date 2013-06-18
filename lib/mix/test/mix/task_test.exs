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
    assert match?([ {"hello", _} ], Mix.Task.clear)
  end

  test :reenable do
    assert Mix.Task.run("hello") == "Hello, World!"
    Mix.Task.reenable("hello")
    assert Mix.Task.run("hello") == "Hello, World!"
  end

  test :get_module do
    assert Mix.Task.get_module("hello") == { :module, Mix.Tasks.Hello }

    assert Mix.Task.get_module("unknown") == { :error, :notask }

    assert Mix.Task.get_module("invalid") == { :error, :invalidtask }
  end

  test :get_module! do
    assert Mix.Task.get_module!("hello") == Mix.Tasks.Hello

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.get_module!("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not respond to run/1", fn ->
      Mix.Task.get_module!("invalid")
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

  defmodule HookApp1 do
    use Mix.Project, no_push: true

    def project, do: []

    task "hello", args, original do
      res = original.(args)
      self <- { HookApp1, "hello", args, res }
    end

    task "not_overriding1", args do
      self <- { HookApp1, "not_overriding1", args }
    end

    task "not_overriding2", args, original do
      self <- { HookApp1, "not_overriding2", args, original }
    end
  end

  defmodule HookApp2 do
    use Mix.Project, no_push: true

    def project, do: []

    task "help", args do
      self <- { HookApp2, "help", args }
    end
  end

  defmodule HookApp3 do
    use Mix.Project, no_push: true

    def project, do: []

    task "help", args, _original do
      self <- { HookApp3, "help", args }
    end

    task name, args, _original do
      self <- { HookApp3, name, args }
    end
  end

  defmodule HookApp4 do
    use Mix.Project, no_push: true

    def project, do: []

    task name, args, _original do
      self <- { HookApp3, name, args }
    end
  end

  test "task hooks override and call original" do
    Mix.Project.push(HookApp1)

    Mix.Task.run("hello", [:arg])
    assert_received { HookApp1, "hello", [:arg], "Hello, World!" }
  after
    Mix.Project.pop
  end

  test "task hooks no override" do
    Mix.Project.push(HookApp1)

    Mix.Task.run("not_overriding1", [:arg1, :arg2])
    assert_received { HookApp1, "not_overriding1", [:arg1, :arg2] }
  after
    Mix.Project.pop
  end

  test "task hooks no override with original" do
    Mix.Project.push(HookApp1)

    Mix.Task.run("not_overriding2", [])
    assert_received { HookApp1, "not_overriding2", [], nil }
  after
    Mix.Project.pop
  end

  test "task hooks no override and call original" do
    Mix.Project.push(HookApp2)

    Mix.Task.run("help", ["help"])

    assert_received { :mix_shell, :info, ["# mix help help\n"] }
    assert_received { HookApp2, "help", ["help"] }
  after
    Mix.Project.pop
  end

  test "task hooks override and dont call original" do
    Mix.Project.push(HookApp3)

    Mix.Task.run("help", ["help"])

    refute_received { :mix_shell, :info, "# mix help help\n" }
    assert_received { HookApp3, "help", ["help"] }
  after
    Mix.Project.pop
  end

  test "task hooks override all" do
    Mix.Project.push(HookApp3)

    Mix.Task.run("no_task_named_this", [:arg])

    assert_received { HookApp3, "no_task_named_this", [:arg] }
  after
    Mix.Project.pop
  end
end
