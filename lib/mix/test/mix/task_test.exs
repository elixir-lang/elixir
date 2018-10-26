Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.TaskTest do
  use MixTest.Case

  defmodule SampleProject do
    def project do
      [app: :sample, version: "0.0.1"]
    end
  end

  test "run/2" do
    assert Mix.Task.run("hello") == "Hello, World!"
    assert Mix.Task.run("hello") == :noop

    assert_raise Mix.NoTaskError, "The task \"unknown\" could not be found", fn ->
      Mix.Task.run("unknown")
    end

    message = "The task \"helli\" could not be found. Did you mean \"hello\"?"

    assert_raise Mix.NoTaskError, message, fn ->
      Mix.Task.run("helli")
    end

    assert_raise Mix.InvalidTaskError, "The task \"invalid\" does not export run/1", fn ->
      Mix.Task.run("invalid")
    end

    message =
      "The task \"acronym.http\" could not be found because the module is named " <>
        "Mix.Tasks.Acronym.HTTP instead of Mix.Tasks.Acronym.Http as expected. " <>
        "Please rename it and try again"

    assert_raise Mix.NoTaskError, message, fn ->
      Mix.Task.run("acronym.http")
    end
  end

  test "run/2 converts OptionParser.ParseError into Mix errors" do
    message = "Could not invoke task \"hello\": 1 error found!\n--unknown : Unknown option"

    assert_raise Mix.Error, message, fn ->
      Mix.Task.run("hello", ["--parser", "--unknown"])
    end

    Mix.Task.clear()

    message =
      "Could not invoke task \"hello\": 1 error found!\n--int : Expected type integer, got \"foo\""

    assert_raise Mix.Error, message, fn ->
      Mix.Task.run("hello", ["--parser", "--int", "foo"])
    end
  end

  test "run/2 outputs task debug info if Mix.debug? is true" do
    Mix.shell(Mix.Shell.IO)
    Mix.debug(true)

    assert ExUnit.CaptureIO.capture_io(fn -> Mix.Task.run("hello") end) =~ "** Running mix hello"
  after
    Mix.shell(Mix.Shell.Process)
    Mix.debug(false)
  end

  test "run/2 tries to load deps if task is missing", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(SampleProject, "sample")

      {:module, _, bin, _} =
        defmodule Elixir.Mix.Tasks.TaskHello do
          use Mix.Task
          def run(_), do: "Hello, World"
        end

      :code.purge(Mix.Tasks.TaskHello)
      :code.delete(Mix.Tasks.TaskHello)

      assert_raise Mix.NoTaskError, fn ->
        Mix.Task.run("task_hello")
      end

      # Clean up the tasks and copy it into deps
      Mix.TasksServer.clear()
      File.mkdir_p!("_build/dev/lib/sample/ebin")
      File.write!("_build/dev/lib/sample/ebin/Elixir.Mix.Tasks.TaskHello.beam", bin)

      # Task was found from deps loadpaths
      assert Mix.Task.run("task_hello") == "Hello, World"

      # The compile task should not have run yet
      assert Mix.TasksServer.run({:task, "compile", Mix.Project.get()})
    end)
  end

  test "run/2 tries to compile if task is missing", context do
    in_tmp(context.test, fn ->
      Mix.Project.push(SampleProject, "sample")

      assert_raise Mix.NoTaskError, fn ->
        Mix.Task.run("unknown")
      end

      # Check if compile task have run
      refute Mix.TasksServer.run({:task, "compile", Mix.Project.get()})
    end)
  end

  test "clear/0" do
    assert Mix.Task.run("hello") == "Hello, World!"
    Mix.Task.clear()
    assert Mix.Task.run("hello") == "Hello, World!"
  end

  test "reenable/1" do
    assert Mix.Task.run("hello") == "Hello, World!"
    Mix.Task.reenable("hello")
    assert Mix.Task.run("hello") == "Hello, World!"
  end

  test "reenable/1 for recursive inside umbrella" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        assert [:ok, :ok] = Mix.Task.run("clean")
        assert :noop = Mix.Task.run("clean")

        Mix.Task.reenable("clean")
        assert [:ok, :ok] = Mix.Task.run("clean")
        assert :noop = Mix.Task.run("clean")
      end)
    end)
  end

  test "reenable/1 for non-recursive inside umbrella" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        assert [:ok, :ok] = Mix.Task.run("clean")
        # loadpaths is not recursive
        assert :ok = Mix.Task.run("loadpaths")
      end)
    end)
  end

  test "rerun/1" do
    assert Mix.Task.run("hello") == "Hello, World!"
    assert Mix.Task.rerun("hello") == "Hello, World!"
  end

  test "rerun/1 for umbrella" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        assert [:ok, :ok] = Mix.Task.run("clean")
        assert :noop = Mix.Task.run("clean")
        assert [:ok, :ok] = Mix.Task.rerun("clean")
      end)
    end)
  end

  test "get!" do
    assert Mix.Task.get!("hello") == Mix.Tasks.Hello

    assert_raise Mix.NoTaskError, "The task \"unknown\" could not be found", fn ->
      Mix.Task.get!("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task \"invalid\" does not export run/1", fn ->
      Mix.Task.get!("invalid")
    end
  end

  test "alias?/1" do
    assert Mix.Task.alias?(:sample) == false
    assert Mix.Task.alias?("sample") == false

    Mix.Project.push(MixTest.Case.Sample)
    assert Mix.Task.alias?(:sample) == true
    assert Mix.Task.alias?("sample") == true
    assert Mix.Task.alias?("another") == false
  after
    Mix.Project.pop()
  end

  test "all_modules/0" do
    Mix.Task.load_all()
    modules = Mix.Task.all_modules()
    assert Mix.Tasks.Hello in modules
    assert Mix.Tasks.Compile in modules
  end

  test "moduledoc/1" do
    Code.prepend_path(MixTest.Case.tmp_path("beams"))
    assert Mix.Task.moduledoc(Mix.Tasks.Hello) == "A test task.\n"
  end

  test "preferred_cli_env/1 returns nil for missing task" do
    assert Mix.Task.preferred_cli_env(:no_task) == nil
  end

  test "preferred_cli_env/1 returns nil when task does not have @preferred_cli_env attribute" do
    assert Mix.Task.preferred_cli_env(:deps) == nil
  end

  test "preferred_cli_env/1 returns specified @preferred_cli_env attribute" do
    assert Mix.Task.preferred_cli_env(:test) == :test
  end

  test "shortdoc/1" do
    assert Mix.Task.shortdoc(Mix.Tasks.Hello) == "This is short documentation, see"
  end
end
