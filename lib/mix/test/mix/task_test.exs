Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.TaskTest do
  use MixTest.Case

  defmodule SampleProject do
    def project do
      [app: :sample, version: "0.0.1"]
    end
  end

  test "run/1" do
    assert Mix.Task.run("hello") == "Hello, World!"
    assert Mix.Task.run("hello") == :noop

    assert_raise Mix.NoTaskError, "The task \"unknown\" could not be found", fn ->
      Mix.Task.run("unknown")
    end

    assert_raise Mix.NoTaskError, "The task \"helli\" could not be found. Did you mean \"hello\"?", fn ->
      Mix.Task.run("helli")
    end

    assert_raise Mix.InvalidTaskError, "The task \"invalid\" does not export run/1", fn ->
      Mix.Task.run("invalid")
    end

    misnamed_message =
      "The task \"acronym.http\" could not be found because the module is named " <>
      "Mix.Tasks.Acronym.HTTP instead of Mix.Tasks.Acronym.Http as expected. " <>
      "Please rename it and try again"
    assert_raise Mix.NoTaskError, misnamed_message, fn ->
      Mix.Task.run("acronym.http")
    end
  end

  test "output task debug info if Mix.debug? is true" do
    Mix.shell Mix.Shell.IO
    Mix.debug(true)

    assert ExUnit.CaptureIO.capture_io(fn -> Mix.Task.run("hello") end) =~
      "** Running mix hello"
  after
    Mix.shell(Mix.Shell.Process)
    Mix.debug(false)
  end

  test "try to deps.loadpaths if task is missing", context do
    in_tmp context.test, fn ->
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
      Mix.TasksServer.clear
      File.mkdir_p!("_build/dev/lib/sample/ebin")
      File.write!("_build/dev/lib/sample/ebin/Elixir.Mix.Tasks.TaskHello.beam", bin)

      # Task was found from deps loadpaths
      assert Mix.Task.run("task_hello") == "Hello, World"

      # The compile task should not have run yet
      assert Mix.TasksServer.run({:task, "compile", Mix.Project.get})
    end
  end

  test "try to compile if task is missing", context do
    in_tmp context.test, fn ->
      Mix.Project.push(SampleProject, "sample")

      assert_raise Mix.NoTaskError, fn ->
        Mix.Task.run("unknown")
      end

      # Check if compile task have run
      refute Mix.TasksServer.run({:task, "compile", Mix.Project.get})
    end
  end

  test "does not try to compile if task is missing in build embedded", context do
    in_tmp context.test, fn ->
      Mix.ProjectStack.post_config(build_embedded: true)
      Mix.Project.push(SampleProject, "sample")

      assert_raise Mix.Error, ~r/Cannot execute task because the project was not yet compiled/, fn ->
        Mix.Task.run("unknown")
      end
    end
  end

  test "clear/0" do
    assert Mix.Task.run("hello") == "Hello, World!"
    Mix.Task.clear
    assert Mix.Task.run("hello") == "Hello, World!"
  end

  test "reenable/1" do
    assert Mix.Task.run("hello") == "Hello, World!"
    Mix.Task.reenable("hello")
    assert Mix.Task.run("hello") == "Hello, World!"
  end

  test "reenable/1 for umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        assert [:ok, :ok] = Mix.Task.run "clean"
        assert :noop      = Mix.Task.run "clean"

        Mix.Task.reenable "clean"
        assert [:ok, :ok] = Mix.Task.run "clean"
        assert :noop      = Mix.Task.run "clean"
      end)
    end
  end

  test "rerun/1" do
    assert Mix.Task.run("hello") == "Hello, World!"
    assert Mix.Task.rerun("hello") == "Hello, World!"
  end

  test "rerun/1 for umbrella" do
    in_fixture "umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        assert [:ok, :ok] = Mix.Task.run "clean"
        assert :noop      = Mix.Task.run "clean"
        assert [:ok, :ok] = Mix.Task.rerun "clean"
      end)
    end
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
    refute Mix.Task.alias?(:sample)
    refute Mix.Task.alias?("sample")

    Mix.Project.push MixTest.Case.Sample
    assert Mix.Task.alias?(:sample)
    assert Mix.Task.alias?("sample")
    refute Mix.Task.alias?("another")
  after
    Mix.Project.pop
  end

  test "all_modules/0" do
    Mix.Task.load_all
    modules = Mix.Task.all_modules
    assert Mix.Tasks.Hello in modules
    assert Mix.Tasks.Compile in modules
  end

  test "moduledoc/1" do
    Code.prepend_path MixTest.Case.tmp_path("beams")
    assert Mix.Task.moduledoc(Mix.Tasks.Hello) == "A test task.\n"
  end

  test "preferred_cli_env/1 returns nil for missing task" do
    assert Mix.Task.preferred_cli_env(:no_task) == nil
  end

  test "preferred_cli_env/1 returns nil when task does not have `preferred_cli_env` attribute" do
    assert Mix.Task.preferred_cli_env(:deps) == nil
  end

  test "preferred_cli_env/1 returns specified `preferred_cli_env` attribute" do
    assert Mix.Task.preferred_cli_env(:test) == :test
  end

  test "shortdoc/1" do
    assert Mix.Task.shortdoc(Mix.Tasks.Hello) == "This is short documentation, see"
  end
end
