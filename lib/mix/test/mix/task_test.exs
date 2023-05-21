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

    assert_raise Mix.NoTaskError,
                 "The task \"unknown\" could not be found\nNote no mix.exs was found in the current directory",
                 fn -> Mix.Task.run("unknown") end

    message =
      "The task \"helli\" could not be found. Did you mean \"hello\"?\nNote no mix.exs was found in the current directory"

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

    assert ExUnit.CaptureIO.capture_io(fn -> Mix.Task.run("hello") end) =~
             ~r"-> Running mix hello\n<- Ran mix hello in \d+ms"
  after
    Mix.shell(Mix.Shell.Process)
    Mix.debug(false)
  end

  defmodule DepsApp do
    def project do
      [
        app: :raw_sample,
        version: "0.1.0",
        deps: [
          {:raw_repo, "0.1.0", path: "custom/raw_repo"}
        ]
      ]
    end
  end

  test "run/2 tries to load deps if task is missing" do
    in_fixture("deps_status", fn ->
      Mix.Project.push(DepsApp)

      assert_raise Mix.NoTaskError, fn ->
        Mix.Task.run("task_hello")
      end

      File.write!("custom/raw_repo/lib/task_hello.ex", """
      defmodule Mix.Tasks.TaskHello do
        use Mix.Task
        def run(_), do: "Hello World v1"
      end
      """)

      # Clean up the tasks and update task
      Mix.Task.clear()

      # Task was found from deps loadpaths
      assert Mix.Task.run("task_hello") == "Hello World v1"

      # The compile task should not have run yet
      assert Mix.Task.run("compile") != :noop

      # Clean up the tasks and update task
      Mix.Task.clear()

      File.write!("custom/raw_repo/lib/task_hello.ex", """
      defmodule Mix.Tasks.TaskHello do
        use Mix.Task
        def run(_), do: "Hello World v2"
      end
      """)

      # Simulate starting a new invocation
      purge([Mix.Tasks.TaskHello])
      Code.delete_path("_build/dev/lib/raw_repo/ebin")
      ensure_touched("custom/raw_repo/lib/task_hello.ex")

      # Task is recompiled to v2
      assert Mix.Task.run("task_hello") == "Hello World v2"
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
        assert [:noop, :noop] = Mix.Task.run("clean")

        Mix.Task.reenable("clean")
        assert [:ok, :ok] = Mix.Task.run("clean")
        assert [:noop, :noop] = Mix.Task.run("clean")
      end)
    end)
  end

  test "aliases considered for umbrella apps" do
    in_fixture("umbrella_test", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        File.mkdir_p!("apps/baz/lib/")

        File.write!("apps/baz/mix.exs", """
        defmodule Baz.MixProject do
          use Mix.Project

          def project do
            [
              app: :baz,
              version: "0.1.0",
              aliases: [help: fn _ -> raise "oops" end]
            ]
          end
        end
        """)

        File.write!("apps/baz/lib/baz.ex", """
        defmodule Baz do
          def hello do
            :world
          end
        end
        """)

        Mix.Task.run("compile")
        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :info, ["==> baz"]}
        Mix.shell().flush()

        # A child alias does not overlap an umbrella task
        assert Mix.Task.run("help") == :ok

        # A missing umbrella alias can be found in children
        assert [:ok, :ok] = Mix.Task.run("mytask")
        assert_received {:mix_shell, :info, ["==> foo"]}
        assert_received {:mix_shell, :info, ["foo_running"]}
        assert_received {:mix_shell, :info, ["==> bar"]}
        assert_received {:mix_shell, :info, ["bar_running"]}
        refute_received {:mix_shell, :info, ["==> baz"]}

        # An unknown task or alias anywhere fails
        assert_raise Mix.NoTaskError, fn ->
          Mix.Task.run("completely_unknown")
        end
      end)
    end)
  end

  test "reenable/1 for recursive inside umbrella child" do
    in_fixture("umbrella_dep/deps/umbrella", fn ->
      Mix.Project.in_project(:umbrella, ".", fn _ ->
        assert [:ok, :ok] = Mix.Task.run("cmd", ["echo", "hello"])
        assert [:ok, :ok] = Mix.Task.run("cmd", ["echo", "world"])

        assert_received {:mix_shell, :run, ["hello" <> _]}
        assert_received {:mix_shell, :run, ["world" <> _]}
        assert_received {:mix_shell, :run, ["hello" <> _]}
        assert_received {:mix_shell, :run, ["world" <> _]}
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
        assert [:noop, :noop] = Mix.Task.run("clean")
        assert [:ok, :ok] = Mix.Task.rerun("clean")
      end)
    end)
  end

  test "get!" do
    Mix.Project.push(MixTest.Case.Sample)
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

  test "shortdoc/1" do
    assert Mix.Task.shortdoc(Mix.Tasks.Hello) == "This is short documentation, see"
  end

  defmodule Elixir.Mix.Tasks.WithRequirement do
    use Mix.Task
    @shortdoc "This is short documentation, see"
    @requirements "help \"compile\""

    @moduledoc """
    A test task.
    """

    def run(_args) do
      "Task with requirements"
    end
  end

  test "requirements/1" do
    assert Mix.Task.requirements(Mix.Tasks.WithRequirement) == ["help \"compile\""]
  end

  test "@requirements are running during task execution" do
    assert ExUnit.CaptureIO.capture_io(fn ->
             assert Mix.Task.run("with_requirement") == "Task with requirements"
           end) =~ "mix compile"

    Mix.Task.reenable("help")

    assert ExUnit.CaptureIO.capture_io(fn ->
             assert Mix.Task.run("with_requirement") == :noop
           end) == ""
  end
end
