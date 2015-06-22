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

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.run("unknown")
    end

    assert_raise Mix.NoTaskError, "The task helli could not be found. Did you mean 'hello'?", fn ->
      Mix.Task.run("helli")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not export run/1", fn ->
      Mix.Task.run("invalid")
    end
  end

  test "try to compile if task is missing" do
    in_fixture "no_mixfile", fn ->
      Mix.Project.push(SampleProject, "sample")

      assert_raise Mix.NoTaskError, fn ->
        Mix.Task.run("unknown")
      end

      # Check if compile task have run
      refute Mix.TasksServer.run({:task, "compile", Mix.Project.get})
    end
  end

  test "does not try to compile if task is missing in build embedded" do
    in_fixture "no_mixfile", fn ->
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

  test "get!" do
    assert Mix.Task.get!("hello") == Mix.Tasks.Hello

    assert_raise Mix.NoTaskError, "The task unknown could not be found", fn ->
      Mix.Task.get!("unknown")
    end

    assert_raise Mix.InvalidTaskError, "The task invalid does not export run/1", fn ->
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

  test "shortdoc/1" do
    assert Mix.Task.shortdoc(Mix.Tasks.Hello) == "This is short documentation, see"
  end
end
