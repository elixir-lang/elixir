Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.ProjectTest do
  use MixTest.Case

  defmodule SampleProject do
    def project do
      [ hello: "world" ]
    end
  end

  defmodule EnvProject do
    def project do
      [ hello: "world",
        env: [ prod: [hello: "new"] ] ]
    end
  end

  test "push and pop projects" do
    refute Mix.Project.get
    Mix.Project.push(SampleProject)

    assert Mix.Project.get == SampleProject

    assert Mix.Project.pop == SampleProject
    assert Mix.Project.pop == nil
  after
    Mix.Project.pop
  end

  test "retrieves configuration from projects" do
    Mix.Project.push(SampleProject)
    assert Mix.project[:hello] == "world"
  after
    Mix.Project.pop
  end

  test "reads environment info when a project is set" do
    Mix.env(:prod)
    Mix.Project.push(EnvProject)
    assert Mix.project[:hello] == "new"
  after
    Mix.Project.pop
  end

  test "retrieves configuration even when a project is not set" do
    assert Mix.project[:compile_path] == "ebin"
  end

  test "raises an error when trying to retrieve the current project but none is set" do
    assert_raise Mix.NoProjectError, fn ->
      Mix.Project.get!
    end
  end
end
