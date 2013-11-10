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
    Mix.Project.push(SampleProject, "sample")

    assert Mix.Project.get == SampleProject

    assert Mix.Project.pop == { SampleProject, "sample" }
    assert Mix.Project.pop == nil
  after
    Mix.Project.pop
  end

  test "does not allow the same project to be pushed twice" do
    Mix.Project.push(SampleProject, "sample")

    assert_raise Mix.Error, %r/Mix.ProjectTest.SampleProject from "another"/, fn ->
      Mix.Project.push(SampleProject, "another")
    end
  after
    Mix.Project.pop
  end

  test "allows nil projects to be pushed twice" do
    Mix.Project.push nil
    Mix.Project.push nil
    assert Mix.Project.pop == { nil, "nofile" }
    assert Mix.Project.pop == { nil, "nofile" }
    assert Mix.Project.pop == nil
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
    assert Mix.project[:default_task] == "run"
  end

  test "raises an error when trying to retrieve the current project but none is set" do
    assert_raise Mix.NoProjectError, fn ->
      Mix.Project.get!
    end
  end
end
