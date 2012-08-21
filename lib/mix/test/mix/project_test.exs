Code.require_file "../../test_helper.exs", __FILE__

defmodule Mix.ProjectTest do
  use MixTest.Case

  defmodule RealProject do
    def project do
      [hello: "world"]
    end
  end

  test "push and pop projects" do
    refute Mix.Project.defined?
    Mix.Project.push(RealProject)

    assert Mix.Project.current == RealProject
    assert Mix.Project.defined?
  after
    Mix.Project.pop
  end
  
  test "retrieves configuration from projects" do
    Mix.Project.push(RealProject)
    assert Mix.project == [hello: "world"]
  after
    Mix.Project.pop
  end

  test "retrieves configuration even when a project is not set" do
    assert Mix.project == []
  end

  test "raises an error when trying to retrieve the current a project but none it set" do
    assert_raise Mix.NoProjectError, fn ->
      Mix.Project.current
    end
  end
end