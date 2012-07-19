defmodule Mix.Tasks.Deps.Loadpaths do
  use Mix.Task

  import Mix.Tasks.Deps, only: [all: 1, deps_path: 1]

  @moduledoc """
  Loads all dependencies. Invokes "deps.check" before
  unless `--no-check` is given.

  This task is not shown in `mix help` but it is part
  of mix public API and can be depended on.
  """
  def run(args) do
    destructure [no_check], args

    unless no_check == "--no-check" do
      Mix.Task.run "deps.check"
    end

    Enum.each all(:ok), fn({ _, app, _, _, _ }) ->
      Code.prepend_path File.join deps_path(app), "ebin"
    end
  end
end
