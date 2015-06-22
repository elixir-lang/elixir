defmodule Mix.Tasks.Compile.All do
  use Mix.Task

  @moduledoc false
  @recursive true

  # This is an internal task used by mix compile which
  # is meant to be recursive and be invoked for each child
  # project.

  def run(args) do
    Mix.Project.get!

    # Build the project structure so we can write down compiled files.
    Mix.Project.build_structure

    res =
      Enum.map(Mix.Tasks.Compile.compilers(), fn(compiler) ->
        Mix.Task.run("compile.#{compiler}", args)
      end)

    true = Code.prepend_path(Mix.Project.compile_path)
    if :ok in res, do: :ok, else: :noop
  end
end
