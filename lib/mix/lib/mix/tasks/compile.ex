defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile source files"

  @moduledoc """
  A meta task that compile source files. It simply runs the
  compilers registered in your project. At the end of compilation
  it ensures load paths are set.

  ## Configuration

  * `:compilers` - compilers to be run, defaults to:

      [:elixir, :app]

    It can be configured to handle custom compilers, for example:

      [compilers: [:elixir, :mycompiler, :app]]

  ## Command line options

  * `--list` - List all enabled compilers.

  """
  def run(["--list"]) do
    Mix.shell.info "Enabled compilers: #{Enum.join get_compilers, ", "}"
  end

  def run(args) do
    Mix.Task.run "deps.loadpaths"

    Enum.each get_compilers, fn(compiler) ->
      Mix.Task.run "compile.#{compiler}", args
    end

    Mix.Task.run "loadpaths"
  end

  defp get_compilers do
    Mix.project[:compilers] || if Mix.Project.defined? do
      [:elixir, :app]
    else
      [:elixir]
    end
  end
end
