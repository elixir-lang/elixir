defmodule Mix.Tasks.Compile do
  use Mix.Task

  @shortdoc "Compile source files"

  @moduledoc """
  A meta task that compile source files. It simply runs the
  compilers registered in your project.

  ## Configuration

  * `:compilers` - compilers to be run, defaults to:

      [:elixir, :app]

    Can be configured in your projects as:

      [compilers: [:elixir, :erlang, :app]]

  ## Command line options

  * `--list` List all enabled compilers.
             `mix help` should give you the full list.

  """
  def run(["--list"]) do
    IO.puts "Enabled compilers: #{Enum.join get_compilers, ", "}"
  end

  def run(args) do
    Enum.each get_compilers, fn(compiler) ->
      Mix.Task.run "compile.#{compiler}", args
    end
  end

  defp get_compilers do
    Mix.Project.config[:compilers] || if Mix.Project.defined? do
      [:elixir]
    else
      [:elixir]
    end
  end
end
