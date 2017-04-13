defmodule Mix.Tasks.Usage do
  use Mix.Task

  @shortdoc "Prints usage information for mix"

  @moduledoc ~S"""
  Usage: mix [task]
    mix                       - Runs the default task (current: "mix run")
    mix new                   - Creates a new Elixir project

  For further help:
    mix help                  - Prints help information for tasks
    mix help                  - Prints all tasks and their shortdoc
    mix help TASK             - Prints full docs for the given task
    mix help --search PATTERN - Prints all tasks that contain PATTERN in the name
    mix help --names          - Prints all task names and aliases
    mix cmd                   - Executes the given command
    mix compile               - Compiles source files
    mix deps                  - Lists dependencies and their status
    mix do                    - Executes the tasks separated by comma
    mix escript               - Lists installed escripts
    mix loadconfig            - Loads and persists the given configuration
    mix local                 - Lists local tasks
    mix local.hex             - Installs Hex locally
    mix local.public_keys     - Manages public keys
    mix local.rebar           - Installs Rebar locally
    mix profile.cprof         - Profiles the given file or expression with cprof
    mix profile.fprof         - Profiles the given file or expression with fprof
    mix run                   - Runs the given file or expression
    iex -S mix                - Starts IEx and runs the default task
  """

  @spec run(OptionParser.argv) :: no_return
  def run(_) do
    Mix.shell.info usage()
  end

  defp usage() do
    Mix.Task.moduledoc(__MODULE__)
    |> String.trim_trailing("\n")
  end
end
