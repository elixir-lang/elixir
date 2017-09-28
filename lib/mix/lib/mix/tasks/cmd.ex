defmodule Mix.Tasks.Cmd do
  use Mix.Task

  @shortdoc "Executes the given command"
  @recursive true

  @moduledoc """
  Executes the given command.

  Useful in umbrella applications to execute a command
  on each child app:

      mix cmd echo pwd

  You can limit which apps the cmd runs in by passing the app names
  before the cmd using --app:

      mix cmd --app app1 --app app2 echo pwd

  Aborts when a command exits with a non-zero status.
  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {args, apps} = parse_apps(args, [])
    if apps == [] or Mix.Project.config[:app] in apps do
      case Mix.Shell.cmd(Enum.join(args, " "), into: %Mix.Shell{}) do
        0 -> :ok
        status -> exit(status)
      end
    end
  end

  defp parse_apps(args, apps) do
    case args do
      ["--app", app | tail] ->
        parse_apps(tail, [String.to_atom(app) | apps])
      args ->
        {args, apps}
    end
  end
end
