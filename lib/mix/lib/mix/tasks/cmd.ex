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

  This task is automatically reenabled, so it can be called multiple times
  with different arguments.

  ## Zombie operating system processes

  Beware that the Erlang VM does not terminate child processes
  when it shuts down. Therefore, if you use `mix cmd` to start
  long running processes and then shut down the VM, it is likely
  that those child processes won't be terminated with the VM.

  A solution is to make sure the child processes listen to the
  standard input and terminate when standard input is closed.
  We discuss this topic at length in the "Zombie operating system processes"
  of the `Port` module documentation.
  """

  @impl true
  def run(args) do
    {args, apps} = parse_apps(args, [])

    if apps == [] or Mix.Project.config()[:app] in apps do
      case Mix.shell().cmd(Enum.join(args, " ")) do
        0 -> :ok
        status -> exit(status)
      end
    end

    Mix.Task.reenable("cmd")
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
