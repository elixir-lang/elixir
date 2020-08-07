defmodule Mix.Tasks.Cmd do
  use Mix.Task

  @shortdoc "Executes the given command"
  @recursive true

  @moduledoc """
  Executes the given command.

  Useful in umbrella applications to execute a command
  on each child app:

      mix cmd pwd

  You can limit which apps the cmd runs in by passing the app names
  before the cmd using --app:

      mix cmd --app app1 --app app2 pwd

  Aborts when a command exits with a non-zero status.

  This task is automatically reenabled, so it can be called multiple times
  with different arguments.

  ## Command line options

    * `--app` - limit running the command to the given app. This option
      may be given multiple times

    * `--cd` - (since v1.10.4) the directory to run the command in

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

  @switches [
    app: :keep,
    cd: :string
  ]

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse_head!(args, strict: @switches)

    apps =
      opts
      |> Keyword.get_values(:app)
      |> Enum.map(&String.to_atom/1)

    if apps == [] or Mix.Project.config()[:app] in apps do
      cmd_opts = Keyword.take(opts, [:cd])

      case Mix.shell().cmd(Enum.join(args, " "), cmd_opts) do
        0 -> :ok
        status -> exit(status)
      end
    end

    Mix.Task.reenable("cmd")
  end
end
