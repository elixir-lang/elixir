# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Mix.Tasks.Cmd do
  use Mix.Task

  @shortdoc "Executes the given command"
  @recursive true

  @moduledoc """
  Executes the given command.

  For example, you can invoke an external command such as make:

      $ mix cmd make

  ## Shell expansion

  When you invoke `mix cmd` from the command line, environment variables,
  glob patterns, and similar are expanded by the current shell and then
  passed to `mix cmd`. For example, if you invoke:

      $ mix cmd echo lib/*

  Your shell will expand "lib/*" and then pass multiple arguments to
  `mix cmd`, which in turn passes them to `echo`. Note that, `mix cmd`
  by itself, does not perform any shell expansion. This means that,
  if you invoke `mix cmd` programatically, as in:

      Mix.Task.run("cmd", ["echo", "lib/*"])

  or through a `mix.exs` alias:

      alias: "cmd echo lib/*"

  It will literally print "lib/*".

  This behaviour is the default from Elixir v1.19. If you want
  `mix cmd` to behave like a shell, you can pass the `--shell`
  option before the command name:

      Mix.Task.run("cmd", ["--shell", "echo", "lib/*"])

  Keep in mind however that, if `--shell` is given, quoted arguments
  are no longer correctly propagated to the underlying command
  (as they get expanded before hand).

  This task is automatically re-enabled, so it can be called multiple times
  with different arguments.

  ## Umbrella applications and directories

  This task is also useful in umbrella applications to execute a command
  on each child app:

      $ mix cmd pwd

  In such cases, Mix will change the current working directory to the root
  of each umbrella application before executing the command.

  You can limit which apps the cmd runs in by using `mix do` with the `--app`
  option:

      $ mix do --app app1 --app app2 cmd pwd

  Note the tasks aborts whenever a command exits with a non-zero status.

  Outside of umbrella projects, you can pass the `--cd` flag before the command,
  to specify a directory:

      $ mix cmd --cd "third-party" make

  ## Command line options

    * `--cd` *(since v1.10.4)* - the directory to run the command in
    * `--shell` *(since v1.19.0)* - perform shell expansion of the arguments

  ## Orphan operating system processes

  Beware that the Erlang VM does not terminate child processes
  when it shuts down. Therefore, if you use `mix cmd` to start
  long running processes and then shut down the VM, it is likely
  that those child processes won't be terminated with the VM.

  A solution is to make sure the child processes listen to the
  standard input and terminate when standard input is closed.
  We discuss this topic at length in the "Orphan operating system processes"
  of the `Port` module documentation.
  """

  @switches [
    app: :keep,
    cd: :string,
    shell: :boolean
  ]

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse_head!(args, strict: @switches)

    if args == [] do
      Mix.raise("No argument was given to mix cmd. Run \"mix help cmd\" for more information")
    end

    apps =
      opts
      |> Keyword.get_values(:app)
      |> Enum.map(&String.to_atom/1)

    if apps != [] do
      IO.warn("the --app in mix cmd is deprecated. Use mix do --app instead.")
    end

    if apps == [] or Mix.Project.config()[:app] in apps do
      path = hd(args)
      rest = tl(args)

      arg =
        cond do
          Keyword.get(opts, :shell, false) ->
            Enum.join([path | rest], " ")

          String.contains?(path, ["/", "\\"]) and Path.type(path) != :absolute ->
            {Path.expand(path, Keyword.get(opts, :cd, ".")), rest}

          true ->
            {path, rest}
        end

      cmd_opts = Keyword.take(opts, [:cd])

      case Mix.shell().cmd(arg, cmd_opts) do
        0 -> :ok
        status -> exit(status)
      end
    end

    Mix.Task.reenable("cmd")
  end
end
