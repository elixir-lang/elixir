defmodule Mix.Tasks.Eval do
  use Mix.Task

  @shortdoc "Evaluates the given code"

  @moduledoc """
  Evaluates the given code.

      mix eval "IO.puts 1 + 2"

  The given code is evaluated after the current application
  has been configured and loaded, but without starting/running it.
  See `mix run` for running code and scripts within a running
  application.

  This task is designed to mirror the `bin/my_app eval` command
  in releases. If you want to start your application, you may do
  so by using functions such as `Application.ensure_all_started/1`.
  For more information about the application life-cycle and
  dynamically configuring applications, see the `Application` module.

  This task is automatically re-enabled, so it can be called multiple
  times with different arguments.

  ## Command-line options

    * `--no-archives-check` - does not check archives
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs
    * `--no-mix-exs` - allows the command to run even if there is no mix.exs

  """

  @impl true
  def run(args) do
    {_opts, head} =
      OptionParser.parse_head!(
        args,
        strict: [
          mix_exs: :boolean,
          compile: :boolean,
          deps_check: :boolean,
          archives_check: :boolean,
          elixir_version_check: :boolean
        ]
      )

    case head do
      [to_eval] ->
        cond do
          Mix.Project.get() ->
            Mix.Task.run("app.config", args)

          "--no-mix-exs" in args ->
            :ok

          true ->
            Mix.raise(
              "Cannot execute \"mix eval\" without a Mix.Project, " <>
                "please ensure you are running Mix in a directory with a mix.exs file " <>
                "or pass the --no-mix-exs option"
            )
        end

        Code.eval_string(to_eval)
        Mix.Task.reenable("eval")

      _ ->
        Mix.raise("\"mix eval\" expects a single string to evaluate as argument")
    end
  end
end
