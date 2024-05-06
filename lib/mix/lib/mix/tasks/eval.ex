defmodule Mix.Tasks.Eval do
  use Mix.Task

  @shortdoc "Evaluates the given code"

  @moduledoc """
  Evaluates the given code within a configured application.

      $ mix eval "IO.puts(1 + 2)"

  The given code is evaluated after the current application
  has been configured but without loading or starting them
  (some applications may be loaded as part of booting but
  that's not guaranteed). See `mix run` for running your
  application and scripts within a started application.

  This task is designed to mirror the `bin/my_app eval` command
  in releases. It is typically used to invoke functions already
  defined within your application. For example, you may have a
  module such as:

      defmodule MyApp.ReleaseTasks do
        def migrate_database do
          Application.load(:my_app)
          ...
        end
      end

  Once defined, you can invoke this function either via `mix eval` or
  via `bin/my_app eval` inside a release as follows:

      $ mix eval MyApp.ReleaseTasks.migrate_database
      $ bin/my_app eval MyApp.ReleaseTasks.migrate_database

  As you can see, the current application has to be either explicitly
  loaded or started in your tasks, either by calling `Application.load/1`
  or `Application.ensure_all_started/1`. This gives you full control over
  the application booting life cycle. For more information, see the
  `Application` module.

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
      [to_eval | argv] ->
        cond do
          Mix.Project.get() ->
            Mix.Task.run("app.config", ["--no-app-loading" | args])

          "--no-mix-exs" in args ->
            :ok

          true ->
            Mix.raise(
              "Cannot execute \"mix eval\" without a Mix.Project, " <>
                "please ensure you are running Mix in a directory with a mix.exs file " <>
                "or pass the --no-mix-exs option"
            )
        end

        old_argv = System.argv()

        try do
          System.argv(argv)

          Code.eval_string(to_eval)
          Mix.Task.reenable("eval")
        after
          System.argv(old_argv)
        end

      _ ->
        Mix.raise("\"mix eval\" expects a single string to evaluate as argument")
    end
  end
end
