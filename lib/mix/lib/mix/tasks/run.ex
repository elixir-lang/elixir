defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Runs the current application"

  @moduledoc """
  Runs the current application.

  `mix run` starts the current application dependencies and the
  application itself. The application will be compiled if it has
  not been compiled yet or it is outdated.

  `mix run` may also run code in the application context through
  additional options. For example, to run a script within the
  current application, you may pass a filename as argument:

      $ mix run my_app_script.exs arg1 arg2 arg3

  Code to be executed can also be passed inline with the `-e` option:

      $ mix run -e "DbUtils.delete_old_records()" -- arg1 arg2 arg3

  In both cases, the command-line arguments for the script or expression
  are available in `System.argv/0`. This mirrors the command line interface
  in the `elixir` executable.

  For starting long running systems, one typically passes the `--no-halt`
  option:

      $ mix run --no-halt

  The `--no-start` option can also be given and the current application,
  nor its dependencies will be started. Alternatively, you may use
  `mix eval` to evaluate a single expression without starting the current
  application.

  If you need to pass options to the Elixir executable at the same time
  you use `mix run`, it can be done as follows:

      $ elixir --sname hello -S mix run --no-halt

  This task is automatically re-enabled, so it can be called multiple times
  with different arguments.

  ## Command-line options

    * `--eval`, `-e` - evaluates the given code
    * `--require`, `-r` - executes the given pattern/file
    * `--parallel`, `-p` - makes all requires parallel
    * `--preload-modules` - preloads all modules defined in applications
    * `--no-archives-check` - does not check archives
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs
    * `--no-halt` - does not halt the system after running the command
    * `--no-mix-exs` - allows the command to run even if there is no mix.exs
    * `--no-start` - does not start applications after compilation

  """

  @impl true
  def run(args) do
    {opts, head} =
      OptionParser.parse_head!(
        args,
        aliases: [r: :require, p: :parallel, e: :eval, c: :config],
        strict: [
          parallel: :boolean,
          require: :keep,
          eval: :keep,
          config: :keep,
          mix_exs: :boolean,
          halt: :boolean,
          compile: :boolean,
          deps_check: :boolean,
          start: :boolean,
          archives_check: :boolean,
          elixir_version_check: :boolean,
          parallel_require: :keep,
          preload_modules: :boolean
        ],
        return_separator: true
      )

    run(args, opts, head, &Code.eval_string/1, &Code.require_file/1)
    if not Keyword.get(opts, :halt, true), do: System.no_halt(true)
    Mix.Task.reenable("run")
    :ok
  end

  @doc false
  @spec run(
          OptionParser.argv(),
          keyword,
          OptionParser.argv(),
          (String.t() -> term()),
          (String.t() -> term())
        ) :: :ok
  def run(args, opts, head, expr_evaluator, file_evaluator) do
    opts =
      Enum.flat_map(opts, fn
        {:parallel_require, value} ->
          IO.warn(
            "the --parallel-require option is deprecated in favour of using " <>
              "--parallel to make all requires parallel and --require VAL for requiring"
          )

          [require: value, parallel: true]

        opt ->
          [opt]
      end)

    {file, argv} =
      case {Keyword.has_key?(opts, :eval), head} do
        {_, ["--" | rest]} -> {nil, rest}
        {true, _} -> {nil, head}
        {_, [head | tail]} -> {head, tail}
        {_, []} -> {nil, []}
      end

    System.argv(argv)
    process_config(opts)

    # Start app after rewriting System.argv,
    # but before requiring and evaling.
    cond do
      Mix.Project.get() ->
        Mix.Task.run("app.start", args)

      "--no-mix-exs" in args ->
        :ok

      true ->
        Mix.raise(
          "Cannot execute \"mix run\" without a Mix.Project, " <>
            "please ensure you are running Mix in a directory with a mix.exs file " <>
            "or pass the --no-mix-exs option"
        )
    end

    process_load(opts, expr_evaluator)

    if file do
      if File.regular?(file) do
        file_evaluator.(file)
      else
        Mix.raise("No such file: #{file}")
      end
    end

    :ok
  end

  defp process_config(opts) do
    for {:config, value} <- opts do
      # TODO: Remove on v2.0.
      IO.warn(
        "the --config flag is deprecated. If you need to handle multiple configurations, " <>
          "it is preferable to dynamically import them in your config files"
      )

      Mix.Tasks.Loadconfig.load_compile(value)
    end

    :ok
  end

  defp process_load(opts, expr_evaluator) do
    require_runner =
      if opts[:parallel] do
        fn files ->
          case Kernel.ParallelCompiler.require(files) do
            {:ok, _, _} -> :ok
            {:error, _, _} -> exit({:shutdown, 1})
          end
        end
      else
        fn files -> Enum.each(files, &Code.require_file/1) end
      end

    Enum.each(opts, fn
      {:require, value} ->
        case filter_patterns(value) do
          [] ->
            Mix.raise("No files matched pattern #{inspect(value)} given to --require")

          filtered ->
            require_runner.(filtered)
        end

      {:eval, value} ->
        expr_evaluator.(value)

      _ ->
        :ok
    end)
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), &File.regular?(&1))
  end
end
