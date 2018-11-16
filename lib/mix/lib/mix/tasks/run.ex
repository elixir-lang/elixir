defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Starts and runs the current application"

  @moduledoc """
  Starts the current application and runs code.

  `mix run` can be used to start the current application dependencies,
  the application itself, and optionally run some code in its context.
  For long running systems, this is typically done with the `--no-halt`
  option:

      mix run --no-halt

  Once the current application and its dependencies have been started,
  you can run a script in its context by passing a filename:

      mix run my_app_script.exs arg1 arg2 arg3

  Code to be executed can also be passed inline with the `-e` option:

      mix run -e "DbUtils.delete_old_records()" -- arg1 arg2 arg3

  In both cases, the command-line arguments for the script or expression
  are available in `System.argv/0`.

  Before doing anything, Mix will compile the current application if
  needed, unless you pass `--no-compile`.

  If for some reason the application needs to be configured before it is
  started, the `--no-start` option can be used and you are then responsible
  for starting all applications by using functions such as
  `Application.ensure_all_started/1`. For more information about the
  application life-cycle and dynamically configuring applications, see
  the `Application` module.

  If you need to pass options to the Elixir executable at the same time
  you use `mix run`, it can be done as follows:

      elixir --sname hello -S mix run --no-halt

  This task is automatically reenabled, so it can be called multiple times
  with different arguments.

  ## Command-line options

    * `--config`, `-c`  - loads the given configuration file
    * `--eval`, `-e` - evaluates the given code
    * `--require`, `-r` - executes the given pattern/file
    * `--parallel`, `-p` - makes all requires parallel
    * `--preload-modules` - preloads all modules defined in applications
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-archives-check` - does not check archives
    * `--no-halt` - does not halt the system after running the command
    * `--no-mix-exs` - allows the command to run even if there is no mix.exs
    * `--no-start` - does not start applications after compilation
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

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
        ]
      )

    run(args, opts, head, &Code.eval_string/1, &Code.require_file/1)
    unless Keyword.get(opts, :halt, true), do: Process.sleep(:infinity)
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
    # TODO: Remove on v2.0
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
    Enum.each(opts, fn
      {:config, value} ->
        Mix.Task.run("loadconfig", [value])

      _ ->
        :ok
    end)
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
