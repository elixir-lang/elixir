defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Runs the given file or expression"

  @moduledoc """
  Runs the given file or expression in the context of the application.

  You can use this task to execute a particular file or command:

      mix run -e Hello.world
      mix run my_script.exs

  This task provides a subset of the functionality available in the
  `elixir` executable, including setting up the `System.argv/0` arguments:

      mix run my_script.exs arg1 arg2 arg3

  You can also use this task to simply start an application and keep
  it running without halting:

      mix run --no-halt

  Before running any command, the task compiles and starts the current
  application. Those can be configured with the options below.

  You may also pass options specific to the `elixir` executable as follows:

      elixir --sname hello -S mix run --no-halt

  ## Command line options

    * `--config`, `-c`  - loads the given configuration file
    * `--eval`, `-e` - evaluate the given code
    * `--require`, `-r` - requires pattern before running the command
    * `--parallel`, `-p` - makes all requires parallel
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-archives-check` - does not check archives
    * `--no-halt` - does not halt the system after running the command
    * `--no-mix-exs` - allows the command to run even if there is no mix.exs
    * `--no-start` - does not start applications after compilation
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args,
      aliases: [r: :require, p: :parallel, e: :eval, c: :config],
      strict: [parallel: :boolean, require: :keep, eval: :keep, config: :keep, mixexs: :boolean,
               halt: :boolean, compile: :boolean, deps_check: :boolean, start: :boolean,
               archives_check: :boolean, elixir_version_check: :boolean, parallel_require: :keep])

    run(args, opts, head, &Code.eval_string/1, &Code.require_file/1)
    unless Keyword.get(opts, :halt, true), do: Process.sleep(:infinity)
    :ok
  end

  @doc false
  @spec run(OptionParser.argv, keyword, OptionParser.argv,
            (String.t -> term()), (String.t -> term())) :: :ok
  def run(args, opts, head, expr_evaluator, file_evaluator) do
    # TODO: Remove on v2.0
    opts =
      Enum.flat_map(opts, fn
        {:parallel_require, value} ->
          IO.warn "the --parallel-require option is deprecated in favour of using " <>
            "--parallel to make all requires parallel and --require VAL for requiring"
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
      Mix.Project.get ->
        Mix.Task.run "app.start", args
      "--no-mix-exs" in args ->
        :ok
      true ->
        Mix.raise "Cannot execute \"mix run\" without a Mix.Project, " <>
                  "please ensure you are running Mix in a directory with a mix.exs file " <>
                  "or pass the --no-mix-exs flag"
    end

    process_load(opts, expr_evaluator)

    if file do
      if File.regular?(file) do
        file_evaluator.(file)
      else
        Mix.raise "No such file: #{file}"
      end
    end

    :ok
  end

  defp process_config(opts) do
    Enum.each opts, fn
      {:config, value} ->
        Mix.Task.run "loadconfig", [value]
      _ ->
        :ok
    end
  end

  defp process_load(opts, expr_evaluator) do
    require_runner =
      if opts[:parallel] do
        &Kernel.ParallelRequire.files/1
      else
        fn(files) -> Enum.each(files, &Code.require_file/1) end
      end

    Enum.each opts, fn
      {:require, value} ->
        case filter_patterns(value) do
          [] ->
            Mix.raise "No files matched pattern #{inspect value} given to --require"
          filtered ->
            require_runner.(filtered)
        end
      {:eval, value} ->
        expr_evaluator.(value)
      _ ->
        :ok
    end
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), &File.regular?(&1))
  end
end
