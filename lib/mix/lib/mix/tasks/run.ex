defmodule Mix.Tasks.Run do
  use Mix.Task

  @shortdoc "Runs the given file or expression"

  @moduledoc """
  Runs the given file or expression in the context of the application.

  You can use this task to execute a particular file or command:

      mix run -e Hello.world
      mix run my_script.exs

  This task provides a subset of the functionality available in the
  `elixir` executable, including setting up the `System.argv`:

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
    * `--no-compile` - do not compile even if files require compilation
    * `--no-deps-check` - do not check dependencies
    * `--no-archives-check` - do not check archives
    * `--no-halt` - do not halt the system after running the command
    * `--no-start` - do not start applications after compilation
    * `--no-elixir-version-check` - do not check the Elixir version from mix.exs

  """

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args,
      aliases: [r: :require, p: :parallel, e: :eval, c: :config],
      strict: [parallel: :boolean, require: :keep, eval: :keep, config: :keep,
               halt: :boolean, compile: :boolean, deps_check: :boolean, start: :boolean,
               archives_check: :boolean, elixir_version_check: :boolean, parallel_require: :keep])

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
        {true, _}    -> {nil, head}
        {_, [h | t]} -> {h, t}
        {_, []}      -> {nil, []}
      end

    System.argv(argv)
    process_config opts

    # Start app after rewriting System.argv,
    # but before requiring and evaling
    Mix.Task.run "app.start", args
    process_load opts

    _ = if file do
      if File.regular?(file) do
        Code.require_file(file)
      else
        Mix.raise "No such file: #{file}"
      end
    end

    unless Keyword.get(opts, :halt, true), do: :timer.sleep(:infinity)
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

  defp process_load(opts) do
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
        Code.eval_string(value)
      _ ->
        :ok
    end
  end

  defp filter_patterns(pattern) do
    Enum.filter(Enum.uniq(Path.wildcard(pattern)), &File.regular?(&1))
  end
end
