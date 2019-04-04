defmodule Mix.Tasks.Profile.Fprof do
  use Mix.Task

  @shortdoc "Profiles the given file or expression with fprof"

  @moduledoc """
  Profiles the given file or expression using Erlang's `fprof` tool.

  `fprof` can be useful when you want to discover the bottlenecks of a
  sequential code.

  Before running the code, it invokes the `app.start` task which compiles
  and loads your project. Then the target expression is profiled, together
  with all processes which are spawned by it. Other processes (e.g. those
  residing in the OTP application supervision tree) are not profiled.

  To profile the code, you can use syntax similar to the `mix run` task:

      mix profile.fprof -e Hello.world
      mix profile.fprof my_script.exs arg1 arg2 arg3

  This task is automatically reenabled, so you can profile multiple times
  in the same Mix invocation.

  ## Command line options

    * `--callers` - prints detailed information about immediate callers and called functions
    * `--details` - includes profile data for each profiled process
    * `--sort key` - sorts the output by given key: `acc` (default) or `own`
    * `--eval`, `-e` - evaluates the given code
    * `--require`, `-r` - requires pattern before running the command
    * `--parallel`, `-p` - makes all requires parallel
    * `--no-compile`    - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-archives-check` - does not check archives
    * `--no-start` - does not start applications after compilation
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs
    * `--no-warmup` - does not execute code once before profiling

  ## Profile output

  Example output:
      #                                        CNT    ACC (ms)    OWN (ms)
      Total                                 200279    1972.188    1964.579
      :fprof.apply_start_stop/4                  0    1972.188       0.012
      anonymous fn/0 in :elixir_compiler_2       1    1972.167       0.001
      Test.run/0                                 1    1972.166       0.007
      Test.do_something/1                        3    1972.131       0.040
      Test.bottleneck/0                          1    1599.490       0.007
      ...

  The default output contains data gathered from all profiled processes.
  All times are wall clock milliseconds. The columns have the following meaning:

    * CNT - total number of invocations of the given function
    * ACC - total time spent in the function
    * OWN - time spent in the function, excluding the time of called functions

  The first row (Total) is the sum of all functions executed in all profiled
  processes. For the given output, we had a total of 200279 function calls and spent
  about 2 seconds running the code.

  More detailed information is returned if you provide the `--callers` and
  `--details` options.

  When `--callers` option is specified, you'll see expanded function entries:

      Mod.caller1/0                             3     200.000       0.017
      Mod.caller2/0                             2     100.000       0.017
        Mod.some_function/0                     5     300.000       0.017  <--
          Mod.called1/0                         4     250.000       0.010
          Mod.called2/0                         1      50.000       0.030

  Here, the arrow (`<--`) indicates the __marked__ function - the function
  described by this paragraph. You also see its immediate callers (above) and
  called functions (below).

  All the values of caller functions describe the marked function. For example,
  the first row means that `Mod.caller1/0` invoked `Mod.some_function/0` 3 times.
  200ms of the total time spent in `Mod.some_function/0` was spent processing
  calls from this particular caller.

  In contrast, the values for the called functions describe those functions, but
  in the context of the marked function. For example, the last row means that
  `Mod.called2/0` was called once by `Mod.some_function/0`, and in that case
  the total time spent in the function was 50ms.

  For a detailed explanation it's worth reading the analysis in
  [Erlang/OTP documentation for fprof](http://www.erlang.org/doc/man/fprof.html#analysis).

  ## Caveats

  You should be aware that the code being profiled is running in an anonymous
  function which is invoked by [`:fprof` module](http://wwww.erlang.org/doc/man/fprof.html).
  Thus, you'll see some additional entries in your profile output,
  such as `:fprof` calls, an anonymous
  function with high ACC time, or an `:undefined` function which represents
  the outer caller (non-profiled code which started the profiler).

  Also, keep in mind that profiling might significantly increase the running time
  of the profiled processes. This might skew your results if, for example, those
  processes perform some I/O operations, since running time of those operations
  will remain unchanged, while CPU bound operations of the profiled processes
  might take significantly longer. Thus, when profiling some intensive program,
  try to reduce such dependencies, or be aware of the resulting bias.

  Finally, it's advised to profile your program with the `prod` environment, since
  this should provide more realistic insights into bottlenecks.
  """

  @switches [
    parallel: :boolean,
    require: :keep,
    eval: :keep,
    config: :keep,
    compile: :boolean,
    deps_check: :boolean,
    start: :boolean,
    archives_check: :boolean,
    details: :boolean,
    callers: :boolean,
    sort: :string,
    elixir_version_check: :boolean,
    warmup: :boolean,
    parallel_require: :keep
  ]

  @aliases [r: :require, p: :parallel, e: :eval, c: :config]

  @impl true
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args, aliases: @aliases, strict: @switches)
    Mix.Task.reenable("profile.fprof")

    Mix.Tasks.Run.run(
      ["--no-mix-exs" | args],
      opts,
      head,
      &profile_code(&1, opts),
      &profile_code(File.read!(&1), opts)
    )
  end

  # Profiling functions

  defp profile_code(code_string, opts) do
    content =
      quote do
        unquote(__MODULE__).profile(
          fn ->
            unquote(Code.string_to_quoted!(code_string))
          end,
          unquote(Macro.escape(Enum.map(opts, &parse_opt/1)))
        )
      end

    # Use compile_quoted since it leaves less noise than eval_quoted
    Code.compile_quoted(content)
  end

  defp parse_opt({:sort, "acc"}), do: {:sort, :acc}
  defp parse_opt({:sort, "own"}), do: {:sort, :own}
  defp parse_opt({:sort, other}), do: Mix.raise("Invalid sort option: #{other}")
  defp parse_opt(other), do: other

  @doc """
  Allows to programmatically run the `fprof` profiler on expression in `fun`.

  ## Options

    * `:callers` - prints detailed information about immediate callers and called functions
    * `:details` - includes profile data for each profiled process
    * `:sort` - sorts the output by given key: `:acc` (default) or `:own`

  """
  def profile(fun, opts \\ []) when is_function(fun, 0) do
    fun
    |> profile_and_analyse(opts)
    |> print_output
  end

  defp profile_and_analyse(fun, opts) do
    if Keyword.get(opts, :warmup, true) do
      IO.puts("Warmup...")
      fun.()
    end

    {:ok, tracer} = :fprof.profile(:start)
    :fprof.apply(fun, [], tracer: tracer)

    {:ok, analyse_dest} = StringIO.open("")

    try do
      :fprof.analyse(
        dest: analyse_dest,
        totals: true,
        details: Keyword.get(opts, :details, false),
        callers: Keyword.get(opts, :callers, false),
        sort: Keyword.get(opts, :sort, :acc)
      )
    else
      :ok ->
        {_in, analysis_output} = StringIO.contents(analyse_dest)
        String.to_charlist(analysis_output)
    after
      StringIO.close(analyse_dest)
    end
  end

  defp print_output(analysis_output) do
    {_analysis_options, analysis_output} = next_term(analysis_output)
    {total_row, analysis_output} = next_term(analysis_output)
    print_total_row(total_row)

    Stream.unfold(analysis_output, &next_term/1)
    |> Enum.each(&print_analysis_result/1)
  end

  defp next_term(charlist) do
    case :erl_scan.tokens([], charlist, 1) do
      {:done, result, leftover} ->
        case result do
          {:ok, tokens, _} ->
            {:ok, term} = :erl_parse.parse_term(tokens)
            {term, leftover}

          {:eof, _} ->
            nil
        end

      _ ->
        nil
    end
  end

  defp print_total_row([{:totals, count, acc, own}]) do
    IO.puts("")
    print_row(["s", "s", "s", "s", "s"], ["", "CNT", "ACC (ms)", "OWN (ms)", ""])
    print_row(["s", "B", ".3f", ".3f", "s"], ["Total", count, acc, own, ""])
  end

  # Represents the "PID" entry
  defp print_analysis_result([{pid_atom, count, :undefined, own} | info]) do
    print_process(pid_atom, count, own)

    if spawned_by = info[:spawned_by] do
      IO.puts("  spawned by #{spawned_by}")
    end

    if spawned_as = info[:spawned_as] do
      IO.puts("  as #{function_text(spawned_as)}")
    end

    if initial_calls = info[:initial_calls] do
      IO.puts("  initial calls:")
      Enum.each(initial_calls, &IO.puts("    #{function_text(&1)}"))
    end

    IO.puts("")
  end

  # The function entry, when --callers option is provided
  defp print_analysis_result({callers, function, subcalls}) do
    IO.puts("")
    Enum.each(callers, &print_function/1)
    print_function(function, "  ", "<--")
    Enum.each(subcalls, &print_function(&1, "    "))
  end

  # The function entry in the total section, and when --callers option is not
  # provided
  defp print_analysis_result({_fun, _count, _acc, _own} = function) do
    print_function(function, "", "")
  end

  defp print_process(pid_atom, count, own) do
    IO.puts([?\n, String.duplicate("-", 100)])
    print_row(["s", "B", "s", ".3f", "s"], ["#{pid_atom}", count, "", own, ""])
  end

  defp print_function({fun, count, acc, own}, prefix \\ "", suffix \\ "") do
    text = "#{prefix}#{function_text(fun)}"
    print_row(["s", "B", ".3f", ".3f", "s"], [text, count, acc, own, suffix])
  end

  defp function_text({module, function, arity}) do
    Exception.format_mfa(module, function, arity)
  end

  defp function_text(other), do: inspect(other)

  @columns [-60, 10, 12, 12, 5]
  defp print_row(formats, data) do
    Stream.zip(@columns, formats)
    |> Stream.map(fn {width, format} -> "~#{width}#{format}" end)
    |> Enum.join()
    |> :io.format(data)

    IO.puts("")
  end
end
