defmodule Mix.Tasks.Profile.Tprof do
  use Mix.Task

  @shortdoc "Profiles the given file or expression with tprof"

  @moduledoc """
  Profiles the given file or expression using Erlang's `tprof` tool.

  Requires Erlang/OTP27 or above.

  [`:tprof`](`:tprof`) is an experimental module introduced in Erlang/OTP 27 which
  provides a unified API for measuring call count, time, and allocation, and aims to
  replace [`:eprof`](`:eprof`) and [`:cprof`](`:cprof`).
  It can be useful when you want to discover the bottlenecks related to any of these
  measurements.

  Before running the code, it invokes the `app.start` task which compiles
  and loads your project. After that, the target expression is profiled together
  with all matching function calls using the Erlang trace BIFs. The tracing of
  the function calls for that is enabled when the profiling is begun, and
  disabled when profiling is stopped.

  To profile the code, you can use syntax similar to the `mix run` task:

      $ mix profile.tprof -e Hello.world
      $ mix profile.tprof -e "[1, 2, 3] |> Enum.reverse |> Enum.map(&Integer.to_string/1)"
      $ mix profile.tprof my_script.exs arg1 arg2 arg3

  By default, tprof uses the `time` type, but you can profile memory too:

      $ mix profile.tprof -e "Enum.map([1, 2, 3], &Integer.to_string/1)" --type memory

  Call count is present with both type `time` and `memory`, but if you only need
  the call count information, you can use the type `calls` which has the lowest footprint:

      $ mix profile.tprof -e "Enum.map([1, 2, 3], &Integer.to_string/1)" --type calls

  This task is automatically re-enabled, so you can profile multiple times
  in the same Mix invocation.

  ## Command line options

    * `--matching` - only profile calls matching the given `Module.function/arity` pattern
    * `--type` - the type of profiling, `calls`, `time` or `memory` (default: `time`)
    * `--calls` - filters out any results with a call count lower than this
    * `--time` - filters out any results that took lower than specified (in µs), the `type` needs to be `time`
    * `--memory` - filters out any results that used less memory than specified (in words), the `type` needs to be `memory`
    * `--sort` - sorts the results by `calls`, `per_call` or by the value of `type` (default: the value of `type`)
    * `--report` - returns the per-process breakdown when `process`, or the total for all processes when `total` (default: `process`).
      Always `total` when `type` is `calls`.
    * `--eval`, `-e` - evaluates the given code
    * `--require`, `-r` - requires pattern before running the command
    * `--parallel`, `-p` - makes all requires parallel
    * `--no-warmup` - skips the warmup step before profiling
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-archives-check` - does not check archives
    * `--no-halt` - does not halt the system after running the command
    * `--no-start` - does not start applications after compilation
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  ## Profile output

  Example output (`time` type):

      Profile results of #PID<0.107.0>
      #                                               CALLS      % TIME µS/CALL
      Total                                              20 100.00    2    0.10
      String.Chars.Integer.to_string/1                    5   0.00    0    0.00
      anonymous fn/0 in :elixir_compiler_1.__FILE__/1     1   0.00    0    0.00
      Enum.each/2                                         1   0.00    0    0.00
      Enum.reduce_range/5                                 3   0.00    0    0.00
      :erlang.integer_to_binary/1                         5  50.00    1    0.20
      anonymous fn/3 in Enum.each/2                       5  50.00    1    0.20

      Profile done over 6 matching functions

  Example output (`memory` type):

      Profile results of #PID<0.107.0>
      #                           CALLS      % WORDS PER CALL
      Total                           6 100.00    19     3.17
      Enum.each/2                     1  21.05     4     4.00
      :erlang.integer_to_binary/1     5  78.95    15     3.00

      Profile done over 2 matching functions

  Example output (`calls` type)

      Profile results over all processes
      #                                               CALLS      %
      Total                                              20 100.00
      anonymous fn/0 in :elixir_compiler_1.__FILE__/1     1   5.00
      Enum.each/2                                         1   5.00
      Enum.reduce_range/5                                 3  15.00
      :erlang.integer_to_binary/1                         5  25.00
      String.Chars.Integer.to_string/1                    5  25.00
      anonymous fn/3 in Enum.each/2                       5  25.00

      Profile done over 6 matching functions

  The default output contains data gathered from all matching functions. The first
  row after the header contains the sums of the partial results and the average time
  or memory usage for all the function calls listed.
  The following rows contain the function call, followed by the number of times that
  the function was called, then by the percentage of time/memory that the call uses,
  then the total time/memory for that function in microseconds/words, and, finally,
  the average time/memory per call in microseconds/words.

  When `--matching` option is specified, call count tracing will be started only for
  the functions matching the given pattern:

      Profile results of #PID<0.106.0>
      #                                CALLS      % TIME µS/CALL
      Total                                5 100.00    1    0.20
      String.Chars.Integer.to_string/1     5 100.00    1    0.20

      Profile done over 1 matching functions

  The pattern can be a module name, such as `String` to count all calls to that module,
  a call without arity, such as `String.split`, to count all calls to that function
  regardless of arity, or a call with arity, such as `String.split/3`, to count all
  calls to that exact module, function and arity.

  ## Caveats

  You should be aware that the code being profiled is running in an anonymous
  function which is invoked by [`:tprof` module](https://www.erlang.org/doc/man/tprof.html).
  Thus, you'll see some additional entries in your profile output. It is also
  important to note that the profiler is stopped as soon as the code has finished running,
  and this may need special attention, when: running asynchronous code as function calls which were
  called before the profiler stopped will not be counted; running synchronous code as long
  running computations and a profiler without a proper MFA trace pattern or filter may
  lead to a result set which is difficult to comprehend.

  You should expect a slowdown in your code execution using this tool since `:tprof` has
  some performance impact on the execution, but the impact is considerably lower than
  `Mix.Tasks.Profile.Fprof`. If you have a large system try to profile a limited
  scenario or focus on the main modules or processes. The `calls` type can also be used,
  which is more limited but has a lower footprint.
  """

  @switches [
    parallel: :boolean,
    require: :keep,
    eval: :keep,
    config: :keep,
    matching: :string,
    halt: :boolean,
    compile: :boolean,
    deps_check: :boolean,
    type: :string,
    calls: :integer,
    time: :integer,
    memory: :integer,
    sort: :string,
    report: :string,
    start: :boolean,
    archives_check: :boolean,
    warmup: :boolean,
    elixir_version_check: :boolean,
    parallel_require: :keep
  ]

  @aliases [
    r: :require,
    p: :parallel,
    e: :eval,
    c: :config
  ]

  @impl true
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args, aliases: @aliases, strict: @switches)
    Mix.Task.reenable("profile.tprof")

    Mix.Tasks.Run.run(
      ["--no-mix-exs" | args],
      opts,
      head,
      &profile_code(&1, opts),
      &profile_code(File.read!(&1), opts)
    )
  end

  defp profile_code(code_string, opts) do
    opts = Enum.map(opts, &parse_opt/1)

    content =
      quote do
        unquote(__MODULE__).profile(
          fn ->
            unquote(Code.string_to_quoted!(code_string))
          end,
          unquote(Macro.escape(opts))
        )
      end

    # Use compile_quoted since it leaves less noise than eval_quoted
    Code.compile_quoted(content)
  end

  defp parse_opt({:matching, matching}) do
    case Mix.Utils.parse_mfa(matching) do
      {:ok, [m, f, a]} -> {:matching, {m, f, a}}
      {:ok, [m, f]} -> {:matching, {m, f, :_}}
      {:ok, [m]} -> {:matching, {m, :_, :_}}
      :error -> Mix.raise("Invalid matching pattern: #{matching}")
    end
  end

  defp parse_opt({:type, "time"}), do: {:type, :time}
  defp parse_opt({:type, "calls"}), do: {:type, :calls}
  defp parse_opt({:type, "memory"}), do: {:type, :memory}
  defp parse_opt({:type, other}), do: Mix.raise("Invalid type option: #{other}")

  defp parse_opt({:report, "process"}), do: {:report, :process}
  defp parse_opt({:report, "total"}), do: {:report, :total}
  defp parse_opt({:report, other}), do: Mix.raise("Invalid report option: #{other}")

  defp parse_opt({:sort, "time"}), do: {:sort, :time}
  defp parse_opt({:sort, "calls"}), do: {:sort, :calls}
  defp parse_opt({:sort, "memory"}), do: {:sort, :memory}
  defp parse_opt({:sort, "per_call"}), do: {:sort, :per_call}
  defp parse_opt({:sort, other}), do: Mix.raise("Invalid sort option: #{other}")
  defp parse_opt(other), do: other

  @doc """
  Allows to programmatically run the `tprof` profiler on expression in `fun`.

  Returns the return value of `fun`.

  ## Options

    * `:matching` - only profile calls matching the given pattern in form of
      `{module, function, arity}`, where each element may be replaced by `:_`
      to allow any value
    * `:type` - the type of profiling, possible values are `:time`, `:memory` or `:calls`,
      (default: `:time`), see [moduledoc](`Mix.Tasks.Profile.Tprof`) for more information

    * `:calls` - filters out any results with a call count lower than this
    * `:time` - filters out any results that took lower than specified (in µs),
      `type` needs to be `:time`
    * `:memory` - filters out any results that used less memory than specified (in words),
      `type` needs to be `:memory`
    * `:sort` - sort the results by `:calls`, `:per_call` or by the value of `type`
      (default: the value of `type`)
    * `:report` - returns the per-process breakdown when `:process`, or the total for all
      processes when `:total` (default: `:process`). Always `:total` when `type` is `:calls`.
    * `:warmup` - if the code should be warmed up before profiling (default: `true`)
    * `:set_on_spawn` - if newly spawned processes should be measured (default: `true`)

  """
  @spec profile((-> result), keyword()) :: result when result: any()
  def profile(fun, opts \\ []) when is_function(fun, 0) do
    Mix.ensure_application!(:tools)
    {type, return_value, results} = profile_and_analyse(fun, opts)
    print_output(type, results)
    return_value
  end

  defp profile_and_analyse(fun, opts) do
    if Keyword.get(opts, :warmup, true) do
      IO.puts("Warmup...\n")
      fun.()
    end

    matching = Keyword.get(opts, :matching, {:_, :_, :_})
    set_on_spawn = Keyword.get(opts, :set_on_spawn, true)
    type = Keyword.get(opts, :type, :time)
    report = Keyword.get(opts, :report, :process)

    sort_by =
      case Keyword.get(opts, :sort) do
        nil ->
          :measurement

        :calls ->
          :calls

        :per_call ->
          :measurement_per_call

        ^type ->
          :measurement

        other ->
          Mix.raise("Incompatible sort option #{inspect(other)} with type #{inspect(type)}")
      end

    tprof_type = to_tprof_type(type)

    {return_value, {^tprof_type, traces}} =
      tprof_module().profile(fun, %{
        set_on_spawn: set_on_spawn,
        pattern: matching,
        type: tprof_type,
        report: :return
      })

    inspected = tprof_module().inspect({tprof_type, traces}, report, sort_by)

    results =
      inspected
      |> Enum.map(fn {pid, {^tprof_type, measurement_total, call_results}} ->
        parsed_calls =
          call_results
          |> filter_results(type, opts)
          |> add_totals(measurement_total)

        {pid, parsed_calls}
      end)

    {type, return_value, results}
  end

  defp to_tprof_type(:calls), do: :call_count
  defp to_tprof_type(:time), do: :call_time
  defp to_tprof_type(:memory), do: :call_memory

  defp filter_results(call_results, type, opts) do
    calls_opt = Keyword.get(opts, :calls, 0)

    measurement_opt =
      get_filter_value!(type, Keyword.get(opts, :time), Keyword.get(opts, :memory))

    Enum.filter(call_results, fn {_module, _fa, count, measurement, _, _} ->
      count >= calls_opt and measurement >= measurement_opt
    end)
  end

  defp get_filter_value!(type, time, _memory) when is_integer(time) and type != :time do
    Mix.raise("Incompatible use of time option with type #{inspect(type)}")
  end

  defp get_filter_value!(type, _time, memory) when is_integer(memory) and type != :memory do
    Mix.raise("Incompatible use of memory option with type #{inspect(type)}")
  end

  defp get_filter_value!(:time, time, nil) when is_integer(time), do: time
  defp get_filter_value!(:memory, nil, memory) when is_integer(memory), do: memory

  defp get_filter_value!(_, nil, nil), do: 0

  defp add_totals(call_results, measurement_total) do
    {function_count, calls} =
      Enum.reduce(call_results, {0, 0}, fn {_mod, _fa, count, _, _, _}, acc ->
        {function_count, calls} = acc
        {function_count + 1, calls + count}
      end)

    {function_count, call_results, calls, measurement_total}
  end

  defp print_output(_type, []) do
    print_function_count(0)
  end

  defp print_output(type, results) do
    Enum.each(results, &print_result(type, &1))
  end

  defp print_result(type, {pid, {function_count, call_results, calls, total_measurement}}) do
    header = header(type)

    formatted_rows = Enum.map(call_results, &format_row/1)
    formatted_total = format_total(total_measurement, calls)

    column_lengths = column_lengths(header, [formatted_total | formatted_rows])

    IO.puts("")

    print_pid_row(pid)
    print_row(header, column_lengths, type)
    print_row(formatted_total, column_lengths, type)
    Enum.each(formatted_rows, &print_row(&1, column_lengths, type))

    IO.puts("")

    print_function_count(function_count)
  end

  defp header(:calls), do: ["#", "CALLS", "%"]
  defp header(:time), do: ["#", "CALLS", "%", "TIME", "µS/CALL"]
  defp header(:memory), do: ["#", "CALLS", "%", "WORDS", "PER CALL"]

  defp print_pid_row(:all) do
    IO.puts("Profile results over all processes")
  end

  defp print_pid_row(pid) when is_pid(pid) do
    IO.puts("Profile results of #{inspect(pid)}")
  end

  defp format_row({module, {function, arity}, count, measurement, per_call, percentage}) do
    mfa = Exception.format_mfa(module, function, arity)
    percentage = :erlang.float_to_binary(percentage, [{:decimals, 2}])
    per_call = :erlang.float_to_binary(per_call, [{:decimals, 2}])
    count = Integer.to_string(count)
    measurement = Integer.to_string(measurement)

    [mfa, count, percentage, measurement, per_call]
  end

  defp format_total(total_measurement, total_count) do
    per_call = :erlang.float_to_binary(divide(total_measurement, total_count), [{:decimals, 2}])

    [
      "Total",
      Integer.to_string(total_count),
      "100.00",
      Integer.to_string(total_measurement),
      per_call
    ]
  end

  defp divide(_, 0), do: 0.0
  defp divide(t, n), do: t / n

  defp column_lengths(header, rows) do
    max_lengths = Enum.map(header, &String.length/1)

    Enum.reduce(rows, max_lengths, fn row, max_lengths ->
      Enum.zip_with(row, max_lengths, fn cell, length -> String.length(cell) |> max(length) end)
    end)
  end

  @call_format "~-*s ~*s ~*s~n"
  @measurement_format "~-*s ~*s ~*s ~*s ~*s~n"

  defp print_row(row, column_lengths, type) do
    to_print =
      column_lengths
      |> Stream.zip(Stream.map(row, &String.to_charlist/1))
      |> Enum.flat_map(&Tuple.to_list/1)

    case type do
      :calls -> :io.format(@call_format, to_print)
      _ -> :io.format(@measurement_format, to_print)
    end
  end

  defp print_function_count(count) do
    IO.puts("Profile done over #{count} matching functions")
  end

  # TODO remove once we require Erlang/OTP 27+
  defp tprof_module do
    if Code.ensure_loaded?(:tprof) do
      :tprof
    else
      Mix.raise("mix profile.tprof requires Erlang/OTP 27 or above")
    end
  end
end
