defmodule Mix.Tasks.Profile.Cprof do
  use Mix.Task

  @shortdoc "Profiles the given file or expression with cprof"

  @moduledoc """
  Profiles the given file or expression using Erlang's `cprof` tool.

  `cprof` can be useful when you want to discover the bottlenecks related
  to function calls.

  Before running the code, it invokes the `app.start` task which compiles
  and loads your project. Then the target expression is profiled, together
  with all matching function calls, by setting breakpoints containing
  counters. These can only be set on BEAM code so BIFs cannot be call
  count traced.

  To profile the code, you can use syntax similar to the `mix run` task:

      mix profile.cprof -e Hello.world
      mix profile.cprof -e "[1, 2, 3] |> Enum.reverse |> Enum.map(&Integer.to_string/1)"
      mix profile.cprof my_script.exs arg1 arg2 arg3

  This task is automatically reenabled, so you can profile multiple times
  in the same Mix invocation.

  ## Command line options

    * `--matching` - only profile calls matching the given `Module.function/arity` pattern
    * `--limit` - filters out any results with a call count less than the limit
    * `--module` - filters out any results not pertaining to the given module
    * `--eval`, `-e` - evaluate the given code
    * `--require`, `-r` - requires pattern before running the command
    * `--parallel`, `-p` - makes all requires parallel
    * `--no-compile` - does not compile even if files require compilation
    * `--no-deps-check` - does not check dependencies
    * `--no-archives-check` - does not check archives
    * `--no-halt` - does not halt the system after running the command
    * `--no-start` - does not start applications after compilation
    * `--no-elixir-version-check` - does not check the Elixir version from mix.exs

  ## Profile output

  Example output:
                                                                           CNT
      Total                                                                 15
      Enum                                                                   6  <--
        Enum."-map/2-lists^map/1-0-"/2                                       4
        Enum.reverse/1                                                       1
        Enum.map/2                                                           1
      :elixir_compiler                                                       4  <--
        anonymous fn/1 in :elixir_compiler.__FILE__/1                        3
        anonymous fn/0 in :elixir_compiler.__FILE__/1                        1
      String.Chars.Integer                                                   3  <--
        String.Chars.Integer.to_string/1                                     3
      :erlang                                                                2  <--
        :erlang.trace_pattern/3                                              2
      Profile done over 20229 matching functions

  The default output contains data gathered from all matching functions. The left
  column structures each module and its total call count trace is presented on the right.
  Each module has its count discriminated by function below. The `<--` symbol is meant to
  help visualize where a new module call count begins.

  The first row (Total) is the sum of all function calls. In the last row the number of
  matching functions that were considered for profiling is presented.

  When `--matching` option is specified, call count tracing will be started only for
  the functions matching the given pattern:

      String.Chars.Integer                                                   3  <--
        String.Chars.Integer.to_string/1                                     3
      Profile done over 1 matching functions

  The pattern can be a module name, such as `String` to count all calls to that module,
  a call without arity, such as `String.split`, to count all calls to that function
  regardless of arity, or a call with arity, such as `String.split/2`, to count all
  calls to that exact module, function and arity.

  ## Caveats

  You should be aware the profiler is stopped as soon as the code has finished running. This
  may need special attention, when:  running asynchronous code as function calls which were
  called before the profiler stopped will not be counted; running synchronous code as long
  running computations and a profiler without a proper MFA trace pattern or filter may
  lead to a result set which is difficult to comprehend.

  Other caveats are the impossibility to call count trace BIFs, since breakpoints can
  only be set on BEAM code; functions calls performed by `:cprof` are not traced; the
  maximum size of a call counter is equal to the host machine's word size
  (for example, 2147483647 in a 32-bit host).
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
    limit: :integer,
    module: :string,
    start: :boolean,
    archives_check: :boolean,
    warmup: :boolean,
    elixir_version_check: :boolean,
    parallel_require: :keep
  ]

  @aliases [r: :require, p: :parallel, e: :eval, c: :config]

  @impl true
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args, aliases: @aliases, strict: @switches)
    Mix.Task.reenable("profile.cprof")

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

  defp parse_opt({:module, module}), do: {:module, string_to_existing_module(module)}
  defp parse_opt(other), do: other

  @doc """
  Allows to programmatically run the `cprof` profiler on expression in `fun`.

  ## Options

    * `:matching` - only profile calls matching the given pattern in form of
      `{module, function, arity}`, where each element may be replaced by `:_`
      to allow any value
    * `:limit` - filters out any results with a call count less than the limit
    * `:module` - filters out any results not pertaining to the given module

  """
  def profile(fun, opts \\ []) when is_function(fun, 0) do
    fun
    |> profile_and_analyse(opts)
    |> print_output

    :cprof.stop()
  end

  defp profile_and_analyse(fun, opts) do
    if Keyword.get(opts, :warmup, true) do
      IO.puts("Warmup...")
      fun.()
    end

    num_matched_functions =
      case Keyword.fetch(opts, :matching) do
        {:ok, matching} -> :cprof.start(matching)
        :error -> :cprof.start()
      end

    apply(fun, [])

    :cprof.pause()

    limit = Keyword.get(opts, :limit)
    module = Keyword.get(opts, :module)

    analysis_result =
      case {limit, module} do
        {nil, nil} ->
          :cprof.analyse()

        {limit, nil} ->
          :cprof.analyse(limit)

        {limit, module} ->
          if limit do
            :cprof.analyse(module, limit)
          else
            :cprof.analyse(module)
          end
      end

    {num_matched_functions, analysis_result}
  end

  defp string_to_existing_module(":" <> module), do: String.to_existing_atom(module)
  defp string_to_existing_module(module), do: Module.concat([module])

  defp print_output({num_matched_functions, {all_call_count, mod_analysis_list}}) do
    print_total_row(all_call_count)
    Enum.each(mod_analysis_list, &print_analysis_result/1)
    print_number_of_matched_functions(num_matched_functions)
  end

  defp print_output({num_matched_functions, {_mod, _call_count, _mod_fun_list} = mod_analysis}) do
    print_analysis_result(mod_analysis)
    print_number_of_matched_functions(num_matched_functions)
  end

  defp print_number_of_matched_functions(num_matched_functions) do
    IO.puts("Profile done over #{num_matched_functions} matching functions")
  end

  defp print_total_row(all_call_count) do
    IO.puts("")
    print_row(["s", "s", "s"], ["", "CNT", ""])
    print_row(["s", "B", "s"], ["Total", all_call_count, ""])
  end

  defp print_analysis_result({module, total_module_count, module_fun_list}) do
    module
    |> Atom.to_string()
    |> module_name_for_printing()
    |> print_module(total_module_count, "", "<--")

    Enum.each(module_fun_list, &print_function(&1, "  "))
  end

  defp print_module(module, count, prefix, suffix) do
    print_row(["s", "B", "s"], ["#{prefix}#{module}", count, suffix])
  end

  defp module_name_for_printing("Elixir." <> rest = _module_name), do: rest
  defp module_name_for_printing(module_name), do: ":" <> module_name

  defp print_function({fun, count}, prefix, suffix \\ "") do
    print_row(["s", "B", "s"], ["#{prefix}#{function_text(fun)}", count, suffix])
  end

  defp function_text({module, function, arity}) do
    Exception.format_mfa(module, function, arity)
  end

  defp function_text(other), do: inspect(other)

  @columns [-60, 12, 5]
  defp print_row(formats, data) do
    Stream.zip(@columns, formats)
    |> Stream.map(fn {width, format} -> "~#{width}#{format}" end)
    |> Enum.join()
    |> :io.format(data)

    IO.puts("")
  end
end
