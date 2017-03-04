defmodule Mix.Tasks.Profile.Cprof do
  use Mix.Task

  @shortdoc "Profiles the given file or expression with cprof"

  @moduledoc """
  Profiles the given file or expression using Erlang's 'cprof' tool.

  'cprof' can be useful when you want to discover the bottlenecks related
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

  ## Command line options

    * `--from-mfa` - a {module, function, arity} expression or 'on_load' to set a trace pattern to count from
    * `--limit`, - filters out any results with a call count less than the limit
    * `--module`, - filters out any results not pertaining to the given module
    * `--config`, `-c`  - loads the given configuration file
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

  When `--from-mfa` option is specified, call count tracing will be started only for 
  the functions matching the given MFA:

      String.Chars.Integer                                                   3  <--
        String.Chars.Integer.to_string/1                                     3
      Profile done over 1 matching functions

  In the example above the MFA `"{String.Chars.Integer, :to_string, :_}"`
  was given, therefore only one function was matched. The MFA must be a tuple
  with `module`, `function` and `arity` (by this order). However, notice that `function` and 
  `arity` may be arbitrary by passing the `:_` atom. Erlang modules have to be passed as 
  atoms, similarly to calling Erlang modules in code (e.g., `:ets`, `:inet`, `:math`). Elixir
  modules can be passed with their expanded name (e.g., `:'Elixir.Enum'`) or their shortened
  name (e.g., `Enum`, `Elixir.Enum`). Some examples are granted below:

      ```
      --from-mfa "{:erlang, :_, :_}" # Matching functions will be all functions in :erlang module

      --from-mfa "{:erlang, :system_monitor, :_}" # Matching function will be all function heads of :erlang.system_monitor/0..2

      --from-mfa "{Enum, :reverse, 1}" # The only matching function will be Enum.reverse/1
      ```

  Another possible value for the `--from-mfa` option is `"on_load" to match all modules and 
  functions which are newly loaded. 

  For a detailed explanation it's worth reading the start function in 
  [Erlang documentation for cprof](http://www.erlang.org/doc/man/cprof.html#start).

  ## Caveats

  You should be aware the profiler is stopped as soon as the code has finished running. This
  may need special attention, when:  running asynchronous code as function calls which were 
  called before the profiler stopped will not be counted; running synchronous code as long 
  running computations and a profiler without a proper MFA trace pattern or filter may 
  lead to a result set which is difficult to comprehend.

  Other caveats are the impossibility to call count trace BIFs, since breakpoints can 
  only be set on BEAM code; functions calls performed by `:cprof` are not traced; the 
  maximum size of a call counter is equal to the host machine's word size 
  (e.g, 2147483647 in a 32-bit host).
  """

  @switches [parallel: :boolean, require: :keep, eval: :keep, config: :keep, from_mfa: :string,
             halt: :boolean, compile: :boolean, deps_check: :boolean, limit: :integer, 
             module: :string, start: :boolean, archives_check: :boolean, warmup: :boolean, 
             elixir_version_check: :boolean, parallel_require: :keep]

  @spec run(OptionParser.argv) :: :ok
  def run(args) do
    {opts, head} = OptionParser.parse_head!(args,
      aliases: [r: :require, p: :parallel, e: :eval, c: :config],
      strict: @switches)

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
    unless "--no-start" in args do
      Mix.Task.run "app.start", args
    end
    process_load opts

    _ = if file do
      if File.regular?(file) do
        profile_code(File.read!(file), opts)
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
        profile_code(value, opts)
      _ ->
        :ok
    end
  end

  defp filter_patterns(pattern) do
    Path.wildcard(pattern)
    |> Enum.uniq
    |> Enum.filter(&File.regular?/1)
  end

  # Profiling functions

  defp profile_code(code_string, opts) do
    content = 
      quote do
        unquote(__MODULE__).profile(fn ->
          unquote(Code.string_to_quoted!(code_string))
        end, unquote(opts))
      end
    # Use compile_quoted since it leaves less noise than eval_quoted
    Code.compile_quoted(content)
  end

  @doc false
  def profile(fun, opts) do
    fun
    |> profile_and_analyse(opts)
    |> print_output

    :cprof.stop()
  end

  defp profile_and_analyse(fun, opts) do
    if Keyword.get(opts, :warmup, true) do
      IO.puts "Warmup..."
      fun.()
    end

    num_matched_functions = case Keyword.get(opts, :from_mfa) do
      nil -> :cprof.start()
      func_spec ->
        case parse_mfa(func_spec) do
          :on_load -> :cprof.start({:on_load})
          {module} -> :cprof.start(module)
          {module, function} -> :cprof.start(module, function)
          {module, function, arity} -> :cprof.start(module, function, arity)
        end
      end

    apply(fun, [])

    :cprof.pause() 

    limit  = Keyword.get(opts, :limit) 
    module = Keyword.get(opts, :module)

    analysis_result = case {limit, module} do
      {nil, nil} -> :cprof.analyse()
      {limit, nil} -> :cprof.analyse(limit)
      {limit, module} -> 
        module_atom = string_to_existing_module_atom(module)
        case limit do
          nil -> :cprof.analyse(module_atom)
          _   -> :cprof.analyse(module_atom, limit)
        end
    end

    {num_matched_functions, analysis_result}
  end

  defp string_to_existing_module_atom(":" <> module), do: String.to_existing_atom(module)
  defp string_to_existing_module_atom(module), do: Module.concat([module])

  defp parse_mfa("on_load"), do: :on_load
  defp parse_mfa(mfa), do: mfa |> Code.eval_string |> validate_mfa

  defp validate_mfa({{m, :_, :_}, _}) when is_atom(m), do: {m}
  defp validate_mfa({{m, f, :_}, _}) when is_atom(m) and is_atom(f), do: {m, f}
  defp validate_mfa({{m, f, a}, _}) when is_atom(m) and is_atom(f) and is_integer(a), do: {m, f, a}
  defp validate_mfa({fs, _}), do: Mix.raise "Invalid MFA: #{inspect fs} (should be {module, function, arity} or on_load)"

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
    IO.puts "Profile done over #{num_matched_functions} matching functions"
  end
  
  defp print_total_row(all_call_count) do
    IO.puts ""
    print_row(["s", "s", "s"], ["", "CNT", ""])
    print_row(["s", "B", "s"], ["Total", all_call_count, ""])
  end

  defp print_analysis_result({module, total_module_count, module_fun_list}) do
    module = module_name_for_printing(module)
    print_module(module, total_module_count, "", "<--")
    Enum.each(module_fun_list, &print_function(&1, "  "))
  end

  defp print_module(module, count, prefix \\ "", suffix \\ "") do
    print_row(
      ["s", "B", "s"],
      ["#{prefix}#{module}", count, suffix]
    )
  end

  defp module_name_for_printing(module), do: module |> Atom.to_string |> do_module_name_for_printing

  defp do_module_name_for_printing("Elixir." <> rem = _module_name), do: rem
  defp do_module_name_for_printing(module_name), do: ":" <> module_name

  defp print_function({fun, count}, prefix \\ "", suffix \\ "") do
    print_row(
      ["s", "B", "s"],
      ["#{prefix}#{function_text(fun)}", count, suffix]
    )
  end

  defp function_text({module, function, arity}) do
    Exception.format_mfa(module, function, arity)
  end

  defp function_text(other), do: inspect(other)

  @columns [-60, 12, 5]
  defp print_row(formats, data) do
    Stream.zip(@columns, formats)
    |> Stream.map(fn({width, format}) -> "~#{width}#{format}" end)
    |> Enum.join
    |> :io.format(data)

    IO.puts ""
  end
end
