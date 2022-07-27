defmodule IEx.Pry do
  @moduledoc """
  The low-level API for prying sessions and setting up breakpoints.
  """

  @doc false
  use GenServer

  @table __MODULE__
  @server __MODULE__
  @timeout :infinity
  @initial_counter 1

  @type id :: integer()
  @type break :: {id, module, {function, arity}, pending :: non_neg_integer}
  @type break_error ::
          :recompilation_failed
          | :no_beam_file
          | :unknown_function_arity
          | :missing_debug_info
          | :outdated_debug_info
          | :non_elixir_module

  @doc """
  Callback for `IEx.pry/0`.

  You can invoke this function directly when you are not able to invoke
  `IEx.pry/0` as a macro. This function expects the binding (from
  `binding/0`) and the environment (from `__ENV__/0`).
  """
  def pry(binding, %Macro.Env{} = env) do
    self = self()
    %{file: file, line: line, module: module, function: function_arity} = env
    {:current_stacktrace, stacktrace} = Process.info(self, :current_stacktrace)

    opts = [
      binding: binding,
      dot_iex_path: "",
      # Remove all tracers because the tracer code is most
      # likely stale by the time we are prying the code.
      env: %{env | tracers: [], lexical_tracker: nil},
      prefix: "pry",
      stacktrace: prune_stacktrace(stacktrace)
    ]

    location =
      case function_arity do
        {function, arity} ->
          "#{Exception.format_mfa(module, function, arity)} (#{Path.relative_to_cwd(file)}:#{line})"

        _ ->
          "#{Path.relative_to_cwd(file)}:#{line}"
      end

    whereami =
      case whereami(file, line, 3) do
        {:ok, lines} -> [?\n, ?\n, lines]
        :error -> []
      end

    # We cannot use colors because IEx may be off
    case IEx.Broker.take_over(location, whereami, [evaluator: self()] ++ opts) do
      {:ok, server, group_leader} ->
        IEx.Evaluator.init(:no_ack, server, group_leader, opts)

      {:error, :no_iex} ->
        extra =
          if match?({:win32, _}, :os.type()) do
            " If you are using Windows, you may need to start IEx with the --werl option."
          else
            ""
          end

        message = "Cannot pry #{inspect(self)} at #{location}. Is an IEx shell running?" <> extra
        IO.puts(:stdio, message)
        {:error, :no_iex}

      {:error, _} = error ->
        error
    end
  end

  def pry(binding, opts) when is_list(opts) do
    vars = for {k, _} when is_atom(k) <- binding, do: {k, nil}
    pry(binding, opts |> :elixir.env_for_eval() |> :elixir_env.with_vars(vars))
  end

  @doc false
  def pry_with_next(next?, binding, opts_or_env) when is_boolean(next?) do
    next? and pry(binding, opts_or_env) == {:ok, true}
  end

  @elixir_internals [:elixir, :erl_eval, IEx.Evaluator, IEx.Pry]

  defp prune_stacktrace([{mod, _, _, _} | t]) when mod in @elixir_internals do
    prune_stacktrace(t)
  end

  defp prune_stacktrace([{Process, :info, 2, _} | t]) do
    prune_stacktrace(t)
  end

  defp prune_stacktrace([h | t]) do
    [h | prune_stacktrace(t)]
  end

  defp prune_stacktrace([]) do
    []
  end

  @doc """
  Formats the location for `whereami/3` prying.

  It receives the `file`, `line` and the snippet `radius` and
  returns `{:ok, lines}`, where lines is a list of chardata
  containing each formatted line, or `:error`.

  The actual line is especially formatted in bold.
  """
  def whereami(file, line, radius)
      when is_binary(file) and is_integer(line) and is_integer(radius) and radius > 0 do
    with true <- File.regular?(file),
         [_ | _] = lines <- whereami_lines(file, line, radius) do
      {:ok, lines}
    else
      _ -> :error
    end
  end

  defp whereami_lines(file, line, radius) do
    min = max(line - radius - 1, 0)
    max = line + radius - 1

    file
    |> File.stream!()
    |> Enum.slice(min..max)
    |> Enum.with_index(min + 1)
    |> Enum.map(&whereami_format_line(&1, line))
  end

  defp whereami_format_line({line_text, line_number}, line) do
    gutter = String.pad_leading(Integer.to_string(line_number), 5, " ")

    if line_number == line do
      IO.ANSI.format_fragment([:bright, gutter, ": ", line_text, :normal])
    else
      [gutter, ": ", line_text]
    end
  end

  @doc """
  Sets up a breakpoint on the given module/function/arity.
  """
  @spec break(module, atom, arity, non_neg_integer) :: {:ok, id()} | {:error, break_error()}
  def break(module, function, arity, breaks \\ 1)
      when is_atom(module) and is_atom(function) and arity in 0..255 and
             is_integer(breaks) and breaks >= 0 do
    break_call(module, function, arity, quote(do: _), breaks)
  end

  @doc """
  Sets up a breakpoint on the given module/function/args with the given `guard`.

  It requires an `env` to be given to make the expansion of the guards.
  """
  @spec break(module, atom, [Macro.t()], Macro.t(), Macro.Env.t(), non_neg_integer) ::
          {:ok, id()} | {:error, break_error()}
  def break(module, function, args, guard, env, breaks \\ 1)
      when is_atom(module) and is_atom(function) and is_list(args) and is_integer(breaks) and
             breaks >= 0 do
    condition = build_args_guard_condition(args, guard, env)
    break_call(module, function, length(args), condition, breaks)
  end

  defp break_call(module, function, arity, condition, breaks) do
    GenServer.call(@server, {:break, module, {function, arity}, condition, breaks}, @timeout)
  end

  @doc """
  Raising variant of `break/4`.
  """
  @spec break!(module, atom, arity, non_neg_integer) :: id()
  def break!(module, function, arity, breaks \\ 1) do
    break_call!(module, function, arity, quote(do: _), breaks)
  end

  @doc """
  Raising variant of `break/6`.
  """
  @spec break!(module, atom, [Macro.t()], Macro.t(), Macro.Env.t(), non_neg_integer) :: id()
  def break!(module, function, args, guard, env, breaks \\ 1)
      when is_atom(module) and is_atom(function) and is_list(args) and is_integer(breaks) and
             breaks >= 0 do
    condition = build_args_guard_condition(args, guard, env)
    break_call!(module, function, length(args), condition, breaks)
  end

  defp break_call!(module, function, arity, condition, breaks) do
    case break_call(module, function, arity, condition, breaks) do
      {:ok, id} ->
        id

      {:error, kind} ->
        message =
          case kind do
            :missing_debug_info ->
              "module #{inspect(module)} was not compiled with debug_info"

            :no_beam_file ->
              "could not find .beam file for #{inspect(module)}"

            :non_elixir_module ->
              "module #{inspect(module)} was not written in Elixir"

            :outdated_debug_info ->
              "module #{inspect(module)} was not compiled with the latest debug_info"

            :recompilation_failed ->
              "the module could not be compiled with breakpoints (likely an internal error)"

            :unknown_function_arity ->
              "unknown function/macro #{Exception.format_mfa(module, function, arity)}"
          end

        raise "could not set breakpoint, " <> message
    end
  end

  defp build_args_guard_condition(args, guards, env) do
    pattern = {:when, [], [{:{}, [], args}, guards]}

    to_expand =
      quote do
        case Unknown.module() do
          unquote(pattern) -> :ok
        end
      end

    {{:case, _, [_, [do: [{:->, [], [[condition], _]}]]]}, _, _} =
      :elixir_expand.expand(to_expand, :elixir_env.env_to_ex(env), env)

    condition
  end

  @doc """
  Resets the breaks on a given breakpoint ID.
  """
  @spec reset_break(id) :: :ok | :not_found
  def reset_break(id) when is_integer(id) do
    GenServer.call(@server, {:reset_break, {id, :_, :_, :_, :_}}, @timeout)
  end

  @doc """
  Resets the breaks for the given `module`, `function` and `arity`.

  If the `module` is not instrumented or if the given `function`
  does not have a breakpoint, it is a no-op and it returns
  `:not_found`. Otherwise it returns `:ok`.
  """
  @spec reset_break(module, atom, arity) :: :ok | :not_found
  def reset_break(module, function, arity) do
    GenServer.call(@server, {:reset_break, {:_, module, {function, arity}, :_, :_}}, @timeout)
  end

  @doc """
  Removes all breakpoints on all modules.

  This effectively loads the non-instrumented version of
  currently instrumented modules into memory.
  """
  @spec remove_breaks :: :ok
  def remove_breaks do
    GenServer.call(@server, :remove_breaks, @timeout)
  end

  @doc """
  Removes breakpoints in the given module.

  This effectively loads the non-instrumented version of
  the module into memory.
  """
  @spec remove_breaks(module) :: :ok | {:error, :no_beam_file}
  def remove_breaks(module) do
    GenServer.call(@server, {:remove_breaks, module}, @timeout)
  end

  @doc """
  Returns all breakpoints.
  """
  @spec breaks :: [break]
  def breaks do
    @server
    |> GenServer.call(:breaks, @timeout)
    |> Enum.sort()
  end

  ## Callbacks

  @doc false
  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: @server)
  end

  @impl true
  def init(:ok) do
    Process.flag(:trap_exit, true)
    :ets.new(@table, [:named_table, :public, write_concurrency: true])
    {:ok, @initial_counter}
  end

  @impl true
  def handle_call({:break, module, fa, condition, breaks}, _from, counter) do
    # If there is a match for the given module and fa, we
    # use the ref, otherwise we create a new one.
    {ref, counter} =
      case :ets.match_object(@table, {:_, module, fa, :_, :_}) do
        [{ref, _, _, _, _}] -> {ref, counter}
        [] -> {counter, counter + 1}
      end

    case fetch_elixir_debug_info_with_fa_check(module, fa) do
      {:ok, beam, backend, elixir} ->
        true = :ets.insert(@table, {ref, module, fa, condition, breaks})
        entries = :ets.match_object(@table, {:_, module, :_, :_, :_})
        {:reply, instrument(beam, backend, elixir, ref, entries), counter}

      {:error, _} = error ->
        {:reply, error, counter}
    end
  end

  def handle_call({:reset_break, pattern}, _from, counter) do
    reset =
      for {ref, module, fa, condition, _} <- :ets.match_object(@table, pattern) do
        if instrumented?(module) do
          :ets.insert(@table, {ref, module, fa, condition, 0})
          true
        else
          :ets.delete(@table, ref)
          false
        end
      end

    if Enum.any?(reset) do
      {:reply, :ok, counter}
    else
      {:reply, :not_found, counter}
    end
  end

  def handle_call(:breaks, _from, counter) do
    entries =
      for {id, module, function_arity, _condition, breaks} <- :ets.tab2list(@table),
          keep_instrumented(id, module) == :ok do
        {id, module, function_arity, max(breaks, 0)}
      end

    {:reply, entries, counter}
  end

  def handle_call(:remove_breaks, _from, _counter) do
    # Make sure to deinstrument before clearing
    # up the table to avoid race conditions.
    @table
    |> :ets.match({:_, :"$1", :_, :_, :_})
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.each(&deinstrument_if_instrumented/1)

    true = :ets.delete_all_objects(@table)
    {:reply, :ok, @initial_counter}
  end

  def handle_call({:remove_breaks, module}, _from, counter) do
    # Make sure to deinstrument before clearing
    # up the table to avoid race conditions.
    reply = deinstrument_if_instrumented(module)
    true = :ets.match_delete(@table, {:_, module, :_, :_, :_})
    {:reply, reply, counter}
  end

  defp keep_instrumented(id, module) do
    if instrumented?(module) do
      :ok
    else
      :ets.delete(@table, id)
      :error
    end
  end

  defp deinstrument_if_instrumented(module) do
    if instrumented?(module) do
      deinstrument(module)
    else
      :ok
    end
  end

  defp deinstrument(module) do
    with [_ | _] = beam <- :code.which(module),
         {:ok, binary} <- File.read(beam) do
      :code.purge(module)
      {:module, _} = :code.load_binary(module, beam, binary)
      :ok
    else
      _ -> {:error, :no_beam_file}
    end
  end

  defp fetch_elixir_debug_info_with_fa_check(module, fa) do
    case :code.which(module) do
      [_ | _] = beam ->
        case :beam_lib.chunks(beam, [:debug_info]) do
          {:ok, {_, [debug_info: {:debug_info_v1, backend, {:elixir_v1, map, _} = elixir}]}} ->
            case List.keyfind(map.definitions, fa, 0) do
              {_, _, _, _} -> {:ok, beam, backend, elixir}
              nil -> {:error, :unknown_function_arity}
            end

          {:ok, {_, [debug_info: {:debug_info_v1, _, _}]}} ->
            {:error, :non_elixir_module}

          {:error, :beam_lib, {:missing_chunk, _, _}} ->
            {:error, :missing_debug_info}

          _ ->
            {:error, :outdated_debug_info}
        end

      _ ->
        {:error, :no_beam_file}
    end
  end

  defp instrument(beam, backend, {:elixir_v1, map, specs}, counter, entries) do
    %{attributes: attributes, definitions: definitions, module: module} = map

    attributes = [{:iex_pry, true} | attributes]
    definitions = Enum.map(definitions, &instrument_definition(&1, map, entries))
    map = %{map | attributes: attributes, definitions: definitions}

    with {:ok, forms} <- backend.debug_info(:erlang_v1, module, {:elixir_v1, map, specs}, []),
         {:ok, _, binary, _} <- :compile.noenv_forms(forms, [:return | map.compile_opts]) do
      :code.purge(module)
      {:module, _} = :code.load_binary(module, beam, binary)
      {:ok, counter}
    else
      _error ->
        {:error, :recompilation_failed}
    end
  end

  defp instrument_definition({fa, kind, meta, clauses} = definition, map, entries) do
    case List.keyfind(entries, fa, 2) do
      {ref, _, ^fa, condition, _} ->
        %{module: module, file: file} = map

        file =
          case meta[:location] do
            {file, _} -> file
            _ -> file
          end

        opts = [module: module, file: file, function: fa]
        clauses = Enum.map(clauses, &instrument_clause(&1, ref, condition, opts))
        {fa, kind, meta, clauses}

      nil ->
        definition
    end
  end

  defp instrument_clause({meta, args, guards, clause}, ref, case_pattern, opts) do
    arity = length(args)
    exprs = unwrap_block(clause)

    # Have an extra binding per argument for case matching.
    case_vars =
      for id <- 1..arity//1 do
        {String.to_atom("arg" <> Integer.to_string(id)), [version: -id], __MODULE__}
      end

    case_head = {:{}, [], case_vars}
    update_op = Macro.escape({5, -1, -1, -1})

    # Generate the take_over condition with the ETS lookup.
    # Remember this is expanded AST, so no aliases allowed,
    # no locals (such as the unary -) and so on.
    initialize_next =
      quote do
        unquote(next_var(arity + 1)) =
          case unquote(case_head) do
            unquote(case_pattern) ->
              :erlang."/="(
                # :ets.update_counter(table, key, {pos, inc, threshold, reset})
                :ets.update_counter(unquote(@table), unquote(ref), unquote(update_op)),
                unquote(-1)
              )

            _ ->
              false
          end
      end

    args =
      case_vars
      |> Enum.zip(args)
      |> Enum.map(fn {var, arg} -> {:=, [], [arg, var]} end)

    # The variable we pass around will start after the arity,
    # as we use the arity to instrument the clause.
    binding = match_binding(args, %{})
    line = Keyword.get(meta, :line, 1)
    exprs = instrument_body(exprs, true, line, arity + 1, binding, opts)

    {meta, args, guards, {:__block__, meta, [initialize_next | exprs]}}
  end

  defp instrument_body([expr | exprs], force?, line, version, binding, opts) do
    next_binding = binding(expr, binding)
    {min_line, max_line} = line_range(expr, line)

    if force? or (min_line > line and min_line != :infinity) do
      pry_var = next_var(version)
      pry_binding = Map.to_list(binding)
      pry_opts = [line: min_line] ++ opts

      pry =
        quote do
          unquote(next_var(version + 1)) =
            :"Elixir.IEx.Pry".pry_with_next(
              unquote(pry_var),
              unquote(pry_binding),
              unquote(pry_opts)
            )
        end

      [pry, expr | instrument_body(exprs, false, max_line, version + 1, next_binding, opts)]
    else
      [expr | instrument_body(exprs, false, max_line, version, next_binding, opts)]
    end
  end

  defp instrument_body([], _force?, _line, _version, _binding, _opts) do
    []
  end

  defp line_range(ast, line) do
    {_, min_max} =
      Macro.prewalk(ast, {:infinity, line}, fn
        {_, meta, _} = ast, {min_line, max_line} when is_list(meta) ->
          line = meta[:line]

          if line > 0 do
            {ast, {min(line, min_line), max(line, max_line)}}
          else
            {ast, {min_line, max_line}}
          end

        ast, acc ->
          {ast, acc}
      end)

    min_max
  end

  defp binding(ast, binding) do
    {_, binding} =
      Macro.prewalk(ast, binding, fn
        {:=, _, [left, _right]}, acc ->
          {:ok, match_binding(left, acc)}

        {:case, _, [arg, _block]}, acc ->
          {arg, acc}

        {special_form, _, _}, acc
        when special_form in [:cond, :fn, :for, :receive, :try, :with] ->
          {:ok, acc}

        expr, acc ->
          {expr, acc}
      end)

    binding
  end

  defp match_binding(match, binding) do
    {_, binding} =
      Macro.prewalk(match, binding, fn
        {name, _, nil} = var, acc when name != :_ and is_atom(name) ->
          {var, Map.put(acc, name, var)}

        # Stop traversing inside pin operators and :: in binaries
        {special_form, _, _}, acc when special_form in [:^, :"::"] ->
          {:ok, acc}

        expr, acc ->
          {expr, acc}
      end)

    binding
  end

  defp next_var(id) do
    {:next?, [version: -id], __MODULE__}
  end

  defp instrumented?(module) do
    module.__info__(:attributes)[:iex_pry] == [true]
  end

  defp unwrap_block(expr), do: expr |> unwrap_block([]) |> Enum.reverse()
  defp unwrap_block({:__block__, _, exprs}, acc), do: Enum.reduce(exprs, acc, &unwrap_block/2)
  defp unwrap_block(expr, acc), do: [expr | acc]

  # IEx backend for Kernel.dbg/2.
  @doc false
  def dbg(ast, options, env)

  def dbg({:|>, _meta, _args} = ast, options, %Macro.Env{} = env) when is_list(options) do
    [first_ast_chunk | asts_chunks] = ast |> Macro.unpipe() |> chunk_pipeline_asts_by_line(env)

    initial_acc = [
      quote do
        env = __ENV__
        options = unquote(options)

        options =
          if IO.ANSI.enabled?() do
            Keyword.put_new(options, :syntax_colors, IO.ANSI.syntax_colors())
          else
            options
          end

        env = unquote(env_with_line_from_asts(first_ast_chunk))

        next? = IEx.Pry.pry_with_next(true, binding(), env)
        value = unquote(pipe_chunk_of_asts(first_ast_chunk))

        IEx.Pry.__dbg_pipe_step__(
          value,
          unquote(asts_chunk_to_strings(first_ast_chunk)),
          _start_with_pipe? = false,
          options
        )
      end
    ]

    for asts_chunk <- asts_chunks, reduce: initial_acc do
      ast_acc ->
        piped_asts = pipe_chunk_of_asts([{quote(do: value), _index = 0}] ++ asts_chunk)

        quote do
          unquote(ast_acc)
          env = unquote(env_with_line_from_asts(asts_chunk))
          next? = IEx.Pry.pry_with_next(next?, binding(), env)
          value = unquote(piped_asts)

          IEx.Pry.__dbg_pipe_step__(
            value,
            unquote(asts_chunk_to_strings(asts_chunk)),
            _start_with_pipe? = true,
            options
          )
        end
    end
  end

  def dbg(ast, options, %Macro.Env{} = env) when is_list(options) do
    options = quote(do: Keyword.put(unquote(options), :print_location, false))

    quote do
      IEx.Pry.pry(binding(), __ENV__)
      unquote(Macro.dbg(ast, options, env))
    end
  end

  # Made public to be called from IEx.Pry.dbg/3 to reduce the amount of generated code.
  @doc false
  def __dbg_pipe_step__(value, string_asts, start_with_pipe?, options) do
    asts_string = Enum.intersperse(string_asts, [:faint, " |> ", :reset])

    asts_string =
      if start_with_pipe? do
        IO.ANSI.format([:faint, "|> ", :reset, asts_string])
      else
        asts_string
      end

    [asts_string, :faint, " #=> ", :reset, inspect(value, options), "\n\n"]
    |> IO.ANSI.format()
    |> IO.write()
  end

  defp chunk_pipeline_asts_by_line(asts, %Macro.Env{line: env_line}) do
    Enum.chunk_by(asts, fn
      {{_fun_or_var, meta, _args}, _pipe_index} -> meta[:line] || env_line
      {_other_ast, _pipe_index} -> env_line
    end)
  end

  defp pipe_chunk_of_asts([{first_ast, _first_index} | asts] = _ast_chunk) do
    Enum.reduce(asts, first_ast, fn {ast, index}, acc -> Macro.pipe(acc, ast, index) end)
  end

  defp asts_chunk_to_strings(asts) do
    Enum.map(asts, fn {ast, _pipe_index} -> Macro.to_string(ast) end)
  end

  defp env_with_line_from_asts(asts) do
    line =
      Enum.find_value(asts, fn
        {{_fun_or_var, meta, _args}, _pipe_index} -> meta[:line]
        {_ast, _pipe_index} -> nil
      end)

    if line do
      quote do
        %{env | line: unquote(line)}
      end
    else
      quote do: env
    end
  end
end
