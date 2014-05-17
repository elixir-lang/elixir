defmodule IEx.Tracer do
  alias IEx.Tracer.Pattern
  alias IEx.Tracer.Server

  def trace(to_trace, options) do
    pattern = Pattern.compile(to_trace, options) |> Macro.escape(unquote: true)
    quote do
      IEx.Tracer.ensure_server_is_running
      Pattern.set(unquote(pattern))
    end
  end

  def trace_off() do
    send(Server, :exit_tracer)
  end

  def ensure_server_is_running(pid \\ Process.whereis(IEx.Tracer))

  def ensure_server_is_running(nil) do
    pid = :proc_lib.spawn_link(Server, :loop, [])
    Process.register(pid, IEx.Tracer)
    ensure_server_is_running(pid)
  end

  def ensure_server_is_running(pid) when is_pid(pid) do
    :erlang.trace(:all, true, [{:tracer, pid}, :call])
  end

  defmodule Pattern do

    def compile(pattern, options \\ []) do
      exported_opt = Enum.member?(options, :exported)
      {mfa, [{args, conditions, trace_options}]} = compile_intern(pattern)
      {
        mfa,
        [{args, conditions, set_trace_options(options, trace_options)}],
        set_transform_opions(exported_opt)
      }
    end

    defp compile_intern({:when, _, [{{:., _, [module, function]}, _, args}, conditions]}) do
      {args, map} = transform_arguments(args)
      conditions = [transform_conditions(conditions, map)]
      {mfa(module, function), [match_spec(args, conditions)]}
    end

    defp compile_intern({:/, _, [{{:., _, [module, function]}, _, []}, arity]}) do
      {mfa(module, function, arity), [match_spec(:_)]}
    end

    defp compile_intern({{:., _, [module, function]}, _, args}) do
      {mfa(module, function), [match_spec(args |> transform_arguments |> elem(0))]}
    end

    defp compile_intern(module_name) do
      {mfa(module_name), [match_spec]}
    end

    def set({pattern, match_options, global_options}) do
      :erlang.trace_pattern(pattern, match_options, global_options)
    end

    defp module_name(module_ast), do: Macro.expand(module_ast, __ENV__)

    defp mfa(module, function \\ :_, arity \\ :_), do: {module_name(module), function, arity}

    defp match_spec(args \\ :_, conditions \\ [])
    defp match_spec([], conditions),   do: {:_, conditions, trace_options}
    defp match_spec(args, conditions), do: {args, conditions, trace_options}

    defp trace_options, do: [{:return_trace}, {:exception_trace}]

    defp set_transform_opions(true),  do: []
    defp set_transform_opions(false), do: [:local]

    defp set_trace_options([], args), do: args

    defp set_trace_options([:stack | options], args) do
      set_trace_options(options, [{:message, {:process_dump}} | args])
    end

    defp set_trace_options([:no_return | options], args) do
      set_trace_options(options, args -- [{:exception_trace}, {:return_trace}])
    end

    defp set_trace_options([_ | options], args) do
      set_trace_options(options, args)
    end

    defp transform_arguments(args, map \\ %{count: 1}, action \\ :save)

    defp transform_arguments(args, map, action) when is_list(args) do
      {new_args, new_map} = Enum.reduce(args, {[], map}, fn(arg, {acc, map}) ->
        {arg, new_map} = transform_arguments(arg, map, action)
        {[arg | acc], new_map}
      end)
      {Enum.reverse(new_args), new_map}
    end

    defp transform_arguments({type, _, args}, map, action) when type in [:'{}', :'<<>>', :'%{}'] do
      {args, new_map} = transform_arguments(args, map, action)
      {(transform_back_fun(type)).(args), new_map}
    end

    defp transform_arguments({key, value}, map, action) do
      {[key, value], new_map} = transform_arguments([key, value], map, action)
      {{key, value}, new_map}
    end

    defp transform_arguments({:__aliases__, _, _} = alias_ast, map, _) do
      {Macro.expand(alias_ast, __ENV__), map}
    end

    defp transform_arguments({atom, _, _}, %{count: count} = map, :save) do
      {count, new_map} =
        case Map.fetch(map, atom) do
          {:ok, exists_id} ->
            {exists_id, map}
          :error ->
            {count, Map.merge(map, [{:count, count + 1}, {atom, count}])}
        end
      {("$#{count}" |> binary_to_atom), new_map}
    end

    defp transform_arguments({atom, _, _} = var, map, :restore) do
      value = case Map.fetch(map, atom) do
        {:ok, value} ->
          "$#{value}" |> binary_to_atom
        :error ->
          {:unquote, [], [quote do: unquote(var)]}
      end
      {value, map}
    end

    defp transform_arguments(arg, map, _action) when is_atom(arg) or
                                            is_number(arg) or
                                            is_binary(arg) do
      {arg, map}
    end

    defp transform_back_fun(:'{}'),   do: &list_to_tuple/1
    defp transform_back_fun(:'<<>>'), do: &list_to_bitstring/1
    defp transform_back_fun(:'%{}'),  do: &:maps.from_list/1

    @function [ :is_atom, :is_float, :is_integer, :is_list, :is_number, :is_map,
                :is_pid, :is_port, :is_reference, :is_tuple, :is_binary, :is_boolean,
                :abs, :hd, :length, :round, :tl, :trunc, :not ]
    defp transform_conditions({name, _, [var]}, map) when name in @function do
      {name, transform_conditions(var, map)}
    end

    @operators [{:and, :andalso}, {:or, :orelse}, :xor, :>, :>=, :<, {:<=, :'=<'}, :==,
                {:===, :'=:='}, {:!=, :'/='}, {:!==, :'=/='}, :+, :-, :*, {:/, :div},
                :rem ]
    Enum.map(@operators,
      fn({elixir_op, erlang_op}) ->
          defp transform_conditions({unquote(elixir_op), _, [left, right]}, map) do
            {unquote(erlang_op), transform_conditions(left, map), transform_conditions(right, map)}
          end
        (op) ->
          defp transform_conditions({unquote(op), _, [left, right]}, map) do
            {unquote(op), transform_conditions(left, map), transform_conditions(right, map)}
          end
      end)

    defp transform_conditions({ :elem, _, [var, index]}, map) do
      { :element, index + 1, transform_conditions(var, map) }
    end

    @function [ :node, :self ]
    defp transform_conditions({ name, _, [] }, _) when name in @function do
      { name }
    end

    defp transform_conditions(data, map) do
      transform_arguments(data, map, :restore) |> elem(0)
    end

    # Not supported at the moment: is_record/{1,2}
  end

  defmodule Server do
    def loop() do
      receive do
        msg when elem(msg, 0) == :trace ->
          print_trace(msg)
          loop()
        :exit_tracer ->
          IO.puts("exit tracer")
      end
    end

    defp print_trace({:trace, pid, :call, mfa}) do
      IO.puts("#{inspect pid} call #{call_mfa(mfa)}")
    end
    defp print_trace({:trace, pid, :call, mfa, dump}) do
      traces = String.split(dump, "\n")
        |> Enum.filter( &Regex.match?(~r/Return addr 0x|CP: 0x/, &1) )
        |> fold_over
        |> Enum.reverse
      IO.puts("#{inspect pid} call #{call_mfa(mfa)}#{traces}")
    end
    defp print_trace({:trace, pid, :return_from, mfa, return}) do
      IO.puts("#{inspect pid} returned #{return_mfa(mfa)}#{inspect return}")
    end
    defp print_trace({:trace, pid, :exception_from, mfa, {class, value}}) do
      IO.puts("#{inspect pid} exception #{return_mfa(mfa)}#{inspect class}:#{inspect value}")
    end
    defp print_trace(msg) do
      IO.puts("unknown message: #{inspect msg}")
    end

    defp call_mfa({module, function, arguments}) do
      "#{inspect module}.#{function}(" <> Enum.map_join(arguments, ", ", &inspect(&1)) <> ")"
    end

    defp return_mfa({module, function, argument}) do
      "#{inspect module}.#{function}/#{argument} -> "
    end

    defp fold_over(list, acc \\ [])

    defp fold_over([_last], acc), do: acc
    defp fold_over([one | tail], acc) do
      fold_over(tail, [extract_function(one) | acc])
    end

    defp extract_function(line) do
      case Regex.run(~r"^.+\((.+):(.+)/(\d+).+\)$", line, capture: :all_but_first) do
        [m, f, a_length] ->
          "\n  #{m |> clean_atom_binary}.#{f |> clean_atom_binary}/#{a_length}"
        nil ->
          ""
      end
    end

    defp clean_atom_binary(binatom), do: String.strip(binatom, ?') |> clean_elixir |> binary_to_atom

    defp clean_elixir("Elixir." <> binatom), do: binatom
    defp clean_elixir(binatom), do: binatom

  end

end
