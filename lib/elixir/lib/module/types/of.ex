defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  alias Module.ParallelChecker
  import Module.Types.{Helpers, Descr}

  @prefix quote(do: ...)
  @suffix quote(do: ...)

  @integer_or_float union(integer(), float())
  @integer_or_binary union(integer(), binary())
  @integer integer()
  @float float()
  @binary binary()

  ## Variables

  @doc """
  Fetches the type of a defined variable.
  """
  def var({_name, meta, _context}, context) do
    version = Keyword.fetch!(meta, :version)
    %{vars: %{^version => %{type: type}}} = context
    type
  end

  @doc """
  Refines the type of a variable.
  """
  def refine_var({var_name, meta, var_context} = var, type, expr, stack, context) do
    version = Keyword.fetch!(meta, :version)

    case context.vars do
      %{^version => %{type: old_type, off_traces: off_traces} = data} ->
        new_type = intersection(type, old_type)
        data = %{data | type: new_type, off_traces: new_trace(expr, type, stack, off_traces)}
        context = put_in(context.vars[version], data)

        if empty?(new_type) do
          {:error,
           warn(__MODULE__, {:refine_var, old_type, type, var, context}, meta, stack, context)}
        else
          {:ok, new_type, context}
        end

      %{} ->
        data = %{
          # TODO: We should not default to dynamic on static mode
          type: dynamic(type),
          name: var_name,
          context: var_context,
          off_traces: new_trace(expr, type, stack, [])
        }

        context = put_in(context.vars[version], data)
        {:ok, type, context}
    end
  end

  defp new_trace(nil, _type, _stack, traces), do: traces
  defp new_trace(expr, type, stack, traces), do: [{expr, stack.file, type} | traces]

  ## Map/structs

  @doc """
  Handles fetching a map key.
  """
  def map_fetch(expr, type, field, stack, context) when is_atom(field) do
    case map_fetch(type, field) do
      :error ->
        {:ok, dynamic(),
         warn({:badmap, expr, type, field, context}, elem(expr, 1), stack, context)}

      # TODO: on static type checking, we want check it is not optional.
      {_optional?, value_type} ->
        if empty?(value_type) do
          {:ok, dynamic(),
           warn({:badkey, expr, type, field, context}, elem(expr, 1), stack, context)}
        else
          {:ok, value_type, context}
        end
    end
  end

  @doc """
  Builds a map.

  A tag may be given but it is not guaranteed.
  """
  def map(tag, pairs, stack, context, of_fun) do
    result =
      reduce_ok(pairs, {tag == :closed, [], [], context}, fn
        {key, value}, {closed?, single, multiple, context} ->
          with {:ok, key_type, context} <- of_fun.(key, stack, context),
               {:ok, value_type, context} <- of_fun.(value, stack, context) do
            case finite_keys(key, key_type) do
              :none ->
                {:ok, {false, single, multiple, context}}

              [key] when multiple == [] ->
                {:ok, {closed?, [{key, value_type} | single], multiple, context}}

              keys ->
                {:ok, {closed?, single, [{keys, value_type} | multiple], context}}
            end
          end
      end)

    with {:ok, {closed?, single, multiple, context}} <- result do
      map =
        case Enum.reverse(multiple) do
          [] ->
            pairs = Enum.reverse(single)
            if closed?, do: closed_map(pairs), else: open_map(pairs)

          [{keys, type} | tail] ->
            for key <- keys, t <- cartesian_map(tail) do
              pairs = Enum.reverse(single, [{key, type} | t])
              if closed?, do: closed_map(pairs), else: open_map(pairs)
            end
            |> Enum.reduce(&union/2)
        end

      {:ok, map, context}
    end
  end

  defp cartesian_map(lists) do
    case lists do
      [] ->
        [[]]

      [{keys, type} | tail] ->
        for key <- keys, t <- cartesian_map(tail), do: [{key, type} | t]
    end
  end

  defp finite_keys(key, _key_type) when is_atom(key), do: [key]

  defp finite_keys(_key, key_type) do
    case atom_fetch(key_type) do
      {:finite, list} -> list
      _ -> :none
    end
  end

  @doc """
  Handles structs.
  """
  def struct({:%, meta, _}, struct, args, defaults?, stack, context, of_fun)
      when is_atom(struct) do
    context = preload_and_maybe_check_export(struct, :__struct__, 0, meta, stack, context)

    # The compiler has already checked the keys are atoms and which ones are required.
    # TODO: Type check and do not assume dynamic for non-specified keys.
    with {:ok, pairs, context} <-
           map_reduce_ok(args, context, fn {key, value}, context when is_atom(key) ->
             with {:ok, type, context} <- of_fun.(value, stack, context) do
               {:ok, {key, type}, context}
             end
           end) do
      defaults =
        if defaults? do
          dynamic = dynamic()

          for key <- Map.keys(struct.__struct__()), key != :__struct__ do
            {key, dynamic}
          end
        else
          []
        end

      {:ok, closed_map([{:__struct__, atom([struct])} | defaults] ++ pairs), context}
    end
  end

  ## Binary

  @doc """
  Handles binaries.

  In the stack, we add nodes such as <<expr>>, <<..., expr>>, etc,
  based on the position of the expression within the binary.
  """
  def binary([], _kind, _stack, context, _of_fun) do
    {:ok, context}
  end

  def binary([head], kind, stack, context, of_fun) do
    binary_segment(head, kind, [head], stack, context, of_fun)
  end

  def binary([head | tail], kind, stack, context, of_fun) do
    case binary_segment(head, kind, [head, @suffix], stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, kind, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  defp binary_many([last], kind, stack, context, of_fun) do
    binary_segment(last, kind, [@prefix, last], stack, context, of_fun)
  end

  defp binary_many([head | tail], kind, stack, context, of_fun) do
    case binary_segment(head, kind, [@prefix, head, @suffix], stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, kind, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  # If the segment is a literal, the compiler has already checked its validity,
  # so we just skip it.
  defp binary_segment({:"::", _meta, [left, _right]}, _kind, _args, _stack, context, _of_fun)
       when is_binary(left) or is_number(left) do
    {:ok, context}
  end

  defp binary_segment({:"::", meta, [left, right]}, kind, args, stack, context, of_fun) do
    expected_type = specifier_info(kind, right)
    expr = {:<<>>, meta, args}

    with {:ok, actual_type, context} <- of_fun.(left, {expected_type, expr}, stack, context) do
      # If we are in a pattern and we have a variable, the refinement
      # will already have checked the type, so we skip the check here
      # as an optimization.
      if (kind == :pattern and is_var(left)) or compatible?(actual_type, expected_type) do
        {:ok, context}
      else
        hints = if meta[:inferred_bitstring_spec], do: [:inferred_bitstring_spec], else: []
        {:error, incompatible_warn(expr, expected_type, actual_type, hints, meta, stack, context)}
      end
    end
  end

  defp specifier_info(kind, {:-, _, [left, _right]}), do: specifier_info(kind, left)
  defp specifier_info(:expr, {:float, _, _}), do: @integer_or_float
  defp specifier_info(:expr, {:utf8, _, _}), do: @integer_or_binary
  defp specifier_info(:expr, {:utf16, _, _}), do: @integer_or_binary
  defp specifier_info(:expr, {:utf32, _, _}), do: @integer_or_binary
  defp specifier_info(:pattern, {:utf8, _, _}), do: @integer
  defp specifier_info(:pattern, {:utf16, _, _}), do: @integer
  defp specifier_info(:pattern, {:utf32, _, _}), do: @integer
  defp specifier_info(:pattern, {:float, _, _}), do: @float
  defp specifier_info(_kind, {:integer, _, _}), do: @integer
  defp specifier_info(_kind, {:bits, _, _}), do: @binary
  defp specifier_info(_kind, {:bitstring, _, _}), do: @binary
  defp specifier_info(_kind, {:bytes, _, _}), do: @binary
  defp specifier_info(_kind, {:binary, _, _}), do: @binary
  defp specifier_info(_kind, _specifier), do: @integer

  ## Remote

  def remote(expr, type, fun, arity, hints, meta, stack, context) do
    case atom_fetch(type) do
      {_, mods} ->
        context =
          Enum.reduce(mods, context, fn mod, context ->
            preload_and_maybe_check_export(mod, fun, arity, meta, stack, context)
          end)

        {mods, context}

      :error ->
        warning = {:badmodule, expr, type, fun, arity, hints, context}
        {[], warn(warning, meta, stack, context)}
    end
  end

  def remote(module, fun, arity, meta, stack, context) do
    preload_and_maybe_check_export(module, fun, arity, meta, stack, context)
  end

  defp preload_and_maybe_check_export(module, fun, arity, meta, stack, context)
       when is_atom(module) do
    if Keyword.get(meta, :runtime_module, false) do
      context
    else
      ParallelChecker.preload_module(stack.cache, module)
      check_export(module, fun, arity, meta, stack, context)
    end
  end

  defp check_export(module, fun, arity, meta, stack, context) do
    case ParallelChecker.fetch_export(stack.cache, module, fun, arity) do
      {:ok, mode, :def, reason} ->
        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:ok, mode, :defmacro, reason} ->
        context = warn({:unrequired_module, module, fun, arity}, meta, stack, context)
        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, stack) do
          warn({:undefined_module, module, fun, arity}, meta, stack, context)
        else
          context
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, stack) do
          exports = ParallelChecker.all_exports(stack.cache, module)
          warn({:undefined_function, module, fun, arity, exports}, meta, stack, context)
        else
          context
        end
    end
  end

  defp check_deprecated(:elixir, module, fun, arity, reason, meta, stack, context) do
    if reason do
      warn({:deprecated, module, fun, arity, reason}, meta, stack, context)
    else
      context
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, stack, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> :string.titlecase()
        warn({:deprecated, module, fun, arity, reason}, meta, stack, context)

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> :string.titlecase()
        reason = "It will be removed in #{removal}. #{reason}"
        warn({:deprecated, module, fun, arity, reason}, meta, stack, context)

      _ ->
        context
    end
  end

  # The protocol code dispatches to unknown modules, so we ignore them here.
  #
  #     try do
  #       SomeProtocol.Atom.__impl__
  #     rescue
  #       ...
  #     end
  #
  # But for protocols we don't want to traverse the protocol code anyway.
  # TODO: remove this clause once we no longer traverse the protocol code.
  defp warn_undefined?(_module, :__impl__, 1, _stack), do: false
  defp warn_undefined?(_module, :module_info, 0, _stack), do: false
  defp warn_undefined?(_module, :module_info, 1, _stack), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _stack), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _stack), do: false

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, stack) do
    not Enum.any?(stack.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  ## Warning helpers

  @doc """
  Emits incompatible types warning for the given expression.
  """
  def incompatible_warn(expr, expected_type, actual_type, hints \\ [], meta, stack, context) do
    warning = {:incompatible, expr, expected_type, actual_type, hints, context}
    warn(__MODULE__, warning, meta, stack, context)
  end

  defp warn(warning, meta, stack, context) do
    warn(__MODULE__, warning, meta, stack, context)
  end

  ## Traces

  def format_traces(expr, %{vars: vars}) do
    {_, versions} =
      Macro.prewalk(expr, %{}, fn
        {var_name, meta, var_context}, versions when is_atom(var_name) and is_atom(var_context) ->
          version = meta[:version]

          case vars do
            %{^version => data} -> {:ok, Map.put(versions, version, data)}
            %{} -> {:ok, versions}
          end

        node, versions ->
          {node, versions}
      end)

    vars = Map.values(versions)

    formatted_traces =
      vars
      |> Enum.sort_by(& &1.name)
      |> Enum.map(&format_trace/1)

    {formatted_traces, trace_hints(vars)}
  end

  defp format_trace(%{off_traces: []}) do
    []
  end

  defp format_trace(%{name: name, context: context, off_traces: traces}) do
    traces =
      traces
      |> Enum.reverse()
      |> Enum.map(fn {expr, file, type} ->
        meta = get_meta(expr)

        location =
          file
          |> Path.relative_to_cwd()
          |> Exception.format_file_line(meta[:line])
          |> String.replace_suffix(":", "")

        """

            # type: #{to_quoted_string(type)}
            # from: #{location}
            #{expr_to_string(expr)}
        """
      end)

    type_or_types = pluralize(traces, "type", "types")
    ["\nwhere #{format_var(name, context)} was given the #{type_or_types}:\n" | traces]
  end

  defp format_var({var, _, context}), do: format_var(var, context)
  defp format_var(var, nil), do: "\"#{var}\""
  defp format_var(var, context), do: "\"#{var}\" (context #{inspect(context)})"

  defp pluralize([_], singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp inferred_bitstring_spec?(trace) do
    match?({{:<<>>, [inferred_bitstring_spec: true] ++ _meta, _}, _file, _type}, trace)
  end

  defp trace_hints(vars) do
    if Enum.any?(vars, fn data -> Enum.any?(data.off_traces, &inferred_bitstring_spec?/1) end) do
      [:inferred_bitstring_spec]
    else
      []
    end
  end

  ## Warning formatting

  def format_warning({:refine_var, old_type, new_type, var, context}) do
    {traces, hints} = format_traces(var, context)

    [
      """
      incompatible types assigned to #{format_var(var)}:

          #{to_quoted_string(old_type)} !~ #{to_quoted_string(new_type)}
      """,
      traces,
      format_hints(hints),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:incompatible, expr, expected_type, actual_type, hints, context}) do
    {traces, trace_hints} = format_traces(expr, context)

    [
      """
      incompatible types in expression:

          #{expr_to_string(expr)}

      expected type:

          #{to_quoted_string(expected_type)}

      but got type:

          #{to_quoted_string(actual_type)}
      """,
      traces,
      format_hints(hints ++ trace_hints),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:badmap, expr, type, key, context}) do
    {traces, trace_hints} = format_traces(expr, context)

    [
      """
      expected a map or struct when accessing .#{key} in expression:

          #{expr_to_string(expr)}

      but got type:

          #{to_quoted_string(type)}
      """,
      traces,
      format_hints([:dot | trace_hints]),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:badkey, expr, type, key, context}) do
    {traces, trace_hints} = format_traces(expr, context)

    [
      """
      missing key .#{key} in expression:

          #{expr_to_string(expr)}

      the given type does not have the given key:

          #{to_quoted_string(type)}
      """,
      traces,
      format_hints(trace_hints),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:badmodule, expr, type, fun, arity, hints, context}) do
    {traces, trace_hints} = format_traces(expr, context)

    [
      """
      expected a module (an atom) when invoking #{fun}/#{arity} in expression:

          #{expr_to_string(expr)}

      but got type:

          #{to_quoted_string(type)}
      """,
      traces,
      format_hints(hints ++ trace_hints),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:undefined_module, module, fun, arity}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined (module ",
      inspect(module),
      " is not available or is yet to be defined)"
    ]
  end

  def format_warning({:undefined_function, module, fun, arity, exports}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined or private",
      UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
    ]
  end

  def format_warning({:deprecated, module, fun, arity, reason}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is deprecated. ",
      reason
    ]
  end

  def format_warning({:unrequired_module, module, fun, arity}) do
    [
      "you must require ",
      inspect(module),
      " before invoking the macro ",
      Exception.format_mfa(module, fun, arity)
    ]
  end
end
