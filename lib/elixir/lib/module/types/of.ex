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
  def refine_var(var, type, expr, formatter \\ :default, stack, context) do
    {var_name, meta, var_context} = var
    version = Keyword.fetch!(meta, :version)

    case context.vars do
      %{^version => %{type: old_type, off_traces: off_traces} = data} ->
        new_type = intersection(type, old_type)

        data = %{
          data
          | type: new_type,
            off_traces: new_trace(expr, type, formatter, stack, off_traces)
        }

        context = put_in(context.vars[version], data)

        # We need to return error otherwise it leads to cascading errors
        if empty?(new_type) do
          {:error, warn({:refine_var, old_type, type, var, context}, meta, stack, context)}
        else
          {:ok, new_type, context}
        end

      %{} ->
        data = %{
          type: type,
          name: var_name,
          context: var_context,
          off_traces: new_trace(expr, type, formatter, stack, [])
        }

        context = put_in(context.vars[version], data)
        {:ok, type, context}
    end
  end

  defp new_trace(nil, _type, _formatter, _stack, traces),
    do: traces

  defp new_trace(expr, type, formatter, stack, traces),
    do: [{expr, stack.file, type, formatter} | traces]

  ## Map/structs

  @doc """
  Handles fetching a map key.
  """
  def map_fetch(expr, type, field, stack, context) when is_atom(field) do
    case map_fetch(type, field) do
      {_optional?, value_type} ->
        {:ok, value_type, context}

      reason ->
        {:ok, dynamic(),
         warn({reason, expr, type, field, context}, elem(expr, 1), stack, context)}
    end
  end

  @doc """
  Builds a closed map.
  """
  def closed_map(pairs, extra \\ [], stack, context, of_fun) do
    result =
      reduce_ok(pairs, {true, extra, [], context}, fn
        {key, value}, {closed?, single, multiple, context} ->
          with {:ok, keys, context} <- of_finite_key_type(key, stack, context, of_fun),
               {:ok, value_type, context} <- of_fun.(value, stack, context) do
            case keys do
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

  defp of_finite_key_type(key, _stack, context, _of_fun) when is_atom(key) do
    {:ok, [key], context}
  end

  defp of_finite_key_type(key, stack, context, of_fun) do
    with {:ok, key_type, context} <- of_fun.(key, stack, context) do
      case atom_fetch(key_type) do
        {:finite, list} -> {:ok, list, context}
        _ -> {:ok, :none, context}
      end
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

  @doc """
  Handles structs creation.
  """
  def struct({:%, meta, _}, struct, args, default_handling, stack, context, of_fun)
      when is_atom(struct) do
    # The compiler has already checked the keys are atoms and which ones are required.
    with {:ok, args_types, context} <-
           map_reduce_ok(args, context, fn {key, value}, context when is_atom(key) ->
             with {:ok, type, context} <- of_fun.(value, stack, context) do
               {:ok, {key, type}, context}
             end
           end) do
      struct(struct, args_types, default_handling, meta, stack, context)
    end
  end

  @doc """
  Struct handling assuming the args have already been converted.
  """
  # TODO: Allow structs fields to be defined. If the fields are defined,
  # then the struct is no longer dynamic. And we need to validate args
  # against the struct types.
  # TODO: Use the struct default values to define the default types.
  def struct(struct, args_types, default_handling, meta, stack, context) do
    context = remote(struct, :__struct__, 0, meta, stack, context)
    term = term()

    defaults =
      for %{field: field} <- struct.__info__(:struct), field != :__struct__ do
        {field, term}
      end

    pairs =
      case default_handling do
        :merge_defaults -> [{:__struct__, atom([struct])} | defaults] ++ args_types
        :skip_defaults -> [{:__struct__, atom([struct])} | args_types]
        :only_defaults -> [{:__struct__, atom([struct])} | defaults]
      end

    {:ok, dynamic(closed_map(pairs)), context}
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

    with {:ok, _type, context} <- of_fun.(left, {expected_type, expr}, stack, context) do
      {:ok, context}
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

  ## Apply

  def apply(:erlang, name, [left, right], expr, stack, context)
      when name in [:>=, :"=<", :>, :<, :min, :max] do
    result = if name in [:min, :max], do: union(left, right), else: boolean()

    cond do
      match?({false, _}, map_fetch(left, :__struct__)) or
          match?({false, _}, map_fetch(right, :__struct__)) ->
        warning = {:struct_comparison, expr, context}
        {:ok, result, warn(warning, elem(expr, 1), stack, context)}

      number_type?(left) and number_type?(right) ->
        {:ok, result, context}

      empty?(intersection(left, right)) ->
        warning = {:mismatched_comparison, expr, context}
        {:ok, result, warn(warning, elem(expr, 1), stack, context)}

      true ->
        {:ok, result, context}
    end
  end

  def apply(mod, name, args, expr, stack, context) do
    case :elixir_rewrite.inline(mod, name, length(args)) do
      {mod, name} -> apply(mod, name, args, expr, stack, context)
      false -> {:ok, dynamic(), context}
    end
  end

  ## Remote

  def remote(type, fun, arity, hints \\ [], expr, meta, stack, context) do
    case atom_fetch(type) do
      {_, mods} ->
        context =
          Enum.reduce(mods, context, fn mod, context ->
            remote(mod, fun, arity, meta, stack, context)
          end)

        {mods, context}

      :error ->
        warning = {:badmodule, expr, type, fun, arity, hints, context}
        {[], warn(warning, meta, stack, context)}
    end
  end

  @doc """
  Checks a module is a valid remote.
  """
  def remote(module, fun, arity, meta, stack, context) when is_atom(module) do
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
  Intersects two types and emit an incompatible warning if empty.
  """
  def intersect(actual, {expected, expr}, stack, context) do
    type = intersection(actual, expected)

    if empty?(type) do
      {:error, incompatible_warn(expr, expected, actual, stack, context)}
    else
      {:ok, type, context}
    end
  end

  @doc """
  Emits incompatible types warning for the given expression.

  This is a generic warning for when the expected/actual types
  themselves may come from several different circumstances.
  """
  def incompatible_warn(expr, expected_type, actual_type, stack, context) do
    meta = get_meta(expr) || stack.meta
    hints = if meta[:inferred_bitstring_spec], do: [:inferred_bitstring_spec], else: []
    warning = {:incompatible, expr, expected_type, actual_type, hints, context}
    warn(warning, meta, stack, context)
  end

  defp warn(warning, meta, stack, context) do
    warn(__MODULE__, warning, meta, stack, context)
  end

  ## Traces

  def collect_traces(expr, %{vars: vars}) do
    {_, versions} =
      Macro.prewalk(expr, %{}, fn
        {var_name, meta, var_context}, versions when is_atom(var_name) and is_atom(var_context) ->
          version = meta[:version]

          case vars do
            %{^version => %{off_traces: [_ | _] = off_traces, name: name, context: context}} ->
              {:ok,
               Map.put(versions, version, %{
                 type: :variable,
                 name: name,
                 context: context,
                 traces: collect_var_traces(off_traces)
               })}

            _ ->
              {:ok, versions}
          end

        node, versions ->
          {node, versions}
      end)

    versions
    |> Map.values()
    |> Enum.sort_by(& &1.name)
  end

  defp collect_var_traces(traces) do
    traces
    |> Enum.reverse()
    |> Enum.map(fn {expr, file, type, formatter} ->
      meta = get_meta(expr)

      {formatted_expr, formatter_hints} =
        case formatter do
          :default -> {expr_to_string(expr), []}
          formatter -> formatter.(expr)
        end

      %{
        file: file,
        meta: meta,
        formatted_expr: formatted_expr,
        formatted_hints: format_hints(formatter_hints ++ expr_hints(expr)),
        formatted_type: to_quoted_string(type)
      }
    end)
  end

  def format_traces(traces) do
    Enum.map(traces, &format_trace/1)
  end

  defp format_trace(%{type: :variable, name: name, context: context, traces: traces}) do
    traces =
      for trace <- traces do
        location =
          trace.file
          |> Path.relative_to_cwd()
          |> Exception.format_file_line(trace.meta[:line])
          |> String.replace_suffix(":", "")

        [
          """

              # type: #{indent(trace.formatted_type, 4)}
              # from: #{location}
              \
          """,
          indent(trace.formatted_expr, 4),
          ?\n,
          trace.formatted_hints
        ]
      end

    type_or_types = pluralize(traces, "type", "types")
    ["\nwhere #{format_var(name, context)} was given the #{type_or_types}:\n" | traces]
  end

  defp format_var({var, _, context}), do: format_var(var, context)
  defp format_var(var, nil), do: "\"#{var}\""
  defp format_var(var, context), do: "\"#{var}\" (context #{inspect(context)})"

  defp pluralize([_], singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp expr_hints({:<<>>, [inferred_bitstring_spec: true] ++ _meta, _}),
    do: [:inferred_bitstring_spec]

  defp expr_hints(_), do: []

  ## Warning formatting

  def format_diagnostic({:refine_var, old_type, new_type, var, context}) do
    traces = collect_traces(var, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types assigned to #{format_var(var)}:

              #{to_quoted_string(old_type)} !~ #{to_quoted_string(new_type)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:incompatible, expr, expected_type, actual_type, hints, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types in expression:

              #{expr_to_string(expr) |> indent(4)}

          expected type:

              #{to_quoted_string(expected_type) |> indent(4)}

          but got type:

              #{to_quoted_string(actual_type) |> indent(4)}
          """,
          format_traces(traces),
          format_hints(hints)
        ])
    }
  end

  def format_diagnostic({:badmap, expr, type, key, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a map or struct when accessing .#{key} in expression:

              #{expr_to_string(expr) |> indent(4)}
          """,
          empty_if(dot_var?(expr), """

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """),
          format_traces(traces),
          format_hints([:dot])
        ])
    }
  end

  def format_diagnostic({:badkey, expr, type, key, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      span: expr |> get_meta() |> :elixir_env.calculate_span(key) |> Keyword.get(:span),
      message:
        IO.iodata_to_binary([
          """
          unknown key .#{key} in expression:

              #{expr_to_string(expr) |> indent(4)}
          """,
          empty_if(dot_var?(expr), """

          the given type does not have the given key:

              #{to_quoted_string(type) |> indent(4)}
          """),
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badmodule, expr, type, fun, arity, hints, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a module (an atom) when invoking #{fun}/#{arity} in expression:

              #{expr_to_string(expr) |> indent(4)}
          """,
          empty_if(dot_var?(expr), """

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """),
          format_traces(traces),
          format_hints(hints)
        ])
    }
  end

  def format_diagnostic({:mismatched_comparison, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison between incompatible types found:

              #{expr_to_string(expr) |> indent(4)}
          """,
          format_traces(traces),
          """

          While Elixir can compare across all types, you are comparing \
          across types which are always distinct, and the result is either \
          always true or always false
          """
        ])
    }
  end

  def format_diagnostic({:struct_comparison, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison with structs found:

              #{expr_to_string(expr) |> indent(4)}
          """,
          format_traces(traces),
          """

          Comparison operators (>, <, >=, <=, min, and max) perform structural \
          and not semantic comparison. Comparing with a struct won't give meaningful \
          results. Structs that can be compared typically define a compare/2 function \
          within their modules that can be used for semantic comparison.
          """
        ])
    }
  end

  def format_diagnostic({:undefined_module, module, fun, arity}) do
    top =
      if fun == :__struct__ and arity == 0 do
        "struct #{inspect(module)}"
      else
        Exception.format_mfa(module, fun, arity)
      end

    %{
      message:
        IO.iodata_to_binary([
          top,
          " is undefined (module ",
          inspect(module),
          " is not available or is yet to be defined)"
        ])
    }
  end

  def format_diagnostic({:undefined_function, module, :__struct__, 0, _exports}) do
    %{
      message:
        "struct #{inspect(module)} is undefined (there is such module but it does not define a struct)"
    }
  end

  def format_diagnostic({:undefined_function, module, fun, arity, exports}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is undefined or private",
          UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
        ])
    }
  end

  def format_diagnostic({:deprecated, module, fun, arity, reason}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is deprecated. ",
          reason
        ])
    }
  end

  def format_diagnostic({:unrequired_module, module, fun, arity}) do
    %{
      message:
        IO.iodata_to_binary([
          "you must require ",
          inspect(module),
          " before invoking the macro ",
          Exception.format_mfa(module, fun, arity)
        ])
    }
  end

  defp dot_var?(expr) do
    match?({{:., _, [var, _fun]}, _, _args} when is_var(var), expr)
  end

  defp empty_if(condition, content) do
    if condition, do: "", else: content
  end
end
