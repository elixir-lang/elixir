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
          {error_type(), error({:refine_var, old_type, type, var, context}, meta, stack, context)}
        else
          {new_type, context}
        end

      %{} ->
        data = %{
          type: type,
          name: var_name,
          context: var_context,
          off_traces: new_trace(expr, type, formatter, stack, [])
        }

        context = put_in(context.vars[version], data)
        {type, context}
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
        {value_type, context}

      reason ->
        {error_type(), error({reason, expr, type, field, context}, elem(expr, 1), stack, context)}
    end
  end

  @doc """
  Builds a closed map.
  """
  def closed_map(pairs, extra \\ [], stack, context, of_fun) do
    {closed?, single, multiple, context} =
      Enum.reduce(pairs, {true, extra, [], context}, fn
        {key, value}, {closed?, single, multiple, context} ->
          {keys, context} = of_finite_key_type(key, stack, context, of_fun)
          {value_type, context} = of_fun.(value, stack, context)

          case keys do
            :none ->
              {false, single, multiple, context}

            [key] when multiple == [] ->
              {closed?, [{key, value_type} | single], multiple, context}

            keys ->
              {closed?, single, [{keys, value_type} | multiple], context}
          end
      end)

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

    {map, context}
  end

  defp of_finite_key_type(key, _stack, context, _of_fun) when is_atom(key) do
    {[key], context}
  end

  defp of_finite_key_type(key, stack, context, of_fun) do
    {key_type, context} = of_fun.(key, stack, context)

    case atom_fetch(key_type) do
      {:finite, list} -> {list, context}
      _ -> {:none, context}
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
    {args_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context when is_atom(key) ->
        {type, context} = of_fun.(value, stack, context)
        {{key, type}, context}
      end)

    struct(struct, args_types, default_handling, meta, stack, context)
  end

  @doc """
  Struct handling assuming the args have already been converted.
  """
  # TODO: Allow structs fields to be defined. If the fields are defined,
  # then the struct is no longer dynamic. And we need to validate args
  # against the struct types.
  # TODO: Use the struct default values to define the default types.
  def struct(struct, args_types, default_handling, meta, stack, context) do
    {info, context} = struct_info(struct, meta, stack, context)
    term = term()
    defaults = for %{field: field} <- info, do: {field, term}

    pairs =
      case default_handling do
        :merge_defaults -> [{:__struct__, atom([struct])} | defaults] ++ args_types
        :skip_defaults -> [{:__struct__, atom([struct])} | args_types]
        :only_defaults -> [{:__struct__, atom([struct])} | defaults]
      end

    {dynamic(closed_map(pairs)), context}
  end

  @doc """
  Returns `__info__(:struct)` information about a struct.
  """
  def struct_info(struct, meta, stack, context) do
    context = remote(struct, :__struct__, 0, meta, stack, context)

    info =
      struct.__info__(:struct) ||
        raise "expected #{inspect(struct)} to return struct metadata, but got none"

    {info, context}
  end

  ## Binary

  @doc """
  Handles binaries.

  In the stack, we add nodes such as <<expr>>, <<..., expr>>, etc,
  based on the position of the expression within the binary.
  """
  def binary([], _kind, _stack, context) do
    context
  end

  def binary([head], kind, stack, context) do
    binary_segment(head, kind, [head], stack, context)
  end

  def binary([head | tail], kind, stack, context) do
    context = binary_segment(head, kind, [head, @suffix], stack, context)
    binary_many(tail, kind, stack, context)
  end

  defp binary_many([last], kind, stack, context) do
    binary_segment(last, kind, [@prefix, last], stack, context)
  end

  defp binary_many([head | tail], kind, stack, context) do
    context = binary_segment(head, kind, [@prefix, head, @suffix], stack, context)
    binary_many(tail, kind, stack, context)
  end

  # If the segment is a literal, the compiler has already checked its validity,
  # so we just skip it.
  defp binary_segment({:"::", _meta, [left, _right]}, _kind, _args, _stack, context)
       when is_binary(left) or is_number(left) do
    context
  end

  defp binary_segment({:"::", meta, [left, right]}, kind, args, stack, context) do
    type = specifier_type(kind, right)
    expr = {:<<>>, meta, args}

    {_type, context} =
      case kind do
        :match ->
          Module.Types.Pattern.of_match_var(left, type, expr, stack, context)

        :guard ->
          Module.Types.Pattern.of_guard(left, type, expr, stack, context)

        :expr ->
          {actual, context} = Module.Types.Expr.of_expr(left, stack, context)
          intersect(actual, type, expr, stack, context)
      end

    specifier_size(kind, right, expr, stack, context)
  end

  defp specifier_type(kind, {:-, _, [left, _right]}), do: specifier_type(kind, left)
  defp specifier_type(:match, {:utf8, _, _}), do: @integer
  defp specifier_type(:match, {:utf16, _, _}), do: @integer
  defp specifier_type(:match, {:utf32, _, _}), do: @integer
  defp specifier_type(:match, {:float, _, _}), do: @float
  defp specifier_type(_kind, {:float, _, _}), do: @integer_or_float
  defp specifier_type(_kind, {:utf8, _, _}), do: @integer_or_binary
  defp specifier_type(_kind, {:utf16, _, _}), do: @integer_or_binary
  defp specifier_type(_kind, {:utf32, _, _}), do: @integer_or_binary
  defp specifier_type(_kind, {:integer, _, _}), do: @integer
  defp specifier_type(_kind, {:bits, _, _}), do: @binary
  defp specifier_type(_kind, {:bitstring, _, _}), do: @binary
  defp specifier_type(_kind, {:bytes, _, _}), do: @binary
  defp specifier_type(_kind, {:binary, _, _}), do: @binary
  defp specifier_type(_kind, _specifier), do: @integer

  defp specifier_size(kind, {:-, _, [left, right]}, expr, stack, context) do
    specifier_size(kind, right, expr, stack, specifier_size(kind, left, expr, stack, context))
  end

  defp specifier_size(:expr, {:size, _, [arg]}, expr, stack, context)
       when not is_integer(arg) do
    {actual, context} = Module.Types.Expr.of_expr(arg, stack, context)
    {_, context} = intersect(actual, integer(), expr, stack, context)
    context
  end

  defp specifier_size(_pattern_or_guard, {:size, _, [arg]}, expr, stack, context)
       when not is_integer(arg) do
    {_type, context} = Module.Types.Pattern.of_guard(arg, integer(), expr, stack, context)
    context
  end

  defp specifier_size(_kind, _specifier, _expr, _stack, context) do
    context
  end

  ## Apply

  # TODO: Implement element without a literal index
  # TODO: Add a test for an open tuple (inferred from a guard)
  # TODO: Implement set_element

  def apply(:erlang, :element, [_, type], {_, meta, [index, _]} = expr, stack, context)
      when is_integer(index) do
    case tuple_fetch(type, index - 1) do
      {_optional?, value_type} ->
        {value_type, context}

      reason ->
        {error_type(), error({reason, expr, type, index - 1, context}, meta, stack, context)}
    end
  end

  def apply(:erlang, name, [left, right], expr, stack, context)
      when name in [:>=, :"=<", :>, :<, :min, :max] do
    result = if name in [:min, :max], do: union(left, right), else: boolean()

    cond do
      match?({false, _}, map_fetch(left, :__struct__)) or
          match?({false, _}, map_fetch(right, :__struct__)) ->
        warning = {:struct_comparison, expr, context}
        {result, warn(__MODULE__, warning, elem(expr, 1), stack, context)}

      number_type?(left) and number_type?(right) ->
        {result, context}

      disjoint?(left, right) ->
        warning = {:mismatched_comparison, expr, context}
        {result, warn(__MODULE__, warning, elem(expr, 1), stack, context)}

      true ->
        {result, context}
    end
  end

  def apply(mod, name, args, expr, stack, context) do
    case :elixir_rewrite.inline(mod, name, length(args)) do
      {mod, name} -> apply(mod, name, args, expr, stack, context)
      false -> {dynamic(), context}
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
        {[], error(warning, meta, stack, context)}
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
        context =
          warn(__MODULE__, {:unrequired_module, module, fun, arity}, meta, stack, context)

        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, stack) do
          warn(__MODULE__, {:undefined_module, module, fun, arity}, meta, stack, context)
        else
          context
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, stack) do
          exports = ParallelChecker.all_exports(stack.cache, module)
          payload = {:undefined_function, module, fun, arity, exports}
          warn(__MODULE__, payload, meta, stack, context)
        else
          context
        end
    end
  end

  defp check_deprecated(:elixir, module, fun, arity, reason, meta, stack, context) do
    if reason do
      warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)
    else
      context
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, stack, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> :string.titlecase()
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> :string.titlecase()
        reason = "It will be removed in #{removal}. #{reason}"
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

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
  Intersects two types and emit an incompatible error if empty.
  """
  def intersect(actual, expected, expr, stack, context) do
    type = intersection(actual, expected)

    if empty?(type) do
      {error_type(), incompatible_error(expr, expected, actual, stack, context)}
    else
      {type, context}
    end
  end

  @doc """
  Emits incompatible types warning for the given expression.

  This is a generic warning for when the expected/actual types
  themselves may come from several different circumstances.
  """
  def incompatible_error(expr, expected_type, actual_type, stack, context) do
    meta = get_meta(expr) || stack.meta
    hints = if meta[:inferred_bitstring_spec], do: [:inferred_bitstring_spec], else: []
    warning = {:incompatible, expr, expected_type, actual_type, hints, context}
    error(warning, meta, stack, context)
  end

  defp error(warning, meta, stack, context) do
    error(__MODULE__, warning, meta, stack, context)
  end

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

  def format_diagnostic({:badtuple, expr, type, index, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a tuple when accessing element at index #{index} in expression:

              #{expr_to_string(expr) |> indent(4)}

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badindex, expr, type, index, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          out of range tuple access at index #{index} in expression:

              #{expr_to_string(expr) |> indent(4)}

          the given type does not have the given index:

              #{to_quoted_string(type) |> indent(4)}
          """,
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
        ]),
      group: true
    }
  end

  def format_diagnostic({:undefined_function, module, :__struct__, 0, _exports}) do
    %{
      message:
        "struct #{inspect(module)} is undefined (there is such module but it does not define a struct)",
      group: true
    }
  end

  def format_diagnostic({:undefined_function, module, fun, arity, exports}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is undefined or private",
          UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
        ]),
      group: true
    }
  end

  def format_diagnostic({:deprecated, module, fun, arity, reason}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is deprecated. ",
          reason
        ]),
      group: true
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
        ]),
      group: true
    }
  end

  defp dot_var?(expr) do
    match?({{:., _, [var, _fun]}, _, _args} when is_var(var), expr)
  end

  defp empty_if(condition, content) do
    if condition, do: "", else: content
  end
end
