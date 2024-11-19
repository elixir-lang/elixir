defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false
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
  Marks a variable with error.
  """
  def error_var(var, context) do
    {var_name, meta, var_context} = var
    version = Keyword.fetch!(meta, :version)

    data = %{
      type: error_type(),
      name: var_name,
      context: var_context,
      off_traces: []
    }

    put_in(context.vars[version], data)
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
          {:error, error_type(),
           error({:refine_var, old_type, type, var, context}, meta, stack, context)}
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
        {value_type, context}

      reason ->
        {error_type(), error({reason, expr, type, field, context}, elem(expr, 1), stack, context)}
    end
  end

  @doc """
  Builds a closed map.
  """
  def closed_map(pairs, stack, context, of_fun) do
    permutate_map(pairs, stack, context, of_fun, fn fallback, _keys, pairs ->
      # TODO: Use the fallback type to actually indicate if open or closed.
      if fallback == none(), do: closed_map(pairs), else: open_map(pairs)
    end)
  end

  @doc """
  Builds permutation of maps according to the given keys.
  """
  def permutate_map(pairs, %{mode: :traversal} = stack, context, of_fun, _of_map) do
    context =
      Enum.reduce(pairs, context, fn {key, value}, context ->
        {_, context} = of_fun.(key, stack, context)
        {_, context} = of_fun.(value, stack, context)
        context
      end)

    {dynamic(), context}
  end

  def permutate_map(pairs, stack, context, of_fun, of_map) do
    {dynamic?, fallback, single, multiple, assert, context} =
      Enum.reduce(pairs, {false, none(), [], [], [], context}, fn
        {key, value}, {dynamic?, fallback, single, multiple, assert, context} ->
          {dynamic_key?, keys, context} = finite_key_type(key, stack, context, of_fun)
          {value_type, context} = of_fun.(value, stack, context)
          dynamic? = dynamic? or dynamic_key? or gradual?(value_type)

          case keys do
            :none ->
              fallback = union(fallback, value_type)

              {fallback, assert} =
                Enum.reduce(single, {fallback, assert}, fn {key, type}, {fallback, assert} ->
                  {union(fallback, type), [key | assert]}
                end)

              {fallback, assert} =
                Enum.reduce(multiple, {fallback, assert}, fn {keys, type}, {fallback, assert} ->
                  {union(fallback, type), keys ++ assert}
                end)

              {dynamic?, fallback, [], [], assert, context}

            [key] when multiple == [] ->
              {dynamic?, fallback, [{key, value_type} | single], multiple, assert, context}

            keys ->
              {dynamic?, fallback, single, [{keys, value_type} | multiple], assert, context}
          end
      end)

    map =
      case Enum.reverse(multiple) do
        [] ->
          of_map.(fallback, Enum.uniq(assert), Enum.reverse(single))

        [{keys, type} | tail] ->
          for key <- keys, t <- cartesian_map(tail) do
            of_map.(fallback, Enum.uniq(assert), Enum.reverse(single, [{key, type} | t]))
          end
          |> Enum.reduce(&union/2)
      end

    if dynamic?, do: {dynamic(map), context}, else: {map, context}
  end

  defp finite_key_type(key, _stack, context, _of_fun) when is_atom(key) do
    {false, [key], context}
  end

  defp finite_key_type(key, stack, context, of_fun) do
    {key_type, context} = of_fun.(key, stack, context)

    case atom_fetch(key_type) do
      {:finite, list} -> {gradual?(key_type), list, context}
      _ -> {gradual?(key_type), :none, context}
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
  Handles instantiation of a new struct.
  """
  def struct_instance(struct, args, meta, stack, context, of_fun)
      when is_atom(struct) do
    # The compiler has already checked the keys are atoms and which ones are required.
    {args_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context when is_atom(key) ->
        {type, context} = of_fun.(value, stack, context)
        {{key, type}, context}
      end)

    {info, context} = struct_info(struct, meta, stack, context)
    {struct_type(struct, info, args_types), context}
  end

  @doc """
  Returns `__info__(:struct)` information about a struct.
  """
  def struct_info(struct, meta, stack, context) do
    case stack.no_warn_undefined do
      %Macro.Env{} = env ->
        case :elixir_map.maybe_load_struct_info(meta, struct, [], false, env) do
          {:ok, info} -> {info, context}
          {:error, desc} -> raise ArgumentError, List.to_string(:elixir_map.format_error(desc))
        end

      _ ->
        # Fetch the signature to validate for warnings.
        {_, context} = Module.Types.Apply.signature(struct, :__struct__, 0, meta, stack, context)

        info =
          struct.__info__(:struct) ||
            raise "expected #{inspect(struct)} to return struct metadata, but got none"

        {info, context}
    end
  end

  @doc """
  Builds a type from the struct info.
  """
  def struct_type(struct, info, args_types \\ []) do
    term = term()
    pairs = for %{field: field} <- info, do: {field, term}
    pairs = [{:__struct__, atom([struct])} | pairs]
    pairs = if args_types == [], do: pairs, else: pairs ++ args_types
    closed_map(pairs)
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

    specifier_size(kind, right, stack, context)
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

  defp specifier_size(kind, {:-, _, [left, right]}, stack, context) do
    specifier_size(kind, right, stack, specifier_size(kind, left, stack, context))
  end

  defp specifier_size(:expr, {:size, _, [arg]} = expr, stack, context)
       when not is_integer(arg) do
    {actual, context} = Module.Types.Expr.of_expr(arg, stack, context)
    {_, context} = intersect(actual, integer(), expr, stack, context)
    context
  end

  defp specifier_size(_pattern_or_guard, {:size, _, [arg]} = expr, stack, context)
       when not is_integer(arg) do
    {_type, context} = Module.Types.Pattern.of_guard(arg, integer(), expr, stack, context)
    context
  end

  defp specifier_size(_kind, _specifier, _stack, context) do
    context
  end

  ## Modules

  @doc """
  Returns modules in a type.

  The call information is used on report reporting.
  """
  def modules(type, fun, arity, hints \\ [], expr, meta, stack, context) do
    case atom_fetch(type) do
      {_, mods} ->
        {mods, context}

      :error ->
        warning = {:badmodule, expr, type, fun, arity, hints, context}
        {[], error(warning, meta, stack, context)}
    end
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

          got type:

              #{to_quoted_string(actual_type) |> indent(4)}

          but expected type:

              #{to_quoted_string(expected_type) |> indent(4)}
          """,
          format_traces(traces),
          format_hints(hints)
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

  defp dot_var?(expr) do
    match?({{:., _, [var, _fun]}, _, _args} when is_var(var), expr)
  end

  defp empty_if(condition, content) do
    if condition, do: "", else: content
  end
end
