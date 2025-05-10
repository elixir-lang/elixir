# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

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
  Refines a variable that already exists (in a body).

  This only happens if the var contains a gradual type,
  or if we are doing a guard analysis or occurrence typing.
  Returns `true` if there was a refinement, `false` otherwise.
  """
  def refine_body_var({_, meta, _}, type, expr, stack, context) do
    version = Keyword.fetch!(meta, :version)
    %{vars: %{^version => %{type: old_type, off_traces: off_traces} = data} = vars} = context

    if gradual?(old_type) and type not in [term(), dynamic()] do
      case compatible_intersection(old_type, type) do
        {:ok, new_type} when new_type != old_type ->
          data = %{
            data
            | type: new_type,
              off_traces: new_trace(expr, new_type, stack, off_traces)
          }

          {new_type, %{context | vars: %{vars | version => data}}}

        _ ->
          {old_type, context}
      end
    else
      {old_type, context}
    end
  end

  @doc """
  Refines the type of a variable.

  Since this happens in a head, we use intersection
  because we want to refine types. Otherwise we should
  use compatibility.
  """
  def refine_head_var(var, type, expr, stack, context) do
    {var_name, meta, var_context} = var
    version = Keyword.fetch!(meta, :version)

    case context.vars do
      %{^version => %{type: old_type, off_traces: off_traces} = data} = vars ->
        new_type = intersection(type, old_type)

        data = %{
          data
          | type: new_type,
            off_traces: new_trace(expr, type, stack, off_traces)
        }

        context = %{context | vars: %{vars | version => data}}

        # We need to return error otherwise it leads to cascading errors
        if empty?(new_type) do
          {:error, error_type(),
           error({:refine_head_var, old_type, type, var, context}, meta, stack, context)}
        else
          {:ok, new_type, context}
        end

      %{} = vars ->
        data = %{
          type: type,
          name: var_name,
          context: var_context,
          off_traces: new_trace(expr, type, stack, [])
        }

        context = %{context | vars: Map.put(vars, version, data)}
        {:ok, type, context}
    end
  end

  defp new_trace(nil, _type, _stack, traces),
    do: traces

  defp new_trace(expr, type, stack, traces),
    do: [{expr, stack.file, type} | traces]

  ## Implementations

  impls = [
    {Atom, atom()},
    {BitString, binary()},
    {Float, float()},
    {Function, fun()},
    {Integer, integer()},
    {List, union(empty_list(), non_empty_list(term(), term()))},
    {Map, open_map(__struct__: if_set(negation(atom())))},
    {Port, port()},
    {PID, pid()},
    {Reference, reference()},
    {Tuple, tuple()},
    {Any, term()}
  ]

  for {for, type} <- impls do
    def impl(unquote(for)), do: unquote(Macro.escape(type))
  end

  def impl(struct) do
    # Elixir did not strictly require the implementation to be available, so we need a fallback.
    # TODO: Assume implementation is available on Elixir v2.0.
    if info = Code.ensure_loaded?(struct) && struct.__info__(:struct) do
      struct_type(struct, info)
    else
      open_map(__struct__: atom([struct]))
    end
  end

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
  def closed_map(pairs, expected, stack, context, of_fun) do
    {pairs_types, context} = pairs(pairs, expected, stack, context, of_fun)

    map =
      permutate_map(pairs_types, stack, fn fallback, _keys, pairs ->
        # TODO: Use the fallback type to actually indicate if open or closed.
        if fallback == none(), do: closed_map(pairs), else: dynamic(open_map(pairs))
      end)

    {map, context}
  end

  @doc """
  Computes the types of key-value pairs.
  """
  def pairs(pairs, _expected, %{mode: :traversal} = stack, context, of_fun) do
    Enum.map_reduce(pairs, context, fn {key, value}, context ->
      {_key_type, context} = of_fun.(key, term(), stack, context)
      {value_type, context} = of_fun.(value, term(), stack, context)
      {{true, :none, value_type}, context}
    end)
  end

  def pairs(pairs, expected, stack, context, of_fun) do
    Enum.map_reduce(pairs, context, fn {key, value}, context ->
      {dynamic_key?, keys, context} = finite_key_type(key, stack, context, of_fun)

      expected_value_type =
        with [key] <- keys, {_, expected_value_type} <- map_fetch(expected, key) do
          expected_value_type
        else
          _ -> term()
        end

      {value_type, context} = of_fun.(value, expected_value_type, stack, context)
      {{dynamic_key? or gradual?(value_type), keys, value_type}, context}
    end)
  end

  defp finite_key_type(key, _stack, context, _of_fun) when is_atom(key) do
    {false, [key], context}
  end

  defp finite_key_type(key, stack, context, of_fun) do
    {key_type, context} = of_fun.(key, term(), stack, context)

    case atom_fetch(key_type) do
      {:finite, list} -> {gradual?(key_type), list, context}
      _ -> {gradual?(key_type), :none, context}
    end
  end

  @doc """
  Builds permutation of maps according to the given pairs types.
  """
  def permutate_map(_pairs_types, %{mode: :traversal}, _of_map) do
    dynamic()
  end

  def permutate_map(pairs_types, _stack, of_map) do
    {dynamic?, fallback, single, multiple, assert} =
      Enum.reduce(pairs_types, {false, none(), [], [], []}, fn
        {dynamic_pair?, keys, value_type}, {dynamic?, fallback, single, multiple, assert} ->
          dynamic? = dynamic? or dynamic_pair?

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

              {dynamic?, fallback, [], [], assert}

            # Because a multiple key may override single keys, we can only
            # collect single keys while there are no multiples.
            [key] when multiple == [] ->
              {dynamic?, fallback, [{key, value_type} | single], multiple, assert}

            keys ->
              {dynamic?, fallback, single, [{keys, value_type} | multiple], assert}
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

    if dynamic?, do: dynamic(map), else: map
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
  # TODO: Type check the fields match the struct
  def struct_instance(struct, args, expected, meta, %{mode: mode} = stack, context, of_fun)
      when is_atom(struct) do
    {_info, context} = struct_info(struct, meta, stack, context)

    # The compiler has already checked the keys are atoms and which ones are required.
    {args_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context when is_atom(key) ->
        value_type =
          with true <- mode != :traversal,
               {_, expected_value_type} <- map_fetch(expected, key) do
            expected_value_type
          else
            _ -> term()
          end

        {type, context} = of_fun.(value, value_type, stack, context)
        {{key, type}, context}
      end)

    {closed_map([{:__struct__, atom([struct])} | args_types]), context}
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
  # TODO: This function should not receive args_types once
  # we introduce typed structs. They are only used by exceptions.
  def struct_type(struct, info, args_types \\ []) do
    term = dynamic()
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

    {actual, context} =
      case kind do
        :match ->
          Module.Types.Pattern.of_match_var(left, type, expr, stack, context)

        :guard ->
          Module.Types.Pattern.of_guard(left, type, expr, stack, context)

        :expr ->
          left = annotate_interpolation(left, right)
          Module.Types.Expr.of_expr(left, type, expr, stack, context)
      end

    if compatible?(actual, type) do
      specifier_size(kind, right, stack, context)
    else
      error = {:badbinary, kind, meta, expr, type, actual, context}
      error(error, meta, stack, context)
    end
  end

  defp annotate_interpolation(
         {{:., _, [String.Chars, :to_string]} = dot, meta, [arg]},
         {:binary, _, nil}
       ) do
    {dot, [type_check: :interpolation] ++ meta, [arg]}
  end

  defp annotate_interpolation(left, _right) do
    left
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
    {actual, context} = Module.Types.Expr.of_expr(arg, integer(), expr, stack, context)
    compatible_size(actual, expr, stack, context)
  end

  defp specifier_size(_pattern_or_guard, {:size, _, [arg]} = expr, stack, context)
       when not is_integer(arg) do
    {actual, context} = Module.Types.Pattern.of_guard(arg, integer(), expr, stack, context)
    compatible_size(actual, expr, stack, context)
  end

  defp specifier_size(_kind, _specifier, _stack, context) do
    context
  end

  defp compatible_size(actual, expr, stack, context) do
    if compatible?(actual, integer()) do
      context
    else
      error = {:badsize, expr, actual, context}
      error(error, elem(expr, 1), stack, context)
    end
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

  ## Warning

  defp error(warning, meta, stack, context) do
    error(__MODULE__, warning, meta, stack, context)
  end

  def format_diagnostic({:refine_head_var, old_type, new_type, var, context}) do
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

  def format_diagnostic({:badbinary, kind, meta, expr, expected_type, actual_type, context}) do
    type = if kind == :match, do: "matching", else: "construction"
    hints = if meta[:inferred_bitstring_spec], do: [:inferred_bitstring_spec], else: []
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types in binary #{type}:

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

  def format_diagnostic({:badsize, expr, type, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected an integer in binary size:

              #{expr_to_string(expr) |> indent(4)}

          got type:

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

              #{expr_to_string(expr, collapse_structs: false) |> indent(4)}

          the given type does not have the given key:

              #{to_quoted_string(type, collapse_structs: false) |> indent(4)}
          """,
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
