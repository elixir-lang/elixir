# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false
  import Module.Types.{Helpers, Descr}

  @prefix quote(do: ...)
  @suffix quote(do: ...)

  @integer_or_float union(integer(), float())
  @integer integer()
  @float float()
  @binary binary()
  @bitstring bitstring()

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

  This purposedly deletes all traces of the variable,
  as it is often invoked when the cause for error is elsewhere.
  """
  def error_var({_, meta, _}, context) do
    error_var(Keyword.fetch!(meta, :version), context)
  end

  def error_var(version, context) do
    update_in(context.vars[version], fn
      %{errored: true} = data -> data
      data -> Map.put(%{data | type: error_type(), off_traces: []}, :errored, true)
    end)
  end

  @doc """
  Declares a variable.
  """
  def declare_var(var, context) do
    {var_name, meta, var_context} = var
    version = Keyword.fetch!(meta, :version)

    case context.vars do
      %{^version => _} ->
        context

      vars ->
        data = %{
          type: term(),
          name: var_name,
          context: var_context,
          off_traces: [],
          paths: [],
          deps: %{}
        }

        %{context | vars: Map.put(vars, version, data)}
    end
  end

  @doc """
  Tracks metadata about variables dependencies and paths.
  """
  def track_var(version, new_deps, new_paths, context) do
    update_in(context.vars[version], fn %{paths: paths, deps: deps} = data ->
      %{data | paths: new_paths ++ paths, deps: Enum.reduce(new_deps, deps, &Map.put(&2, &1, []))}
    end)
  end

  @doc """
  Refines a variable that already exists (in a body).

  This only happens if the var contains a gradual type,
  or if we are doing a guard analysis or occurrence typing.
  Returns `true` if there was a refinement, `false` otherwise.
  """
  def refine_body_var({_, meta, _}, type, expr, stack, context) do
    refine_body_var(Keyword.fetch!(meta, :version), type, expr, stack, context)
  end

  def refine_body_var(version, type, expr, stack, context)
      when is_integer(version) or is_reference(version) do
    %{vars: %{^version => %{type: old_type, off_traces: off_traces} = data} = vars} = context

    context =
      case context.conditional_vars do
        %{} = conditional_vars ->
          %{context | conditional_vars: Map.put(conditional_vars, version, true)}

        nil ->
          context
      end

    if gradual?(old_type) and type not in [term(), dynamic()] and not is_map_key(data, :errored) do
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
  def refine_head_var({_, meta, _}, type, expr, stack, context) do
    refine_head_var(Keyword.fetch!(meta, :version), type, expr, stack, context)
  end

  def refine_head_var(version, type, expr, stack, context)
      when is_integer(version) or is_reference(version) do
    case context.vars do
      %{^version => %{errored: true}} ->
        {:ok, error_type(), context}

      %{^version => %{type: old_type, off_traces: off_traces} = data} = vars ->
        new_type = intersection(type, old_type)

        data = %{
          data
          | type: new_type,
            off_traces: new_trace(expr, type, stack, off_traces)
        }

        if empty?(new_type) do
          data = Map.put(%{data | type: error_type()}, :errored, true)
          context = %{context | vars: %{vars | version => data}}
          {:error, old_type, context}
        else
          context = %{context | vars: %{vars | version => data}}
          {:ok, new_type, context}
        end
    end
  end

  defp new_trace(nil, _type, _stack, traces),
    do: traces

  defp new_trace(expr, type, stack, traces),
    do: [{expr, stack.file, type} | traces]

  @doc """
  Preserves `context` in first argument while
  resetting it to the vars in the second argument.
  """
  def reset_vars(context, %{
        subpatterns: subpatterns,
        vars: vars,
        conditional_vars: conditional_vars
      }),
      do: %{context | subpatterns: subpatterns, vars: vars, conditional_vars: conditional_vars}

  @doc """
  Executes the args with acc using conditional variables.
  """
  def with_conditional_vars(args, acc, expr, stack, context, fun) do
    %{vars: vars, conditional_vars: conditional_vars} = context

    {vars_conds, {acc, context}} =
      Enum.map_reduce(args, {acc, context}, fn arg, {acc, context} ->
        {acc, context} = fun.(arg, acc, %{context | vars: vars, conditional_vars: %{}})
        %{vars: vars, conditional_vars: cond_vars} = context
        {{vars, cond_vars}, {acc, context}}
      end)

    context = %{context | vars: vars, conditional_vars: conditional_vars}
    {acc, reduce_conditional_vars(vars_conds, expr, stack, context)}
  end

  @doc """
  Reduces conditional variables collected separately.
  """
  def reduce_conditional_vars([{vars, cond} | vars_conds], expr, stack, context) do
    %{vars: pre_vars} = context

    Enum.reduce(Map.keys(cond), context, fn version, context ->
      if is_map_key(pre_vars, version) and
           Enum.all?(vars_conds, fn {_vars, cond} -> is_map_key(cond, version) end) do
        %{^version => %{type: type}} = vars

        type =
          Enum.reduce(vars_conds, type, fn {vars, _cond}, acc ->
            %{^version => %{type: type}} = vars
            union(acc, type)
          end)

        {_, context} = refine_body_var(version, type, expr, stack, context)
        context
      else
        context
      end
    end)
  end

  ## Implementations

  impls = [
    {Atom, atom()},
    {BitString, bitstring()},
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

  @doc """
  Currently, for protocol implementations, we only store
  the open struct definition. This is because we don't want
  to reconsolidate whenever the struct changes, but at the
  moment we can't store references either. Ideally struct
  types on protocol dispatches would be lazily resolved.
  """
  def impl(for, mode \\ :closed)

  for {for, type} <- impls do
    def impl(unquote(for), _mode), do: unquote(Macro.escape(type))
  end

  def impl(struct, mode) do
    # Elixir did not strictly require the implementation to be available,
    # so we need to deal with such cases accordingly.
    # TODO: Assume implementation is available on Elixir v2.0.
    # A warning is emitted since v1.19+.
    if info = mode == :closed && Code.ensure_loaded?(struct) && struct.__info__(:struct) do
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
    case map_fetch_key(type, field) do
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

    {dynamic?, domain, single, multiple} =
      Enum.reduce(pairs_types, {false, [], [], []}, fn
        {pos_neg_domain, dynamic_pair?, value_type}, {dynamic?, domain, single, multiple} ->
          dynamic? = dynamic? or dynamic_pair?

          case pos_neg_domain do
            # If atom is included in domain keys, it unions all previous
            # single and multiple, except the ones negated:
            #
            #     %{foo: :bar, term() => :baz}
            #     #=> %{foo: :bar or :baz, term() => :baz}
            #
            #     %{foo: :bar, not :foo => :baz}
            #     #=> %{foo: :bar, term() => :baz}
            #
            # In case the negated term does not appear, we set it to none():
            #
            #     %{foo: :bar, term() => :baz}
            #     #=> %{term() => :baz, foo: :bar or :baz}
            #
            #     %{not :foo => :baz}
            #     #=> %{term() => :baz, foo: none()}
            #
            # In case we are dealing with multiple keys, we always merge the
            # domain. A more precise approach would be to postpone doing so
            # until the cartesian map is distributed but those should be very
            # uncommon.
            {[], negs, domain_keys} ->
              if :atom in domain_keys do
                {single, multiple} = union_negated(negs, value_type, single, multiple)
                {dynamic?, [{domain_keys, value_type} | domain], single, multiple}
              else
                {dynamic?, [{domain_keys, value_type} | domain], single, multiple}
              end

            {pos, [], domain_keys} ->
              domain =
                case domain_keys do
                  [] -> domain
                  _ -> [{domain_keys, value_type} | domain]
                end

              case pos do
                # Because a multiple key may override single keys, we can only
                # collect single keys while there are no multiples.
                [key] when multiple == [] ->
                  {dynamic?, domain, [{key, value_type} | single], multiple}

                _ ->
                  {dynamic?, domain, single, [{pos, value_type} | multiple]}
              end
          end
      end)

    non_multiple = Enum.reverse(single, domain)

    map =
      case Enum.reverse(multiple) do
        [] ->
          closed_map(non_multiple)

        [{keys, type} | tail] ->
          for key <- keys, t <- cartesian_map(tail) do
            closed_map(non_multiple ++ [{key, type} | t])
          end
          |> Enum.reduce(&union/2)
      end

    {if(dynamic?, do: dynamic(map), else: map), context}
  end

  defp union_negated([], new_type, single, multiple) do
    single = Enum.map(single, fn {key, old_type} -> {key, union(old_type, new_type)} end)
    multiple = Enum.map(multiple, fn {keys, old_type} -> {keys, union(old_type, new_type)} end)
    {single, multiple}
  end

  defp union_negated(negated, new_type, single, multiple) do
    {single, matched} =
      Enum.map_reduce(single, [], fn {key, old_type}, matched ->
        if key in negated do
          {{key, old_type}, [key | matched]}
        else
          {{key, union(old_type, new_type)}, matched}
        end
      end)

    multiple =
      Enum.map(multiple, fn {keys, old_type} ->
        {keys, union(old_type, new_type)}
      end)

    {Enum.map(negated -- matched, fn key -> {key, not_set()} end) ++ single, multiple}
  end

  defp pairs(pairs, expected, stack, context, of_fun) do
    Enum.map_reduce(pairs, context, fn {key, value}, context ->
      {pos_neg_domain, dynamic_key?, context} = map_key_type(key, stack, context, of_fun)

      expected_value_type =
        with {[key], [], []} <- pos_neg_domain,
             {_, expected_value_type} <- map_fetch_key(expected, key) do
          expected_value_type
        else
          _ -> term()
        end

      {value_type, context} = of_fun.(value, expected_value_type, stack, context)
      {{pos_neg_domain, dynamic_key? or gradual?(value_type), value_type}, context}
    end)
  end

  defp map_key_type(key, _stack, context, _of_fun) when is_atom(key) do
    {{[key], [], []}, false, context}
  end

  defp map_key_type(key, stack, context, of_fun) do
    {key_type, context} = of_fun.(key, term(), stack, context)
    domain_keys = to_domain_keys(key_type)

    pos_neg_domain =
      case atom_fetch(key_type) do
        {:finite, list} -> {list, [], List.delete(domain_keys, :atom)}
        {:infinite, list} -> {[], list, domain_keys}
        :error -> {[], [], domain_keys}
      end

    {pos_neg_domain, gradual?(key_type), context}
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
  def struct_instance(struct, args, expected, meta, stack, context, of_fun)
      when is_atom(struct) do
    {_info, context} = struct_info(struct, meta, stack, context)

    # The compiler has already checked the keys are atoms and which ones are required.
    {args_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context when is_atom(key) ->
        value_type =
          case map_fetch_key(expected, key) do
            {_, expected_value_type} -> expected_value_type
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

  ## Bitstrings

  @doc """
  Handles bitstrings.

  In the stack, we add nodes such as <<expr>>, <<..., expr>>, etc,
  based on the position of the expression within the binary.
  """
  def bitstring([], _kind, _stack, context) do
    {binary(), context}
  end

  def bitstring([head], kind, stack, context) do
    {alignment, context} = bitstring_segment(head, kind, [head], stack, context)
    {alignment_to_type(alignment), context}
  end

  def bitstring([head | tail], kind, stack, context) do
    {alignment, context} = bitstring_segment(head, kind, [head, @suffix], stack, context)
    bitstring_tail(tail, alignment, kind, stack, context)
  end

  defp bitstring_tail([last], alignment, kind, stack, context) do
    {seg_alignment, context} = bitstring_segment(last, kind, [@prefix, last], stack, context)
    {alignment_to_type(alignment(seg_alignment, alignment)), context}
  end

  defp bitstring_tail([head | tail], alignment, kind, stack, context) do
    {seg_alignment, context} =
      bitstring_segment(head, kind, [@prefix, head, @suffix], stack, context)

    bitstring_tail(tail, alignment(seg_alignment, alignment), kind, stack, context)
  end

  defp alignment(left, right) when is_integer(left) and is_integer(right), do: left + right
  defp alignment(_left, _right), do: :unknown

  defp alignment_to_type(:unknown), do: bitstring()
  defp alignment_to_type(integer) when rem(integer, 8) == 0, do: binary()
  defp alignment_to_type(_integer), do: bitstring_no_binary()

  # If the segment is a literal, the compiler has already checked its validity,
  # so we just check the size.
  defp bitstring_segment({:"::", _meta, [left, right]}, kind, _args, stack, context)
       when is_binary(left) or is_number(left) do
    {_type, alignment_type} = specifier_type(kind, right)
    {alignment_value, context} = specifier_size(kind, right, stack, {:default, context})

    # We don't need to check for bitstrings because the left side
    # is either a binary (aligned), float (aligned), or integer
    # (which we check below).
    if alignment_type == :integer and alignment_value != :default do
      {alignment_value, context}
    else
      {0, context}
    end
  end

  defp bitstring_segment({:"::", meta, [left, right]}, kind, args, stack, context) do
    {type, alignment_type} = specifier_type(kind, right)
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
      {alignment_value, context} = specifier_size(kind, right, stack, {:default, context})

      case alignment_type do
        :aligned ->
          {0, context}

        :integer when alignment_value == :default ->
          {0, context}

        # There is no size, so the aligment depends on the type.
        # If the type is exclusively a binary, then it is aligned.
        :bitstring when alignment_value == :default ->
          if bitstring_no_binary_type?(actual), do: {:unknown, context}, else: {0, context}

        _ ->
          {alignment_value, context}
      end
    else
      error = {:badbinary, kind, meta, expr, type, actual, context}
      {:unknown, error(error, meta, stack, context)}
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
  defp specifier_type(:match, {:utf8, _, _}), do: {@integer, :aligned}
  defp specifier_type(:match, {:utf16, _, _}), do: {@integer, :aligned}
  defp specifier_type(:match, {:utf32, _, _}), do: {@integer, :aligned}
  defp specifier_type(:match, {:float, _, _}), do: {@float, :aligned}
  defp specifier_type(_kind, {:float, _, _}), do: {@integer_or_float, :aligned}
  defp specifier_type(_kind, {:utf8, _, _}), do: {@integer, :aligned}
  defp specifier_type(_kind, {:utf16, _, _}), do: {@integer, :aligned}
  defp specifier_type(_kind, {:utf32, _, _}), do: {@integer, :aligned}
  defp specifier_type(_kind, {:integer, _, _}), do: {@integer, :integer}
  defp specifier_type(_kind, {:bits, _, _}), do: {@bitstring, :bitstring}
  defp specifier_type(_kind, {:bitstring, _, _}), do: {@bitstring, :bitstring}
  defp specifier_type(_kind, {:bytes, _, _}), do: {@binary, :aligned}
  defp specifier_type(_kind, {:binary, _, _}), do: {@binary, :aligned}
  defp specifier_type(_kind, _specifier), do: {@integer, :integer}

  defp specifier_size(kind, {:-, _, [left, right]}, stack, align_context) do
    specifier_size(kind, right, stack, specifier_size(kind, left, stack, align_context))
  end

  defp specifier_size(_, {:size, _, [arg]}, _stack, {unit, context})
       when is_integer(arg) do
    size = if unit == :default, do: arg, else: arg * unit
    {size, context}
  end

  defp specifier_size(:expr, {:size, _, [arg]} = expr, stack, {_, context}) do
    {actual, context} = Module.Types.Expr.of_expr(arg, integer(), expr, stack, context)
    {:unknown, compatible_size(actual, expr, stack, context)}
  end

  defp specifier_size(_match_or_guard, {:size, _, [arg]} = expr, stack, {_, context}) do
    {actual, context} = Module.Types.Pattern.of_guard(arg, integer(), expr, stack, context)
    {:unknown, compatible_size(actual, expr, stack, context)}
  end

  # We currently assume the unit always comes before size
  defp specifier_size(_, {:unit, _, [unit]}, _stack, {:default, context}) do
    {unit, context}
  end

  defp specifier_size(_kind, _specifier, _stack, align_context) do
    align_context
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
      {:finite, mods} ->
        {mods, context}

      {:infinite, _} ->
        {[], context}

      :error ->
        warning = {:badmodule, expr, type, fun, arity, hints, context}
        {[], error(warning, meta, stack, context)}
    end
  end

  ## Warning

  defp error(warning, meta, stack, context) do
    error(__MODULE__, warning, meta, stack, context)
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
