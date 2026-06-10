# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types do
  @moduledoc false
  alias Module.Types.{Apply, Descr, Expr, Helpers, Pattern}

  # The mode controls what happens on function application when
  # there are gradual arguments. Non-gradual arguments always
  # perform subtyping and return its output (OUT).
  #
  #   * :strict - Requires types signatures (not implemented).
  #     * Strong arrows with gradual performs subtyping and returns OUT
  #     * Weak arrows with gradual performs subtyping and returns OUT
  #
  #   * :static - Type signatures have been given.
  #     * Strong arrows with gradual performs compatibility and returns OUT
  #     * Weak arrows with gradual performs compatibility and returns dynamic()
  #
  #   * :dynamic - Type signatures have not been given.
  #     * Strong arrows with gradual performs compatibility and returns dynamic(OUT)
  #     * Weak arrows with gradual performs compatibility and returns dynamic()
  #
  #   * :infer - Same as :dynamic but skips remote calls.
  #
  # The mode may also control exhaustiveness checks in the future (to be decided).
  # We may also want for applications with subtyping in dynamic mode to always
  # intersect with dynamic, but this mode may be too lax (to be decided based on
  # feedback).
  @modes [:static, :dynamic, :infer]

  # These functions are not inferred because they are added/managed by the compiler
  @no_infer [behaviour_info: 1]

  @doc false
  def infer(module, file, attrs, defs, used_private, env, {_, cache}) do
    # We don't care about inferring signatures for protocols,
    # those will be replaced anyway. There is also nothing to
    # infer if there is no cache system, we only do traversals.
    infer_signatures? =
      :elixir_config.get(:infer_signatures) != false and cache != nil and not protocol?(attrs)

    impl = impl_for(attrs)

    finder =
      fn fun_arity ->
        case :lists.keyfind(fun_arity, 1, defs) do
          {_, kind, _, _} = def ->
            default_domain(infer_mode(kind, infer_signatures?), def, fun_arity, impl)

          false ->
            false
        end
      end

    handler = fn meta, fun_arity, stack, context ->
      case local_handler(meta, fun_arity, stack, context, finder) do
        false ->
          undefined_function!(:undefined_function, meta, fun_arity, stack, env)
          false

        {kind, _, _} = triplet ->
          if (kind == :defmacro or kind == :defmacrop) and not Keyword.has_key?(meta, :super) do
            undefined_function!(:incorrect_dispatch, meta, fun_arity, stack, env)
            false
          else
            triplet
          end
      end
    end

    stack = stack(:infer, file, module, {:__info__, 1}, env, cache, handler)

    # In case there are loops, the other we traverse matters,
    # so we sort the definitions for determinism
    {types, private, %{local_sigs: reachable_sigs} = context} =
      for {fun_arity, kind, meta, _clauses} = def <- Enum.sort(defs),
          reduce: {[], [], context()} do
        {types, private, context} when kind in [:def, :defmacro] ->
          # Optimized version of finder, since we already have the definition
          finder = fn _ ->
            default_domain(infer_mode(kind, infer_signatures?), def, fun_arity, impl)
          end

          {_kind, inferred, context} = local_handler(meta, fun_arity, stack, context, finder)

          if infer_signatures? and kind == :def and fun_arity not in @no_infer do
            {[{fun_arity, group_clauses_by_return(inferred)} | types], private, context}
          else
            {types, private, context}
          end

        {types, private, context} ->
          {types, [def | private], context}
      end

    # Now traverse all used privates to find any other private that have been used by them.
    context =
      %{local_sigs: used_sigs} =
      for fun_arity <- used_private, reduce: context do
        context ->
          {_kind, _inferred, context} = local_handler([], fun_arity, stack, context, finder)
          context
      end

    {unreachable, _context} =
      Enum.reduce(private, {[], context}, fn
        {fun_arity, kind, meta, _clauses}, {unreachable, context} ->
          warn_unused_def(fun_arity, kind, meta, used_sigs, env)

          # Find anything undefined within unused functions
          {_kind, _inferred, context} = local_handler([], fun_arity, stack, context, finder)

          # defp is reachable if used, defmacrop only if directly invoked
          private_sigs = if kind == :defp, do: used_sigs, else: reachable_sigs

          if is_map_key(private_sigs, fun_arity) do
            {unreachable, context}
          else
            {[fun_arity | unreachable], context}
          end
      end)

    {Map.new(types), unreachable}
  end

  defp infer_mode(kind, infer_signatures?) do
    if infer_signatures? and kind in [:def, :defp], do: :infer, else: :traverse
  end

  defp protocol?(attrs) do
    List.keymember?(attrs, :__protocol__, 0)
  end

  defp impl_for(attrs) do
    case List.keyfind(attrs, :__impl__, 0) do
      {:__impl__, [protocol: protocol, for: for]} ->
        if Code.ensure_loaded?(protocol) and function_exported?(protocol, :behaviour_info, 1) do
          {for, protocol.behaviour_info(:callbacks)}
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp default_domain(mode, def, {_, arity} = fun_arity, impl) do
    with {for, callbacks} <- impl,
         true <- fun_arity in callbacks do
      args = [
        Descr.dynamic(Module.Types.Of.impl(for))
        | List.duplicate(Descr.dynamic(), arity - 1)
      ]

      {_fun_arity, kind, meta, clauses} = def

      clauses =
        for {meta, args, guards, body} <- clauses do
          {[type_check: {:impl, for}] ++ meta, args, guards, body}
        end

      {mode, {fun_arity, kind, meta, clauses}, args}
    else
      _ -> {mode, def, List.duplicate(Descr.dynamic(), arity)}
    end
  end

  defp undefined_function!(reason, meta, {fun, arity}, stack, env) do
    env = %{env | function: stack.function, file: stack.file}
    tuple = {reason, {fun, arity}, stack.module}
    :elixir_errors.module_error(Helpers.with_span(meta, fun), env, __MODULE__, tuple)
  end

  defp warn_unused_def(fun_arity, kind, meta, used, env) do
    default = Keyword.get(meta, :defaults, 0)

    cond do
      Keyword.get(meta, :context) != nil or Keyword.get(meta, :from_super) == true ->
        :ok

      default == 0 ->
        case is_map_key(used, fun_arity) do
          true -> :ok
          false -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_def, fun_arity, kind})
        end

      default > 0 ->
        {name, arity} = fun_arity
        min = arity - default
        max = arity

        case min_reachable_default(max, min, :none, name, used) do
          :none -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_def, fun_arity, kind})
          ^min -> :ok
          ^max -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_args, fun_arity})
          diff -> :elixir_errors.file_warn(meta, env, __MODULE__, {:unused_args, fun_arity, diff})
        end
    end

    :ok
  end

  defp min_reachable_default(max, min, last, name, used) when max >= min do
    fun_arity = {name, max}

    case is_map_key(used, fun_arity) do
      true -> min_reachable_default(max - 1, min, max, name, used)
      false -> min_reachable_default(max - 1, min, last, name, used)
    end
  end

  defp min_reachable_default(_max, _min, last, _name, _used) do
    last
  end

  @doc false
  def warnings(module, file, attrs, defs, no_warn_undefined, cache) do
    impl = impl_for(attrs)

    finder = fn fun_arity ->
      case :lists.keyfind(fun_arity, 1, defs) do
        {_, _, _, _} = def -> default_domain(:dynamic, def, fun_arity, impl)
        false -> false
      end
    end

    handler = &local_handler(&1, &2, &3, &4, finder)
    stack = stack(:dynamic, file, module, {:__info__, 1}, no_warn_undefined, cache, handler)

    context =
      Enum.reduce(defs, context(), fn {fun_arity, _kind, meta, _clauses} = def, context ->
        # Optimized version of finder, since we already the definition
        finder = fn _ -> default_domain(:dynamic, def, fun_arity, impl) end
        {_kind, _inferred, context} = local_handler(meta, fun_arity, stack, context, finder)
        context
      end)

    context = warn_unused_clauses(defs, stack, context)
    context.warnings
  end

  defp warn_unused_clauses(defs, stack, context) do
    for {fun_arity, pending} <- context.local_used,
        pending != [],
        {_fun_arity, kind, meta, clauses} = List.keyfind(defs, fun_arity, 0),
        not Keyword.get(meta, :from_super, false),
        reduce: context do
      context ->
        {_kind, info, mapping} = Map.fetch!(context.local_sigs, fun_arity)

        if pending != [] do
          {used_indexes, unused_indexes} =
            Enum.reduce(mapping, {[], []}, fn {clause_index, type_index},
                                              {used_indexes, unused_indexes} ->
              if type_index in pending and not skip_unused_clause?(info, type_index) do
                {used_indexes, [clause_index | unused_indexes]}
              else
                {[clause_index | used_indexes], unused_indexes}
              end
            end)

          unused_indexes = Enum.uniq(unused_indexes) -- used_indexes

          Enum.reduce(unused_indexes, context, fn clause_index, context ->
            {meta, _args, _guards, _body} = Enum.fetch!(clauses, clause_index)
            stack = %{stack | function: fun_arity}
            Helpers.warn(__MODULE__, {:unused_clause, kind, fun_arity}, meta, stack, context)
          end)
        else
          context
        end
    end
  end

  defp skip_unused_clause?(info, type_index) do
    case info do
      # If an inferred clause returns an empty type, then the reverse arrow
      # will never propagate its domain up, which may lead to the clause never
      # being invoked.
      {:infer, _, inferred} ->
        {_args_types, return} = Enum.fetch!(inferred, type_index)
        Descr.empty?(return)

      _ ->
        false
    end
  end

  defp local_handler(_meta, fun_arity, stack, context, finder) do
    case context.local_sigs do
      %{^fun_arity => {kind, inferred, _mapping}} ->
        {kind, inferred, context}

      %{^fun_arity => kind} when is_atom(kind) ->
        {kind, :none, context}

      local_sigs ->
        case finder.(fun_arity) do
          {mode, {fun_arity, kind, meta, clauses}, expected} ->
            context = put_in(context.local_sigs, Map.put(local_sigs, fun_arity, kind))

            {inferred, mapping, context} =
              local_handler(mode, fun_arity, kind, meta, clauses, expected, stack, context)

            context =
              update_in(context.local_sigs, &Map.put(&1, fun_arity, {kind, inferred, mapping}))

            {kind, inferred, context}

          false ->
            false
        end
    end
  end

  defp local_handler(:traverse, {_, arity}, _kind, _meta, clauses, _expected, stack, context) do
    context =
      Enum.reduce(clauses, context, fn {_meta, _args, _guards, body}, context ->
        Module.Types.Traverse.of_expr(body, stack, context)
      end)

    inferred = {:infer, nil, [{List.duplicate(Descr.term(), arity), Descr.dynamic()}]}
    {inferred, [{0, 0}], context}
  end

  defp local_handler(mode, fun_arity, kind, meta, clauses, expected, stack, context) do
    {fun, _arity} = fun_arity
    stack = stack |> fresh_stack(mode, fun_arity) |> with_file_meta(meta)
    base_info = {:def, kind, fun, expected}

    case clauses do
      [{meta, args, [], {:super, _, [_ | _]} = body}] ->
        default_local_handler(meta, args, body, base_info, kind, fun, expected, stack, context)

      _ ->
        infer_local_handler(clauses, base_info, kind, fun, expected, stack, context)
    end
  end

  defp default_local_handler(meta, args, body, base_info, kind, fun, expected, stack, context) do
    guards = []
    previous = Pattern.init_previous()
    fresh_context = fresh_context(context)
    info = {base_info, args, guards}

    try do
      {trees, _, _, _, head_context} =
        Pattern.of_head(args, guards, expected, previous, info, meta, stack, fresh_context)

      # Compute the intersected arrows from the function call
      {:super, meta, call_args} = body
      {_kind, call_fun} = Keyword.fetch!(meta, :super)
      term = Descr.term()
      of_fun = &Expr.of_expr/5

      {arrows, body_context} =
        Apply.local_arrows(call_fun, call_args, term, body, stack, head_context, of_fun)

      # For each arrow, compute the default arrow
      {_, _, mapping, inferred} =
        Enum.reduce(arrows, {0, 0, [], []}, fn
          {clause_domain, return_type}, {index, total, mapping, inferred} ->
            of_fun = &Expr.of_expr(&1, &2, body, stack, &3)

            {_clause_args, clause_context} =
              Helpers.zip_map_reduce(call_args, clause_domain, head_context, of_fun)

            clause_types = Pattern.of_domain(trees, stack, clause_context)

            {type_index, inferred} =
              add_inferred(inferred, clause_types, return_type, total - 1, [])

            total = if type_index == -1, do: total + 1, else: total
            {index + 1, total, [{0, index} | mapping], inferred}
        end)

      domain =
        case inferred do
          [_] ->
            nil

          _ ->
            inferred
            |> Enum.map(fn {args, _} -> args end)
            |> Enum.zip_with(fn types -> Enum.reduce(types, &Descr.opt_union/2) end)
        end

      {{:infer, domain, Enum.reverse(inferred)}, mapping, restore_context(body_context, context)}
    rescue
      e ->
        internal_error!(e, __STACKTRACE__, kind, meta, fun, args, guards, body, stack)
    end
  end

  defp infer_local_handler(clauses, base_info, kind, fun, expected, stack, context) do
    {_, _, _, domain, mapping, clauses_types, clauses_context} =
      Enum.reduce(clauses, {0, 0, Pattern.init_previous(), [], [], [], context}, fn
        {meta, args, guards, body},
        {index, total, previous, domain, mapping, inferred, acc_context} ->
          fresh_context = fresh_context(acc_context)
          info = {base_info, args, guards}

          try do
            {trees, _precise?, head_no_previous_args_types, previous, head_context} =
              Pattern.of_head(args, guards, expected, previous, info, meta, stack, fresh_context)

            {return_type, context} =
              Expr.of_expr(body, Descr.term(), body, stack, head_context)

            args_types = Pattern.of_domain(trees, stack, context)

            {type_index, inferred} =
              add_inferred(inferred, args_types, return_type, total - 1, [])

            domain =
              case domain do
                [] ->
                  args_types

                _ ->
                  head_args_types = Pattern.of_domain(trees, stack, head_context)
                  compute_domain(args_types, head_args_types, head_no_previous_args_types, domain)
              end

            if type_index == -1 do
              mapping = [{index, total} | mapping]
              {index + 1, total + 1, previous, domain, mapping, inferred, context}
            else
              mapping = [{index, type_index} | mapping]
              {index + 1, total, previous, domain, mapping, inferred, context}
            end
          rescue
            e ->
              internal_error!(e, __STACKTRACE__, kind, meta, fun, args, guards, body, stack)
          end
      end)

    domain =
      case clauses_types do
        [_] -> nil
        _ -> domain
      end

    inferred = {:infer, domain, Enum.reverse(clauses_types)}
    {inferred, mapping, restore_context(clauses_context, context)}
  end

  defp compute_domain(
         [arg | args_types],
         [head_arg | head_args_types],
         [no_prev_arg | no_prev_args_types],
         [d | domain]
       ) do
    [
      # This is an optimization that broadens the domain, but it is acceptable
      # because the domain is used for reverse arrows and not type checking.
      #
      # The overall idea is that, if we have a function with three clauses,
      # the domain is computed by unioning their inferred types. However, their
      # inferred types often have the different of the previous clauses:
      #
      #     opt_union(r3 ^ (c3 - c2 - c1), r2 ^ (c2 - c1), r1 ^ c1)
      #
      # Where `rN` represents the refinement in every function body.
      #
      # What this function does is, if the type of a given arg in a clause
      # before and after the body is the same (meaning r3 is term), then
      # we replace all of `(c3 - c2 - c1)` by just `c3`, which removes
      # many of the differences in the node. However, keep in mind that,
      # because `r2` may have refine `c2` in the previous clause, the domain
      # may end-up being broader. Take this example:
      #
      #     % %{..., foo: integer()} -> binary()
      #     def example(%{foo: var}), do: Integer.to_string(var)
      #
      #     % %{...} and not %{..., foo: term()} -> :error
      #     def example(%{}), do: :error
      #
      # The actual domain is:
      #
      #     %{..., foo: not_set()} or %{..., foo: integer()}
      #     #=> %{..., foo: if_set(integer())}
      #
      # But we will infer:
      #
      #     %{...} or %{..., foo: integer()}
      #     #=> %{...}
      #
      # We lose precision but this is exactly what we want: to have simpler types.
      # Furthermore, the signature used in type checking is not refined in any way,
      # so type checking is still sound.
      if arg == head_arg do
        Descr.opt_union(Descr.upper_bound(no_prev_arg), d)
      else
        Descr.opt_union(arg, d)
      end
      | compute_domain(args_types, head_args_types, no_prev_args_types, domain)
    ]
  end

  defp compute_domain([], [], [], []), do: []

  # We check for term equality of types as an optimization
  # to reduce the amount of check we do at runtime.
  defp add_inferred([{args, existing_return} | tail], args, return, index, acc),
    do: {index, Enum.reverse(acc, [{args, Descr.opt_union(existing_return, return)} | tail])}

  defp add_inferred([head | tail], args, return, index, acc),
    do: add_inferred(tail, args, return, index - 1, [head | acc])

  defp add_inferred([], args, return, -1, acc),
    do: {-1, [{args, return} | Enum.reverse(acc)]}

  # Compact clauses that have the same return and differ in exactly one
  # argument by unioning that argument. For example:
  #
  #   (integer(), atom() -> boolean()) and (float(), atom() -> boolean())
  #
  # becomes:
  #
  #   (number(), atom() -> boolean())
  #
  # Arity-zero clauses have no argument position to widen.
  defp group_clauses_by_return({:infer, domain, [{[_ | _], _} | _] = clauses}) do
    clauses =
      Enum.reduce(clauses, [], fn {args, return}, acc ->
        group_clause_by_return(acc, args, return)
      end)

    {:infer, domain, clauses}
  end

  defp group_clauses_by_return(info), do: info

  defp group_clause_by_return([{existing_args, return} | tail], args, return) do
    case union_args(existing_args, args, [], false) do
      nil ->
        [{existing_args, return} | group_clause_by_return(tail, args, return)]

      new_args ->
        [{new_args, return} | tail]
    end
  end

  defp group_clause_by_return([head | tail], args, return) do
    [head | group_clause_by_return(tail, args, return)]
  end

  defp group_clause_by_return([], args, return), do: [{args, return}]

  defp union_args([arg | existing], [arg | args], acc, changed?) do
    union_args(existing, args, [arg | acc], changed?)
  end

  # Allow exactly one differing argument. That one position is widened
  # with union/2. A second difference means the clauses must stay separate.
  defp union_args([existing_arg | existing], [arg | args], acc, false) do
    union_args(existing, args, [Descr.opt_union(existing_arg, arg) | acc], true)
  end

  defp union_args([_ | _], [_ | _], _acc, true), do: nil

  # In theory fully equal args are merged on add_inferred
  defp union_args([], [], acc, _changed?), do: Enum.reverse(acc)

  defp with_file_meta(stack, meta) do
    case Keyword.fetch(meta, :file) do
      {:ok, {meta_file, _}} -> %{stack | file: meta_file}
      :error -> stack
    end
  end

  defp internal_error!(e, trace, kind, meta, fun, args, guards, body, stack) do
    def_expr = {kind, meta, [guards_to_expr(guards, {fun, [], args}), [do: body]]}

    exception =
      RuntimeError.exception("""
      found error while checking types for #{Exception.format_mfa(stack.module, fun, length(args))}:

      #{Exception.format_banner(:error, e, trace)}\

      The exception happened while checking this code:

      #{Macro.to_string(def_expr)}

      Please report this bug at: https://github.com/elixir-lang/elixir/issues
      """)

    reraise exception, trace
  end

  defp guards_to_expr([], left) do
    left
  end

  defp guards_to_expr([guard | guards], left) do
    guards_to_expr(guards, {:when, [], [left, guard]})
  end

  @doc false
  def stack(mode, file, module, function, no_warn_undefined, cache, handler)
      when mode in @modes do
    %{
      # The fallback meta used for literals in patterns and guards
      meta: [],
      # File of module
      file: file,
      # Module of definitions
      module: module,
      # Current function
      function: function,
      # Handling of undefined remote calls. May be one of:
      #
      # * List of calls to not warn on as undefined
      #
      # * The atom `:all` to not mark anything as undefined
      #
      # * A Macro.Env struct to not mark anything as undefined
      #   also used to extract structs from
      #
      no_warn_undefined: no_warn_undefined,
      # A tuple with cache information (may be nil)
      cache: cache,
      # The mode to be used, see the @modes attribute
      mode: mode,
      # The function for handling local calls
      local_handler: handler,
      # Reverse arrow handling (nil | :cache | :except_none | :include_none)
      reverse_arrow: nil
    }
  end

  @doc false
  def context() do
    %{
      # A list of all warnings found so far
      warnings: [],
      # All vars and their types
      vars: %{},
      # Stores special metadata used by list heads and domain keys in patterns
      subpatterns: %{},
      # Variables that are specific to the current environment/conditional
      conditional_vars: nil,
      # Track metadata specific to patterns and guards
      pattern_info: nil,
      # If type checking has found an error/failure
      failed: false,
      # Local signatures used by local handler
      local_sigs: %{},
      # Track which clauses have been used across private local calls
      local_used: %{},
      # Cached reverse arrows
      reverse_arrows: %{}
    }
  end

  defp fresh_stack(stack, mode, function) when mode in @modes do
    %{stack | mode: mode, function: function, reverse_arrow: nil}
  end

  defp fresh_context(context) do
    %{context | vars: %{}, failed: false, reverse_arrows: %{}}
  end

  defp restore_context(later_context, %{
         vars: vars,
         failed: failed,
         reverse_arrows: reverse_arrows
       }) do
    %{later_context | vars: vars, failed: failed, reverse_arrows: reverse_arrows}
  end

  ## Diagnostics

  def format_diagnostic({:unused_clause, kind, {fun, arity}}) do
    %{
      message:
        "this clause of #{kind} #{fun}/#{arity} is never used (or it will always fail/warn when invoked)"
    }
  end

  ## Module errors

  def format_error({:unused_args, {name, arity}}),
    do:
      "default values for the optional arguments in the private function #{name}/#{arity} are never used"

  def format_error({:unused_args, {name, arity}, count}) when arity - count == 1,
    do:
      "the default value for the last optional argument in the private function #{name}/#{arity} is never used"

  def format_error({:unused_args, {name, arity}, count}),
    do:
      "the default values for the last #{arity - count} optional arguments in the private function #{name}/#{arity} are never used"

  def format_error({:unused_def, {name, arity}, :defp}),
    do: "function #{name}/#{arity} is unused"

  def format_error({:unused_def, {name, arity}, :defmacrop}),
    do: "macro #{name}/#{arity} is unused"

  def format_error({:undefined_function, {f, a}, _})
      when {f, a} in [__info__: 1, behaviour_info: 1, module_info: 1, module_info: 0],
      do:
        "undefined function #{f}/#{a} (this function is auto-generated by the compiler and must always be called as a remote, as in __MODULE__.#{f}/#{a})"

  def format_error({:undefined_function, {f, a}, module}),
    do:
      "undefined function #{f}/#{a} (expected #{inspect(module)} to define such a function or for it to be imported, but none are available)"

  def format_error({:incorrect_dispatch, {f, a}, _module}),
    do: "cannot invoke macro #{f}/#{a} before its definition"
end
