# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule Module.Types do
  @moduledoc false
  alias Module.Types.{Descr, Expr, Pattern, Helpers}

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
            {[{fun_arity, inferred} | types], private, context}
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

        clauses_indexes =
          for type_index <- pending,
              not skip_unused_clause?(info, type_index),
              {clause_index, ^type_index} <- mapping,
              do: clause_index

        Enum.reduce(clauses_indexes, context, fn clause_index, context ->
          {meta, _args, _guards, _body} = Enum.fetch!(clauses, clause_index)
          stack = %{stack | function: fun_arity}
          Helpers.warn(__MODULE__, {:unused_clause, kind, fun_arity}, meta, stack, context)
        end)
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

    {_, _, mapping, clauses_types, clauses_context} =
      Enum.reduce(clauses, {0, 0, [], [], context}, fn
        {meta, args, guards, body}, {index, total, mapping, inferred, context} ->
          context = fresh_context(context)

          try do
            {trees, context} =
              Pattern.of_head(args, guards, expected, {:infer, expected}, meta, stack, context)

            {return_type, context} =
              Expr.of_expr(body, Descr.term(), body, stack, context)

            args_types = Pattern.of_domain(trees, stack, context)

            {type_index, inferred} =
              add_inferred(inferred, args_types, return_type, total - 1, [])

            if type_index == -1 do
              {index + 1, total + 1, [{index, total} | mapping], inferred, context}
            else
              {index + 1, total, [{index, type_index} | mapping], inferred, context}
            end
          rescue
            e ->
              internal_error!(e, __STACKTRACE__, kind, meta, fun, args, guards, body, stack)
          end
      end)

    domain =
      case clauses_types do
        [_] ->
          nil

        _ ->
          clauses_types
          |> Enum.map(fn {args, _} -> args end)
          |> Enum.zip_with(fn types -> Enum.reduce(types, &Descr.union/2) end)
      end

    inferred = {:infer, domain, Enum.reverse(clauses_types)}
    {inferred, mapping, restore_context(clauses_context, context)}
  end

  # We check for term equality of types as an optimization
  # to reduce the amount of check we do at runtime.
  defp add_inferred([{args, existing_return} | tail], args, return, index, acc),
    do: {index, Enum.reverse(acc, [{args, Descr.union(existing_return, return)} | tail])}

  defp add_inferred([head | tail], args, return, index, acc),
    do: add_inferred(tail, args, return, index - 1, [head | acc])

  defp add_inferred([], args, return, -1, acc),
    do: {-1, [{args, return} | Enum.reverse(acc)]}

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
      local_handler: handler
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
      # Track metadata specific to matches and guards
      pattern_info: nil,
      # If type checking has found an error/failure
      failed: false,
      # Local signatures used by local handler
      local_sigs: %{},
      # Track which clauses have been used across private local calls
      local_used: %{}
    }
  end

  defp fresh_stack(stack, mode, function) when mode in @modes do
    %{stack | mode: mode, function: function}
  end

  defp fresh_context(context) do
    %{context | vars: %{}, failed: false}
  end

  defp restore_context(later_context, %{vars: vars, failed: failed}) do
    %{later_context | vars: vars, failed: failed}
  end

  ## Diagnostics

  def format_diagnostic({:unused_clause, kind, {fun, arity}}) do
    %{
      message: "this clause of #{kind} #{fun}/#{arity} is never used"
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
