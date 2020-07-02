defmodule Kernel.Typespec do
  @moduledoc false

  ## Deprecated API moved to Code.Typespec

  @doc false
  @deprecated "Use Code.Typespec.spec_to_quoted/2 instead"
  def spec_to_ast(name, spec) do
    Code.Typespec.spec_to_quoted(name, spec)
  end

  @doc false
  @deprecated "Use Code.Typespec.type_to_quoted/1 instead"
  def type_to_ast(type) do
    Code.Typespec.type_to_quoted(type)
  end

  @doc false
  @deprecated "Use Code.fetch_docs/1 instead"
  def beam_typedocs(module) when is_atom(module) or is_binary(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, _, _, _, docs} ->
        for {{:type, name, arity}, _, _, doc, _} <- docs do
          case doc do
            %{"en" => doc_string} -> {{name, arity}, doc_string}
            :none -> {{name, arity}, nil}
            # Hidden or unknown format are ignored
            _ -> {{name, arity}, false}
          end
        end

      {:error, _} ->
        nil
    end
  end

  @doc false
  @deprecated "Use Code.Typespec.fetch_types/1 instead"
  def beam_types(module) when is_atom(module) or is_binary(module) do
    case Code.Typespec.fetch_types(module) do
      {:ok, types} -> types
      :error -> nil
    end
  end

  @doc false
  @deprecated "Use Code.Typespec.fetch_specs/1 instead"
  def beam_specs(module) when is_atom(module) or is_binary(module) do
    case Code.Typespec.fetch_specs(module) do
      {:ok, specs} -> specs
      :error -> nil
    end
  end

  @doc false
  @deprecated "Use Code.Typespec.fetch_callbacks/1 instead"
  def beam_callbacks(module) when is_atom(module) or is_binary(module) do
    case Code.Typespec.fetch_callbacks(module) do
      {:ok, callbacks} -> callbacks
      :error -> nil
    end
  end

  ## Hooks for Module functions

  def defines_type?(module, {name, arity} = signature)
      when is_atom(module) and is_atom(name) and arity in 0..255 do
    {_set, bag} = :elixir_module.data_tables(module)

    finder = fn {_kind, expr, _caller} ->
      type_to_signature(expr) == signature
    end

    :lists.any(finder, get_typespecs(bag, [:type, :opaque, :typep]))
  end

  def spec_to_callback(module, {name, arity} = signature)
      when is_atom(module) and is_atom(name) and arity in 0..255 do
    {set, bag} = :elixir_module.data_tables(module)

    filter = fn {:spec, expr, pos} ->
      if spec_to_signature(expr) == signature do
        kind = :callback
        store_typespec(bag, kind, expr, pos)

        case :ets.lookup(set, {:function, name, arity}) do
          [{{:function, ^name, ^arity}, _, line, _, doc, doc_meta}] ->
            store_doc(set, kind, name, arity, line, :doc, doc, doc_meta)

          _ ->
            nil
        end

        true
      else
        false
      end
    end

    :lists.filter(filter, get_typespecs(bag, :spec)) != []
  end

  ## Typespec definition and storage

  @doc """
  Defines a typespec.

  Invoked by `Kernel.@/1` expansion.
  """
  def deftypespec(:spec, expr, _line, _file, module, pos) do
    {_set, bag} = :elixir_module.data_tables(module)
    store_typespec(bag, :spec, expr, pos)
  end

  def deftypespec(kind, expr, line, _file, module, pos)
      when kind in [:callback, :macrocallback] do
    {set, bag} = :elixir_module.data_tables(module)

    case spec_to_signature(expr) do
      {name, arity} ->
        # store doc only once in case callback has multiple clauses
        unless :ets.member(set, {kind, name, arity}) do
          {line, doc} = get_doc_info(set, :doc, line)
          store_doc(set, kind, name, arity, line, :doc, doc, %{})
        end

      :error ->
        :error
    end

    store_typespec(bag, kind, expr, pos)
  end

  def deftypespec(kind, expr, line, file, module, pos)
      when kind in [:type, :typep, :opaque] do
    {set, bag} = :elixir_module.data_tables(module)

    case type_to_signature(expr) do
      {name, arity} when kind == :typep ->
        {line, doc} = get_doc_info(set, :typedoc, line)

        if doc do
          warning =
            "type #{name}/#{arity} is private, @typedoc's are always discarded for private types"

          :elixir_errors.erl_warn(line, file, warning)
        end

      {name, arity} ->
        {line, doc} = get_doc_info(set, :typedoc, line)
        spec_meta = if kind == :opaque, do: %{opaque: true}, else: %{}
        store_doc(set, :type, name, arity, line, :typedoc, doc, spec_meta)

      :error ->
        :error
    end

    store_typespec(bag, kind, expr, pos)
  end

  defp get_typespecs(bag, keys) when is_list(keys) do
    :lists.flatmap(&get_typespecs(bag, &1), keys)
  end

  defp get_typespecs(bag, key) do
    :ets.lookup_element(bag, {:accumulate, key}, 2)
  catch
    :error, :badarg -> []
  end

  defp take_typespecs(bag, keys) when is_list(keys) do
    :lists.flatmap(&take_typespecs(bag, &1), keys)
  end

  defp take_typespecs(bag, key) do
    :lists.map(&elem(&1, 1), :ets.take(bag, {:accumulate, key}))
  end

  defp store_typespec(bag, key, expr, pos) do
    :ets.insert(bag, {{:accumulate, key}, {key, expr, pos}})
    :ok
  end

  defp store_doc(set, kind, name, arity, line, doc_kind, doc, spec_meta) do
    doc_meta = get_doc_meta(spec_meta, doc_kind, set)
    :ets.insert(set, {{kind, name, arity}, line, doc, doc_meta})
  end

  defp get_doc_info(set, attr, line) do
    case :ets.take(set, attr) do
      [{^attr, {line, doc}, _}] -> {line, doc}
      [] -> {line, nil}
    end
  end

  defp get_doc_meta(spec_meta, doc_kind, set) do
    case :ets.take(set, {doc_kind, :meta}) do
      [{{^doc_kind, :meta}, metadata, _}] -> Map.merge(metadata, spec_meta)
      [] -> spec_meta
    end
  end

  defp spec_to_signature({:when, _, [spec, _]}), do: type_to_signature(spec)
  defp spec_to_signature(other), do: type_to_signature(other)

  defp type_to_signature({:"::", _, [{name, _, context}, _]})
       when is_atom(name) and name != :"::" and is_atom(context),
       do: {name, 0}

  defp type_to_signature({:"::", _, [{name, _, args}, _]}) when is_atom(name) and name != :"::",
    do: {name, length(args)}

  defp type_to_signature(_), do: :error

  ## Translation from Elixir AST to typespec AST

  @doc false
  def translate_typespecs_for_module(_set, bag) do
    type_typespecs = take_typespecs(bag, [:type, :opaque, :typep])
    defined_type_pairs = collect_defined_type_pairs(type_typespecs)

    state = %{
      defined_type_pairs: defined_type_pairs,
      used_type_pairs: [],
      local_vars: %{},
      undefined_type_error_enabled?: true
    }

    {types, state} = :lists.mapfoldl(&translate_type/2, state, type_typespecs)
    {specs, state} = :lists.mapfoldl(&translate_spec/2, state, take_typespecs(bag, :spec))
    {callbacks, state} = :lists.mapfoldl(&translate_spec/2, state, take_typespecs(bag, :callback))

    {macrocallbacks, state} =
      :lists.mapfoldl(&translate_spec/2, state, take_typespecs(bag, :macrocallback))

    optional_callbacks = :lists.flatten(get_typespecs(bag, :optional_callbacks))
    used_types = filter_used_types(types, state)

    {used_types, specs, callbacks, macrocallbacks, optional_callbacks}
  end

  defp collect_defined_type_pairs(type_typespecs) do
    fun = fn {_kind, expr, pos}, type_pairs ->
      %{file: file, line: line} = env = :elixir_locals.get_cached_env(pos)

      case type_to_signature(expr) do
        {name, arity} = type_pair ->
          if built_in_type?(name, arity) do
            message = "type #{name}/#{arity} is a built-in type and it cannot be redefined"
            compile_error(env, message)
          end

          if Map.has_key?(type_pairs, type_pair) do
            {error_full_path, error_line} = type_pairs[type_pair]
            error_relative_path = Path.relative_to_cwd(error_full_path)

            compile_error(
              env,
              "type #{name}/#{arity} is already defined in #{error_relative_path}:#{error_line}"
            )
          end

          Map.put(type_pairs, type_pair, {file, line})

        :error ->
          compile_error(env, "invalid type specification: #{Macro.to_string(expr)}")
      end
    end

    :lists.foldl(fun, %{}, type_typespecs)
  end

  defp filter_used_types(types, state) do
    fun = fn {_kind, {name, arity} = type_pair, _line, _type, export} ->
      if not export and not :lists.member(type_pair, state.used_type_pairs) do
        %{^type_pair => {file, line}} = state.defined_type_pairs
        :elixir_errors.erl_warn(line, file, "type #{name}/#{arity} is unused")
        false
      else
        true
      end
    end

    :lists.filter(fun, types)
  end

  defp translate_type({kind, {:"::", _, [{name, _, args}, definition]}, pos}, state) do
    caller = :elixir_locals.get_cached_env(pos)
    state = clean_local_state(state)

    args =
      if is_atom(args) do
        []
      else
        :lists.map(&variable/1, args)
      end

    vars = :lists.filter(&match?({:var, _, _}, &1), args)
    var_names = :lists.map(&elem(&1, 2), vars)
    state = :lists.foldl(&update_local_vars(&2, &1), state, var_names)
    {spec, state} = typespec(definition, var_names, caller, state)
    type = {name, spec, vars}
    arity = length(args)

    ensure_no_underscore_local_vars!(caller, var_names)
    ensure_no_unused_local_vars!(caller, state.local_vars)

    {kind, export} =
      case kind do
        :type -> {:type, true}
        :typep -> {:type, false}
        :opaque -> {:opaque, true}
      end

    invalid_args = :lists.filter(&(not valid_variable_ast?(&1)), args)

    unless invalid_args == [] do
      invalid_args = :lists.join(", ", :lists.map(&Macro.to_string/1, invalid_args))

      message =
        "@type definitions expect all arguments to be variables. The type " <>
          "#{name}/#{arity} has an invalid argument(s): #{invalid_args}"

      compile_error(caller, message)
    end

    if underspecified?(kind, arity, spec) do
      message = "@#{kind} type #{name}/#{arity} is underspecified and therefore meaningless"
      :elixir_errors.erl_warn(caller.line, caller.file, message)
    end

    {{kind, {name, arity}, caller.line, type, export}, state}
  end

  defp valid_variable_ast?({variable_name, _, context})
       when is_atom(variable_name) and is_atom(context),
       do: true

  defp valid_variable_ast?(_), do: false

  defp underspecified?(:opaque, 0, {:type, _, type, []}) when type in [:any, :term], do: true
  defp underspecified?(_kind, _arity, _spec), do: false

  defp translate_spec({kind, {:when, _meta, [spec, guard]}, pos}, state) do
    caller = :elixir_locals.get_cached_env(pos)
    translate_spec(kind, spec, guard, caller, state)
  end

  defp translate_spec({kind, spec, pos}, state) do
    caller = :elixir_locals.get_cached_env(pos)
    translate_spec(kind, spec, [], caller, state)
  end

  defp translate_spec(kind, {:"::", meta, [{name, _, args}, return]}, guard, caller, state)
       when is_atom(name) and name != :"::" do
    translate_spec(kind, meta, name, args, return, guard, caller, state)
  end

  defp translate_spec(_kind, {name, _meta, _args} = spec, _guard, caller, _state)
       when is_atom(name) and name != :"::" do
    spec = Macro.to_string(spec)
    compile_error(caller, "type specification missing return type: #{spec}")
  end

  defp translate_spec(_kind, spec, _guard, caller, _state) do
    spec = Macro.to_string(spec)
    compile_error(caller, "invalid type specification: #{spec}")
  end

  defp translate_spec(kind, meta, name, args, return, guard, caller, state) when is_atom(args),
    do: translate_spec(kind, meta, name, [], return, guard, caller, state)

  defp translate_spec(kind, meta, name, args, return, guard, caller, state) do
    ensure_no_defaults!(args)
    state = clean_local_state(state)

    unless Keyword.keyword?(guard) do
      error = "expected keywords as guard in type specification, got: #{Macro.to_string(guard)}"
      compile_error(caller, error)
    end

    line = line(meta)
    vars = Keyword.keys(guard)
    {fun_args, state} = fn_args(meta, args, return, vars, caller, state)
    spec = {:type, line, :fun, fun_args}

    {spec, state} =
      case guard_to_constraints(guard, vars, meta, caller, state) do
        {[], state} -> {spec, state}
        {constraints, state} -> {{:type, line, :bounded_fun, [spec, constraints]}, state}
      end

    ensure_no_unused_local_vars!(caller, state.local_vars)

    arity = length(args)
    {{kind, {name, arity}, caller.line, spec}, state}
  end

  # TODO: Remove char_list type by v2.0
  defp built_in_type?(:char_list, 0), do: true
  defp built_in_type?(:charlist, 0), do: true
  defp built_in_type?(:as_boolean, 1), do: true
  defp built_in_type?(:struct, 0), do: true
  defp built_in_type?(:nonempty_charlist, 0), do: true
  defp built_in_type?(:keyword, 0), do: true
  defp built_in_type?(:keyword, 1), do: true
  defp built_in_type?(:var, 0), do: true
  defp built_in_type?(name, arity), do: :erl_internal.is_type(name, arity)

  defp ensure_no_defaults!(args) do
    fun = fn
      {:"::", _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left

      other ->
        ensure_not_default(other)
        other
    end

    :lists.foreach(fun, args)
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in typespecs"
  end

  defp ensure_not_default(_), do: :ok

  defp guard_to_constraints(guard, vars, meta, caller, state) do
    line = line(meta)

    fun = fn
      {_name, {:var, _, context}}, {constraints, state} when is_atom(context) ->
        {constraints, state}

      {name, type}, {constraints, state} ->
        {spec, state} = typespec(type, vars, caller, state)
        constraint = [{:atom, line, :is_subtype}, [{:var, line, name}, spec]]
        state = update_local_vars(state, name)
        {[{:type, line, :constraint, constraint} | constraints], state}
    end

    {constraints, state} = :lists.foldl(fun, {[], state}, guard)
    {:lists.reverse(constraints), state}
  end

  ## To typespec conversion

  defp line(meta) do
    Keyword.get(meta, :line, 0)
  end

  # Handle unions
  defp typespec({:|, meta, [_, _]} = exprs, vars, caller, state) do
    exprs = collect_union(exprs)
    {union, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, exprs)
    {{:type, line(meta), :union, union}, state}
  end

  # Handle binaries
  defp typespec({:<<>>, meta, []}, _, _, state) do
    line = line(meta)
    {{:type, line, :binary, [{:integer, line, 0}, {:integer, line, 0}]}, state}
  end

  defp typespec(
         {:<<>>, meta, [{:"::", unit_meta, [{:_, _, ctx1}, {:*, _, [{:_, _, ctx2}, unit]}]}]},
         _,
         _,
         state
       )
       when is_atom(ctx1) and is_atom(ctx2) and is_integer(unit) and unit >= 0 do
    line = line(meta)
    {{:type, line, :binary, [{:integer, line, 0}, {:integer, line(unit_meta), unit}]}, state}
  end

  defp typespec({:<<>>, meta, [{:"::", size_meta, [{:_, _, ctx}, size]}]}, _, _, state)
       when is_atom(ctx) and is_integer(size) and size >= 0 do
    line = line(meta)
    {{:type, line, :binary, [{:integer, line(size_meta), size}, {:integer, line, 0}]}, state}
  end

  defp typespec(
         {
           :<<>>,
           meta,
           [
             {:"::", size_meta, [{:_, _, ctx1}, size]},
             {:"::", unit_meta, [{:_, _, ctx2}, {:*, _, [{:_, _, ctx3}, unit]}]}
           ]
         },
         _,
         _,
         state
       )
       when is_atom(ctx1) and is_atom(ctx2) and is_atom(ctx3) and is_integer(size) and
              is_integer(unit) and size >= 0 and unit >= 0 do
    args = [{:integer, line(size_meta), size}, {:integer, line(unit_meta), unit}]
    {{:type, line(meta), :binary, args}, state}
  end

  defp typespec({:<<>>, _meta, _args}, _vars, caller, _state) do
    message =
      "invalid binary specification, expected <<_::size>>, <<_::_*unit>>, " <>
        "or <<_::size, _::_*unit>> with size and unit being non-negative integers"

    compile_error(caller, message)
  end

  ## Handle maps and structs
  defp typespec({:map, meta, args}, _vars, _caller, state) when args == [] or is_atom(args) do
    {{:type, line(meta), :map, :any}, state}
  end

  defp typespec({:%{}, meta, fields} = map, vars, caller, state) do
    fun = fn
      {{:required, meta2, [k]}, v}, state ->
        {arg1, state} = typespec(k, vars, caller, state)
        {arg2, state} = typespec(v, vars, caller, state)
        {{:type, line(meta2), :map_field_exact, [arg1, arg2]}, state}

      {{:optional, meta2, [k]}, v}, state ->
        {arg1, state} = typespec(k, vars, caller, state)
        {arg2, state} = typespec(v, vars, caller, state)
        {{:type, line(meta2), :map_field_assoc, [arg1, arg2]}, state}

      {k, v}, state ->
        {arg1, state} = typespec(k, vars, caller, state)
        {arg2, state} = typespec(v, vars, caller, state)
        {{:type, line(meta), :map_field_exact, [arg1, arg2]}, state}

      {:|, _, [_, _]}, _state ->
        error =
          "invalid map specification. When using the | operator in the map key, " <>
            "make sure to wrap the key type in parentheses: #{Macro.to_string(map)}"

        compile_error(caller, error)

      _, _state ->
        compile_error(caller, "invalid map specification: #{Macro.to_string(map)}")
    end

    {fields, state} = :lists.mapfoldl(fun, state, fields)
    {{:type, line(meta), :map, fields}, state}
  end

  defp typespec({:%, _, [name, {:%{}, meta, fields}]}, vars, caller, state) do
    # We cannot set a function name to avoid tracking
    # as a compile time dependency, because for structs it actually is one.
    module = Macro.expand(name, caller)

    struct =
      module
      |> Macro.struct!(caller)
      |> Map.delete(:__struct__)
      |> Map.to_list()

    unless Keyword.keyword?(fields) do
      compile_error(caller, "expected key-value pairs in struct #{Macro.to_string(name)}")
    end

    types =
      :lists.map(
        fn {field, _} -> {field, Keyword.get(fields, field, quote(do: term()))} end,
        :lists.sort(struct)
      )

    fun = fn {field, _} ->
      unless Keyword.has_key?(struct, field) do
        compile_error(
          caller,
          "undefined field #{inspect(field)} on struct #{Macro.to_string(name)}"
        )
      end
    end

    :lists.foreach(fun, fields)
    typespec({:%{}, meta, [__struct__: module] ++ types}, vars, caller, state)
  end

  # Handle records
  defp typespec({:record, meta, [atom]}, vars, caller, state) do
    typespec({:record, meta, [atom, []]}, vars, caller, state)
  end

  defp typespec({:record, meta, [tag, field_specs]}, vars, caller, state)
       when is_atom(tag) and is_list(field_specs) do
    # We cannot set a function name to avoid tracking
    # as a compile time dependency because for records it actually is one.
    case Macro.expand({tag, [], [{:{}, [], []}]}, caller) do
      {_, _, [name, fields | _]} when is_list(fields) ->
        types =
          :lists.map(
            fn {field, _} ->
              {:"::", [],
               [
                 {field, [], nil},
                 Keyword.get(field_specs, field, quote(do: term()))
               ]}
            end,
            fields
          )

        fun = fn {field, _} ->
          unless Keyword.has_key?(fields, field) do
            compile_error(caller, "undefined field #{field} on record #{inspect(tag)}")
          end
        end

        :lists.foreach(fun, field_specs)
        typespec({:{}, meta, [name | types]}, vars, caller, state)

      _ ->
        compile_error(caller, "unknown record #{inspect(tag)}")
    end
  end

  defp typespec({:record, _meta, [_tag, _field_specs]}, _vars, caller, _state) do
    message = "invalid record specification, expected the record name to be an atom literal"
    compile_error(caller, message)
  end

  # Handle ranges
  defp typespec({:.., meta, [left, right]}, vars, caller, state) do
    {left, state} = typespec(left, vars, caller, state)
    {right, state} = typespec(right, vars, caller, state)
    :ok = validate_range(left, right, caller)

    {{:type, line(meta), :range, [left, right]}, state}
  end

  # Handle special forms
  defp typespec({:__MODULE__, _, atom}, vars, caller, state) when is_atom(atom) do
    typespec(caller.module, vars, caller, state)
  end

  defp typespec({:__aliases__, _, _} = alias, vars, caller, state) do
    # We set a function name to avoid tracking
    # aliases in typespecs as compile time dependencies.
    atom = Macro.expand(alias, %{caller | function: {:typespec, 0}})
    typespec(atom, vars, caller, state)
  end

  # Handle funs
  defp typespec([{:->, meta, [args, return]}], vars, caller, state)
       when is_list(args) do
    {args, state} = fn_args(meta, args, return, vars, caller, state)
    {{:type, line(meta), :fun, args}, state}
  end

  # Handle type operator
  defp typespec(
         {:"::", meta, [{var_name, var_meta, context}, expr]} = ann_type,
         vars,
         caller,
         state
       )
       when is_atom(var_name) and is_atom(context) do
    case typespec(expr, vars, caller, state) do
      {{:ann_type, _, _}, _state} ->
        message =
          "invalid type annotation. Type annotations cannot be nested: " <>
            "#{Macro.to_string(ann_type)}"

        # TODO: Make this an error on v2.0 and remove the code below
        :elixir_errors.erl_warn(caller.line, caller.file, message)

        # This may be generating an invalid typespec but we need to generate it
        # to avoid breaking existing code that was valid but only broke Dialyzer
        {right, state} = typespec(expr, vars, caller, state)
        {{:ann_type, line(meta), [{:var, line(var_meta), var_name}, right]}, state}

      {right, state} ->
        {{:ann_type, line(meta), [{:var, line(var_meta), var_name}, right]}, state}
    end
  end

  defp typespec({:"::", meta, [left, right]} = expr, vars, caller, state) do
    message =
      "invalid type annotation. When using the | operator to represent the union of types, " <>
        "make sure to wrap type annotations in parentheses: #{Macro.to_string(expr)}"

    # TODO: Make this an error on v2.0, and remove the code below and
    # the :undefined_type_error_enabled? key from the state
    :elixir_errors.erl_warn(caller.line, caller.file, message)

    # This may be generating an invalid typespec but we need to generate it
    # to avoid breaking existing code that was valid but only broke Dialyzer
    state = %{state | undefined_type_error_enabled?: false}
    {left, state} = typespec(left, vars, caller, state)
    state = %{state | undefined_type_error_enabled?: true}
    {right, state} = typespec(right, vars, caller, state)
    {{:ann_type, line(meta), [left, right]}, state}
  end

  # Handle unary ops
  defp typespec({op, meta, [integer]}, _, _, state) when op in [:+, :-] and is_integer(integer) do
    line = line(meta)
    {{:op, line, op, {:integer, line, integer}}, state}
  end

  # Handle remote calls in the form of @module_attribute.type.
  # These are not handled by the general remote type clause as calling
  # Macro.expand/2 on the remote does not expand module attributes (but expands
  # things like __MODULE__).
  defp typespec(
         {{:., meta, [{:@, _, [{attr, _, _}]}, name]}, _, args} = orig,
         vars,
         caller,
         state
       ) do
    remote = Module.get_attribute(caller.module, attr)

    unless is_atom(remote) and remote != nil do
      message =
        "invalid remote in typespec: #{Macro.to_string(orig)} (@#{attr} is #{inspect(remote)})"

      compile_error(caller, message)
    end

    {remote_spec, state} = typespec(remote, vars, caller, state)
    {name_spec, state} = typespec(name, vars, caller, state)
    type = {remote_spec, meta, name_spec, args}
    remote_type(type, vars, caller, state)
  end

  # Handle remote calls
  defp typespec({{:., meta, [remote, name]}, _, args} = orig, vars, caller, state) do
    # We set a function name to avoid tracking
    # aliases in typespecs as compile time dependencies.
    remote = Macro.expand(remote, %{caller | function: {:typespec, 0}})

    cond do
      not is_atom(remote) ->
        compile_error(caller, "invalid remote in typespec: #{Macro.to_string(orig)}")

      remote == caller.module ->
        typespec({name, meta, args}, vars, caller, state)

      true ->
        {remote_spec, state} = typespec(remote, vars, caller, state)
        {name_spec, state} = typespec(name, vars, caller, state)
        type = {remote_spec, meta, name_spec, args}
        remote_type(type, vars, caller, state)
    end
  end

  # Handle tuples
  defp typespec({:tuple, meta, []}, _vars, _caller, state) do
    {{:type, line(meta), :tuple, :any}, state}
  end

  defp typespec({:{}, meta, t}, vars, caller, state) when is_list(t) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, t)
    {{:type, line(meta), :tuple, args}, state}
  end

  defp typespec({left, right}, vars, caller, state) do
    typespec({:{}, [], [left, right]}, vars, caller, state)
  end

  # Handle blocks
  defp typespec({:__block__, _meta, [arg]}, vars, caller, state) do
    typespec(arg, vars, caller, state)
  end

  # Handle variables or local calls
  defp typespec({name, meta, atom}, vars, caller, state) when is_atom(atom) do
    if :lists.member(name, vars) do
      state = update_local_vars(state, name)
      {{:var, line(meta), name}, state}
    else
      typespec({name, meta, []}, vars, caller, state)
    end
  end

  # Handle local calls
  defp typespec({:string, meta, args}, vars, caller, state) do
    warning =
      "string() type use is discouraged. " <>
        "For character lists, use charlist() type, for strings, String.t()\n" <>
        Exception.format_stacktrace(Macro.Env.stacktrace(caller))

    :elixir_errors.erl_warn(caller.line, caller.file, warning)
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    {{:type, line(meta), :string, args}, state}
  end

  defp typespec({:nonempty_string, meta, args}, vars, caller, state) do
    warning =
      "nonempty_string() type use is discouraged. " <>
        "For non-empty character lists, use nonempty_charlist() type, for strings, String.t()\n" <>
        Exception.format_stacktrace(Macro.Env.stacktrace(caller))

    :elixir_errors.erl_warn(caller.line, caller.file, warning)
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    {{:type, line(meta), :nonempty_string, args}, state}
  end

  defp typespec({type, _meta, []}, vars, caller, state) when type in [:charlist, :char_list] do
    if type == :char_list do
      warning = "the char_list() type is deprecated, use charlist()"
      :elixir_errors.erl_warn(caller.line, caller.file, warning)
    end

    typespec(quote(do: :elixir.charlist()), vars, caller, state)
  end

  defp typespec({:nonempty_charlist, _meta, []}, vars, caller, state) do
    typespec(quote(do: :elixir.nonempty_charlist()), vars, caller, state)
  end

  defp typespec({:struct, _meta, []}, vars, caller, state) do
    typespec(quote(do: :elixir.struct()), vars, caller, state)
  end

  defp typespec({:as_boolean, _meta, [arg]}, vars, caller, state) do
    typespec(quote(do: :elixir.as_boolean(unquote(arg))), vars, caller, state)
  end

  defp typespec({:keyword, _meta, args}, vars, caller, state) when length(args) <= 1 do
    typespec(quote(do: :elixir.keyword(unquote_splicing(args))), vars, caller, state)
  end

  defp typespec({:fun, meta, args}, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    {{:type, line(meta), :fun, args}, state}
  end

  defp typespec({name, meta, args}, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    arity = length(args)

    case :erl_internal.is_type(name, arity) do
      true ->
        {{:type, line(meta), name, args}, state}

      false ->
        if state.undefined_type_error_enabled? and
             not Map.has_key?(state.defined_type_pairs, {name, arity}) do
          compile_error(
            caller,
            "type #{name}/#{arity} undefined (no such type in #{inspect(caller.module)})"
          )
        end

        state =
          if :lists.member({name, arity}, state.used_type_pairs) do
            state
          else
            %{state | used_type_pairs: [{name, arity} | state.used_type_pairs]}
          end

        {{:user_type, line(meta), name, args}, state}
    end
  end

  # Handle literals
  defp typespec(atom, _, _, state) when is_atom(atom) do
    {{:atom, 0, atom}, state}
  end

  defp typespec(integer, _, _, state) when is_integer(integer) do
    {{:integer, 0, integer}, state}
  end

  defp typespec([], vars, caller, state) do
    typespec({nil, [], []}, vars, caller, state)
  end

  defp typespec([{:..., _, atom}], vars, caller, state) when is_atom(atom) do
    typespec({:nonempty_list, [], []}, vars, caller, state)
  end

  defp typespec([spec, {:..., _, atom}], vars, caller, state) when is_atom(atom) do
    typespec({:nonempty_list, [], [spec]}, vars, caller, state)
  end

  defp typespec([spec], vars, caller, state) do
    typespec({:list, [], [spec]}, vars, caller, state)
  end

  defp typespec(list, vars, caller, state) when is_list(list) do
    [head | tail] = :lists.reverse(list)

    union =
      :lists.foldl(
        fn elem, acc -> {:|, [], [validate_kw(elem, list, caller), acc]} end,
        validate_kw(head, list, caller),
        tail
      )

    typespec({:list, [], [union]}, vars, caller, state)
  end

  defp typespec(other, _vars, caller, _state) do
    compile_error(caller, "unexpected expression in typespec: #{Macro.to_string(other)}")
  end

  ## Helpers

  defp compile_error(caller, desc) do
    raise CompileError, file: caller.file, line: caller.line, description: desc
  end

  defp remote_type({remote, meta, name, args}, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    {{:remote_type, line(meta), [remote, name, args]}, state}
  end

  defp collect_union({:|, _, [a, b]}), do: [a | collect_union(b)]
  defp collect_union(v), do: [v]

  defp validate_kw({key, _} = t, _, _caller) when is_atom(key), do: t

  defp validate_kw(_, original, caller) do
    compile_error(caller, "unexpected list in typespec: #{Macro.to_string(original)}")
  end

  defp validate_range({:op, _, :-, {:integer, meta, first}}, last, caller) do
    validate_range({:integer, meta, -first}, last, caller)
  end

  defp validate_range(first, {:op, _, :-, {:integer, meta, last}}, caller) do
    validate_range(first, {:integer, meta, -last}, caller)
  end

  defp validate_range({:integer, _, first}, {:integer, _, last}, _caller) when first < last do
    :ok
  end

  defp validate_range(_, _, caller) do
    message =
      "invalid range specification, expected both sides to be integers, " <>
        "with the left side lower than the right side"

    compile_error(caller, message)
  end

  defp fn_args(meta, args, return, vars, caller, state) do
    {fun_args, state} = fn_args(meta, args, vars, caller, state)
    {spec, state} = typespec(return, vars, caller, state)

    case [fun_args, spec] do
      [{:type, _, :any}, {:type, _, :any, []}] -> {[], state}
      x -> {x, state}
    end
  end

  defp fn_args(meta, [{:..., _, _}], _vars, _caller, state) do
    {{:type, line(meta), :any}, state}
  end

  defp fn_args(meta, args, vars, caller, state) do
    {args, state} = :lists.mapfoldl(&typespec(&1, vars, caller, &2), state, args)
    {{:type, line(meta), :product, args}, state}
  end

  defp variable({name, meta, args}) when is_atom(name) and is_atom(args) do
    {:var, line(meta), name}
  end

  defp variable(expr), do: expr

  defp clean_local_state(state) do
    %{state | local_vars: %{}}
  end

  defp update_local_vars(%{local_vars: local_vars} = state, var_name) do
    case Map.fetch(local_vars, var_name) do
      {:ok, :used_once} -> %{state | local_vars: Map.put(local_vars, var_name, :used_multiple)}
      {:ok, :used_multiple} -> state
      :error -> %{state | local_vars: Map.put(local_vars, var_name, :used_once)}
    end
  end

  defp ensure_no_underscore_local_vars!(caller, var_names) do
    case :lists.member(:_, var_names) do
      true ->
        compile_error(caller, "type variable '_' is invalid")

      false ->
        :ok
    end
  end

  defp ensure_no_unused_local_vars!(caller, local_vars) do
    fun = fn {name, used_times} ->
      case {:erlang.atom_to_list(name), used_times} do
        {[?_ | _], :used_once} ->
          :ok

        {[?_ | _], :used_multiple} ->
          warning =
            "the underscored type variable \"#{name}\" is used more than once in the " <>
              "type specification. A leading underscore indicates that the value of the " <>
              "variable should be ignored. If this is intended please rename the variable to " <>
              "remove the underscore"

          :elixir_errors.erl_warn(caller.line, caller.file, warning)

        {_, :used_once} ->
          compile_error(caller, "type variable #{name} is unused")

        _ ->
          :ok
      end
    end

    :lists.foreach(fun, :maps.to_list(local_vars))
  end
end
