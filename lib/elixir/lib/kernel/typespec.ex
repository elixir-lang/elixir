defmodule Kernel.Typespec do
  # TODO: Remove deprecated code on 2.0 and move this module to Module.Typespec.
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
            :none -> {{name, arity}, nil}
            :hidden -> {{name, arity}, false}
            %{"en" => doc_string} -> {{name, arity}, doc_string}
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

    :lists.any(finder, get_typespec(bag, :type))
  end

  def spec_to_callback(module, {name, arity} = signature)
      when is_atom(module) and is_atom(name) and arity in 0..255 do
    {_set, bag} = :elixir_module.data_tables(module)

    filter = fn {expr, _} = object ->
      if spec_to_signature(expr) == signature do
        :ets.delete_object(bag, {:spec, object})
        store_typespec(bag, :callback, object)
        true
      else
        false
      end
    end

    :lists.filter(filter, get_typespec(bag, :spec)) != []
  end

  ## Typespec definition and storage

  @doc """
  Defines a typespec.

  Invoked by `Kernel.@/1` expansion.
  """
  def deftypespec(:spec, expr, _line, _file, module, pos) do
    {set, bag} = :elixir_module.data_tables(module)
    store_typespec(bag, :spec, {expr, pos})
    store_module_attribute(set, :spec, {expr, pos})
  end

  def deftypespec(kind, expr, line, _file, module, pos)
      when kind in [:callback, :macrocallback] do
    {set, bag} = :elixir_module.data_tables(module)

    case spec_to_signature(expr) do
      {name, arity} ->
        {line, doc} = get_doc_info(set, :doc, line)
        store_doc(set, kind, name, arity, line, :doc, doc, %{})

      :error ->
        :error
    end

    store_typespec(bag, kind, {expr, pos})
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

          :elixir_errors.warn(line, file, warning)
        end

      {name, arity} ->
        {line, doc} = get_doc_info(set, :typedoc, line)
        spec_meta = if kind == :opaque, do: %{opaque: true}, else: %{}
        store_doc(set, :type, name, arity, line, :typedoc, doc, spec_meta)

      :error ->
        :error
    end

    store_typespec(bag, :type, {kind, expr, pos})
  end

  defp get_typespec(bag, key) do
    :ets.lookup_element(bag, key, 2)
  catch
    :error, :badarg -> []
  end

  defp store_module_attribute(set, key, value) do
    :ets.insert(set, {key, value, nil})
    :ok
  end

  defp store_typespec(bag, key, value) do
    :ets.insert(bag, {key, value})
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

  defp type_to_signature({:::, _, [{name, _, context}, _]})
       when is_atom(name) and name != ::: and is_atom(context),
       do: {name, 0}

  defp type_to_signature({:::, _, [{name, _, args}, _]}) when is_atom(name) and name != :::,
    do: {name, length(args)}

  defp type_to_signature(_), do: :error

  ## Translation from Elixir AST to typespec AST

  @doc false
  def translate_typespecs_for_module(_set, bag) do
    type_typespecs = take_typespec(bag, :type)
    defined_type_pairs = collect_defined_type_pairs(type_typespecs)

    state = %{
      defined_type_pairs: defined_type_pairs,
      used_type_pairs: [],
      local_vars: %{},
      undefined_type_error_enabled?: true
    }

    {types, state} = Enum.map_reduce(type_typespecs, state, &translate_type/2)
    {specs, state} = Enum.map_reduce(take_typespec(bag, :spec), state, &translate_spec/2)
    {callbacks, state} = Enum.map_reduce(take_typespec(bag, :callback), state, &translate_spec/2)

    {macrocallbacks, state} =
      Enum.map_reduce(take_typespec(bag, :macrocallback), state, &translate_spec/2)

    optional_callbacks = List.flatten(get_typespec(bag, {:accumulate, :optional_callbacks}))
    used_types = filter_used_types(types, state)

    {used_types, specs, callbacks, macrocallbacks, optional_callbacks}
  end

  defp take_typespec(bag, key) do
    :ets.take(bag, key)
  end

  defp collect_defined_type_pairs(type_typespecs) do
    Enum.reduce(type_typespecs, %{}, fn {_, {_, expr, pos}}, type_pairs ->
      %{file: file, line: line} = env = :elixir_locals.get_cached_env(pos)

      case type_to_signature(expr) do
        {name, arity} = type_pair ->
          if builtin_type?(name, arity) do
            message = "type #{name}/#{arity} is a builtin type and it cannot be redefined"
            compile_error(env, message)
          end

          if Map.has_key?(type_pairs, type_pair) do
            compile_error(env, "type #{name}/#{arity} is already defined")
          end

          Map.put(type_pairs, type_pair, {file, line})

        :error ->
          compile_error(env, "invalid type specification: #{Macro.to_string(expr)}")
      end
    end)
  end

  defp filter_used_types(types, state) do
    Enum.filter(types, fn {_kind, {name, arity} = type_pair, _line, _type, export} ->
      if type_pair not in state.used_type_pairs and not export do
        %{^type_pair => {file, line}} = state.defined_type_pairs
        :elixir_errors.warn(line, file, "type #{name}/#{arity} is unused")
        false
      else
        true
      end
    end)
  end

  defp translate_type({_, {kind, {:::, _, [{name, _, args}, definition]}, pos}}, state) do
    caller = :elixir_locals.get_cached_env(pos)
    state = clean_local_state(state)

    args =
      if is_atom(args) do
        []
      else
        for(arg <- args, do: variable(arg))
      end

    vars = for {:var, _, var} <- args, do: var
    state = Enum.reduce(vars, state, &update_local_vars(&2, &1))
    {spec, state} = typespec(definition, vars, caller, state)
    vars = for {:var, _, _} = var <- args, do: var
    type = {name, spec, vars}
    arity = length(args)

    ensure_no_unused_local_vars!(caller, state.local_vars)

    {kind, export} =
      case kind do
        :type -> {:type, true}
        :typep -> {:type, false}
        :opaque -> {:opaque, true}
      end

    invalid_args = Enum.reject(args, &valid_variable_ast?/1)

    unless invalid_args == [] do
      invalid_args = invalid_args |> Enum.map(&Macro.to_string/1) |> Enum.join(", ")

      message =
        "@type definitions expect all arguments to be variables. The type " <>
          "#{name}/#{arity} has an invalid argument(s): #{invalid_args}"

      compile_error(caller, message)
    end

    if underspecified?(kind, arity, spec) do
      message = "@#{kind} type #{name}/#{arity} is underspecified and therefore meaningless"
      :elixir_errors.warn(caller.line, caller.file, message)
    end

    {{kind, {name, arity}, caller.line, type, export}, state}
  end

  defp valid_variable_ast?({variable_name, _, context})
       when is_atom(variable_name) and is_atom(context),
       do: true

  defp valid_variable_ast?(_), do: false

  defp underspecified?(:opaque, 0, {:type, _, type, []}) when type in [:any, :term], do: true
  defp underspecified?(_kind, _arity, _spec), do: false

  defp translate_spec({kind, {{:when, _meta, [spec, guard]}, pos}}, state) do
    caller = :elixir_locals.get_cached_env(pos)
    translate_spec(kind, spec, guard, caller, state)
  end

  defp translate_spec({kind, {spec, pos}}, state) do
    caller = :elixir_locals.get_cached_env(pos)
    translate_spec(kind, spec, [], caller, state)
  end

  defp translate_spec(kind, {:::, meta, [{name, _, args}, return]}, guard, caller, state)
       when is_atom(name) and name != ::: do
    translate_spec(kind, meta, name, args, return, guard, caller, state)
  end

  defp translate_spec(_kind, {name, _meta, _args} = spec, _guard, caller, _state)
       when is_atom(name) and name != ::: do
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

  # TODO: Remove char_list type by 2.0
  defp builtin_type?(:char_list, 0), do: true
  defp builtin_type?(:charlist, 0), do: true
  defp builtin_type?(:as_boolean, 1), do: true
  defp builtin_type?(:struct, 0), do: true
  defp builtin_type?(:nonempty_charlist, 0), do: true
  defp builtin_type?(:keyword, 0), do: true
  defp builtin_type?(:keyword, 1), do: true
  defp builtin_type?(name, arity), do: :erl_internal.is_type(name, arity)

  defp ensure_no_defaults!(args) do
    Enum.each(args, fn
      {:::, _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left

      other ->
        ensure_not_default(other)
        other
    end)
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in typespecs"
  end

  defp ensure_not_default(_), do: :ok

  defp guard_to_constraints(guard, vars, meta, caller, state) do
    line = line(meta)

    Enum.flat_map_reduce(guard, state, fn
      {_name, {:var, _, context}}, state when is_atom(context) ->
        {[], state}

      {name, type}, state ->
        {spec, state} = typespec(type, vars, caller, state)
        constraint = [{:atom, line, :is_subtype}, [{:var, line, name}, spec]]
        state = update_local_vars(state, name)

        {[{:type, line, :constraint, constraint}], state}
    end)
  end

  ## To typespec conversion

  defp line(meta) do
    Keyword.get(meta, :line, 0)
  end

  # Handle unions
  defp typespec({:|, meta, [_, _]} = exprs, vars, caller, state) do
    exprs = collect_union(exprs)
    {union, state} = Enum.map_reduce(exprs, state, &typespec(&1, vars, caller, &2))
    {{:type, line(meta), :union, union}, state}
  end

  # Handle binaries
  defp typespec({:<<>>, meta, []}, _, _, state) do
    line = line(meta)
    {{:type, line, :binary, [{:integer, line, 0}, {:integer, line, 0}]}, state}
  end

  defp typespec(
         {:<<>>, meta, [{:::, unit_meta, [{:_, _, ctx1}, {:*, _, [{:_, _, ctx2}, unit]}]}]},
         _,
         _,
         state
       )
       when is_atom(ctx1) and is_atom(ctx2) and is_integer(unit) do
    line = line(meta)
    {{:type, line, :binary, [{:integer, line, 0}, {:integer, line(unit_meta), unit}]}, state}
  end

  defp typespec({:<<>>, meta, [{:::, size_meta, [{:_, _, ctx}, size]}]}, _, _, state)
       when is_atom(ctx) and is_integer(size) do
    line = line(meta)
    {{:type, line, :binary, [{:integer, line(size_meta), size}, {:integer, line, 0}]}, state}
  end

  defp typespec(
         {
           :<<>>,
           meta,
           [
             {:::, size_meta, [{:_, _, ctx1}, size]},
             {:::, unit_meta, [{:_, _, ctx2}, {:*, _, [{:_, _, ctx3}, unit]}]}
           ]
         },
         _,
         _,
         state
       )
       when is_atom(ctx1) and is_atom(ctx2) and is_atom(ctx3) and is_integer(size) and
              is_integer(unit) do
    args = [{:integer, line(size_meta), size}, {:integer, line(unit_meta), unit}]
    {{:type, line(meta), :binary, args}, state}
  end

  ## Handle maps and structs
  defp typespec({:map, meta, args}, _vars, _caller, state) when args == [] or is_atom(args) do
    {{:type, line(meta), :map, :any}, state}
  end

  defp typespec({:%{}, meta, fields} = map, vars, caller, state) do
    {fields, state} =
      Enum.map_reduce(fields, state, fn
        {k, v}, state when is_atom(k) ->
          {arg1, state} = typespec(k, vars, caller, state)
          {arg2, state} = typespec(v, vars, caller, state)
          {{:type, line(meta), :map_field_exact, [arg1, arg2]}, state}

        {{:required, meta2, [k]}, v}, state ->
          {arg1, state} = typespec(k, vars, caller, state)
          {arg2, state} = typespec(v, vars, caller, state)
          {{:type, line(meta2), :map_field_exact, [arg1, arg2]}, state}

        {{:optional, meta2, [k]}, v}, state ->
          {arg1, state} = typespec(k, vars, caller, state)
          {arg2, state} = typespec(v, vars, caller, state)
          {{:type, line(meta2), :map_field_assoc, [arg1, arg2]}, state}

        {k, v}, state ->
          # TODO: Warn on Elixir v1.8 (since v1.6 is the first version to drop support for 18 and
          # older)
          # warning =
          #   "invalid map specification. %{foo => bar} is deprecated in favor of " <>
          #   "%{required(foo) => bar} and %{optional(foo) => bar}."
          # :elixir_errors.warn(caller.line, caller.file, warning)
          {arg1, state} = typespec(k, vars, caller, state)
          {arg2, state} = typespec(v, vars, caller, state)
          {{:type, line(meta), :map_field_assoc, [arg1, arg2]}, state}

        {:|, _, [_, _]}, _state ->
          error =
            "invalid map specification. When using the | operator in the map key, " <>
              "make sure to wrap the key type in parentheses: #{Macro.to_string(map)}"

          compile_error(caller, error)

        _, _state ->
          compile_error(caller, "invalid map specification: #{Macro.to_string(map)}")
      end)

    {{:type, line(meta), :map, fields}, state}
  end

  defp typespec({:%, _, [name, {:%{}, meta, fields}]}, vars, caller, state) do
    # We cannot set a function name to avoid tracking
    # as a compile time dependency, because for structs it actually is one.
    module = Macro.expand(name, caller)

    struct =
      if module == caller.module do
        Module.get_attribute(module, :struct) ||
          compile_error(caller, "struct is not defined for #{Macro.to_string(name)}")
      else
        module.__struct__
      end

    struct = struct |> Map.from_struct() |> Map.to_list()

    unless Keyword.keyword?(fields) do
      compile_error(caller, "expected key-value pairs in struct #{Macro.to_string(name)}")
    end

    types =
      Enum.map(struct, fn {field, _} ->
        {field, Keyword.get(fields, field, quote(do: term()))}
      end)

    Enum.each(fields, fn {field, _} ->
      unless Keyword.has_key?(struct, field) do
        compile_error(caller, "undefined field #{field} on struct #{Macro.to_string(name)}")
      end
    end)

    typespec({:%{}, meta, [__struct__: module] ++ types}, vars, caller, state)
  end

  # Handle records
  defp typespec({:record, meta, [atom]}, vars, caller, state) do
    typespec({:record, meta, [atom, []]}, vars, caller, state)
  end

  defp typespec({:record, meta, [tag, field_specs]}, vars, caller, state) do
    # We cannot set a function name to avoid tracking
    # as a compile time dependency because for records it actually is one.
    case Macro.expand({tag, [], [{:{}, [], []}]}, caller) do
      {_, _, [name, fields | _]} when is_list(fields) ->
        types =
          Enum.map(fields, fn {field, _} ->
            Keyword.get(field_specs, field, quote(do: term()))
          end)

        Enum.each(field_specs, fn {field, _} ->
          unless Keyword.has_key?(fields, field) do
            compile_error(caller, "undefined field #{field} on record #{inspect(tag)}")
          end
        end)

        typespec({:{}, meta, [name | types]}, vars, caller, state)

      _ ->
        compile_error(caller, "unknown record #{inspect(tag)}")
    end
  end

  # Handle ranges
  defp typespec({:.., meta, args}, vars, caller, state) do
    {args, state} = Enum.map_reduce(args, state, &typespec(&1, vars, caller, &2))
    {{:type, line(meta), :range, args}, state}
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
  defp typespec([{:->, meta, [arguments, return]}], vars, caller, state)
       when is_list(arguments) do
    {args, state} = fn_args(meta, arguments, return, vars, caller, state)
    {{:type, line(meta), :fun, args}, state}
  end

  # Handle type operator
  defp typespec(
         {:::, meta, [{var_name, var_meta, context}, expr]} = ann_type,
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

        # TODO: make this an error in elixir 2.0 and remove the code below
        :elixir_errors.warn(caller.line, caller.file, message)

        # This may be generating an invalid typespec but we need to generate it
        # to avoid breaking existing code that was valid but only broke dialyzer
        {right, state} = typespec(expr, vars, caller, state)
        {{:ann_type, line(meta), [{:var, line(var_meta), var_name}, right]}, state}

      {right, state} ->
        {{:ann_type, line(meta), [{:var, line(var_meta), var_name}, right]}, state}
    end
  end

  defp typespec({:::, meta, [left, right]} = expr, vars, caller, state) do
    message =
      "invalid type annotation. When using the | operator to represent the union of types, " <>
        "make sure to wrap type annotations in parentheses: #{Macro.to_string(expr)}"

    # TODO: make this an error in Elixir 2.0, and remove the code below and the
    # :undefined_type_error_enabled? key from the state
    :elixir_errors.warn(caller.line, caller.file, message)

    # This may be generating an invalid typespec but we need to generate it
    # to avoid breaking existing code that was valid but only broke dialyzer
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

    unless is_atom(remote) do
      compile_error(caller, "invalid remote in typespec: #{Macro.to_string(orig)}")
    end

    {remote_spec, state} = typespec(remote, vars, caller, state)
    {name_spec, state} = typespec(name, vars, caller, state)
    type = {remote_spec, meta, name_spec, args}
    remote_type(type, vars, caller, state)
  end

  # Handle tuples
  defp typespec({:tuple, meta, []}, _vars, _caller, state) do
    {{:type, line(meta), :tuple, :any}, state}
  end

  defp typespec({:{}, meta, t}, vars, caller, state) when is_list(t) do
    {args, state} = Enum.map_reduce(t, state, &typespec(&1, vars, caller, &2))
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
    if name in vars do
      state = update_local_vars(state, name)
      {{:var, line(meta), name}, state}
    else
      typespec({name, meta, []}, vars, caller, state)
    end
  end

  # Handle local calls
  defp typespec({:string, meta, arguments}, vars, caller, state) do
    warning =
      "string() type use is discouraged. " <>
        "For character lists, use charlist() type, for strings, String.t()\n" <>
        Exception.format_stacktrace(Macro.Env.stacktrace(caller))

    :elixir_errors.warn(caller.line, caller.file, warning)

    {arguments, state} = Enum.map_reduce(arguments, state, &typespec(&1, vars, caller, &2))
    {{:type, line(meta), :string, arguments}, state}
  end

  defp typespec({:nonempty_string, meta, arguments}, vars, caller, state) do
    warning =
      "nonempty_string() type use is discouraged. " <>
        "For non-empty character lists, use nonempty_charlist() type, for strings, String.t()\n" <>
        Exception.format_stacktrace(Macro.Env.stacktrace(caller))

    :elixir_errors.warn(caller.line, caller.file, warning)

    {arguments, state} = Enum.map_reduce(arguments, state, &typespec(&1, vars, caller, &2))
    {{:type, line(meta), :nonempty_string, arguments}, state}
  end

  # TODO: Remove char_list type by 2.0
  defp typespec({type, _meta, []}, vars, caller, state) when type in [:charlist, :char_list] do
    if type == :char_list do
      warning = "the char_list() type is deprecated, use charlist()"
      :elixir_errors.warn(caller.line, caller.file, warning)
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
    {args, state} = Enum.map_reduce(args, state, &typespec(&1, vars, caller, &2))
    {{:type, line(meta), :fun, args}, state}
  end

  defp typespec({name, meta, arguments}, vars, caller, state) do
    {arguments, state} = Enum.map_reduce(arguments, state, &typespec(&1, vars, caller, &2))
    arity = length(arguments)

    case :erl_internal.is_type(name, arity) do
      true ->
        {{:type, line(meta), name, arguments}, state}

      false ->
        if state.undefined_type_error_enabled? and
             not Map.has_key?(state.defined_type_pairs, {name, arity}) do
          compile_error(caller, "type #{name}/#{arity} undefined")
        end

        state =
          if {name, arity} in state.used_type_pairs do
            state
          else
            %{state | used_type_pairs: [{name, arity} | state.used_type_pairs]}
          end

        {{:user_type, line(meta), name, arguments}, state}
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
    [head | tail] = Enum.reverse(list)

    union =
      Enum.reduce(tail, validate_kw(head, list, caller), fn elem, acc ->
        {:|, [], [validate_kw(elem, list, caller), acc]}
      end)

    typespec({:list, [], [union]}, vars, caller, state)
  end

  defp typespec(other, _vars, caller, _state) do
    compile_error(caller, "unexpected expression in typespec: #{Macro.to_string(other)}")
  end

  ## Helpers

  defp compile_error(caller, desc) do
    raise CompileError, file: caller.file, line: caller.line, description: desc
  end

  defp remote_type({remote, meta, name, arguments}, vars, caller, state) do
    {arguments, state} = Enum.map_reduce(arguments, state, &typespec(&1, vars, caller, &2))
    {{:remote_type, line(meta), [remote, name, arguments]}, state}
  end

  defp collect_union({:|, _, [a, b]}), do: [a | collect_union(b)]
  defp collect_union(v), do: [v]

  defp validate_kw({key, _} = t, _, _caller) when is_atom(key), do: t

  defp validate_kw(_, original, caller) do
    compile_error(caller, "unexpected list in typespec: #{Macro.to_string(original)}")
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
    {args, state} = Enum.map_reduce(args, state, &typespec(&1, vars, caller, &2))
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

  defp ensure_no_unused_local_vars!(caller, local_vars) do
    for {name, :used_once} <- local_vars do
      compile_error(caller, "type variable #{name} is unused")
    end
  end
end
