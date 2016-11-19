defmodule Kernel.Typespec do
  @moduledoc false

  @doc """
  Defines a type.
  This macro is responsible for handling the attribute `@type`.

  ## Examples

      @type my_type :: atom

  """
  defmacro deftype(type) do
    quote do
      Kernel.Typespec.deftype(:type, unquote(Macro.escape(type, unquote: true)), __ENV__)
    end
  end

  @doc """
  Defines an opaque type.
  This macro is responsible for handling the attribute `@opaque`.

  ## Examples

      @opaque my_type :: atom

  """
  defmacro defopaque(type) do
    quote do
      Kernel.Typespec.deftype(:opaque, unquote(Macro.escape(type, unquote: true)), __ENV__)
    end
  end

  @doc """
  Defines a private type.
  This macro is responsible for handling the attribute `@typep`.

  ## Examples

      @typep my_type :: atom

  """
  defmacro deftypep(type) do
    quote do
      Kernel.Typespec.deftype(:typep, unquote(Macro.escape(type, unquote: true)), __ENV__)
    end
  end

  @doc """
  Defines a spec.
  This macro is responsible for handling the attribute `@spec`.

  ## Examples

      @spec add(number, number) :: number

  """
  defmacro defspec(spec) do
    quote do
      Kernel.Typespec.defspec(:spec, unquote(Macro.escape(spec, unquote: true)), __ENV__)
    end
  end

  @doc """
  Defines a callback.
  This macro is responsible for handling the attribute `@callback`.

  ## Examples

      @callback add(number, number) :: number

  """
  defmacro defcallback(spec) do
    quote do
      Kernel.Typespec.defspec(:callback, unquote(Macro.escape(spec, unquote: true)), __ENV__)
    end
  end


  @doc """
  Defines a macro callback.
  This macro is responsible for handling the attribute `@macrocallback`.

  ## Examples

      @macrocallback add(number, number) :: Macro.t

  """
  defmacro defmacrocallback(spec) do
    quote do
      Kernel.Typespec.defspec(:macrocallback, unquote(Macro.escape(spec, unquote: true)), __ENV__)
    end
  end

  defmacro defoptional_callbacks(callbacks) do
    quote do
      Module.store_typespec(__ENV__.module, :optional_callbacks, {__ENV__.line, unquote(callbacks)})
    end
  end

  @doc """
  Defines a `type`, `typep` or `opaque` by receiving a typespec expression.
  """
  def define_type(kind, expr, doc \\ nil, env) do
    Module.store_typespec(env.module, kind, {kind, expr, doc, env})
  end

  @doc """
  Defines a `spec` by receiving a typespec expression.
  """
  @spec define_spec(atom, Macro.t, Macro.Env.t) :: Keyword.t
  def define_spec(kind, expr, env) do
    defspec(kind, expr, env)
  end

  @doc """
  Returns `true` if the current module defines a given type
  (private, opaque or not). This function is only available
  for modules being compiled.
  """
  @spec defines_type?(module, atom, arity) :: boolean
  def defines_type?(module, name, arity)
      when is_atom(module) and is_atom(name) and arity in 0..255 do
    finder = fn {_kind, expr, _caller} ->
      type_to_signature(expr) == {name, arity}
    end
    :lists.any(finder, Module.get_attribute(module, :type)) or
    :lists.any(finder, Module.get_attribute(module, :opaque))
  end

  @doc """
  Returns `true` if the current module defines a given spec.
  This function is only available for modules being compiled.
  """
  @spec defines_spec?(module, atom, arity) :: boolean
  def defines_spec?(module, name, arity)
      when is_atom(module) and is_atom(name) and arity in 0..255 do
    finder = fn {_kind, expr, _caller} ->
      spec_to_signature(expr) == {name, arity}
    end
    :lists.any(finder, Module.get_attribute(module, :spec))
  end

  @doc """
  Returns `true` if the current module defines a callback.
  This function is only available for modules being compiled.
  """
  @spec defines_callback?(module, atom, arity) :: boolean
  def defines_callback?(module, name, arity)
      when is_atom(module) and is_atom(name) and arity in 0..255 do
    finder = fn {_kind, expr, _caller} ->
      spec_to_signature(expr) == {name, arity}
    end
    :lists.any(finder, Module.get_attribute(module, :callback))
  end

  @doc """
  Converts a spec clause back to Elixir AST.
  """
  @spec spec_to_ast(atom, tuple) :: {atom, Keyword.t, [Macro.t]}
  def spec_to_ast(name, spec)
  def spec_to_ast(name, {:type, line, :fun, [{:type, _, :product, args}, result]})
      when is_atom(name) do
    meta = [line: line]
    body = {name, meta, Enum.map(args, &typespec_to_ast/1)}

    vars = args ++ [result]
      |> Enum.flat_map(&collect_vars/1)
      |> Enum.uniq
      |> Enum.map(&{&1, {:var, meta, nil}})

    spec = {:::, meta, [body, typespec_to_ast(result)]}

    if vars == [] do
      spec
    else
      {:when, meta, [spec, vars]}
    end
  end

  def spec_to_ast(name, {:type, line, :fun, []}) when is_atom(name) do
    {:::, [line: line], [{name, [line: line], []}, quote(do: term)]}
  end

  def spec_to_ast(name, {:type, line, :bounded_fun, [{:type, _, :fun, [{:type, _, :product, args}, result]}, constraints]})
      when is_atom(name) do
    guards =
      for {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, var}, type]]} <- constraints do
        {var, typespec_to_ast(type)}
      end

    meta = [line: line]

    vars = args ++ [result]
           |> Enum.flat_map(&collect_vars/1)
           |> Enum.uniq
           |> Kernel.--(Keyword.keys(guards))
           |> Enum.map(&{&1, {:var, meta, nil}})

    args = for arg <- args, do: typespec_to_ast(arg)

    {:when, meta, [
      {:::, meta, [{name, [line: line], args}, typespec_to_ast(result)]},
      guards ++ vars
    ]}
  end

  @doc """
  Converts a type clause back to Elixir AST.
  """
  def type_to_ast(type)
  def type_to_ast({{:record, record}, fields, args}) when is_atom(record) do
    fields = for field <- fields, do: typespec_to_ast(field)
    args = for arg <- args, do: typespec_to_ast(arg)
    type = {:{}, [], [record | fields]}
    quote do: unquote(record)(unquote_splicing(args)) :: unquote(type)
  end

  def type_to_ast({name, type, args}) when is_atom(name) do
    args = for arg <- args, do: typespec_to_ast(arg)
    quote do: unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  @doc false
  # TODO: Remove on v2.0
  def beam_typedocs(module) when is_atom(module) or is_binary(module) do
    IO.write :stderr, "Kernel.Typespec.beam_typedocs/1 is deprecated, please use Code.get_docs/2 instead\n" <>
                      Exception.format_stacktrace
    if docs = Code.get_docs(module, :type_docs) do
      for {tuple, _, _, doc} <- docs, do: {tuple, doc}
    end
  end

  @doc """
  Returns all types available from the module's BEAM code.

  The result is returned as a list of tuples where the first
  element is the type (`:typep`, `:type` and `:opaque`).

  The module must have a corresponding BEAM file which can be
  located by the runtime system.
  """
  @spec beam_types(module | binary) :: [tuple] | nil
  def beam_types(module) when is_atom(module) or is_binary(module) do
    case abstract_code(module) do
      {:ok, abstract_code} ->
        exported_types = for {:attribute, _, :export_type, types} <- abstract_code, do: types
        exported_types = :lists.flatten(exported_types)

        for {:attribute, _, kind, {name, _, args} = type} <- abstract_code, kind in [:opaque, :type] do
          cond do
            kind == :opaque -> {:opaque, type}
            {name, length(args)} in exported_types -> {:type, type}
            true -> {:typep, type}
          end
        end
      _ ->
        nil
    end
  end

  @doc """
  Returns all specs available from the module's BEAM code.

  The result is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding BEAM file which can be
  located by the runtime system.
  """
  @spec beam_specs(module | binary) :: [tuple] | nil
  def beam_specs(module) when is_atom(module) or is_binary(module) do
    from_abstract_code(module, :spec)
  end

  @doc """
  Returns all callbacks available from the module's BEAM code.

  The result is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding BEAM file
  which can be located by the runtime system.
  """
  @spec beam_callbacks(module | binary) :: [tuple] | nil
  def beam_callbacks(module) when is_atom(module) or is_binary(module) do
    from_abstract_code(module, :callback)
  end

  defp from_abstract_code(module, kind) do
    case abstract_code(module) do
      {:ok, abstract_code} ->
        for {:attribute, _, abs_kind, value} <- abstract_code, kind == abs_kind, do: value
      :error ->
        nil
    end
  end

  defp abstract_code(module) do
    case :beam_lib.chunks(abstract_code_beam(module), [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_raw_abstract_v1, abstract_code}}]}} ->
        {:ok, abstract_code}
      _ ->
        :error
    end
  end

  defp abstract_code_beam(module) when is_atom(module) do
    case :code.get_object_code(module) do
      {^module, beam, _filename} -> beam
      :error -> module
    end
  end

  defp abstract_code_beam(binary) when is_binary(binary) do
    binary
  end

  ## Helpers

  @doc false
  def spec_to_signature({:when, _, [spec, _]}),
    do: type_to_signature(spec)
  def spec_to_signature(other),
    do: type_to_signature(other)

  @doc false
  def type_to_signature({:::, _, [{name, _, context}, _]}) when is_atom(name) and is_atom(context),
    do: {name, 0}
  def type_to_signature({:::, _, [{name, _, args}, _]}) when is_atom(name),
    do: {name, length(args)}
  def type_to_signature(_),
    do: :error

  ## Macro callbacks

  @doc false
  def defspec(kind, expr, caller) when kind in [:callback, :macrocallback] do
    case spec_to_signature(expr) do
      {name, arity} ->
        store_callbackdoc(caller, caller.module, kind, name, arity)
      :error ->
        :error
    end
    Module.store_typespec(caller.module, kind, {kind, expr, caller})
  end

  @doc false
  def defspec(kind, expr, caller) do
    Module.store_typespec(caller.module, kind, {kind, expr, caller})
  end

  defp store_callbackdoc(caller, module, kind, name, arity) do
    table = :elixir_module.data_table(module)
    {line, doc} = get_doc_info(table, :doc, caller)
    :ets.insert(table, {{:callbackdoc, {name, arity}}, line, kind, doc})
  end

  defp get_doc_info(table, attr, caller) do
    case :ets.take(table, attr) do
      [{^attr, {line, doc}, _, _}] -> {line, doc}
      [] -> {caller.line, nil}
    end
  end

  @doc false
  def deftype(kind, expr, caller) do
    module = caller.module
    case type_to_signature(expr) do
      {name, arity} -> store_typedoc(caller, caller.module, kind, name, arity)
      :error -> :error
    end
    Module.store_typespec(module, kind, {kind, expr, caller})
  end

  defp store_typedoc(caller, module, kind, name, arity) do
    table = :elixir_module.data_table(module)
    {line, doc} = get_doc_info(table, :typedoc, caller)

    if kind == :typep && doc do
      :elixir_errors.warn(caller.line, caller.file, "type #{name}/#{arity} is private, " <>
                          "@typedoc's are always discarded for private types")
    end

    :ets.insert(table, {{:typedoc, {name, arity}}, line, kind, doc})
  end

  ## Translation from Elixir AST to typespec AST

  @doc false
  def translate_type(kind, {:::, _, [{name, _, args}, definition]}, caller) when is_atom(name) and name != ::: do
    args =
      if is_atom(args) do
        []
      else
        for(arg <- args, do: variable(arg))
      end

    vars  = for {:var, _, var} <- args, do: var
    spec  = typespec(definition, vars, caller)
    vars  = for {:var, _, _} = var <- args, do: var
    type  = {name, spec, vars}
    arity = length(vars)

    {kind, export} =
      case kind do
        :type   -> {:type, true}
        :typep  -> {:type, false}
        :opaque -> {:opaque, true}
      end

    if elixir_builtin_type?(name, arity) do
      :elixir_errors.handle_file_error(caller.file,
        {caller.line, :erl_lint, {:builtin_type, {name, arity}}})
    end

    {{kind, {name, arity}, type}, caller.line, export}
  end

  def translate_type(_kind, other, caller) do
    type_spec = Macro.to_string(other)
    compile_error caller, "invalid type specification: #{type_spec}"
  end

  defp elixir_builtin_type?(:as_boolean, 1), do: true
  defp elixir_builtin_type?(:struct, 0), do: true
  defp elixir_builtin_type?(:charlist, 0), do: true
  # TODO: Deprecate char_list type by v1.5
  defp elixir_builtin_type?(:char_list, 0), do: true
  defp elixir_builtin_type?(:keyword, 0), do: true
  defp elixir_builtin_type?(:keyword, 1), do: true
  defp elixir_builtin_type?(_, _), do: false

  @doc false
  def translate_spec(kind, {:when, _meta, [spec, guard]}, caller) do
    translate_spec(kind, spec, guard, caller)
  end

  def translate_spec(kind, spec, caller) do
    translate_spec(kind, spec, [], caller)
  end

  defp translate_spec(kind, {:::, meta, [{name, _, args}, return]}, guard, caller)
      when is_atom(name) and name != ::: do
    translate_spec(kind, meta, name, args, return, guard, caller)
  end

  defp translate_spec(_kind, {name, _meta, _args} = spec, _guard, caller) when is_atom(name) and name != ::: do
    spec = Macro.to_string(spec)
    compile_error caller, "type specification missing return type: #{spec}"
  end

  defp translate_spec(_kind, spec, _guard, caller) do
    spec = Macro.to_string(spec)
    compile_error caller, "invalid type specification: #{spec}"
  end

  defp translate_spec(kind, meta, name, args, return, guard, caller) when is_atom(args),
    do: translate_spec(kind, meta, name, [], return, guard, caller)
  defp translate_spec(:macrocallback, meta, name, args, return, guard, caller),
    do: translate_spec(:callback, meta, :"MACRO-#{name}", macro_args(args), return, guard, caller)
  defp translate_spec(kind, meta, name, args, return, guard, caller) do
    ensure_no_defaults!(args)

    unless Keyword.keyword?(guard) do
      compile_error caller, "expected keywords as guard in type specification, " <>
                            "got: #{Macro.to_string(guard)}"
    end

    vars = Keyword.keys(guard)
    spec = {:type, line(meta), :fun, fn_args(meta, args, return, vars, caller)}

    spec =
      case guard_to_constraints(guard, vars, meta, caller) do
        [] -> spec
        constraints -> {:type, line(meta), :bounded_fun, [spec, constraints]}
      end

    arity = length(args)
    {{kind, {name, arity}, spec}, caller.line}
  end

  defp macro_args(args) do
    [quote(do: {line :: Macro.Env.line, env :: Macro.Env.t}) | args]
  end

  defp ensure_no_defaults!(args) do
    :lists.foreach fn
      {:::, _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left
      other ->
        ensure_not_default(other)
        other
    end, args
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in type spec"
  end

  defp ensure_not_default(_), do: :ok

  defp guard_to_constraints(guard, vars, meta, caller) do
    line = line(meta)

    :lists.foldl(fn
      {_name, {:var, _, context}}, acc when is_atom(context) ->
        acc
      {name, type}, acc ->
        constraint = [{:atom, line, :is_subtype}, [{:var, line, name}, typespec(type, vars, caller)]]
        type = {:type, line, :constraint, constraint}
        [type | acc]
    end, [], guard) |> :lists.reverse
  end

  ## To AST conversion

  defp collect_vars({:ann_type, _line, args}) when is_list(args) do
    []
  end

  defp collect_vars({:type, _line, _kind, args}) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({:remote_type, _line, args}) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({:typed_record_field, _line, type}) do
    collect_vars(type)
  end

  defp collect_vars({:paren_type, _line, [type]}) do
    collect_vars(type)
  end

  defp collect_vars({:var, _line, var}) do
    [erl_to_ex_var(var)]
  end

  defp collect_vars(_) do
    []
  end

  defp typespec_to_ast({:user_type, line, name, args}) do
    typespec_to_ast({:type, line, name, args})
  end

  defp typespec_to_ast({:type, line, :tuple, :any}) do
    {:tuple, [line: line], []}
  end

  defp typespec_to_ast({:type, line, :tuple, args}) do
    args = for arg <- args, do: typespec_to_ast(arg)
    {:{}, [line: line], args}
  end

  defp typespec_to_ast({:type, _line, :list, [{:type, _, :union, unions} = arg]}) do
    case unpack_typespec_kw(unions, []) do
      {:ok, ast} -> ast
      :error -> [typespec_to_ast(arg)]
    end
  end

  defp typespec_to_ast({:type, line, :list, []}) do
    {:list, [line: line], []}
  end

  defp typespec_to_ast({:type, _line, :list, [arg]}) do
    [typespec_to_ast(arg)]
  end

  defp typespec_to_ast({:type, line, :nonempty_list, []}) do
    [{:..., [line: line], nil}]
  end

  defp typespec_to_ast({:type, line, :nonempty_list, [arg]}) do
    [typespec_to_ast(arg), {:..., [line: line], nil}]
  end

  defp typespec_to_ast({:type, line, :map, :any}) do
    {:map, [line: line], []}
  end

  defp typespec_to_ast({:type, line, :map, fields}) do
    fields = Enum.map fields, fn
      {:type, _, :map_field_assoc, :any} ->
        {{:optional, [], [{:any, [], []}]}, {:any, [], []}}
      {:type, _, :map_field_exact, [{:atom, _, k}, v]} ->
        {k, typespec_to_ast(v)}
      {:type, _, :map_field_exact, [k, v]} ->
        {{:required, [], [typespec_to_ast(k)]}, typespec_to_ast(v)}
      {:type, _, :map_field_assoc, [k, v]} ->
        {{:optional, [], [typespec_to_ast(k)]}, typespec_to_ast(v)}
    end

    {struct, fields} = Keyword.pop(fields, :__struct__)
    map = {:%{}, [line: line], fields}

    if struct do
      {:%, [line: line], [struct, map]}
    else
      map
    end
  end

  defp typespec_to_ast({:type, line, :binary, [arg1, arg2]}) do
    [arg1, arg2] = for arg <- [arg1, arg2], do: typespec_to_ast(arg)
    case {typespec_to_ast(arg1), typespec_to_ast(arg2)} do
      {arg1, 0} ->
        quote line: line, do: <<_ :: unquote(arg1)>>
      {0, arg2} ->
        quote line: line, do: <<_ :: _ * unquote(arg2)>>
      {arg1, arg2} ->
        quote line: line, do: <<_ :: unquote(arg1), _ :: _ * unquote(arg2)>>
    end
  end

  defp typespec_to_ast({:type, line, :union, args}) do
    args = for arg <- args, do: typespec_to_ast(arg)
    Enum.reduce Enum.reverse(args), fn(arg, expr) -> {:|, [line: line], [arg, expr]} end
  end

  defp typespec_to_ast({:type, line, :fun, [{:type, _, :product, args}, result]}) do
    args = for arg <- args, do: typespec_to_ast(arg)
    [{:->, [line: line], [args, typespec_to_ast(result)]}]
  end

  defp typespec_to_ast({:type, line, :fun, [args, result]}) do
    [{:->, [line: line], [[typespec_to_ast(args)], typespec_to_ast(result)]}]
  end

  defp typespec_to_ast({:type, line, :fun, []}) do
    typespec_to_ast({:type, line, :fun, [{:type, line, :any}, {:type, line, :any, []} ]})
  end

  defp typespec_to_ast({:type, line, :range, [left, right]}) do
    {:.., [line: line], [typespec_to_ast(left), typespec_to_ast(right)]}
  end

  defp typespec_to_ast({:type, _line, nil, []}) do
    []
  end

  defp typespec_to_ast({:type, line, name, args}) do
    args = for arg <- args, do: typespec_to_ast(arg)
    {name, [line: line], args}
  end

  defp typespec_to_ast({:var, line, var}) do
    {erl_to_ex_var(var), line, nil}
  end

  defp typespec_to_ast({:op, line, op, arg}) do
    {op, [line: line], [typespec_to_ast(arg)]}
  end

  # Special shortcut(s)
  # TODO: Deprecate char_list type by v1.5
  defp typespec_to_ast({:remote_type, line, [{:atom, _, :elixir}, {:atom, _, type}, []]})
      when type in [:charlist, :char_list] do
    typespec_to_ast({:type, line, :charlist, []})
  end

  defp typespec_to_ast({:remote_type, line, [{:atom, _, :elixir}, {:atom, _, :struct}, []]}) do
    typespec_to_ast({:type, line, :struct, []})
  end

  defp typespec_to_ast({:remote_type, line, [{:atom, _, :elixir}, {:atom, _, :as_boolean}, [arg]]}) do
    typespec_to_ast({:type, line, :as_boolean, [arg]})
  end

  defp typespec_to_ast({:remote_type, line, [{:atom, _, :elixir}, {:atom, _, :keyword}, args]}) do
    typespec_to_ast({:type, line, :keyword, args})
  end

  defp typespec_to_ast({:remote_type, line, [mod, name, args]}) do
    args = for arg <- args, do: typespec_to_ast(arg)
    dot  = {:., [line: line], [typespec_to_ast(mod), typespec_to_ast(name)]}
    {dot, [line: line], args}
  end

  defp typespec_to_ast({:ann_type, line, [var, type]}) do
    {:::, [line: line], [typespec_to_ast(var), typespec_to_ast(type)]}
  end

  defp typespec_to_ast({:typed_record_field,
                         {:record_field, line, {:atom, line1, name}},
                         type}) do
    typespec_to_ast({:ann_type, line, [{:var, line1, name}, type]})
  end

  defp typespec_to_ast({:type, _, :any}) do
    quote do: ...
  end

  defp typespec_to_ast({:paren_type, _, [type]}) do
    typespec_to_ast(type)
  end

  defp typespec_to_ast({t, _line, atom}) when is_atom(t) do
    atom
  end

  defp typespec_to_ast(other), do: other

  defp erl_to_ex_var(var) do
    case Atom.to_string(var) do
      <<"_", c::binary-1, rest::binary>> ->
        String.to_atom("_#{String.downcase(c)}#{rest}")
      <<c::binary-1, rest::binary>> ->
        String.to_atom("#{String.downcase(c)}#{rest}")
    end
  end

  ## To typespec conversion

  defp line(meta) do
    case :lists.keyfind(:line, 1, meta) do
      {:line, line} -> line
      false -> 0
    end
  end

  # Handle unions
  defp typespec({:|, meta, [_, _]} = exprs, vars, caller) do
    exprs = collect_union(exprs)
    union = for e <- exprs, do: typespec(e, vars, caller)
    {:type, line(meta), :union, union}
  end

  # Handle binaries
  defp typespec({:<<>>, meta, []}, _, _) do
    {:type, line(meta), :binary, [{:integer, line(meta), 0}, {:integer, line(meta), 0}]}
  end

  defp typespec({:<<>>, meta, [{:::, unit_meta, [{:_, _, ctx1}, {:*, _, [{:_, _, ctx2}, unit]}]}]}, _, _)
      when is_atom(ctx1) and is_atom(ctx2) and is_integer(unit) do
    {:type, line(meta), :binary, [{:integer, line(meta), 0}, {:integer, line(unit_meta), unit}]}
  end

  defp typespec({:<<>>, meta, [{:::, size_meta, [{:_, _, ctx}, size]}]}, _, _)
      when is_atom(ctx) and is_integer(size) do
    {:type, line(meta), :binary, [{:integer, line(size_meta), size}, {:integer, line(meta), 0}]}
  end

  defp typespec({:<<>>, meta, [{:::, size_meta, [{:_, _, ctx1}, size]}, {:::, unit_meta, [{:_, _, ctx2}, {:*, _, [{:_, _, ctx3}, unit]}]}]}, _, _)
      when is_atom(ctx1) and is_atom(ctx2) and is_atom(ctx3) and is_integer(size) and is_integer(unit) do
    {:type, line(meta), :binary, [{:integer, line(size_meta), size}, {:integer, line(unit_meta), unit}]}
  end

  ## Handle maps and structs
  defp typespec({:map, meta, args}, _vars, _caller) when args == [] or is_atom(args) do
    {:type, line(meta), :map, :any}
  end

  defp typespec({:%{}, meta, fields} = map, vars, caller) do
    fields =
      :lists.map(fn
        {k, v} when is_atom(k) ->
          {:type, line(meta), :map_field_exact, [typespec(k, vars, caller), typespec(v, vars, caller)]}
        {{:required, meta2, [k]}, v} ->
          {:type, line(meta2), :map_field_exact, [typespec(k, vars, caller), typespec(v, vars, caller)]}
        {{:optional, meta2, [k]}, v} ->
          {:type, line(meta2), :map_field_assoc, [typespec(k, vars, caller), typespec(v, vars, caller)]}
        {k, v} ->
          # TODO: Emit warnings on v1.5
          # :elixir_errors.warn(caller.line, caller.file,
          #   "invalid map specification. %{foo => bar} is deprecated in favor of " <>
          #   "%{required(foo) => bar} and %{optional(foo) => bar}. required/1 is an " <>
          #   "OTP 19 only feature, if you are targeting OTP 18 use optional/1.")
          {:type, line(meta), :map_field_assoc, [typespec(k, vars, caller), typespec(v, vars, caller)]}
        {:|, _, [_, _]} ->
          compile_error(caller,
            "invalid map specification. When using the | operator in the map key, " <>
            "make sure to wrap the key type in parentheses: #{Macro.to_string(map)}")
        _ ->
          compile_error(caller, "invalid map specification: #{Macro.to_string(map)}")
      end, fields)

    {:type, line(meta), :map, fields}
  end

  defp typespec({:%, _, [name, {:%{}, meta, fields}]}, vars, caller) do
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

    struct = struct |> Map.from_struct |> Map.to_list

    unless Keyword.keyword?(fields) do
      compile_error(caller, "expected key-value pairs in struct #{Macro.to_string(name)}")
    end

    types =
      :lists.map(fn {field, _} ->
        {field, Keyword.get(fields, field, quote(do: term()))}
      end, struct)

    :lists.foreach(fn {field, _} ->
      unless Keyword.has_key?(struct, field) do
        compile_error(caller, "undefined field #{field} on struct #{Macro.to_string(name)}")
      end
    end, fields)

    typespec({:%{}, meta, [__struct__: module] ++ types}, vars, caller)
  end

  # Handle records
  defp typespec({:record, meta, [atom]}, vars, caller) do
    typespec({:record, meta, [atom, []]}, vars, caller)
  end

  defp typespec({:record, meta, [atom, fields]}, vars, caller) do
    # We cannot set a function name to avoid tracking
    # as a compile time dependency because for records it actually is one.
    case Macro.expand({atom, [], [{atom, [], []}]}, caller) do
      keyword when is_list(keyword) ->
        types =
          :lists.map(fn {field, _} ->
            Keyword.get(fields, field, quote(do: term()))
          end, keyword)

        :lists.foreach(fn {field, _} ->
          unless Keyword.has_key?(keyword, field) do
            compile_error(caller, "undefined field #{field} on record #{inspect atom}")
          end
        end, fields)

        typespec({:{}, meta, [atom | types]}, vars, caller)
      _ ->
        compile_error(caller, "unknown record #{inspect atom}")
    end
  end

  # Handle ranges
  defp typespec({:.., meta, args}, vars, caller) do
    args = for arg <- args, do: typespec(arg, vars, caller)
    {:type, line(meta), :range, args}
  end

  # Handle special forms
  defp typespec({:__MODULE__, _, atom}, vars, caller) when is_atom(atom) do
    typespec(caller.module, vars, caller)
  end

  defp typespec({:__aliases__, _, _} = alias, vars, caller) do
    # We set a function name to avoid tracking
    # aliases in typespecs as compile time dependencies.
    atom = Macro.expand(alias, %{caller | function: {:typespec, 0}})
    typespec(atom, vars, caller)
  end

  # Handle funs
  defp typespec([{:->, meta, [arguments, return]}], vars, caller) when is_list(arguments) do
    args = fn_args(meta, arguments, return, vars, caller)
    {:type, line(meta), :fun, args}
  end

  # Handle type operator
  defp typespec({:::, meta, [var, expr]}, vars, caller) do
    left  = typespec(var, [elem(var, 0) | vars], caller)
    right = typespec(expr, vars, caller)
    {:ann_type, line(meta), [left, right]}
  end

  # Handle unary ops
  defp typespec({op, meta, [integer]}, _, _) when op in [:+, :-] and is_integer(integer) do
    {:op, line(meta), op, {:integer, line(meta), integer}}
  end

  # Handle remote calls in the form of @module_attribute.type.
  # These are not handled by the general remote type clause as calling
  # Macro.expand/2 on the remote does not expand module attributes (but expands
  # things like __MODULE__).
  defp typespec({{:., meta, [{:@, _, [{attr, _, _}]}, name]}, _, args} = orig, vars, caller) do
    remote = Module.get_attribute(caller.module, attr)
    unless is_atom(remote) and remote != nil do
      message = "invalid remote in typespec: #{Macro.to_string(orig)} (@#{attr} is #{inspect remote})"
      compile_error(caller, message)
    end
    remote_type({typespec(remote, vars, caller), meta, typespec(name, vars, caller), args}, vars, caller)
  end

  # Handle remote calls
  defp typespec({{:., meta, [remote, name]}, _, args} = orig, vars, caller) do
    # We set a function name to avoid tracking
    # aliases in typespecs as compile time dependencies.
    remote = Macro.expand(remote, %{caller | function: {:typespec, 0}})
    unless is_atom(remote) do
      compile_error(caller, "invalid remote in typespec: #{Macro.to_string(orig)}")
    end
    remote_type({typespec(remote, vars, caller), meta, typespec(name, vars, caller), args}, vars, caller)
  end

  # Handle tuples
  defp typespec({:tuple, meta, args}, _vars, _caller) when args == [] or is_atom(args) do
    {:type, line(meta), :tuple, :any}
  end

  defp typespec({:{}, meta, t}, vars, caller) when is_list(t) do
    args = for e <- t, do: typespec(e, vars, caller)
    {:type, line(meta), :tuple, args}
  end

  defp typespec({left, right}, vars, caller) do
    typespec({:{}, [], [left, right]}, vars, caller)
  end

  # Handle blocks
  defp typespec({:__block__, _meta, [arg]}, vars, caller) do
    typespec(arg, vars, caller)
  end

  # Handle variables or local calls
  defp typespec({name, meta, atom}, vars, caller) when is_atom(atom) do
    if :lists.member(name, vars) do
      {:var, line(meta), name}
    else
      typespec({name, meta, []}, vars, caller)
    end
  end

  # Handle local calls
  defp typespec({type, meta, arguments}, vars, caller) when type in [:string, :nonempty_string] do
    :elixir_errors.warn caller.line, caller.file, "#{type}() type use is discouraged. For character lists, use " <>
      "charlist() type, for strings, String.t()\n#{Exception.format_stacktrace(Macro.Env.stacktrace(caller))}"
    arguments = for arg <- arguments, do: typespec(arg, vars, caller)
    {:type, line(meta), type, arguments}
  end

  # TODO: Deprecate char_list type by v1.5
  defp typespec({type, _meta, []}, vars, caller) when type in [:charlist, :char_list] do
    typespec((quote do: :elixir.charlist()), vars, caller)
  end

  defp typespec({:struct, _meta, []}, vars, caller) do
    typespec((quote do: :elixir.struct()), vars, caller)
  end

  defp typespec({:as_boolean, _meta, [arg]}, vars, caller) do
    typespec((quote do: :elixir.as_boolean(unquote(arg))), vars, caller)
  end

  defp typespec({:keyword, _meta, args}, vars, caller) when length(args) <= 1 do
    typespec((quote do: :elixir.keyword(unquote_splicing(args))), vars, caller)
  end

  defp typespec({:fun, meta, args}, vars, caller) do
    args = for arg <- args, do: typespec(arg, vars, caller)
    {:type, line(meta), :fun, args}
  end

  defp typespec({name, meta, arguments}, vars, caller) do
    arguments = for arg <- arguments, do: typespec(arg, vars, caller)
    arity = length(arguments)
    type = if :erl_internal.is_type(name, arity), do: :type, else: :user_type
    {type, line(meta), name, arguments}
  end

  # Handle literals
  defp typespec(atom, _, _) when is_atom(atom) do
    {:atom, 0, atom}
  end

  defp typespec(integer, _, _) when is_integer(integer) do
    {:integer, 0, integer}
  end

  defp typespec([], vars, caller) do
    typespec({nil, [], []}, vars, caller)
  end

  defp typespec([{:..., _, atom}], vars, caller) when is_atom(atom) do
    typespec({:nonempty_list, [], []}, vars, caller)
  end

  defp typespec([spec, {:..., _, atom}], vars, caller) when is_atom(atom) do
    typespec({:nonempty_list, [], [spec]}, vars, caller)
  end

  defp typespec([spec], vars, caller) do
    typespec({:list, [], [spec]}, vars, caller)
  end

  defp typespec(list, vars, caller) when is_list(list) do
    [h | t] = :lists.reverse(list)
    union = :lists.foldl(fn(x, acc) ->
      {:|, [], [validate_kw(x, list, caller), acc]}
    end, validate_kw(h, list, caller), t)
    typespec({:list, [], [union]}, vars, caller)
  end

  defp typespec(other, _vars, caller) do
    compile_error(caller, "unexpected expression in typespec: #{Macro.to_string other}")
  end

  ## Helpers

  defp compile_error(caller, desc) do
    raise CompileError, file: caller.file, line: caller.line, description: desc
  end

  defp remote_type({remote, meta, name, arguments}, vars, caller) do
    arguments = for arg <- arguments, do: typespec(arg, vars, caller)
    {:remote_type, line(meta), [ remote, name, arguments ]}
  end

  defp collect_union({:|, _, [a, b]}), do: [a | collect_union(b)]
  defp collect_union(v), do: [v]

  defp validate_kw({key, _} = t, _, _caller) when is_atom(key), do: t
  defp validate_kw(_, original, caller) do
    compile_error(caller, "unexpected list in typespec: #{Macro.to_string original}")
  end

  defp fn_args(meta, args, return, vars, caller) do
    case [fn_args(meta, args, vars, caller), typespec(return, vars, caller)] do
      [{:type, _, :any}, {:type, _, :any, []}] -> []
      x -> x
    end
  end

  defp fn_args(meta, [{:..., _, _}], _vars, _caller) do
    {:type, line(meta), :any}
  end

  defp fn_args(meta, args, vars, caller) do
    args = for arg <- args, do: typespec(arg, vars, caller)
    {:type, line(meta), :product, args}
  end

  defp variable({name, meta, _}) do
    {:var, line(meta), name}
  end

  defp unpack_typespec_kw([{:type, _, :tuple, [{:atom, _, atom}, type]} | t], acc) do
    unpack_typespec_kw(t, [{atom, typespec_to_ast(type)} | acc])
  end

  defp unpack_typespec_kw([], acc) do
    {:ok, :lists.reverse(acc)}
  end

  defp unpack_typespec_kw(_, _acc) do
    :error
  end
end
