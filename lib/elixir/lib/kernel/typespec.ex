defmodule Kernel.Typespec do
  @moduledoc """
  Provides macros and functions for working with typespecs.

  The attributes `@type`, `@opaque`, `@typep`, `@spec` and
  `@callback` available in modules are handled by the equivalent
  macros defined by this module.

  ## Defining a type

      @type type_name :: type
      @typep type_name :: type
      @opaque type_name :: type

  For more details, see documentation for `deftype`, `deftypep` and `defopaque`
  below.

  ## Defining a specification

      @spec function_name(type, type) :: type
      @callback function_name(type, type) :: type

  For more details, see documentation for `defspec` and `defcallback` below.

  ## Types

  The type syntax provided by Elixir is fairly similar to the one
  in Erlang.

  Most of the built-in types provided in Erlang (for example, `pid()`)
  are expressed the same way: `pid()` or simply `pid`. Parametrized types
  are also supported (`list(integer())`) and so are remote types (`Enum.t`).

  Certain data type shortcuts (`[...]`, `<<>>` and `{...}`) are supported as
  well.

  Main differences lie in how bit strings and functions are defined:

  ### Bit Strings

  Bit string with a base size of 3:

      <<_ :: 3>>

  Bit string with a unit size of 8:

      <<_ :: _ * 8>>

  ### Anonymous functions

  Any anonymous function:

      ((...) -> any)
      or
      (... -> any)

  Anonymous function with arity of zero:

      (() -> type)

  Anonymous function with some arity:

      ((type, type) -> type)
      or
      (type, type -> type)

  ## Notes

  Elixir discourages the use of type `string()` as it might be confused
  with binaries which are referred to as "strings" in Elixir (as opposed to
  character lists). In order to use the type that is called `string()` in Erlang,
  one has to use the `char_list()` type which is a synonym for `string()`. If you
  use `string()`, you'll get a warning from the compiler.

  If you want to refer to the "string" type (the one operated by functions in the
  String module), use `String.t()` type instead.

  See http://www.erlang.org/doc/reference_manual/typespec.html
  for more information.
  """

  @doc """
  Defines a type.
  This macro is the one responsible for handling the attribute `@type`.

  ## Examples

      @type my_type :: atom

  """
  defmacro deftype(type) do
    quote do
      Kernel.Typespec.deftype(:type, unquote(Macro.escape type), __ENV__)
    end
  end

  @doc """
  Defines an opaque type.
  This macro is the one responsible for handling the attribute `@opaque`.

  ## Examples

      @opaque my_type :: atom

  """
  defmacro defopaque(type) do
    quote do
      Kernel.Typespec.deftype(:opaque, unquote(Macro.escape type), __ENV__)
    end
  end

  @doc """
  Defines a private type.
  This macro is the one responsible for handling the attribute `@typep`.

  ## Examples

      @typep my_type :: atom

  """
  defmacro deftypep(type) do
    quote do
      Kernel.Typespec.deftype(:typep, unquote(Macro.escape type), __ENV__)
    end
  end

  @doc """
  Defines a spec.
  This macro is the one responsible for handling the attribute `@spec`.

  ## Examples

      @spec add(number, number) :: number

  """
  defmacro defspec(spec) do
    quote do
      Kernel.Typespec.defspec(:spec, unquote(Macro.escape spec), __ENV__)
    end
  end

  @doc """
  Defines a callback.
  This macro is the one responsible for handling the attribute `@callback`.

  ## Examples

      @callback add(number, number) :: number

  """
  defmacro defcallback(spec) do
    quote do
      Kernel.Typespec.defspec(:callback, unquote(Macro.escape spec), __ENV__)
    end
  end

  ## Helpers

  @doc """
  Defines a `type`, `typep` or `opaque` by receiving Erlang's typespec.
  """
  def define_type(caller, kind, { name, _, vars } = type) when kind in [:type, :typep, :opaque] do
    { kind, export } =
      case kind do
        :type   -> { :type, true }
        :typep  -> { :type, false }
        :opaque -> { :opaque, true }
      end

    module = caller.module
    arity  = length(vars)

    Module.compile_typespec module, kind, type

    if export do
      Module.compile_typespec(module, :export_type, [{ name, arity }])
    end

    define_doc(caller, kind, name, arity, export)
    type
  end

  defp define_doc(caller, kind, name, arity, export) do
    module = caller.module
    doc    = Module.get_attribute(module, :typedoc)

    if doc do
      if export do
        Module.add_doc(module, caller.line, kind, { name, arity }, doc)
      else
        :elixir_errors.warn "#{caller.file}:#{caller.line}: type #{name}/#{arity} is private, " <>
                            "@typedoc's are always discarded for private types\n"
      end
    end

    Module.delete_attribute(module, :typedoc)
  end

  @doc """
  Defines a `spec` by receiving Erlang's typespec.
  """
  def define_spec(module, tuple, definition) do
    Module.compile_typespec module, :spec, { tuple, definition }
  end

  @doc """
  Defines a `callback` by receiving Erlang's typespec.
  """
  def define_callback(module, tuple, definition) do
    Module.compile_typespec module, :callback, { tuple, definition }
  end

  @doc """
  Returns `true` if the current module defines a given type
  (private, opaque or not). This function is only available
  for modules being compiled.
  """
  def defines_type?(module, name, arity) do
    finder = &match?({ ^name, _, vars } when length(vars) == arity, &1)
    :lists.any(finder, Module.get_attribute(module, :type)) or
      :lists.any(finder, Module.get_attribute(module, :opaque))
  end

  @doc """
  Returns `true` if the current module defines a given spec.
  This function is only available for modules being compiled.
  """
  def defines_spec?(module, name, arity) do
    tuple = { name, arity }
    :lists.any(&match?(^tuple, &1), Module.get_attribute(module, :spec))
  end

  @doc """
  Returns `true` if the current module defines a callback.
  This function is only available for modules being compiled.
  """
  def defines_callback?(module, name, arity) do
    tuple = { name, arity }
    :lists.any(&match?(^tuple, &1), Module.get_attribute(module, :callback))
  end

  @doc """
  Converts a spec clause back to Elixir AST.
  """
  def spec_to_ast(name, { :type, line, :fun, [{:type, _, :product, args}, result] }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { :::, [line: line], [{ name, [line: line], args }, typespec_to_ast(result)] }
  end

  def spec_to_ast(name, { :type, line, :fun, [] }) do
    { :::, [line: line], [{ name, [line: line], [] }, quote(do: term)] }
  end

  def spec_to_ast(name, { :type, line, :bounded_fun, [{ :type, _, :fun, [{ :type, _, :product, args }, result] }, constraints] }) do
    [h|t] =
      lc {:type, line, :constraint, [{:atom, _, :is_subtype}, [var, type]]} inlist constraints do
        { :is_subtype, [line: line], [typespec_to_ast(var), typespec_to_ast(type)] }
      end

    args = lc arg inlist args, do: typespec_to_ast(arg)
    guards = Enum.reduce t, h, fn(x, acc) -> { :and, line, [acc, x] } end

    { :::, [line: line], [{ :when, [line: line], [{ name, [line: line], args }, guards] }, typespec_to_ast(result)] }
  end

  @doc """
  Converts a type clause back to Elixir AST.
  """
  def type_to_ast({ { :record, record }, fields, args }) when is_atom(record) do
    fields = lc field inlist fields, do: typespec_to_ast(field)
    args = lc arg inlist args, do: typespec_to_ast(arg)
    type = { :{}, [], [record|fields] }
    quote do: unquote(record)(unquote_splicing(args)) :: unquote(type)
  end

  def type_to_ast({ name, type, args }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  @doc """
  Returns all type docs available from the module's beam code.

  It is returned as a list of tuples where the first element is the pair of type
  name and arity and the second element is the documentation.

  The module has to have a corresponding beam file on the disk which can be
  located by the runtime system.
  """
  # This is in Kernel.Typespec because it works very similar to beam_types and
  # uses some of the introspection available here.
  def beam_typedocs(module) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        type_docs = lc { :attribute, _, :typedoc, tup } inlist abstract_code, do: tup
        List.flatten(type_docs)
      _ ->
        []
    end
  end

  @doc """
  Returns all types available from the module's beam code.

  It is returned as a list of tuples where the first
  element is the type (`:typep`, `:type` and `:opaque`).

  The module has to have a corresponding beam file on the disk which can be
  located by the runtime system.
  """
  def beam_types(module) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        exported_types = lc { :attribute, _, :export_type, types } inlist abstract_code, do: types
        exported_types = List.flatten(exported_types)

        lc { :attribute, _, kind, { name, _, args } = type } inlist abstract_code, kind in [:opaque, :type] do
          cond do
            kind == :opaque -> { :opaque, type }
            :lists.member({ name, length(args) }, exported_types) -> { :type, type }
            true -> { :typep, type }
          end
        end
      _ ->
        []
    end
  end

  @doc """
  Returns all specs available from the module's beam code.

  It is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module has to have a corresponding beam file on the disk which can be
  located by the runtime system.
  """
  def beam_specs(module) do
    from_abstract_code(module, :spec)
  end

  @doc """
  Returns all callbacks available from the module's beam code.

  It is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module has to have a corresponding beam file on the disk which can be
  located by the runtime system.
  """
  def beam_callbacks(module) do
    from_abstract_code(module, :callback)
  end

  defp from_abstract_code(module, kind) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        lc { :attribute, _, abs_kind, value } inlist abstract_code, kind == abs_kind, do: value
      _ ->
        []
    end
  end

  defp abstract_code(module) do
    case :beam_lib.chunks(abstract_code_beam(module), [:abstract_code]) do
      {:ok, { _, [{ :abstract_code, { _raw_abstract_v1, abstract_code } }] } } ->
        { :ok, abstract_code }
      _ ->
        []
    end
  end

  defp abstract_code_beam(module) when is_atom(module) do
    case :code.get_object_code(module) do
      { ^module, beam, _filename } -> beam
      :error -> module
    end
  end

  defp abstract_code_beam(binary) when is_binary(binary) do
    binary
  end

  ## Macro callbacks

  @doc false
  def deftype(kind, { :::, _, [type, definition] }, caller) do
    do_deftype(kind, type, definition, caller)
  end

  def deftype(kind, {name, _meta, args} = type, caller)
      when is_atom(name) and not is_list(args) do
    do_deftype(kind, type, { :term, [line: caller.line], nil }, caller)
  end

  def deftype(_kind, other, caller) do
    type_spec = Macro.to_string(other)
    compile_error caller, "invalid type specification #{type_spec}"
  end

  defp do_deftype(kind, { name, _, args }, definition, caller) do
    args =
      if is_atom(args) do
        []
      else
        lc(arg inlist args, do: variable(arg))
      end

    vars = lc { :var, _, var } inlist args, do: var
    spec = typespec(definition, vars, caller)

    vars = lc { :var, _, _ } = var inlist args, do: var
    type = { name, spec, vars }

    define_type(caller, kind, type)
  end

  @doc false
  def defspec(type, {:::, _, [{ :when, _, [{ name, meta, args }, constraints_guard] }, return] }, caller) do
    if is_atom(args), do: args = []
    constraints = guard_to_constraints(constraints_guard, caller)
    spec = { :type, line(meta), :fun, fn_args(meta, args, return, Keyword.keys(constraints), caller) }
    spec = { :type, line(meta), :bounded_fun, [spec, Keyword.values(constraints)] }
    code = { { name, Kernel.length(args) }, spec }
    Module.compile_typespec(caller.module, type, code)
    code
  end

  def defspec(type, {:::, _, [{ name, meta, args }, return]}, caller) do
    if is_atom(args), do: args = []
    spec = { :type, line(meta), :fun, fn_args(meta, args, return, [], caller) }
    code = { { name, Kernel.length(args) }, spec }
    Module.compile_typespec(caller.module, type, code)
    code
  end

  def defspec(_type, other, caller) do
    spec = Macro.to_string(other)
    compile_error caller, "invalid function type specification #{spec}"
  end

  defp guard_to_constraints({ :is_subtype, meta, [{ name, _, _ }, type] }, caller) do
    line = line(meta)
    contraints = [{ :atom, line, :is_subtype }, [{:var, line, name}, typespec(type, [], caller)]]
    [{ name, { :type, line, :constraint, contraints } }]
  end

  defp guard_to_constraints({ :and, _, [left, right] }, caller) do
    guard_to_constraints(left, caller) ++ guard_to_constraints(right, caller)
  end

  ## To AST conversion

  defp typespec_to_ast({ :type, line, :tuple, :any }) do
    typespec_to_ast({:type, line, :tuple, []})
  end

  defp typespec_to_ast({ :type, line, :tuple, args }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { :{}, [line: line], args }
  end

  defp typespec_to_ast({ :type, _line, :list, [arg] }) do
    case unpack_typespec_kw(arg, []) do
      { :ok, ast } -> ast
      :error -> [typespec_to_ast(arg)]
    end
  end

  defp typespec_to_ast({ :type, _line, :list, args }) do
    lc arg inlist args, do: typespec_to_ast(arg)
  end

  defp typespec_to_ast({ :type, line, :binary, [arg1, arg2] }) do
    [arg1, arg2] = lc arg inlist [arg1, arg2], do: typespec_to_ast(arg)
    cond do
      arg2 == 0 ->
        quote line: line, do: <<_ :: unquote(arg1)>>
      arg1 == 0 ->
        quote line: line, do: <<_ :: _ * unquote(arg2)>>
      true ->
        quote line: line, do: <<_ :: unquote(arg1) * unquote(arg2)>>
    end
  end

  defp typespec_to_ast({ :type, line, :union, args }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    Enum.reduce tl(args), hd(args),
      fn(arg, expr) -> { :|, [line: line], [expr, arg] } end
  end

  defp typespec_to_ast({ :type, line, :fun, [{:type, _, :product, args}, result] }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { :->, [line: line], [{args, [line: line], typespec_to_ast(result)}] }
  end

  defp typespec_to_ast({ :type, line, :fun, [args, result] }) do
    { :->, [line: line], [{[typespec_to_ast(args)], [line: line], typespec_to_ast(result)}] }
  end

  defp typespec_to_ast({ :type, line, :fun, [] }) do
    typespec_to_ast({ :type, line, :fun, [{:type, line, :any}, {:type, line, :any, []} ] })
  end

  defp typespec_to_ast({ :type, line, :range, [left, right] }) do
    { :"..", [line: line], [typespec_to_ast(left), typespec_to_ast(right)] }
  end

  defp typespec_to_ast({ :type, line, name, args }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { name, [line: line], args }
  end

  defp typespec_to_ast({ :var, line, var }) do
    var =
      case atom_to_binary(var) do
        <<"_", c :: [binary, size(1)], rest :: binary>> ->
          binary_to_atom("_#{String.downcase(c)}#{rest}")
        <<c :: [binary, size(1)], rest :: binary>> ->
          binary_to_atom("#{String.downcase(c)}#{rest}")
      end
    { var, line, nil }
  end

  # Special shortcut(s)
  defp typespec_to_ast({ :remote_type, line, [{:atom, _, :elixir}, {:atom, _, :char_list}, []] }) do
    typespec_to_ast({:type, line, :char_list, []})
  end

  defp typespec_to_ast({ :remote_type, line, [{:atom, _, :elixir}, {:atom, _, :as_boolean}, [arg]] }) do
    typespec_to_ast({:type, line, :as_boolean, [arg]})
  end

  defp typespec_to_ast({ :remote_type, line, [mod, name, args] }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    dot  = { :., [line: line], [typespec_to_ast(mod), typespec_to_ast(name)] }
    { dot, [line: line], args }
  end

  defp typespec_to_ast({ :ann_type, line, [var, type] }) do
    { :::, [line: line], [typespec_to_ast(var), typespec_to_ast(type)] }
  end

  defp typespec_to_ast({ :typed_record_field,
                         { :record_field, line, { :atom, line1, name }},
                         type }) do
    typespec_to_ast({ :ann_type, line, [{ :var, line1, name }, type] })
  end

  defp typespec_to_ast({:type, _, :any}) do
    quote do: ...
  end

  defp typespec_to_ast({:paren_type, _, [type]}) do
    typespec_to_ast(type)
  end

  defp typespec_to_ast({ t, _line, atom }) when is_atom(t) do
    atom
  end

  defp typespec_to_ast(other), do: other

  ## From AST conversion

  defp line(meta) do
    case :lists.keyfind(:line, 1, meta) do
      { :line, line } -> line
      false -> 0
    end
  end

  # Handle unions
  defp typespec({ :|, meta, [_, _] } = exprs, vars, caller) do
    exprs = Enum.reverse(collect_union(exprs))
    union = lc e inlist exprs, do: typespec(e, vars, caller)
    { :type, line(meta), :union, union }
  end

  # Handle binaries
  defp typespec({:<<>>, meta, []}, _, _) do
    {:type, line(meta), :binary, [{:integer, line(meta), 0}, {:integer, line(meta), 0}]}
  end

  defp typespec({:<<>>, meta, [{:::, _, [{:_, meta1, atom}, {:*, _, [{:_, meta2, atom}, unit]}]}]}, _, _) when is_atom(atom) do
    {:type, line(meta), :binary, [{:integer, line(meta1), 0}, {:integer, line(meta2), unit}]}
  end

  defp typespec({:<<>>, meta, [{:::, meta1, [{:_, meta2, atom}, base]}]}, _, _) when is_atom(atom) do
    {:type, line(meta), :binary, [{:integer, line(meta1), base}, {:integer, line(meta2), 0}]}
  end

  # Handle ranges
  defp typespec({:"..", meta, args}, vars, caller) do
    typespec({:range, meta, args}, vars, caller)
  end

  # Handle special forms
  defp typespec({:__MODULE__, _, atom}, vars, caller) when is_atom(atom) do
    typespec(caller.module, vars, caller)
  end

  defp typespec({:__aliases__, _, _} = alias, vars, caller) do
    atom = Macro.expand alias, caller
    typespec(atom, vars, caller)
  end

  # Handle funs
  defp typespec({:->, meta, [{[{:fun, _, arguments}], cmeta, return}]}, vars, caller) when is_list(arguments) do
    typespec({:->, meta, [{arguments, cmeta, return}]}, vars, caller)
  end

  defp typespec({:->, meta, [{arguments, _, return}]}, vars, caller) when is_list(arguments) do
    args = fn_args(meta, arguments, return, vars, caller)
    { :type, line(meta), :fun, args }
  end

  # Handle type operator
  defp typespec({:"::", meta, [var, expr] }, vars, caller) do
    left  = typespec(var, [elem(var, 0)|vars], caller)
    right = typespec(expr, vars, caller)
    { :ann_type, line(meta), [left, right] }
  end

  # Handle unary ops
  defp typespec({op, meta, [integer]}, _, _) when op in [:+, :-] and is_integer(integer) do
    { :op, line(meta), op, {:integer, line(meta), integer} }
  end

  # Handle access macro
  defp typespec({{:., meta, [Kernel, :access]}, meta1, [target, args]}, vars, caller) do
    access = {{:., meta, [Kernel, :access]}, meta1,
              [target, args ++ [_: { :any, [], [] }]]}
    typespec(Macro.expand(access, caller), vars, caller)
  end

  # Handle remote calls
  defp typespec({{:., meta, [remote, name]}, _, args} = orig, vars, caller) do
    remote = Macro.expand remote, caller
    unless is_atom(remote) do
      compile_error(caller, "invalid remote in typespec: #{Macro.to_string(orig)}")
    end
    remote_type({typespec(remote, vars, caller), meta, typespec(name, vars, caller), args}, vars, caller)
  end

  # Handle tuples
  defp typespec({:tuple, meta, atom}, vars, caller) when is_atom(atom) do
    typespec({:{}, meta, []}, vars, caller)
  end

  defp typespec({:{}, meta, []}, _, _) do
    { :type, line(meta), :tuple, :any }
  end

  defp typespec({:{}, meta, t}, vars, caller) when is_list(t) do
    args = lc e inlist t, do: typespec(e, vars, caller)
    { :type, line(meta), :tuple, args }
  end

  # Handle blocks
  defp typespec({:__block__, _meta, [arg]}, vars, caller) do
    typespec(arg, vars, caller)
  end

  # Handle variables or local calls
  defp typespec({name, meta, atom}, vars, caller) when is_atom(atom) do
    if :lists.member(name, vars) do
      { :var, line(meta), name }
    else
      typespec({name, meta, []}, vars, caller)
    end
  end

  # Handle local calls
  defp typespec({:string, meta, arguments}, vars, caller) do
    IO.write "warning: string() type use is discouraged. For character lists, use " <>
      "char_list() type, for strings, String.t()\n#{Exception.format_stacktrace(caller.stacktrace)}"
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    { :type, line(meta), :string, arguments }
  end

  defp typespec({:char_list, _meta, arguments}, vars, caller) do
    typespec((quote do: :elixir.char_list(unquote_splicing(arguments))), vars, caller)
  end

  defp typespec({:as_boolean, _meta, arguments}, vars, caller) do
    typespec((quote do: :elixir.as_boolean(unquote_splicing(arguments))), vars, caller)
  end

  defp typespec({name, meta, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    { :type, line(meta), name, arguments }
  end

  # Handle literals
  defp typespec(atom, _, _) when is_atom(atom) do
    { :atom, 0, atom }
  end

  defp typespec(integer, _, _) when is_integer(integer) do
    { :integer, 0, integer }
  end

  defp typespec([], vars, caller) do
    typespec({ nil, [], [] }, vars, caller)
  end

  defp typespec([spec], vars, caller) do
    typespec({ :list, [], [spec] }, vars, caller)
  end

  defp typespec([spec, {:"...", _, quoted}], vars, caller) when is_atom(quoted) do
    typespec({ :nonempty_list, [], [spec] }, vars, caller)
  end

  defp typespec([h|t] = l, vars, caller) do
    union = Enum.reduce(t, validate_kw(h, l, caller), fn(x, acc) ->
      { :|, [], [acc, validate_kw(x, l, caller)] }
    end)
    typespec({ :list, [], [union] }, vars, caller)
  end

  defp typespec(t, vars, caller) when is_tuple(t) do
    args = lc e inlist tuple_to_list(t), do: typespec(e, vars, caller)
    { :type, 0, :tuple, args }
  end

  ## Helpers

  defp compile_error(caller, desc) do
    raise CompileError, file: caller.file, line: caller.line, description: desc
  end

  defp remote_type({remote, meta, name, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    { :remote_type, line(meta), [ remote, name, arguments ] }
  end

  defp collect_union({ :|, _, [a, b] }), do: [b|collect_union(a)]
  defp collect_union(v), do: [v]

  defp validate_kw({ key, _ } = t, _, _caller) when is_atom(key), do: t
  defp validate_kw(_, original, caller) do
    compile_error(caller, "unexpected list #{Macro.to_string original} in typespec")
  end

  defp fn_args(meta, args, return, vars, caller) do
    case [fn_args(meta, args, vars, caller), typespec(return, vars, caller)] do
      [{:type, _, :any}, {:type, _, :any, []}] -> []
      x -> x
    end
  end

  defp fn_args(meta, [{:"...", _, _}], _vars, _caller) do
    { :type, line(meta), :any }
  end

  defp fn_args(meta, args, vars, caller) do
    args = lc arg inlist args, do: typespec(arg, vars, caller)
    { :type, line(meta), :product, args }
  end

  defp variable({name, meta, _}) do
    {:var, line(meta), name}
  end

  defp unpack_typespec_kw({ :type, _, :union, [
         next,
         { :type, _, :tuple, [{ :atom, _, atom }, type] }
       ] }, acc) do
    unpack_typespec_kw(next, [{atom, typespec_to_ast(type)}|acc])
  end

  defp unpack_typespec_kw({ :type, _, :tuple, [{ :atom, _, atom }, type] }, acc) do
    { :ok, [{atom, typespec_to_ast(type)}|acc] }
  end

  defp unpack_typespec_kw(_, _acc) do
    :error
  end
end
