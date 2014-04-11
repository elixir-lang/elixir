defmodule Kernel.Typespec do
  @moduledoc """
  Provides macros and functions for working with typespecs.

  Elixir comes with a notation for declaring types and specifications. Elixir is
  dynamically typed, as such typespecs are never used by the compiler to
  optimize or modify code. Still, using typespecs is useful as documentation and
  tools such as [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) can
  analyze the code with typespecs to find bugs.

  The attributes `@type`, `@opaque`, `@typep`, `@spec` and `@callback` available
  in modules are handled by the equivalent macros defined by this module. See
  sub-sections "Defining a type" and "Defining a specification" below.

  ## Types and their syntax

  The type syntax provided by Elixir is fairly similar to the one in
  [Erlang](http://www.erlang.org/doc/reference_manual/typespec.html).

  Most of the built-in types provided in Erlang (for example, `pid()`) are
  expressed the same way: `pid()` or simply `pid`. Parametrized types are also
  supported (`list(integer)`) and so are remote types (`Enum.t`).

  Integers and atom literals are allowed as types (ex. `1`, `:atom` or
  `false`). All other types are built of unions of predefined types. Certain
  shorthands are allowed, such as `[...]`, `<<>>` and `{...}`.

  ### Predefined types

      Type :: any         # the top type, the set of all terms
            | none        # the bottom type, contains no terms
            | pid
            | port
            | reference
            | Atom
            | Bitstring
            | float
            | Fun
            | Integer
            | List
            | Tuple
            | Union
            | UserDefined # Described in section "Defining a type"

      Atom :: atom
            | ElixirAtom # `:foo`, `:bar`, ...

      Bitstring :: <<>>
                 | << _ :: M >>             # M is a positive integer
                 | << _ :: _ * N >>         # N is a positive integer
                 | << _ :: M, _ :: _ * N >>

      Fun :: (... -> any)    # any function
           | (... -> Type)   # any arity, returning Type
           | (() -> Type))
           | (TList -> Type)

      Integer :: integer
               | ElixirInteger                # ..., -1, 0, 1, ... 42 ...
               | ElixirInteger..ElixirInteger # an integer range

      List :: list(Type)                        # proper list ([]-terminated)
            | improper_list(Type1, Type2)       # Type1=contents, Type2=termination
            | maybe_improper_list(Type1, Type2) # Type1 and Type2 as above
            | nonempty_list(Type)               # proper non-empty list
            | []                                # empty list
            | [Type]                            # shorthand for list(Type)
            | [Type, ...]                       # shorthand for nonempty_list(Type)

      Tuple :: tuple     # a tuple of any size
             | {}        # empty tuple
             | { TList }

      TList :: Type
             | Type, TList

      Union :: Type1 | Type2

  ### Bit strings

  Bit string with a base size of 3:

      << _ :: 3 >>

  Bit string with a unit size of 8:

      << _ :: _ * 8 >>

  ### Anonymous functions

  Any anonymous function:

      ((...) -> any)
      (... -> any)

  Anonymous function with arity of zero:

      (() -> type)

  Anonymous function with some arity:

      ((type, type) -> type)
      (type, type -> type)

  ## Built-in types

  Built-in type         | Defined as
  :-------------------- | :---------
  `term`                | `any`
  `binary`              | `<< _ :: _ * 8 >>`
  `bitstring`           | `<< _ :: _ * 1 >>`
  `boolean`             | `false` &#124; `true`
  `byte`                | `0..255`
  `char`                | `0..0xffff`
  `number`              | `integer` &#124; `float`
  `list`                | `[any]`
  `maybe_improper_list` | `maybe_improper_list(any, any)`
  `nonempty_list`       | `nonempty_list(any)`
  `iodata`              | `iolist` &#124; `binary`
  `iolist`              | `maybe_improper_list(byte` &#124; `binary` &#124; `iolist, binary` &#124; `[])`
  `module`              | `atom`
  `mfa`                 | `{ atom, atom, arity }`
  `arity`               | `0..255`
  `node`                | `atom`
  `timeout`             | `:infinity` &#124; `non_neg_integer`
  `no_return`           | `none`
  `fun`                 | `(... -> any)`

  Some built-in types cannot be expressed with valid syntax according to the
  language defined above.

  Built-in type     | Can be interpreted as
  :---------------- | :--------------------
  `non_neg_integer` | `0..`
  `pos_integer`     | `1..`
  `neg_integer`     | `..-1`

  Types defined in other modules are referred to as "remote types", they are
  referenced as `Module.type_name` (ex. `Enum.t` or `String.t`).

  ## Defining a type

      @type type_name :: type
      @typep type_name :: type
      @opaque type_name :: type

  A type defined with `@typep` is private. An opaque type, defined with
  `@opaque` is a type where the internal structure of the type will not be
  visible, but the type is still public.

  Types can be parametrised by defining variables as parameters, these variables
  can then be used to define the type.

      @type dict(key, value) :: [{ key, value }]

  Types can also be defined for records, see `defrecord/3`.

  ## Defining a specification

      @spec function_name(type1, type2) :: return_type
      @callback function_name(type1, type2) :: return_type

  Callbacks are used to define the callbacks functions of behaviours (see
  `Behaviour`).

  Guards can be used to restrict type variables given as arguments to the
  function.

      @spec function(arg) :: [arg] when arg: atom

  Type variables with no restriction can also be defined.

      @spec function(arg) :: [arg] when arg: var

  Specifications can be overloaded just like ordinary functions.

      @spec function(integer) :: atom
      @spec function(atom)    :: integer

  ## Notes

  Elixir discourages the use of type `string` as it might be confused with
  binaries which are referred to as "strings" in Elixir (as opposed to character
  lists). In order to use the type that is called `string` in Erlang, one has to
  use the `char_list` type which is a synonym for `string`. If you use `string`,
  you'll get a warning from the compiler.

  If you want to refer to the "string" type (the one operated on by functions in
  the `String` module), use `String.t` type instead.
  """

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
        :elixir_errors.warn caller.line, caller.file, "type #{name}/#{arity} is private, " <>
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
    Enum.any?(Module.get_attribute(module, :type), finder) or
      Enum.any?(Module.get_attribute(module, :opaque), finder)
  end

  @doc """
  Returns `true` if the current module defines a given spec.
  This function is only available for modules being compiled.
  """
  def defines_spec?(module, name, arity) do
    tuple = { name, arity }
    Enum.any?(Module.get_attribute(module, :spec), &match?(^tuple, &1))
  end

  @doc """
  Returns `true` if the current module defines a callback.
  This function is only available for modules being compiled.
  """
  def defines_callback?(module, name, arity) do
    tuple = { name, arity }
    Enum.any?(Module.get_attribute(module, :callback), &match?(^tuple, &1))
  end

  @doc """
  Converts a spec clause back to Elixir AST.
  """
  def spec_to_ast(name, { :type, line, :fun, [{:type, _, :product, args}, result] }) do
    meta = [line: line]
    body = { name, meta, Enum.map(args, &typespec_to_ast/1) }

    vars = args ++ [result]
      |> Enum.flat_map(&collect_vars/1)
      |> Enum.uniq
      |> Enum.map(&{ &1, { :var, meta, nil } })

    spec = { :::, meta, [body, typespec_to_ast(result)] }

    if vars == [] do
      spec
    else
      { :when, meta, [spec, vars] }
    end
  end

  def spec_to_ast(name, { :type, line, :fun, [] }) do
    { :::, [line: line], [{ name, [line: line], [] }, quote(do: term)] }
  end

  def spec_to_ast(name, { :type, line, :bounded_fun, [{ :type, _, :fun, [{ :type, _, :product, args }, result] }, constraints] }) do
    guards =
      for { :type, _, :constraint, [{ :atom, _, :is_subtype }, [{ :var, _, var }, type]] } <- constraints do
        { var, typespec_to_ast(type) }
      end

    meta = [line: line]

    vars = args ++ [result]
           |> Enum.flat_map(&collect_vars/1)
           |> Enum.uniq
           |> Kernel.--(Keyword.keys(guards))
           |> Enum.map(&{ &1, { :var, meta, nil } })

    args = for arg <- args, do: typespec_to_ast(arg)

    { :when, meta, [
      { :::, meta, [{ name, [line: line], args }, typespec_to_ast(result)] },
      guards ++ vars
    ] }
  end

  @doc """
  Converts a type clause back to Elixir AST.
  """
  def type_to_ast({ { :record, record }, fields, args }) when is_atom(record) do
    fields = for field <- fields, do: typespec_to_ast(field)
    args = for arg <- args, do: typespec_to_ast(arg)
    type = { :{}, [], [record|fields] }
    quote do: unquote(record)(unquote_splicing(args)) :: unquote(type)
  end

  def type_to_ast({ name, type, args }) do
    args = for arg <- args, do: typespec_to_ast(arg)
    quote do: unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  @doc """
  Returns all type docs available from the module's beam code.

  The result is returned as a list of tuples where the first element is the pair of type
  name and arity and the second element is the documentation.

  The module must have a corresponding beam file which can be
  located by the runtime system.
  """
  @spec beam_typedocs(module | binary) :: [tuple] | nil
  def beam_typedocs(module) when is_atom(module) or is_binary(module) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        type_docs = for { :attribute, _, :typedoc, tup } <- abstract_code, do: tup
        List.flatten(type_docs)
      _ ->
        nil
    end
  end

  @doc """
  Returns all types available from the module's beam code.

  The result is returned as a list of tuples where the first
  element is the type (`:typep`, `:type` and `:opaque`).

  The module must have a corresponding beam file which can be
  located by the runtime system.
  """
  @spec beam_types(module | binary) :: [tuple] | nil
  def beam_types(module) when is_atom(module) or is_binary(module) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        exported_types = for { :attribute, _, :export_type, types } <- abstract_code, do: types
        exported_types = List.flatten(exported_types)

        for { :attribute, _, kind, { name, _, args } = type } <- abstract_code, kind in [:opaque, :type] do
          cond do
            kind == :opaque -> { :opaque, type }
            { name, length(args) } in exported_types -> { :type, type }
            true -> { :typep, type }
          end
        end
      _ ->
        nil
    end
  end

  @doc """
  Returns all specs available from the module's beam code.

  The result is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding beam file which can be
  located by the runtime system.
  """
  @spec beam_specs(module | binary) :: [tuple] | nil
  def beam_specs(module) when is_atom(module) or is_binary(module) do
    from_abstract_code(module, :spec)
  end

  @doc """
  Returns all callbacks available from the module's beam code.

  The result is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding beam file
  which can be located by the runtime system.
  """
  @spec beam_callbacks(module | binary) :: [tuple] | nil
  def beam_callbacks(module) when is_atom(module) or is_binary(module) do
    from_abstract_code(module, :callback)
  end

  defp from_abstract_code(module, kind) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        for { :attribute, _, abs_kind, value } <- abstract_code, kind == abs_kind, do: value
      :error ->
        nil
    end
  end

  defp abstract_code(module) do
    case :beam_lib.chunks(abstract_code_beam(module), [:abstract_code]) do
      { :ok, { _, [{ :abstract_code, { _raw_abstract_v1, abstract_code } }] } } ->
        { :ok, abstract_code }
      _ ->
        :error
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
  def deftype(kind, { :::, _, [{ name, _, args }, definition] }, caller) when is_atom(name) and name != ::: do
    args =
      if is_atom(args) do
        []
      else
        for(arg <- args, do: variable(arg))
      end

    vars = for { :var, _, var } <- args, do: var
    spec = typespec(definition, vars, caller)

    vars = for { :var, _, _ } = var <- args, do: var
    type = { name, spec, vars }

    define_type(caller, kind, type)
  end

  def deftype(_kind, other, caller) do
    type_spec = Macro.to_string(other)
    compile_error caller, "invalid type specification: #{type_spec}"
  end

  @doc false

  def defspec(type, { :when, _meta, [spec, guard] }, caller) do
    defspec(type, spec, guard, caller)
  end

  def defspec(type, spec, caller) do
    defspec(type, spec, [], caller)
  end

  defp defspec(type, { :::, meta, [{ name, _, args }, return] }, guard, caller) when is_atom(name) and name != ::: do
    if is_atom(args), do: args = []

    unless Keyword.keyword?(guard) do
      guard = Macro.to_string(guard)
      compile_error caller, "expected keywords as guard in function type specification, got: #{guard}"
    end

    vars = Keyword.keys(guard)
    constraints = guard_to_constraints(guard, vars, meta, caller)

    spec = { :type, line(meta), :fun, fn_args(meta, args, return, vars, caller) }
    if constraints != [] do
      spec = { :type, line(meta), :bounded_fun, [spec, constraints] }
    end

    code = { { name, Kernel.length(args) }, spec }
    Module.compile_typespec(caller.module, type, code)
    code
  end

  defp defspec(_type, spec, _guard, caller) do
    spec = Macro.to_string(spec)
    compile_error caller, "invalid function type specification: #{spec}"
  end

  defp guard_to_constraints(guard, vars, meta, caller) do
    line = line(meta)

    Enum.reduce(guard, [], fn
      { _name, { :var, _, context } }, acc when is_atom(context) ->
        acc
      { name, type }, acc ->
        constraint = [{ :atom, line, :is_subtype }, [{:var, line, name}, typespec(type, vars, caller)]]
        type = { :type, line, :constraint, constraint }
        [type|acc]
    end) |> Enum.reverse
  end

  ## To AST conversion

  defp collect_vars({ :ann_type, _line, args }) when is_list(args) do
    []
  end

  defp collect_vars({ :type, _line, _kind, args }) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({ :remote_type, _line, args }) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({ :typed_record_field, _line, type }) do
    collect_vars(type)
  end

  defp collect_vars({ :paren_type, _line, [type] }) do
    collect_vars(type)
  end

  defp collect_vars({ :var, _line, var }) do
    [erl_to_ex_var(var)]
  end

  defp collect_vars(_) do
    []
  end

  defp typespec_to_ast({ :type, line, :tuple, :any }) do
    { :tuple, [line: line], [] }
  end

  defp typespec_to_ast({ :type, line, :tuple, args }) do
    args = for arg <- args, do: typespec_to_ast(arg)
    { :{}, [line: line], args }
  end

  defp typespec_to_ast({ :type, _line, :list, [{ :type, _, :union, unions } = arg] }) do
    case unpack_typespec_kw(unions, []) do
      { :ok, ast } -> ast
      :error -> [typespec_to_ast(arg)]
    end
  end

  defp typespec_to_ast({ :type, _line, :list, args }) do
    for arg <- args, do: typespec_to_ast(arg)
  end

  defp typespec_to_ast({ :type, line, :map, fields }) do
    fields = Enum.map fields, fn { :type, _, :map_field_assoc, k, v } ->
      { typespec_to_ast(k), typespec_to_ast(v) }
    end

    { struct, fields } = Keyword.pop(fields, :__struct__)
    map = { :%{}, [line: line], fields }

    if struct do
      { :%, [line: line], [struct, map] }
    else
      map
    end
  end

  defp typespec_to_ast({ :type, line, :binary, [arg1, arg2] }) do
    [arg1, arg2] = for arg <- [arg1, arg2], do: typespec_to_ast(arg)
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
    args = for arg <- args, do: typespec_to_ast(arg)
    Enum.reduce Enum.reverse(args), fn(arg, expr) -> { :|, [line: line], [arg, expr] } end
  end

  defp typespec_to_ast({ :type, line, :fun, [{:type, _, :product, args}, result] }) do
    args = for arg <- args, do: typespec_to_ast(arg)
    [{ :->, [line: line], [args, typespec_to_ast(result)] }]
  end

  defp typespec_to_ast({ :type, line, :fun, [args, result] }) do
    [{ :->, [line: line], [[typespec_to_ast(args)], typespec_to_ast(result)] }]
  end

  defp typespec_to_ast({ :type, line, :fun, [] }) do
    typespec_to_ast({ :type, line, :fun, [{:type, line, :any}, {:type, line, :any, []} ] })
  end

  defp typespec_to_ast({ :type, line, :range, [left, right] }) do
    { :"..", [line: line], [typespec_to_ast(left), typespec_to_ast(right)] }
  end

  defp typespec_to_ast({ :type, line, name, args }) do
    args = for arg <- args, do: typespec_to_ast(arg)
    { name, [line: line], args }
  end

  defp typespec_to_ast({ :var, line, var }) do
    { erl_to_ex_var(var), line, nil }
  end

  defp typespec_to_ast({ :op, line, op, arg }) do
    { op, [line: line], [typespec_to_ast(arg)] }
  end

  # Special shortcut(s)
  defp typespec_to_ast({ :remote_type, line, [{:atom, _, :elixir}, {:atom, _, :char_list}, []] }) do
    typespec_to_ast({:type, line, :char_list, []})
  end

  defp typespec_to_ast({ :remote_type, line, [{:atom, _, :elixir}, {:atom, _, :as_boolean}, [arg]] }) do
    typespec_to_ast({:type, line, :as_boolean, [arg]})
  end

  defp typespec_to_ast({ :remote_type, line, [mod, name, args] }) do
    args = for arg <- args, do: typespec_to_ast(arg)
    dot  = { :., [line: line], [typespec_to_ast(mod), typespec_to_ast(name)] }
    { dot, [line: line], args }
  end

  defp typespec_to_ast({ :ann_type, line, [var, type] }) do
    { :::, [line: line], [typespec_to_ast(var), typespec_to_ast(type)] }
  end

  defp typespec_to_ast({ :typed_record_field,
                         { :record_field, line, { :atom, line1, name } },
                         type }) do
    typespec_to_ast({ :ann_type, line, [{ :var, line1, name }, type] })
  end

  defp typespec_to_ast({ :type, _, :any }) do
    quote do: ...
  end

  defp typespec_to_ast({ :paren_type, _, [type] }) do
    typespec_to_ast(type)
  end

  defp typespec_to_ast({ t, _line, atom }) when is_atom(t) do
    atom
  end

  defp typespec_to_ast(other), do: other

  defp erl_to_ex_var(var) do
    case atom_to_binary(var) do
      <<"_", c :: [binary, size(1)], rest :: binary>> ->
        binary_to_atom("_#{String.downcase(c)}#{rest}")
      <<c :: [binary, size(1)], rest :: binary>> ->
        binary_to_atom("#{String.downcase(c)}#{rest}")
    end
  end

  ## From AST conversion

  defp line(meta) do
    case :lists.keyfind(:line, 1, meta) do
      { :line, line } -> line
      false -> 0
    end
  end

  # Handle unions
  defp typespec({ :|, meta, [_, _] } = exprs, vars, caller) do
    exprs = collect_union(exprs)
    union = for e <- exprs, do: typespec(e, vars, caller)
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

  ## Handle maps and structs
  defp typespec({:%{}, meta, fields}, vars, caller) do
    fields = Enum.map(fields, fn { k, v } ->
      {:type, line(meta), :map_field_assoc, typespec(k, vars, caller), typespec(v, vars, caller)}
    end)
    {:type, line(meta), :map, fields}
  end

  defp typespec({:%, _, [name, {:%{}, meta, fields}]}, vars, caller) do
    typespec({:%{}, meta, [{:__struct__, name}|fields]}, vars, caller)
  end

  # Handle ranges
  defp typespec({:.., meta, args}, vars, caller) do
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
  defp typespec([{:->, meta, [arguments, return]}], vars, caller) when is_list(arguments) do
    args = fn_args(meta, arguments, return, vars, caller)
    { :type, line(meta), :fun, args }
  end

  # Handle type operator
  defp typespec({:::, meta, [var, expr] }, vars, caller) do
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
  defp typespec({:tuple, meta, args}, _vars, _caller) when args == [] or is_atom(args) do
    { :type, line(meta), :tuple, :any }
  end

  defp typespec({:{}, meta, t}, vars, caller) when is_list(t) do
    args = for e <- t, do: typespec(e, vars, caller)
    { :type, line(meta), :tuple, args }
  end

  defp typespec({ left, right }, vars, caller) do
    typespec({ :{}, [], [left, right] }, vars, caller)
  end

  # Handle blocks
  defp typespec({:__block__, _meta, [arg]}, vars, caller) do
    typespec(arg, vars, caller)
  end

  # Handle variables or local calls
  defp typespec({name, meta, atom}, vars, caller) when is_atom(atom) do
    if name in vars do
      { :var, line(meta), name }
    else
      typespec({name, meta, []}, vars, caller)
    end
  end

  # Handle local calls
  defp typespec({:string, meta, arguments}, vars, caller) do
    :elixir_errors.warn caller.line, caller.file, "string() type use is discouraged. For character lists, use " <>
      "char_list() type, for strings, String.t()\n#{Exception.format_stacktrace(caller.stacktrace)}"
    arguments = for arg <- arguments, do: typespec(arg, vars, caller)
    { :type, line(meta), :string, arguments }
  end

  defp typespec({:char_list, _meta, arguments}, vars, caller) do
    typespec((quote do: :elixir.char_list(unquote_splicing(arguments))), vars, caller)
  end

  defp typespec({:as_boolean, _meta, arguments}, vars, caller) do
    typespec((quote do: :elixir.as_boolean(unquote_splicing(arguments))), vars, caller)
  end

  defp typespec({name, meta, arguments}, vars, caller) do
    arguments = for arg <- arguments, do: typespec(arg, vars, caller)
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

  defp typespec(list, vars, caller) do
    [h|t] = Enum.reverse(list)
    union = Enum.reduce(t, validate_kw(h, list, caller), fn(x, acc) ->
      { :|, [], [validate_kw(x, list, caller), acc] }
    end)
    typespec({ :list, [], [union] }, vars, caller)
  end

  ## Helpers

  defp compile_error(caller, desc) do
    raise CompileError, file: caller.file, line: caller.line, description: desc
  end

  defp remote_type({remote, meta, name, arguments}, vars, caller) do
    arguments = for arg <- arguments, do: typespec(arg, vars, caller)
    { :remote_type, line(meta), [ remote, name, arguments ] }
  end

  defp collect_union({ :|, _, [a, b] }), do: [a|collect_union(b)]
  defp collect_union(v), do: [v]

  defp validate_kw({ key, _ } = t, _, _caller) when is_atom(key), do: t
  defp validate_kw(_, original, caller) do
    compile_error(caller, "unexpected list in typespec: #{Macro.to_string original}")
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
    args = for arg <- args, do: typespec(arg, vars, caller)
    { :type, line(meta), :product, args }
  end

  defp variable({name, meta, _}) do
    {:var, line(meta), name}
  end

  defp unpack_typespec_kw([{ :type, _, :tuple, [{ :atom, _, atom }, type] }|t], acc) do
    unpack_typespec_kw(t, [{atom, typespec_to_ast(type)}|acc])
  end

  defp unpack_typespec_kw([], acc) do
    { :ok, :lists.reverse(acc) }
  end

  defp unpack_typespec_kw(_, _acc) do
    :error
  end
end
