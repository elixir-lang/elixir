defmodule Kernel.Typespec do
  @moduledoc """
  Holds macros and functions for working with typespecs.

  The attributes `@type`, `@opaque`, `@typep`, `@spec` and
  `@callback` available in modules are handled by the equivalent
  macros defined by this module.

  ## Defining a type

  @type type_name :: type
  @typep type_name :: type
  @opaque type_name :: type

  For more details, see documentation for deftype, deftypep and defopaque in
  Kernel.Typespec

  ## Defining a specification

  @spec function_name(type, type) :: type
  @callback function_name(type, type) :: type

  For more details, see documentation for defspec and defcallback in
  Kernel.Typespec

  ## Types

  The type syntax provided by Elixir is fairly similar to the one
  in Erlang.

  Most of the built-in types provided in Erlang (for example, `pid()`)
  are expressed the same way: `pid()` or simply `pid`. Parametrized types
  are also supported: `list(integer())` and so are remote types: `Enum.t`.

  Certain data type shortcuts ([...], <<>> and {...}) are supported as well.

  Main differences lie in how bit strings and functions are defined:

  ### Bit Strings

  Bit string with a base size of 3:

      <<_ :: 3>>

  Bit string with a unit size of 8:

      <<_ :: _ * 8>>

  ### Functions

  Any function:

      (fun(...) -> any)
      or
      ((...) -> any)
      or
      (... -> any)

  Function with arity of zero:
  
      (fun() -> type)
      or
      (() -> type)

  Function with some arity:
  
      (fun(type, type) -> type)
      or
      ((type, type) -> type)
      or
      (type, type -> type)

  ## Notes

  Elixir discourages the use of type `string()` as it might be confused
  with binaries which are referred to as "strings" in Elixir (as opposed to 
  character lists). In order to use the type that is called `string()` in Erlang,
  one has to use the `char_list()` type which is a synonym to `string()`. If yu
  use `string()`, you'll get a warning from the compiler.

  If you want to refer to the "string" type (the one operated by functions in the
  String module), use `String.t()` type instead.

  See http://www.erlang.org/doc/reference_manual/typespec.html
  for more information.
  """

  @doc """
  Defines a type.
  This macro is the one responsible to handle the attribute @type.

  ## Examples

      @type my_type :: atom

  """
  defmacro deftype(type) do
    quote do
      Kernel.Typespec.deftype(:type, (quote line: :keep, do: unquote(type)), __ENV__)
    end
  end

  @doc """
  Defines an opaque type.
  This macro is the one responsible to handle the attribute @opaque.

  ## Examples

      @opaque my_type :: atom

  """
  defmacro defopaque(type) do
    quote do
      Kernel.Typespec.deftype(:opaque, (quote line: :keep, do: unquote(type)), __ENV__)
    end
  end

  @doc """
  Defines a private type.
  This macro is the one responsible to handle the attribute @typep.

  ## Examples

      @typep my_type :: atom

  """
  defmacro deftypep(type) do
    quote do
      Kernel.Typespec.deftype(:typep, (quote line: :keep, do: unquote(type)), __ENV__)
    end
  end

  @doc """
  Defines a spec.
  This macro is the one responsible to handle the attribute @spec.

  ## Examples

      @spec add(number, number) :: number

  """
  defmacro defspec(spec) do
    quote do
      Kernel.Typespec.defspec(:spec, (quote line: :keep, do: unquote spec), __ENV__)
    end
  end

  @doc """
  Defines a callback.
  This macro is the one responsible to handle the attribute @callback.

  ## Examples

      @callback add(number, number) :: number

  """
  defmacro defcallback(spec) do
    quote do
      Kernel.Typespec.defspec(:callback, (quote line: :keep, do: unquote(spec)), __ENV__)
    end
  end

  ## Helpers

  @doc """
  Defines a `type`, `typep` or `opaque` by receiving Erlang's typespec.
  """
  def define_type(module, kind, { name, _, vars } = type) when kind in [:type, :typep, :opaque] do
    { kind, export } =
      case kind do
        :type   -> { :type, true }
        :typep  -> { :type, false }
        :opaque -> { :opaque, true }
      end

    Module.compile_typespec module, kind, type
    if export, do:
      Module.compile_typespec module, :export_type, [{ name, length(vars) }]
    type
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
  Returns true if the current module defines a given type
  (private, opaque or not). This function is only available
  for modules being compiled.
  """
  def defines_type?(module, name, arity) do
    finder = match?({ ^name, _, vars } when length(vars) == arity, &1)
    :lists.any(finder, Module.get_attribute(module, :type)) or
      :lists.any(finder, Module.get_attribute(module, :opaque))
  end

  @doc """
  Returns true if the current module defines a given spec.
  This function is only available for modules being compiled.
  """
  def defines_spec?(module, name, arity) do
    tuple = { name, arity }
    :lists.any(match?(^tuple, &1), Module.get_attribute(module, :spec))
  end

  @doc """
  Returns true if the current module defines a callback.
  This function is only available for modules being compiled.
  """
  def defines_callback?(module, name, arity) do
    tuple = { name, arity }
    :lists.any(match?(^tuple, &1), Module.get_attribute(module, :callback))
  end

  @doc """
  Converts a spec clause back to Elixir AST.
  """
  def spec_to_ast(name, { :type, line, :fun, [{:type, _, :product, args},result] }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { :::, line, [{ name, line, args }, typespec_to_ast(result)] }
  end

  def spec_to_ast(name, { :type, line, :fun, [] }) do
    { :::, line, [{ name, line, [] }, quote(do: term)] }
  end

  def spec_to_ast(name, { :type, line, :bounded_fun, [{ :type, _, :fun, [{ :type, _, :product, args }, result] }, constraints] }) do
    [h|t] =
      lc {:type, line, :constraint, [{:atom, _, :is_subtype}, [var, type]]} inlist constraints do
        { :is_subtype, line, [typespec_to_ast(var), typespec_to_ast(type)] }
      end

    args = lc arg inlist args, do: typespec_to_ast(arg)
    guards = Enum.reduce t, h, fn(x, acc) -> { :and, line, [acc, x] } end

    { :::, line, [{ :when, line, [{ name, line, args }, guards] }, typespec_to_ast(result)] }
  end

  @doc """
  Converts a type clause back to Elixir AST.
  """
  def type_to_ast({ { :record, record }, fields, args }) when is_atom(record) do
    fields = lc field inlist fields, do: typespec_to_ast(field)
    args = lc arg inlist args, do: typespec_to_ast(arg)
    type = { :{}, 0, [record|fields] }
    quote do: unquote(record)(unquote_splicing(args)) :: unquote(type)
  end

  def type_to_ast({ name, type, args }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  @doc """
  Returns all types available from the beam.

  It is returned as a list of tuples where the first
  element is the type (`:typep`, `:type` and `:opaque`).

  The module has to have a corresponding beam file
  on the file system.
  """
  def beam_types(module) do
    case abstract_code(module) do
      { :ok, abstract_code } ->
        exported_types = lc { :attribute, _, :export_type, types } inlist abstract_code, do: types
        exported_types = List.flatten(exported_types)

        lc { :attribute, _, kind, { name, _, args } = type } inlist abstract_code, kind in [:opaque, :type] do
          cond do
            kind == :opaque -> { :opaque, type }
            List.member?(exported_types, { name, length(args) }) -> { :type, type }
            true -> { :typep, type }
          end
        end
      _ ->
        []
    end
  end

  @doc """
  Returns all specs available from the beam.

  It is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module has to have a corresponding beam file
  on the file system.
  """
  def beam_specs(module) do
    from_abstract_code(module, :spec)
  end

  @doc """
  Returns all callbacks available from the beam.

  It is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module has to have a corresponding beam file
  on the file system.
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
      {:ok, { _, [{ :abstract_code, { raw_abstract_v1, abstract_code } }] } } ->
        { :ok, abstract_code }
      _ ->
        []
    end
  end

  defp abstract_code_beam(module) when is_atom(module) do
    case :code.which(module) do
      :non_existing -> module
      file -> file
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

  def deftype(kind, type, caller) do
    do_deftype(kind, type, { :term, caller.line, nil }, caller)
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

    define_type(caller.module, kind, type)
  end

  @doc false
  def defspec(type, {:::, _, [{ :when, _, [{ name, line, args }, constraints_guard] }, return] }, caller) do
    if is_atom(args), do: args = []
    constraints = guard_to_constraints(constraints_guard, caller)
    spec = { :type, line, :fun, fn_args(line, args, return, Keyword.keys(constraints), caller) }
    spec = { :type, line, :bounded_fun, [spec, Keyword.values(constraints)] }
    code = { { name, Kernel.length(args) }, spec }
    Module.compile_typespec(caller.module, type, code)
    code
  end

  def defspec(type, {:::, _, [{ name, line, args }, return]}, caller) do
    if is_atom(args), do: args = []
    spec = { :type, line, :fun, fn_args(line, args, return, [], caller) }
    code = { { name, Kernel.length(args) }, spec }
    Module.compile_typespec(caller.module, type, code)
    code
  end

  defp guard_to_constraints({ :is_subtype, line, [{ name, _, _ }, type] }, caller) do
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
    { :{}, line, args }
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
      fn(arg, expr) -> { :|, line, [expr, arg] } end
  end

  defp typespec_to_ast({ :type, line, :fun, [{:type, _, :product, args},result] }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { :"->", line, [{args, typespec_to_ast(result)}] }
  end

  defp typespec_to_ast({ :type, line, :fun, [args, result] }) do
    { :"->", line, [{[typespec_to_ast(args)], typespec_to_ast(result)}] }
  end

  defp typespec_to_ast({ :type, line, :fun, [] }) do
    typespec_to_ast({ :type, line, :fun, [{:type, line, :any}, {:type,line,:any, []} ] })
  end

  defp typespec_to_ast({ :type, line, :range, [left, right] }) do
    { :"..", line, [typespec_to_ast(left), typespec_to_ast(right)] }
  end

  defp typespec_to_ast({ :type, line, name, args }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { name, line, args }
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

  # special shortcut(s)
  defp typespec_to_ast({ :remote_type, line, [{:atom, _, :elixir}, {:atom, _, :char_list}, []] }) do
    typespec_to_ast({:type, line, :char_list, []})
  end

  defp typespec_to_ast({ :remote_type, line, [mod, name, args] }) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    dot  = { :., line, [typespec_to_ast(mod), typespec_to_ast(name)] }
    { dot, line, args }
  end

  defp typespec_to_ast({ :ann_type, line, [var, type] }) do
    { :::, line, [typespec_to_ast(var), typespec_to_ast(type)] }
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

  # Handle unions
  defp typespec({ :|, line, [_,_] } = exprs, vars, caller) do
    exprs = :lists.reverse(collect_union(exprs))
    union = lc e inlist exprs, do: typespec(e, vars, caller)
    { :type, line, :union, union }
  end

  # Handle binaries
  defp typespec({:<<>>, line, []}, _,_) do
     {:type, line, :binary, [{:integer, line, 0}, {:integer, line, 0}]}
  end

  defp typespec({:<<>>, line, [{:::, _, [{:_, line1, atom}, {:*, _, [{:_, line2, atom}, unit]}]}]}, _, _) when is_atom(atom) do
     {:type, line, :binary, [{:integer, line1, 0}, {:integer, line2, unit}]}
  end

  defp typespec({:<<>>, line, [{:::, line1, [{:_, line2, atom}, base]}]}, _, _) when is_atom(atom) do
     {:type, line, :binary, [{:integer, line1, base}, {:integer, line2, 0}]}
  end

  # Handle ranges
  defp typespec({:"..", line, args}, vars, caller) do
    typespec({:range, line, args}, vars, caller)
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
  defp typespec({:->, line, [{[{:fun, _, arguments}], return}]}, vars, caller) when is_list(arguments) do
    typespec({:->, line, [{arguments, return}]}, vars, caller)
  end

  defp typespec({:->, line, [{arguments, return}]}, vars, caller) when is_list(arguments) do
    args = fn_args(line, arguments, return, vars, caller)
    { :type, line, :fun, args }
  end

  # Handle type operator
  defp typespec({:"::", line, [var, expr] }, vars, caller) do
    left  = typespec(var, [elem(var, 0)|vars], caller)
    right = typespec(expr, vars, caller)
    { :ann_type, line, [left, right] }
  end

  # Handle unary ops
  defp typespec({op, line, [integer]}, _, _) when op in [:+, :-] and is_integer(integer) do
    { :op, line, op, {:integer, line, integer} }
  end

  # Handle access macro
  defp typespec({{:., line, [Kernel, :access]}, line1, [target, args]}, vars, caller) do
    access = {{:., line, [Kernel, :access]}, line1,
              [target, args ++ [_: (quote hygiene: false, do: any)]]}
    typespec(Macro.expand(access, caller), vars, caller)
  end

  # Handle remote calls
  defp typespec({{:., line, [remote, name]}, _, args}, vars, caller) do
    remote = Macro.expand remote, caller
    unless is_atom(remote), do: raise ArgumentError, message: "invalid remote in typespec"
    remote_type({typespec(remote, vars, caller), line, typespec(name, vars, caller), args}, vars, caller)
  end

  # Handle tuples
  defp typespec({:tuple, line, atom}, vars, caller) when is_atom(atom) do
    typespec({:{}, line, []}, vars, caller)
  end

  defp typespec({:{}, line, []}, _, _) do
    { :type, line, :tuple, :any }
  end

  defp typespec({:{}, line, t}, vars, caller) when is_list(t) do
    args = lc e inlist t, do: typespec(e, vars, caller)
    { :type, line, :tuple, args }
  end

  # Handle variables or local calls
  defp typespec({name, line, atom}, vars, caller) when is_atom(atom) do
    if List.member?(vars, name) do
      { :var, line, name }
    else
      typespec({name, line, []}, vars, caller)
    end
  end

  # Handle local calls
  defp typespec({:string, line, arguments}, vars, caller) do
    IO.write "warning: string() type use is discouraged. For character lists, use " <>
      "char_list() type, for strings, String.t()\n#{Exception.env_stacktrace(caller)}"
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    { :type, line, :string, arguments }
  end

  defp typespec({:char_list, _line, arguments}, vars, caller) do
    typespec((quote do: :elixir.char_list(unquote_splicing(arguments))), vars, caller)
  end

  defp typespec({name, line, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    { :type, line, name, arguments }
  end

  # Handle literals
  defp typespec(atom, _, _) when is_atom(atom) do
    { :atom, 0, atom }
  end

  defp typespec(integer, _, _) when is_integer(integer) do
    { :integer, 0, integer }
  end

  defp typespec([], vars, caller) do
    typespec({ nil, 0, [] }, vars, caller)
  end

  defp typespec([spec], vars, caller) do
    typespec({ :list, 0, [spec] }, vars, caller)
  end

  defp typespec([spec, {:"...", _, quoted}], vars, caller) when is_atom(quoted) do
    typespec({ :nonempty_list, 0, [spec] }, vars, caller)
  end

  defp typespec(l, _, _) when is_list(l) do
    raise ArgumentError, message: "Unexpected list #{inspect l}"
  end

  defp typespec(t, vars, caller) when is_tuple(t) do
    args = lc e inlist tuple_to_list(t), do: typespec(e, vars, caller)
    { :type, 0, :tuple, args }
  end

  ## Helpers

  defp remote_type({remote, line, name, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    { :remote_type, line, [ remote, name, arguments ] }
  end

  defp collect_union({ :|, _, [a, b] }), do: [b|collect_union(a)]
  defp collect_union(v), do: [v]

  defp fn_args(line, args, return, vars, caller) do
    case [fn_args(line, args, vars, caller), typespec(return, vars, caller)] do
      [{:type,_,:any},{:type,_,:any,[]}] -> []
      x -> x
    end
  end

  defp fn_args(line, [{:"...", _, _}], _vars, _caller) do
    { :type, line, :any }
  end

  defp fn_args(line, args, vars, caller) do
    args = lc arg inlist args, do: typespec(arg, vars, caller)
    { :type, line, :product, args }
  end

  defp variable({name, line, _}) do
    {:var, line, name}
  end
end