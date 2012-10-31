defmodule Kernel.Typespec do
  @moduledoc """
  This is the module that converts Elixir typespecs
  to Erlang typespecs syntax. Everytime `@spec`,
  `@type`, `@typep`, `@callback` and `@opaque` are used
  they proxy to the functions in this module.
  """

  defmacro deftype(type) do
    quote do
      Kernel.Typespec.deftype(:type, (quote line: :keep, do: unquote(type)), __ENV__)
    end
  end

  defmacro defopaque(type) do
    quote do
      Kernel.Typespec.deftype(:opaque, (quote line: :keep, do: unquote(type)), __ENV__)
    end
  end

  defmacro deftypep(type) do
    quote do
      Kernel.Typespec.deftype(:typep, (quote line: :keep, do: unquote(type)), __ENV__)
    end
  end

  def deftype(kind, { :::, _, [type, definition] }, caller) do
    do_deftype(kind, type, definition, caller)
  end

  def deftype(kind, type, caller) do
    do_deftype(kind, type, { :term, caller.line, nil }, caller)
  end

  defp do_deftype(raw_kind, { name, _, args }, definition, caller) do
    args =
      if is_atom(args) do
        []
      else
        lc(arg inlist args, do: variable(arg))
      end

    { kind, export } =
      case raw_kind do
        :type   -> { :type, true }
        :typep  -> { :type, false }
        :opaque -> { :opaque, true }
      end

    vars = lc { :var, _, var } inlist args, do: var
    spec = typespec(definition, vars, caller)

    vars = lc { :var, _, _ } = var inlist args, do: var
    type = { name, spec, vars }

    Module.compile_type caller.module, kind, type

    if export do
      Module.compile_type(caller.module, :export_type, [{ name, length(vars) }])
    end

    { raw_kind, type }
  end

  defmacro defspec(spec, block) do
    quote do
      Kernel.Typespec.defspec(:spec, (quote line: :keep, do: unquote spec),
        (quote line: :keep, do: unquote block), __ENV__)
    end
  end

  defmacro defcallback(spec, block) do
    quote do
      Kernel.Typespec.defspec(:callback, (quote line: :keep, do: unquote spec),
        (quote line: :keep, do: unquote block), __ENV__)
    end
  end

  def defspec(type, { name, line, args }, [do: return], caller) do
    if is_atom(args), do: args = []
    spec  = { :type, line, :fun, fn_args(line, args, return, [], caller) }
    code  = { { type, { name, Kernel.length(args) } }, [spec] }
    :ets.insert(spec_table_for(caller.module), code)
    code
  end

  @doc """
  Get the types defined for the given module. This function
  is only available for modules being compiled. If the module
  was already compiled, you need to loop its attributes
  to get such information.
  """
  def get_types(module) do
    Module.get_attribute(module, :type) ++ Module.get_attribute(module, :opaque)
  end

  @doc """
  Returns true if the current module defines a given type
  (opaque or not). This function is only available for modules
  being compiled.
  """
  def defines_type?(module, name, arity) do
    finder = match?({ ^name, _, vars } when length(vars) == arity, &1)
    :lists.any(finder, Module.get_attribute(module, :type)) or
      :lists.any(finder, Module.get_attribute(module, :opaque))
  end

  @doc """
  Get the specs defined for the given module. This function
  is only available for modules being compiled. If the module
  was already compiled, you need to loop its attributes
  to get such information.
  """
  def get_specs(module) do
    specs = :ets.tab2list(spec_table_for(module))
    keys  = :lists.ukeysort(1, specs)
    lc { k, _ } inlist keys, do: { k, :proplists.append_values(k, specs) }
  end

  @doc """
  Defines a callback from a spec if one is available.
  Returns true if successful, false otherwise.
  """
  def callback_from_spec(module, name, arity) do
    table = spec_table_for(module)
    pairs = :ets.lookup(table, { :spec, { name, arity } })
    specs = :lists.foldl(fn { _key, specs }, acc -> acc ++ specs end, [], pairs)

    if specs != [] do
      :ets.insert(table, { { :callback, { name, arity } }, specs })
      true
    else
      false
    end
  end

  @doc """
  Returns type spec retrieved from a BEAM file in a readable form
  """
  def to_binary(typespec) do
    Macro.to_binary(typespec_to_ast(typespec))
  end

  ## to_ast

  defp typespec_to_ast({{:spec, {name, _arity}}, args}) do
    contents = lc arg inlist args do
      {:__block__, _, [v]} = spec_typespec_to_ast(arg)
      [product|args] = Enum.reverse(v)
      args = Enum.reverse(args)
      quote do
        @spec unquote(name)(unquote_splicing(args)), unquote(product)
      end
    end
    quote do
      unquote_splicing(contents)
    end
  end

  defp typespec_to_ast({:type, {name, type, args}}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: @type unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  defp typespec_to_ast({:typep, {name, type, args}}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: @typep unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  defp typespec_to_ast({:opaque, {name, type, args}}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: @opaque unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_ast(type))
  end

  defp typespec_to_ast({:type, line, :tuple, args}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    { :{}, line, args }
  end

  defp typespec_to_ast({:type, _line, :binary, [arg1, arg2]}) do
    [arg1, arg2] = lc arg inlist [arg1, arg2], do: typespec_to_ast(arg)
    cond do
      arg2 == 0 ->
        quote do: <<_ :: unquote(arg1)>>
      arg1 == 0 ->
        quote do: <<_ :: _ * unquote(arg2)>>
      true ->
        quote do: <<_ :: unquote(arg1) * unquote(arg2)>>
    end
  end

  defp typespec_to_ast({:type, _line, :union, args}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    Enum.reduce tl(args), hd(args),
      fn(arg, expr) ->
        quote do: unquote(expr) | unquote(arg)
      end
  end

  defp typespec_to_ast({:type, _line, :fun, [{:type, _, :product, args},result]}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do
      fun(unquote_splicing(args ++ [[do: typespec_to_ast(result)]]))
    end
  end

  defp typespec_to_ast({:type, _, :fun, []}) do
    quote do: fun()
  end

  defp typespec_to_ast({:type, _, name, args}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: unquote(name)(unquote_splicing(args))
  end

  defp typespec_to_ast({:var, line, var}) do
    quote hygiene: false, do: unquote({var, line, nil})
  end

  defp typespec_to_ast({:remote_type, _line, [mod, name, args]}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do: unquote(typespec_to_ast(mod)).unquote(typespec_to_ast(name))(unquote_splicing(args))
  end

  defp typespec_to_ast({t, _line, imm}) when is_atom(t) do
    imm
  end

  defp spec_typespec_to_ast({:type, _line, :fun, [{:type, _, :product, args},result]}) do
    args = lc arg inlist args, do: typespec_to_ast(arg)
    quote do
      [unquote_splicing(args), [do: unquote(typespec_to_ast(result))]]
    end
  end

  defp spec_typespec_to_ast({:type, _, :fun, []}) do
    quote do: []
  end

  ## Typespec conversion

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

  # Handle aliases
  defp typespec({:__aliases__, _, _} = alias, vars, caller) do
    atom = Macro.expand alias, caller
    typespec(atom, vars, caller)
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
    unless is_atom(remote), do: raise(ArgumentError, message: "Invalid remote in typespec")
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

  # Handle funs
  defp typespec({:fun, line, arguments}, vars, caller) when is_list(arguments) do
    args =
      case :lists.reverse(arguments) do
        [[{:do,h}]|t] -> fn_args(line, :lists.reverse(t), h, vars, caller)
        [] -> []
        _  -> [fn_args(line, arguments, vars, caller)]
      end

    { :type, line, :fun, args }
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
    IO.write "#{caller.file}.#{caller.line}: warning: string() type use is discouraged. For character lists,  use char_list() type, for strings, String.t()\n"
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

  defp typespec(l, _, _) when is_list(l) do
    raise(ArgumentError, message: "Unexpected list #{inspect l}")
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
    [fn_args(line, args, vars, caller), typespec(return, vars, caller)]
  end

  defp fn_args(line, [{:"...", _, _}], _vars, _caller) do
    { :type, line, :any }
  end

  defp fn_args(line, args, vars, caller) do
    args = lc arg inlist args, do: typespec(arg, vars, caller)
    { :type, line, :product, args }
  end

  defp spec_table_for(module) do
    table = list_to_atom :lists.concat([:s, module])
    unless table == :ets.info(table, :name), do:
      raise(ArgumentError, message: "cannot manage specs for #{inspect module} because it was already compiled")
    table
  end

  defp variable({name, line, _}) do
    {:var, line, name}
  end
end