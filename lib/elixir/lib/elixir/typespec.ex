defmodule Elixir.Typespec do
  @moduledoc """
  This is the module that converts Elixir typespecs
  to Erlang typespecs syntax. Everytime @spec, @type
  and @typep are used they proxy to the functions
  in this module.
  """

  defmacro deftype(name, options // []) do
    _deftype(name, true, __CALLER__, options)
  end

  defmacro deftypep(name) do
    _deftype(name, false, __CALLER__, [])
  end

  defmacro defspec(spec, block) do
    _defspec(:spec, __CALLER__, spec, block)
  end

  defmacro defcallback(spec, block) do
    _defspec(:callback, __CALLER__, spec, block)
  end

  @doc """
  Get the types defined for the given module. This function
  is only available for modules being compiled. If the module
  was already compiled, you need to loop its attributes
  to get such information.
  """
  def get_types(module) do
    Module.read_attribute(module, :type) ++ Module.read_attribute(module, :opaque)
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

  ## Typespec conversion

  # Handle unions
  defp typespec({ :|, line, [_,_] } = exprs, vars, caller) do
    exprs = List.reverse(collect_union(exprs))
    union = lc e inlist exprs, do: typespec(e, vars, caller)
    { :type, line, :union, union }
  end

  # Handle binaries
  defp typespec({:<<>>, line, []}, _,_) do
     {:type, line, :binary, [{:integer, line, 0}, {:integer, line, 0}]}
  end

  defp typespec({:<<>>, line, [{:|, _, [{:_, line1, atom}, {:*, _, [{:_, line2, atom}, unit]}]}]}, _, _) when is_atom(atom) do
     {:type, line, :binary, [{:integer, line1, 0}, {:integer, line2, unit}]}
  end

  defp typespec({:<<>>, line, [{:|, line1, [{:_, line2, atom}, base]}]}, _, _) when is_atom(atom) do
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
    left  = typespec(var, [elem(var,1)|vars], caller)
    right = typespec(expr, vars, caller)
    { :ann_type, line, [left, right] }
  end

  # Handle unary ops
  defp typespec({op, line, [integer]}, _, _) when op in [:+, :-] and is_integer(integer) do
    { :op, line, op, {:integer, line, integer} }
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
      case List.reverse(arguments) do
        [[{:do,h}]|t] -> fn_args(line, List.reverse(t), h, vars, caller)
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

  def _deftype({:"::", _, [name, definition]}, export, caller, opts) do
    _deftype(name, definition, export, caller, opts)
  end

  def _deftype(name, export, caller, opts) do
    _deftype(name, { :term, caller.line, nil }, export, caller, opts)
  end

  defp _deftype({name, _, args}, definition, export, caller, options) do
    args = if is_atom(args), do: [], else: lc(arg inlist args, do: variable(arg))
    vars = lc {:var, _, var} inlist args, do: var
    spec = typespec(definition, vars, caller)

    vars = lc ({:var, _, _} = var) inlist args, do: var
    attr = if options[:opaque], do: :opaque, else: :type

    export = if export do
      quote do: Module.compile_type(__MODULE__, :export_type, [{name, length(vars)}])
    else
      nil
    end

    quote do
      name = unquote(name)
      spec = unquote(Macro.escape(spec))
      vars = unquote(Macro.escape(vars))
      type = { name, spec, vars }
      Module.compile_type __MODULE__, unquote(attr), type
      unquote(export)
      { unquote(attr), type }
    end
  end

  defp _defspec(type, caller, {name, line, args},[{:do,return}]) do
    spec  = { :type, line, :fun, fn_args(line, args, return, [], caller) }
    code  = Macro.escape { {type, { name, length(args) }}, [spec] }
    table = spec_table_for(caller.module)

    quote do
      code = unquote(code)
      :ets.insert(unquote(table), code)
      code
    end
  end


  defp spec_table_for(module) do
    table = list_to_atom Erlang.lists.concat([:s, module])
    unless table == :ets.info(table, :name), do:
      raise(ArgumentError, message: "cannot manage specs for #{inspect module} because it was already compiled")
    table
  end

  defp variable({name, line, _}) do
    {:var, line, name}
  end
end