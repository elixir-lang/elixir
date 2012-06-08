defmodule Typespec do

  defmacro __using__(options) do
    Module.add_compile_callback(__CALLER__.module, __MODULE__, :__aggregate_specs__)
    Module.register_attribute(__CALLER__.module, :__specs__, accumulate: true, persist: false)
    Module.register_attribute(__CALLER__.module, :opaque, accumulate: false, persist: true)
    Module.register_attribute(__CALLER__.module, :type, accumulate: false, persist: true)
    Module.register_attribute(__CALLER__.module, :opaque, accumulate: false, persist: true)
    quote do
      import Typespec
      @__typespec_options__ unquote(options)
    end
  end

  defmacro __aggregate_specs__(module) do
    options = Module.read_attribute(module, :__typespec_options__)
    specs = List.reverse(Module.read_attribute(module, :__specs__))
    specs = :lists.ukeysort(1, lc {k, _} inlist specs, do: {k, :proplists.append_values(k, specs)})
    lc attr inlist specs, do: Module.add_attribute module, :spec, attr
    case options[:keep_data] do
      nil -> :ok
      true ->
        quote do
          def __specs__, do: unquote(Macro.escape(specs))
        end
    end
  end

  defp remote_type({remote, line, name, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    {:remote_type, line, [remote, name, arguments]}
  end

  defp collect_union({:|, _, [a, b]}), do: [b|collect_union(a)]
  defp collect_union(v), do: [v]

  defp typespec({:|, line, [_,_]} = orexpr, vars, caller) do
    union = lc e inlist List.reverse(collect_union(orexpr)), do: typespec(e, vars, caller)
     { :type, line, :union, lc(e inlist union, do: e) }
  end

  defp typespec({:"<<>>", line, []}, _,_) do
     {:type, line, :binary, [{:integer, line, 0}, {:integer, line, 0}]}
  end

  defp typespec({:"<<>>", line, [{:"|", _, [{:_, line1, atom}, {:"*", _, [{:_, line2, atom}, unit]}]}]}, _, _) when atom in [:quoted, nil] do
     {:type, line, :binary, [{:integer, line1, 0}, {:integer, line2, unit}]}
  end

  defp typespec({:"<<>>", line, [{:"|", line1, [{:_, line2, atom}, base]}]}, _, _) when atom in [:quoted, nil] do
     {:type, line, :binary, [{:integer, line1, base}, {:integer, line2, 0}]}
  end
  
  defp typespec({:"..", line, args}, vars, caller) do
    typespec({:range, line, args}, vars, caller)
  end

  defp typespec({:__aliases__, _, _} = alias, vars, caller) do
    atom = Macro.expand alias, caller
    typespec(atom, vars, caller)
  end

  defp typespec(atom, _, _) when is_atom(atom) do
     {:atom, 0, atom}
  end
  
  defp typespec(integer, _, _) when is_integer(integer) do
     {:integer, 0, integer}
  end

  defp typespec({:"::", line,[var,expr]}, vars, caller) do
     {:ann_type, line, [typespec(var, [elem(var,1)|vars], caller), typespec(expr, vars, caller)]}
  end

  defp typespec({:"-", line, [integer]}, _, _) when is_integer(integer) do
     {:op, line, :-, {:integer, line, integer}}
  end

  defp typespec({{:".", line, [{:atom, _, remote}, name]}, _, args}, vars, caller) do
    remote_type({typespec(remote, vars, caller), line, typespec(name, vars, caller), args}, vars, caller)
  end

  defp typespec({{:".", line, [{:__aliases__, line, _} = alias, name]}, line2, args}, vars, caller) do
    remote = Macro.expand alias, caller
    typespec({{:".", line, [{:atom, line, remote}, name]}, line2, args}, vars, caller)
  end

  defp typespec([], vars, caller) do
    typespec({:nil, 0, []}, vars, caller)
  end

  defp typespec([spec], vars, caller) do
    typespec({:"list", 0, [spec]}, vars, caller)
  end

  defp typespec(l, _, _) when is_list(l) do
    raise(ArgumentError, message: "Unexpected list #{inspect l}")
  end

  defp typespec({:tuple, line, atom}, vars, caller) when atom in [:quoted, nil] do
    typespec({:"{}", line, []}, vars, caller)
  end

  defp typespec({:"{}", line, []}, _, _) do
     {:type, line, :tuple, :any}
  end

  defp typespec({:"{}", line, t}, vars, caller) when is_list(t) do
     {:type, line, :tuple, lc(e inlist t, do: typespec(e, vars, caller))}
  end

  defp typespec({:fun, line, arguments}, vars, caller) when is_list(arguments) do
    case List.reverse(arguments) do
      [[{:do, returns}]|t] ->  typespec({{:fun, line, List.reverse(t)}, returns}, vars, caller)
      [] -> typespec({{:fun, line, []}}, vars, caller)
    end
  end

  defp typespec({{:fun, line, atom}, returns}, vars, caller) when atom in [:quoted, nil] do
    typespec({{:fun, line, [{:type, line, :any}]}, returns}, vars, caller)
  end

  defp typespec({{:fun, line, [{:'...', _, _}]}, returns}, vars, caller) do
     {:type, line, :fun, [{:type, line, :any}, typespec(returns, vars, caller)]}

  end

  defp typespec({{:fun, line, arguments}, returns}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
     {:type, line, :fun, [{:type, line, :product, arguments},
                        typespec(returns, vars, caller)]}
  end

  defp typespec({{:fun, line, []}}, _, _) do
     {:type, line, :fun, []}
  end

  defp typespec({name, line, atom}, vars, caller) when atom in [:quoted, nil] do
    if List.member?(vars, name) do
       {:var, line, name}
    else
      typespec({name, line, []}, vars, caller)
    end
  end

  defp typespec({name, line, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
     {:type, line, name, arguments}
  end

  defp typespec(name, vars, caller) when is_atom(name) do
    typespec({name, 0, []}, vars, caller)
  end

  defp typespec(t, vars, caller) when is_tuple(t) do
     {:type, 0, :tuple, lc(e inlist tuple_to_list(t), do: typespec(e, vars, caller))}
  end

  defp variable({name, line, _}) do
    {:var, line, name}
  end

  defp _deftype({name, _, args}, definition, export, caller, options) do
    args = 
    case args do
      :quoted -> []
      nil -> []
      _ -> lc arg inlist args, do: variable(arg)
    end
    spec = typespec(definition, (lc {:var, _, var} inlist args, do: var), caller)
    vars = lc ({:var, _, _} = var) inlist args, do: var
    type      = Macro.escape { name, spec, vars }
    type_attr = if options[:opaque], do: :opaque, else: :type
    quote do
      Module.add_attribute __MODULE__, unquote(type_attr), unquote(type)
      if unquote(export) do
        Module.add_attribute __MODULE__, :export_type, [{unquote(name), unquote(length(vars))}]
      end
      {unquote(type_attr), unquote(type)}
    end
  end

  defmacro deftype({:"::", _, [name, definition]}, options), do: _deftype(name, definition, true, __CALLER__, options)
  defmacro deftype(name, options), do: _deftype(name, (quote do: term), true, __CALLER__, options)
  defmacro deftype({:"::", _, [name, definition]}), do: _deftype(name, definition, true, __CALLER__, [])
  defmacro deftype(name), do: _deftype(name, (quote do: term), true, __CALLER__, [])

  defmacro deftypep({:"::", _, [name, definition]}), do: _deftype(name, definition, false, __CALLER__, [])
  defmacro deftypep(name), do: _deftype(name, (quote do: term), false, __CALLER__, [])

  defmacro defspec({name, line, args},[{:do, returns}]) do
    spec = typespec({{:fun, line, args}, returns}, [], __CALLER__)
    code = Macro.escape { { name, length(args) }, [spec] }
    quote do
      @__specs__ unquote(code)
      {:spec, unquote(code)}
    end
  end
end