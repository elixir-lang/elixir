defmodule Typespec do

  defmacro __using__(options) do
    Module.add_compile_callback(__CALLER__.module, __MODULE__, :__aggregate_specs__)
    Module.register_attribute(__CALLER__.module, :__specs__, accumulate: true, persist: false)
    quote do
      import Typespec
      @__typespec_options__ unquote(options)
    end
  end

  defmacro __aggregate_specs__(module) do
    options = Module.read_attribute(module, :__typespec_options__)
    specs = :lists.reverse(Module.read_attribute(module, :__specs__))
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
    quote do: {:remote_type, unquote(line), [unquote(remote), unquote(name), unquote(arguments)]}
  end

  defp collect_union({:"|", _, [a, b]}), do: [b|collect_union(a)]
  defp collect_union(v), do: [v]

  defp typespec({:"|", line, [_,_]} = orexpr, vars, caller) do
    union = lc e inlist :lists.reverse(collect_union(orexpr)), do: typespec(e, vars, caller)
    quote do: {:type, unquote(line), :union, lc e inlist unquote(union), do: e}
  end

  defp typespec({:"<<>>", line, []}, _,_) do
    quote do: {:type, unquote(line), :binary, [{:integer, unquote(line), 0}, {:integer, unquote(line), 0}]}
  end

  defp typespec({:"<<>>", line, [{:"|", _, [{:_, line1, atom}, {:"*", _, [{:_, line2, atom}, unit]}]}]}, _, _) when atom in [:quoted, nil] do
    quote do: {:type, unquote(line), :binary, [{:integer, unquote(line1), 0}, {:integer, unquote(line2), unquote(unit)}]}
  end

  defp typespec({:"<<>>", line, [{:"|", line1, [{:_, line2, atom}, base]}]}, _, _) when atom in [:quoted, nil] do
    quote do: {:type, unquote(line), :binary, [{:integer, unquote(line1), unquote(base)}, {:integer, unquote(line2), 0}]}
  end

  defp typespec({:__aliases__, _, _} = alias, vars, caller) do
    atom = Macro.expand alias, caller
    typespec(atom, vars, caller)
  end

  defp typespec(atom, _, _) when is_atom(atom) do
    quote do: {:atom, 0, unquote(atom)}
  end
  
  defp typespec(integer, _, _) when is_integer(integer) do
    quote do: {:integer, 0, unquote(integer)}
  end

  defp typespec({:"::", line,[var,expr]}, vars, caller) do
    quote do: {:ann_type, unquote(line), [unquote(typespec(var, [elem(var,1)|vars], caller)), unquote(typespec(expr, vars, caller))]}
  end

  defp typespec({:"-", line, [integer]}, _, _) when is_integer(integer) do
    quote do: {:op, unquote(line), :-, {:integer, unquote(line), unquote(integer)}}
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
    quote do: {:type, unquote(line), :tuple, :any}
  end

  defp typespec({:"{}", line, t}, vars, caller) when is_list(t) do
    quote do: {:type, unquote(line), :tuple, unquote((lc e inlist t, do: typespec(e, vars, caller)))}
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

  defp typespec({{:fun, line, [:'...']}, returns}, vars, caller) do
    quote do: {:type, unquote(line), :fun, [{:type, unquote(line), :any}, unquote(typespec(returns, vars, caller))]}

  end

  defp typespec({{:fun, line, arguments}, returns}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    quote do: {:type, unquote(line), :fun, [{:type, unquote(line), :product, unquote(arguments)},
                        unquote(typespec(returns, vars, caller))]}
  end

  defp typespec({{:fun, line, []}}, _, _) do
    quote do: {:type, unquote(line), :fun, []}
  end

  defp typespec({name, line, atom}, vars, caller) when atom in [:quoted, nil] do
    if List.member?(vars, name) do
      quote do: {:var, unquote(line), unquote(name)}
    else
      typespec({name, line, []}, vars, caller)
    end
  end

  defp typespec({name, line, arguments}, vars, caller) do
    arguments = lc arg inlist arguments, do: typespec(arg, vars, caller)
    quote do: {:type, unquote(line), unquote(name), unquote(arguments)}
  end

  defp typespec(name, vars, caller) when is_atom(name) do
    typespec({name, 0, []}, vars, caller)
  end

  defp typespec(t, vars, caller) when is_tuple(t) do
    quote do: {:type, 0, :tuple, unquote((lc e inlist tuple_to_list(t), do: typespec(e, vars, caller)))}
  end

  defp variable({name, line, _}) do
    {:var, line, name}
  end

  defp _deftype({name, _, args}, definition, export, caller) do
    args = 
    case args do
      :quoted -> []
      nil -> []
      _ -> lc arg inlist args, do: variable(arg)
    end
    spec = typespec(definition, (lc {:var, _, var} inlist args, do: var), caller)
    vars = lc {:var, line, name} inlist args, do: (quote do: {:var, unquote(line), unquote(name)})
    type = quote do: {unquote(name), unquote(spec), unquote(vars)}
    quote do
      Module.add_attribute __MODULE__, :type, unquote(type)
      if unquote(export) do
        Module.add_attribute __MODULE__, :export_type, [{unquote(name), unquote(length(vars))}]
      end
      {:type, unquote(type)}
    end
  end

  defmacro deftype(name), do: _deftype(name, (quote do: term), true, __CALLER__)
  defmacro deftype(name, [{:as, definition}]), do: _deftype(name, definition, true, __CALLER__)
  defmacro deftypep(name, [{:as, definition}]), do: _deftype(name, definition, false, __CALLER__)

  defmacro defspec({name, line, args},[{:returns, returns}]) do
    spec = typespec({{:fun, line, args}, returns}, [], __CALLER__)
    code = quote do: {{unquote(name), unquote(length(args))}, [unquote(spec)]}
    quote do
      @__specs__ unquote(code)
      {:spec, unquote(code)}
    end
  end
end