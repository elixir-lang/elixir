defmodule Typespec do

    defp remote_type({remote, line, name, arguments}, vars) do
        arguments =  lc arg in arguments, do: typespec(arg, vars)
        quote do: {:remote_type, unquote(line), [unquote(remote), unquote(name), unquote(arguments)]}
    end

    def pre_process({:fun, line, args}, [{:returns, v}]) do
        {{:fun, line, args}, v}
    end
    def pre_process(other, _) do
        other
    end

    defp collect_union({:or, _, [a, b]}), do: [b|collect_union(a)]
    defp collect_union(v), do: [v]

    defp typespec({:or, line, [_,_]} = orexpr, vars) do
        union = lc e in :lists.reverse(collect_union(orexpr)), do: typespec(e, vars)
        quote do: {:type, unquote(line), :union, lc e in unquote(union), do: e}
    end

    defp typespec({:"<<>>", line, []}, _) do
        quote do: {:type, unquote(line), :binary, [{:integer, unquote(line), 0}, {:integer, unquote(line), 0}]}
    end

    defp typespec({:"<<>>", line, [{:"|", _, [{:_, line1, atom}, {:"*", _, [{:_, line2, atom}, unit]}]}]}, _) when atom in [:quoted, nil] do
        quote do: {:type, unquote(line), :binary, [{:integer, unquote(line1), 0}, {:integer, unquote(line2), unquote(unit)}]}
    end

    defp typespec({:"<<>>", line, [{:"|", line1, [{:_, line2, atom}, base]}]}, _) when atom in [:quoted, nil] do
        quote do: {:type, unquote(line), :binary, [{:integer, unquote(line1), unquote(base)}, {:integer, unquote(line2), 0}]}
    end

    defp typespec({:__aliases__, _, _} = alias, vars) do
        {atom, _} = Code.eval_quoted alias
        typespec(atom, vars)
    end

    defp typespec(atom, _) when is_atom(atom) do
        quote do: {:atom, 0, unquote(atom)}
    end
    
    defp typespec(integer, _) when is_integer(integer) do
        quote do: {:integer, 0, unquote(integer)}
    end

    defp typespec({:"=", line,[var,expr]}, vars) do
        quote do: {:ann_type, unquote(line), [unquote(typespec(var, [elem(var,1)|vars])), unquote(typespec(expr, vars))]}
    end

    defp typespec({:"-", line, [integer]}, _) when is_integer(integer) do
        quote do: {:op, unquote(line), :-, {:integer, unquote(line), unquote(integer)}}
    end

    defp typespec({{:".", line, [{:atom, _, remote}, name]}, _, args}, vars) do
        remote_type({typespec(remote, vars), line, typespec(name, vars), args}, vars)
    end

    defp typespec({{:".", line, [{:__aliases__, line, _} = alias, name]}, line2, args}, vars) do
        {remote, _} = Code.eval_quoted(alias)
        typespec({{:".", line, [{:atom, line, remote}, name]}, line2, args}, vars)
    end

    defp typespec([], vars) do
        typespec({:nil, 0, []}, vars)
    end

    defp typespec([spec], vars) do
        typespec({:"list", 0, [spec]}, vars)
    end

    defp typespec(l, _) when is_list(l) do
        throw({:badarg,l})
    end

    defp typespec({:tuple, line, atom}, vars) when atom in [:quoted, nil] do
        typespec({:"{}", line, []}, vars)
    end

    defp typespec({:"{}", line, []}, _) do
        quote do: {:type, unquote(line), :tuple, :any}
    end

    defp typespec({:"{}", line, t}, vars) when is_list(t) do
        quote do: {:type, unquote(line), :tuple, unquote((lc e in t, do: typespec(e, vars)))}
    end
    

    defp typespec({{:fun, line, atom}, returns}, vars) when atom in [:quoted, nil] do
        typespec({{:fun, line, [{:type, line, :any}]}, returns}, vars)
    end

    defp typespec({{:fun, line, [{:type, _, :any}]}, returns}, vars) do
        quote do: {:type, unquote(line), :fun, [{:type, unquote(line), :any}, unquote(typespec(returns, vars))]}

    end

    defp typespec({{:fun, line, arguments}, returns}, vars) do
        arguments = lc arg in arguments, do: typespec(arg, vars)
        quote do: {:type, unquote(line), :fun, [{:type, unquote(line), :product, unquote(arguments)},
                                                unquote(typespec(returns, vars))]}
    end

    defp typespec({name, line, atom}, vars) when atom in [:quoted, nil] do
        if List.member?(vars, name) do
            quote do: {:var, unquote(line), unquote(name)}
        else
            typespec({name, line, []}, vars)
        end
    end

    defp typespec({name, line, arguments}, vars) do
        arguments = lc arg in arguments, do: typespec(arg, vars)
        quote do: {:type, unquote(line), unquote(name), unquote(arguments)}
    end

    defp typespec(name, vars) when is_atom(name) do
        typespec({name, 0, []}, vars)
    end

    defp typespec(t, vars) when is_tuple(t) do
        quote do: {:type, 0, :tuple, unquote((lc e in tuple_to_list(t), do: typespec(e, vars)))}
    end

    defp variable({name, line, _}) do
        {:var, line, name}
    end

    defp _deftype({name, _, args}, definition, tail, export) do
        args = 
        case args do
            :quoted -> []
            nil -> []
            _ -> lc arg in args, do: variable(arg)
        end
        definition = pre_process(definition, tail)
        spec = typespec(definition, lc {:var, _, var} in args, do: var)
        vars = lc {:var, line, name} in args, do: (quote do: {:var, unquote(line), unquote(name)})
        type = quote do: {unquote(name), unquote(spec), unquote(vars)}
        quote do
            Module.add_attribute __MODULE__, :type, unquote(type)
            if unquote(export) do
                Module.add_attribute __MODULE__, :export_type, [{unquote(name), unquote(length(vars))}]
            end
            {:type, unquote(type)}
        end
    end

    defmacro deftype(name), do: _deftype(name, (quote do: term), [], true)
    defmacro deftype(name, [{:as, definition}|t]), do: _deftype(name, definition, t, true)
    defmacro deftypep(name, [{:as, definition}|t]), do: _deftype(name, definition, t, false)

    defmacro defspec({name, line, args},[{:returns, returns}]) do
        spec = typespec({{:fun, line, args}, returns}, [])
        code = quote do: {{unquote(name), unquote(length(args))}, [unquote(spec)]}
        quote do
            Module.add_attribute __MODULE__, :spec, unquote(code)
            {:spec, unquote(code)}
        end
    end
end