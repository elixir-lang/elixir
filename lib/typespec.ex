defmodule Typespec do

    defp remote_type({remote, line, name, arguments}, vars, caller) do
        arguments =  lc arg in arguments, do: typespec(arg, vars, caller)
        quote do: {:remote_type, unquote(line), [unquote(remote), unquote(name), unquote(arguments)]}
    end

    def pre_process({:fun, line, args}, [{:returns, v}]) do
        {{:fun, line, args}, v}
    end
    def pre_process(other, _) do
        other
    end

    defp collect_union({:"|", _, [a, b]}), do: [b|collect_union(a)]
    defp collect_union(v), do: [v]

    defp typespec({:"|", line, [_,_]} = orexpr, vars, caller) do
        union = lc e in :lists.reverse(collect_union(orexpr)), do: typespec(e, vars, caller)
        quote do: {:type, unquote(line), :union, lc e in unquote(union), do: e}
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
        atom = Macro.expand_aliases alias, caller
        typespec(atom, vars, caller)
    end

    defp typespec(atom, _, _) when is_atom(atom) do
        quote do: {:atom, 0, unquote(atom)}
    end
    
    defp typespec(integer, _, _) when is_integer(integer) do
        quote do: {:integer, 0, unquote(integer)}
    end

    defp typespec({:"=", line,[var,expr]}, vars, caller) do
        quote do: {:ann_type, unquote(line), [unquote(typespec(var, [elem(var,1)|vars], caller)), unquote(typespec(expr, vars, caller))]}
    end

    defp typespec({:"-", line, [integer]}, _, _) when is_integer(integer) do
        quote do: {:op, unquote(line), :-, {:integer, unquote(line), unquote(integer)}}
    end

    defp typespec({{:".", line, [{:atom, _, remote}, name]}, _, args}, vars, caller) do
        remote_type({typespec(remote, vars, caller), line, typespec(name, vars, caller), args}, vars, caller)
    end

    defp typespec({{:".", line, [{:__aliases__, line, _} = alias, name]}, line2, args}, vars, caller) do
        remote = Macro.expand_aliases alias, caller
        typespec({{:".", line, [{:atom, line, remote}, name]}, line2, args}, vars, caller)
    end

    defp typespec([], vars, caller) do
        typespec({:nil, 0, []}, vars, caller)
    end

    defp typespec([spec], vars, caller) do
        typespec({:"list", 0, [spec]}, vars, caller)
    end

    defp typespec(l, _, _) when is_list(l) do
        throw({:badarg,l})
    end

    defp typespec({:tuple, line, atom}, vars, caller) when atom in [:quoted, nil] do
        typespec({:"{}", line, []}, vars, caller)
    end

    defp typespec({:"{}", line, []}, _, _) do
        quote do: {:type, unquote(line), :tuple, :any}
    end

    defp typespec({:"{}", line, t}, vars, caller) when is_list(t) do
        quote do: {:type, unquote(line), :tuple, unquote((lc e in t, do: typespec(e, vars, caller)))}
    end
    

    defp typespec({{:fun, line, atom}, returns}, vars, caller) when atom in [:quoted, nil] do
        typespec({{:fun, line, [{:type, line, :any}]}, returns}, vars, caller)
    end

    defp typespec({{:fun, line, [{:type, _, :any}]}, returns}, vars, caller) do
        quote do: {:type, unquote(line), :fun, [{:type, unquote(line), :any}, unquote(typespec(returns, vars, caller))]}

    end

    defp typespec({{:fun, line, arguments}, returns}, vars, caller) do
        arguments = lc arg in arguments, do: typespec(arg, vars, caller)
        quote do: {:type, unquote(line), :fun, [{:type, unquote(line), :product, unquote(arguments)},
                                                unquote(typespec(returns, vars, caller))]}
    end

    defp typespec({name, line, atom}, vars, caller) when atom in [:quoted, nil] do
        if List.member?(vars, name) do
            quote do: {:var, unquote(line), unquote(name)}
        else
            typespec({name, line, []}, vars, caller)
        end
    end

    defp typespec({name, line, arguments}, vars, caller) do
        arguments = lc arg in arguments, do: typespec(arg, vars, caller)
        quote do: {:type, unquote(line), unquote(name), unquote(arguments)}
    end

    defp typespec(name, vars, caller) when is_atom(name) do
        typespec({name, 0, []}, vars, caller)
    end

    defp typespec(t, vars, caller) when is_tuple(t) do
        quote do: {:type, 0, :tuple, unquote((lc e in tuple_to_list(t), do: typespec(e, vars, caller)))}
    end

    defp variable({name, line, _}) do
        {:var, line, name}
    end

    defp _deftype({name, _, args}, definition, tail, export, caller) do
        args = 
        case args do
            :quoted -> []
            nil -> []
            _ -> lc arg in args, do: variable(arg)
        end
        definition = pre_process(definition, tail)
        spec = typespec(definition, (lc {:var, _, var} in args, do: var), caller)
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

    defmacro deftype(name), do: _deftype(name, (quote do: term), [], true, __CALLER__)
    defmacro deftype(name, [{:as, definition}|t]), do: _deftype(name, definition, t, true, __CALLER__)
    defmacro deftypep(name, [{:as, definition}|t]), do: _deftype(name, definition, t, false, __CALLER__)

    defmacro defspec({name, line, args},[{:returns, returns}]) do
        spec = typespec({{:fun, line, args}, returns}, [], __CALLER__)
        code = quote do: {{unquote(name), unquote(length(args))}, [unquote(spec)]}
        quote do
            Module.add_attribute __MODULE__, :spec, unquote(code)
            {:spec, unquote(code)}
        end
    end
end