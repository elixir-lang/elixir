defmodule Typespec do
  @moduledoc """
  Public type specification API (as opposed to Kernel.Typespec)
  """

  @doc """
  Returns type spec retrieved from a BEAM file in a readable form
  """
  def to_binary(typespec) do
    Macro.to_binary(typespec_to_ast(typespec))
  end

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
    {:{}, line, args}
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

end