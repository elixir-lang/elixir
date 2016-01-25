defmodule IEx.DocHelp.Elixir do
  @moduledoc """
  This module provides access to the documentation stored
  by Elixir code in the BEAM files.

  If the function implements a callback and does not have
  documentation, this module will search the module that defines
  the callback and provide that documentation for the function.
  """

  import IEx.DocHelp

  @behaviour IEx.DocHelp

  def documentation(module) do
    knowable(module, fn -> get_doc(module) end)
  end

  def documentation(module, function) do
    knowable(module, fn -> get_doc(module, function) end)
  end

  def documentation(module, function, arity) do
    knowable(module, fn -> get_doc(module, function, arity) end)
  end

  defp knowable(module,doc_func) do
    case get_docs_available?(module) do
      true -> doc_func.()
      _  -> {:unknown, [{inspect(module), ""}]}
    end
  end

  defp get_doc(module) when is_atom(module) do
    findable(module,
      :moduledoc,
      fn -> not_found_doc_return(module) end,
      fn(docs) -> {:found, [{inspect(module), elem(docs, 1)}]} end)
  end

  defp get_doc(module, function) when is_atom(module) and is_atom(function) do
    findable(module,
      :docs,
      fn -> not_found_doc_return(module, function) end,
      fn(docs) -> find_doc(docs, module, function) end)
  end

  defp get_doc(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
     findable(module,
      :docs,
      fn -> not_found_doc_return(module, function, arity) end,
      fn(docs) -> find_doc(docs, module, function, arity) end)
  end

  defp findable(module, type, not_found, found) do
    docs = Code.get_docs(module, type)
    case docs do
      nil -> not_found.()
      _   -> found.(docs)
    end
  end

  defp find_doc(docs, module, function) do
    doc_list = Enum.filter(docs, fn(x) -> match_function(x, function) end)
    case doc_list do
      [] -> not_found_doc_return(module,function)
      _  -> {:found, get_docstrings(module, doc_list)}
    end
  end

  defp find_doc(docs, module, function, arity) do
    doc_list = Enum.filter(docs, fn(x) -> match_function(x, function, arity) end)
    case doc_list do
      [] -> not_found_doc_return(module, function, arity)
      _  -> {:found, get_docstrings(module, doc_list)}
    end
  end

  # Check both the documentation for the function as callback or
  # as a function in the module in which the callback is defined.
  defp get_callback_doc(module, function, arity) do
    cmodule = callback_module(module, function, arity)
    if is_nil(cmodule) do
      nil
    else
      case IEx.DocHelp.CallBack.documentation(cmodule, function, arity) do
        {:found, [{_header, nil}]} ->
          case documentation(cmodule, function, arity) do
            {:found, [{_header, doc}]} -> doc
            {:not_found, _list} -> nil
          end
        {:found, [{_header, doc}]} -> doc
        {:not_found, _list} -> nil
      end
    end
  end

  defp get_docstrings(module, doc_list) do
    for {{func, arity}, _line, type, args, docstring} <- doc_list do
      case docstring do
        nil -> {build_header(type, func, args), get_callback_doc(module, func, arity)}
        _ -> {build_header(type, func, args), docstring}
      end
    end
  end

  defp build_header(type, func, args) do
    "#{to_string(type)} "<>"#{to_string(func)}"<>stringify_args(args)
  end

  # Turn this [{:string, [], nil}, {:char, [], nil}] into this (string, char)
  defp stringify_args(args) do
    inner =
      args
      |> Enum.map(fn(tp) -> format_doc_arg(tp) end)
      |> Enum.join(", ")

    "("<>inner<>")"
  end

  defp format_doc_arg({:\\, _, [left, right]}) do
    format_doc_arg(left) <> " \\\\ " <> Macro.to_string(right)
  end

  defp format_doc_arg({var, _, _}) do
    Atom.to_string(var)
  end

  defp find_default_doc(doc, function, min) do
    case elem(doc, 0) do
      {^function, max} when max > min ->
        defaults = Enum.count elem(doc, 3), &match?({:\\, _, _}, &1)
        min + defaults >= max
      _ ->
        false
    end
  end

  defp match_function(docstring, function) do
    {func, _arity} = elem(docstring, 0)
    function == func
  end

  # To duplicate current iex behaviour this should
  # match foo/1 when foo/2 has a default second arg.
  defp match_function(docstring, function, arity) do
    case {function, arity} == elem(docstring, 0) do
      true  -> true
      false -> find_default_doc(docstring, function, arity)
    end
  end

  # Move this to IEx.DocHelp and make it public?
  # Returns module in which behaviour is defined.
  defp callback_module(mod, fun, arity) do
    mod.module_info(:attributes)
    |> Keyword.get_values(:behaviour)
    |> Stream.concat()
    |> Stream.filter(fn(module)->
      module.module_info(:attributes)
      |> Enum.filter_map(&match?({:callback, _}, &1), fn {_, [{t,_}|_]} -> t end)
      |> Enum.any?(&match?({^fun, ^arity}, &1))
    end)
    |> Enum.at(0)
  end

end