defmodule IEx.DocHelp.CallBack do
  @moduledoc """
  This module implements a DocHelp behaviour that is used to
  provide the documentation for a callback.
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
    docs = Code.get_docs(module, :moduledoc)
    case docs do
      nil -> not_found_doc_return(module)
      _   -> {:found, [{inspect(module), elem(docs, 1)}]}
    end
  end

  defp get_doc(module, function) when is_atom(module) and is_atom(function) do
    docs = Code.get_docs(module, :callback_docs)
    case docs do
      nil -> not_found_doc_return(module, function)
      _   -> find_doc(docs, module, function)
    end
  end

  defp get_doc(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    docs = Code.get_docs(module, :callback_docs)
    case docs do
      nil -> not_found_doc_return(module, function, arity)
      _   -> find_doc(docs, module, function, arity)
    end
  end

  #  match on all arities.
  defp find_doc(docs, module, function) do
    doc_list = Enum.filter(docs, fn(x) -> match_function(x, function) end)
    case doc_list do
      [] -> not_found_doc_return(module, function)
      _  -> {:found, get_docstrings(doc_list)}
    end
  end

  defp find_doc(docs, module, function, arity) do
    doc_list = Enum.filter(docs, fn(x) -> match_function(x, function, arity) end)
    case doc_list do
      [] -> not_found_doc_return(module, function, arity)
      _  -> {:found, get_docstrings(doc_list) }
    end
  end

  defp get_docstrings(doc_list) do
    for {{func, _arity}, _line, type, docstring } <- doc_list do
      {"#{to_string(type)} "<>"#{to_string(func)}", docstring }
    end
  end

  defp match_function(docstring, function) do
    {func, _arity} = elem(docstring, 0)
    function == func
  end

  defp match_function(docstring, function, arity) do
    {function, arity} == elem(docstring, 0)
  end

end