defmodule IEx.H_Elixir do

  @moduledoc """
	This module duplicates the prior behaviour of the iex h
	command using the new dynamic backend interface. 
	"""

	def documentation(module) do 
		case is_elixir?(module) do
			true -> get_doc(module)
			_	 -> {:unknown, [{inspect( module), ""}]} 
		end 
	end 

	def documentation(module, function) do 
		case is_elixir?(module) do
			true -> get_doc(module, function)
			_	 -> {:unknown, [{inspect( module), ""}]} 
		end 
	end 

  def documentation(module, function, arity) do 
    case is_elixir?(module) do
      true -> get_doc(module, function, arity)
      _  -> {:unknown, [{inspect( module), ""}]} 
    end 
  end 


	def is_elixir?(module) do
		Atom.to_string(module) |>
		String.starts_with?("Elixir.")
	end

	def get_doc(module) when is_atom(module) do
		docs = Code.get_docs(module, :moduledoc)
		case docs do 
			nil -> {:not_found, [{inspect(module), "No moduledocs found\n"}]}
			_   -> {:found, [{inspect(module), elem(docs, 1)}]}
		end 
	end 

	def get_doc(module, function) when is_atom(module) and is_atom(function) do
		docs = Code.get_docs(module, :docs)
		case docs do 
			nil -> {:not_found, [{"#{inspect module}.#{function}", "No documentation for #{inspect module}.#{function} found\n"}]}
			_   -> find_doc(docs, module, function)
		end 
	end 

  def get_doc(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    docs = Code.get_docs(module, :docs)
    case docs do 
      nil -> {:not_found, [{"#{inspect module}.#{function}", "No documentation for #{inspect module}.#{function} found\n"}]}
      _   -> find_doc(docs, module, function, arity)
    end 
  end 

	#  match on all arities.
	def find_doc(docs, module ,function) do
		doc_list = docs |> Enum.filter(fn(x) -> match_function(x, function) end) 
		case doc_list do 
			[] -> {:not_found, [{"#{inspect module}.#{function}", "No documentation for #{inspect module}.#{function} found\n"}]}
			_  -> {:found, get_docstrings(doc_list)}
		end 
	end 

  #  match on all arities.
  def find_doc(docs, module ,function, arity) do
    doc_list = docs |> Enum.filter(fn(x) -> match_function(x, function, arity) end) 
    case doc_list do 
      [] -> {:not_found, [{"#{inspect module}.#{function}/#{arity}", "No documentation for #{inspect module}.#{function}/#{arity} found\n"}]}
      _  -> {:found, get_docstrings(doc_list) }
    end 
  end 

  defp get_docstrings(doc_list) do
    for {{func, _arity}, _line, type, args, docstring } <- doc_list do
      {"#{to_string(type)} "<>"#{to_string(func)}"<>stringify_args(args), docstring }
    end
  end 

  # Turn this [{:string, [], nil}, {:char, [], nil}] into this (string, char)
  defp stringify_args(args) do
    inner = args |> Enum.map(fn(tp) -> format_doc_arg(tp) end) |> Enum.join(", ")
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

  # Not happy about magic numbers in elem.
  defp match_function(docstring, function) do
    {func, _arity} = elem(docstring, 0)
    function == func 
  end 

  # Not happy about magic numbers in elem.
  # To duplicate current iex behaviour this should 
  # match foo/1 when foo/2 has a default second arg. 
  defp match_function(docstring, function, arity) do 
    case {function, arity} == elem(docstring,0) do
    	true  -> true
      false -> find_default_doc(docstring, function, arity)
    end 
  end 
end