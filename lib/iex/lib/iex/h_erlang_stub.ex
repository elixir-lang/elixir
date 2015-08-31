defmodule IEx.H_Erlang_Stub do
  @moduledoc """
  Stub module to suggest possible addons for Erlang documentation. 
  """

  def documentation(module) do 
    case is_erlang?(module) do
      true -> get_doc(module)
      _    -> {:unknown, [{inspect(module), "" }]} 
    end 
  end 
  
  def documentation(module, function) do 
    case is_erlang?(module) do
      true -> get_doc(module, function)
      _    -> {:unknown, [{inspect(module), "" }]} 
    end 
  end 

  def documentation(module, function,arity) do 
    case is_erlang?(module) do
      true -> get_doc(module, function,arity)
      _    -> {:unknown, [{inspect(module), "" }]} 
    end 
  end 
  
  def is_erlang?(module) do
    case is_elixir?(module) do 
      true -> false
      _    -> true
    end 
  end

  def is_elixir?(module) do
    Atom.to_string(module) |>
    String.starts_with?("Elixir.")
  end

  def get_doc(module) do
    {:not_found, [{inspect(module), "#{inspect(module)} is an Erlang module\n and currently there is no helper installed to provide Erlang documentation"}]}
  end 

  def get_doc(module, function) do 
    {:not_found, [{inspect(module), "#{inspect(module)}.#{inspect(function)} is an Erlang module function\n and currently there is no helper installed to provide Erlang documentation"}]} 
  end 

  def get_doc(module, function, arity) do 
     {:not_found, [{inspect(module), "#{inspect(module)}.#{inspect(function)}/#{to_string(arity)} is an Erlang module function\n and currently there is no helper installed to provide Erlang documentation"}]}
  end 

end