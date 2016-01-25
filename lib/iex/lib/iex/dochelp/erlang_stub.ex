defmodule IEx.DocHelp.ErlangStub do
  @moduledoc """
  Stub module to suggest possible addons for Erlang documentation.

  Currently only returns {:unknown, doc_list }
  with a suggestive error message.
  """

  @suggestive "and currently there is no helper installed to provide Erlang documentation"

  import IEx.DocHelp

  @behaviour IEx.DocHelp

  def documentation(module) do
    knowable(module, fn -> get_doc(module) end)
  end

  def documentation(module, function) do
    knowable(module, fn -> get_doc(module, function) end)
  end

  def documentation(module, function,arity) do
    knowable(module, fn -> get_doc(module, function,arity) end)
  end

  defp knowable(module,doc_func) do
    case erlang?(module) do
      true -> doc_func.()
      _  -> {:unknown, [{inspect(module), ""}]}
    end
  end

  defp erlang?(module), do: ! get_docs_available?(module)

  defp get_doc(module) do
    {:not_found, [{inspect(module), "#{inspect(module)} is an Erlang module\n #{@suggestive}"}]}
  end

  defp get_doc(module, function) do
    {:not_found, [{inspect(module), "#{inspect(module)}.#{inspect(function)} is an Erlang module function\n #{@suggestive}"}]}
  end

  defp get_doc(module, function, arity) do
     {:not_found, [{inspect(module), "#{inspect(module)}.#{inspect(function)}/#{to_string(arity)} is an Erlang module function\n #{@suggestive}"}]}
  end

end