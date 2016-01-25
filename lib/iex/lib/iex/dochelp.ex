defmodule IEx.DocHelp do
  @moduledoc """
  Documents the callbacks required for a DocHelp module.

  This interface consists of the functions

  * `documentation(module)`

  * `documentation(module, function)`

  * `documentation(module, function, arity)`

  All of these functions return a tuple of status and doc_list.

  * `{:found, doc_list}`     - Documentation was found

  * `{:not_found, doc_list}` - Documentation was not found and
    this helper is sure it can't be found

  * `{:unknown, doc_list}`   - Helper does not know how to find docs for
    arguments given.

  doc_list consists of a list of `{header, markdown}` tuples for each
  item found.

  The module also includes some helper functions for writing
  DocHelp implementations.
  """

  @type header :: String.t
  @type body :: String.t
  @type doc_tuple :: {header, body}
  @type doc_list :: [doc_tuple]
  @type doc_return :: {:found, doc_list} | {:not_found, doc_list} | {:unknown, doc_list}

  @doc """
  Return the summary documentation for a module.

  The return value is a tuple of status and doc_list.
  """
  @callback documentation(module) :: doc_return

  @doc """
  Return the documentation for all arities of the function in a module.

  The return value is a tuple of status and doc_list.
  """
  @callback documentation(module, atom) :: doc_return

  @doc """
  Return the documentation for a specific arity of the function in a module.

  The return value is a tuple of status and doc_list.
  """
  @callback documentation(module, atom, integer) :: doc_return

  @doc """
  Checks whether `Code.get_docs` should have documentation for the module.
  """
  def get_docs_available?(module) do
    module
    |> Atom.to_string
    |> String.starts_with?("Elixir.")
  end

  @doc """
  Returns error message when documentation is not found.

  """
  def not_found_doc_return(module) do
    {:not_found, [{inspect(module), "No moduledocs found\n"}]}
  end

  @doc """
  Returns error message when documentation is not found.

  """
  def not_found_doc_return(module, function) do
    {:not_found,
      [{"#{inspect module}.#{function}", "No documentation for #{inspect module}.#{function} found\n"}]}
  end

  @doc """
  Returns error message when documentation is not found.

  """
  def not_found_doc_return(module, function, arity) do
    {:not_found,
      [{"#{inspect module}.#{function}", "No documentation for #{inspect module}.#{function}/#{arity} found\n"}]}
  end

end