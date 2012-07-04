# We generate this record using a raw module due to
# bootstrap constraints. Notice the fields are not
# represented by a keywords list because we want
# to keep control over the order.
defmodule Macro.Env do
  @doc """
  A record that contains compile time environment information,
  It can be accessed at any time by calling __ENV__.
  """

  def __record__(kind, _), do: __record__(kind)
  def __record__(:name),   do: Macro.Env

  # When adding removing new fields,
  # src/elixir_tree_helpers.erl needs to be changed as well.
  def __record__(:fields) do
    [{:module,nil},{:file,nil},{:line,nil},{:function,nil},{:aliases,nil},
     {:context,nil},{:requires,nil},{:macros,nil}]
  end

  @doc """
  Returns the current module name.
  """
  def module(record),  do: elem(record, 2)

  @doc """
  Returns the current file name as a binary.
  """
  def file(record),    do: elem(record, 3)

  @doc """
  Returns the current line as an integer.
  """
  def line(record),    do: elem(record, 4)

  @doc """
  Returns a tuple as { Atom, Integer }, where the first element
  is the function name and the seconds its arity. Returns `nil`
  if not inside a function.
  """
  def function(record), do: elem(record, 5)

  @doc """
  Returns a list of two item tuples, where the first
  item is the aliased name and the second the actual name.
  """
  def aliases(record), do: elem(record, 6)

  @doc """
  Returns wether the compilation environment is currently
  inside a guard.
  """
  def in_guard?(record), do: elem(record, 7) == :guard

  @doc """
  Returns wether the compilation environment is currently
  inside a match clause.
  """
  def in_match?(record), do: elem(record, 7) == :assign

  @doc """
  Returns the list of required modules.
  """
  def requires(record), do: elem(record, 8)

  @doc """
  Returns a list of macros imported from each module.
  """
  def macros(record), do: elem(record, 9)

  @doc """
  Returns a keywords list containing the file and line
  information as keys.
  """
  def location(record) do
    [file: file(record), line: line(record)]
  end
end