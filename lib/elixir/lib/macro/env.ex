defmodule Macro.Env do
  Record.deffunctions(__MODULE__, [:module, :file, :line, :function,
    :aliases, :context, :requires, :macros], except: [:writers, :extensions])

  @moduledoc """
  A record that contains compile time environment information,
  It can be accessed at any time by calling __ENV__.
  """

  @doc """
  Returns the current module name.
  """
  def module(record)

  @doc """
  Returns the current file name as a binary.
  """
  def file(record)

  @doc """
  Returns the current line as an integer.
  """
  def line(record)

  @doc """
  Returns a tuple as { Atom, Integer }, where the first element
  is the function name and the seconds its arity. Returns `nil`
  if not inside a function.
  """
  def function(record)

  @doc """
  Returns a list of two item tuples, where the first
  item is the aliased name and the second the actual name.
  """
  def aliases(record)

  @doc """
  Returns the context of the environment. It can be nil
  (default context), inside a guard or inside an assign.
  """
  def context(record)

  @doc """
  Returns the list of required modules.
  """
  def requires(record)

  @doc """
  Returns a list of macros imported from each module.
  """
  def macros(record)

  @doc """
  Returns a keywords list containing the file and line
  information as keys.
  """
  def location(record) do
    [file: file(record), line: line(record)]
  end

  @doc """
  Returns wether the compilation environment is currently
  inside a guard.
  """
  def in_guard?(record), do: context(record) == :guard

  @doc """
  Returns wether the compilation environment is currently
  inside a match clause.
  """
  def in_match?(record), do: context(record) == :assign
end