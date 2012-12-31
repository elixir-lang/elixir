defmodule Macro.Env do
  @moduledoc """
  A record that holds compile time environment information.

  The current environment can be accessed at any time as
  `__ENV__`. Inside macros, the caller environment can be
  accessed as `__CALLER__`. It contains the following fields:

  * `module` - the current module name.
  * `file` - the current file name as a binary
  * `line` - the current line as an integer
  * `function` - a tuple as `{ atom, integer` }, where the first
    element is the function name and the seconds its arity. Returns
    `nil` if not inside a function
  * `aliases` -  a list of two item tuples, where the first
    item is the aliased name and the second the actual name
  * `context` - the context of the environment. It can be nil
    (default context), inside a guard or inside an assign
  * `requires` - the list of required modules
  * `functions` - a list of functions imported from each module
  * `macros` - a list of macros imported from each module
  """

  @type name_arity :: { atom, non_neg_integer }
  @type file :: binary
  @type line :: non_neg_integer
  @type aliases :: [{ module, module }]
  @type context :: :assign | :guard | nil
  @type requires :: [module]
  @type functions :: [{ module, [name_arity] }]
  @type macros :: [{ module, [name_arity] }]

  fields = [:module, :file, :line, :function,
            :aliases, :context, :requires, :functions, :macros]

  types  = quote do: [module: module, file: file, line: line,
    function: name_arity, aliases: aliases, requires: requires,
    functions: functions, macros: macros]

  Record.deffunctions(fields, __MODULE__)
  Record.deftypes(fields, types, __MODULE__)

  @doc """
  Returns a keyword list containing the file and line
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

  @doc """
  Returns the environment stacktrace.
  """
  def stacktrace(record) do
    case record.function do
      { name, arity } -> [{ module(record), name, arity, location(record) }]
      nil -> [{ module(record), location(record) }]
    end
  end
end
