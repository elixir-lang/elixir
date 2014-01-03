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
  * `context` - the context of the environment. It can be nil
    (default context), inside a guard or inside an assign
  * `aliases` -  a list of two item tuples, where the first
    item is the aliased name and the second the actual name
  * `requires` - the list of required modules
  * `functions` - a list of functions imported from each module
  * `macros` - a list of macros imported from each module
  * `macro_aliases` - a list of aliases defined inside the current macro
  * `context_modules` - a list of modules defined in the current context
  * `vars` - a list keeping all defined variables as { var, context }
  * `export_vars` - a list keeping all variables to be exported in a construct (may be nil)
  * `lexical_tracker` - PID to the lexical tracker which is responsible to keep user info
  * `local` - the module to expand local functions to
  """

  @type name_arity :: { atom, non_neg_integer }
  @type file :: binary
  @type line :: non_neg_integer
  @type aliases :: [{ module, module }]
  @type macro_aliases :: [{ module, { integer, module } }]
  @type context :: :match | :guard | nil
  @type requires :: [module]
  @type functions :: [{ module, [name_arity] }]
  @type macros :: [{ module, [name_arity] }]
  @type context_modules :: [module]
  @type vars :: [{ atom, atom | non_neg_integer }]
  @type export_vars :: vars | nil
  @type lexical_tracker :: pid
  @type local :: module | nil

  fields = [:module, :file, :line, :function, :context, :requires, :aliases, :functions,
            :macros, :macro_aliases, :context_modules, :vars, :export_vars, :lexical_tracker,
            :local]

  types  = quote do: [module: module, file: file, line: line,
    function: name_arity, context: context, requires: requires, aliases: aliases,
    functions: functions, macros: macros,  macro_aliases: aliases,
    context_modules: context_modules, vars: vars, export_vars: export_vars,
    lexical_tracker: lexical_tracker, local: local]

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
  Returns whether the compilation environment is currently
  inside a guard.
  """
  def in_guard?(record), do: context(record) == :guard

  @doc """
  Returns whether the compilation environment is currently
  inside a match clause.
  """
  def in_match?(record), do: context(record) == :match

  @doc """
  Returns the environment stacktrace.
  """
  def stacktrace(record) do
    cond do
      nil?(record.module) ->
        [{ :elixir_compiler, :__FILE__, 1, location(record) }]
      nil?(record.function) ->
        [{ module(record), :__MODULE__, 0, location(record) }]
      true ->
        { name, arity } = record.function
        [{ module(record), name, arity, location(record) }]
    end
  end
end
