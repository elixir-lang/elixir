defmodule Macro.Env do
  @moduledoc """
  A struct that holds compile time environment information.

  The current environment can be accessed at any time as
  `__ENV__/0`. Inside macros, the caller environment can be
  accessed as `__CALLER__/0`.

  An instance of `Macro.Env` must not be modified by hand. If you need to
  create a custom environment to pass to `Code.eval_quoted/3`, use the
  following trick:

      def make_custom_env do
        import SomeModule, only: [some_function: 2]
        alias A.B.C
        __ENV__
      end

  You may then call `make_custom_env()` to get a struct with the desired
  imports and aliases included.

  It contains the following fields:

    * `aliases` -  a list of two-element tuples, where the first
      element is the aliased name and the second one the actual name
    * `context` - the context of the environment; it can be `nil`
      (default context), `:guard` (inside a guard) or `:match` (inside a match)
    * `context_modules` - a list of modules defined in the current context
    * `file` - the current file name as a binary
    * `function` - a tuple as `{atom, integer}`, where the first
      element is the function name and the second its arity; returns
      `nil` if not inside a function
    * `functions` - a list of functions imported from each module
    * `line` - the current line as an integer
    * `macro_aliases` - a list of aliases defined inside the current macro
    * `macros` - a list of macros imported from each module
    * `module` - the current module name
    * `requires` - the list of required modules

  The following fields are private to Elixir's macro expansion mechanism and
  must not be accessed directly:

    * `lexical_tracker`
    * `tracers`
    * `versioned_vars`

    * `current_vars`
    * `unused_vars`

  """

  @type aliases :: [{module, module}]
  @type context :: :match | :guard | nil
  @type context_modules :: [module]
  @type file :: binary
  @type functions :: [{module, [name_arity]}]
  @type lexical_tracker :: pid | nil
  @type line :: non_neg_integer
  @type macro_aliases :: [{module, {term, module}}]
  @type macros :: [{module, [name_arity]}]
  @type name_arity :: {atom, arity}
  @type requires :: [module]
  @type variable :: {atom, atom | term}

  @typep tracers :: [module]
  @typep var_version :: non_neg_integer
  @typep versioned_vars :: %{optional(variable) => var_version}

  @typep current_vars ::
           {%{optional(variable) => var_version}, %{optional(variable) => var_version} | false}
  @typep unused_vars ::
           {%{optional({atom, var_version}) => non_neg_integer | false}, non_neg_integer}

  @type t :: %{
          __struct__: __MODULE__,
          aliases: aliases,
          context: context,
          context_modules: context_modules,
          current_vars: current_vars,
          file: file,
          function: name_arity | nil,
          functions: functions,
          lexical_tracker: lexical_tracker,
          line: line,
          macro_aliases: macro_aliases,
          macros: macros,
          module: module,
          unused_vars: unused_vars,
          requires: requires,
          tracers: tracers,
          versioned_vars: versioned_vars
        }

  # Define the __struct__ callbacks by hand for bootstrap reasons.
  @doc false
  def __struct__ do
    %{
      __struct__: __MODULE__,
      aliases: [],
      context: nil,
      context_modules: [],
      current_vars: {%{}, %{}},
      file: "nofile",
      function: nil,
      functions: [],
      lexical_tracker: nil,
      line: 0,
      macro_aliases: [],
      macros: [],
      module: nil,
      requires: [],
      tracers: [],
      unused_vars: {%{}, 0},
      versioned_vars: %{}
    }
  end

  @doc false
  def __struct__(kv) do
    Enum.reduce(kv, __struct__(), fn {k, v}, acc -> :maps.update(k, v, acc) end)
  end

  @doc """
  Returns a list of variables in the current environment.

  Each variable is identified by a tuple of two elements,
  where the first element is the variable name as an atom
  and the second element is its context, which may be an
  atom or an integer.
  """
  @doc since: "1.7.0"
  @spec vars(t) :: [variable]
  def vars(env)

  def vars(%{__struct__: Macro.Env, versioned_vars: vars}) do
    Map.keys(vars)
  end

  @doc """
  Checks if a variable belongs to the environment.
  """
  @doc since: "1.7.0"
  @spec has_var?(t, variable) :: boolean()
  def has_var?(env, var)

  def has_var?(%{__struct__: Macro.Env, versioned_vars: vars}, var) do
    Map.has_key?(vars, var)
  end

  @doc """
  Returns a keyword list containing the file and line
  information as keys.
  """
  @spec location(t) :: keyword
  def location(env)

  def location(%{__struct__: Macro.Env, file: file, line: line}) do
    [file: file, line: line]
  end

  @doc """
  Returns a `Macro.Env` in the match context.
  """
  @spec to_match(t) :: t
  def to_match(%{__struct__: Macro.Env} = env) do
    %{env | context: :match}
  end

  @doc """
  Returns whether the compilation environment is currently
  inside a guard.
  """
  @spec in_guard?(t) :: boolean
  def in_guard?(env)
  def in_guard?(%{__struct__: Macro.Env, context: context}), do: context == :guard

  @doc """
  Returns whether the compilation environment is currently
  inside a match clause.
  """
  @spec in_match?(t) :: boolean
  def in_match?(env)
  def in_match?(%{__struct__: Macro.Env, context: context}), do: context == :match

  @doc """
  Returns the environment stacktrace.
  """
  @spec stacktrace(t) :: list
  def stacktrace(%{__struct__: Macro.Env} = env) do
    cond do
      is_nil(env.module) ->
        [{:elixir_compiler, :__FILE__, 1, relative_location(env)}]

      is_nil(env.function) ->
        [{env.module, :__MODULE__, 0, relative_location(env)}]

      true ->
        {name, arity} = env.function
        [{env.module, name, arity, relative_location(env)}]
    end
  end

  defp relative_location(env) do
    [file: String.to_charlist(Path.relative_to_cwd(env.file)), line: env.line]
  end
end
