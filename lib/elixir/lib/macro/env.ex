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

    * `module` - the current module name
    * `file` - the current file name as a binary
    * `line` - the current line as an integer
    * `function` - a tuple as `{atom, integer}`, where the first
      element is the function name and the second its arity; returns
      `nil` if not inside a function
    * `context` - the context of the environment; it can be `nil`
      (default context), `:guard` (inside a guard) or `:match` (inside a match)
    * `aliases` -  a list of two-element tuples, where the first
      element is the aliased name and the second one the actual name
    * `requires` - the list of required modules
    * `functions` - a list of functions imported from each module
    * `macros` - a list of macros imported from each module
    * `macro_aliases` - a list of aliases defined inside the current macro
    * `context_modules` - a list of modules defined in the current context

  The following fields are private to Elixir's macro expansion mechanism and
  must not be accessed directly. See the functions in this module that exposes
  the relevant information from the fields below whenever necessary:

    * `current_vars`
    * `prematch_vars`
    * `contextual_vars`
    * `lexical_tracker`
    * `tracers`

  The following fields are deprecated and must not be accessed or relied on:

    * `vars` - a list keeping all defined variables as `{var, context}`

  """

  @type name_arity :: {atom, arity}
  @type file :: binary
  @type line :: non_neg_integer
  @type aliases :: [{module, module}]
  @type macro_aliases :: [{module, {term, module}}]
  @type context :: :match | :guard | nil
  @type requires :: [module]
  @type functions :: [{module, [name_arity]}]
  @type macros :: [{module, [name_arity]}]
  @type context_modules :: [module]
  @type lexical_tracker :: pid | nil
  @type variable :: {atom, atom | term}

  @typep vars :: [variable]
  @typep var_type :: :term
  @typep var_version :: non_neg_integer
  @typep current_vars ::
           {%{optional(variable) => {var_version, var_type}},
            %{optional({variable, var_version}) => non_neg_integer | false}}
  @typep prematch_vars ::
           %{optional(variable) => {var_version, var_type}} | :warn | :raise | :pin | :apply
  @typep contextual_vars :: [atom]
  @typep tracers :: [module]

  @type t :: %{
          __struct__: __MODULE__,
          module: atom,
          file: file,
          line: line,
          function: name_arity | nil,
          context: context,
          requires: requires,
          aliases: aliases,
          functions: functions,
          macros: macros,
          macro_aliases: macro_aliases,
          context_modules: context_modules,
          vars: vars,
          current_vars: current_vars,
          prematch_vars: prematch_vars,
          lexical_tracker: lexical_tracker,
          contextual_vars: contextual_vars,
          tracers: tracers
        }

  # TODO: Remove :vars field on v2.0
  def __struct__ do
    %{
      __struct__: __MODULE__,
      module: nil,
      file: "nofile",
      line: 0,
      function: nil,
      context: nil,
      requires: [],
      aliases: [],
      functions: [],
      macros: [],
      macro_aliases: [],
      context_modules: [],
      vars: [],
      current_vars: {%{}, %{}},
      prematch_vars: :warn,
      lexical_tracker: nil,
      contextual_vars: [],
      tracers: []
    }
  end

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

  def vars(%{__struct__: Macro.Env, current_vars: {current_vars, _}}) do
    Map.keys(current_vars)
  end

  @doc """
  Checks if a variable belongs to the environment.
  """
  @doc since: "1.7.0"
  @spec has_var?(t, variable) :: boolean()
  def has_var?(env, var)

  def has_var?(%{__struct__: Macro.Env, current_vars: {current_vars, _}}, var) do
    Map.has_key?(current_vars, var)
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
  def to_match(%{__struct__: Macro.Env, context: :match} = env) do
    env
  end

  def to_match(%{__struct__: Macro.Env, current_vars: {vars, _}} = env) do
    %{env | context: :match, prematch_vars: vars}
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
