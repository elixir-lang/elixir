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
        import SomeModule, only: [some_function: 2], warn: false
        alias A.B.C, warn: false
        __ENV__
      end

  You may then call `make_custom_env()` to get a struct with the desired
  imports and aliases included.

  It contains the following fields:

    * `context` - the context of the environment; it can be `nil`
      (default context), `:guard` (inside a guard) or `:match` (inside a match)
    * `context_modules` - a list of modules defined in the current context
    * `file` - the current absolute file name as a binary
    * `function` - a tuple as `{atom, integer}`, where the first
      element is the function name and the second its arity; returns
      `nil` if not inside a function
    * `line` - the current line as an integer
    * `module` - the current module name

  The following fields are private to Elixir's macro expansion mechanism and
  must not be accessed directly:

    * `aliases`
    * `functions`
    * `macro_aliases`
    * `macros`
    * `lexical_tracker`
    * `requires`
    * `tracers`
    * `versioned_vars`

  """

  @type context :: :match | :guard | nil
  @type context_modules :: [module]
  @type file :: binary
  @type line :: non_neg_integer
  @type name_arity :: {atom, arity}
  @type variable :: {atom, atom | term}

  @typep aliases :: [{module, module}]
  @typep functions :: [{module, [name_arity]}]
  @typep lexical_tracker :: pid | nil
  @typep macro_aliases :: [{module, {term, module}}]
  @typep macros :: [{module, [name_arity]}]
  @typep requires :: [module]
  @typep tracers :: [module]
  @typep versioned_vars :: %{optional(variable) => var_version :: non_neg_integer}

  @type t :: %{
          __struct__: __MODULE__,
          aliases: aliases,
          context: context,
          context_modules: context_modules,
          file: file,
          function: name_arity | nil,
          functions: functions,
          lexical_tracker: lexical_tracker,
          line: line,
          macro_aliases: macro_aliases,
          macros: macros,
          module: module,
          requires: requires,
          tracers: tracers,
          versioned_vars: versioned_vars
        }

  fields = [
    aliases: [],
    context: nil,
    context_modules: [],
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
    versioned_vars: %{}
  ]

  # Define the __struct__ callbacks by hand for bootstrap reasons.
  {struct, [], kv, body} = Kernel.Utils.defstruct(__MODULE__, fields, false, __ENV__)
  def __struct__(), do: unquote(:elixir_quote.escape(struct, false, :none))
  def __struct__(unquote(kv)), do: unquote(body)

  @doc """
  Prunes compile information from the environment.

  This happens when the environment is captured at compilation
  time, for example, in the module body, and then used to
  evaluate code after the module has been defined.
  """
  @doc since: "1.14.0"
  @spec prune_compile_info(t) :: t
  def prune_compile_info(env) do
    %{env | lexical_tracker: nil, tracers: []}
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

  ## Examples

      iex> x = 13
      iex> x
      13
      iex> Macro.Env.has_var?(__ENV__, {:x, nil})
      true
      iex> Macro.Env.has_var?(__ENV__, {:unknown, nil})
      false

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
  Fetches the alias for the given atom.

  Returns `{:ok, alias}` if the alias exists, `:error`
  otherwise.

  ## Examples

      iex> alias Foo.Bar, as: Baz
      iex> Baz
      Foo.Bar
      iex> Macro.Env.fetch_alias(__ENV__, :Baz)
      {:ok, Foo.Bar}
      iex> Macro.Env.fetch_alias(__ENV__, :Unknown)
      :error

  """
  @doc since: "1.13.0"
  @spec fetch_alias(t, atom) :: {:ok, atom} | :error
  def fetch_alias(env, atom)

  def fetch_alias(%{__struct__: Macro.Env, aliases: aliases}, atom) when is_atom(atom),
    do: Keyword.fetch(aliases, :"Elixir.#{atom}")

  @doc """
  Fetches the macro alias for the given atom.

  Returns `{:ok, macro_alias}` if the alias exists, `:error`
  otherwise.

  A macro alias is only used inside quoted expansion. See
  `fetch_alias/2` for a more general example.
  """
  @doc since: "1.13.0"
  @spec fetch_macro_alias(t, atom) :: {:ok, atom} | :error
  def fetch_macro_alias(env, atom)

  def fetch_macro_alias(%{__struct__: Macro.Env, macro_aliases: aliases}, atom)
      when is_atom(atom),
      do: Keyword.fetch(aliases, :"Elixir.#{atom}")

  @doc """
  Returns the modules from which the given `{name, arity}` was
  imported.

  It returns a list of two element tuples in the shape of
  `{:function | :macro, module}`. The elements in the list
  are in no particular order and the order is not guaranteed.

  ## Examples

      iex> Macro.Env.lookup_import(__ENV__, {:duplicate, 2})
      []
      iex> import Tuple, only: [duplicate: 2], warn: false
      iex> Macro.Env.lookup_import(__ENV__, {:duplicate, 2})
      [{:function, Tuple}]
      iex> import List, only: [duplicate: 2], warn: false
      iex> Macro.Env.lookup_import(__ENV__, {:duplicate, 2})
      [{:function, List}, {:function, Tuple}]

      iex> Macro.Env.lookup_import(__ENV__, {:def, 1})
      [{:macro, Kernel}]

  """
  @doc since: "1.13.0"
  @spec lookup_import(t, name_arity) :: [{:function | :macro, module}]
  def lookup_import(env, name_arity)

  def lookup_import(
        %{__struct__: Macro.Env, functions: functions, macros: macros},
        {name, arity} = pair
      )
      when is_atom(name) and is_integer(arity) do
    f = for {mod, pairs} <- functions, :ordsets.is_element(pair, pairs), do: {:function, mod}
    m = for {mod, pairs} <- macros, :ordsets.is_element(pair, pairs), do: {:macro, mod}
    f ++ m
  end

  @doc """
  Returns the names of any aliases for the given module or atom.

  ## Examples

      iex> alias Foo.Bar
      iex> Bar
      Foo.Bar
      iex> Macro.Env.lookup_alias_as(__ENV__, Foo.Bar)
      [Elixir.Bar]
      iex> alias Foo.Bar, as: Baz
      iex> Baz
      Foo.Bar
      iex> Macro.Env.lookup_alias_as(__ENV__, Foo.Bar)
      [Elixir.Bar, Elixir.Baz]
      iex> Macro.Env.lookup_alias_as(__ENV__, Unknown)
      []

  """
  @doc since: "1.15.0"
  @spec lookup_alias_as(t, atom) :: [atom]
  def lookup_alias_as(env, atom)

  def lookup_alias_as(%{__struct__: Macro.Env, aliases: aliases}, atom) when is_atom(atom) do
    for {name, ^atom} <- aliases, do: name
  end

  @doc """
  Returns `true` if the given module has been required.

  ## Examples

      iex> Macro.Env.required?(__ENV__, Integer)
      false
      iex> require Integer
      iex> Macro.Env.required?(__ENV__, Integer)
      true

      iex> Macro.Env.required?(__ENV__, Kernel)
      true
  """
  @doc since: "1.13.0"
  @spec required?(t, module) :: boolean
  def required?(env, module)

  def required?(%{__struct__: Macro.Env, requires: requires}, mod) when is_atom(mod),
    do: mod in requires

  @doc """
  Prepend a tracer to the list of tracers in the environment.

  ## Examples

      Macro.Env.prepend_tracer(__ENV__, MyCustomTracer)

  """
  @doc since: "1.13.0"
  @spec prepend_tracer(t, module) :: t
  def prepend_tracer(%{__struct__: Macro.Env, tracers: tracers} = env, tracer) do
    %{env | tracers: [tracer | tracers]}
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
