defmodule Macro.Env do
  @moduledoc """
  A struct that holds compile time environment information.

  The current environment can be accessed at any time as
  `__ENV__/0`. Inside macros, the caller environment can be
  accessed as `__CALLER__/0`.

  The majority of the functions in this module are provided
  for low-level tools, which need to integrate with the Elixir
  compiler, such as language servers and embedded languages.
  For regular usage in Elixir code and macros, you must use
  the `Macro` module instead. In particular, avoid modifying
  the `Macro.Env` struct directly and prefer to use high-level
  constructs, such as a `import`, `aliases`, and so forth to
  build your own environment. For example, to build a custom
  environment, you can define a function such as:

      def make_custom_env do
        import SomeModule, only: [some_function: 2], warn: false
        alias A.B.C, warn: false
        __ENV__
      end

  ## Struct fields

  The `Macro.Env` struct contains the following fields:

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
  {_struct, [], escaped_struct, kv, body} =
    Kernel.Utils.defstruct(__MODULE__, fields, false, __ENV__)

  def __struct__(), do: unquote(escaped_struct)
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

  # TODO: Deprecate on Elixir 1.21 in favor of expand_alias/4
  @doc false
  def fetch_alias(%{__struct__: Macro.Env, aliases: aliases}, atom) when is_atom(atom),
    do: Keyword.fetch(aliases, :"Elixir.#{atom}")

  # TODO: Deprecate on Elixir 1.21 in favor of expand_alias/4
  @doc false
  def fetch_macro_alias(%{__struct__: Macro.Env, macro_aliases: aliases}, atom)
      when is_atom(atom),
      do: Keyword.fetch(aliases, :"Elixir.#{atom}")

  @doc """
  Returns the modules from which the given `{name, arity}` was
  imported.

  It returns a list of two element tuples in the shape of
  `{:function | :macro, module}`. The elements in the list
  are in no particular order and the order is not guaranteed.

  > #### Use only for introspection {: .warning}
  >
  > This function does not emit compiler tracing events,
  > which may block the compiler from correctly tracking
  > dependencies. Use this function for reflection purposes
  > but to do not use it to expand imports into qualified
  > calls. Instead, use `expand_import/5`.

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

  trace_option = """
  `:trace` - when set to `false`, it disables compilation tracers and
  lexical tracker. This option must only be used by language servers and
  other tools that need to introspect code without affecting how it is compiled.
  Disabling tracer inside macros or regular code expansion is extremely
  discouraged as it blocks the compiler from accurately tracking dependencies\
  """

  @doc """
  Defines the given `module` as required in the environment.

  It does not check or assert the module is available.
  This is used by tools which need to mimic the Elixir compiler.
  The appropriate `:require` compiler tracing event will be emitted.

  ## Additional options

  It accepts the same options as `Kernel.SpecialForm.require/2` plus:

    * #{trace_option}

  ## Examples

      iex> env = __ENV__
      iex> Macro.Env.required?(env, Integer)
      false
      iex> {:ok, env} = Macro.Env.define_require(env, [line: 10], Integer)
      iex> Macro.Env.required?(env, Integer)
      true

  If the `:as` option is given, it will also define an alias:

      iex> env = __ENV__
      iex> {:ok, env} = Macro.Env.define_require(env, [line: 10], Foo.Bar, as: Baz)
      iex> Macro.Env.expand_alias(env, [], [:Baz])
      {:alias, Foo.Bar}

  """
  @doc since: "1.17.0"
  @spec define_require(t, Macro.metadata(), module) :: {:ok, t}
  def define_require(env, meta, module, opts \\ [])
      when is_list(meta) and is_atom(module) and is_list(opts) do
    {trace, opts} = Keyword.pop(opts, :trace, true)
    env = :elixir_aliases.require(meta, module, opts, env, trace)
    result = :elixir_aliases.alias(meta, module, false, opts, env, trace)
    maybe_define_error(result, :elixir_aliases)
  end

  @doc """
  Defines the given `module` as imported in the environment.

  It assumes `module` is available. This is used by tools which
  need to mimic the Elixir compiler. The appropriate `:import`
  compiler tracing event will be emitted.

  ## Additional options

  It accepts the same options as `Kernel.SpecialForm.import/2` plus:

    * `:emit_warnings` - emit warnings found when defining imports

    * #{trace_option}

    * `:info_callback` - a function to use instead of `c:Module.__info__/1`.
      The function will be invoked with `:functions` or `:macros` argument.
      It has to return a list of `{function, arity}` key value pairs.
      If it fails, it defaults to using module metadata based on `module_info/1`.

  ## Examples

      iex> env = __ENV__
      iex> Macro.Env.lookup_import(env, {:flatten, 1})
      []
      iex> {:ok, env} = Macro.Env.define_import(env, [line: 10], List)
      iex> Macro.Env.lookup_import(env, {:flatten, 1})
      [{:function, List}]

  It accepts the same options as `Kernel.SpecialForm.import/2`:

      iex> env = __ENV__
      iex> Macro.Env.lookup_import(env, {:is_odd, 1})
      []
      iex> {:ok, env} = Macro.Env.define_import(env, [line: 10], Integer, only: :macros)
      iex> Macro.Env.lookup_import(env, {:is_odd, 1})
      [{:macro, Integer}]

  ## Info callback override

      iex> env = __ENV__
      iex> Macro.Env.lookup_import(env, {:flatten, 1})
      []
      iex> {:ok, env} = Macro.Env.define_import(env, [line: 10], SomeModule, [info_callback: fn :functions -> [{:flatten, 1}]; :macros -> [{:some, 2}]; end])
      iex> Macro.Env.lookup_import(env, {:flatten, 1})
      [{:function, SomeModule}]
      iex> Macro.Env.lookup_import(env, {:some, 2})
      [{:macro, SomeModule}]

  """
  @doc since: "1.17.0"
  @spec define_import(t, Macro.metadata(), module, keyword) :: {:ok, t} | {:error, String.t()}
  def define_import(env, meta, module, opts \\ [])
      when is_list(meta) and is_atom(module) and is_list(opts) do
    {trace, opts} = Keyword.pop(opts, :trace, true)
    {warnings, opts} = Keyword.pop(opts, :emit_warnings, true)
    {info_callback, opts} = Keyword.pop(opts, :info_callback, &module.__info__/1)

    result = :elixir_import.import(meta, module, opts, env, warnings, trace, info_callback)
    maybe_define_error(result, :elixir_import)
  end

  @doc """
  Defines the given `as` an alias to `module` in the environment.

  This is used by tools which need to mimic the Elixir compiler.
  The appropriate `:alias` compiler tracing event will be emitted.

  ## Additional options

  It accepts the same options as `Kernel.SpecialForm.alias/2` plus:

    * #{trace_option}

  ## Examples

      iex> env = __ENV__
      iex> Macro.Env.expand_alias(env, [], [:Baz])
      :error
      iex> {:ok, env} = Macro.Env.define_alias(env, [line: 10], Foo.Bar, as: Baz)
      iex> Macro.Env.expand_alias(env, [], [:Baz])
      {:alias, Foo.Bar}
      iex> Macro.Env.expand_alias(env, [], [:Baz, :Bat])
      {:alias, Foo.Bar.Bat}

  If no `:as` option is given, the alias will be inferred from the module:

      iex> env = __ENV__
      iex> {:ok, env} = Macro.Env.define_alias(env, [line: 10], Foo.Bar)
      iex> Macro.Env.expand_alias(env, [], [:Bar])
      {:alias, Foo.Bar}

  If it is not possible to infer one, an error is returned:

      iex> Macro.Env.define_alias(__ENV__, [line: 10], :an_atom)
      {:error,
       "alias cannot be inferred automatically for module: :an_atom, " <>
         "please use the :as option. Implicit aliasing is only supported with Elixir modules"}

  """
  @doc since: "1.17.0"
  @spec define_alias(t, Macro.metadata(), module, keyword) :: {:ok, t} | {:error, String.t()}
  def define_alias(env, meta, module, opts \\ [])
      when is_list(meta) and is_atom(module) and is_list(opts) do
    {trace, opts} = Keyword.pop(opts, :trace, true)
    result = :elixir_aliases.alias(meta, module, true, opts, env, trace)
    maybe_define_error(result, :elixir_aliases)
  end

  defp maybe_define_error({:ok, env}, _mod),
    do: {:ok, env}

  defp maybe_define_error({:error, reason}, mod),
    do: {:error, Kernel.to_string(mod.format_error(reason))}

  @doc """
  Expands an alias given by the alias segments.

  It returns `{:alias, alias}` if the segments is a list
  of atoms and an alias was found. Returns `:error` otherwise.

  This expansion may emit the `:alias_expansion` trace event
  but it does not emit the `:alias_reference` one.

  ## Options

    * #{trace_option}

  ## Examples

      iex> alias List, as: MyList
      iex> Macro.Env.expand_alias(__ENV__, [], [:MyList])
      {:alias, List}
      iex> Macro.Env.expand_alias(__ENV__, [], [:MyList, :Nested])
      {:alias, List.Nested}

  If there is no alias or the alias starts with `Elixir.`
  (which disables aliasing), then `:error` is returned:

      iex> alias List, as: MyList
      iex> Macro.Env.expand_alias(__ENV__, [], [:Elixir, MyList])
      :error
      iex> Macro.Env.expand_alias(__ENV__, [], [:AnotherList])
      :error

  """
  @doc since: "1.17.0"
  @spec expand_alias(t, keyword, [atom()], keyword) ::
          {:alias, atom()} | :error
  def expand_alias(env, meta, list, opts \\ [])
      when is_list(meta) and is_list(list) and is_list(opts) do
    trace = Keyword.get(opts, :trace, true)

    case :elixir_aliases.expand(meta, list, env, trace) do
      atom when is_atom(atom) -> {:alias, atom}
      [_ | _] -> :error
    end
  end

  @doc """
  Expands an import given by `name` and `arity`.

  If the import points to a macro, it returns a tuple
  with the module and a function that expands the macro.
  The function expects the metadata to be attached to the
  expansion and the arguments of the macro.

  If the import points to a function, it returns a tuple
  with the module and the function name.

  If any import is found, the appropriate compiler tracing
  event will be emitted.

  Otherwise returns `{:error, reason}`.

  ## Options

    * `:allow_locals` - when set to `false`, it does not attempt to capture
      local macros defined in the current module in `env`

    * `:check_deprecations` - when set to `false`, does not check for deprecations
      when expanding macros

    * #{trace_option}

  """
  @doc since: "1.17.0"
  @spec expand_import(t, keyword, atom(), arity(), keyword) ::
          {:macro, module(), (Macro.metadata(), args :: [Macro.t()] -> Macro.t())}
          | {:function, module(), atom()}
          | {:error, :not_found | {:conflict, module()} | {:ambiguous, [module()]}}
  def expand_import(env, meta, name, arity, opts \\ [])
      when is_list(meta) and is_atom(name) and is_integer(arity) and is_list(opts) do
    case :elixir_import.special_form(name, arity) do
      true ->
        {:error, :not_found}

      false ->
        allow_locals = Keyword.get(opts, :allow_locals, true)
        trace = Keyword.get(opts, :trace, true)
        module = env.module

        extra =
          case allow_locals and function_exported?(module, :__info__, 1) do
            true -> [{module, module.__info__(:macros)}]
            false -> []
          end

        case :elixir_dispatch.expand_import(meta, name, arity, env, extra, allow_locals, trace) do
          {:macro, receiver, expander} ->
            {:macro, receiver, wrap_expansion(receiver, expander, meta, name, arity, env, opts)}

          {:function, receiver, name} ->
            {:function, receiver, name}

          error ->
            {:error, error}
        end
    end
  end

  @doc """
  Expands a require given by `module`, `name`, and `arity`.

  If the require points to a macro and the module has been
  required, it returns a tuple with the module and a function
  that expands the macro. The function expects the metadata
  to be attached to the expansion and the arguments of the macro.
  The appropriate `:remote_macro` compiler tracing event will
  be emitted if a macro is found (note a `:remote_function`
  event is not emitted in `:error` cases).

  Otherwise returns `:error`.

  ## Options

    * `:check_deprecations` - when set to `false`, does not check for deprecations
      when expanding macros

    * #{trace_option}

  """
  @doc since: "1.17.0"
  @spec expand_require(t, keyword, module(), atom(), arity(), keyword) ::
          {:macro, module(), (Macro.metadata(), args :: [Macro.t()] -> Macro.t())}
          | :error
  def expand_require(env, meta, module, name, arity, opts \\ [])
      when is_list(meta) and is_atom(module) and is_atom(name) and is_integer(arity) and
             is_list(opts) do
    trace = Keyword.get(opts, :trace, true)

    case :elixir_dispatch.expand_require(meta, module, name, arity, env, trace) do
      {:macro, receiver, expander} ->
        {:macro, receiver, wrap_expansion(receiver, expander, meta, name, arity, env, opts)}

      :error ->
        :error
    end
  end

  defp wrap_expansion(receiver, expander, meta, name, arity, env, opts) do
    fn expansion_meta, args ->
      if Keyword.get(opts, :check_deprecations, true) do
        :elixir_dispatch.check_deprecated(:macro, meta, receiver, name, arity, env)
      end

      quoted = expander.(args, env)
      next = :elixir_module.next_counter(env.module)
      :elixir_quote.linify_with_context_counter(expansion_meta, {receiver, next}, quoted)
    end
  end

  @doc """
  Returns an environment in the guard context.
  """
  @doc since: "1.17.0"
  @spec to_guard(t) :: t
  def to_guard(%{__struct__: Macro.Env} = env) do
    %{env | context: :guard}
  end

  @doc """
  Returns an environment in the match context.
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
