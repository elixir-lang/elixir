defmodule Code do
  @moduledoc ~S"""
  Utilities for managing code compilation, code evaluation, and code loading.

  This module complements Erlang's [`:code` module](`:code`)
  to add behavior which is specific to Elixir. For functions to
  manipulate Elixir's AST (rather than evaluating it), see the
  `Macro` module.

  ## Working with files

  This module contains three functions for compiling and evaluating files.
  Here is a summary of them and their behavior:

    * `require_file/2` - compiles a file and tracks its name. It does not
      compile the file again if it has been previously required.

    * `compile_file/2` - compiles a file without tracking its name. Compiles the
      file multiple times when invoked multiple times.

    * `eval_file/2` - evaluates the file contents without tracking its name. It
      returns the result of the last expression in the file, instead of the modules
      defined in it. Evaluated files do not trigger the compilation tracers described
      in the next section.

  In a nutshell, the first must be used when you want to keep track of the files
  handled by the system, to avoid the same file from being compiled multiple
  times. This is common in scripts.

  `compile_file/2` must be used when you are interested in the modules defined in a
  file, without tracking. `eval_file/2` should be used when you are interested in
  the result of evaluating the file rather than the modules it defines.

  The functions above work with Elixir source. If you want to work
  with modules compiled to bytecode, which have the `.beam` extension
  and are typically found below the _build directory of a Mix project,
  see the functions in Erlang's [`:code`](`:code`) module.

  ## Code loading on the Erlang VM

  Erlang has two modes to load code: interactive and embedded.

  By default, the Erlang VM runs in interactive mode, where modules
  are loaded as needed. In embedded mode the opposite happens, as all
  modules need to be loaded upfront or explicitly.

  You can use `ensure_loaded/1` (as well as `ensure_loaded?/1` and
  `ensure_loaded!/1`) to check if a module is loaded before using it and
  act.

  ## `ensure_compiled/1` and `ensure_compiled!/1`

  Elixir also includes `ensure_compiled/1` and `ensure_compiled!/1`
  functions that are a superset of `ensure_loaded/1`.

  Since Elixir's compilation happens in parallel, in some situations
  you may need to use a module that was not yet compiled, therefore
  it can't even be loaded.

  When invoked, `ensure_compiled/1` and `ensure_compiled!/1` halt the
  compilation of the caller until the module becomes available. Note that
  the distinction between `ensure_compiled/1` and `ensure_compiled!/1`
  is important: if you are using `ensure_compiled!/1`, you are
  indicating to the compiler that you can only continue if said module
  is available.

  If you are using `Code.ensure_compiled/1`, you are implying you may
  continue without the module and therefore Elixir may return
  `{:error, :unavailable}` for cases where the module is not yet available
  (but may be available later on).

  For those reasons, developers must typically use `Code.ensure_compiled!/1`.
  In particular, do not do this:

      case Code.ensure_compiled(module) do
        {:module, _} -> module
        {:error, _} -> raise ...
      end

  Finally, note you only need `ensure_compiled!/1` to check for modules
  being defined within the same project. It does not apply to modules from
  dependencies as dependencies are always compiled upfront.

  In most cases, `ensure_loaded/1` is enough. `ensure_compiled!/1`
  must be used in rare cases, usually involving macros that need to
  invoke a module for callback information. The use of `ensure_compiled/1`
  is even less likely.

  ## Compilation tracers

  Elixir supports compilation tracers, which allow modules to observe constructs
  handled by the Elixir compiler when compiling files. A tracer is a module
  that implements the `trace/2` function. The function receives the event name
  as first argument and `Macro.Env` as second and it must return `:ok`. It is
  very important for a tracer to do as little work as possible synchronously
  and dispatch the bulk of the work to a separate process. **Slow tracers will
  slow down compilation**.

  You can configure your list of tracers via `put_compiler_option/2`. The
  following events are available to tracers:

    * `:start` - (since v1.11.0) invoked whenever the compiler starts to trace
      a new lexical context. A lexical context is started when compiling a new
      file or when defining a module within a function. Note evaluated code
      does not start a new lexical context (because they don't track unused
      aliases, imports, etc) but defining a module inside evaluated code will.

      Note this event may be emitted in parallel, where multiple files/modules
      invoke `:start` and run at the same time. The value of the `lexical_tracker`
      of the macro environment, albeit opaque, can be used to uniquely identify
      the environment.

    * `:stop` - (since v1.11.0) invoked whenever the compiler stops tracing a
      new lexical context, such as a new file.

    * `{:import, meta, module, opts}` - traced whenever `module` is imported.
      `meta` is the import AST metadata and `opts` are the import options.

    * `{:imported_function, meta, module, name, arity}` and
      `{:imported_macro, meta, module, name, arity}` - traced whenever an
      imported function or macro is invoked. `meta` is the call AST metadata,
      `module` is the module the import is from, followed by the `name` and `arity`
      of the imported function/macro. A :remote_function/:remote_macro event
      may still be emitted for the imported module/name/arity.

    * `{:imported_quoted, meta, module, name, [arity]}` - traced whenever an
      imported function or macro is processed inside a `quote/2`. `meta` is the
      call AST metadata, `module` is the module the import is from, followed by
      the `name` and a list of `arities` of the imported function/macro.

    * `{:alias, meta, alias, as, opts}` - traced whenever `alias` is aliased
      to `as`. `meta` is the alias AST metadata and `opts` are the alias options.

    * `{:alias_expansion, meta, as, alias}` traced whenever there is an alias
      expansion for a previously defined `alias`, i.e. when the user writes `as`
      which is expanded to `alias`. `meta` is the alias expansion AST metadata.

    * `{:alias_reference, meta, module}` - traced whenever there is an alias
      in the code, i.e. whenever the user writes `MyModule.Foo.Bar` in the code,
      regardless if it was expanded or not.

    * `{:require, meta, module, opts}` - traced whenever `module` is required.
      `meta` is the require AST metadata and `opts` are the require options.
      If the `meta` option contains the `:from_macro`, then module was called
      from within a macro and therefore must be treated as a compile-time dependency.

    * `{:struct_expansion, meta, module, keys}` - traced whenever `module`'s struct
      is expanded. `meta` is the struct AST metadata and `keys` are the keys being
      used by expansion

    * `{:remote_function, meta, module, name, arity}` and
      `{:remote_macro, meta, module, name, arity}` - traced whenever a remote
      function or macro is referenced. `meta` is the call AST metadata, `module`
      is the invoked module, followed by the `name` and `arity`.

    * `{:local_function, meta, name, arity}` and
      `{:local_macro, meta, name, arity}` - traced whenever a local
      function or macro is referenced. `meta` is the call AST metadata, followed by
      the `name` and `arity`.

    * `{:compile_env, app, path, return}` - traced whenever `Application.compile_env/3`
      or `Application.compile_env!/2` are called. `app` is an atom, `path` is a list
      of keys to traverse in the application environment and `return` is either
      `{:ok, value}` or `:error`.

    * `:defmodule` - (since v1.16.2) traced as soon as the definition of a module
      starts. This is invoked early on in the module life cycle, `Module.open?/1`
      still returns `false` for such traces

    * `{:on_module, bytecode, _ignore}` - (since v1.13.0) traced whenever a module
      is defined. This is equivalent to the `@after_compile` callback and invoked
      after any `@after_compile` in the given module. The third element is currently
      `:none` but it may provide more metadata in the future. It is best to ignore
      it at the moment. Note that `Module` functions expecting not yet compiled modules
      (such as `Module.definitions_in/1`) are still available at the time this event
      is emitted.

  The `:tracers` compiler option can be combined with the `:parser_options`
  compiler option to enrich the metadata of the traced events above.

  New events may be added at any time in the future, therefore it is advised
  for the `trace/2` function to have a "catch-all" clause.

  Below is an example tracer that prints all remote function invocations:

      defmodule MyTracer do
        def trace({:remote_function, _meta, module, name, arity}, env) do
          IO.puts("#{env.file}:#{env.line} #{inspect(module)}.#{name}/#{arity}")
          :ok
        end

        def trace(_event, _env) do
          :ok
        end
      end
  """

  @typedoc """
  A list with all variables and their values.

  The binding keys are usually atoms, but they may be a tuple for variables
  defined in a different context.
  """
  @type binding :: [{atom() | tuple(), any}]

  @typedoc """
  Diagnostics returned by the compiler and code evaluation.

  The file and position relate to where the diagnostic should be shown.
  If there is a file and position, then the diagnostic is precise
  and you can use the given file and position for generating snippets,
  IDEs annotations, and so on. An optional span is available with
  the line and column the diagnostic ends.

  Otherwise, a stacktrace may be given, which you can place your own
  heuristics to provide better reporting.

  The source field points to the source file the compiler tracked
  the error to. For example, a file `lib/foo.ex` may embed `.eex`
  templates from `lib/foo/bar.eex`. A syntax error on the EEx template
  will point to file `lib/foo/bar.eex` but the source is `lib/foo.ex`.
  """
  @type diagnostic(severity) :: %{
          required(:source) => Path.t() | nil,
          required(:file) => Path.t() | nil,
          required(:severity) => severity,
          required(:message) => String.t(),
          required(:position) => position(),
          required(:stacktrace) => Exception.stacktrace(),
          required(:span) => {line :: pos_integer(), column :: pos_integer()} | nil,
          optional(:details) => term(),
          optional(any()) => any()
        }

  @typedoc "The line. 0 indicates no line."
  @type line() :: non_neg_integer()

  @typedoc """
  The position of the diagnostic.

  Can be either a line number or a `{line, column}`.
  Line and columns numbers are one-based.
  A position of `0` represents unknown.
  """
  @type position() :: line() | {line :: pos_integer(), column :: pos_integer()}

  @boolean_compiler_options [
    :docs,
    :debug_info,
    :ignore_already_consolidated,
    :ignore_module_conflict,
    :relative_paths,
    :warnings_as_errors
  ]

  @list_compiler_options [:no_warn_undefined, :tracers, :parser_options]

  @available_compiler_options @boolean_compiler_options ++
                                @list_compiler_options ++ [:on_undefined_variable]

  @doc """
  Lists all required files.

  ## Examples

      Code.require_file("../eex/test/eex_test.exs")
      List.first(Code.required_files()) =~ "eex_test.exs"
      #=> true

  """
  @doc since: "1.7.0"
  @spec required_files() :: [binary]
  def required_files do
    :elixir_code_server.call(:required)
  end

  @deprecated "Use Code.required_files/0 instead"
  @doc false
  def loaded_files do
    required_files()
  end

  @doc false
  @deprecated "Use Code.Fragment.cursor_context/2 instead"
  def cursor_context(code, options \\ []) do
    Code.Fragment.cursor_context(code, options)
  end

  @doc """
  Removes files from the required files list.

  The modules defined in the file are not removed;
  calling this function only removes them from the list,
  allowing them to be required again.

  The list of files is managed per Erlang VM node.

  ## Examples

      # Require EEx test code
      Code.require_file("../eex/test/eex_test.exs")

      # Now unrequire all files
      Code.unrequire_files(Code.required_files())

      # Note that modules are still available
      function_exported?(EExTest.Compiled, :before_compile, 0)
      #=> true

  """
  @doc since: "1.7.0"
  @spec unrequire_files([binary]) :: :ok
  def unrequire_files(files) when is_list(files) do
    :elixir_code_server.cast({:unrequire_files, files})
  end

  @deprecated "Use Code.unrequire_files/1 instead"
  @doc false
  def unload_files(files) do
    unrequire_files(files)
  end

  @doc """
  Appends a path to the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code. The list of files is managed per Erlang
  VM node.

  The path is expanded with `Path.expand/1` before being appended.
  It requires the path to exist. Returns a boolean indicating if
  the path was successfully added.

  ## Examples

      Code.append_path(".")
      #=> true

      Code.append_path("/does_not_exist")
      #=> false

  ## Options

    * `:cache` - (since v1.15.0) when true, the code path is cached
      the first time it is traversed in order to reduce file system
      operations. It requires Erlang/OTP 26, otherwise it is a no-op.

  """
  @spec append_path(Path.t(), cache: boolean()) :: true | false
  def append_path(path, opts \\ []) do
    apply(:code, :add_pathz, [to_charlist(Path.expand(path)) | cache(opts)]) == true
  end

  @doc """
  Prepends a path to the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code. The list of files is managed per Erlang
  VM node.

  The path is expanded with `Path.expand/1` before being prepended.
  It requires the path to exist. Returns a boolean indicating if
  the path was successfully added.

  ## Examples

      Code.prepend_path(".")
      #=> true

      Code.prepend_path("/does_not_exist")
      #=> false

  ## Options

    * `:cache` - (since v1.15.0) when true, the code path is cached
      the first time it is traversed in order to reduce file system
      operations. It requires Erlang/OTP 26, otherwise it is a no-op.

  """
  @spec prepend_path(Path.t(), cache: boolean()) :: boolean()
  def prepend_path(path, opts \\ []) do
    apply(:code, :add_patha, [to_charlist(Path.expand(path)) | cache(opts)]) == true
  end

  @doc """
  Prepends a list of `paths` to the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code. The list of files is managed per Erlang
  VM node.

  All paths are expanded with `Path.expand/1` before being prepended.
  Only existing paths are prepended. This function always returns `:ok`,
  regardless of how many paths were prepended. Use `prepend_path/1`
  if you need more control.

  ## Examples

      Code.prepend_paths([".", "/does_not_exist"])
      #=> :ok

  ## Options

    * `:cache` - when true, the code path is cached the first time
      it is traversed in order to reduce file system operations.
      It requires Erlang/OTP 26, otherwise it is a no-op.
  """
  @doc since: "1.15.0"
  @spec prepend_paths([Path.t()], cache: boolean()) :: :ok
  def prepend_paths(paths, opts \\ []) when is_list(paths) do
    apply(:code, :add_pathsa, [Enum.map(paths, &to_charlist(Path.expand(&1))) | cache(opts)])
  end

  @doc """
  Appends a list of `paths` to the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code. The list of files is managed per Erlang
  VM node.

  All paths are expanded with `Path.expand/1` before being appended.
  Only existing paths are appended. This function always returns `:ok`,
  regardless of how many paths were appended. Use `append_path/1`
  if you need more control.

  ## Examples

      Code.append_paths([".", "/does_not_exist"])
      #=> :ok

  ## Options

    * `:cache` - when true, the code path is cached the first time
      it is traversed in order to reduce file system operations.
      It requires Erlang/OTP 26, otherwise it is a no-op.
  """
  @doc since: "1.15.0"
  @spec append_paths([Path.t()], cache: boolean()) :: :ok
  def append_paths(paths, opts \\ []) when is_list(paths) do
    apply(:code, :add_pathsz, [Enum.map(paths, &to_charlist(Path.expand(&1))) | cache(opts)])
  end

  defp cache(opts) do
    if function_exported?(:code, :add_path, 2) do
      if opts[:cache], do: [:cache], else: [:nocache]
    else
      []
    end
  end

  @doc """
  Deletes a path from the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for finding
  module code. The list of files is managed per Erlang VM node.

  The path is expanded with `Path.expand/1` before being deleted. If the
  path does not exist, this function returns `false`.

  ## Examples

      Code.prepend_path(".")
      Code.delete_path(".")
      #=> true

      Code.delete_path("/does_not_exist")
      #=> false

  """
  @spec delete_path(Path.t()) :: boolean
  def delete_path(path) do
    case :code.del_path(to_charlist(Path.expand(path))) do
      result when is_boolean(result) ->
        result

      {:error, :bad_name} ->
        raise ArgumentError,
              "invalid argument #{inspect(path)}"
    end
  end

  @doc """
  Deletes a list of paths from the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for finding
  module code. The list of files is managed per Erlang VM node.

  The path is expanded with `Path.expand/1` before being deleted. If the
  path does not exist, this function returns `false`.
  """
  @doc since: "1.15.0"
  @spec delete_paths([Path.t()]) :: :ok
  def delete_paths(paths) when is_list(paths) do
    for path <- paths do
      _ = :code.del_path(to_charlist(Path.expand(path)))
    end

    :ok
  end

  @doc """
  Evaluates the contents given by `string`.

  The `binding` argument is a list of all variables and their values.
  The `opts` argument is a keyword list of environment options.

  **Warning**: `string` can be any Elixir code and will be executed with
  the same privileges as the Erlang VM: this means that such code could
  compromise the machine (for example by executing system commands).
  Don't use `eval_string/3` with untrusted input (such as strings coming
  from the network).

  ## Options

  It accepts the same options as `env_for_eval/1`. Additionally, you may
  also pass an environment as second argument, so the evaluation happens
  within that environment.

  Returns a tuple of the form `{value, binding}`, where `value` is the value
  returned from evaluating `string`. If an error occurs while evaluating
  `string`, an exception will be raised.

  `binding` is a list with all variable names and their values after evaluating
  `string`. The binding keys are usually atoms, but they may be a tuple for variables
  defined in a different context. The names are in no particular order.

  ## Examples

      iex> {result, binding} = Code.eval_string("a + b", [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      iex> result
      3
      iex> Enum.sort(binding)
      [a: 1, b: 2]

      iex> {result, binding} = Code.eval_string("c = a + b", [a: 1, b: 2], __ENV__)
      iex> result
      3
      iex> Enum.sort(binding)
      [a: 1, b: 2, c: 3]

      iex> {result, binding} = Code.eval_string("a = a + b", [a: 1, b: 2])
      iex> result
      3
      iex> Enum.sort(binding)
      [a: 3, b: 2]

  For convenience, you can pass `__ENV__/0` as the `opts` argument and
  all imports, requires and aliases defined in the current environment
  will be automatically carried over:

      iex> {result, binding} = Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
      iex> result
      3
      iex> Enum.sort(binding)
      [a: 1, b: 2]

  """
  @spec eval_string(List.Chars.t(), binding, Macro.Env.t() | keyword) :: {term, binding}
  def eval_string(string, binding \\ [], opts \\ [])

  def eval_string(string, binding, %Macro.Env{} = env) do
    validated_eval_string(string, binding, env)
  end

  def eval_string(string, binding, opts) when is_list(opts) do
    validated_eval_string(string, binding, opts)
  end

  defp validated_eval_string(string, binding, opts_or_env) do
    %{line: line, file: file} = env = env_for_eval(opts_or_env)
    forms = :elixir.string_to_quoted!(to_charlist(string), line, 1, file, [])
    {value, binding, _env} = eval_verify(:eval_forms, [forms, binding, env])
    {value, binding}
  end

  defp eval_verify(fun, args) do
    Module.ParallelChecker.verify(fn ->
      apply(:elixir, fun, args)
    end)
  end

  @doc """
  Executes the given `fun` and capture all diagnostics.

  Diagnostics are warnings and errors emitted during code
  evaluation or single-file compilation and by functions
  such as `IO.warn/2`.

  If using `mix compile` or `Kernel.ParallelCompiler`,
  note they already capture and return diagnostics.

  ## Options

    * `:log` - if the diagnostics should be logged as they happen.
      Defaults to `false`.

  > #### Rescuing errors {: .info}
  >
  > `with_diagnostics/2` does not automatically handle exceptions.
  > You may capture them by adding a `try/1` in `fun`:
  >
  >     {result, all_errors_and_warnings} =
  >       Code.with_diagnostics(fn ->
  >         try do
  >           {:ok, Code.compile_quoted(quoted)}
  >         rescue
  >           err -> {:error, err}
  >         end
  >       end)

  """
  @doc since: "1.15.0"
  @spec with_diagnostics(keyword(), (-> result)) :: {result, [diagnostic(:warning | :error)]}
        when result: term()
  def with_diagnostics(opts \\ [], fun) do
    value = :erlang.get(:elixir_code_diagnostics)
    log = Keyword.get(opts, :log, false)
    :erlang.put(:elixir_code_diagnostics, {[], log})

    try do
      result = fun.()
      {diagnostics, _log?} = :erlang.get(:elixir_code_diagnostics)
      {result, Enum.reverse(diagnostics)}
    after
      if value == :undefined do
        :erlang.erase(:elixir_code_diagnostics)
      else
        :erlang.put(:elixir_code_diagnostics, value)
      end
    end
  end

  @doc """
  Prints a diagnostic into the standard error.

  A diagnostic is either returned by `Kernel.ParallelCompiler`
  or by `Code.with_diagnostics/2`.

  ## Options

    * `:snippet` - whether to read the code snippet in the diagnostic location.
      As it may impact performance, it is not recommended to be used in runtime.
      Defaults to `true`.
  """
  @doc since: "1.15.0"
  @spec print_diagnostic(diagnostic(:warning | :error), keyword()) :: :ok
  def print_diagnostic(diagnostic, opts \\ []) do
    read_snippet? = Keyword.get(opts, :snippet, true)
    :elixir_errors.print_diagnostic(diagnostic, read_snippet?)
    :ok
  end

  @doc ~S"""
  Formats the given code `string`.

  The formatter receives a string representing Elixir code and
  returns iodata representing the formatted code according to
  pre-defined rules.

  ## Options

  Regular options (do not change the AST):

    * `:file` - the file which contains the string, used for error
      reporting

    * `:line` - the line the string starts, used for error reporting

    * `:line_length` - the line length to aim for when formatting
      the document. Defaults to 98. This value indicates when an expression
      should be broken over multiple lines but it is not guaranteed
      to do so. See the "Line length" section below for more information

    * `:locals_without_parens` - a keyword list of name and arity
      pairs that should be kept without parens whenever possible.
      The arity may be the atom `:*`, which implies all arities of
      that name. The formatter already includes a list of functions
      and this option augments this list.

    * `:force_do_end_blocks` (since v1.9.0) - when `true`, converts all
      inline usages of `do: ...`,  `else: ...` and friends into `do`-`end`
      blocks. Defaults to `false`. Note that this option is convergent:
      once you set it to `true`, **all keywords** will be converted.
      If you set it to `false` later on, `do`-`end` blocks won't be
      converted back to keywords.

  Migration options (change the AST), see the "Migration formatting" section below:

    * `:migrate` (since v1.18.0) - when `true`, sets all other migration options
      to `true` by default. Defaults to `false`.

    * `:migrate_bitstring_modifiers` (since v1.18.0) - when `true`,
      removes unnecessary parentheses in known bitstring
      [modifiers](`<<>>/1`), for example `<<foo::binary()>>`
      becomes `<<foo::binary>>`, or adds parentheses for custom
      modifiers, where `<<foo::custom_type>>` becomes `<<foo::custom_type()>>`.
      Defaults to the value of the `:migrate` option. This option changes the AST.

    * `:migrate_charlists_as_sigils` (since v1.18.0) - when `true`,
      formats charlists as [`~c`](`Kernel.sigil_c/2`) sigils, for example
      `'foo'` becomes `~c"foo"`.
      Defaults to the value of the `:migrate` option. This option changes the AST.

    * `:migrate_unless` (since v1.18.0) - when `true`,
      rewrites `unless` expressions using `if` with a negated condition, for example
      `unless foo, do:` becomes `if !foo, do:`.
      Defaults to the value of the `:migrate` option. This option changes the AST.

  ## Design principles

  The formatter was designed under three principles.

  First, the formatter never changes the semantics of the code by default.
  This means the input AST and the output AST are almost always equivalent.

  The second principle is to provide as little configuration as possible.
  This eases the formatter adoption by removing contention points while
  making sure a single style is followed consistently by the community as
  a whole.

  The formatter does not hard code names. The formatter will not behave
  specially because a function is named `defmodule`, `def`, or the like. This
  principle mirrors Elixir's goal of being an extensible language where
  developers can extend the language with new constructs as if they were
  part of the language. When it is absolutely necessary to change behavior
  based on the name, this behavior should be configurable, such as the
  `:locals_without_parens` option.

  ## Running the formatter

  The formatter attempts to fit the most it can on a single line and
  introduces line breaks wherever possible when it cannot.

  In some cases, this may lead to undesired formatting. Therefore, **some
  code generated by the formatter may not be aesthetically pleasing and
  may require explicit intervention from the developer**. That's why we
  do not recommend to run the formatter blindly in an existing codebase.
  Instead you should format and sanity check each formatted file.

  For example, the formatter may break a long function definition over
  multiple clauses:

      def my_function(
        %User{name: name, age: age, ...},
        arg1,
        arg2
      ) do
        ...
      end

  While the code above is completely valid, you may prefer to match on
  the struct variables inside the function body in order to keep the
  definition on a single line:

      def my_function(%User{} = user, arg1, arg2) do
        %{name: name, age: age, ...} = user
        ...
      end

  In some situations, you can use the fact the formatter does not generate
  elegant code as a hint for refactoring. Take this code:

      def board?(board_id, %User{} = user, available_permissions, required_permissions) do
        Tracker.OrganizationMembers.user_in_organization?(user.id, board.organization_id) and
          required_permissions == Enum.to_list(MapSet.intersection(MapSet.new(required_permissions), MapSet.new(available_permissions)))
      end

  The code above has very long lines and running the formatter is not going
  to address this issue. In fact, the formatter may make it more obvious that
  you have complex expressions:

      def board?(board_id, %User{} = user, available_permissions, required_permissions) do
        Tracker.OrganizationMembers.user_in_organization?(user.id, board.organization_id) and
          required_permissions ==
            Enum.to_list(
              MapSet.intersection(
                MapSet.new(required_permissions),
                MapSet.new(available_permissions)
              )
            )
      end

  Take such cases as a suggestion that your code should be refactored:

      def board?(board_id, %User{} = user, available_permissions, required_permissions) do
        Tracker.OrganizationMembers.user_in_organization?(user.id, board.organization_id) and
          matching_permissions?(required_permissions, available_permissions)
      end

      defp matching_permissions?(required_permissions, available_permissions) do
        intersection =
          required_permissions
          |> MapSet.new()
          |> MapSet.intersection(MapSet.new(available_permissions))
          |> Enum.to_list()

        required_permissions == intersection
      end

  To sum it up: since the formatter cannot change the semantics of your
  code, sometimes it is necessary to tweak or refactor the code to get
  optimal formatting. To help better understand how to control the formatter,
  we describe in the next sections the cases where the formatter keeps the
  user encoding and how to control multiline expressions.

  ## Line length

  Another point about the formatter is that the `:line_length` configuration
  indicates when an expression should be broken over multiple lines but it is
  not guaranteed to do so. In many cases, it is not possible for the formatter
  to break your code apart, which means it will go over the line length.
  For example, if you have a long string:

      "this is a very long string that will go over the line length"

  The formatter doesn't know how to break it apart without changing the
  code underlying syntax representation, so it is up to you to step in:

      "this is a very long string " <>
         "that will go over the line length"

  The string concatenation makes the code fit on a single line and also
  gives more options to the formatter.

  This may also appear in keywords such as do/end blocks and operators,
  where the `do` keyword may go over the line length because there is no
  opportunity for the formatter to introduce a line break in a readable way.
  For example, if you do:

      case very_long_expression() do
      end

  And only the `do` keyword is beyond the line length, Elixir **will not**
  emit this:

      case very_long_expression()
      do
      end

  So it prefers to not touch the line at all and leave `do` above the
  line limit.

  ## Keeping user's formatting

  The formatter respects the input format in some cases. Those are
  listed below:

    * Insignificant digits in numbers are kept as is. The formatter,
      however, always inserts underscores for decimal numbers with more
      than 5 digits and converts hexadecimal digits to uppercase

    * Strings, charlists, atoms and sigils are kept as is. No character
      is automatically escaped or unescaped. The choice of delimiter is
      also respected from the input

    * Newlines inside blocks are kept as in the input except for:
      1) expressions that take multiple lines will always have an empty
      line before and after and 2) empty lines are always squeezed
      together into a single empty line

    * The choice between `:do` keyword and `do`-`end` blocks is left
      to the user

    * Lists, tuples, bitstrings, maps, structs and function calls will be
      broken into multiple lines if they are followed by a newline in the
      opening bracket and preceded by a new line in the closing bracket

    * Newlines before certain operators (such as the pipeline operators)
      and before other operators (such as comparison operators)

  The behaviors above are not guaranteed. We may remove or add new
  rules in the future. The goal of documenting them is to provide better
  understanding on what to expect from the formatter.

  ### Multi-line lists, maps, tuples, and the like

  You can force lists, tuples, bitstrings, maps, structs and function
  calls to have one entry per line by adding a newline after the opening
  bracket and a new line before the closing bracket lines. For example:

      [
        foo,
        bar
      ]

  If there are no newlines around the brackets, then the formatter will
  try to fit everything on a single line, such that the snippet below

      [foo,
       bar]

  will be formatted as

      [foo, bar]

  You can also force function calls and keywords to be rendered on multiple
  lines by having each entry on its own line:

      defstruct name: nil,
                age: 0

  The code above will be kept with one keyword entry per line by the
  formatter. To avoid that, just squash everything into a single line.

  ### Parens and no parens in function calls

  Elixir has two syntaxes for function calls. With parens and no parens.
  By default, Elixir will add parens to all calls except for:

    1. calls that have `do`-`end` blocks
    2. local calls without parens where the name and arity of the local
       call is also listed under `:locals_without_parens` (except for
       calls with arity 0, where the compiler always require parens)

  The choice of parens and no parens also affects indentation. When a
  function call with parens doesn't fit on the same line, the formatter
  introduces a newline around parens and indents the arguments with two
  spaces:

      some_call(
        arg1,
        arg2,
        arg3
      )

  On the other hand, function calls without parens are always indented
  by the function call length itself, like this:

      some_call arg1,
                arg2,
                arg3

  If the last argument is a data structure, such as maps and lists, and
  the beginning of the data structure fits on the same line as the function
  call, then no indentation happens, this allows code like this:

      Enum.reduce(some_collection, initial_value, fn element, acc ->
        # code
      end)

      some_function_without_parens %{
        foo: :bar,
        baz: :bat
      }

  ## Code comments

  The formatter handles code comments and guarantees a space is always added
  between the beginning of the comment (#) and the next character.

  The formatter also extracts all trailing comments to their previous line.
  For example, the code below

      hello #world

  will be rewritten to

      # world
      hello

  While the formatter attempts to preserve comments in most situations,
  that's not always possible, because code comments are handled apart from
  the code representation (AST). While the formatter can preserve code
  comments between expressions and function arguments, the formatter
  cannot currently preserve them around operators. For example, the following
  code will move the code comments to before the operator usage:

      foo() ||
        # also check for bar
        bar()

  In some situations, code comments can be seen as ambiguous by the formatter.
  For example, the comment in the anonymous function below

      fn
        arg1 ->
          body1
          # comment

        arg2 ->
          body2
      end

  and in this one

      fn
        arg1 ->
          body1

        # comment
        arg2 ->
          body2
      end

  are considered equivalent (the nesting is discarded alongside most of
  user formatting). In such cases, the code formatter will always format to
  the latter.

  ## Newlines

  The formatter converts all newlines in code from `\r\n` to `\n`.

  ## Migration formatting

  As part of the Elixir release cycle, deprecations are being introduced,
  emitting warnings which might require existing code to be changed.
  In order to reduce the burden on developers when upgrading Elixir to the
  next version, the formatter exposes some options, disabled by default,
  in order to automate this process.

  These options should address most of the typical use cases, but given they
  introduce changes to the AST, there is a non-zero risk for meta-programming
  heavy projects that relied on a specific AST, or projects that are
  re-defining functions from the `Kernel`. In such cases, migrations cannot
  be applied blindly and some extra changes might be needed in order to
  address the deprecation warnings.
  """
  @doc since: "1.6.0"
  @spec format_string!(binary, keyword) :: iodata
  def format_string!(string, opts \\ []) when is_binary(string) and is_list(opts) do
    line_length = Keyword.get(opts, :line_length, 98)

    to_quoted_opts =
      [
        unescape: false,
        literal_encoder: &{:ok, {:__block__, &2, [&1]}},
        token_metadata: true,
        emit_warnings: false
      ] ++ opts

    {forms, comments} = string_to_quoted_with_comments!(string, to_quoted_opts)
    to_algebra_opts = [comments: comments] ++ opts
    doc = Code.Formatter.to_algebra(forms, to_algebra_opts)
    Inspect.Algebra.format(doc, line_length)
  end

  @doc """
  Formats a file.

  See `format_string!/2` for more information on code formatting and
  available options.
  """
  @doc since: "1.6.0"
  @spec format_file!(binary, keyword) :: iodata
  def format_file!(file, opts \\ []) when is_binary(file) and is_list(opts) do
    string = File.read!(file)
    formatted = format_string!(string, [file: file, line: 1] ++ opts)
    [formatted, ?\n]
  end

  @doc """
  Evaluates the quoted contents.

  **Warning**: Calling this function inside a macro is considered bad
  practice as it will attempt to evaluate runtime values at compile time.
  Macro arguments are typically transformed by unquoting them into the
  returned quoted expressions (instead of evaluated).

  See `eval_string/3` for a description of `binding` and `opts`.

  ## Examples

      iex> contents = quote(do: var!(a) + var!(b))
      iex> {result, binding} = Code.eval_quoted(contents, [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      iex> result
      3
      iex> Enum.sort(binding)
      [a: 1, b: 2]

  For convenience, you can pass `__ENV__/0` as the `opts` argument and
  all options will be automatically extracted from the current environment:

      iex> contents = quote(do: var!(a) + var!(b))
      iex> {result, binding} = Code.eval_quoted(contents, [a: 1, b: 2], __ENV__)
      iex> result
      3
      iex> Enum.sort(binding)
      [a: 1, b: 2]

  """
  @spec eval_quoted(Macro.t(), binding, Macro.Env.t() | keyword) :: {term, binding}
  def eval_quoted(quoted, binding \\ [], env_or_opts \\ []) do
    {value, binding, _env} =
      eval_verify(:eval_quoted, [quoted, binding, env_for_eval(env_or_opts)])

    {value, binding}
  end

  @doc """
  Returns an environment for evaluation.

  It accepts either a `Macro.Env`, that is then pruned and prepared,
  or a list of options. It returns an environment that is ready for
  evaluation.

  Most functions in this module will automatically prepare the given
  environment for evaluation, so you don't need to explicitly call
  this function, with the exception of `eval_quoted_with_env/3`,
  which was designed precisely to be called in a loop, to implement
  features such as interactive shells or anything else with multiple
  evaluations.

  ## Options

  If an env is not given, the options can be:

    * `:file` - the file to be considered in the evaluation

    * `:line` - the line on which the script starts

    * `:module` - the module to run the environment on
  """
  @doc since: "1.14.0"
  def env_for_eval(env_or_opts), do: :elixir.env_for_eval(env_or_opts)

  @doc """
  Evaluates the given `quoted` contents with `binding` and `env`.

  This function is meant to be called in a loop, to implement features
  such as interactive shells or anything else with multiple evaluations.
  Therefore, the first time you call this function, you must compute
  the initial environment with `env_for_eval/1`. The remaining calls
  must pass the environment that was returned by this function.

  ## Options

    * `:prune_binding` - (since v1.14.2) prune binding to keep only
      variables read or written by the evaluated code. Note that
      variables used by modules are always pruned, even if later used
      by the modules. You can submit to the `:on_module` tracer event
      and access the variables used by the module from its environment.

  """
  @doc since: "1.14.0"
  @spec eval_quoted_with_env(Macro.t(), binding, Macro.Env.t(), keyword) ::
          {term, binding, Macro.Env.t()}
  def eval_quoted_with_env(quoted, binding, %Macro.Env{} = env, opts \\ [])
      when is_list(binding) do
    eval_verify(:eval_quoted, [quoted, binding, env, opts])
  end

  @doc ~S"""
  Converts the given string to its quoted form.

  Returns `{:ok, quoted_form}` if it succeeds,
  `{:error, {meta, message_info, token}}` otherwise.

  ## Options

    * `:file` - the filename to be reported in case of parsing errors.
      Defaults to `"nofile"`.

    * `:line` - the starting line of the string being parsed.
      Defaults to 1.

    * `:column` - (since v1.11.0) the starting column of the string being parsed.
      Defaults to 1.

    * `:columns` - when `true`, attach a `:column` key to the quoted
      metadata. Defaults to `false`.

    * `:unescape` (since v1.10.0) - when `false`, preserves escaped sequences.
      For example, `"null byte\\t\\x00"` will be kept as is instead of being
      converted to a bitstring literal. Note if you set this option to false, the
      resulting AST is no longer valid, but it can be useful to analyze/transform
      source code, typically in combination with `quoted_to_algebra/2`.
      Defaults to `true`.

    * `:existing_atoms_only` - when `true`, raises an error
      when non-existing atoms are found by the tokenizer.
      Defaults to `false`.

    * `:token_metadata` (since v1.10.0) - when `true`, includes token-related
      metadata in the expression AST, such as metadata for `do` and `end`
      tokens, for closing tokens, end of expressions, as well as delimiters
      for sigils. See `t:Macro.metadata/0`. Defaults to `false`.

    * `:literal_encoder` (since v1.10.0) - how to encode literals in the AST.
      It must be a function that receives two arguments, the literal and its
      metadata, and it must return `{:ok, ast :: Macro.t}` or
      `{:error, reason :: binary}`. If you return anything than the literal
      itself as the `term`, then the AST is no longer valid. This option
      may still useful for textual analysis of the source code.

    * `:static_atoms_encoder` - the static atom encoder function, see
      "The `:static_atoms_encoder` function" section below. Note this
      option overrides the `:existing_atoms_only` behavior for static
      atoms but `:existing_atoms_only` is still used for dynamic atoms,
      such as atoms with interpolations.

    * `:emit_warnings` (since v1.16.0) - when `false`, does not emit
      tokenizing/parsing related warnings. Defaults to `true`.

  ## `Macro.to_string/2`

  The opposite of converting a string to its quoted form is
  `Macro.to_string/2`, which converts a quoted form to a string/binary
  representation.

  ## The `:static_atoms_encoder` function

  When `static_atoms_encoder: &my_encoder/2` is passed as an argument,
  `my_encoder/2` is called every time the tokenizer needs to create a
  "static" atom. Static atoms are atoms in the AST that function as
  aliases, remote calls, local calls, variable names, regular atoms
  and keyword lists.

  The encoder function will receive the atom name (as a binary) and a
  keyword list with the current file, line and column. It must return
  `{:ok, token :: term} | {:error, reason :: binary}`.

  The encoder function is supposed to create an atom from the given
  string. To produce a valid AST, it is required to return `{:ok, term}`,
  where `term` is an atom. It is possible to return something other than an atom,
  however, in that case the AST is no longer "valid" in that it cannot
  be used to compile or evaluate Elixir code. A use case for this is
  if you want to use the Elixir parser in a user-facing situation, but
  you don't want to exhaust the atom table.

  The atom encoder is not called for *all* atoms that are present in
  the AST. It won't be invoked for the following atoms:

    * operators (`:+`, `:-`, and so on)

    * syntax keywords (`fn`, `do`, `else`, and so on)

    * atoms containing interpolation (`:"#{1 + 1} is two"`), as these
      atoms are constructed at runtime

    * atoms used to represent single-letter sigils like `:sigil_X`
      (but multi-letter sigils like `:sigil_XYZ` are encoded).

  """
  @spec string_to_quoted(List.Chars.t(), keyword) ::
          {:ok, Macro.t()} | {:error, {location :: keyword, binary | {binary, binary}, binary}}
  def string_to_quoted(string, opts \\ []) when is_list(opts) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    column = Keyword.get(opts, :column, 1)

    case :elixir.string_to_tokens(to_charlist(string), line, column, file, opts) do
      {:ok, tokens} ->
        :elixir.tokens_to_quoted(tokens, file, opts)

      {:error, _error_msg} = error ->
        error
    end
  end

  @doc """
  Converts the given string to its quoted form.

  It returns the AST if it succeeds,
  raises an exception otherwise. The exception is a `TokenMissingError`
  in case a token is missing (usually because the expression is incomplete),
  `MismatchedDelimiterError` (in case of mismatched opening and closing delimiters) and
  `SyntaxError` otherwise.

  Check `string_to_quoted/2` for options information.
  """
  @spec string_to_quoted!(List.Chars.t(), keyword) :: Macro.t()
  def string_to_quoted!(string, opts \\ []) when is_list(opts) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    column = Keyword.get(opts, :column, 1)
    :elixir.string_to_quoted!(to_charlist(string), line, column, file, opts)
  end

  @doc """
  Converts the given string to its quoted form and a list of comments.

  This function is useful when performing textual changes to the source code,
  while preserving information like comments and literals position.

  Returns `{:ok, quoted_form, comments}` if it succeeds,
  `{:error, {line, error, token}}` otherwise.

  Comments are maps with the following fields:

    * `:line` - The line number of the source code

    * `:text` - The full text of the comment, including the leading `#`

    * `:previous_eol_count` - How many end of lines there are between the comment and the previous AST node or comment

    * `:next_eol_count` - How many end of lines there are between the comment and the next AST node or comment

  Check `string_to_quoted/2` for options information.

  ## Examples

      iex> Code.string_to_quoted_with_comments("\""
      ...> :foo
      ...>
      ...> # Hello, world!
      ...>
      ...>
      ...> # Some more comments!
      ...> "\"")
      {:ok, :foo, [
        %{line: 3, column: 1, previous_eol_count: 2, next_eol_count: 3, text: "\# Hello, world!"},
        %{line: 6, column: 1, previous_eol_count: 3, next_eol_count: 1, text: "\# Some more comments!"},
      ]}

      iex> Code.string_to_quoted_with_comments(":foo # :bar")
      {:ok, :foo, [
        %{line: 1, column: 6, previous_eol_count: 0, next_eol_count: 0, text: "\# :bar"}
      ]}

  """
  @doc since: "1.13.0"
  @spec string_to_quoted_with_comments(List.Chars.t(), keyword) ::
          {:ok, Macro.t(), list(map())} | {:error, {location :: keyword, term, term}}
  def string_to_quoted_with_comments(string, opts \\ []) when is_list(opts) do
    charlist = to_charlist(string)
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    column = Keyword.get(opts, :column, 1)

    Process.put(:code_formatter_comments, [])
    opts = [preserve_comments: &preserve_comments/5] ++ opts

    with {:ok, tokens} <- :elixir.string_to_tokens(charlist, line, column, file, opts),
         {:ok, forms} <- :elixir.tokens_to_quoted(tokens, file, opts) do
      comments = Enum.reverse(Process.get(:code_formatter_comments))
      {:ok, forms, comments}
    end
  after
    Process.delete(:code_formatter_comments)
  end

  @doc """
  Converts the given string to its quoted form and a list of comments.

  Returns the AST and a list of comments if it succeeds, raises an exception
  otherwise. The exception is a `TokenMissingError` in case a token is missing
  (usually because the expression is incomplete), `SyntaxError` otherwise.

  Check `string_to_quoted/2` for options information.
  """
  @doc since: "1.13.0"
  @spec string_to_quoted_with_comments!(List.Chars.t(), keyword) :: {Macro.t(), list(map())}
  def string_to_quoted_with_comments!(string, opts \\ []) do
    charlist = to_charlist(string)

    case string_to_quoted_with_comments(charlist, opts) do
      {:ok, forms, comments} ->
        {forms, comments}

      {:error, {location, error, token}} ->
        :elixir_errors.parse_error(
          location,
          Keyword.get(opts, :file, "nofile"),
          error,
          token,
          {charlist, Keyword.get(opts, :line, 1), Keyword.get(opts, :column, 1)}
        )
    end
  end

  defp preserve_comments(line, column, tokens, comment, rest) do
    comments = Process.get(:code_formatter_comments)

    comment = %{
      line: line,
      column: column,
      previous_eol_count: previous_eol_count(tokens),
      next_eol_count: next_eol_count(rest, 0),
      text: List.to_string(comment)
    }

    Process.put(:code_formatter_comments, [comment | comments])
  end

  defp next_eol_count([?\s | rest], count), do: next_eol_count(rest, count)
  defp next_eol_count([?\t | rest], count), do: next_eol_count(rest, count)
  defp next_eol_count([?\n | rest], count), do: next_eol_count(rest, count + 1)
  defp next_eol_count([?\r, ?\n | rest], count), do: next_eol_count(rest, count + 1)
  defp next_eol_count(_, count), do: count

  defp previous_eol_count([{token, {_, _, count}} | _])
       when token in [:eol, :",", :";"] and count > 0 do
    count
  end

  defp previous_eol_count([]), do: 1
  defp previous_eol_count(_), do: 0

  @doc ~S"""
  Converts a quoted expression to an algebra document using Elixir's formatter rules.

  The algebra document can be converted into a string by calling:

      doc
      |> Inspect.Algebra.format(:infinity)
      |> IO.iodata_to_binary()

  For a high-level function that does the same, see `Macro.to_string/1`.

  ## Formatting considerations

  The Elixir AST does not contain metadata for literals like strings, lists, or
  tuples with two elements, which means that the produced algebra document will
  not respect all of the user preferences and comments may be misplaced.
  To get better results, you can use the `:token_metadata`, `:unescape` and
  `:literal_encoder` options to `string_to_quoted/2` to provide additional
  information to the formatter:

      [
        literal_encoder: &{:ok, {:__block__, &2, [&1]}},
        token_metadata: true,
        unescape: false
      ]

  This will produce an AST that contains information such as `do` blocks start
  and end lines or sigil delimiters, and by wrapping literals in blocks they can
  now hold metadata like line number, string delimiter and escaped sequences, or
  integer formatting (such as `0x2a` instead of `47`). However, **note this AST is
  not valid**. If you evaluate it, it won't have the same semantics as the regular
  Elixir AST due to the `:unescape` and `:literal_encoder` options. However,
  those options are useful if you're doing source code manipulation, where it's
  important to preserve user choices and comments placing.

  ## Options

    * `:comments` - the list of comments associated with the quoted expression.
      Defaults to `[]`. It is recommended that both `:token_metadata` and
      `:literal_encoder` options are given to `string_to_quoted_with_comments/2`
      in order to get proper placement for comments

    * `:escape` - when `true`, escaped sequences like `\n` will be escaped into
      `\\n`. If the `:unescape` option was set to `false` when using
      `string_to_quoted/2`, setting this option to `false` will prevent it from
      escaping the sequences twice. Defaults to `true`.

    * `:locals_without_parens` - a keyword list of name and arity
      pairs that should be kept without parens whenever possible.
      The arity may be the atom `:*`, which implies all arities of
      that name. The formatter already includes a list of functions
      and this option augments this list.

    * `:syntax_colors` - a keyword list of colors the output is colorized.
      See `Inspect.Opts` for more information.
  """
  @doc since: "1.13.0"
  @spec quoted_to_algebra(Macro.t(), keyword) :: Inspect.Algebra.t()
  def quoted_to_algebra(quoted, opts \\ []) do
    quoted
    |> Code.Normalizer.normalize(opts)
    |> Code.Formatter.to_algebra(opts)
  end

  @doc """
  Evaluates the given file.

  Accepts `relative_to` as an argument to tell where the file is located.

  While `require_file/2` and `compile_file/2` return the loaded modules and their
  bytecode, `eval_file/2` simply evaluates the file contents and returns the
  evaluation result and its binding (exactly the same return value as `eval_string/3`).
  """
  @spec eval_file(binary, nil | binary) :: {term, binding}
  def eval_file(file, relative_to \\ nil) when is_binary(file) do
    {charlist, file} = find_file!(file, relative_to)
    eval_string(charlist, [], file: file, line: 1)
  end

  @deprecated "Use Code.require_file/2 or Code.compile_file/2 instead"
  @doc false
  def load_file(file, relative_to \\ nil) when is_binary(file) do
    {charlist, file} = find_file!(file, relative_to)
    :elixir_code_server.call({:acquire, file})

    loaded =
      Module.ParallelChecker.verify(fn ->
        :elixir_compiler.string(charlist, file, fn _, _ -> :ok end)
      end)

    :elixir_code_server.cast({:required, file})
    loaded
  end

  @doc """
  Requires the given `file`.

  Accepts `relative_to` as an argument to tell where the file is located.
  If the file was already required, `require_file/2` doesn't do anything and
  returns `nil`.

  Note that if `require_file/2` is invoked by different processes concurrently,
  the first process to invoke `require_file/2` acquires a lock and the remaining
  ones will block until the file is available. This means that if `require_file/2`
  is called more than once with a given file, that file will be compiled only once.
  The first process to call `require_file/2` will get the list of loaded modules,
  others will get `nil`. The list of required files is managed per Erlang VM node.

  See `compile_file/2` if you would like to compile a file without tracking its
  filenames. Finally, if you would like to get the result of evaluating a file rather
  than the modules defined in it, see `eval_file/2`.

  ## Examples

  If the file has not been required, it returns the list of modules:

      modules = Code.require_file("eex_test.exs", "../eex/test")
      List.first(modules)
      #=> {EExTest.Compiled, <<70, 79, 82, 49, ...>>}

  If the file has been required, it returns `nil`:

      Code.require_file("eex_test.exs", "../eex/test")
      #=> nil

  """
  @spec require_file(binary, nil | binary) :: [{module, binary}] | nil
  def require_file(file, relative_to \\ nil) when is_binary(file) do
    {charlist, file} = find_file!(file, relative_to)

    case :elixir_code_server.call({:acquire, file}) do
      :required ->
        nil

      :proceed ->
        loaded =
          Module.ParallelChecker.verify(fn ->
            :elixir_compiler.string(charlist, file, fn _, _ -> :ok end)
          end)

        :elixir_code_server.cast({:required, file})
        loaded
    end
  end

  @doc """
  Gets all compilation options from the code server.

  To get individual options, see `get_compiler_option/1`.
  For a description of all options, see `put_compiler_option/2`.

  ## Examples

      Code.compiler_options()
      #=> %{debug_info: true, docs: true, ...}

  """
  @spec compiler_options :: map
  def compiler_options do
    for key <- @available_compiler_options, into: %{} do
      {key, :elixir_config.get(key)}
    end
  end

  @doc """
  Stores all given compilation options.

  Changing the compilation options affect all processes
  running in a given Erlang VM node. To store individual
  options and for a description of all options, see
  `put_compiler_option/2`.

  Returns a map with previous values.

  ## Examples

      Code.compiler_options(warnings_as_errors: true)
      #=> %{warnings_as_errors: false}

  """
  @spec compiler_options(Enumerable.t({atom, term})) :: %{optional(atom) => term}
  def compiler_options(opts) do
    for {key, value} <- opts, into: %{} do
      previous = get_compiler_option(key)
      put_compiler_option(key, value)
      {key, previous}
    end
  end

  @doc """
  Returns the value of a given compiler option.

  For a description of all options, see `put_compiler_option/2`.

  ## Examples

      Code.get_compiler_option(:debug_info)
      #=> true

  """
  @doc since: "1.10.0"
  @spec get_compiler_option(atom) :: term
  def get_compiler_option(key) when key in @available_compiler_options do
    :elixir_config.get(key)
  end

  @doc """
  Returns a list with all available compiler options.

  For a description of all options, see `put_compiler_option/2`.

  ## Examples

      Code.available_compiler_options()
      #=> [:docs, :debug_info, ...]

  """
  @spec available_compiler_options() :: [atom]
  def available_compiler_options do
    @available_compiler_options
  end

  @doc """
  Stores a compilation option.

  Changing the compilation options affect all processes running in a
  given Erlang VM node.

  Available options are:

    * `:docs` - when `true`, retains documentation in the compiled module.
      Defaults to `true`.

    * `:debug_info` - when `true`, retains debug information in the compiled
      module. Defaults to `true`.
      This enables static analysis tools as it allows developers to
      partially reconstruct the original source code. Therefore, disabling
      `:debug_info` is not recommended as it removes the ability of the
      Elixir compiler and other tools to provide feedback. If you want to
      remove the `:debug_info` while deploying, tools like `mix release`
      already do such by default.
      Additionally, `mix test` disables it via the `:test_elixirc_options`
      project configuration option.
      This option can also be overridden per module using the `@compile` directive.

    * `:ignore_already_consolidated` (since v1.10.0) - when `true`, does not warn
      when a protocol has already been consolidated and a new implementation is added.
      Defaults to `false`.

    * `:ignore_module_conflict` - when `true`, does not warn when a module has
      already been defined. Defaults to `false`.

    * `:relative_paths` - when `true`, uses relative paths in quoted nodes,
      warnings, and errors generated by the compiler. Note disabling this option
      won't affect runtime warnings and errors. Defaults to `true`.

    * `:warnings_as_errors` - causes compilation to fail when warnings are
      generated. Defaults to `false`.

    * `:no_warn_undefined` (since v1.10.0) - list of modules and `{Mod, fun, arity}`
      tuples that will not emit warnings that the module or function does not exist
      at compilation time. Pass atom `:all` to skip warning for all undefined
      functions. This can be useful when doing dynamic compilation. Defaults to `[]`.

    * `:tracers` (since v1.10.0) - a list of tracers (modules) to be used during
      compilation. See the module docs for more information. Defaults to `[]`.

    * `:parser_options` (since v1.10.0) - a keyword list of options to be given
      to the parser when compiling files. It accepts the same options as
      `string_to_quoted/2` (except by the options that change the AST itself).
      This can be used in combination with the tracer to retrieve localized
      information about events happening during compilation. Defaults to `[columns: true]`.
      This option only affects code compilation functions, such as `compile_string/2`
      and `compile_file/2` but not `string_to_quoted/2` and friends, as the
      latter is used for other purposes beyond compilation.

    * `:on_undefined_variable` (since v1.15.0) - either `:raise` or `:warn`.
      When `:raise` (the default), undefined variables will trigger a compilation
      error. You may be set it to `:warn` if you want undefined variables to
      emit a warning and expand as to a local call to the zero-arity function
      of the same name (for example, `node` would be expanded as `node()`).
      This `:warn` behavior only exists for compatibility reasons when working
      with old dependencies, its usage is discouraged and it will be removed
      in future releases.

  It always returns `:ok`. Raises an error for invalid options.

  ## Examples

      Code.put_compiler_option(:debug_info, true)
      #=> :ok

  """
  @doc since: "1.10.0"
  @spec put_compiler_option(atom, term) :: :ok
  def put_compiler_option(key, value) when key in @boolean_compiler_options do
    if not is_boolean(value) do
      raise "compiler option #{inspect(key)} should be a boolean, got: #{inspect(value)}"
    end

    :elixir_config.put(key, value)
    :ok
  end

  def put_compiler_option(:no_warn_undefined, value) do
    if value != :all and not is_list(value) do
      raise "compiler option :no_warn_undefined should be a list or the atom :all, " <>
              "got: #{inspect(value)}"
    end

    :elixir_config.put(:no_warn_undefined, value)
    :ok
  end

  def put_compiler_option(key, value) when key in @list_compiler_options do
    if not is_list(value) do
      raise "compiler option #{inspect(key)} should be a list, got: #{inspect(value)}"
    end

    if key == :parser_options and not Keyword.keyword?(value) do
      raise "compiler option #{inspect(key)} should be a keyword list, " <>
              "got: #{inspect(value)}"
    end

    if key == :tracers and not Enum.all?(value, &is_atom/1) do
      raise "compiler option #{inspect(key)} should be a list of modules, " <>
              "got: #{inspect(value)}"
    end

    :elixir_config.put(key, value)
    :ok
  end

  # TODO: Make this option have no effect on Elixir v2.0
  # TODO: Warn if mode is :warn on Elixir v1.19
  def put_compiler_option(:on_undefined_variable, value) when value in [:raise, :warn] do
    :elixir_config.put(:on_undefined_variable, value)
    :ok
  end

  def put_compiler_option(key, _value) do
    raise "unknown compiler option: #{inspect(key)}"
  end

  @doc """
  Purge compiler modules.

  The compiler utilizes temporary modules to compile code. For example,
  `elixir_compiler_1`, `elixir_compiler_2`, and so on. In case the compiled code
  stores references to anonymous functions or similar, the Elixir compiler
  may be unable to reclaim those modules, keeping an unnecessary amount of
  code in memory and eventually leading to modules such as `elixir_compiler_12345`.

  This function purges all modules currently kept by the compiler, allowing
  old compiler module names to be reused. If there are any processes running
  any code from such modules, they will be terminated too.

  This function is only meant to be called if you have a long running node
  that is constantly evaluating code.

  It returns `{:ok, number_of_modules_purged}`.
  """
  @doc since: "1.7.0"
  @spec purge_compiler_modules() :: {:ok, non_neg_integer()}
  def purge_compiler_modules() do
    :elixir_code_server.call(:purge_compiler_modules)
  end

  @doc """
  Compiles the given string.

  Returns a list of tuples where the first element is the module name
  and the second one is its bytecode (as a binary). A `file` can be
  given as a second argument which will be used for reporting warnings
  and errors.

  **Warning**: `string` can be any Elixir code and code can be executed with
  the same privileges as the Erlang VM: this means that such code could
  compromise the machine (for example by executing system commands).
  Don't use `compile_string/2` with untrusted input (such as strings coming
  from the network).
  """
  @spec compile_string(List.Chars.t(), binary) :: [{module, binary}]
  def compile_string(string, file \\ "nofile") when is_binary(file) do
    Module.ParallelChecker.verify(fn ->
      :elixir_compiler.string(to_charlist(string), file, fn _, _ -> :ok end)
    end)
  end

  @doc """
  Compiles the quoted expression.

  Returns a list of tuples where the first element is the module name and
  the second one is its bytecode (as a binary). A `file` can be
  given as second argument which will be used for reporting warnings
  and errors.
  """
  @spec compile_quoted(Macro.t(), binary) :: [{module, binary}]
  def compile_quoted(quoted, file \\ "nofile") when is_binary(file) do
    Module.ParallelChecker.verify(fn ->
      :elixir_compiler.quoted(quoted, file, fn _, _ -> :ok end)
    end)
  end

  @doc """
  Compiles the given file.

  Accepts `relative_to` as an argument to tell where the file is located.

  Returns a list of tuples where the first element is the module name and
  the second one is its bytecode (as a binary). Opposite to `require_file/2`,
  it does not track the filename of the compiled file.

  If you would like to get the result of evaluating file rather than the
  modules defined in it, see `eval_file/2`.

  For compiling many files concurrently, see `Kernel.ParallelCompiler.compile/2`.
  """
  @doc since: "1.7.0"
  @spec compile_file(binary, nil | binary) :: [{module, binary}]
  def compile_file(file, relative_to \\ nil) when is_binary(file) do
    Module.ParallelChecker.verify(fn ->
      {charlist, file} = find_file!(file, relative_to)
      :elixir_compiler.string(charlist, file, fn _, _ -> :ok end)
    end)
  end

  @doc """
  Ensures the given module is loaded.

  If the module is already loaded, this works as no-op. If the module
  was not yet loaded, it tries to load it.

  If it succeeds in loading the module, it returns `{:module, module}`.
  If not, returns `{:error, reason}` with the error reason.

  See the module documentation for more information on code loading.

  ## Examples

      iex> Code.ensure_loaded(Atom)
      {:module, Atom}

      iex> Code.ensure_loaded(DoesNotExist)
      {:error, :nofile}

  """
  @spec ensure_loaded(module) ::
          {:module, module} | {:error, :embedded | :badfile | :nofile | :on_load_failure}
  def ensure_loaded(module) when is_atom(module) do
    :code.ensure_loaded(module)
  end

  @doc """
  Ensures the given module is loaded.

  Similar to `ensure_loaded/1`, but returns `true` if the module
  is already loaded or was successfully loaded. Returns `false`
  otherwise.

  ## Examples

      iex> Code.ensure_loaded?(String)
      true

  """
  @spec ensure_loaded?(module) :: boolean
  def ensure_loaded?(module) when is_atom(module) do
    match?({:module, ^module}, ensure_loaded(module))
  end

  @doc """
  Same as `ensure_loaded/1` but raises if the module cannot be loaded.
  """
  @doc since: "1.12.0"
  @spec ensure_loaded!(module) :: module
  def ensure_loaded!(module) do
    case ensure_loaded(module) do
      {:module, module} ->
        module

      {:error, reason} ->
        raise ArgumentError,
              "could not load module #{inspect(module)} due to reason #{inspect(reason)}"
    end
  end

  @doc """
  Ensures the given modules are loaded.

  Similar to `ensure_loaded/1`, but accepts a list of modules instead of a single
  module, and loads all of them.

  If all modules load successfully, returns `:ok`. Otherwise, returns `{:error, errors}`
  where `errors` is a list of tuples made of the module and the reason it failed to load.

  ## Examples

      iex> Code.ensure_all_loaded([Atom, String])
      :ok

      iex> Code.ensure_all_loaded([Atom, DoesNotExist])
      {:error, [{DoesNotExist, :nofile}]}

  """
  @doc since: "1.15.0"
  @spec ensure_all_loaded([module]) :: :ok | {:error, [{module, reason}]}
        when reason: :badfile | :nofile | :on_load_failure
  def ensure_all_loaded(modules) when is_list(modules) do
    :code.ensure_modules_loaded(modules)
  end

  @doc """
  Same as `ensure_all_loaded/1` but raises if any of the modules cannot be loaded.
  """
  @doc since: "1.15.0"
  @spec ensure_all_loaded!([module]) :: :ok
  def ensure_all_loaded!(modules) do
    case ensure_all_loaded(modules) do
      :ok ->
        :ok

      {:error, errors} ->
        formatted_errors =
          errors
          |> Enum.sort()
          |> Enum.map_join("\n", fn {module, reason} ->
            "  * #{inspect(module)} due to reason #{inspect(reason)}"
          end)

        raise ArgumentError, "could not load the following modules:\n\n" <> formatted_errors
    end
  end

  @doc """
  Similar to `ensure_compiled!/1` but indicates you can continue without said module.

  While `ensure_compiled!/1` indicates to the Elixir compiler you can
  only continue when said module is available, this function indicates
  you may continue compilation without said module.

  If it succeeds in loading the module, it returns `{:module, module}`.
  If not, returns `{:error, reason}` with the error reason.
  If the module being checked is currently in a compiler deadlock,
  this function returns `{:error, :unavailable}`. Unavailable doesn't
  necessarily mean the module doesn't exist, just that it is not currently
  available, but it (or may not) become available in the future.

  Therefore, if you can only continue if the module is available, use
  `ensure_compiled!/1` instead. In particular, do not do this:

      case Code.ensure_compiled(module) do
        {:module, _} -> module
        {:error, _} -> raise ...
      end

  See the module documentation for more information on code loading.
  """
  @spec ensure_compiled(module) ::
          {:module, module}
          | {:error, :embedded | :badfile | :nofile | :on_load_failure | :unavailable}
  def ensure_compiled(module) when is_atom(module) do
    ensure_compiled(module, :soft)
  end

  @doc """
  Ensures the given module is compiled and loaded.

  If the module is already loaded, it works as no-op. If the module was
  not compiled yet, `ensure_compiled!/1` halts the compilation of the caller
  until the module given to `ensure_compiled!/1` becomes available or
  all files for the current project have been compiled. If compilation
  finishes and the module is not available or is in a deadlock, an error
  is raised.

  Given this function halts compilation, use it carefully. In particular,
  avoid using it to guess which modules are in the system. Overuse of this
  function can also lead to deadlocks, where two modules check at the same time
  if the other is compiled. This returns a specific unavailable error code,
  where we cannot successfully verify a module is available or not.

  See the module documentation for more information on code loading.
  """
  @doc since: "1.12.0"
  @spec ensure_compiled!(module) :: module
  def ensure_compiled!(module) do
    case ensure_compiled(module, :hard) do
      {:module, module} ->
        module

      {:error, reason} ->
        raise ArgumentError,
              "could not load module #{inspect(module)} due to reason #{inspect(reason)}"
    end
  end

  defp ensure_compiled(module, mode) do
    case :code.ensure_loaded(module) do
      {:error, :nofile} = error ->
        if can_await_module_compilation?() do
          case Kernel.ErrorHandler.ensure_compiled(module, :module, mode) do
            :found -> {:module, module}
            :deadlock -> {:error, :unavailable}
            :not_found -> {:error, :nofile}
          end
        else
          error
        end

      other ->
        other
    end
  end

  @doc """
  Returns `true` if the module is loaded.

  This function doesn't attempt to load the module. For such behavior,
  `ensure_loaded?/1` can be used.

  ## Examples

      iex> Code.loaded?(Atom)
      true

      iex> Code.loaded?(NotYetLoaded)
      false

  """
  @doc since: "1.15.0"
  @spec loaded?(module) :: boolean
  def loaded?(module) do
    :erlang.module_loaded(module)
  end

  @doc """
  Returns `true` if the current process can await for module compilation.

  When compiling Elixir code via `Kernel.ParallelCompiler`, which is
  used by Mix and `elixirc`, calling a module that has not yet been
  compiled will block the caller until the module becomes available.
  Executing Elixir scripts, such as passing a filename to `elixir`,
  does not await.
  """
  @doc since: "1.11.0"
  @spec can_await_module_compilation? :: boolean
  def can_await_module_compilation? do
    :erlang.process_info(self(), :error_handler) == {:error_handler, Kernel.ErrorHandler}
  end

  @doc false
  @deprecated "Use Code.ensure_compiled/1 instead (see the proper disclaimers in its docs)"
  def ensure_compiled?(module) when is_atom(module) do
    match?({:module, ^module}, ensure_compiled(module))
  end

  @doc ~S"""
  Returns the docs for the given module or path to `.beam` file.

  When given a module name, it finds its BEAM code and reads the docs from it.

  When given a path to a `.beam` file, it will load the docs directly from that
  file.

  It returns the term stored in the documentation chunk in the format defined by
  [EEP 48](https://www.erlang.org/eeps/eep-0048.html) or `{:error, reason}` if
  the chunk is not available.

  ## Examples

      # Module documentation of an existing module
      iex> {:docs_v1, _, :elixir, _, %{"en" => module_doc}, _, _} = Code.fetch_docs(Atom)
      iex> module_doc |> String.split("\n") |> Enum.at(0)
      "Atoms are constants whose values are their own name."

      # A module that doesn't exist
      iex> Code.fetch_docs(ModuleNotGood)
      {:error, :module_not_found}

  """
  @doc since: "1.7.0"
  @spec fetch_docs(module | String.t()) ::
          {:docs_v1, annotation, beam_language, format, module_doc :: doc_content, metadata,
           docs :: [doc_element]}
          | {:error,
             :module_not_found
             | :chunk_not_found
             | {:invalid_chunk, binary}
             | :invalid_beam}
        when annotation: :erl_anno.anno(),
             beam_language: :elixir | :erlang | atom(),
             doc_content: %{optional(binary) => binary} | :none | :hidden,
             doc_element:
               {{kind :: atom, function_name :: atom, arity}, annotation, signature, doc_content,
                metadata},
             format: binary,
             signature: [binary],
             metadata: map
  def fetch_docs(module_or_path)

  def fetch_docs(module) when is_atom(module) do
    case get_beam_and_path(module) do
      {bin, beam_path} ->
        case fetch_docs_from_beam(bin) do
          {:error, :chunk_not_found} ->
            app_root = Path.expand(Path.join(["..", ".."]), beam_path)
            path = Path.join([app_root, "doc", "chunks", "#{module}.chunk"])
            fetch_docs_from_chunk(path)

          other ->
            other
        end

      :error ->
        case :code.is_loaded(module) do
          {:file, :preloaded} ->
            # The ERTS directory is not necessarily included in releases
            # unless it is listed as an extra application.
            case :code.lib_dir(:erts) do
              path when is_list(path) ->
                path = Path.join([path, "doc", "chunks", "#{module}.chunk"])
                fetch_docs_from_chunk(path)

              {:error, _} ->
                {:error, :chunk_not_found}
            end

          _ ->
            {:error, :module_not_found}
        end
    end
  end

  def fetch_docs(path) when is_binary(path) do
    fetch_docs_from_beam(String.to_charlist(path))
  end

  defp get_beam_and_path(module) do
    with {^module, beam, filename} <- :code.get_object_code(module),
         info_pairs when is_list(info_pairs) <- :beam_lib.info(beam),
         {:ok, ^module} <- Keyword.fetch(info_pairs, :module) do
      {beam, filename}
    else
      _ -> :error
    end
  end

  @docs_chunk [?D, ?o, ?c, ?s]

  defp fetch_docs_from_beam(bin_or_path) do
    case :beam_lib.chunks(bin_or_path, [@docs_chunk]) do
      {:ok, {_module, [{@docs_chunk, bin}]}} ->
        load_docs_chunk(bin)

      {:error, :beam_lib, {:missing_chunk, _, @docs_chunk}} ->
        {:error, :chunk_not_found}

      {:error, :beam_lib, {:file_error, _, :enoent}} ->
        {:error, :module_not_found}

      {:error, :beam_lib, _} ->
        {:error, :invalid_beam}
    end
  end

  defp fetch_docs_from_chunk(path) do
    case File.read(path) do
      {:ok, bin} ->
        load_docs_chunk(bin)

      {:error, _} ->
        {:error, :chunk_not_found}
    end
  end

  defp load_docs_chunk(bin) do
    :erlang.binary_to_term(bin)
  rescue
    _ ->
      {:error, {:invalid_chunk, bin}}
  end

  @doc false
  @deprecated "Code.get_docs/2 always returns nil as its outdated documentation is no longer stored on BEAM files. Use Code.fetch_docs/1 instead"
  def get_docs(_module, _kind) do
    nil
  end

  ## Helpers

  # Finds the file given the relative_to path.
  #
  # If the file is found, returns its path in binary, fails otherwise.
  defp find_file!(file, relative_to) do
    file =
      if relative_to do
        Path.expand(file, relative_to)
      else
        Path.expand(file)
      end

    case File.read(file) do
      {:ok, bin} -> {String.to_charlist(bin), file}
      {:error, reason} -> raise Code.LoadError, file: file, reason: reason
    end
  end
end
