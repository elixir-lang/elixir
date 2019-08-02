defmodule Code do
  @moduledoc """
  Utilities for managing code compilation, code evaluation, and code loading.

  This module complements Erlang's [`:code` module](http://www.erlang.org/doc/man/code.html)
  to add behaviour which is specific to Elixir. Almost all of the functions in this module
  have global side effects on the behaviour of Elixir.

  ## Working with files

  This module contains three functions for compiling and evaluating files.
  Here is a summary of them and their behaviour:

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

  ## Compilation tracers

  Elixir supports compilation tracers, which allows modules to observe constructs
  handled by the Elixir compiler when compiling files. A tracer is a module
  that implements the `trace/2` function. The function receives the event name
  as first argument and `Macro.Env` as second and it must return `:ok`. It is
  very important for a tracer to do as little work as possible synchronously
  and dispatch the bulk of the work to a separate process. **Slow tracers will
  slow down compilation**.

  You can configure your list of tracers via `put_compiler_option/2`. The
  following events are available to tracers:

    * `{:import, meta, module, opts}` - traced whenever `module` is imported.
      `meta` is the import AST metadata and `opts` are the import options.

    * `{:imported_function, meta, module, name, arity}` and
      `{:imported_macro, meta, module, name, arity}` - traced whenever an
      imported function or macro is invoked. `meta` is the call AST metadata,
      `module` is the module the import is from, followed by the `name` and `arity`
      of the imported function/macro.

    * `{:alias, meta, alias, as, opts}` - traced whenever `alias` is aliased
      to `as`. `meta` is the alias AST metadata and `opts` are the alias options.

    * `{:alias_expansion, meta, as, alias}` traced whenever there is an alias
      expansion, i.e. when the user writes `as` which is expanded to `alias`.
      `meta` is the alias expansion AST metadata.

    * `{:require, meta, module, opts}` - traced whenever `module` is required.
      `meta` is the require AST metadata and `opts` are the require options.

    * `{:struct_expansion, meta, module}` - traced whenever `module`' struct
      is expanded. `meta` is the struct AST metadata.

    * `{:remote_reference, meta, module}` - traced whenever there is a remote
      reference to `module`, i.e. whenever the user writes `MyModule.Foo.Bar`
      in the code, regardless if it is an alias or not.

    * `{:remote_function, meta, module, name, arity}` and
      `{:remote_macro, meta, module, name, arity}` - traced whenever a remote
      function or macro is referenced. `meta` is the call AST metadata, `module`
      is the invoked module, followed by the `name` and `arity`.

    * `{:local_function, meta, module, name, arity}` and
      `{:local_macro, meta, module, name, arity}` - traced whenever a local
      function or macro is referenced. `meta` is the call AST metadata, `module`
      is the invoked module, followed by the `name` and `arity`.

  The `:tracers` compiler option can be combined with the `:parser_options`
  compiler option to enrich the metadata of the traced events above.

  New events may be added at any time in the future, therefore it is advised
  for the `trace/2` function to have a "catch-all" clause.
  """

  @boolean_compiler_options [
    :docs,
    :debug_info,
    :ignore_module_conflict,
    :relative_paths,
    :warnings_as_errors
  ]

  @list_compiler_options [:no_warn_undefined, :tracers, :parser_options]

  @available_compiler_options @boolean_compiler_options ++ @list_compiler_options

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

  @doc """
  Removes files from the required files list.

  The modules defined in the file are not removed;
  calling this function only removes them from the list,
  allowing them to be required again.

  ## Examples

      # Require EEx test code
      Code.require_file("../eex/test/eex_test.exs")

      # Now unrequire all files
      Code.unrequire_files(Code.required_files())

      # Notice modules are still available
      function_exported?(EExTest.Compiled, :before_compile, 0)
      #=> true

  """
  @doc since: "1.7.0"
  @spec unrequire_files([binary]) :: :ok
  def unrequire_files(files) do
    :elixir_code_server.cast({:unrequire_files, files})
  end

  @deprecated "Use Code.unrequire_files/1 instead"
  @doc false
  def unload_files(files) do
    unrequire_files(files)
  end

  @doc """
  Appends a path to the end of the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code.

  The path is expanded with `Path.expand/1` before being appended.
  If this path does not exist, an error is returned.

  ## Examples

      Code.append_path(".")
      #=> true

      Code.append_path("/does_not_exist")
      #=> {:error, :bad_directory}

  """
  @spec append_path(Path.t()) :: true | {:error, :bad_directory}
  def append_path(path) do
    :code.add_pathz(to_charlist(Path.expand(path)))
  end

  @doc """
  Prepends a path to the beginning of the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for finding
  module code.

  The path is expanded with `Path.expand/1` before being prepended.
  If this path does not exist, an error is returned.

  ## Examples

      Code.prepend_path(".")
      #=> true

      Code.prepend_path("/does_not_exist")
      #=> {:error, :bad_directory}

  """
  @spec prepend_path(Path.t()) :: true | {:error, :bad_directory}
  def prepend_path(path) do
    :code.add_patha(to_charlist(Path.expand(path)))
  end

  @doc """
  Deletes a path from the Erlang VM code path list. This is the list of
  directories the Erlang VM uses for finding module code.

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
    :code.del_path(to_charlist(Path.expand(path)))
  end

  @doc """
  Evaluates the contents given by `string`.

  The `binding` argument is a keyword list of variable bindings.
  The `opts` argument is a keyword list of environment options.

  **Warning**: `string` can be any Elixir code and will be executed with
  the same privileges as the Erlang VM: this means that such code could
  compromise the machine (for example by executing system commands).
  Don't use `eval_string/3` with untrusted input (such as strings coming
  from the network).

  ## Options

  Options can be:

    * `:file` - the file to be considered in the evaluation

    * `:line` - the line on which the script starts

  Additionally, the following scope values can be configured:

    * `:aliases` - a list of tuples with the alias and its target

    * `:requires` - a list of modules required

    * `:functions` - a list of tuples where the first element is a module
      and the second a list of imported function names and arity; the list
      of function names and arity must be sorted

    * `:macros` - a list of tuples where the first element is a module
      and the second a list of imported macro names and arity; the list
      of function names and arity must be sorted

  Notice that setting any of the values above overrides Elixir's default
  values. For example, setting `:requires` to `[]` will no longer
  automatically require the `Kernel` module. In the same way setting
  `:macros` will no longer auto-import `Kernel` macros like `Kernel.if/2`,
  `Kernel.SpecialForms.case/2`, and so on.

  Returns a tuple of the form `{value, binding}`,
  where `value` is the value returned from evaluating `string`.
  If an error occurs while evaluating `string` an exception will be raised.

  `binding` is a keyword list with the value of all variable bindings
  after evaluating `string`. The binding key is usually an atom, but it
  may be a tuple for variables defined in a different context.

  ## Examples

      iex> Code.eval_string("a + b", [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      {3, [a: 1, b: 2]}

      iex> Code.eval_string("c = a + b", [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2, c: 3]}

      iex> Code.eval_string("a = a + b", [a: 1, b: 2])
      {3, [a: 3, b: 2]}

  For convenience, you can pass `__ENV__/0` as the `opts` argument and
  all imports, requires and aliases defined in the current environment
  will be automatically carried over:

      iex> Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2]}

  """
  @spec eval_string(List.Chars.t(), list, Macro.Env.t() | keyword) :: {term, binding :: list}
  def eval_string(string, binding \\ [], opts \\ [])

  def eval_string(string, binding, %Macro.Env{} = env) do
    {value, binding, _env, _scope} = :elixir.eval(to_charlist(string), binding, Map.to_list(env))
    {value, binding}
  end

  def eval_string(string, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    {value, binding, _env, _scope} = :elixir.eval(to_charlist(string), binding, opts)
    {value, binding}
  end

  @doc ~S"""
  Formats the given code `string`.

  The formatter receives a string representing Elixir code and
  returns iodata representing the formatted code according to
  pre-defined rules.

  ## Options

    * `:file` - the file which contains the string, used for error
      reporting

    * `:line` - the line the string starts, used for error reporting

    * `:line_length` - the line length to aim for when formatting
      the document. Defaults to 98. Note this value is used as
      reference but it is not enforced by the formatter as sometimes
      user intervention is required. See "Running the formatter"
      section

    * `:locals_without_parens` - a keyword list of name and arity
      pairs that should be kept without parens whenever possible.
      The arity may be the atom `:*`, which implies all arities of
      that name. The formatter already includes a list of functions
      and this option augments this list.

    * `:rename_deprecated_at` - rename all known deprecated functions
      at the given version to their non-deprecated equivalent. It
      expects a valid `Version` which is usually the minimum Elixir
      version supported by the project.

    * `:force_do_end_blocks` (since v1.9.0) - when `true`, converts all
      inline usages of `do: ...`,  `else: ...` and friends into `do/end`
      blocks. Defaults to `false`. Notice this option is convergent:
      once you set it to `true`, all keywords will be converted. If you
      set it to `false` later on, `do/end` blocks won't be converted
      back to keywords.

  ## Design principles

  The formatter was designed under three principles.

  First, the formatter never changes the semantics of the code by
  default. This means the input AST and the output AST are equivalent.
  Optional behaviour, such as `:rename_deprecated_at`, is allowed to
  break this guarantee.

  The second principle is to provide as little configuration as possible.
  This eases the formatter adoption by removing contention points while
  making sure a single style is followed consistently by the community as
  a whole.

  The formatter does not hard code names. The formatter will not behave
  specially because a function is named `defmodule`, `def`, etc. This
  principle mirrors Elixir's goal of being an extensible language where
  developers can extend the language with new constructs as if they were
  part of the language. When it is absolutely necessary to change behaviour
  based on the name, this behaviour should be configurable, such as the
  `:locals_without_parens` option.

  ## Running the formatter

  The formatter attempts to fit the most it can on a single line and
  introduces line breaks wherever possible when it cannot.

  In some cases, this may lead to undesired formatting. Therefore, **some
  code generated by the formatter may not be aesthetically pleasing and
  may require explicit intervention from the developer**. That's why we
  do not recommend to run the formatter blindly in an existing codebase.
  Instead you should format and sanity check each formatted file.

  Let's see some examples. The code below:

      "this is a very long string ... #{inspect(some_value)}"

  may be formatted as:

      "this is a very long string ... #{
        inspect(some_value)
      }"

  This happens because the only place the formatter can introduce a
  new line without changing the code semantics is in the interpolation.
  In those scenarios, we recommend developers to directly adjust the
  code. Here we can use the binary concatenation operator `<>/2`:

      "this is a very long string " <>
        "... #{inspect(some_value)}"

  The string concatenation makes the code fit on a single line and also
  gives more options to the formatter.

  A similar example is when the formatter breaks a function definition
  over multiple clauses:

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

  ## Keeping user's formatting

  The formatter respects the input format in some cases. Those are
  listed below:

    * Insignificant digits in numbers are kept as is. The formatter
      however always inserts underscores for decimal numbers with more
      than 5 digits and converts hexadecimal digits to uppercase

    * Strings, charlists, atoms and sigils are kept as is. No character
      is automatically escaped or unescaped. The choice of delimiter is
      also respected from the input

    * Newlines inside blocks are kept as in the input except for:
      1) expressions that take multiple lines will always have an empty
      line before and after and 2) empty lines are always squeezed
      together into a single empty line

    * The choice between `:do` keyword and `do/end` blocks is left
      to the user

    * Lists, tuples, bitstrings, maps, structs and function calls will be
      broken into multiple lines if they are followed by a newline in the
      opening bracket and preceded by a new line in the closing bracket

    * Newlines before certain operators (such as the pipeline operators)
      and before other operators (such as comparison operators)

  The behaviours above are not guaranteed. We may remove or add new
  rules in the future. The goal of documenting them is to provide better
  understanding on what to expect from the formatter.

  ### Multi-line lists, maps, tuples, etc.

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

    1. calls that have do/end blocks
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

  The formatter also handles code comments in a way to guarantee a space
  is always added between the beginning of the comment (#) and the next
  character.

  The formatter also extracts all trailing comments to their previous line.
  For example, the code below

      hello #world

  will be rewritten to

      # world
      hello

  Because code comments are handled apart from the code representation (AST),
  there are some situations where code comments are seen as ambiguous by the
  code formatter. For example, the comment in the anonymous function below

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
  """
  @doc since: "1.6.0"
  @spec format_string!(binary, keyword) :: iodata
  def format_string!(string, opts \\ []) when is_binary(string) and is_list(opts) do
    line_length = Keyword.get(opts, :line_length, 98)
    algebra = Code.Formatter.to_algebra!(string, opts)
    Inspect.Algebra.format(algebra, line_length)
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

  See `eval_string/3` for a description of bindings and options.

  ## Examples

      iex> contents = quote(do: var!(a) + var!(b))
      iex> Code.eval_quoted(contents, [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      {3, [a: 1, b: 2]}

  For convenience, you can pass `__ENV__/0` as the `opts` argument and
  all options will be automatically extracted from the current environment:

      iex> contents = quote(do: var!(a) + var!(b))
      iex> Code.eval_quoted(contents, [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2]}

  """
  @spec eval_quoted(Macro.t(), list, Macro.Env.t() | keyword) :: {term, binding :: list}
  def eval_quoted(quoted, binding \\ [], opts \\ [])

  def eval_quoted(quoted, binding, %Macro.Env{} = env) do
    {value, binding, _env, _scope} = :elixir.eval_quoted(quoted, binding, Map.to_list(env))
    {value, binding}
  end

  def eval_quoted(quoted, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    {value, binding, _env, _scope} = :elixir.eval_quoted(quoted, binding, opts)
    {value, binding}
  end

  defp validate_eval_opts(opts) do
    if f = opts[:functions], do: validate_imports(:functions, f)
    if m = opts[:macros], do: validate_imports(:macros, m)
    if a = opts[:aliases], do: validate_aliases(:aliases, a)
    if r = opts[:requires], do: validate_requires(:requires, r)
  end

  defp validate_requires(kind, requires) do
    valid = is_list(requires) and Enum.all?(requires, &is_atom(&1))

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [module]"
    end
  end

  defp validate_aliases(kind, aliases) do
    valid = is_list(aliases) and Enum.all?(aliases, fn {k, v} -> is_atom(k) and is_atom(v) end)

    unless valid do
      raise ArgumentError,
            "expected :#{kind} option given to eval in the format: [{module, module}]"
    end
  end

  defp validate_imports(kind, imports) do
    valid =
      is_list(imports) and
        Enum.all?(imports, fn {k, v} ->
          is_atom(k) and is_list(v) and
            Enum.all?(v, fn {name, arity} -> is_atom(name) and is_integer(arity) end)
        end)

    unless valid do
      raise ArgumentError,
            "expected :#{kind} option given to eval in the format: [{module, [{name, arity}]}]"
    end
  end

  @doc ~S"""
  Converts the given string to its quoted form.

  Returns `{:ok, quoted_form}` if it succeeds,
  `{:error, {line, error, token}}` otherwise.

  ## Options

    * `:file` - the filename to be reported in case of parsing errors.
      Defaults to "nofile".

    * `:line` - the starting line of the string being parsed.
      Defaults to 1.

    * `:columns` - when `true`, attach a `:column` key to the quoted
      metadata. Defaults to `false`.

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
      option overrides the `:existing_atoms_only` behaviour for static
      atoms but `:existing_atoms_only` is still used for dynamic atoms,
      such as atoms with interpolations.

    * `:warn_on_unnecessary_quotes` - when `false`, does not warn
      when atoms, keywords or calls have unnecessary quotes on
      them. Defaults to `true`.

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
  string. It is required to return either `{:ok, term}`, where term is
  an atom. It is possible to return something else than an atom,
  however, in that case the AST is no longer "valid" in that it cannot
  be used to compile or evaluate Elixir code. A use case for this is
  if you want to use the Elixir parser in a user-facing situation, but
  you don't want to exhaust the atom table.

  The atom encoder is not called for *all* atoms that are present in
  the AST. It won't be invoked for the following atoms:

    * operators (`:+`, `:-`, and so on)

    * syntax keywords (`fn`, `do`, `else`, and so on)

    * atoms containing interpolation (`:"#{1 + 1} is two"`), as these
      atoms are constructed at runtime.

  """
  @spec string_to_quoted(List.Chars.t(), keyword) ::
          {:ok, Macro.t()} | {:error, {line :: pos_integer, term, term}}
  def string_to_quoted(string, opts \\ []) when is_list(opts) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)

    case :elixir.string_to_tokens(to_charlist(string), line, file, opts) do
      {:ok, tokens} ->
        :elixir.tokens_to_quoted(tokens, file, opts)

      {:error, _error_msg} = error ->
        error
    end
  end

  @doc """
  Converts the given string to its quoted form.

  It returns the ast if it succeeds,
  raises an exception otherwise. The exception is a `TokenMissingError`
  in case a token is missing (usually because the expression is incomplete),
  `SyntaxError` otherwise.

  Check `string_to_quoted/2` for options information.
  """
  @spec string_to_quoted!(List.Chars.t(), keyword) :: Macro.t()
  def string_to_quoted!(string, opts \\ []) when is_list(opts) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    :elixir.string_to_quoted!(to_charlist(string), line, file, opts)
  end

  @doc """
  Evals the given file.

  Accepts `relative_to` as an argument to tell where the file is located.

  While `require_file/2` and `compile_file/2` return the loaded modules and their
  bytecode, `eval_file/2` simply evaluates the file contents and returns the
  evaluation result and its bindings (exactly the same return value as `eval_string/3`).
  """
  @spec eval_file(binary, nil | binary) :: {term, binding :: list}
  def eval_file(file, relative_to \\ nil) when is_binary(file) do
    file = find_file(file, relative_to)
    eval_string(File.read!(file), [], file: file, line: 1)
  end

  @deprecated "Use Code.require_file/2 or Code.compile_file/2 instead"
  @doc false
  def load_file(file, relative_to \\ nil) when is_binary(file) do
    file = find_file(file, relative_to)
    :elixir_code_server.call({:acquire, file})
    loaded = :elixir_compiler.file(file, fn _, _ -> :ok end)
    :elixir_code_server.cast({:required, file})
    loaded
  end

  @doc """
  Requires the given `file`.

  Accepts `relative_to` as an argument to tell where the file is located.
  If the file was already required, `require_file/2` doesn't do anything and
  returns `nil`.

  Notice that if `require_file/2` is invoked by different processes concurrently,
  the first process to invoke `require_file/2` acquires a lock and the remaining
  ones will block until the file is available. This means that if `require_file/2`
  is called more than once with a given file, that file will be compiled only once.
  The first process to call `require_file/2` will get the list of loaded modules,
  others will get `nil`.

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
    file = find_file(file, relative_to)

    case :elixir_code_server.call({:acquire, file}) do
      :required ->
        nil

      :proceed ->
        loaded = :elixir_compiler.file(file, fn _, _ -> :ok end)
        :elixir_code_server.cast({:required, file})
        loaded
    end
  end

  @doc """
  Gets all compilation options from the code server.

  To get invidual options, see `get_compiler_option/1`.
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

  To store invidual options, see `put_compiler_option/2`.
  For a description of all options, see `put_compiler_option/2`.

  ## Examples

      Code.compiler_options()
      #=> %{debug_info: true, docs: true, ...}

  """
  @spec compiler_options(Enumerable.t()) :: %{optional(atom) => boolean}
  def compiler_options(opts) do
    for {key, value} <- opts, into: %{} do
      put_compiler_option(key, value)
      {key, value}
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

  These options are global since they are stored by Elixir's code server.

  Available options are:

    * `:docs` - when `true`, retain documentation in the compiled module.
      Defaults to `true`.

    * `:debug_info` - when `true`, retain debug information in the compiled
      module. This allows a developer to reconstruct the original source
      code. Defaults to `false`.

    * `:ignore_module_conflict` - when `true`, override modules that were
      already defined without raising errors. Defaults to `false`.

    * `:relative_paths` - when `true`, use relative paths in quoted nodes,
      warnings and errors generated by the compiler. Note disabling this option
      won't affect runtime warnings and errors. Defaults to `true`.

    * `:warnings_as_errors` - causes compilation to fail when warnings are
      generated. Defaults to `false`.

    * `:no_warn_undefined` - list of modules and `{Mod, fun, arity}` tuples
      that will not emit warnings that the module or function does not exist
      at compilation time. This can be useful when doing dynamic compilation.
      Defaults to `[]`.

    * `:tracers` - a list of tracers (modules) to be used during compilation. See the
      module docs for more information. Defaults to `[]`.

    * `:parser_options` - a keyword list of options to be given to the parser
      when compiling files. It accepts the same options as `string_to_quoted/2`
      (except by the options that change the AST itself). This can be used in
      combination with the tracer to retrieve localized information about
      events happening during compilation. Defaults to `[]`.

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

  def put_compiler_option(key, _value) do
    raise "unknown compiler option: #{inspect(key)}"
  end

  @doc """
  Purge compiler modules.

  The compiler utilizes temporary modules to compile code. For example,
  `elixir_compiler_1`, `elixir_compiler_2`, etc. In case the compiled code
  stores references to anonymous functions or similar, the Elixir compiler
  may be unable to reclaim those modules, keeping an unnecessary amount of
  code in memory and eventually leading to modules such as `elixir_compiler_12345`.

  This function purges all modules currently kept by the compiler, allowing
  old compiler module names to be reused. If there are any processes running
  any code from such modules, they will be terminated too.

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
  given as second argument which will be used for reporting warnings
  and errors.

  **Warning**: `string` can be any Elixir code and code can be executed with
  the same privileges as the Erlang VM: this means that such code could
  compromise the machine (for example by executing system commands).
  Don't use `compile_string/2` with untrusted input (such as strings coming
  from the network).
  """
  @spec compile_string(List.Chars.t(), binary) :: [{module, binary}]
  def compile_string(string, file \\ "nofile") when is_binary(file) do
    :elixir_compiler.string(to_charlist(string), file, fn _, _ -> :ok end)
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
    :elixir_compiler.quoted(quoted, file, fn _, _ -> :ok end)
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
    :elixir_compiler.file(find_file(file, relative_to), fn _, _ -> :ok end)
  end

  @doc """
  Ensures the given module is loaded.

  If the module is already loaded, this works as no-op. If the module
  was not yet loaded, it tries to load it.

  If it succeeds in loading the module, it returns `{:module, module}`.
  If not, returns `{:error, reason}` with the error reason.

  ## Code loading on the Erlang VM

  Erlang has two modes to load code: interactive and embedded.

  By default, the Erlang VM runs in interactive mode, where modules
  are loaded as needed. In embedded mode the opposite happens, as all
  modules need to be loaded upfront or explicitly.

  Therefore, this function is used to check if a module is loaded
  before using it and allows one to react accordingly. For example, the `URI`
  module uses this function to check if a specific parser exists for a given
  URI scheme.

  ## `ensure_compiled/1`

  Elixir also contains an `ensure_compiled/1` function that is a
  superset of `ensure_loaded/1`.

  Since Elixir's compilation happens in parallel, in some situations
  you may need to use a module that was not yet compiled, therefore
  it can't even be loaded.

  When invoked, `ensure_compiled/1` halts the compilation of the caller
  until the module given to `ensure_compiled/1` becomes available or
  all files for the current project have been compiled. If compilation
  finishes and the module is not available, an error tuple is returned.

  `ensure_compiled/1` does not apply to dependencies, as dependencies
  must be compiled upfront.

  In most cases, `ensure_loaded/1` is enough. `ensure_compiled/1`
  must be used in rare cases, usually involving macros that need to
  invoke a module for callback information.

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

      iex> Code.ensure_loaded?(Atom)
      true

  """
  @spec ensure_loaded?(module) :: boolean
  def ensure_loaded?(module) when is_atom(module) do
    match?({:module, ^module}, ensure_loaded(module))
  end

  @doc """
  Ensures the given module is compiled and loaded.

  If the module is already loaded, it works as no-op. If the module was
  not loaded yet, it checks if it needs to be compiled first and then
  tries to load it.

  If it succeeds in loading the module, it returns `{:module, module}`.
  If not, returns `{:error, reason}` with the error reason.

  If the module being checked is currently in a compiler deadlock,
  this functions returns `{:error, :nofile}`.

  Check `ensure_loaded/1` for more information on module loading
  and when to use `ensure_loaded/1` or `ensure_compiled/1`.
  """
  @spec ensure_compiled(module) ::
          {:module, module} | {:error, :embedded | :badfile | :nofile | :on_load_failure}
  def ensure_compiled(module) when is_atom(module) do
    case :code.ensure_loaded(module) do
      {:error, :nofile} = error ->
        if is_pid(:erlang.get(:elixir_compiler_pid)) do
          case Kernel.ErrorHandler.ensure_compiled(module, :module, :soft) do
            :found -> {:module, module}
            :not_found -> error
          end
        else
          error
        end

      other ->
        other
    end
  end

  @doc """
  Ensures the given module is compiled and loaded.

  Similar to `ensure_compiled/1`, but returns `true` if the module
  is already loaded or was successfully loaded and compiled.
  Returns `false` otherwise.
  """
  @spec ensure_compiled?(module) :: boolean
  def ensure_compiled?(module) when is_atom(module) do
    match?({:module, ^module}, ensure_compiled(module))
  end

  @doc ~S"""
  Returns the docs for the given module or path to `.beam` file.

  When given a module name, it finds its BEAM code and reads the docs from it.

  When given a path to a `.beam` file, it will load the docs directly from that
  file.

  It returns the term stored in the documentation chunk in the format defined by
  [EEP 48](http://erlang.org/eep/eeps/eep-0048.html) or `{:error, reason}` if
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
          | {:error, :module_not_found | :chunk_not_found | {:invalid_chunk, binary}}
        when annotation: :erl_anno.anno(),
             beam_language: :elixir | :erlang | :lfe | :alpaca | atom(),
             doc_content: %{required(binary) => binary} | :none | :hidden,
             doc_element:
               {{kind :: atom, function_name :: atom, arity}, annotation, signature, doc_content,
                metadata},
             format: binary,
             signature: [binary],
             metadata: map
  def fetch_docs(module_or_path)

  def fetch_docs(module) when is_atom(module) do
    case :code.get_object_code(module) do
      {_module, bin, _beam_path} -> do_fetch_docs(bin)
      :error -> {:error, :module_not_found}
    end
  end

  def fetch_docs(path) when is_binary(path) do
    do_fetch_docs(String.to_charlist(path))
  end

  @docs_chunk 'Docs'

  defp do_fetch_docs(bin_or_path) do
    case :beam_lib.chunks(bin_or_path, [@docs_chunk]) do
      {:ok, {_module, [{@docs_chunk, bin}]}} ->
        try do
          :erlang.binary_to_term(bin)
        rescue
          _ -> {:error, {:invalid_chunk, bin}}
        end

      {:error, :beam_lib, {:missing_chunk, _, @docs_chunk}} ->
        {:error, :chunk_not_found}
    end
  end

  @doc ~S"""
  Deprecated function to retrieve old documentation format.

  Elixir v1.7 adopts [EEP 48](http://erlang.org/eep/eeps/eep-0048.html)
  which is a new documentation format meant to be shared across all
  BEAM languages. The old format, used by `Code.get_docs/2`, is no
  longer available, and therefore this function always returns `nil`.
  Use `Code.fetch_docs/1` instead.
  """
  @deprecated "Code.get_docs/2 always returns nil as its outdated documentation is no longer stored on BEAM files. Use Code.fetch_docs/1 instead"
  @spec get_docs(module, :moduledoc | :docs | :callback_docs | :type_docs | :all) :: nil
  def get_docs(_module, _kind) do
    nil
  end

  ## Helpers

  # Finds the file given the relative_to path.
  #
  # If the file is found, returns its path in binary, fails otherwise.
  defp find_file(file, relative_to) do
    file =
      if relative_to do
        Path.expand(file, relative_to)
      else
        Path.expand(file)
      end

    if File.regular?(file) do
      file
    else
      raise Code.LoadError, file: file
    end
  end
end
