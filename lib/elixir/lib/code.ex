defmodule Code do
  @moduledoc """
  Utilities for managing code compilation, code evaluation and code loading.

  This module complements Erlang's [`:code` module](http://www.erlang.org/doc/man/code.html)
  to add behaviour which is specific to Elixir. Almost all of the functions in this module
  have global side effects on the behaviour of Elixir.
  """

  @doc """
  Lists all loaded files.

  ## Examples

      Code.require_file("../eex/test/eex_test.exs")
      List.first(Code.loaded_files) =~ "eex_test.exs" #=> true

  """
  def loaded_files do
    :elixir_code_server.call :loaded
  end

  @doc """
  Removes files from the loaded files list.

  The modules defined in the file are not removed;
  calling this function only removes them from the list,
  allowing them to be required again.

  ## Examples

      # Load EEx test code, unload file, check for functions still available
      Code.load_file("../eex/test/eex_test.exs")
      Code.unload_files(Code.loaded_files)
      function_exported?(EExTest.Compiled, :before_compile, 0) #=> true

  """
  def unload_files(files) do
    :elixir_code_server.cast {:unload_files, files}
  end

  @doc """
  Appends a path to the end of the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code.

  The path is expanded with `Path.expand/1` before being appended.
  If this path does not exist, an error is returned.

  ## Examples

      Code.append_path(".") #=> true

      Code.append_path("/does_not_exist") #=> {:error, :bad_directory}

  """
  def append_path(path) do
    :code.add_pathz(to_charlist(Path.expand path))
  end

  @doc """
  Prepends a path to the beginning of the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for finding
  module code.

  The path is expanded with `Path.expand/1` before being prepended.
  If this path does not exist, an error is returned.

  ## Examples

      Code.prepend_path(".") #=> true

      Code.prepend_path("/does_not_exist") #=> {:error, :bad_directory}

  """
  def prepend_path(path) do
    :code.add_patha(to_charlist(Path.expand path))
  end

  @doc """
  Deletes a path from the Erlang VM code path list. This is the list of
  directories the Erlang VM uses for finding module code.

  The path is expanded with `Path.expand/1` before being deleted. If the
  path does not exist it returns `false`.

  ## Examples

      Code.prepend_path(".")
      Code.delete_path(".") #=> true

      Code.delete_path("/does_not_exist") #=> false

  """
  def delete_path(path) do
    :code.del_path(to_charlist(Path.expand path))
  end

  @doc """
  Evaluates the contents given by `string`.

  The `binding` argument is a keyword list of variable bindings.
  The `opts` argument is a keyword list of environment options.

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
  values. For example, setting `:requires` to `[]`, will no longer
  automatically require the `Kernel` module; in the same way setting
  `:macros` will no longer auto-import `Kernel` macros like `if/2`, `case/2`,
  etc.

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
  def eval_string(string, binding \\ [], opts \\ [])

  def eval_string(string, binding, %Macro.Env{} = env) do
    {value, binding, _env, _scope} = :elixir.eval to_charlist(string), binding, Map.to_list(env)
    {value, binding}
  end

  def eval_string(string, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    {value, binding, _env, _scope} = :elixir.eval to_charlist(string), binding, opts
    {value, binding}
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
  def eval_quoted(quoted, binding \\ [], opts \\ [])

  def eval_quoted(quoted, binding, %Macro.Env{} = env) do
    {value, binding, _env, _scope} = :elixir.eval_quoted quoted, binding, Map.to_list(env)
    {value, binding}
  end

  def eval_quoted(quoted, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    {value, binding, _env, _scope} = :elixir.eval_quoted quoted, binding, opts
    {value, binding}
  end

  defp validate_eval_opts(opts) do
    if f = opts[:functions], do: validate_imports(:functions, f)
    if m = opts[:macros],    do: validate_imports(:macros, m)
    if a = opts[:aliases],   do: validate_aliases(:aliases, a)
    if r = opts[:requires],  do: validate_requires(:requires, r)
  end

  defp validate_requires(kind, requires) do
    valid = is_list(requires) and Enum.all?(requires, &is_atom(&1))

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [module]"
    end
  end

  defp validate_aliases(kind, aliases) do
    valid = is_list(aliases) and Enum.all?(aliases, fn {k, v} ->
      is_atom(k) and is_atom(v)
    end)

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [{module, module}]"
    end
  end

  defp validate_imports(kind, imports) do
    valid = is_list(imports) and Enum.all?(imports, fn {k, v} ->
      is_atom(k) and is_list(v) and Enum.all?(v, fn {name, arity} ->
        is_atom(name) and is_integer(arity)
      end)
    end)

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [{module, [{name, arity}]}]"
    end
  end

  @doc """
  Converts the given string to its quoted form.

  Returns `{:ok, quoted_form}`
  if it succeeds, `{:error, {line, error, token}}` otherwise.

  ## Options

    * `:file` - the filename to be used in stacktraces
      and the file reported in the `__ENV__/0` macro

    * `:line` - the line reported in the `__ENV__/0` macro

    * `:existing_atoms_only` - when `true`, raises an error
      when non-existing atoms are found by the tokenizer

  ## Macro.to_string/2

  The opposite of converting a string to its quoted form is
  `Macro.to_string/2`, which converts a quoted form to a string/binary
  representation.
  """
  def string_to_quoted(string, opts \\ []) when is_list(opts) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    :elixir.string_to_quoted(to_charlist(string), line, file, opts)
  end

  @doc """
  Converts the given string to its quoted form.

  It returns the ast if it succeeds,
  raises an exception otherwise. The exception is a `TokenMissingError`
  in case a token is missing (usually because the expression is incomplete),
  `SyntaxError` otherwise.

  Check `string_to_quoted/2` for options information.
  """
  def string_to_quoted!(string, opts \\ []) when is_list(opts) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    :elixir.string_to_quoted!(to_charlist(string), line, file, opts)
  end

  @doc """
  Evals the given file.

  Accepts `relative_to` as an argument to tell where the file is located.

  While `load_file` loads a file and returns the loaded modules and their
  byte code, `eval_file` simply evaluates the file contents and returns the
  evaluation result and its bindings.
  """
  def eval_file(file, relative_to \\ nil) do
    file = find_file(file, relative_to)
    eval_string File.read!(file), [], [file: file, line: 1]
  end

  @doc """
  Loads the given file.

  Accepts `relative_to` as an argument to tell where the file is located.
  If the file was already required/loaded, loads it again.

  It returns a list of tuples `{ModuleName, <<byte_code>>}`, one tuple for
  each module defined in the file.

  Notice that if `load_file` is invoked by different processes concurrently,
  the target file will be loaded concurrently many times. Check `require_file/2`
  if you don't want a file to be loaded concurrently.

  ## Examples

      Code.load_file("eex_test.exs", "../eex/test") |> List.first
      #=> {EExTest.Compiled, <<70, 79, 82, 49, ...>>}

  """
  def load_file(file, relative_to \\ nil) when is_binary(file) do
    file = find_file(file, relative_to)
    :elixir_code_server.call {:acquire, file}
    loaded = :elixir_compiler.file file
    :elixir_code_server.cast {:loaded, file}
    loaded
  end

  @doc """
  Requires the given `file`.

  Accepts `relative_to` as an argument to tell where the file is located.
  The return value is the same as that of `load_file/2`. If the file was already
  required/loaded, doesn't do anything and returns `nil`.

  Notice that if `require_file` is invoked by different processes concurrently,
  the first process to invoke `require_file` acquires a lock and the remaining
  ones will block until the file is available. I.e. if `require_file` is called
  N times with a given file, it will be loaded only once. The first process to
  call `require_file` will get the list of loaded modules, others will get `nil`.

  Check `load_file/2` if you want a file to be loaded multiple times. See also
  `unload_files/1`

  ## Examples

  If the code is already loaded, it returns `nil`:

      Code.require_file("eex_test.exs", "../eex/test") #=> nil

  If the code is not loaded yet, it returns the same as `load_file/2`:

      Code.require_file("eex_test.exs", "../eex/test") |> List.first
      #=> {EExTest.Compiled, <<70, 79, 82, 49, ...>>}

  """
  def require_file(file, relative_to \\ nil) when is_binary(file) do
    file = find_file(file, relative_to)

    case :elixir_code_server.call({:acquire, file}) do
      :loaded  ->
        nil
      {:queued, ref}  ->
        receive do {:elixir_code_server, ^ref, :loaded} -> nil end
      :proceed ->
        loaded = :elixir_compiler.file file
        :elixir_code_server.cast {:loaded, file}
        loaded
    end
  end

  @doc """
  Gets the compilation options from the code server.

  Check `compiler_options/1` for more information.

  ## Examples

      Code.compiler_options
      #=> %{debug_info: true, docs: true,
            warnings_as_errors: false, ignore_module_conflict: false}

  """
  def compiler_options do
    :elixir_config.get :compiler_options
  end

  @doc """
  Returns a list with the available compiler options.

  See `Code.compiler_options/1` for more info.

  ## Examples

      iex> Code.available_compiler_options
      [:docs, :debug_info, :ignore_module_conflict, :relative_paths, :warnings_as_errors]

  """
  def available_compiler_options do
    [:docs, :debug_info, :ignore_module_conflict, :relative_paths, :warnings_as_errors]
  end

  @doc """
  Sets compilation options.

  These options are global since they are stored by Elixir's Code Server.

  Available options are:

    * `:docs` - when `true`, retain documentation in the compiled module,
      `true` by default

    * `:debug_info` - when `true`, retain debug information in the compiled
      module; this allows a developer to reconstruct the original source
      code, `false` by default

    * `:ignore_module_conflict` - when `true`, override modules that were
      already defined without raising errors, `false` by default

    * `:relative_paths` - when `true`, use relative paths in quoted nodes,
      warnings and errors generated by the compiler, `true` by default.
      Note disabling this option won't affect runtime warnings and errors.

    * `:warnings_as_errors` - causes compilation to fail when warnings are
      generated

  It returns the new list of compiler options.

  ## Examples

      Code.compiler_options(debug_info: true)
      #=> %{debug_info: true, docs: true,
            warnings_as_errors: false, ignore_module_conflict: false}

  """
  def compiler_options(opts) do
    available = available_compiler_options()

    Enum.each(opts, fn({key, value}) ->
      cond do
        not key in available ->
          raise "unknown compiler option: #{inspect(key)}"
        not is_boolean(value) ->
          raise "compiler option #{inspect(key)} should be a boolean, got: #{inspect(value)}"
        true ->
          :ok
      end
    end)

    :elixir_config.update :compiler_options, &Enum.into(opts, &1)
  end

  @doc """
  Compiles the given string.

  Returns a list of tuples where the first element is the module name
  and the second one is its byte code (as a binary).

  For compiling many files at once, check `Kernel.ParallelCompiler.files/2`.
  """
  def compile_string(string, file \\ "nofile") when is_binary(file) do
    :elixir_compiler.string to_charlist(string), file
  end

  @doc """
  Compiles the quoted expression.

  Returns a list of tuples where the first element is the module name and
  the second one is its byte code (as a binary).
  """
  def compile_quoted(quoted, file \\ "nofile") when is_binary(file) do
    :elixir_compiler.quoted quoted, file
  end

  @doc """
  Ensures the given module is loaded.

  If the module is already loaded, this works as no-op. If the module
  was not yet loaded, it tries to load it.

  If it succeeds loading the module, it returns `{:module, module}`.
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

  ## Code.ensure_compiled/1

  Elixir also contains an `ensure_compiled/1` function that is a
  superset of `ensure_loaded/1`.

  Since Elixir's compilation happens in parallel, in some situations
  you may need to use a module that was not yet compiled, therefore
  it can't even be loaded.

  `ensure_compiled/1` halts the current process until the
  module we are depending on is available.

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
  def ensure_loaded?(module) when is_atom(module) do
    match?({:module, ^module}, ensure_loaded(module))
  end

  @doc """
  Ensures the given module is compiled and loaded.

  If the module is already loaded, it works as no-op. If the module was
  not loaded yet, it checks if it needs to be compiled first and then
  tries to load it.

  If it succeeds loading the module, it returns `{:module, module}`.
  If not, returns `{:error, reason}` with the error reason.

  Check `ensure_loaded/1` for more information on module loading
  and when to use `ensure_loaded/1` or `ensure_compiled/1`.
  """
  @spec ensure_compiled(module) ::
        {:module, module} | {:error, :embedded | :badfile | :nofile | :on_load_failure}
  def ensure_compiled(module) when is_atom(module) do
    case :code.ensure_loaded(module) do
      {:error, :nofile} = error ->
        if is_pid(:erlang.get(:elixir_compiler_pid)) and
           Kernel.ErrorHandler.ensure_compiled(module, :module) do
          {:module, module}
        else
          error
        end
      other -> other
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
  Returns the docs for the given module.

  When given a module name, it finds its BEAM code and reads the docs from it.

  When given a path to a .beam file, it will load the docs directly from that
  file.

  The return value depends on the `kind` value:

    * `:docs` - list of all docstrings attached to functions and macros
      using the `@doc` attribute

    * `:moduledoc` - tuple `{<line>, <doc>}` where `line` is the line on
      which module definition starts and `doc` is the string
      attached to the module using the `@moduledoc` attribute

    * `:callback_docs` - list of all docstrings attached to
      `@callbacks` using the `@doc` attribute

    * `:type_docs` - list of all docstrings attached to
      `@type` callbacks using the `@typedoc` attribute

    * `:all` - a keyword list with `:docs` and `:moduledoc`, `:callback_docs`,
      and `:type_docs`.

  If the module cannot be found, it returns `nil`.

  ## Examples

      # Get the module documentation
      iex> {_line, text} = Code.get_docs(Atom, :moduledoc)
      iex> String.split(text, "\n") |> Enum.at(0)
      "Convenience functions for working with atoms."

      # Module doesn't exist
      iex> Code.get_docs(ModuleNotGood, :all)
      nil

  """
  @doc_kinds [:docs, :moduledoc, :callback_docs, :type_docs, :all]

  def get_docs(module, kind) when is_atom(module) and kind in @doc_kinds do
    case :code.get_object_code(module) do
      {_module, bin, _beam_path} ->
        do_get_docs(bin, kind)

      :error -> nil
    end
  end

  def get_docs(binpath, kind) when is_binary(binpath) and kind in @doc_kinds do
    do_get_docs(String.to_charlist(binpath), kind)
  end

  @docs_chunk 'ExDc'

  defp do_get_docs(bin_or_path, kind) do
    case :beam_lib.chunks(bin_or_path, [@docs_chunk]) do
      {:ok, {_module, [{@docs_chunk, bin}]}} ->
        lookup_docs(:erlang.binary_to_term(bin), kind)

      {:error, :beam_lib, {:missing_chunk, _, @docs_chunk}} -> nil
    end
  end

  defp lookup_docs({:elixir_docs_v1, docs}, kind),
    do: do_lookup_docs(docs, kind)

  # unsupported chunk version
  defp lookup_docs(_, _), do: nil

  defp do_lookup_docs(docs, :all), do: docs
  defp do_lookup_docs(docs, kind),
    do: Keyword.get(docs, kind)

  ## Helpers

  # Finds the file given the relative_to path.
  #
  # If the file is found, returns its path in binary, fails otherwise.
  defp find_file(file, relative_to) do
    file = if relative_to do
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
