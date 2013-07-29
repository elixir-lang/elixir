defmodule Code do
  defexception LoadError, file: nil do
    def message(exception) do
      "could not load #{exception.file}"
    end
  end

  @moduledoc """
  The Code module is responsible for managing code compilation,
  code evaluation and code loading.

  It complements [Erlang's code module](http://www.erlang.org/doc/man/code.html)
  to add behavior which is specific to Elixir.

  """

  @doc """
  Returns all loaded files.
  """
  def loaded_files do
    :elixir_code_server.call :loaded
  end

  @doc """
  Removes the given files from the loaded files list.
  The modules defined in the file are not removed,
  calling this function only removes them from the list,
  allowing them to be required again.
  """
  def unload_files(files) do
    :elixir_code_server.cast { :unload_files, files }
  end

  @doc """
  Appends a path to the Erlang VM code path.
  The path is expanded with `Path.expand/1` before being appended.
  """
  def append_path(path) do
    :code.add_pathz(Path.expand to_char_list(path))
  end

  @doc """
  Prepends a path to the Erlang VM code path.
  The path is expanded with `Path.expand/1` before being prepended.
  """
  def prepend_path(path) do
    :code.add_patha(Path.expand to_char_list(path))
  end

  @doc """
  Deletes a path from the Erlang VM code path.
  The path is expanded with `Path.expand/1` before being deleted.
  """
  def delete_path(path) do
    :code.del_path(Path.expand to_char_list(path))
  end

  @doc """
  Evaluates the contents given by `string`. The second argument is 
  a keyword list of variable bindings, followed by a keyword list of
  environment options. Those options can be:

  * `:file` - the file to be considered in the evaluation
  * `:line` - the line on which the script starts
  * `:delegate_locals_to` - delegate local calls to the given module,
    the default is to not delegate

  Additionally, the following scope values can be configured:

  * `:aliases` - a list of tuples with the alias and its target
  * `:requires` - a list of modules required
  * `:functions` - a list of tuples where the first element is a module
    and the second a list of imported function names and arity. The list
    of function names and arity must be sorted
  * `:macros` - a list of tuples where the first element is a module
    and the second a list of imported macro names and arity. The list
    of function names and arity must be sorted

  Notice that setting any of the values above overrides Elixir's default
  values. For example, setting `:requires` to `[]`, will no longer
  automatically require the `Kernel` module; in the same way setting
  `:macros` will no longer auto-import `Kernel` macros like `if`, `case`,
  etc.

  Returns a tuple of the form `{ value, binding }`,
  where `value` is the the value returned from evaluating `string`; `binding`
  is a keyword list with the value of all variable bindings after evaluating
  `string`. If an error occurs while evaluating `string` an exception will be raised.

  ## Examples

      iex> Code.eval_string("a + b", [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      {3, [a: 1, b: 2]}

      iex> Code.eval_string("c = a + b", [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2, c: 3]}

      iex> Code.eval_string("a = a + b", [a: 1, b: 2])          
      {3, [a: 3, b: 2]}

  For convenience, you can pass `__ENV__` as the `opts` argument and
  all imports, requires and aliases defined in the current environment
  will be automatically carried over:

      iex> Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2]}

  """
  def eval_string(string, binding // [], opts // [])

  def eval_string(string, binding, Macro.Env[] = env) do
    do_eval_string(string, binding, env.to_keywords)
  end

  def eval_string(string, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    do_eval_string(string, binding, opts)
  end

  defp do_eval_string(string, binding, opts) when is_list(binding) do
    { value, binding, _scope } =
      :elixir.eval :unicode.characters_to_list(string), binding, opts
    { value, binding }
  end

  @doc """
  Evaluates the quoted contents.

  See `eval_string/3` for a description of arguments and return values.

  ## Examples

      iex> contents = quote(hygiene: [vars: false], do: a + b)
      ...> Code.eval_quoted(contents, [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      {3, [a: 1, b: 2]}

  For convenience, you can pass `__ENV__` as the `opts` argument and
  all options will be automatically extracted from the current environment:

      iex> contents = quote(hygiene: [vars: false], do: a + b)
      ...> Code.eval_quoted(contents, [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2]}

  """
  def eval_quoted(quoted, binding // [], opts // [])

  def eval_quoted(quoted, binding, Macro.Env[] = env) do
    do_eval_quoted(quoted, binding, env.to_keywords)
  end

  def eval_quoted(quoted, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    do_eval_quoted(quoted, binding, opts)
  end

  defp do_eval_quoted(quoted, binding, opts) when is_list(binding) do
    { value, binding, _scope } =
      :elixir.eval_quoted [quoted], binding, opts
    { value, binding }
  end

  defp validate_eval_opts(opts) do
    if f = opts[:functions], do: validate_imports(:functions, f)
    if m = opts[:macros],    do: validate_imports(:macros, m)
    if a = opts[:aliases],   do: validate_aliases(:aliases, a)
    if r = opts[:requires],  do: validate_requires(:requires, r)
  end

  defp validate_requires(kind, requires) do
    valid = is_list(requires) and Enum.all?(requires, is_atom(&1))

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [module]"
    end
  end

  defp validate_aliases(kind, aliases) do
    valid = is_list(aliases) and Enum.all?(aliases, fn { k, v } ->
      is_atom(k) and is_atom(v)
    end)

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [{ module, module }]"
    end
  end

  defp validate_imports(kind, imports) do
    valid = is_list(imports) and Enum.all?(imports, fn { k, v } ->
      is_atom(k) and is_list(v) and Enum.all?(v, fn { name, arity } ->
        is_atom(name) and is_integer(arity)
      end)
    end)

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [{ module, [{ name, arity }] }]"
    end
  end

  @doc """
  Converts the given string to its quoted form. Returns `{ :ok, quoted_form }`
  if it succeeds, `{ :error, { line, error, token } }` otherwise.

  ## Options

  * `:file` - The filename to be used in stacktraces
    and the file reported in the `__ENV__` variable.

  * `:line` - The line reported in the `__ENV__` variable.

  * `:existing_atoms_only` - When `true`, raises an error
    when non-existing atoms are found by the tokenizer.

  ## Macro.to_string/1

  The opposite of converting a string to its quoted form is
  `Macro.to_string/1`, which converts a quoted form to a string/binary
  representation.
  """
  def string_to_quoted(string, opts // []) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    res  = :elixir_translator.forms(:unicode.characters_to_list(string), line, file, opts)

    case res do
      { :ok, forms } -> { :ok, unpack_quote(line, forms) }
      _ -> res
    end
  end

  @doc """
  Converts the given string to its quoted form. It returns the ast if it succeeds,
  raises an exception otherwise. The exception is a `TokenMissingError`
  in case a token is missing (usually because the expression is incomplete),
  `SyntaxError` otherwise.

  Check `string_to_quoted/2` for options information.
  """
  def string_to_quoted!(string, opts // []) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    res  = :elixir_translator.forms!(:unicode.characters_to_list(string), line, file, opts)
    unpack_quote(line, res)
  end

  defp unpack_quote(_line, []),                              do: nil
  defp unpack_quote(_line, [forms]) when not is_list(forms), do: forms
  defp unpack_quote(line, forms),                            do: { :__block__, [line: line], forms }

  @doc """
  Loads the given `file`. Accepts `relative_to` as an argument to tell where
  the file is located. If the file was already required/loaded, loads it again.
  It returns a list of tuples `{ ModuleName, <<byte_code>> }`, one tuple for each
  module defined in the file.

  Notice that if `load_file` is invoked by different processes
  concurrently, the target file will be invoked concurrently
  many times. I.e. if `load_file` is called N times with
  a given file, the given file will be loaded N times. Check
  `require_file/2` if you don't want a file to be loaded concurrently.
  """
  def load_file(file, relative_to // nil) when is_binary(file) do
    file = find_file(file, relative_to)
    :elixir_code_server.call { :acquire, file }
    loaded = :elixir_compiler.file file
    :elixir_code_server.cast { :loaded, file }
    loaded
  end

  @doc """
  Requires the given `file`. Accepts `relative_to` as an argument to tell where
  the file is located. The return value is the same as that of `load_file/2`. If
  the file was already required/loaded, doesn't do anything and returns nil.

  Notice that if `require_file` is invoked by different processes concurrently,
  the first process to invoke `require_file` acquires a lock and the remaining
  ones will block until the file is available. I.e. if `require_file` is called
  N times with a given file, it will be loaded only once. The first process to
  call `require_file` will get the list of loaded modules, others will get `nil`.

  Check `load_file/2` if you want a file to be loaded concurrently.
  """
  def require_file(file, relative_to // nil) when is_binary(file) do
    file = find_file(file, relative_to)

    case :elixir_code_server.call({ :acquire, file }) do
      :loaded  ->
        nil
      { :queued, ref }  ->
        receive do { :elixir_code_server, ^ref, :loaded } -> nil end
      :proceed ->
        loaded = :elixir_compiler.file file
        :elixir_code_server.cast { :loaded, file }
        loaded
    end
  end

  @doc """
  Loads the compilation options from the code server.
  Check `compiler_options/1` for more information.
  """
  def compiler_options do
    :elixir_code_server.call :compiler_options
  end

  @doc """
  Sets compilation options. These options are global
  since they are stored by Elixir's Code Server.

  Available options are:

  * `:docs` - when `true`, retain documentation in the compiled module,
    `true` by default;

  * `:debug_info` - when `true`, retain debug information in the compiled module.
    This allows a developer to reconstruct the original source
    code, for such reasons, `false` by default;

  * `:ignore_module_conflict` - when `true`, override modules that were already defined
    without raising errors, `false` by default;

  * `:warnings_as_errors` - cause compilation to fail when warnings are generated;

  """
  def compiler_options(opts) do
    :elixir_code_server.cast { :compiler_options, opts }
  end

  @doc """
  Compiles the given string and returns a list of tuples where
  the first element is the module name and the second one is its
  binary.

  For compiling many files at once, check `Kernel.ParallelCompiler`.
  """
  def compile_string(string, file // "nofile") when is_binary(file) do
    :elixir_compiler.string :unicode.characters_to_list(string), file
  end

  @doc """
  Compiles the quoted expression and returns a list of tuples where
  the first element is the module name and the second one is its
  binary.
  """
  def compile_quoted(quoted, file // "nofile") when is_binary(file) do
    :elixir_compiler.quoted [quoted], file
  end

  @doc """
  Ensures the given module is loaded. If the module is already
  loaded, it works as no-op. If the module was not yet loaded,
  it tries to load it.

  If it succeeds loading the module, it returns
  `{ :module, module }`. If not, returns `{ :error, reason }` with
  the error reason.

  ## Code loading on the Erlang VM

  Erlang has two modes to load code: interactive and embedded.

  By default, the Erlang VM runs in interactive mode, where modules
  are loaded as needed. In embedded mode the opposite happens, as all
  modules need to be loaded upfront or explicitly.

  Therefore, this function is used to check if a module is loaded
  before using it and allows one to react accordingly. For example, the `URI` 
  module uses this function to check if a specific parser exists for a given
  URI scheme.

  ## Code.ensure_compiled

  Elixir also contains an `ensure_compiled/1` function that is a
  superset of `ensure_loaded/1`.

  Since Elixir's compilation happens in parallel, in some situations
  you may need to use a module but that was not yet compiled, therefore
  it can't even be loaded.

  `ensure_compiled/1` halts the current process until the
  module we are depending on is available.

  In most cases, `ensure_loaded` is enough. `ensure_compiled`
  must be used in some rare cases, usually involving macros
  that need to invoke a module for callback information.
  """
  def ensure_loaded(module) when is_atom(module) do
    :code.ensure_loaded(module)
  end

  @doc """
  Similar to `ensure_loaded/1`, but returns a boolean in case
  it could be ensured or not.
  """
  def ensure_loaded?(module) do
    match?({ :module, ^module }, ensure_loaded(module))
  end

  @doc """
  Ensures the given module is compiled and loaded. If the module
  is already loaded, it works as no-op. If the module was not
  loaded yet, it checks if it needs to be compiled first and 
  then tries to load it.

  If it succeeds loading the module, it returns
  `{ :module, module }`. If not, returns `{ :error, reason }` with
  the error reason.

  Check `ensure_loaded/1` for more information on module loading
  and when to use `ensure_loaded/1` or `ensure_compiled/1`.
  """
  def ensure_compiled(module) when is_atom(module) do
    case :code.ensure_loaded(module) do
      { :error, :nofile } = error ->
        case :erlang.get(:elixir_ensure_compiled) do
          :undefined -> error
          _ ->
            try do
              module.__info__(:module)
              { :module, module }
            rescue
              UndefinedFunctionError -> error
            end
        end
      other -> other
    end
  end

  @doc """
  Similar to `ensure_compiled/1`, but returns a boolean in case
  it could be ensured or not.
  """
  def ensure_compiled?(module) do
    match?({ :module, ^module }, ensure_compiled(module))
  end

  ## Helpers

  # Finds the file given the relative_to path.
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
      raise LoadError, file: file
    end
  end
end
