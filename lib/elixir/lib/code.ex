defmodule Code do
  defexception LoadError, file: nil do
    def message(exception) do
      "could not load #{exception.file}"
    end
  end

  @moduledoc """
  The Code module is responsible to manage code compilation,
  code evaluation and code loading.

  It complements (Erlang's code module)[1] to add behavior
  which is specific to Elixir.

    [1]: (www.erlang.org/doc/man/code.html)

  """

  @doc """
  Returns all the loaded files.
  """
  def loaded_files do
    server_call :loaded
  end

  @doc """
  Removes the given files from the loaded files list.
  The modules defined in the file are not removed,
  calling this function only removes it from the list,
  allowing it to be required again.
  """
  def unload_files(files) do
    server_cast { :unload_files, files }
  end

  @doc """
  Appends a path to Erlang VM code path.
  The path is expanded with `File.expand_path` before added.
  """
  def append_path(path) do
    :code.add_pathz(File.expand_path to_char_list(path))
  end

  @doc """
  Prepends a path to Erlang VM code path.
  The path is expanded with `File.expand_path` before added.
  """
  def prepend_path(path) do
    :code.add_patha(File.expand_path to_char_list(path))
  end

  @doc """
  Deletes a path from Erlang VM code path.
  The path is expanded with `File.expand_path` before deleted.
  """
  def delete_path(path) do
    :code.del_path(File.expand_path to_char_list(path))
  end

  @doc """
  Evalutes the contents given by string. The second argument is the
  binding (which should be a keyword) followed by a keyword list of
  environment options. Those options can be:

  * `:file` - the file to be considered in the evaluation
  * `:line` - the line the script starts
  * `:aliases` - a list of tuples with the alias and its target
  * `:requires` - a list of modules required
  * `:functions` - a list of tuples where the first element is a module
    and the second a list of imported function names and arity
  * `:macros` - a list of tuples where the first element is a module
    and the second a list of imported macro names and arity
  * `:delegate_locals_to` - delegate local calls to the given module,
    the default is to not delegate

  ## Examples

      Code.eval "a + b", [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  For convenience, you can my pass `__ENV__` as argument and
  all imports, requires and aliases will be automatically carried
  over:

      Code.eval "a + b", [a: 1, b: 2], __ENV__
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  """
  def eval(string, binding // [], opts // [])

  def eval(string, binding, Macro.Env[] = env) do
    eval(string, binding, env.to_keywords)
  end

  def eval(string, binding, opts) do
    { value, binding, _scope } =
      :elixir.eval :unicode.characters_to_list(string), binding, opts
    { value, binding }
  end

  @doc """
  Evalutes the quoted contents.

  This function accepts a list of environment options.
  Check `Code.eval` for more information.

  ## Examples

      contents = quote hygiene: false, do: a + b

      Code.eval_quoted contents, [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  For convenience, you can my pass `__ENV__` as argument and
  all options will be automatically extracted from the environment:

      Code.eval_quoted contents, [a: 1, b: 2], __ENV__
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  """
  def eval_quoted(quoted, binding // [], opts // [])

  def eval_quoted(quoted, binding, Macro.Env[] = env) do
    eval_quoted(quoted, binding, env.to_keywords)
  end

  def eval_quoted(quoted, binding, opts) do
    { value, binding, _scope } =
      :elixir.eval_quoted [quoted], binding, opts
    { value, binding }
  end

  @doc """
  Converts the given string to AST. It returns { :ok, ast }
  if it succeeds, { :error, { line, error, token } } otherwise.

  ## Options

  * `:file` - The filename to be used in stacktraces
    and the file reported in the __ENV__ variable.

  * `:line` - The line reported in the __ENV__ variable.

  * `:existing_atoms_only` - When true, raises an error
    when non-existing atoms are found by the tokenizer.

  """
  def string_to_ast(string, opts // []) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    res  = :elixir_translator.forms(:unicode.characters_to_list(string), line, file, opts)

    case res do
      { :ok, ast } -> { :ok, unpack_ast(line, ast) }
      _ -> res
    end
  end

  @doc """
  Converts the given string to AST. It returns the ast if it succeeds,
  raises an exception otherwise. The exception is a TokenMissingError
  in case a token is missing (usually because the expression is incomplete),
  SyntaxError otherwise.

  Check `Code.string_to_ast/2` for options information.
  """
  def string_to_ast!(string, opts // []) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    res  = :elixir_translator.forms!(:unicode.characters_to_list(string), line, file, opts)
    unpack_ast(line, res)
  end

  defp unpack_ast(_line, []),                              do: nil
  defp unpack_ast(_line, [forms]) when not is_list(forms), do: forms
  defp unpack_ast(line, forms),                            do: { :__block__, line, forms }

  @doc """
  Loads the given `file`. Accepts `relative_to` as an argument
  to tell where the file is located. If the file was already
  required/loaded, loads it again. It returns all the modules
  defined in the file.

  Notice that if `load_file` is invoked by different processes
  concurrently, the target file will be invoked concurrently
  in many times. I.e. if `load_file` is called N times with
  a given file, the given file will be loaded N times. Check
  `require_file` if you don't want a file to be loaded concurrently.
  """
  def load_file(file, relative_to // nil) when is_binary(file) do
    file = find_file(file, relative_to)
    server_call { :acquire, file }
    loaded = :elixir_compiler.file file
    server_cast { :loaded, file }
    loaded
  end

  @doc """
  Requires the given `file`. Accepts `relative_to` as an argument
  to tell where the file is located. If the file was already
  required/loaded, loads it again. It returns all the modules
  defined in the file.

  Notice that if `require_file` is invoked by different processes
  concurrently, the first process to invoke `require_file` acquires
  a lock and the remaining ones will block until the file is
  available. I.e. if `require_file` is called N times with a given
  file, the given file will be loaded only once. Check `load_file`
  if you want a file to be loaded concurrently.
  """
  def require_file(file, relative_to // nil) when is_binary(file) do
    file = find_file(file, relative_to)

    case server_call({ :acquire, file }) do
      :loaded  ->
        nil
      { :queued, ref }  ->
        receive do { :elixir_code_server, ^ref, :loaded } -> nil end
      :proceed ->
        loaded = :elixir_compiler.file file
        server_cast { :loaded, file }
        loaded
    end
  end

  @doc """
  Loads the compilation options from the code server.
  Check compiler_options/1 for more information.
  """
  def compiler_options do
    server_call :compiler_options
  end

  @doc """
  Sets compilation options. Those options are global
  since they are stored by Elixir's Code Server.

  Available options are:

  * docs       - when true, retain documentation in the compiled module.
                 True by default;
  * debug_info - when true, retain debug information in the compiled module.
                 This allows a developer to reconstruct the original source
                 code, for such reasons, false by default;
  * ignore_module_conflict - when true, override modules that were already defined
                             without raising errors, false by default;

  """
  def compiler_options(opts) do
    server_call { :compiler_options, opts }
  end

  @doc """
  Compiles the given string and returns a list of tuples where
  the first element is the module name and the second one is its
  binary.

  For compiling many files at once, check `Kernel.ParallelCompiler`.
  """
  def compile_string(string, file // "nofile") when is_binary(file) do
    :elixir_compiler.string :unicode.characters_to_list(string), to_binary(file)
  end

  @doc """
  Ensures the given module is loaded. If the module is already
  loaded, it works as no-op. If the module was not loaded yet,
  it tries to load it.

  If it succeeds loading the module anyhow, it returns
  `{ :module, module }`. If not, returns `{ :error, reason }` with
  the error reason.

  ## Code loading on the Erlang VM

  Erlang has two modes to load code: interactive and embedded.

  By default, the Erlang VM runs on interactive mode, where modules
  are loaded as needed. In embedded mode the opposite happens, as all
  modules need to be loaded upfront or explicitly.

  Therefore, this function is useful to check if a module is loaded
  before using it and react accordingly. For example, the `URI` module
  uses this function to check if a specific parser exists for a given
  URI scheme.

  ## Code.ensure_compiled

  Elixir also contains an `ensure_compiled/1` function that is a
  superset of `ensure_loaded/1`.

  Since Elixir's compilation happens in parallel, in some situations
  you may need to use a module but it was not compiled yet, therefore
  it can't even be loaded.

  `ensure_compiled/1` puts a halt in the current process until the
  module we are depending on is available.

  In most of the cases, `ensure_loaded` is enough. `ensure_compiled`
  must be used just in same rare conditions, usually involving macros
  that needs to invoke a module for callback information.
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
  loaded yet, it checks if it needs to be compiled first and just
  then tries to load it.

  If it succeeds loading the module anyhow, it returns
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
    file = to_binary(file)

    file = if relative_to do
      File.expand_path(file, relative_to)
    else
      File.expand_path(file)
    end

    if File.regular?(file) do
      file
    else
      raise LoadError, file: file
    end
  end

  defp server_call(args) do
    :gen_server.call(:elixir_code_server, args)
  end

  defp server_cast(args) do
    :gen_server.cast(:elixir_code_server, args)
  end
end
