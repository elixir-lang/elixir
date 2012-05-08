defmodule Code do
  @moduledoc """
  The Code module is responsible to manage code compilation,
  code evaluation and code loading.
  """

  @doc """
  Returns all the loaded files.
  """
  def loaded_files do
    server_call :loaded
  end

  @doc """
  Appends a path to Erlang VM code path.
  """
  def append_path(path) do
    Erlang.code.add_pathz(to_char_list(path))
  end

  @doc """
  Prepends a path to Erlang VM code path.
  """
  def prepend_path(path) do
    Erlang.code.add_patha(to_char_list(path))
  end

  @doc """
  Evalutes the contents given by string. The second argument is the binding
  (which should be a Keyword) followed by a keyword list of options. The
  options can be:

  * `:file` - the file to be considered in the evaluation
  * `:line` - the line the script starts
  * `:delegate_locals_to` - delegate local calls to the given module,
    otherwise functions are evaluated inside Erlang's default scope.

  ## Examples

      Code.eval "a + b", [a: 1, b: 2], file: __FILE__, line: __LINE__
      # => { 3, [ {:a, 1}, {:b, 2} ] }

  """
  def eval(string, binding // [], opts // []) do
    { value, binding, _scope } =
      Erlang.elixir.eval :unicode.characters_to_list(string), binding, opts
    { value, binding }
  end

  @doc """
  Evalutes the quoted contents.

  ## Examples

      contents = quote hygiene: false, do: a + b
      Code.eval_quoted contents, [a: 1, b: 2], file: __FILE__, line: __LINE__
      # => { 3, [ {:a, 1}, {:b, 2} ] }

  """
  def eval_quoted(quoted, binding // [], opts // []) do
    { value, binding, _scope } =
      Erlang.elixir.eval_quoted [quoted], binding, opts
    { value, binding }
  end

  @doc """
  Loads the given `file`. Accepts `relative_to` as an argument to tell
  where the file is located. If the file was already required/loaded,
  loads it again. It returns the full path of the loaded file.

  When loading a file, you may skip passing .exs as extension as Elixir
  automatically adds it for you.
  """
  def load_file(file, relative_to // nil) do
    file = find_file(file, relative_to)
    server_call { :loaded, file }
    Erlang.elixir_compiler.file to_char_list(file)
    file
  end

  @doc """
  Requires the given `file`. Accepts `relative_to` as an argument to tell
  where the file is located. If the file was already required/loaded,
  returns nil, otherwise the full path of the loaded file.

  When requiring a file, you may skip passing .exs as extension as
  Elixir automatically adds it for you.
  """
  def require_file(file, relative_to // nil) do
    file = find_file(file, relative_to)

    case server_call { :loaded, file } do
    match: :ok
      Erlang.elixir_compiler.file to_char_list(file)
      file
    match: :duplicated
      nil
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

  * docs       - when true, retain documentation in the compiled module;
  * debug_info - when true, retain debug information in the compiled module.
    Notice debug information can be used to reconstruct the source code;
  * ignore_module_conflict - when true, override modules that were already defined;

  """
  def compiler_options(opts) do
    server_call { :compiler_options, opts }
  end

  @doc """
  Compiles the given string and returns a list of tuples where
  the first element is the module name and the second one is its
  binary.

  For compiling many files at once, check `Elixir.ParallelCompiler`.
  """
  def compile_string(string, file // 'nofile') do
    Erlang.elixir_compiler.string :unicode.characters_to_list(string), to_char_list(file)
  end

  @doc """
  Ensure if the given module is loaded. If the module is already loaded,
  it works as no-op. If the module was not loaded yet, it tries to load it.

  If it succeeds loading the module anyhow, it returns `{ :module, module }`.
  If not, returns `{ :error, reason }` with the error reason.
  """
  def ensure_loaded(module) when is_atom(module) do
    Erlang.code.ensure_loaded(module)
  end

  ## Helpers

  # Finds the file given the relative_to path.
  # If the file is found, returns its path in binary, fails otherwise.
  defp find_file(file, relative_to) do
    file = to_binary(file)

    file = if relative_to do
      File.expand_path(file, relative_to)
    else:
      File.expand_path(file)
    end

    if File.regular?(file) do
      file
    else:
      prefix = "#{file}.exs"
      if File.regular?(prefix) do
        prefix
      else:
        raise ArgumentError, message: "could not load #{file}"
      end
    end
  end

  defp server_call(args) do
    Erlang.gen_server.call(:elixir_code_server, args)
  end
end
