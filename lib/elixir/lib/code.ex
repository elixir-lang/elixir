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

      Code.eval "a + b", [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  When passing the __ENV__'s file and line, we could simply get
  the location which already returns both fields as a keywords lists:

      Code.eval "a + b", [a: 1, b: 2], __ENV__.location
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  """
  def eval(string, binding // [], opts // []) do
    { value, binding, _scope } =
      Erlang.elixir.eval :unicode.characters_to_list(string), binding, opts
    { value, binding }
  end

  @doc """
  Evalutes the quoted contents.

  ## Options

  This function accepts a list of options. The supported
  options are:

  * `:file` - The filename to be used in stacktraces
    and the file reported in the __ENV__ variable.

  * `:line` - The line reported in the __ENV__ variable.

  ## Examples

      contents = quote hygiene: false, do: a + b

      Code.eval_quoted contents, [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  When passing the __ENV__'s file and line, we could simply get
  the location which already returns both fields as a keywords lists:

      Code.eval_quoted contents, [a: 1, b: 2], __ENV__.location
      #=> { 3, [ {:a, 1}, {:b, 2} ] }

  """
  def eval_quoted(quoted, binding // [], opts // []) do
    { value, binding, _scope } =
      Erlang.elixir.eval_quoted [quoted], binding, opts
    { value, binding }
  end

  @doc """
  Converts the given string to AST. It returns { :ok, ast }
  if it succeeds, { :error, { line, error, token } } otherwise.

  ## Options

  * `:file` - The filename to be used in stacktraces
    and the file reported in the __ENV__ variable.

  * `:line` - The line reported in the __ENV__ variable.

  """
  def string_to_ast(string, opts // []) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    res  = :elixir_translator.raw_forms(:unicode.characters_to_list(string), line, file)
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

  ## Options

  * `:file` - The filename to be used in stacktraces
    and the file reported in the __ENV__ variable.

  * `:line` - The line reported in the __ENV__ variable.

  """
  def string_to_ast!(string, opts // []) do
    file = Keyword.get opts, :file, "nofile"
    line = Keyword.get opts, :line, 1
    res  = :elixir_translator.forms(:unicode.characters_to_list(string), line, file)
    unpack_ast(line, res)
  end

  defp unpack_ast(_line, []),                              do: nil
  defp unpack_ast(_line, [forms]) when not is_list(forms), do: forms
  defp unpack_ast(line, forms),                            do: { :__block__, line, forms }

  @doc """
  Loads the given `file`. Accepts `relative_to` as an argument to tell
  where the file is located. If the file was already required/loaded,
  loads it again. It returns the full path of the loaded file.

  When loading a file, you may skip passing .exs as extension as Elixir
  automatically adds it for you.
  """
  def load_file(file, relative_to // nil) when is_binary(file) do
    file = find_file(file, relative_to)
    server_call { :loaded, file }
    Erlang.elixir_compiler.file file
    file
  end

  @doc """
  Requires the given `file`. Accepts `relative_to` as an argument to tell
  where the file is located. If the file was already required/loaded,
  returns nil, otherwise the full path of the loaded file.

  When requiring a file, you may skip passing .exs as extension as
  Elixir automatically adds it for you.
  """
  def require_file(file, relative_to // nil) when is_binary(file) do
    file = find_file(file, relative_to)

    case server_call({ :loaded, file }) do
      :ok         -> Erlang.elixir_compiler.file file
      :duplicated -> []
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

  For compiling many files at once, check `Elixir.ParallelCompiler`.
  """
  def compile_string(string, file // "nofile") when is_binary(file) do
    Erlang.elixir_compiler.string :unicode.characters_to_list(string), to_binary(file)
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
    else
      File.expand_path(file)
    end

    if File.regular?(file) do
      file
    else
      prefix = "#{file}.exs"
      if File.regular?(prefix) do
        prefix
      else
        raise ArgumentError, message: "could not load #{file}"
      end
    end
  end

  defp server_call(args) do
    Erlang.gen_server.call(:elixir_code_server, args)
  end
end
