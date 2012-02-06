defmodule Code do
  def version, do: "0.4.0.dev"

  # Returns the current ARGV.
  def argv do
    server_call :argv
  end

  # Returns all the loaded files.
  def loaded_files do
    server_call :loaded
  end

  # Registers a function that will be invoked
  # at the end of program execution. Useful for
  # invoking a hook on scripted mode.
  #
  # The function must expect the exit status code
  # as argument.
  def at_exit(fun) when is_function(fun, 1) do
    server_call { :at_exit, fun }
  end

  def append_path(path) do
    Erlang.code.add_pathz(to_char_list(path))
  end

  def prepend_path(path) do
    Erlang.code.add_patha(to_char_list(path))
  end

  # Evalutes the quotes contents.
  #
  # ## Examples
  #
  #     contents = quote do: a + b
  #     Code.eval_quoted contents, [a: 1, b: 2], __FILE__, __LINE__ # => 3
  #
  def eval_quoted(quoted, binding, filename, line) do
    Erlang.elixir.eval_quoted [quoted], binding, line, to_char_list(filename)
  end

  # Loads the given `file`. Accepts `relative_to` as an argument to tell
  # where the file is located. If the file was already required/loaded,
  # loads it again. It returns the full path of the loaded file.
  #
  # When loading a file, you may skip passing .exs as extension as Elixir
  # automatically adds it for you.
  def load_file(file, relative_to // nil) do
    load_and_push_file find_file(file, relative_to)
  end

  # Requires the given `file`. Accepts `relative_to` as an argument to tell
  # where the file is located. If the file was already required/loaded,
  # returns nil, otherwise the full path of the loaded file.
  #
  # When requiring a file, you may skip passing .exs as extension as
  # Elixir automatically adds it for you.
  def require_file(file, relative_to // nil) do
    file = find_file(file, relative_to)
    if List.member?(loaded_files, file) do
      nil
    else:
      load_and_push_file file
    end
  end

  # Compiles `file` and returns a list of tuples where the first element
  # is the module name and the second one is its binary.
  def compile_file(file) do
    Erlang.elixir_compiler.file to_char_list(file)
  end

  # Compiles `file` and add the result to the given `destination`.
  # Destination needs to be a directory.
  def compile_file_to_dir(file, destination) do
    Erlang.elixir_compiler.file_to_path to_char_list(file), to_char_list(destination)
  end

  # Get the stacktrace.
  def stacktrace do
    filter_stacktrace Erlang.erlang.get_stacktrace
  end

  ## Helpers

  # Filter stacktrace by removing internal BOOTSTRAP calls.
  defp filter_stacktrace([{ _mod, :BOOTSTRAP, _, _ }|t]), do: filter_stacktrace(t)
  defp filter_stacktrace([{ _mod, :BOOTSTRAP, _ }|t]),    do: filter_stacktrace(t)
  defp filter_stacktrace([h|t]), do: [h|filter_stacktrace(t)]
  defp filter_stacktrace([]), do: []

  defp load_and_push_file(file) do
    server_call { :loaded, file }
    compile_file file
    file
  end

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
      file = "#{file}.exs"
      if File.regular?(file) do
        file
      else:
        error { :enoent, file }
      end
    end
  end

  defp server_call(args) do
    Erlang.gen_server.call(:elixir_code_server, args)
  end
end