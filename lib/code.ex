module Code
  % Returns the extra argv options given when elixir is invoked.
  def argv
    server_call 'argv
  end

  % Returns all loaded files.
  def loaded_files
    server_call 'loaded
  end

  % Return all paths, including Erlang ones.
  def paths
    Erlang.code.get_paths.map _.to_bin
  end

  % Prepend a path to Erlang's code path.
  def prepend_path(path)
    Erlang.code.add_patha path.to_char_list
  end

  % Append a path to Erlang's code path.
  def append_path(path)
    Erlang.code.add_pathz path.to_char_list
  end

  % Returns elixir version.
  def version
    "0.2.0.dev"
  end

  def require(_file)
    IO.puts "[ELIXIR] Code.require is deprecated. Elixir now compile files and " \
      "relies on Erlang's autoload. Check the README for more information."
  end

  % Compile a *file* and add the result to the given *destination*. Destination
  % needs to be a directory.
  def compile_file(file, destination)
    Erlang.elixir_compiler.file(file.to_char_list, destination.to_char_list)
  end

  % Require the given *file*. Accepts *relative_to* as an argument to tell
  % where the file is located. If the file was already required/loaded,
  % returns nil, otherwise the full path of the loaded file.
  %
  % When requiring a file, you may skip passing .exs as extension as Elixir
  % automatically adds it for you.
  def require_file(file, relative_to := nil)
    file = find_file(file, relative_to)
    if loaded_files.include?(file)
      nil
    else
      load_and_push_file file
    end
  end

  % Require the given *file*. Accepts *relative_to* as an argument to tell
  % where the file is located. If the file was already required/loaded,
  % loads it again. It returns the full path of the loaded file.
  %
  % When loading a file, you may skip passing .exs as extension as Elixir
  % automatically adds it for you.
  def load_file(file, relative_to := nil)
    load_and_push_file find_file(file, relative_to)
  end

  private

  def load_and_push_file(file)
    server_call { 'loaded, file }
    Erlang.elixir.file file.to_char_list
    file
  end

  def find_file(file, relative_to)
    file = if relative_to
      File.expand_path(file, relative_to)
    else
      File.expand_path(file)
    end

    if File.regular?(file)
      file
    else
      file = file + ".exs"
      if File.regular?(file)
        file
      else
        error { 'enoent, file }
      end
    end
  end

  def server_call(args)
    GenServer.call('elixir_code_server, args)
  end
end