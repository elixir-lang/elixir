% elixir: cache

module Code
  def argv
    server_call 'argv
  end

  def version
    "0.2.0.dev"
  end

  def require(_file)
    IO.puts "[ELIXIR] Code.require is deprecated. Elixir now compile files and " \
      "relies on Erlang's autoload. Check the README for more information."
  end

  def loaded_files
    server_call 'loaded
  end

  def require_file(file, relative_to := nil)
    file = find_file(file, relative_to)
    if loaded_files.include?(file)
      nil
    else
      load_and_push_file file
    end
  end

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