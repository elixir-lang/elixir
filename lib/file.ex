object File
  module Mixin
    def expand_path(string)
      fullpath = Erlang.filename.absname(string.to_bin)
      ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}.replace_all(fullpath, "\\4")
    end

    def expand_path(string, relative)
      fullpath = Erlang.filename.absname(string.to_bin, File.expand_path(relative).to_bin)
      ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}.replace_all(fullpath, "\\4")
    end

    def join(a, b)
      Erlang.filename.join(a.to_bin, b.to_bin)
    end

    % Check if the given path exists and it is a file.
    def is_file?(path)
      Erlang.elixir_file_methods.is_file(path.to_bin)
    end
  end
end