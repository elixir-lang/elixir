object File
  module Mixin
    def expand_path(string)
      expand_path(string, [])
    end

    def expand_path(string, relative)
      fullpath = absname(string, relative)
      ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}.replace_all(fullpath, "\\4")
    end

    def join(a, b)
      Erlang.filename.join(a.to_bin, b.to_bin)
    end

    % Check if the given path exists and it is a file.
    def is_file?(path)
      Erlang.elixir_file_methods.is_file(path.to_bin)
    end

    private

    def absname(string, [])
      Erlang.filename.absname(string.to_bin)
    end

    def absname(string, relative)
      Erlang.filename.absname(string.to_bin, File.expand_path(relative, []).to_bin)
    end
  end
end