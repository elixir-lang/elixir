% elixir: cache

object File
  module Mixin
    def expand_path(string, relative := [])
      fullpath = absname(string, relative)
      strip_dots ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}, fullpath
    end

    def split(filename)
      Erlang.filename.split(filename.to_bin).map -> (i) String.new i
    end

    def join(list)
      String.new Erlang.filename.join list.map(_.to_bin)
    end

    def join(a, b)
      String.new Erlang.filename.join(a.to_bin, b.to_bin)
    end

    private

    def absname(string, [])
      Erlang.filename.absname(string.to_bin)
    end

    def absname(string, relative)
      Erlang.filename.absname(string.to_bin, File.expand_path(relative, []).to_bin)
    end

    def strip_dots(regexp, path)
      new = regexp.replace_all(path, "\\4")
      if new == path
        path
      else
        strip_dots(regexp, new)
      end
    end
  end
end