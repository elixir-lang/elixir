object File
  module Mixin
    def expand_path(string)
      fullpath = Erlang.filename.absname(string.to_list)
      ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}.replace_all(fullpath, "\\4")
    end
  end
end