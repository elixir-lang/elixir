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

    % Try to read the given file. If possible, returns a string
    % with the file contents. Raises an error otherwise.
    def read(filename)
      case Erlang.file.read_file(filename.to_bin)
      match { 'ok, binary }
        String.new binary
      match { 'error, other }
        self.error other
      end
    end

    % Returns if filename is a regular file or not.
    def regular?(filename)
      Erlang.filelib.is_regular(filename.to_bin)
    end

    % Try to read the given file. If possible, returns a File::Info
    % object. Raises an error otherwise.
    def read_info(filename)
      case Erlang.file.read_file_info(filename.to_bin)
      match { 'ok, info }
        File::Info.new info
      match { 'error, other }
        self.error other
      end
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

object File::Info
  proto Record
  record 'file_info, 'from_lib: "kernel/include/file.hrl"

  def regular?
    @type == 'regular
  end

  def directory?
    @type == 'directory
  end

  def read?
    @access == 'read orelse @access == 'read_write
  end

  def write?
    @access == 'write orelse @access == 'read_write
  end
end