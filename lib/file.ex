module File
  def expand_path(string, relative := [])
    fullpath = absname(string, relative)
    strip_dots ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}, fullpath
  end

  def split(filename)
    Erlang.filename.split(filename)
  end

  def join(list)
    Erlang.filename.join list
  end

  def join(a, b)
    Erlang.filename.join(a, b)
  end

  % Try to read the given file. If possible, returns a string
  % with the file contents. Raises an error otherwise.
  def read(filename)
    case Erlang.file.read_file(filename)
    match { 'ok, binary }
      binary
    match { 'error, other }
      error other
    end
  end

  % Returns if filename is a regular file or not.
  def regular?(filename)
    Erlang.filelib.is_regular(filename)
  end

  % Try to read the given file. If possible, returns a File::Info
  % object. Raises an error otherwise.
  def read_info(filename)
    case Erlang.file.read_file_info(filename)
    match { 'ok, info }
      #File::Info(info)
    match { 'error, other }
      error other
    end
  end

  private

  def absname(string, [])
    Erlang.filename.absname(string)
  end

  def absname(string, relative)
    Erlang.filename.absname(string, File.expand_path(relative, []))
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

module File::Info
  mixin Record
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