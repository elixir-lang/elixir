module File

  % Opens the file named by *filename* according to *modes*.
  % Default modes: ['read].
  % Available modes: 'read | 'write | 'append | 'exclusive | 'raw | 'binary
  def new(filename, modes := ['read])
    #File::Behavior(filename, modes)
  end

  % Same, as *new*, but expects a function block. Returns the closed file.
  def open(filename, modes, fun)
    file = new(filename, modes)
    try
      fun[file]
    after
      file.close
    end
    file
  end

  % Opens for read.
  def open(filename, fun) when Erlang.is_function(fun)
    open(filename, ['read], fun)
  end

  % Converts a pathname to an absolute pathname.
  def expand_path(string, relative := [])
    fullpath = absname(string, relative)
    strip_dots ~r{((/\.)|(/[^/]*[^\\]/\.\.))(/|\z)}, fullpath
  end

  % Split the filename into its parts
  def split(filename)
    Erlang.filename.split(filename)
  end

  % Join all the paths in the given list to make a filename
  def join(list)
    Erlang.filename.join list
  end

  % Join paths *a* and *b*
  def join(a, b)
    Erlang.filename.join(a, b)
  end

  % Retrieves all filenames according to the wildcard expression.
  % Check for more info http://www.erlang.org/doc/man/filelib.html#wildcard-1
  def wildcard(wildcard, include_dot := false)
    Erlang.elixir_glob.wildcard(wildcard, include_dot)
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

  % Try to touch the given file. Returns ok if successful.
  def touch(filename)
    case Erlang.file.write_file(filename, [])
    match 'ok
      'ok
    match { 'error, reason }
      error reason
    end
  end

  % Try to delete the given file. Returns ok if successful.
  def delete(filename)
    case Erlang.file.delete(filename)
    match 'ok
      'ok
    match { 'error, reason }
      error reason
    end
  end
  alias_local 'delete, 'unlink, 1

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

module File::Behavior
  attr_reader ['path]

  def __bound__(filename, modes := ['read])
    case Erlang.file  .open(filename, modes)
    match { 'ok, device }
      @('device: device, 'path: filename)
    match { 'error, reason }
      error reason
    end
  end

  def inspect
    "<##{__module_name__} #{path}>"
  end

  % Returns file info
  def info
    File.read_info path
  end

  % Writes *bytes* to file
  def write(bytes)
    case Erlang.file.write @device, bytes
    match 'ok
      'ok
    match { 'error, reason }
      error reason
    end
  end

  % Close the file, prevents further writing
  def close
    case Erlang.file.close @device
    match 'ok
      'ok
    match { 'error, reason }
      error reason
    end
  end

  % Sets the position of the file to *location*. Returns the new position
  % (as absolute offset) if successful. Location is one of the following:
  %
  %    offset:          The same as {'bof, offset}.
  %    {'bof, offset}:  Absolute offset.
  %    {'cur, offset}:  Offset from the current position.
  %    {'eof, offset}:  Offset the end of file.
  def pos(location)
    case Erlang.file.position @device, location
    match { 'ok, position }
      position
    match { 'error, reason }
      error reason
    end
  end

  % Sets the position to 0
  def rewind
    pos(0)
  end

  % Reads n byte from the current position
  def read(n)
    case Erlang.file.read @device, n
    match { 'ok, bytes }
      bytes
    match { 'error, reason }
      error reason
    match 'eof
      'eof
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