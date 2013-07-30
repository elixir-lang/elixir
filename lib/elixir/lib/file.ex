defrecord File.Stat, Record.extract(:file_info, from_lib: "kernel/include/file.hrl") do
  @moduledoc """
  A record responsible to hold file information. Its fields are:

  * `size` - Size of file in bytes.
  * `type` - `:device`, `:directory`, `:regular`, `:other`. The type of the file.
  * `access` - `:read`, `:write`, `:read_write`, `:none`. The current system access to
                the file.
  * `atime` - The last time the file was read.
  * `mtime` - The last time the file was written.
  * `ctime` - The interpretation of this time field depends on the operating
              system. On Unix, it is the last time the file or the inode was
              changed. In Windows, it is the create time.
  * `mode` - The file permissions.
  * `links` - The number of links to this file. This is always 1 for file
              systems which have no concept of links.
  * `major_device` - Identifies the file system where the file is located.
                     In windows, the number indicates a drive as follows:
                     0 means A:, 1 means B:, and so on.
  * `minor_device` - Only valid for character devices on Unix. In all other
                     cases, this field is zero.
  * `inode` - Gives the inode number. On non-Unix file systems, this field
              will be zero.
  * `uid` - Indicates the owner of the file.
  * `gid` - Gives the group that the owner of the file belongs to. Will be
            zero for non-Unix file systems.

  The time type returned in `atime`, `mtime`, and `ctime` is dependent on the
  time type set in options. `{:time, type}` where type can be `:local`,
  `:universal`, or `:posix`. Default is `:local`.
  """
end

defexception File.Error, [reason: nil, action: "", path: nil] do
  def message(exception) do
    formatted = list_to_binary(:file.format_error(reason exception))
    "could not #{action exception} #{path exception}: #{formatted}"
  end
end

defexception File.CopyError, [reason: nil, action: "", source: nil, destination: nil, on: nil] do
  def message(exception) do
    formatted = list_to_binary(:file.format_error(reason exception))
    location  = if on = on(exception), do: ". #{on}", else: ""
    "could not #{action exception} from #{source exception} to " <>
      "#{destination exception}#{location}: #{formatted}"
  end
end

defexception File.IteratorError, reason: nil do
  def message(exception) do
    formatted = list_to_binary(:file.format_error(reason exception))
    "error during file iteration: #{formatted}"
  end
end

defmodule File do
  @moduledoc """
  This module contains functions to manipulate files.

  Some of those functions are low-level, allowing the user
  to interact with the file or IO devices, like `open/2`,
  `copy/3` and others. This module also provides higher
  level functions that works with filenames and have their naming
  based on UNIX variants. For example, one can copy a file
  via `cp/3` and remove files and directories recursively
  via `rm_rf/1`

  In order to write and read files, one must use the functions
  in the `IO` module. By default, a file is opened in binary mode
  which requires the functions `IO.binread/2` and `IO.binwrite/2`
  to interact with the file. A developer may pass `:utf8` as an
  option when opening the file and then all other functions
  from `IO` are available, since they work directly with Unicode
  data.

  Most of the functions in this module return `:ok` or
  `{ :ok, result }` in case of success, `{ :error, reason }`
  otherwise. Those function are also followed by a variant
  that ends with `!` which returns the result (without the
  `{ :ok, result }` tuple) in case of success or raises an
  exception in case it fails. For example:

      File.read("hello.txt")
      #=> { :ok, "World" }

      File.read("invalid.txt")
      #=> { :error, :enoent }

      File.read!("hello.txt")
      #=> "World"

      File.read!("invalid.txt")
      #=> raises File.Error

  In general, a developer should use the former in case he wants
  to react if the file does not exist. The latter should be used
  when the developer expects his software to fail in case the
  file cannot be read (i.e. it is literally an exception).
  """

  alias :file,     as: F
  alias :filename, as: FN
  alias :filelib,  as: FL

  @doc """
  Returns `true` if the path is a regular file.

  ## Examples

      File.regular? __FILE__ #=> true

  """
  def regular?(path) do
    FL.is_regular(path)
  end

  @doc """
  Returns `true` if the path is a directory.
  """
  def dir?(path) do
    FL.is_dir(path)
  end

  @doc """
  Returns `true` if the given path exists.
  It can be regular file, directory, socket,
  symbolic link, named pipe or device file.

  ## Examples

      File.exists?("test/")
      #=> true

      File.exists?("missing.txt")
      #=> false

      File.exists?("/dev/null")
      #=> true

  """
  def exists?(path) do
    match?({ :ok, _ }, F.read_file_info(path))
  end

  @doc """
  Tries to create the directory `path`. Missing parent directories are not created.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

  * :eacces  - Missing search or write permissions for the parent directories of `path`.
  * :eexist  - There is already a file or directory named `path`.
  * :enoent  - A component of `path` does not exist.
  * :enospc  - There is a no space left on the device.
  * :enotdir - A component of `path` is not a directory
               On some platforms, `:enoent` is returned instead.
  """
  def mkdir(path) do
    F.make_dir(path)
  end

  @doc """
  Same as `mkdir/1`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def mkdir!(path) do
    case mkdir(path) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "make directory", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Tries to create the directory `path`. Missing parent directories are created.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

  * :eacces  - Missing search or write permissions for the parent directories of `path`.
  * :enospc  - There is a no space left on the device.
  * :enotdir - A component of `path` is not a directory.
  """
  def mkdir_p(path) do
    FL.ensure_dir(FN.join(path, "."))
  end

  @doc """
  Same as `mkdir_p/1`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def mkdir_p!(path) do
    case mkdir_p(path) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "make directory (with -p)", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Returns `{:ok, binary}`, where `binary` is a binary data object that contains the contents
  of `path`, or `{:error, reason}` if an error occurs.

  Typical error reasons:

  * :enoent  - The file does not exist.
  * :eacces  - Missing permission for reading the file,
               or for searching one of the parent directories.
  * :eisdir  - The named file is a directory.
  * :enotdir - A component of the file name is not a directory.
               On some platforms, `:enoent` is returned instead.
  * :enomem  - There is not enough memory for the contents of the file.

  You can use `:file.format_error/1` to get a descriptive string of the error.
  """
  def read(path) do
    F.read_file(path)
  end

  @doc """
  Returns binary with the contents of the given filename or raises
  `File.Error` if an error occurs.
  """
  def read!(path) do
    case read(path) do
      { :ok, binary } ->
        binary
      { :error, reason } ->
        raise File.Error, reason: reason, action: "read file", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Returns information about the `path`. If it exists, it
  returns a `{ :ok, info }` tuple, where info is a
  `File.Info` record. Retuns `{ :error, reason }` with
  the same reasons as `read/1` if a failure occurs.

  ## Options

  The accepted options are:

  * `:time` if the time should be `:local`, `:universal` or `:posix`.
    Default is `:local`.

  """
  def stat(path, opts // []) do
    case F.read_file_info(path, opts) do
      {:ok, fileinfo} ->
        {:ok, set_elem(fileinfo, 0, File.Stat) }
      error ->
        error
    end
  end

  @doc """
  Same as `stat/2` but returns the `File.Stat` directly and
  throws `File.Error` if an error is returned.
  """
  def stat!(path, opts // []) do
    case stat(path, opts) do
      {:ok, info}      -> info
      {:error, reason} ->
        raise File.Error, reason: reason, action: "read file stats", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Writes the given `File.Stat` back to the filesystem at the given
  path. Returns `:ok` or `{ :error, reason }`.
  """
  def write_stat(path, File.Stat[] = stat, opts // []) do
    F.write_file_info(path, set_elem(stat, 0, :file_info), opts)
  end

  @doc """
  Same as `write_stat/3` but raises an exception if it fails.
  Returns `:ok` otherwise.
  """
  def write_stat!(path, File.Stat[] = stat, opts // []) do
    case write_stat(path, stat, opts) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "write file stats", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Updates modification time (mtime) and access time (atime) of
  the given file. File is created if it doesn’t exist.
  """
  def touch(path, time // :calendar.local_time) do
    case F.change_time(path, time) do
      { :error, :enoent } -> write(path, "")
      other -> other
    end
  end

  @doc """
  Same as `touch/2` but raises an exception if it fails.
  Returns `:ok` otherwise.
  """
  def touch!(path, time // :calendar.local_time) do
    case touch(path, time) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "touch", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Copies the contents of `source` to `destination`.

  Both parameters can be a filename or an io device opened
  with `open/2`. `bytes_count` specifies the number of
  bytes to copy, the default being `:infinity`.

  If file `destination` already exists, it is overwritten
  by the contents in `source`.

  Returns `{ :ok, bytes_copied }` if successful,
  `{ :error, reason }` otherwise.

  Compared to the `cp/3`, this function is more low-level,
  allowing a copy from device to device limited by a number of
  bytes. On the other hand, `cp/3` performs more extensive
  checks on both source and destination and it also preserves
  the file mode after copy.

  Typical error reasons are the same as in `open/2`,
  `read/1` and `write/3`.
  """
  def copy(source, destination, bytes_count // :infinity) do
    F.copy(source, destination, bytes_count)
  end

  @doc """
  The same as `copy/3` but raises an `File.CopyError` if it fails.
  Returns the `bytes_copied` otherwise.
  """
  def copy!(source, destination, bytes_count // :infinity) do
    case copy(source, destination, bytes_count) do
      { :ok, bytes_count } -> bytes_count
      { :error, reason } ->
        raise File.CopyError, reason: reason, action: "copy",
          source: :unicode.characters_to_binary(source), destination: :unicode.characters_to_binary(destination)
    end
  end

  @doc """
  Copies the contents in `source` to `destination` preserving its mode.

  Similar to the command `cp` in Unix systems, this function
  behaves differently depending if `destination` is a directory
  or not. In particular, if `destination` is a directory, it
  copies the contents of `source` to `destination/source`.

  If a file already exists in the destination, it invokes a
  callback which should return `true` if the existing file
  should be overwritten, `false` otherwise. It defaults to return `true`.

  It returns `:ok` in case of success, returns
  `{ :error, reason }` otherwise.

  If you want to copy contents from an io device to another device
  or do a straight copy from a source to a destination without
  preserving modes, check `copy/3` instead.
  """
  def cp(source, destination, callback // fn(_, _) -> true end) do
    output =
      if dir?(destination) do
        mkdir(destination)
        FN.join(destination, FN.basename(source))
      else
        destination
      end

    case do_cp_file(source, output, callback, []) do
      { :error, reason, _ } -> { :error, reason }
      _ -> :ok
    end
  end

  @doc """
  The same as `cp/3`, but raises `File.CopyError` if it fails.
  Returns the list of copied files otherwise.
  """
  def cp!(source, destination, callback // fn(_, _) -> true end) do
    case cp(source, destination, callback) do
      :ok -> :ok
      { :error, reason } ->
        raise File.CopyError, reason: reason, action: "copy recursively",
          source: :unicode.characters_to_binary(source),
          destination: :unicode.characters_to_binary(destination)
    end
  end

  @doc %B"""
  Copies the contents in source to destination.
  Similar to the command `cp -r` in Unix systems,
  this function behaves differently depending
  if `source` and `destination` are a file or a directory.

  If both are files, it simply copies `source` to
  `destination`. However, if `destination` is a directory,
  it copies the contents of `source` to `destination/source`
  recursively.

  If a file already exists in the destination,
  it invokes a callback which should return
  `true` if the existing file should be overwritten,
  `false` otherwise. It defaults to return `true`.

  If a directory already exists in the destination
  where a file is meant to be (or otherwise), this
  function will fail.

  This function may fail while copying files,
  in such cases, it will leave the destination
  directory in a dirty state, where already
  copied files won't be removed.

  It returns `{ :ok, files_and_directories }` in case of
  success with all files and directories copied in no
  specific order, `{ :error, reason }` otherwise.

  ## Examples

      # Copies "a.txt" to "tmp/a.txt"
      File.cp_r "a.txt", "tmp"

      # Copies all files in "samples" to "tmp/samples"
      File.cp_r "samples", "tmp"

      # Copies all files in "samples" to "tmp"
      File.cp_r "samples/.", "tmp"

      # Same as before, but asks the user how to proceed in case of conflicts
      File.cp_r "samples/.", "tmp", fn(source, destination) ->
        IO.gets("Overwriting #{destination} by #{source}. Type y to confirm.") == "y"
      end

  """
  def cp_r(source, destination, callback // fn(_, _) -> true end) when is_function(callback) do
    output =
      if dir?(destination) || dir?(source) do
        mkdir(destination)
        FN.join(destination, FN.basename(source))
      else
        destination
      end

    case do_cp_r(source, output, callback, []) do
      { :error, _, _ } = error -> error
      res -> { :ok, res }
    end
  end

  @doc """
  The same as `cp_r/3`, but raises `File.CopyError` if it fails.
  Returns the list of copied files otherwise.
  """
  def cp_r!(source, destination, callback // fn(_, _) -> true end) do
    case cp_r(source, destination, callback) do
      { :ok, files } -> files
      { :error, reason, file } ->
        raise File.CopyError, reason: reason, action: "copy recursively",
          source: :unicode.characters_to_binary(source),
          destination: :unicode.characters_to_binary(destination),
          on: file
    end
  end

  # src may be a file or a directory, dest is definitely
  # a directory. Returns nil unless an error is found.
  defp do_cp_r(src, dest, callback, acc) when is_list(acc) do
    case :elixir.file_type(src) do
      { :ok, :regular } ->
        do_cp_file(src, dest, callback, acc)
      { :ok, :symlink } ->
        case F.read_link(src) do
          { :ok, link } -> do_cp_link(link, src, dest, callback, acc)
          { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
        end
      { :ok, :directory } ->
        case F.list_dir(src) do
          { :ok, files } ->
            case mkdir(dest) do
              success in [:ok, { :error, :eexist }] ->
                Enum.reduce(files, [dest|acc], fn(x, acc) ->
                  do_cp_r(FN.join(src, x), FN.join(dest, x), callback, acc)
                end)
              { :error, reason } -> { :error, reason, :unicode.characters_to_binary(dest) }
            end
          { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
        end
      { :ok, _ } -> { :error, :eio, src }
      { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
    end
  end

  # If we reach this clause, there was an error while
  # processing a file.
  defp do_cp_r(_, _, _, acc) do
    acc
  end

  defp copy_file_mode!(src, dest) do
    src_stat = File.stat!(src)
    dest_stat = File.stat!(dest)
    File.write_stat!(dest, File.Stat.mode(File.Stat.mode(src_stat), dest_stat))
  end

  # Both src and dest are files.
  defp do_cp_file(src, dest, callback, acc) do
    case copy(src, { dest, [:exclusive] }) do
      { :ok, _ } ->
        copy_file_mode!(src, dest)
        [dest|acc]
      { :error, :eexist } ->
        if callback.(src, dest) do
          rm(dest)
          case copy(src, dest) do
            { :ok, _ } ->
              copy_file_mode!(src, dest)
              [dest|acc]
            { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
          end
        else
          acc
        end
      { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
    end
  end

  # Both src and dest are files.
  defp do_cp_link(link, src, dest, callback, acc) do
    case F.make_symlink(link, dest) do
      :ok ->
        [dest|acc]
      { :error, :eexist } ->
        if callback.(src, dest) do
          rm(dest)
          case F.make_symlink(link, dest) do
            :ok -> [dest|acc]
            { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
          end
        else
          acc
        end
      { :error, reason } -> { :error, reason, :unicode.characters_to_binary(src) }
    end
  end

  @doc """
  Writes `content` to the file `path`. The file is created if it
  does not exist. If it exists, the previous contents are overwritten.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

  * :enoent - A component of the file name does not exist.
  * :enotdir - A component of the file name is not a directory.
               On some platforms, enoent is returned instead.
  * :enospc - There is a no space left on the device.
  * :eacces - Missing permission for writing the file or searching one of the parent directories.
  * :eisdir - The named file is a directory.
  """
  def write(path, content, modes // []) do
    F.write_file(path, content, modes)
  end

  @doc """
  Same as `write/3` but raises an exception if it fails, returns `:ok` otherwise.
  """
  def write!(path, content, modes // []) do
    case F.write_file(path, content, modes) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "write to file", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Tries to delete the file `path`.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

  * :enoent  - The file does not exist.
  * :eacces  - Missing permission for the file or one of its parents.
  * :eperm   - The file is a directory and user is not super-user.
  * :enotdir - A component of the file name is not a directory.
               On some platforms, enoent is returned instead.
  * :einval  - Filename had an improper type, such as tuple.

  ## Examples

      File.rm('file.txt')
      #=> :ok

      File.rm('tmp_dir/')
      #=> {:error, :eperm}

  """
  def rm(path) do
    F.delete(path)
  end

  @doc """
  Same as `rm/1`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def rm!(path) do
    case rm(path) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "remove file", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Tries to delete the dir at `path`.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  ## Examples

      File.rddir('tmp_dir')
      #=> :ok

      File.rmdir('file.txt')
      #=> {:error, :enotdir}

  """
  def rmdir(path) do
    F.del_dir(path)
  end

  @doc """
  Same as `rmdir/1`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def rmdir!(path) do
    case rmdir(path) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "remove directory", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Remove files and directories recursively at the given `path`.
  Symlinks are not followed but simply removed, non-existing
  files are simply ignored (i.e. doesn't make this function fail).

  Returns `{ :ok, files_and_directories }` with all files and
  directories removed in no specific order, `{ :error, reason }`
  otherwise.

  ## Examples

      File.rm_rf "samples"
      #=> { :ok, ["samples", "samples/1.txt"] }

      File.rm_rf "unknown"
      #=> { :ok, [] }

  """
  def rm_rf(path) do
    do_rm_rf(path, { :ok, [] })
  end

  defp do_rm_rf(path, { :ok, acc } = entry) do
    case safe_list_dir(path) do
      { :ok, files } ->
        res =
          Enum.reduce files, entry, fn(file, tuple) ->
            do_rm_rf(FN.join(path, file), tuple)
          end

        case res do
          { :ok, acc } ->
            case rmdir(path) do
              :ok -> { :ok, [path|acc] }
              { :error, :enoent } -> res
              reason -> { :error, reason, :unicode.characters_to_binary(path) }
            end
          reason -> { :error, reason, :unicode.characters_to_binary(path) }
        end
      { :error, reason } when reason in [:enotdir, :eio] ->
        case rm(path) do
          :ok -> { :ok, [path|acc] }
          { :error, reason } when reason in [:enoent, :enotdir] -> entry
          reason -> { :error, reason, :unicode.characters_to_binary(path) }
        end
      { :error, :enoent } -> entry
      { :error, reason } -> { :error, reason, :unicode.characters_to_binary(path) }
    end
  end

  defp do_rm_rf(_, reason) do
    reason
  end

  defp safe_list_dir(path) do
    case F.read_link(path) do
      { :ok, _ } -> { :error, :enotdir }
      _ -> F.list_dir(path)
    end
  end

  @doc """
  Same as `rm_rf/1` but raises `File.Error` in case of failures,
  otherwise the list of files or directories removed.
  """
  def rm_rf!(path) do
    case rm_rf(path) do
      { :ok, files } -> files
      { :error, reason, _ } ->
        raise File.Error, reason: reason,
          action: "remove files and directories recursively from",
          path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Opens the given `path` according to the given list of modes.

  In order to write and read files, one must use the functions
  in the `IO` module. By default, a file is opened in binary mode
  which requires the functions `IO.binread/2` and `IO.binwrite/2`
  to interact with the file. A developer may pass `:utf8` as an
  option when opening the file and then all other functions from
  `IO` are available, since they work directly with Unicode data.

  The allowed modes:

  * `:read` - The file, which must exist, is opened for reading.

  * `:write` -  The file is opened for writing. It is created if it does not exist.
                If the file exists, and if write is not combined with read, the file will be truncated.

  * `:append` - The file will be opened for writing, and it will be created if it does not exist.
                Every write operation to a file opened with append will take place at the end of the file.

  * `:exclusive` - The file, when opened for writing, is created if it does not exist.
                   If the file exists, open will return { :error, :eexist }.

  * `:charlist` - When this term is given, read operations on the file will return char lists rather than binaries;

  * `:compressed` -  Makes it possible to read or write gzip compressed files.
                     The compressed option must be combined with either read or write, but not both.
                     Note that the file size obtained with `stat/1` will most probably not
                     match the number of bytes that can be read from a compressed file.

  * `:utf8` - This option denotes how data is actually stored in the disk file and
              makes the file perform automatic translation of characters to and from utf-8.
              If data is sent to a file in a format that cannot be converted to the utf-8
              or if data is read by a function that returns data in a format that cannot cope
              with the character range of the data, an error occurs and the file will be closed.

  If a function is given to modes (instead of a list), it dispatches to `open/3`.

  Check http://www.erlang.org/doc/man/file.html#open-2 for more information about
  other options like `read_ahead` and `delayed_write`.

  This function returns:

  * `{ :ok, io_device }` - The file has been opened in the requested mode.
                         `io_device` is actually the pid of the process which handles the file.
                         This process is linked to the process which originally opened the file.
                         If any process to which the `io_device` is linked terminates, the file will
                         be closed and the process itself will be terminated. An `io_device` returned
                         from this call can be used as an argument to the `IO` module functions.

  * `{ :error, reason }` - The file could not be opened.

  ## Examples

      { :ok, file } = File.open("foo.tar.gz", [:read, :compressed])
      IO.read(file, :line)
      File.close(file)

  """
  def open(path, modes // [])

  def open(path, modes) when is_list(modes) do
    F.open(path, open_defaults(modes, true))
  end

  def open(path, function) when is_function(function) do
    open(path, [], function)
  end

  @doc """
  Similar to `open/2` but expects a function as last argument.

  The file is opened, given to the function as argument and
  automatically closed after the function returns, regardless
  if there was an error or not.

  It returns `{ :ok, function_result }` in case of success,
  `{ :error, reason }` otherwise.

  Do not use this function with :delayed_write option
  since automatically closing the file may fail
  (as writes are delayed).

  ## Examples

      File.open("file.txt", [:read, :write], fn(file) ->
        IO.read(file, :line)
      end)

  """
  def open(path, modes, function) do
    case open(path, modes) do
      { :ok, device } ->
        try do
          { :ok, function.(device) }
        after
          :ok = close(device)
        end
      other -> other
    end
  end

  @doc """
  Same as `open/2` but raises an error if file could not be opened.
  Returns the `io_device` otherwise.
  """
  def open!(path, modes // []) do
    case open(path, modes) do
      { :ok, device }    -> device
      { :error, reason } ->
        raise File.Error, reason: reason, action: "open", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Same as `open/3` but raises an error if file could not be opened.
  Returns the function result otherwise.
  """
  def open!(path, modes, function) do
    case open(path, modes, function) do
      { :ok, device }    -> device
      { :error, reason } ->
        raise File.Error, reason: reason, action: "open", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Gets the current working directory. In rare circumstances, this function can
  fail on Unix. It may happen if read permission does not exist for the parent
  directories of the current directory. For this reason, returns `{ :ok, cwd }`
  in case of success, `{ :error, reason }` otherwise.
  """
  def cwd() do
    case F.get_cwd do
      { :ok, cwd } -> { :ok, :unicode.characters_to_binary(cwd) }
      { :error, _ } = error -> error
    end
  end

  @doc """
  The same as `cwd/0`, but raises an exception if it fails.
  """
  def cwd!() do
    case F.get_cwd do
      { :ok, cwd } -> :unicode.characters_to_binary(cwd)
      { :error, reason } ->
          raise File.Error, reason: reason, action: "get current working directory"
    end
  end

  @doc """
  Sets the current working directory. Returns `:ok` if successful,
  `{ :error, reason }` otherwise.
  """
  def cd(path) do
    F.set_cwd(path)
  end

  @doc """
  The same as `cd/1`, but raises an exception if it fails.
  """
  def cd!(path) do
    case F.set_cwd(path) do
      :ok -> :ok
      { :error, reason } ->
          raise File.Error, reason: reason, action: "set current working directory to", path: :unicode.characters_to_binary(path)
    end
  end

  @doc """
  Changes the current directory to the given `path`,
  executes the given function and then revert back
  to the previous path regardless if there is an exception.

  Raises an error if retrieving or changing the current
  directory fails.
  """
  def cd!(path, function) do
    old = cwd!
    cd!(path)
    try do
      function.()
    after
      cd!(old)
    end
  end

  @doc """
  Returns list of files in the given directory.

  It returns `{ :ok, [files] }` in case of success,
  `{ :error, reason }` otherwise.
  """
  def ls(path // ".") do
    case F.list_dir(path) do
      { :ok, file_list } -> { :ok, Enum.map(file_list, :unicode.characters_to_binary(&1)) }
      { :error, _ } = error -> error
    end
  end

  @doc """
  The same as `ls/1` but raises `File.Error`
  in case of an error.
  """
  def ls!(dir // ".") do
    case ls(dir) do
      { :ok, value } -> value
      { :error, reason } ->
        raise File.Error, reason: reason, action: "list directory", path: :unicode.characters_to_binary(dir)
    end
  end

  @doc """
  Closes the file referenced by `io_device`. It mostly returns `:ok`, except
  for some severe errors such as out of memory.

  Note that if the option `:delayed_write` was used when opening the file,
  `close/1` might return an old write error and not even try to close the file.
  See `open/2`.
  """
  def close(io_device) do
    F.close(io_device)
  end

  @doc """
  Opens the given `file` with the given `mode` and
  returns its stream. The returned stream will
  fail for the same reasons as `File.open!`. Note
  that the file is opened when the iteration begins.
  """
  def stream!(file, mode // []) do
    fn(acc, fun) ->
      device = open!(file, mode)
      try do
        IO.stream(device, acc, fun)
      after
        F.close(device)
      end
    end
  end

  @doc """
  Opens the given `file` with the given `mode` and
  returns its binstream. The returned stream will
  fail for the same reasons as `open!/2`. Note
  that the file is opened when the iteration begins.
  """
  def binstream!(file, mode // []) do
    fn(fun, acc) ->
      device = open!(file, mode)
      try do
        IO.binstream(device, fun, acc)
      after
        F.close(device)
      end
    end
  end

  @doc """
  Changes the unix file `mode` for a given `file`.
  Returns `:ok` on success, or `{:error, reason}`
  on failure.
  """
  def chmod(file, mode) do
    F.change_mode(file, mode)
  end

  @doc """
  Same as `chmod/2`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def chmod!(file, mode) do
    case chmod(file, mode) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "change mode for", path: :unicode.characters_to_binary(file)
    end
  end

  @doc """
  Changes the user group given by the group id `gid`
  for a given `file`. Returns `:ok` on success, or
  `{:error, reason}` on failure.
  """
  def chgrp(file, gid) do
    F.change_group(file, gid)
  end

  @doc """
  Same as `chgrp/2`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def chgrp!(file, gid) do
    case chgrp(file, gid) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "change group for", path: :unicode.characters_to_binary(file)
    end
  end

  @doc """
  Changes the owner given by the user id `gid`
  for a given `file`. Returns `:ok` on success,
  or `{:error, reason}` on failure.
  """
  def chown(file, uid) do
    F.change_owner(file, uid)
  end

  @doc """
  Same as `chown/2`, but raises an exception in case of failure. Otherwise `:ok`.
  """
  def chown!(file, gid) do
    case chown(file, gid) do
      :ok -> :ok
      { :error, reason } ->
        raise File.Error, reason: reason, action: "change owner for", path: :unicode.characters_to_binary(file)
    end
  end

  ## Helpers

  defp open_defaults([:charlist|t], _add_binary) do
    open_defaults(t, false)
  end

  defp open_defaults([:utf8|t], add_binary) do
    open_defaults([{ :encoding, :utf8 }|t], add_binary)
  end

  defp open_defaults([h|t], add_binary) do
    [h|open_defaults(t, add_binary)]
  end

  defp open_defaults([], true),  do: [:binary]
  defp open_defaults([], false), do: []
end
