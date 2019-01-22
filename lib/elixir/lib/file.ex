defmodule File do
  @moduledoc ~S"""
  This module contains functions to manipulate files.

  Some of those functions are low-level, allowing the user
  to interact with files or IO devices, like `open/2`,
  `copy/3` and others. This module also provides higher
  level functions that work with filenames and have their naming
  based on UNIX variants. For example, one can copy a file
  via `cp/3` and remove files and directories recursively
  via `rm_rf/1`.

  Paths given to functions in this module can be either relative to the
  current working directory (as returned by `File.cwd/0`), or absolute
  paths. Shell conventions like `~` are not expanded automatically.
  To use paths like `~/Downloads`, you can use `Path.expand/1` or
  `Path.expand/2` to expand your path to an absolute path.

  ## Encoding

  In order to write and read files, one must use the functions
  in the `IO` module. By default, a file is opened in binary mode,
  which requires the functions `IO.binread/2` and `IO.binwrite/2`
  to interact with the file. A developer may pass `:utf8` as an
  option when opening the file, then the slower `IO.read/2` and
  `IO.write/2` functions must be used as they are responsible for
  doing the proper conversions and providing the proper data guarantees.

  Note that filenames when given as charlists in Elixir are
  always treated as UTF-8. In particular, we expect that the
  shell and the operating system are configured to use UTF-8
  encoding. Binary filenames are considered raw and passed
  to the OS as is.

  ## API

  Most of the functions in this module return `:ok` or
  `{:ok, result}` in case of success, `{:error, reason}`
  otherwise. Those functions also have a variant
  that ends with `!` which returns the result (instead of the
  `{:ok, result}` tuple) in case of success or raises an
  exception in case it fails. For example:

      File.read("hello.txt")
      #=> {:ok, "World"}

      File.read("invalid.txt")
      #=> {:error, :enoent}

      File.read!("hello.txt")
      #=> "World"

      File.read!("invalid.txt")
      #=> raises File.Error

  In general, a developer should use the former in case they want
  to react if the file does not exist. The latter should be used
  when the developer expects their software to fail in case the
  file cannot be read (i.e. it is literally an exception).

  ## Processes and raw files

  Every time a file is opened, Elixir spawns a new process. Writing
  to a file is equivalent to sending messages to the process that
  writes to the file descriptor.

  This means files can be passed between nodes and message passing
  guarantees they can write to the same file in a network.

  However, you may not always want to pay the price for this abstraction.
  In such cases, a file can be opened in `:raw` mode. The options `:read_ahead`
  and `:delayed_write` are also useful when operating on large files or
  working with files in tight loops.

  Check `:file.open/2` for more information about such options and
  other performance considerations.
  """

  @type posix :: :file.posix()
  @type io_device :: :file.io_device()
  @type stat_options :: [time: :local | :universal | :posix]
  @type mode ::
          :append
          | :binary
          | :charlist
          | :compressed
          | :delayed_write
          | :exclusive
          | :raw
          | :read
          | :read_ahead
          | :sync
          | :write
          | {:read_ahead, pos_integer}
          | {:delayed_write, non_neg_integer, non_neg_integer}
          | encoding_mode()

  @type encoding_mode ::
          :utf8
          | {
              :encoding,
              :latin1
              | :unicode
              | :utf8
              | :utf16
              | :utf32
              | {:utf16, :big | :little}
              | {:utf32, :big | :little}
            }

  @type stream_mode ::
          encoding_mode()
          | :trim_bom
          | {:read_ahead, pos_integer | false}
          | {:delayed_write, non_neg_integer, non_neg_integer}

  @type erlang_time ::
          {{year :: non_neg_integer(), month :: 1..12, day :: 1..31},
           {hour :: 0..23, minute :: 0..59, second :: 0..59}}

  @type posix_time :: integer()

  @doc """
  Returns `true` if the path is a regular file.

  This function follows symbolic links, so if a symbolic link points to a
  regular file, `true` is returned.

  ## Options

  The supported options are:

    * `:raw` - a single atom to bypass the file server and only check
      for the file locally

  ## Examples

      File.regular?(__ENV__.file)
      #=> true

  """
  @spec regular?(Path.t(), [regular_option]) :: boolean
        when regular_option: :raw
  def regular?(path, opts \\ []) do
    :elixir_utils.read_file_type(IO.chardata_to_string(path), opts) == {:ok, :regular}
  end

  @doc """
  Returns `true` if the given path is a directory.

  This function follows symbolic links, so if a symbolic link points to a
  directory, `true` is returned.

  ## Options

  The supported options are:

    * `:raw` - a single atom to bypass the file server and only check
      for the file locally

  ## Examples

      File.dir?("./test")
      #=> true

      File.dir?("test")
      #=> true

      File.dir?("/usr/bin")
      #=> true

      File.dir?("~/Downloads")
      #=> false

      "~/Downloads" |> Path.expand() |> File.dir?()
      #=> true

  """
  @spec dir?(Path.t(), [dir_option]) :: boolean
        when dir_option: :raw
  def dir?(path, opts \\ []) do
    :elixir_utils.read_file_type(IO.chardata_to_string(path), opts) == {:ok, :directory}
  end

  @doc """
  Returns `true` if the given path exists.

  It can be a regular file, directory, socket, symbolic link, named pipe, or device file.
  Returns `false` for symbolic links pointing to non-existing targets.

  ## Options

  The supported options are:

    * `:raw` - a single atom to bypass the file server and only check
      for the file locally

  ## Examples

      File.exists?("test/")
      #=> true

      File.exists?("missing.txt")
      #=> false

      File.exists?("/dev/null")
      #=> true

  """
  @spec exists?(Path.t(), [exists_option]) :: boolean
        when exists_option: :raw
  def exists?(path, opts \\ []) do
    opts = [{:time, :posix}] ++ opts
    match?({:ok, _}, :file.read_file_info(IO.chardata_to_string(path), opts))
  end

  @doc """
  Tries to create the directory `path`.

  Missing parent directories are not created.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

    * `:eacces`  - missing search or write permissions for the parent
      directories of `path`
    * `:eexist`  - there is already a file or directory named `path`
    * `:enoent`  - a component of `path` does not exist
    * `:enospc`  - there is no space left on the device
    * `:enotdir` - a component of `path` is not a directory;
      on some platforms, `:enoent` is returned instead

  """
  @spec mkdir(Path.t()) :: :ok | {:error, posix}
  def mkdir(path) do
    :file.make_dir(IO.chardata_to_string(path))
  end

  @doc """
  Same as `mkdir/1`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec mkdir!(Path.t()) :: :ok
  def mkdir!(path) do
    case mkdir(path) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "make directory",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Tries to create the directory `path`.

  Missing parent directories are created. Returns `:ok` if successful, or
  `{:error, reason}` if an error occurs.

  Typical error reasons are:

    * `:eacces`  - missing search or write permissions for the parent
      directories of `path`
    * `:enospc`  - there is no space left on the device
    * `:enotdir` - a component of `path` is not a directory

  """
  @spec mkdir_p(Path.t()) :: :ok | {:error, posix}
  def mkdir_p(path) do
    do_mkdir_p(IO.chardata_to_string(path))
  end

  defp do_mkdir_p("/") do
    :ok
  end

  defp do_mkdir_p(path) do
    if dir?(path) do
      :ok
    else
      parent = Path.dirname(path)

      if parent == path do
        # Protect against infinite loop
        {:error, :einval}
      else
        _ = do_mkdir_p(parent)

        case :file.make_dir(path) do
          {:error, :eexist} = error ->
            if dir?(path), do: :ok, else: error

          other ->
            other
        end
      end
    end
  end

  @doc """
  Same as `mkdir_p/1`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec mkdir_p!(Path.t()) :: :ok
  def mkdir_p!(path) do
    case mkdir_p(path) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "make directory (with -p)",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Returns `{:ok, binary}`, where `binary` is a binary data object that contains the contents
  of `path`, or `{:error, reason}` if an error occurs.

  Typical error reasons:

    * `:enoent`  - the file does not exist
    * `:eacces`  - missing permission for reading the file,
      or for searching one of the parent directories
    * `:eisdir`  - the named file is a directory
    * `:enotdir` - a component of the file name is not a directory;
      on some platforms, `:enoent` is returned instead
    * `:enomem`  - there is not enough memory for the contents of the file

  You can use `:file.format_error/1` to get a descriptive string of the error.
  """
  @spec read(Path.t()) :: {:ok, binary} | {:error, posix}
  def read(path) do
    :file.read_file(IO.chardata_to_string(path))
  end

  @doc """
  Returns a binary with the contents of the given filename,
  or raises a `File.Error` exception if an error occurs.
  """
  @spec read!(Path.t()) :: binary
  def read!(path) do
    case read(path) do
      {:ok, binary} ->
        binary

      {:error, reason} ->
        raise File.Error, reason: reason, action: "read file", path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Returns information about the `path`. If it exists, it
  returns a `{:ok, info}` tuple, where info is a
  `File.Stat` struct. Returns `{:error, reason}` with
  the same reasons as `read/1` if a failure occurs.

  ## Options

  The accepted options are:

    * `:time` - configures how the file timestamps are returned

  The values for `:time` can be:

    * `:universal` - returns a `{date, time}` tuple in UTC (default)
    * `:local` - returns a `{date, time}` tuple using the same time zone as the
      machine
    * `:posix` - returns the time as integer seconds since epoch

  """
  @spec stat(Path.t(), stat_options) :: {:ok, File.Stat.t()} | {:error, posix}
  def stat(path, opts \\ []) do
    opts = Keyword.put_new(opts, :time, :universal)

    case :file.read_file_info(IO.chardata_to_string(path), opts) do
      {:ok, fileinfo} ->
        {:ok, File.Stat.from_record(fileinfo)}

      error ->
        error
    end
  end

  @doc """
  Same as `stat/2` but returns the `File.Stat` directly,
  or raises a `File.Error` exception if an error is returned.
  """
  @spec stat!(Path.t(), stat_options) :: File.Stat.t()
  def stat!(path, opts \\ []) do
    case stat(path, opts) do
      {:ok, info} ->
        info

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "read file stats",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Returns information about the `path`. If the file is a symlink, sets
  the `type` to `:symlink` and returns a `File.Stat` struct for the link. For any
  other file, returns exactly the same values as `stat/2`.

  For more details, see `:file.read_link_info/2`.

  ## Options

  The accepted options are:

    * `:time` - configures how the file timestamps are returned

  The values for `:time` can be:

    * `:universal` - returns a `{date, time}` tuple in UTC (default)
    * `:local` - returns a `{date, time}` tuple using the machine time
    * `:posix` - returns the time as integer seconds since epoch

  """
  @spec lstat(Path.t(), stat_options) :: {:ok, File.Stat.t()} | {:error, posix}
  def lstat(path, opts \\ []) do
    opts = Keyword.put_new(opts, :time, :universal)

    case :file.read_link_info(IO.chardata_to_string(path), opts) do
      {:ok, fileinfo} ->
        {:ok, File.Stat.from_record(fileinfo)}

      error ->
        error
    end
  end

  @doc """
  Same as `lstat/2` but returns the `File.Stat` struct directly,
  or raises a `File.Error` exception if an error is returned.
  """
  @spec lstat!(Path.t(), stat_options) :: File.Stat.t()
  def lstat!(path, opts \\ []) do
    case lstat(path, opts) do
      {:ok, info} ->
        info

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "read file stats",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Reads the symbolic link at `path`.

  If `path` exists and is a symlink, returns `{:ok, target}`, otherwise returns
  `{:error, reason}`.

  For more details, see `:file.read_link/1`.

  Typical error reasons are:

    * `:einval` - path is not a symbolic link
    * `:enoent` - path does not exist
    * `:enotsup` - symbolic links are not supported on the current platform

  """
  @doc since: "1.5.0"
  @spec read_link(Path.t()) :: {:ok, binary} | {:error, posix}
  def read_link(path) do
    case path |> IO.chardata_to_string() |> :file.read_link() do
      {:ok, target} -> {:ok, IO.chardata_to_string(target)}
      error -> error
    end
  end

  @doc """
  Same as `read_link/1` but returns the target directly,
  or raises a `File.Error` exception if an error is returned.
  """
  @doc since: "1.5.0"
  @spec read_link!(Path.t()) :: binary
  def read_link!(path) do
    case read_link(path) do
      {:ok, resolved} ->
        resolved

      {:error, reason} ->
        raise File.Error, reason: reason, action: "read link", path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Writes the given `File.Stat` back to the file system at the given
  path. Returns `:ok` or `{:error, reason}`.
  """
  @spec write_stat(Path.t(), File.Stat.t(), stat_options) :: :ok | {:error, posix}
  def write_stat(path, stat, opts \\ []) do
    opts = Keyword.put_new(opts, :time, :universal)
    :file.write_file_info(IO.chardata_to_string(path), File.Stat.to_record(stat), opts)
  end

  @doc """
  Same as `write_stat/3` but raises a `File.Error` exception if it fails.
  Returns `:ok` otherwise.
  """
  @spec write_stat!(Path.t(), File.Stat.t(), stat_options) :: :ok
  def write_stat!(path, stat, opts \\ []) do
    case write_stat(path, stat, opts) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "write file stats",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Updates modification time (mtime) and access time (atime) of
  the given file.

  The file is created if it doesn't exist. Requires datetime in UTC
  (as returned by `:erlang.universaltime()`) or an integer
  representing the POSIX timestamp (as returned by `System.os_time(:second)`).

  ## Examples

      File.touch("/tmp/a.txt", {{2018, 1, 30}, {13, 59, 59}})
      #=> :ok
      File.touch("/fakedir/b.txt", {{2018, 1, 30}, {13, 59, 59}})
      {:error, :enoent}

      File.touch("/tmp/a.txt", 1544519753)
      #=> :ok

  """
  @spec touch(Path.t(), erlang_time() | posix_time()) :: :ok | {:error, posix}
  def touch(path, time \\ System.os_time(:second))

  def touch(path, time) when is_tuple(time) do
    path = IO.chardata_to_string(path)

    with {:error, :enoent} <- :elixir_utils.change_universal_time(path, time),
         :ok <- write(path, "", [:append]),
         do: :elixir_utils.change_universal_time(path, time)
  end

  def touch(path, time) when is_integer(time) do
    path = IO.chardata_to_string(path)

    with {:error, :enoent} <- :elixir_utils.change_posix_time(path, time),
         :ok <- write(path, "", [:append]),
         do: :elixir_utils.change_posix_time(path, time)
  end

  @doc """
  Same as `touch/2` but raises a `File.Error` exception if it fails.
  Returns `:ok` otherwise.

  The file is created if it doesn't exist. Requires datetime in UTC
  (as returned by `:erlang.universaltime()`) or an integer
  representing the POSIX timestamp (as returned by `System.os_time(:second)`).

  ## Examples

      File.touch!("/tmp/a.txt", {{2018, 1, 30}, {13, 59, 59}})
      #=> :ok
      File.touch!("/fakedir/b.txt", {{2018, 1, 30}, {13, 59, 59}})
      #=> ** (File.Error) could not touch "/fakedir/b.txt": no such file or directory

      File.touch!("/tmp/a.txt", 1544519753)

  """
  @spec touch!(Path.t(), erlang_time() | posix_time()) :: :ok
  def touch!(path, time \\ System.os_time(:second)) do
    case touch(path, time) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error, reason: reason, action: "touch", path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Creates a hard link `new` to the file `existing`.

  Returns `:ok` if successful, `{:error, reason}` otherwise.
  If the operating system does not support hard links, returns
  `{:error, :enotsup}`.
  """
  @doc since: "1.5.0"
  @spec ln(Path.t(), Path.t()) :: :ok | {:error, posix}
  def ln(existing, new) do
    :file.make_link(IO.chardata_to_string(existing), IO.chardata_to_string(new))
  end

  @doc """
  Same as `ln/2` but raises a `File.LinkError` exception if it fails.
  Returns `:ok` otherwise.
  """
  @doc since: "1.5.0"
  @spec ln!(Path.t(), Path.t()) :: :ok
  def ln!(existing, new) do
    case ln(existing, new) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.LinkError,
          reason: reason,
          action: "create hard link",
          existing: IO.chardata_to_string(existing),
          new: IO.chardata_to_string(new)
    end
  end

  @doc """
  Creates a symbolic link `new` to the file or directory `existing`.

  Returns `:ok` if successful, `{:error, reason}` otherwise.
  If the operating system does not support symlinks, returns
  `{:error, :enotsup}`.
  """
  @doc since: "1.5.0"
  @spec ln_s(Path.t(), Path.t()) :: :ok | {:error, posix}
  def ln_s(existing, new) do
    :file.make_symlink(IO.chardata_to_string(existing), IO.chardata_to_string(new))
  end

  @doc """
  Same as `ln_s/2` but raises a `File.LinkError` exception if it fails.
  Returns `:ok` otherwise.
  """
  @spec ln_s!(Path.t(), Path.t()) :: :ok
  def ln_s!(existing, new) do
    case ln_s(existing, new) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.LinkError,
          reason: reason,
          action: "create symlink",
          existing: IO.chardata_to_string(existing),
          new: IO.chardata_to_string(new)
    end
  end

  @doc """
  Copies the contents of `source` to `destination`.

  Both parameters can be a filename or an IO device opened
  with `open/2`. `bytes_count` specifies the number of
  bytes to copy, the default being `:infinity`.

  If file `destination` already exists, it is overwritten
  by the contents in `source`.

  Returns `{:ok, bytes_copied}` if successful,
  `{:error, reason}` otherwise.

  Compared to the `cp/3`, this function is more low-level,
  allowing a copy from device to device limited by a number of
  bytes. On the other hand, `cp/3` performs more extensive
  checks on both source and destination and it also preserves
  the file mode after copy.

  Typical error reasons are the same as in `open/2`,
  `read/1` and `write/3`.
  """
  @spec copy(Path.t() | io_device, Path.t() | io_device, pos_integer | :infinity) ::
          {:ok, non_neg_integer} | {:error, posix}
  def copy(source, destination, bytes_count \\ :infinity) do
    :file.copy(maybe_to_string(source), maybe_to_string(destination), bytes_count)
  end

  @doc """
  The same as `copy/3` but raises a `File.CopyError` exception if it fails.
  Returns the `bytes_copied` otherwise.
  """
  @spec copy!(Path.t() | io_device, Path.t() | io_device, pos_integer | :infinity) ::
          non_neg_integer
  def copy!(source, destination, bytes_count \\ :infinity) do
    case copy(source, destination, bytes_count) do
      {:ok, bytes_count} ->
        bytes_count

      {:error, reason} ->
        raise File.CopyError,
          reason: reason,
          action: "copy",
          source: maybe_to_string(source),
          destination: maybe_to_string(destination)
    end
  end

  @doc """
  Renames the `source` file to `destination` file.  It can be used to move files
  (and directories) between directories.  If moving a file, you must fully
  specify the `destination` filename, it is not sufficient to simply specify
  its directory.

  Returns `:ok` in case of success, `{:error, reason}` otherwise.

  Note: The command `mv` in Unix systems behaves differently depending on
  whether `source` is a file and the `destination` is an existing directory.
  We have chosen to explicitly disallow this behaviour.

  ## Examples

      # Rename file "a.txt" to "b.txt"
      File.rename("a.txt", "b.txt")

      # Rename directory "samples" to "tmp"
      File.rename("samples", "tmp")

  """
  @spec rename(Path.t(), Path.t()) :: :ok | {:error, posix}
  def rename(source, destination) do
    :file.rename(source, destination)
  end

  @doc """
  The same as `rename/2` but raises a `File.RenameError` exception if it fails.
  Returns `:ok` otherwise.
  """
  @doc since: "1.9.0"
  @spec rename!(Path.t(), Path.t()) :: :ok
  def rename!(source, destination) do
    case rename(source, destination) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.RenameError,
          reason: reason,
          action: "rename",
          source: IO.chardata_to_string(source),
          destination: IO.chardata_to_string(destination)
    end
  end

  @doc """
  Copies the contents in `source_file` to `destination_file` preserving its modes.

  `source_file` and `destination_file` must be a file or a symbolic link to one,
  or in the case of destination, a path to a non-existent file. If either one of
  them is a directory, `{:error, :eisdir}` will be returned.

  If a file already exists in the destination, it invokes a
  callback which should return `true` if the existing file
  should be overwritten, `false` otherwise. The callback defaults to return `true`.

  The function returns `:ok` in case of success. Otherwise, it returns
  `{:error, reason}`.

  If you want to copy contents from an IO device to another device
  or do a straight copy from a source to a destination without
  preserving modes, check `copy/3` instead.

  Note: The command `cp` in Unix systems behaves differently depending on
  whether the destination is an existing directory or not. We have chosen to
  explicitly disallow copying to a destination which is a directory,
  and an error will be returned if tried.
  """
  @spec cp(Path.t(), Path.t(), (Path.t(), Path.t() -> boolean)) :: :ok | {:error, posix}
  def cp(source_file, destination_file, callback \\ fn _, _ -> true end) do
    source_file = IO.chardata_to_string(source_file)
    destination_file = IO.chardata_to_string(destination_file)

    case do_cp_file(source_file, destination_file, callback, []) do
      {:error, reason, _} -> {:error, reason}
      _ -> :ok
    end
  end

  defp path_differs?(path, path), do: false

  defp path_differs?(p1, p2) do
    Path.expand(p1) !== Path.expand(p2)
  end

  @doc """
  The same as `cp/3`, but raises a `File.CopyError` exception if it fails.
  Returns `:ok` otherwise.
  """
  @spec cp!(Path.t(), Path.t(), (Path.t(), Path.t() -> boolean)) :: :ok
  def cp!(source_file, destination_file, callback \\ fn _, _ -> true end) do
    case cp(source_file, destination_file, callback) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.CopyError,
          reason: reason,
          action: "copy",
          source: IO.chardata_to_string(source_file),
          destination: IO.chardata_to_string(destination_file)
    end
  end

  @doc ~S"""
  Copies the contents in `source` to `destination` recursively, maintaining the
  source directory structure and modes.

  If `source` is a file or a symbolic link to it, `destination` must be a path
  to an existent file, a symbolic link to one, or a path to a non-existent file.

  If `source` is a directory, or a symbolic link to it, then `destination` must
  be an existent `directory` or a symbolic link to one, or a path to a non-existent directory.

  If the source is a file, it copies `source` to
  `destination`. If the `source` is a directory, it copies
  the contents inside source into the `destination` directory.

  If a file already exists in the destination, it invokes `callback`.
  `callback` must be a function that takes two arguments: `source` and `destination`.
  The callback should return `true` if the existing file should be overwritten and `false` otherwise.

  This function may fail while copying files,
  in such cases, it will leave the destination
  directory in a dirty state, where file which have already been copied
  won't be removed.

  The function returns `{:ok, files_and_directories}` in case of
  success, `files_and_directories` lists all files and directories copied in no
  specific order. It returns `{:error, reason, file}` otherwise.

  Note: The command `cp` in Unix systems behaves differently depending on
  whether `destination` is an existing directory or not. We have chosen to
  explicitly disallow this behaviour. If `source` is a `file` and `destination`
  is a directory, `{:error, :eisdir}` will be returned.

  ## Examples

      # Copies file "a.txt" to "b.txt"
      File.cp_r("a.txt", "b.txt")

      # Copies all files in "samples" to "tmp"
      File.cp_r("samples", "tmp")

      # Same as before, but asks the user how to proceed in case of conflicts
      File.cp_r("samples", "tmp", fn source, destination ->
        IO.gets("Overwriting #{destination} by #{source}. Type y to confirm. ") == "y\n"
      end)

  """
  @spec cp_r(Path.t(), Path.t(), (Path.t(), Path.t() -> boolean)) ::
          {:ok, [binary]} | {:error, posix, binary}
  def cp_r(source, destination, callback \\ fn _, _ -> true end) when is_function(callback, 2) do
    source =
      source
      |> IO.chardata_to_string()
      |> assert_no_null_byte!("File.cp_r/3")

    destination =
      destination
      |> IO.chardata_to_string()
      |> assert_no_null_byte!("File.cp_r/3")

    case do_cp_r(source, destination, callback, []) do
      {:error, _, _} = error -> error
      res -> {:ok, res}
    end
  end

  @doc """
  The same as `cp_r/3`, but raises a `File.CopyError` exception if it fails.
  Returns the list of copied files otherwise.
  """
  @spec cp_r!(Path.t(), Path.t(), (Path.t(), Path.t() -> boolean)) :: [binary]
  def cp_r!(source, destination, callback \\ fn _, _ -> true end) do
    case cp_r(source, destination, callback) do
      {:ok, files} ->
        files

      {:error, reason, file} ->
        raise File.CopyError,
          reason: reason,
          action: "copy recursively",
          on: file,
          source: IO.chardata_to_string(source),
          destination: IO.chardata_to_string(destination)
    end
  end

  defp do_cp_r(src, dest, callback, acc) when is_list(acc) do
    case :elixir_utils.read_link_type(src) do
      {:ok, :regular} ->
        do_cp_file(src, dest, callback, acc)

      {:ok, :symlink} ->
        case :file.read_link(src) do
          {:ok, link} -> do_cp_link(link, src, dest, callback, acc)
          {:error, reason} -> {:error, reason, src}
        end

      {:ok, :directory} ->
        case :file.list_dir(src) do
          {:ok, files} ->
            case mkdir(dest) do
              success when success in [:ok, {:error, :eexist}] ->
                Enum.reduce(files, [dest | acc], fn x, acc ->
                  do_cp_r(Path.join(src, x), Path.join(dest, x), callback, acc)
                end)

              {:error, reason} ->
                {:error, reason, dest}
            end

          {:error, reason} ->
            {:error, reason, src}
        end

      {:ok, _} ->
        {:error, :eio, src}

      {:error, reason} ->
        {:error, reason, src}
    end
  end

  # If we reach this clause, there was an error while
  # processing a file.
  defp do_cp_r(_, _, _, acc) do
    acc
  end

  defp copy_file_mode!(src, dest) do
    write_stat!(dest, %{stat!(dest) | mode: stat!(src).mode})
  end

  # Both src and dest are files.
  defp do_cp_file(src, dest, callback, acc) do
    case :file.copy(src, {dest, [:exclusive]}) do
      {:ok, _} ->
        copy_file_mode!(src, dest)
        [dest | acc]

      {:error, :eexist} ->
        if path_differs?(src, dest) and callback.(src, dest) do
          case copy(src, dest) do
            {:ok, _} ->
              copy_file_mode!(src, dest)
              [dest | acc]

            {:error, reason} ->
              {:error, reason, src}
          end
        else
          acc
        end

      {:error, reason} ->
        {:error, reason, src}
    end
  end

  # Both src and dest are files.
  defp do_cp_link(link, src, dest, callback, acc) do
    case :file.make_symlink(link, dest) do
      :ok ->
        [dest | acc]

      {:error, :eexist} ->
        if path_differs?(src, dest) and callback.(src, dest) do
          # If rm/1 fails, :file.make_symlink/2 will fail
          _ = rm(dest)

          case :file.make_symlink(link, dest) do
            :ok -> [dest | acc]
            {:error, reason} -> {:error, reason, src}
          end
        else
          acc
        end

      {:error, reason} ->
        {:error, reason, src}
    end
  end

  @doc """
  Writes `content` to the file `path`.

  The file is created if it does not exist. If it exists, the previous
  contents are overwritten. Returns `:ok` if successful, or `{:error, reason}`
  if an error occurs.

  `content` must be `iodata` (a list of bytes or a binary). Setting the
  encoding for this function has no effect.

  **Warning:** Every time this function is invoked, a file descriptor is opened
  and a new process is spawned to write to the file. For this reason, if you are
  doing multiple writes in a loop, opening the file via `File.open/2` and using
  the functions in `IO` to write to the file will yield much better performance
  than calling this function multiple times.

  Typical error reasons are:

    * `:enoent`  - a component of the file name does not exist
    * `:enotdir` - a component of the file name is not a directory;
      on some platforms, `:enoent` is returned instead
    * `:enospc`  - there is no space left on the device
    * `:eacces`  - missing permission for writing the file or searching one of
      the parent directories
    * `:eisdir`  - the named file is a directory

  Check `File.open/2` for other available options.
  """
  @spec write(Path.t(), iodata, [mode]) :: :ok | {:error, posix}
  def write(path, content, modes \\ []) do
    modes = normalize_modes(modes, false)
    :file.write_file(IO.chardata_to_string(path), content, modes)
  end

  @doc """
  Same as `write/3` but raises a `File.Error` exception if it fails.
  Returns `:ok` otherwise.
  """
  @spec write!(Path.t(), iodata, [mode]) :: :ok
  def write!(path, content, modes \\ []) do
    modes = normalize_modes(modes, false)

    case :file.write_file(path, content, modes) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "write to file",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Tries to delete the file `path`.

  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Note the file is deleted even if in read-only mode.

  Typical error reasons are:

    * `:enoent`  - the file does not exist
    * `:eacces`  - missing permission for the file or one of its parents
    * `:eperm`   - the file is a directory and user is not super-user
    * `:enotdir` - a component of the file name is not a directory;
      on some platforms, `:enoent` is returned instead
    * `:einval`  - filename had an improper type, such as tuple

  ## Examples

      File.rm("file.txt")
      #=> :ok

      File.rm("tmp_dir/")
      #=> {:error, :eperm}

  """
  @spec rm(Path.t()) :: :ok | {:error, posix}
  def rm(path) do
    path = IO.chardata_to_string(path)

    case :file.delete(path) do
      :ok ->
        :ok

      {:error, :eacces} = e ->
        change_mode_windows(path) || e

      {:error, _} = e ->
        e
    end
  end

  defp change_mode_windows(path) do
    if match?({:win32, _}, :os.type()) do
      case :file.read_file_info(path) do
        {:ok, file_info} when elem(file_info, 3) in [:read, :none] ->
          change_mode_windows(path, file_info)

        _ ->
          nil
      end
    end
  end

  defp change_mode_windows(path, file_info) do
    case chmod(path, elem(file_info, 7) + 0o200) do
      :ok -> :file.delete(path)
      {:error, _reason} = error -> error
    end
  end

  @doc """
  Same as `rm/1`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec rm!(Path.t()) :: :ok
  def rm!(path) do
    case rm(path) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error, reason: reason, action: "remove file", path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Tries to delete the dir at `path`.

  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.
  It returns `{:error, :eexist}` if the directory is not empty.

  ## Examples

      File.rmdir("tmp_dir")
      #=> :ok

      File.rmdir("non_empty_dir")
      #=> {:error, :eexist}

      File.rmdir("file.txt")
      #=> {:error, :enotdir}

  """
  @spec rmdir(Path.t()) :: :ok | {:error, posix}
  def rmdir(path) do
    :file.del_dir(IO.chardata_to_string(path))
  end

  @doc """
  Same as `rmdir/1`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec rmdir!(Path.t()) :: :ok | {:error, posix}
  def rmdir!(path) do
    case rmdir(path) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "remove directory",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Removes files and directories recursively at the given `path`.
  Symlinks are not followed but simply removed, non-existing
  files are simply ignored (i.e. doesn't make this function fail).

  Returns `{:ok, files_and_directories}` with all files and
  directories removed in no specific order, `{:error, reason, file}`
  otherwise.

  ## Examples

      File.rm_rf("samples")
      #=> {:ok, ["samples", "samples/1.txt"]}

      File.rm_rf("unknown")
      #=> {:ok, []}

  """
  @spec rm_rf(Path.t()) :: {:ok, [binary]} | {:error, posix, binary}
  def rm_rf(path) do
    path
    |> IO.chardata_to_string()
    |> assert_no_null_byte!("File.rm_rf/1")
    |> do_rm_rf({:ok, []})
  end

  defp do_rm_rf(path, {:ok, _} = entry) do
    case safe_list_dir(path) do
      {:ok, files} when is_list(files) ->
        res =
          Enum.reduce(files, entry, fn file, tuple ->
            do_rm_rf(Path.join(path, file), tuple)
          end)

        case res do
          {:ok, acc} ->
            case rmdir(path) do
              :ok -> {:ok, [path | acc]}
              {:error, :enoent} -> res
              {:error, reason} -> {:error, reason, path}
            end

          reason ->
            reason
        end

      {:ok, :directory} ->
        do_rm_directory(path, entry)

      {:ok, :regular} ->
        do_rm_regular(path, entry)

      {:error, reason} when reason in [:enoent, :enotdir] ->
        entry

      {:error, reason} ->
        {:error, reason, path}
    end
  end

  defp do_rm_rf(_, reason) do
    reason
  end

  defp do_rm_regular(path, {:ok, acc} = entry) do
    case rm(path) do
      :ok -> {:ok, [path | acc]}
      {:error, :enoent} -> entry
      {:error, reason} -> {:error, reason, path}
    end
  end

  # On Windows, symlinks are treated as directory and must be removed
  # with rmdir/1. But on Unix, we remove them via rm/1. So we first try
  # to remove it as a directory and, if we get :enotdir, we fall back to
  # a file removal.
  defp do_rm_directory(path, {:ok, acc} = entry) do
    case rmdir(path) do
      :ok -> {:ok, [path | acc]}
      {:error, :enotdir} -> do_rm_regular(path, entry)
      {:error, :enoent} -> entry
      {:error, reason} -> {:error, reason, path}
    end
  end

  defp safe_list_dir(path) do
    case :elixir_utils.read_link_type(path) do
      {:ok, :symlink} ->
        case :elixir_utils.read_file_type(path) do
          {:ok, :directory} -> {:ok, :directory}
          _ -> {:ok, :regular}
        end

      {:ok, :directory} ->
        :file.list_dir(path)

      {:ok, _} ->
        {:ok, :regular}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Same as `rm_rf/1` but raises a `File.Error` exception in case of failures,
  otherwise the list of files or directories removed.
  """
  @spec rm_rf!(Path.t()) :: [binary]
  def rm_rf!(path) do
    case rm_rf(path) do
      {:ok, files} ->
        files

      {:error, reason, _} ->
        raise File.Error,
          reason: reason,
          path: IO.chardata_to_string(path),
          action: "remove files and directories recursively from"
    end
  end

  @doc ~S"""
  Opens the given `path`.

  In order to write and read files, one must use the functions
  in the `IO` module. By default, a file is opened in `:binary` mode,
  which requires the functions `IO.binread/2` and `IO.binwrite/2`
  to interact with the file. A developer may pass `:utf8` as an
  option when opening the file and then all other functions from
  `IO` are available, since they work directly with Unicode data.

  `modes_or_function` can either be a list of modes or a function. If it's a
  list, it's considered to be a list of modes (that are documented below). If
  it's a function, then it's equivalent to calling `open(path, [],
  modes_or_function)`. See the documentation for `open/3` for more information
  on this function.

  The allowed modes:

    * `:binary` - opens the file in binary mode, disabling special handling of unicode sequences
      (default mode).

    * `:read` - the file, which must exist, is opened for reading.

    * `:write` - the file is opened for writing. It is created if it does not
      exist.

      If the file does exists, and if write is not combined with read, the file
      will be truncated.

    * `:append` - the file will be opened for writing, and it will be created
      if it does not exist. Every write operation to a file opened with append
      will take place at the end of the file.

    * `:exclusive` - the file, when opened for writing, is created if it does
      not exist. If the file exists, open will return `{:error, :eexist}`.

    * `:charlist` - when this term is given, read operations on the file will
      return charlists rather than binaries.

    * `:compressed` - makes it possible to read or write gzip compressed files.

      The compressed option must be combined with either read or write, but not
      both. Note that the file size obtained with `stat/1` will most probably
      not match the number of bytes that can be read from a compressed file.

    * `:utf8` - this option denotes how data is actually stored in the disk
      file and makes the file perform automatic translation of characters to
      and from UTF-8.

      If data is sent to a file in a format that cannot be converted to the
      UTF-8 or if data is read by a function that returns data in a format that
      cannot cope with the character range of the data, an error occurs and the
      file will be closed.

    * `:delayed_write`, `:raw`, `:ram`, `:read_ahead`, `:sync`, `{:encoding, ...}`,
      `{:read_ahead, pos_integer}`, `{:delayed_write, non_neg_integer, non_neg_integer}` -
      for more information about these options see `:file.open/2`.

  This function returns:

    * `{:ok, io_device}` - the file has been opened in the requested mode.

      `io_device` is actually the PID of the process which handles the file.
      This process is linked to the process which originally opened the file.
      If any process to which the `io_device` is linked terminates, the file
      will be closed and the process itself will be terminated.

      An `io_device` returned from this call can be used as an argument to the
      `IO` module functions.

    * `{:error, reason}` - the file could not be opened.

  ## Examples

      {:ok, file} = File.open("foo.tar.gz", [:read, :compressed])
      IO.read(file, :line)
      File.close(file)

  """
  @spec open(Path.t(), [mode | :ram]) :: {:ok, io_device} | {:error, posix}
  @spec open(Path.t(), (io_device -> res)) :: {:ok, res} | {:error, posix} when res: var
  def open(path, modes_or_function \\ [])

  def open(path, modes) when is_list(modes) do
    :file.open(IO.chardata_to_string(path), normalize_modes(modes, true))
  end

  def open(path, function) when is_function(function, 1) do
    open(path, [], function)
  end

  @doc """
  Similar to `open/2` but expects a function as its last argument.

  The file is opened, given to the function as an argument and
  automatically closed after the function returns, regardless
  if there was an error when executing the function.

  Returns `{:ok, function_result}` in case of success,
  `{:error, reason}` otherwise.

  This function expects the file to be closed with success,
  which is usually the case unless the `:delayed_write` option
  is given. For this reason, we do not recommend passing
  `:delayed_write` to this function.

  ## Examples

      File.open("file.txt", [:read, :write], fn file ->
        IO.read(file, :line)
      end)

  See `open/2` for the list of available `modes`.
  """
  @spec open(Path.t(), [mode | :ram], (io_device -> res)) :: {:ok, res} | {:error, posix}
        when res: var
  def open(path, modes, function) when is_list(modes) and is_function(function, 1) do
    case open(path, modes) do
      {:ok, io_device} ->
        try do
          {:ok, function.(io_device)}
        after
          :ok = close(io_device)
        end

      other ->
        other
    end
  end

  @doc """
  Similar to `open/2` but raises a `File.Error` exception if the file
  could not be opened. Returns the IO device otherwise.

  See `open/2` for the list of available modes.
  """
  @spec open!(Path.t(), [mode | :ram]) :: io_device
  @spec open!(Path.t(), (io_device -> res)) :: res when res: var
  def open!(path, modes_or_function \\ []) do
    case open(path, modes_or_function) do
      {:ok, io_device_or_function_result} ->
        io_device_or_function_result

      {:error, reason} ->
        raise File.Error, reason: reason, action: "open", path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Similar to `open/3` but raises a `File.Error` exception if the file
  could not be opened.

  If it succeeds opening the file, it returns the `function` result on the IO device.

  See `open/2` for the list of available `modes`.
  """
  @spec open!(Path.t(), [mode | :ram], (io_device -> res)) :: res when res: var
  def open!(path, modes, function) do
    case open(path, modes, function) do
      {:ok, function_result} ->
        function_result

      {:error, reason} ->
        raise File.Error, reason: reason, action: "open", path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Gets the current working directory.

  In rare circumstances, this function can fail on Unix. It may happen
  if read permissions do not exist for the parent directories of the
  current directory. For this reason, returns `{:ok, cwd}` in case
  of success, `{:error, reason}` otherwise.
  """
  @spec cwd() :: {:ok, binary} | {:error, posix}
  def cwd() do
    case :file.get_cwd() do
      {:ok, base} -> {:ok, IO.chardata_to_string(fix_drive_letter(base))}
      {:error, _} = error -> error
    end
  end

  defp fix_drive_letter([l, ?:, ?/ | rest] = original) when l in ?A..?Z do
    case :os.type() do
      {:win32, _} -> [l + ?a - ?A, ?:, ?/ | rest]
      _ -> original
    end
  end

  defp fix_drive_letter(original), do: original

  @doc """
  The same as `cwd/0`, but raises a `File.Error` exception if it fails.
  """
  @spec cwd!() :: binary
  def cwd!() do
    case cwd() do
      {:ok, cwd} ->
        cwd

      {:error, reason} ->
        raise File.Error, reason: reason, action: "get current working directory"
    end
  end

  @doc """
  Sets the current working directory.

  Returns `:ok` if successful, `{:error, reason}` otherwise.
  """
  @spec cd(Path.t()) :: :ok | {:error, posix}
  def cd(path) do
    :file.set_cwd(IO.chardata_to_string(path))
  end

  @doc """
  The same as `cd/1`, but raises a `File.Error` exception if it fails.
  """
  @spec cd!(Path.t()) :: :ok
  def cd!(path) do
    case cd(path) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "set current working directory to",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Changes the current directory to the given `path`,
  executes the given function and then reverts back
  to the previous path regardless of whether there is an exception.

  Raises an error if retrieving or changing the current
  directory fails.
  """
  @spec cd!(Path.t(), (() -> res)) :: res when res: var
  def cd!(path, function) do
    old = cwd!()
    cd!(path)

    try do
      function.()
    after
      cd!(old)
    end
  end

  @doc """
  Returns the list of files in the given directory.

  Returns `{:ok, files}` in case of success,
  `{:error, reason}` otherwise.
  """
  @spec ls(Path.t()) :: {:ok, [binary]} | {:error, posix}
  def ls(path \\ ".") do
    case :file.list_dir(IO.chardata_to_string(path)) do
      {:ok, file_list} -> {:ok, Enum.map(file_list, &IO.chardata_to_string/1)}
      {:error, _} = error -> error
    end
  end

  @doc """
  The same as `ls/1` but raises a `File.Error` exception in case of an error.
  """
  @spec ls!(Path.t()) :: [binary]
  def ls!(path \\ ".") do
    case ls(path) do
      {:ok, value} ->
        value

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "list directory",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Closes the file referenced by `io_device`. It mostly returns `:ok`, except
  for some severe errors such as out of memory.

  Note that if the option `:delayed_write` was used when opening the file,
  `close/1` might return an old write error and not even try to close the file.
  See `open/2` for more information.
  """
  @spec close(io_device) :: :ok | {:error, posix | :badarg | :terminated}
  def close(io_device) do
    :file.close(io_device)
  end

  @doc """
  Returns a `File.Stream` for the given `path` with the given `modes`.

  The stream implements both `Enumerable` and `Collectable` protocols,
  which means it can be used both for read and write.

  The `line_or_bytes` argument configures how the file is read when
  streaming, by `:line` (default) or by a given number of bytes.

  Operating the stream can fail on open for the same reasons as
  `File.open!/2`. Note that the file is automatically opened each time streaming
  begins. There is no need to pass `:read` and `:write` modes, as those are
  automatically set by Elixir.

  ## Raw files

  Since Elixir controls when the streamed file is opened, the underlying
  device cannot be shared and as such it is convenient to open the file
  in raw mode for performance reasons. Therefore, Elixir **will** open
  streams in `:raw` mode with the `:read_ahead` option unless an encoding
  is specified. This means any data streamed into the file must be
  converted to `t:iodata/0` type. If you pass e.g. `[encoding: :utf8]`
  or `[encoding: {:utf16, :little}]` in the modes parameter,
  the underlying stream will use `IO.write/2` and the `String.Chars` protocol
  to convert the data. See `IO.binwrite/2` and `IO.write/2` .

  One may also consider passing the `:delayed_write` option if the stream
  is meant to be written to under a tight loop.

  ## Byte order marks

  If you pass `:trim_bom` in the modes parameter, the stream will
  trim UTF-8, UTF-16 and UTF-32 byte order marks when reading from file.

  Note that this function does not try to discover the file encoding basing
  on BOM.

  ## Examples

      # Read in 2048 byte chunks rather than lines
      File.stream!("./test/test.data", [], 2048)
      #=> %File.Stream{line_or_bytes: 2048, modes: [:raw, :read_ahead, :binary],
      #=>   path: "./test/test.data", raw: true}

  See `Stream.run/1` for an example of streaming into a file.
  """
  @spec stream!(Path.t(), stream_mode, :line | pos_integer) :: File.Stream.t()
  def stream!(path, modes \\ [], line_or_bytes \\ :line) do
    modes = normalize_modes(modes, true)
    File.Stream.__build__(IO.chardata_to_string(path), modes, line_or_bytes)
  end

  @doc """
  Changes the `mode` for a given `file`.

  Returns `:ok` on success, or `{:error, reason}` on failure.

  ## Permissions

  File permissions are specified by adding together the following octal modes:

    * `0o400` - read permission: owner
    * `0o200` - write permission: owner
    * `0o100` - execute permission: owner

    * `0o040` - read permission: group
    * `0o020` - write permission: group
    * `0o010` - execute permission: group

    * `0o004` - read permission: other
    * `0o002` - write permission: other
    * `0o001` - execute permission: other

  For example, setting the mode `0o755` gives it
  write, read and execute permission to the owner
  and both read and execute permission to group
  and others.
  """
  @spec chmod(Path.t(), non_neg_integer) :: :ok | {:error, posix}
  def chmod(path, mode) do
    :file.change_mode(IO.chardata_to_string(path), mode)
  end

  @doc """
  Same as `chmod/2`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec chmod!(Path.t(), non_neg_integer) :: :ok
  def chmod!(path, mode) do
    case chmod(path, mode) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "change mode for",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Changes the group given by the group id `gid`
  for a given `file`. Returns `:ok` on success, or
  `{:error, reason}` on failure.
  """
  @spec chgrp(Path.t(), non_neg_integer) :: :ok | {:error, posix}
  def chgrp(path, gid) do
    :file.change_group(IO.chardata_to_string(path), gid)
  end

  @doc """
  Same as `chgrp/2`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec chgrp!(Path.t(), non_neg_integer) :: :ok
  def chgrp!(path, gid) do
    case chgrp(path, gid) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "change group for",
          path: IO.chardata_to_string(path)
    end
  end

  @doc """
  Changes the owner given by the user id `uid`
  for a given `file`. Returns `:ok` on success,
  or `{:error, reason}` on failure.
  """
  @spec chown(Path.t(), non_neg_integer) :: :ok | {:error, posix}
  def chown(path, uid) do
    :file.change_owner(IO.chardata_to_string(path), uid)
  end

  @doc """
  Same as `chown/2`, but raises a `File.Error` exception in case of failure.
  Otherwise `:ok`.
  """
  @spec chown!(Path.t(), non_neg_integer) :: :ok
  def chown!(path, uid) do
    case chown(path, uid) do
      :ok ->
        :ok

      {:error, reason} ->
        raise File.Error,
          reason: reason,
          action: "change owner for",
          path: IO.chardata_to_string(path)
    end
  end

  ## Helpers

  @read_ahead_size 64 * 1024

  defp assert_no_null_byte!(binary, operation) do
    case :binary.match(binary, "\0") do
      {_, _} ->
        raise ArgumentError,
              "cannot execute #{operation} for path with null byte, got: #{inspect(binary)}"

      :nomatch ->
        binary
    end
  end

  defp normalize_modes([:utf8 | rest], binary?) do
    [encoding: :utf8] ++ normalize_modes(rest, binary?)
  end

  defp normalize_modes([:read_ahead | rest], binary?) do
    [read_ahead: @read_ahead_size] ++ normalize_modes(rest, binary?)
  end

  # TODO: Remove :char_list mode on v2.0
  defp normalize_modes([mode | rest], _binary?) when mode in [:charlist, :char_list] do
    if mode == :char_list do
      IO.warn("the :char_list mode is deprecated, use :charlist")
    end

    normalize_modes(rest, false)
  end

  defp normalize_modes([mode | rest], binary?) do
    [mode | normalize_modes(rest, binary?)]
  end

  defp normalize_modes([], true), do: [:binary]
  defp normalize_modes([], false), do: []

  defp maybe_to_string(path) when is_list(path), do: IO.chardata_to_string(path)
  defp maybe_to_string(path) when is_binary(path), do: path
  defp maybe_to_string(path), do: path
end
