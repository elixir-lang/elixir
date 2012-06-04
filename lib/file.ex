defmodule File do
  @moduledoc """
  This module contains function to manipulate files,
  filenames and the filesystem. Many of the functions
  that interact with the filesystem have their naming
  based on its UNIX variants. For example, deleting a
  file is done with `File.rm`. Getting its stats with
  `File.stat`. If you want to read or write to a file
  in chunks, check the IO module.
  """

  require Erlang.file,     as: F
  require Erlang.filename, as: FN
  require Erlang.filelib,  as: FL

  defrecord Stat, Record.extract(:file_info, from_lib: "kernel/include/file.hrl"), moduledoc: """
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

  defexception Error, [reason: nil, action: "", path: nil] do
    def message(exception) do
      formatted = list_to_binary(F.format_error(exception.reason))
      "could not #{exception.action} #{exception.path}: #{formatted}"
    end
  end

  @doc """
  Expands the path by returning its absolute name and expanding
  any `.` and `..` characters.

  ## Examples

      File.expand_path("/foo/bar/../bar") == "/foo/bar"

  """
  def expand_path(path) do
    normalize FN.absname(path)
  end

  @doc """
  Expands the path to the relative location and expanding
  any `.` and `..` characters. If the path is already an
  absolute path, the relative location is ignored.

  ## Examples

      File.expand_path("foo/bar/../bar", "/baz") == "/baz/foo/bar"
      File.expand_path("/foo/bar/../bar", "/baz") == "/foo/bar"

  """
  def expand_path(path, relative_to) do
    normalize FN.absname(FN.absname(path, relative_to))
  end

  @doc """
  Returns true if the path is a regular file.

  ## Examples

      File.regular? __FILE__ #=> true

  """
  def regular?(path) do
    FL.is_regular(path)
  end

  @doc """
  Returns true if the path is a directory.
  """
  def dir?(path) do
    FL.is_dir(path)
  end

  @doc """
  Returns true if the given argument exists.
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
  Returns the last component of the path or the path
  itself if it does not contain any directory separators.

  ## Examples

      File.basename("foo")
      #=> "foo"

      File.basename("foo/bar")
      #=> "bar"

      File.basename("/")
      #=> ""

  """
  def basename(path) do
    FN.basename(path)
  end

  @doc """
  Returns the last component of `path` with the `extension`
  stripped. This function should be used to remove a specific
  extension which might, or might not, be there.

  ## Examples

      File.basename("~/foo/bar.ex", ".ex")
      #=> "bar"
      File.basename("~/foo/bar.exs", ".ex")
      #=> "bar.exs"
      File.basename("~/foo/bar.old.ex", ".ex")
      #=> "bar.old"

  """
  def basename(path, extension) do
    FN.basename(path, extension)
  end

  @doc """
  Return the `directory` component of `path`.

  ## Examples

      File.dirname("/foo/bar.ex")
      #=> "foo"

  """
  def dirname(path) do
    FN.dirname(path)
  end

  @doc """
  Return the `extension` of the last component of `path`.

  ## Examples

      File.extname("foo.erl")
      #=> ".erl"
      File.extname("~/foo/bar")
      #=> ""

  """
  def extname(path) do
    FN.extension(path)
  end

  @doc """
  Returns the `path` with the `extension` stripped.

  ## Examples

      File.rootname("/foo/bar")
      #=> "/foo/bar"
      File.rootname("/foo/bar.ex")
      #=> "/foo/bar"

  """
  def rootname(path) do
    FN.rootname(path)
  end

  @doc """
  Returns the `path` with the `extension` stripped. This function should be used to
  remove a specific extension which might, or might not, be there.

  ## Examples

      File.rootname("/foo/bar.erl", ".erl")
      #=> "/foo/bar"
      File.rootname("/foo/bar.erl", ".ex")
      #=> "/foo/bar.erl"

  """
  def rootname(path, extension) do
   FN.rootname(path, extension)
  end

  @doc """
  Returns a string with one or more paths components joint by the path separator.
  This function should be used to convert a list of strings in a path.

  ## Examples

      File.join(["~", "foo"])
      #=> "~/foo"
      File.join(["foo"])
      #=> "foo"
      File.join(["/", "foo", "bar"])
      #=> "/foo/bar"

  """
  def join(paths) do
    FN.join(paths)
  end

  @doc """
  Join two paths.

  ## Examples

      File.join("foo", "bar")
      #=> "foo/bar"

  """
  def join(left, right) do
    FN.join(left, right)
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
  Tries to create the directory `path`. Missing parent directories are created.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

  * :eacces  - Missing search or write permissions for the parent directories of `path`.
  * :enospc  - There is a no space left on the device.
  * :enotdir - A component of `path` is not a directory
               On some platforms, `:enoent` is returned instead.
  """
  def mkdir_p(path) do
    FL.ensure_dir(join(path, "."))
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

  You can use `Erlang.file.format_error(reason)` to get a descriptive string of the error.
  """
  def read(path) do
    F.read_file(path)
  end

  @doc """
  Returns binary with the contents of the given filename or raises
  File.Error if an error occurs.
  """
  def read!(path) do
    case read(path) do
      { :ok, binary } ->
        binary
      { :error, reason } ->
        raise File.Error, reason: reason, action: "read file", path: to_binary(path)
    end
  end

  @doc """
  Returns a list with the path splitted by the path separator.
  If an empty string is given, then it returns the root path.

  ## Examples

       File.split("")
       #=> ["/"]
       File.split("foo")
       #=> ["foo"]
       File.split("/foo/bar")
       #=> ["/", "foo", "bar"]

  """
  def split(path) do
    FN.split(path)
  end

  @doc """
  Traverses files and directories according to the given glob expression.

  The wildcard string looks like an ordinary filename, except that certain
  "wildcard characters" are interpreted in a special way. The following
  characters are special:

  * `?` - Matches one character.
  * `*` - Matches any number of characters up to the end of
          the filename, the next dot, or the next slash.
  * `**` - Two adjacent <c>*</c>'s used as a single pattern will
           match all files and zero or more directories and subdirectories.
  * `[char1,char2,...]` - Matches any of the characters listed. Two characters
                          separated by a hyphen will match a range of characters.
  * `{item1,item2,...}` - Matches one of the alternatives.

  Other characters represent themselves. Only filenames that have exactly
  the same character in the same position will match. Note that matching
  is case-sensitive; i.e. "a" will not match "A".

  ## Examples

  Imagine you have a directory called `projects` with three Elixir projects
  inside of it: `elixir`, `exdoc` and `dynamo`. You can find all `.beam` files
  inside their ebin directories all projects as follows:

      File.wildcard("projects/*/ebin/**/*.beam")

  If you want to search for both `.beam` and `.app` files, you could do:

      File.wildcard("projects/*/ebin/**/*.{beam,app}")

  """
  def wildcard(glob) when is_binary(glob) do
    paths = Erlang.elixir_glob.wildcard binary_to_list(glob)
    Enum.map paths, list_to_binary(&1)
  end

  def wildcard(path) when is_list(path) do
    Erlang.elixir_glob.wildcard(path)
  end

  @doc """
  Returns information about a file. If the file exists, it
  returns a `{ :ok, info }` tuple, where info is  as a
  `File.Info` record. Retuns `{ :error, reason }` with
  the same reasons as `File.read` if a failure occurs.

  ## Options

  The accepted options are:

  * `:time` if the time should be local, universal or posix.
    Default is local.

  """
  def stat(path, opts // []) do
    case :file.read_file_info(path, opts) do
      {:ok, fileinfo} ->
        {:ok, Stat.new fileinfo}
      error ->
        error
    end
  end

  def read_info(path, opts // []) do
    IO.puts "File.read_info is deprecated in favor of File.stat"
    stat(path, opts)
  end

  @doc """
  Same as `stat` but returns the `File.Stat` directly and
  throws `File.Error` if an error is returned.
  """
  def stat!(path, opts // []) do
    case stat(path, opts) do
      {:ok, info} ->
        info
      {:error, reason} ->
        raise File.Error, reason: reason, action: "read file stats", path: to_binary(path)
    end
  end

  @doc """
  Tries to write `content` to the file `filename`. The file is created if it
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
  def write(filename, content, modes // []) do
    F.write_file(filename, content, modes)
  end

  @doc """
  Tries to delete the file `filename`.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

  * :enoent  - The file does not exist.
  * :eacces  - Missing permission for the file or one of its parents.
  * :eperm   - The file is a directory and user is not super-user.
  * :enotdir - A component of the file name is not a directory.
               On some platforms, enoent is returned instead.
  * :einval  - Filename had an improper type, such as tuple.

  ## Examples

      File.rm('foo.txt')
      #=> :ok

      File.rm('tmp_dir/')
      #=> {:error, :eperm}

  """
  def rm(filename) do
    F.delete(filename)
  end

  ## Helpers

  # Normalize the given path by removing "..".
  defp normalize(path), do: normalize(split(path), [])

  defp normalize([top|t], [_|acc]) when top in ["..", '..'] do
    normalize t, acc
  end

  defp normalize([top|t], acc) when top in [".", '.'] do
    normalize t, acc
  end

  defp normalize([h|t], acc) do
    normalize t, [h|acc]
  end

  defp normalize([], acc) do
    join List.reverse(acc)
  end
end
