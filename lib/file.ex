defmodule File do
  require Erlang.file,     as: F
  require Erlang.filename, as: FN
  require Erlang.filelib,  as: FL

  defrecord Info, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

  defexception Exception, reason: nil, action: "", path: nil do
    def message(exception) do
      formatted = list_to_binary(F.format_error(exception.reason))
      "could not #{exception.action} #{exception.path}: #{formatted}"
    end
  end

  def expand_path(path) do
    normalize FN.absname(path)
  end

  def expand_path(path, relative_to) do
    normalize FN.absname(FN.absname(path, relative_to))
  end

  def regular?(filename) do
    FL.is_regular(filename)
  end

  def dir?(directory) do
    FL.is_dir(directory)
  end

  @doc """
  Returns the last component of the `filename` or the file
  name itself if it does not contain any directory separators.

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
  Returns the last component of `filename` with the `extension`
  stripped. This function should be used to remove a specific extension
  which might, or might not, be there.

  ## Examples

      File.basename("~/foo/bar.ex", ".ex")
      #=> "bar"
      File.basename("~/foo/bar.exs", ".ex")
      #=> "bar.ecs"
      File.basename("~/foo/bar.old.ex", ".ex")
      #=> "kalle.old"

  """
  def basename(path, extension) do
    FN.basename(path, extension)
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
  Returns `{:ok, binary}`, where `binary` is a binary data object that contains the contents
  of `filename`, or `{:error, reason}` if an error occurs.

  Typical error reasons:

  * :enoent - The file does not exist.
  * :eacces - Missing permission for reading the file,
              or for searching one of the parent directories.
  * :eisdir - The named file is a directory.
  * :enotdir - A component of the file name is not a directory.
               On some platforms, enoent is returned instead.
  * :enomem - There is not enough memory for the contents of the file.

  You can use `Erlang.file.format_error(reason)` to get a descriptive string of the error.
  """
  def read(filename) do
    F.read_file(filename)
  end

  @doc """
  Returns binary with the contents of the given filename or raises
  File.Exception if an error occurs.
  """
  def read!(filename) do
    result = read(filename)

    case result do
    match: { :ok, binary }
      binary
    match: { :error, reason }
      raise Exception, reason: reason, action: "read file", path: filename
    end
  end

  @doc """
  Returns a list with the path splitted by the path separator. If an empty string
  is given, then it returns the root path.

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
  Returns information about a file. Info is returned
  as a `FileInfo` record containing the following elements:

  * `size` - Size of file in bytes.
  * `type` - `:device`, `:directory`, `:regular`, `:other`. The type of the file.
  * `access` - `:read`, `:write`, `:read_write`, `:none`. The current system access to
               the file.
  * `atime` - The last time the file was read.
  * `mtime` - The last time the file was written.
  * `ctime` - The interpretation of this time field depends on the operating
              system. On Unix, it is the last time the file or the inode was
              changed. in Windows, it is the create time.
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
  def read_info(path, opts // []) do
    case :file.read_file_info(path, opts) do
    match: {:ok, fileinfo}
      {:ok, Info.new fileinfo}
    match: error
      error
    end
  end

  @doc """
  Same as `read_info` but returns only the FileInfo and
  throws an exception if an error occurs.
  """
  def read_info!(path, opts // []) do
    case read_info(path, opts) do
    match: {:ok, info}
      info
    match: {:error, reason}
      raise Exception, reason: reason, action: "read info of file", path: path
    end
  end

  ## Helpers

  # Normalize the given path by removing "..".
  defp normalize(path), do: normalize(split(path), [])

  defp normalize([top|t], [_|acc]) when top == ".." or top == '..' do
    normalize t, acc
  end

  defp normalize([top|t], acc) when top == "." or top == '.' do
    normalize t, acc
  end

  defp normalize([h|t], acc) do
    normalize t, [h|acc]
  end

  defp normalize([], acc) do
    join List.reverse(acc)
  end
end
